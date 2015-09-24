package nz.co.aetheric.psycho.emotiv.epoc

import java.util

import com.codeminders.hidapi._
import com.google.common.collect.Lists
import com.typesafe.scalalogging.LazyLogging
import lombok.Getter
import lombok.extern.java.Log
import javax.annotation.concurrent.NotThreadSafe
import javax.crypto.spec.SecretKeySpec
import java.io.Closeable
import java.io.IOException
import java.util.concurrent.TimeoutException
import java.lang.String.format
import java.lang.System.currentTimeMillis

/**
 * Wrapper for the low level HIDAPI to access an Emotiv EEG.
 * <p/>
 * Supported devices are discovered on construction and a
 * poll is provided to obtain raw packets.
 *
 * @author Sam Halliday
 */
@Log
@NotThreadSafe
object EpocHid extends LazyLogging {
	private[emokit] val VENDOR_ID: Int = 8609
	private[emokit] val PRODUCT_ID: Int = 1
	private[emokit] val BUFSIZE: Int = 32
	private[emokit] val TIMEOUT: Int = 1000
	private val supportedResearch: util.List[Array[Byte]] = Lists.newArrayList()
	private val supportedConsumer: util.List[Array[Byte]] = Lists.newArrayList()
	try {
		try {
			ClassPathLibraryLoader.loadNativeHIDLibrary
			supportedConsumer.add(Array[Byte](33, -1, 31, -1, 30, 0, 0, 0))
			supportedConsumer.add(Array[Byte](32, -1, 31, -1, 30, 0, 0, 0))
			supportedConsumer.add(Array[Byte](-32, -1, 31, -1, 0, 0, 0, 0))
		}
		catch {
			case e: Exception =>
				throw new ExceptionInInitializerError(e)
		}
	}
}

@Log
@NotThreadSafe
final class EpocHid extends Closeable with LazyLogging {

	@volatile
	private var research: Boolean = false

	private val device: HIDDevice = {
		val device = findEmotiv
		device.enableBlocking()
		device
	}

	@Getter
	@volatile
	private var closed: Boolean = false

	def isOpen : Boolean = !closed
	def isClosed : Boolean = closed

	@throws(classOf[IOException])
	def close() {
		closed = true
		device.close()
	}

	@throws(classOf[Throwable])
	override def finalize() {
		this synchronized {
			close()
			super.finalize()
		}
	}

	/**
	 * @param buf use the supplied buffer.
	 * @throws java.io.IOException if there was no response from the Emotiv.
	 * @throws java.util.concurrent.TimeoutException    which may indicate that the Emotiv is not connected.
	 */
	@throws(classOf[TimeoutException])
	@throws(classOf[IOException])
	def poll(buf: Array[Byte]): Array[Byte] = {
		assert(buf.length == EpocHid.BUFSIZE)
		var n: Int = 0
		val startTime: Long = currentTimeMillis
		while ( {
			n = device.readTimeout(buf, 0)
			n
		} == 0 && currentTimeMillis - startTime < EpocHid.TIMEOUT) {
			try {
				Thread.sleep(10)
			}
			catch {
				case e: InterruptedException =>
			}
			Thread.`yield`()
		}
		if (n != EpocHid.BUFSIZE) throw new IOException(format("Bad Packet: (%s) %s", n, util.Arrays.toString(buf)))
		return buf
	}

	/**
	 * @return the crypto key for this device.
	 * @throws java.io.IOException x x
	 */
	@throws(classOf[IOException])
	def getKey: SecretKeySpec = {
		val serial: String = getSerial
		val raw: Array[Byte] = serial.getBytes
		assert(raw.length == 16)
		val bytes: Array[Byte] = new Array[Byte](16)
		bytes(0) = raw(15)
		bytes(1) = 0
		bytes(2) = raw(14)
		bytes(3) = if (research) 'H'.toByte else 'T'.toByte
		bytes(4) = if (research) raw(15) else raw(13)
		bytes(5) = if (research) 0.toByte else 16
		bytes(6) = if (research) raw(14) else raw(12)
		bytes(7) = if (research) 'T'.toByte else 'B'.toByte
		bytes(8) = if (research) raw(13) else raw(15)
		bytes(9) = if (research) 16.toByte else 0
		bytes(10) = if (research) raw(12) else raw(14)
		bytes(11) = if (research) 'B'.toByte else 'H'.toByte
		bytes(12) = raw(13)
		bytes(13) = 0
		bytes(14) = raw(12)
		bytes(15) = 'P'
		return new SecretKeySpec(bytes, "AES")
	}

	/**
	 * @return
	 */
	@throws(classOf[IOException])
	def getSerial: String = {
		val serial: String = device.getSerialNumberString
		if (!serial.startsWith("SN") || serial.length != 16) throw new IOException("Bad serial: " + serial)
		return serial
	}

	@throws(classOf[IOException])
	private def findDevices(vendor: Int, product: Int): util.List[HIDDeviceInfo] = {
		val manager: HIDManager = HIDManager.getInstance
		val infos: Array[HIDDeviceInfo] = manager.listDevices
		val devs: util.List[HIDDeviceInfo] = Lists.newArrayList()
		for (info <- infos) {
			if (info.getVendor_id == vendor && info.getProduct_id == product) devs.add(info)
		}
		return devs
	}

	@throws(classOf[IOException])
	private def findEmotiv: HIDDevice = {
		val infos: util.List[HIDDeviceInfo] = findDevices(EpocHid.VENDOR_ID, EpocHid.PRODUCT_ID)
		import scala.collection.JavaConversions._
		for (info <- infos) {
			val dev: HIDDevice = info.open
			try {
				val report: Array[Byte] = new Array[Byte](9)
				val size: Int = dev.getFeatureReport(report)
				val result: Array[Byte] = util.Arrays.copyOf(report, size)
				logger.info(format("Found (%s) %s [%s] with report: %s", dev.getManufacturerString,
					dev.getProductString, dev.getSerialNumberString, util.Arrays.toString(result)))
				import scala.collection.JavaConversions._
				for (check <- EpocHid.supportedConsumer) {
					if (util.Arrays.equals(check, result)) {
						return dev
					}
				}
				import scala.collection.JavaConversions._
				for (check <- EpocHid.supportedResearch) {
					if (util.Arrays.equals(check, result)) {
						research = true
						return dev
					}
				}
				dev.close()
			}
			catch {
				case e: Exception =>
					dev.close()
			}
		}
		throw new HIDDeviceNotFoundException("Send all this information to https://github.com/fommil/emokit-java/issues and let us know if you have the 'research' or 'consumer' product.")
	}
}
