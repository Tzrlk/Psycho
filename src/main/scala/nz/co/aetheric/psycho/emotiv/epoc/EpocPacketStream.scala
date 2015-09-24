package nz.co.aetheric.psycho.emotiv.epoc

import java.io.{Closeable, IOException}
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.TimeoutException
import javax.crypto.{BadPaddingException, Cipher, IllegalBlockSizeException}

import com.typesafe.scalalogging.LazyLogging
import nz.co.aetheric.psycho.emotiv.epoc.EpocSensors.EpocSensor

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration.Duration

class EpocPacketStream extends Iterable with Closeable with LazyLogging {

	private val raw: EpocHid = new EpocHid()
	private val accessed: AtomicBoolean = new AtomicBoolean()
	private val timeout: Duration = Duration(5, "millis")

	private lazy val cipher: Cipher = {
		try {
			val cipher = Cipher.getInstance("AES/ECB/NoPadding")
			cipher.init(Cipher.DECRYPT_MODE, raw.getKey)
			cipher
		} catch {
			case e: Exception =>
				throw new IllegalStateException("no javax.crypto support")
		}
	}

	private var last: EpocPacket = null
	private var lastCounter: Byte = -1

	private val reader = new Iterator[EpocPacket] {

		val bytes: Array[Byte] = new Array[Byte](EpocHid.BUFSIZE)

		override def hasNext: Boolean = raw.isOpen

		@throws(classOf[TimeoutException])
		@throws(classOf[IOException])
		@throws(classOf[BadPaddingException])
		@throws(classOf[IllegalBlockSizeException])
		override def next(): EpocPacket = {

			raw.poll(bytes)

			val timestamp: Long = System.currentTimeMillis
			val decrypted: Array[Byte] = cipher.doFinal(bytes)

			val counter: Byte = decrypted(0)
			var battery: Int = 0

			if (counter != lastCounter + 1 && lastCounter != 127) {
				logger.warn("missed a packet")
			}

			if (counter < 0) {
				lastCounter = -1
				battery = 0xFF & counter

			} else {
				lastCounter = counter
			}

			val channel: EpocSensors.EpocSensor = EpocSensors.getQualityChannel(counter)

			val quality = new mutable.HashMap[EpocSensor, Int]()

			if (channel != null) {
				val reading: Int = EpocSensors.QUALITY.apply(decrypted)
				quality.put(channel, reading)
			}

			last = new EpocPacket(timestamp, battery, bytes, quality.toMap, Future {
				return next()
			})

			return last

		}

	}

	/**
	 * Poll the device in a background thread and sends signals to registered
	 * listeners using a thread pool.
	 */
	def open(): Unit = {

		if (accessed.getAndSet(true)) {
			throw new IllegalStateException("Cannot be called more than once.")
		}

	}


	override def iterator: Iterator[EpocPacket] = {
		return new Iterator[EpocPacket] {

			var position: EpocPacket = reader.next()

			override def hasNext: Boolean = raw.isOpen

			override def next(): EpocPacket = {
				val prev = position
				position = position.next.result(timeout)
				prev
			}

		}
	}

	@throws(classOf[IOException])
	override def close(): Unit = {
		raw.close()
	}

}