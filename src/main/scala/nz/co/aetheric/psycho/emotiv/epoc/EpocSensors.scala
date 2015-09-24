package nz.co.aetheric.psycho.emotiv.epoc

import nz.co.aetheric.toBytes
import nz.co.aetheric.EnumerationMacros._

import scala.collection.immutable.HashMap

object EpocSensors {

	sealed abstract class EpocSensor(val bits : Array[Byte]) {

		def apply(frame : Array[Byte]) : Int = {

			var level = 0
			var i : Int = bits.length - 1

			while (i >= 0) {
				level <<= 1
				val b: Int = (bits(i) >> 3) + 1
				val o: Int = bits(i) % 8
				level |= ((0xFF & frame(b)) >>> o) & 1
				i -= 1
			}

			level
		}

	}

	case object QUALITY extends EpocSensor(toBytes(99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112))
	case object F3 extends EpocSensor(toBytes(10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5, 6, 7))
	case object FC5 extends EpocSensor(toBytes(28, 29, 30, 31, 16, 17, 18, 19, 20, 21, 22, 23, 8, 9))
	case object AF3 extends EpocSensor(toBytes(46, 47, 32, 33, 34, 35, 36, 37, 38, 39, 24, 25, 26, 27))
	case object F7 extends EpocSensor(toBytes(48, 49, 50, 51, 52, 53, 54, 55, 40, 41, 42, 43, 44, 45))
	case object T7 extends EpocSensor(toBytes(66, 67, 68, 69, 70, 71, 56, 57, 58, 59, 60, 61, 62, 63))
	case object P7 extends EpocSensor(toBytes(84, 85, 86, 87, 72, 73, 74, 75, 76, 77, 78, 79, 64, 65))
	case object O1 extends EpocSensor(toBytes(102, 103, 88, 89, 90, 91, 92, 93, 94, 95, 80, 81, 82, 83))
	case object O2 extends EpocSensor(toBytes(140, 141, 142, 143, 128, 129, 130, 131, 132, 133, 134, 135, 120, 121))
	case object P8 extends EpocSensor(toBytes(158, 159, 144, 145, 146, 147, 148, 149, 150, 151, 136, 137, 138, 139))
	case object T8 extends EpocSensor(toBytes(160, 161, 162, 163, 164, 165, 166, 167, 152, 153, 154, 155, 156, 157))
	case object F8 extends EpocSensor(toBytes(178, 179, 180, 181, 182, 183, 168, 169, 170, 171, 172, 173, 174, 175))
	case object AF4 extends EpocSensor(toBytes(196, 197, 198, 199, 184, 185, 186, 187, 188, 189, 190, 191, 176, 177))
	case object FC6 extends EpocSensor(toBytes(214, 215, 200, 201, 202, 203, 204, 205, 206, 207, 192, 193, 194, 195))
	case object F4 extends EpocSensor(toBytes(216, 217, 218, 219, 220, 221, 222, 223, 208, 209, 210, 211, 212, 213))

	val sensors : Set[EpocSensor] = sealedInstancesOf[EpocSensor]
	private val map = new HashMap[Byte, EpocSensor] = Map(
		0 -> F3,
		1 -> FC5,
		2 -> AF3,
		3 -> F7,
		4 -> T7,
		5 -> P7,
		6 -> O1,
		7 -> O2,
		8 -> P8,
		9 -> T8,
		10 -> F8,
		11 -> AF4,
		12 -> FC6,
		13 -> F4,
		14 -> F8,
		15 -> AF4
	)

	def getQualityChannel(counter: Byte): EpocSensor = {

		if (64 <= counter && counter <= 75) {
			return getQualityChannel((counter - 64).toByte)
		}

		map.get(counter).get

	}

}


