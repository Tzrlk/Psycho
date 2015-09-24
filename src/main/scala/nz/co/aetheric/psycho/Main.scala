package nz.co.aetheric.psycho

import java.util.stream.StreamSupport

import nz.co.aetheric.psycho.emotiv.epoc.{EpocPacket, EpocPacketStream}

class Main {

	def main(args: Array[String]) {

		val stream = new EpocPacketStream()
		try {

			stream.open()

			StreamSupport.stream[EpocPacket](stream.iterator.spliterator, false)

		} finally {
			stream.close()
		}

	}
}