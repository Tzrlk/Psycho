package nz.co.aetheric.psycho.emotiv.epoc

import nz.co.aetheric.psycho.emotiv.epoc.EpocSensors.EpocSensor

import scala.concurrent.Future

case class EpocPacket(

	time : Long,

	battery : Integer,

	frame : Array[Byte],

	quality : Map[EpocSensor, Int],

	next: Future[EpocPacket]

)