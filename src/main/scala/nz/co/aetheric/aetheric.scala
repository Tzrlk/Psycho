package nz.co

package object aetheric {

	def toBytes(xs: Int*) = xs.map(_.toByte).toArray

}