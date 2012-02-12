package vc.ksk.elevator



case class Person(id:Int,from:Int,to:Int,at:Int) extends Ordered[Person]{
	def compare(that:Person) = {
	    that.at - this.at
	}
}