import dotty.phantom._

class Casper extends PhantomAny

class PhantomFunTArgs1 extends PhantomsFunction1[Int, Unit] { // error: Type argument Int does not conform to upper bound PhantomAny
  def apply(p1: Int) = ???
}

class PhantomFunTArgs2 extends PhantomsFunction1[Casper, Casper] { // error: Type argument Casper does not conform to upper bound Any
  def apply(p1: Casper) = new Casper
}
