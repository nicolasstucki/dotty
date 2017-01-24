import dotty.phantom._

class Casper extends PhantomAny

class PhantomFunTArgs2 extends PhantomsFunction1[Casper, Casper] { // error: Type argument Casper does not conform to upper bound Any
  def apply(p1: Casper) = new Casper
}
