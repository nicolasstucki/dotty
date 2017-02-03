
class Casper extends PhantomAny

class PhantomFunTArgs1 extends Function0[Casper] { // error: Type argument Casper does not conform to upper bound Any
  def apply() = new Casper
}

class PhantomFunTArgs2 extends Function1[Casper, Casper] { // error: Type argument Casper does not conform to upper bound Any
  def apply(p1: Casper) = new Casper
}
