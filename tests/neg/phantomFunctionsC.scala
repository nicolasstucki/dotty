import dotty.phantom._

class Casper extends PhantomAny

class PhantomFun1NoApply extends PhantomsFunction1[Casper, Unit] // error: class PhantomFun1NoApply needs to be abstract, since def apply: (p0: Casper)Unit is not defined
