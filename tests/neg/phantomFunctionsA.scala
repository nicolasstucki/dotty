import dotty.phantom._

class Casper extends PhantomAny

class PhantomFun0 extends PhantomsFunction0[Unit] { // error: not found: type PhantomsFunction0
  def apply(): Unit = ???
}

class `PhantomFun-1` extends `PhantomsFunction-1`[Unit] { // error: not found: type PhantomsFunction-1
  def apply(): Unit = ???
}


class PhantomFun0ABC extends PhantomsFunctionABC[Unit] { // error: not found: type PhantomsFunctionABC
  def apply(): Unit = ???
}

class `PhantomFun👻` extends `PhantomsFunction👻`[Unit] { // error: not found: type PhantomsFunction👻
  def apply(): Unit = ???
}
