
class Blinky extends PhantomAny

class phantomVarLazyVal {
  var a = new Blinky // error
  lazy val b = new Blinky // error
}
