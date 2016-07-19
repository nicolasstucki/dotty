import dotty.phantom.PhantomAny
import dotty.phantom.PhantomNothing

class Blinky extends PhantomAny

class phantomClassOf {
  classOf[Blinky] // error
  classOf[PhantomAny] // error
  classOf[PhantomNothing] // error
}
