
class Blinky extends PhantomAny

class phantomClassOf {
  classOf[Blinky] // error
  classOf[PhantomAny] // error
  classOf[PhantomNothing] // error
}
