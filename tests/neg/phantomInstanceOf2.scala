
class Blinky extends PhantomAny

class phantomInstanceOf2 {
  new Blinky().asInstanceOf[Any] // error
  new Blinky().asInstanceOf[Nothing] // error
  new Blinky().asInstanceOf[PhantomAny] // error
}
