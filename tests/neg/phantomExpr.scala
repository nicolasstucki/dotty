import dotty.phantom.PhantomAny

object Blinky extends PhantomAny
object Inky extends PhantomAny

class Foo {
  val b = true

  def fooIf1 = if (b) { Blinky } else { Inky } // error // error
  def fooIf2 = if (b) { Blinky } else { "" } // error
  def fooIf3 = if (b) { "" } else { Blinky } // error

  def fooIf4 = if (true) { Blinky } else { Inky } // error // error
  def fooIf5 = if (false) { Blinky } else { Inky } // error // error

  def fooMatch1 = Blinky match { case Blinky => () } // error
  def fooMatch2 = 1 match { case _ => Blinky } // error
  def fooMatch3 = 1 match { case Blinky => () } // error

  def fooTry1 = try { Blinky } catch { case e: Throwable => } // error
  def fooTry2 = try { } catch { case e: Throwable => Blinky } // error

  def fooReturn1: PhantomAny = return Blinky // error
  def fooReturn2 = return Blinky // error

}
