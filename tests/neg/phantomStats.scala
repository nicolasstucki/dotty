import dotty.phantom.PhantomAny
import dotty.phantom.PhantomNothing

class Blinky extends PhantomAny

class phantomStats {

  new Blinky // error: Expression returning a phantom type can not be in statement position.
  phantom1 // error: Expression returning a phantom type can not be in statement position.
  phantom2() // error: Expression returning a phantom type can not be in statement position.

  def boo(): Unit = {
    new Blinky // error: Expression returning a phantom type can not be in statement position.
    phantom1 // error: Expression returning a phantom type can not be in statement position.
    phantom2() // error: Expression returning a phantom type can not be in statement position.

    { // error: Expression returning a phantom type can not be in statement position.
      phantom1
    }

    { // error: Expression returning a phantom type can not be in statement position.
      {
        phantom1
      }
    }

    {
      phantom1 // error: Expression returning a phantom type can not be in statement position.

      {
        phantom1 // error: Expression returning a phantom type can not be in statement position.

        {
          phantom1 // error: Expression returning a phantom type can not be in statement position.
          ()
        }
      }
    }
    ()
  }

  def phantom1: PhantomAny = new Blinky
  def phantom2(): PhantomAny = new Blinky

  def phantom3(): PhantomAny = {
    println(1) // error: Methods returning phantom values can not have statements.
    println(2) // error: Methods returning phantom values can not have statements.
    new Blinky
  }

  def phantom4(): PhantomAny = {
    {
      println(1) // error: Methods returning phantom values can not have statements.
      println(2) // error: Methods returning phantom values can not have statements.
      new Blinky
    }
  }
}
