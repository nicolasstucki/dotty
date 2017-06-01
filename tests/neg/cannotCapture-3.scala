import scala.annotation.{CannotBeCaptured, CannotCapture}
import Boo.{boo, BooAny}

object Foo {

  new StoicFunction0[Unit] {
    @CannotCapture def apply(): Unit = {
      boo // error
      ()
    }
  }

  new StoicFunction0[StoicPhantomFunction1[BooAny, Unit]] {
    @CannotCapture def apply(): StoicPhantomFunction1[BooAny, Unit] = {
      new StoicPhantomFunction1[BooAny, Unit] {
        @CannotCapture def apply(x1: BooAny): Unit = {
          boo // error
        }
      }
    }
  }

  new StoicFunction0[StoicPhantomFunction1[BooAny, Unit]] {
    @CannotCapture def apply(): StoicPhantomFunction1[BooAny, Unit] = {
      new StoicPhantomFunction1[BooAny, Unit] {
        @CannotCapture def apply(x1: BooAny): Unit = {
          x1
        }
      }
    }
  }.apply().apply(boo)

  new StoicPhantomFunction1[BooAny, StoicFunction0[Unit]] {
    @CannotCapture def apply(x1: BooAny): StoicFunction0[Unit] = {
      new StoicFunction0[Unit] {
        @CannotCapture def apply(): Unit = {
            x1 // error
        }
      }
    }
  }.apply(boo).apply()

  new StoicPhantomFunction1[BooAny, Function0[Unit]] {
    @CannotCapture def apply(x1: BooAny): Function0[Unit] = {
      new Function0[Unit] {
        def apply(): Unit = {
          x1
        }
      }
    }
  }.apply(boo).apply()

  val ff1: StoicFunction0[Unit] = () => {
    boo // error
    ()
  }

  type ->[T1, R] = StoicFunction1[T1, R]

  val ff2: Int -> Unit = n => {
    boo // error
    ()
  }


  val ff3: StoicPhantomFunction1[BooAny, Unit] = (x) => {
    x
    ()
  }
  ff3.apply(boo)

  @CannotCapture
  class C {
    boo // error
  }

  @CannotCapture
  class D(x: BooAny) {
    x
  }

  @CannotCapture
  object O {
    boo // error
  }

  @CannotCapture
  val x = boo // error

  @CannotCapture
  lazy val y = boo // error

}


@CannotBeCaptured
object Boo extends Phantom {
  type BooAny <: this.Any
  def boo: BooAny = assume
  def boo2(): BooAny = assume
}

trait StoicPhantomFunction1[-T1 <: BooAny, +R] {
  @CannotCapture def apply(x1: T1): R
}

trait StoicFunction0[+R] extends Function0[R] {
  @CannotCapture def apply(): R
}

trait StoicFunction1[-T1, +R] extends Function1[T1, R] {
  @CannotCapture def apply(x1: T1): R
}

trait StoicFunction2[-T1, -T2, +R] extends Function2[T1, T2, R] {
  @CannotCapture def apply(x1: T1, x2: T2): R
}
