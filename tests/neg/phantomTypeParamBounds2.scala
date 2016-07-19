import dotty.phantom.PhantomAny
import dotty.phantom.PhantomNothing

class phantomTypeParamBounds2 {
  def fun1[X <: PhantomAny & Any] = ??? // error
  def fun2[X <: PhantomAny | Any] = ??? // error
  def fun3[X >: PhantomNothing & Nothing] = ??? // error
  def fun4[X >: PhantomNothing | Nothing] = ??? // error

  def fun5[X >: PhantomAny & Any <: PhantomAny & Any] = ??? // error // error
}
