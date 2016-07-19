import dotty.phantom.PhantomAny
import dotty.phantom.PhantomNothing

class phantomTypeParamBounds1 {
  def fun5[X >: PhantomNothing <: Any] = ??? // error
  def fun6[X >: Nothing <: PhantomAny] = ??? // error
}
