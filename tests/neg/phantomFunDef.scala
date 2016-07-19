import dotty.phantom.PhantomAny
import dotty.phantom.PhantomNothing

class phantomFunDef {
  def fun1(a: Int, b: PhantomAny) = ??? // error
  def fun2(n: Int)(a: Int, b: PhantomAny) = ??? // error
  def fun3(p: PhantomAny)(a: Int, b: PhantomAny) = ??? // error

  def fun4() = ??? // error
  def fun4(p: PhantomAny) = ??? // error
  def fun4(p1: PhantomAny, p2: PhantomAny) = ??? // error

}
