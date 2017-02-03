
class BooFunDef1 {
  def fun1(b: PhantomAny | Any) = ??? // error
  def fun2(b: PhantomAny | Any | Any) = ??? // error
  def fun3(b: Any | PhantomAny | Any) = ??? // error
  def fun4(b: PhantomAny | PhantomAny | Any) = ??? // error

  def fun5(b: PhantomAny & Any) = ??? // error
  def fun6(b: Any & PhantomAny & Any) = ??? // error
  def fun7(b: PhantomAny & Any & Any) = ??? // error
  def fun8(b: Any & Any & PhantomAny) = ??? // error
}
