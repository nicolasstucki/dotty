
class phantomFunDef {
  def fun() = ??? // error
  def fun(p: PhantomAny) = ??? // error
  def fun(p1: PhantomAny, p2: PhantomAny) = ??? // error
}
