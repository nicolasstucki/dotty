package dotty.tools.dotc.linker

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types._

final class SubstituteOuterTargs(outerTargs: OuterTargs)(implicit ctx: Context) extends DeepTypeMap {
  def apply(tp: Type): Type = {
    lazy val substitution = outerTargs.mp.getOrElse(tp.typeSymbol.owner, Nil)

    def termTypeIfNeed(t: Type): Type = {
      if (tp.isInstanceOf[TermType] && !t.isInstanceOf[TermType]) {
        t match {
          case t: TypeAlias =>
            assert(t.underlying.isInstanceOf[TermType])
            t.underlying
          case t: ClassInfo =>
            t.appliedRef
          case _ =>
            assert(false)
            null
        }
      } else t
    }

    val res = tp match {
      case tp: RefinedType => mapOver(tp) // otherwise we will loose refinement
      case tp: TypeAlias => mapOver(tp) // map underlying
      case tp: AppliedType => mapOver(tp) // map underlying
      case _ if tp.typeSymbol.exists && substitution.nonEmpty =>
        var typ = tp
        var id = substitution.find(x => x._1 == tp.typeSymbol.name)
        var limit = 30
        var stack: List[Type] = Nil
        while (id.isEmpty && (limit > 0) && (typ.typeSymbol.info.typeSymbol ne typ.typeSymbol)) {
          typ = typ.typeSymbol.info
          stack = typ :: stack
          id = substitution.find(x => x._1 == typ.typeSymbol.name)
          limit -= 1
        }

        if (id.isDefined) {
          val t = termTypeIfNeed(id.get._2.stripTypeVar)
          if (!(t =:= typ))
            apply(termTypeIfNeed(t))
          else t
        } else tp
      case t: TypeRef if t.prefix.normalizedPrefix eq NoPrefix =>
        val tmp = apply(t.info)
        if (tmp ne t.info) termTypeIfNeed(tmp)
        else mapOver(t)
      case _ => mapOver(tp)
    }
    assert(!(tp.isInstanceOf[TypeType] ^ res.isInstanceOf[TypeType]), (tp, res))
    res
  }
}
