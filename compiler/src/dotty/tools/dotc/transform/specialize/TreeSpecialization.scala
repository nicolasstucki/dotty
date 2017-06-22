package dotty.tools.dotc
package transform
package specialize

import core._
import Types._
import Contexts.Context
import Symbols._
import Decorators._
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{TreeTypeMap, tpd}
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.linker._

object TreeSpecialization {
  import tpd._

  def specializedDefDef(ddef: DefDef, specSym: Symbol, outerTargs: OuterTargs)(implicit ctx: Context): DefDef = {
    val oldSym = ddef.symbol
    def rhsFn(tparams: List[Type])(vparamss: List[List[Tree]]) = {
      val transformTparams: Map[Symbol, Type] = ddef.tparams.map(_.symbol).zip(tparams).toMap
      val transformVparams: Map[Symbol, Tree] = (ddef.vparamss.flatten.map(_.symbol) zip vparamss.flatten).toMap

      // Transform references to types
      val typeMap = new TypeMap() {
        override def apply(tp: Type): Type = tp match {
          case tp: TypeRef if tp.typeSymbol.owner == oldSym && tp.typeSymbol.is(Param) =>
            transformTparams.getOrElse(tp.typeSymbol, tp)
          case tp: TermRef if tp.termSymbol.owner == oldSym && tp.termSymbol.is(Param) =>
            transformVparams.get(tp.termSymbol).map(_.symbol.termRef).getOrElse(tp)
          case _ => mapOver(tp)
        }
      }

      // Transform references to terms
      def treeMap(tree: Tree): Tree = tree match {
        case tree: Ident if tree.symbol.isTerm && tree.symbol.is(Param) && tree.symbol.owner == oldSym =>
          transformVparams(tree.symbol)
        case Return(expr, from) if from.symbol == oldSym => Return(expr, ref(specSym))
        case _ => tree
      }

      // Apply transforms
      val treeTypeMap = new TreeTypeMap(typeMap, treeMap, oldSym :: Nil, specSym :: Nil)
      treeTypeMap.transform(ddef.rhs)
    }

    val specDefDef = polyDefDef(specSym.asTerm, rhsFn)

    postSpecializedDefDef(specDefDef, outerTargs)
  }

  private def postSpecializedDefDef(specDefDef: DefDef, outerTargs: OuterTargs)(implicit ctx: Context): DefDef = {
    def treeMap(tree: Tree): Tree = tree match {
      case Match(sel, cases) =>
        val newCases = cases.filter {
          case CaseDef(Bind(_, Typed(_, tp)), _, _) => sel.tpe <:< tp.tpe
          // TODO: other patterns?
          case _ => true
        }
        if (newCases.isEmpty) Throw(New(defn.MatchErrorType, List(ref(sel.symbol))))
        else cpy.Match(tree)(sel, newCases)
      case tree @ TypeApply(fun, args) =>
        fun match {
          case Select(qual, _) if fun.symbol == defn.Any_isInstanceOf =>
            if (qual.tpe <:< args.head.tpe) Literal(Constants.Constant(true))
            else if (!(args.head.tpe <:< qual.tpe)) Literal(Constants.Constant(false))
            else tree
          case _ => tree
        }
      case _ => tree
    }
    val substMap = new SubstituteOuterTargs(outerTargs)
    val transformInnerCalls = new TreeTypeMap(typeMap = substMap, treeMap = treeMap)
    transformInnerCalls.transform(specDefDef).asInstanceOf[DefDef]
  }

}
