package dotty.tools.dotc
package transform

import core._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}
import Types._
import Contexts.Context
import Symbols._
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{TreeTypeMap, tpd}
import dotty.tools.dotc.core.DenotTransformers.InfoTransformer
import dotty.tools.dotc.core.Flags._

class Specialized2 extends MiniPhaseTransform with InfoTransformer { thisTransformer =>
  import tpd._

  override def phaseName = "specialized2"

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val specializations = ctx.phaseOfClass(classOf[Specialized]).asInstanceOf[Specialized].specializations
    val newSyms = specializations.toList.filter(_._1._1 == tree.symbol).map(_._2)
    val specTrees = newSyms.map(newSym => createSpecializedDefDef(tree, newSym))
    if (specTrees.isEmpty) tree
    else Thicket(tree :: specTrees)
  }

  private def createSpecializedDefDef(ddef: DefDef, specSym: Symbol)(implicit ctx: Context) = {
    val oldSym = ddef.symbol
    def rhsFn(tparams: List[Type])(vparamss: List[List[Tree]]) = {
      val transformTparams: Map[Symbol, Type] =
        ddef.tparams.map(_.symbol).zip(tparams).toMap

      def vparamssSyms(vparamss: List[List[Tree]]): List[Symbol] = vparamss.flatten.map(_.symbol)
      val transformVparams: Map[Symbol, Symbol] =
        (vparamssSyms(ddef.vparamss) zip vparamssSyms(vparamss)).toMap

      def treeMap(tree: Tree): Tree = tree match {
        case t: Ident if t.symbol.is(Param) && t.symbol.owner == oldSym => ref(transformVparams(t.symbol))
        case Return(t, from) if from.symbol == oldSym => Return(t, ref(specSym))
        case t => t
      }

      val typeMap = new TypeMap() {
        override def apply(tp: Type): Type = tp match {
          case tp: TypeRef if tp.typeSymbol.owner == oldSym && tp.typeSymbol.is(Param) =>
            transformTparams.getOrElse(tp.typeSymbol, tp)
          case _ => mapOver(tp)
        }
      }

      val treeTypeMap = new TreeTypeMap(typeMap, treeMap, oldSym :: Nil, specSym :: Nil)

      treeTypeMap.transform(ddef.rhs)
    }

    val a = polyDefDef(specSym.asTerm, rhsFn)
    a
  }

  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = {
    val specializations = ctx.phaseOfClass(classOf[Specialized]).asInstanceOf[Specialized].specializations
    tp match {
      case tp: ClassInfo =>
        val newSyms = specializations.toList.filter(_._1._1.owner == tp.cls).map(_._2)
        if (newSyms.isEmpty) tp
        else {
          val newDecls = tp.decls.cloneScope.openForMutations
          newSyms.foreach(sym => newDecls.enter(sym))
          ClassInfo(tp.prefix, tp.cls, tp.classParents, newDecls, tp.selfInfo)
        }
      case _ => tp
    }
  }
}
