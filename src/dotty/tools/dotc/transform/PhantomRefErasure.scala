package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.DenotTransformers._
import dotty.tools.dotc.core.Phases._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

import scala.annotation.tailrec

class PhantomRefErasure extends MiniPhaseTransform with InfoTransformer {
  import tpd._
  import Phantoms._

  override def phaseName: String = "phantomRefErasure"

  /** List of names of phases that should precede this phase */
  override def runsAfter: Set[Class[_ <: Phase]] =
    Set(classOf[InterceptedMethods], classOf[Splitter], classOf[ElimRepeated])

  /** Check what the phase achieves, to be called at any point after it is finished. */
  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    def assertNotPhantom(tree: Tree): Unit =
      assert(!tree.tpe.isPhantom, "All phantom type values should be erased in " + tree)
    tree match {
      case _: TypeTree         =>
      case _: Trees.TypeDef[_] =>
      case Block(stats, _)     => stats.foreach(assertNotPhantom)

      case DefDef(_, _, vparamss, tpt, _) if !returnsPhantom(tpt.tpe) =>
        vparamss.foreach(_.foreach(assertNotPhantom))

      case _ =>
    }
  }

  /* Tree transform */

  override def transformStats(trees: List[Tree])(implicit ctx: Context, info: TransformerInfo): List[Tree] = {
    trees.collect {
      case stat: TypeDef     => stat
      case stat: ValOrDefDef => stat
      case Block(stats, expr) if expr.tpe.isPhantom => Block(stats, ref(defn.BoxedUnit_UNIT))
      case stat if !stat.tpe.isPhantom => stat
    }
  }

  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo): Tree = {
    tree.qualifier match {
      case _: Ident => tree
      case qual if tree.tpe.isPhantom =>
        // We keep the name of selected member in the name of the synthetic val to ease debugging.
        val synthVal = SyntheticValDef(ctx.freshName("phantomLift$" + tree.name + "$").toTermName, tree.qualifier)
        val synthValRef = Ident(synthVal.symbol.termRef)
        transform(Block(List(synthVal), Select(synthValRef, tree.name)))

      case _ => tree
    }
  }

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (returnsPhantom(tree.tpe)) {
      tree
    } else if (!tree.args.exists(arg => arg.tpe.isPhantom)) {
      tree.fun match {
        case Block(stats, fun) => Block(stats, cpy.Apply(tree)(fun, tree.args))
        case _                 => tree
      }
    } else {
      val synthVals = tree.args.map { arg =>
        SyntheticValDef(ctx.freshName("phantomLift$").toTermName, arg)
      }
      val synthValRefs = synthVals.map(synthVal => Ident(synthVal.symbol.termRef))
      val newArgs = synthValRefs.filter(synthValRef => !synthValRef.tpe.isPhantom)
      val args = transformStats(synthVals)
      tree.fun match {
        case Block(stats, fun) =>
          Block(stats ::: args, cpy.Apply(tree)(fun, newArgs))
        case _ =>
          val newApply = cpy.Apply(tree)(tree.fun, newArgs)
          if (args.isEmpty) newApply
          else Block(args, newApply)
      }
    }
  }

  override def transformDefDef(ddef: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (ddef.tpt.tpe.isPhantom) {
     ddef
    } else {
      val newVparamss = ddef.vparamss.map(_.filter(vparam => !vparam.tpt.typeOpt.isPhantom))
      if (newVparamss == ddef.vparamss) ddef
      else cpy.DefDef(ddef)(vparamss = newVparamss)
    }
  }

  /* Symbol transform */

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = erasedPhantomParameters(tp)

}
