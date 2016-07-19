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
      assert(!isPhantom(tree.tpe), "All phantom type values should be erased in " + tree)
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
      case Block(stats, expr) if isPhantom(expr.tpe) => Block(stats, ref(defn.BoxedUnit_UNIT))
      case stat if !isPhantom(stat.tpe) => stat
    }
  }

  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo): Tree = {
    tree.qualifier match {
      case _: Ident => tree
      case qual if isPhantom(tree.tpe) =>
        // We keep the name of selected member in the name of the synthetic val to ease debugging.
        val synthVal = SyntheticValDef(ctx.freshName("phantom$" + tree.name + "$").toTermName, tree.qualifier)
        val synthValRef = Ident(synthVal.symbol.termRef)
        transform(Block(List(synthVal), Select(synthValRef, tree.name)))

      case _ => tree
    }
  }

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    @tailrec def hasParams(tpe: Type): Boolean = tpe match {
      case tpe: TermRef    => hasParams(tpe.info)
      case tpe: MethodType => tpe.paramTypes.nonEmpty
      case _               => false
    }

    // This transformation assumes that phantom and non phantom parameters are not mixed together
    if (tree.args.isEmpty || hasParams(tree.fun.typeOpt)) {
      tree
    } else {
      val args = transformStats(tree.args.map(transform))
      val newApply = cpy.Apply(tree)(tree.fun, Nil)
      if (args.isEmpty) newApply
      else Block(args, newApply)
    }
  }

  override def transformDefDef(ddef: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (isPhantom(ddef.tpt.tpe)) {
     ddef
    } else {
      val newVparamss = ddef.vparamss.map(_.filter(vparam => !isPhantom(vparam.tpt.typeOpt)))
      if (newVparamss == ddef.vparamss) ddef
      else cpy.DefDef(ddef)(vparamss = newVparamss)
    }
  }

  /* Symbol transform */

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = erasedPhantomParameters(tp)

}
