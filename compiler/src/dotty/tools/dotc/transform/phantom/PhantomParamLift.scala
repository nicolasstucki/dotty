package dotty.tools.dotc.transform.phantom

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.DenotTransformers._
import dotty.tools.dotc.core.Phases._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

class PhantomParamLift extends MiniPhaseTransform {
  import Phantoms._
  import tpd._

  override def phaseName: String = "phantomParamLift"

  /** List of names of phases that should precede this phase */
  override def runsAfter: Set[Class[_ <: Phase]] =
    Set(classOf[PhantomChecks])

  /** Check what the phase achieves, to be called at any point after it is finished. */
  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    def assertNotPhantom(tree: Tree): Unit =
      assert(!tree.tpe.isPhantom, "All phantom type values should be erased in " + tree)
    tree match {
      case Apply(fun, args) if !returnsPhantom(tree.tpe) && args.exists(_.tpe.isPhantom) =>
        args.foreach(arg => assert(arg.isInstanceOf[Ident], arg))
      case _ =>
    }
  }

  /* Tree transform */

  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo): Tree = {
    tree.qualifier match {
      case _: Ident => tree
      case qual if tree.tpe.isPhantom =>
        // We keep the name of selected member in the name of the synthetic val to ease debugging.
        val synthVal = SyntheticValDef(ctx.freshName("phantomLift$" + tree.name + "$").toTermName, qual)
        val synthValRef = Ident(synthVal.symbol.termRef)
        transform(Block(List(synthVal), Select(synthValRef, tree.name)))

      case _ => tree
    }
  }

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (returnsPhantom(tree.tpe) || !tree.args.exists(arg => arg.tpe.isPhantom)) {
      tree
    } else {
      val synthVals = tree.args.map { arg =>
        SyntheticValDef(ctx.freshName("phantomLift$").toTermName, transformFollowingDeep(arg))
      }
      val newArgs = synthVals.map(synthVal => Ident(synthVal.symbol.termRef))
      Block(synthVals, cpy.Apply(tree)(tree.fun, newArgs))
    }
  }

}
