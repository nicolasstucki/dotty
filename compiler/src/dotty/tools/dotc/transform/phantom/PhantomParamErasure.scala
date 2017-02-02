package dotty.tools.dotc.transform.phantom


import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.DenotTransformers._
import dotty.tools.dotc.core.Phases._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

import scala.annotation.tailrec

class PhantomParamErasure extends MiniPhaseTransform with InfoTransformer {
  import tpd._
  import PhantomParamErasure._

  override def phaseName: String = "phantomParamErasure"

  /** Check what the phase achieves, to be called at any point after it is finished. */
  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    def assertNotPhantom(tree: Tree): Unit =
      assert(!tree.tpe.isPhantom, "All phantom type values should be erased in " + tree)
    tree match {
      case Apply(_, args) if !returnsPhantom(tree.tpe.finalResultType) =>
        args.foreach(assertNotPhantom)
      case DefDef(_, _, vparamss, tpt, _) if !returnsPhantom(tpt.tpe) =>
        vparamss.foreach(_.foreach(assertNotPhantom))
      case _ =>
    }
  }

  /* Tree transform */

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (returnsPhantom(tree.tpe) || !tree.args.exists(_.tpe.isPhantom)) tree
    else cpy.Apply(tree)(tree.fun, tree.args.filter(!_.tpe.isPhantom))
  }

  override def transformDefDef(ddef: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (ddef.tpt.tpe.isPhantom || !ddef.vparamss.exists(_.exists(_.tpt.typeOpt.isPhantom))) ddef
    else cpy.DefDef(ddef)(vparamss = ddef.vparamss.map(_.filter(!_.tpt.typeOpt.isPhantom)))
  }

  /* Symbol transform */

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = erasedPhantomParameters(tp)

}

object PhantomParamErasure {

  def erasedPhantomParameters(tp: Type)(implicit ctx: Context): Type = {
    if (returnsPhantom(tp)) tp
    else erasedPhantomParametersImpl(tp)
  }

  @tailrec private def returnsPhantom(tp: Type)(implicit ctx: Context): Boolean = tp match {
    case tp: MethodicType => returnsPhantom(tp.resultType)
    case _                => tp.isPhantom
  }

  private def erasedPhantomParametersImpl(tp: Type)(implicit ctx: Context): Type = tp match {
    case tp: JavaMethodType => tp
    case tp: MethodType =>
      val erasedResultType = erasedPhantomParametersImpl(tp.resultType)
      val (erasedParamNames, erasedParamTypes) =
        tp.paramNames.zip(tp.paramTypes).filter(tup => !tup._2.isPhantom).unzip
      if (tp.resultType == erasedResultType && tp.paramNames == erasedParamNames && tp.paramTypes == erasedParamTypes) {
        tp
      } else {
        tp match {
          case _: ImplicitMethodType => ImplicitMethodType(erasedParamNames, erasedParamTypes, erasedResultType)
          case _                     => MethodType(erasedParamNames, erasedParamTypes, erasedResultType)
        }
      }

    case tp: PolyType => tp.derivedPolyType(tp.paramNames, tp.paramBounds, erasedPhantomParametersImpl(tp.resultType))
    case _            => tp
  }
}
