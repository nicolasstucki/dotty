package dotty.tools.dotc.transform.phantom

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.DenotTransformers._
import dotty.tools.dotc.core.Phases._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

class PhantomTypeErasure extends MiniPhaseTransform with InfoTransformer {

  import tpd._

  override def phaseName: String = "phantomTypeErasure"

  /** List of names of phases that should precede this phase */
  override def runsAfter: Set[Class[_ <: Phase]] =
    Set(classOf[PhantomParamErasure], classOf[PhantomFunctions], classOf[PhantomDeclErasure])

  /** Check what the phase achieves, to be called at any point after it is finished. */
  override def checkPostCondition(tree: tpd.Tree)(implicit ctx: Context): Unit = {
    if (!tree.isInstanceOf[TypeTree]) // TODO erase type parameters and remove guard
      assert(!tree.tpe.isPhantom, "All phantom types should be erased in " + tree)
  }

  /* Tree transform */

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (tree.tpe.isPhantom) EmptyTree
    else tree
  }

  /*
  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val erasedArgs = tree.args.filter(!_.tpe.isPhantom)
    if (tree.args.lengthCompare(erasedArgs.length) == 0) tree
    else if (erasedArgs.isEmpty) tree.fun
    else TypeApply(tree.fun, tree.args.filter(!_.tpe.isPhantom))
  }
  */

  /* Symbol transform */

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = tp match {
    /*
    case tp: PolyType if tp.paramBounds.exists(_.isPhantom) =>
      val (newBounds, newNamesAndVariances) = tp.paramBounds.zip(tp.paramNames.zip(tp.variances)).filter(!_._1.isPhantom).unzip
      val (newNames, newVariances) = newNamesAndVariances.unzip

      def erasedResType(polyType: PolyType): Type = polyType.resType match {
        case mt: MethodType =>
          MethodType(mt.paramNames, mt.paramTypes, erasePhantomRefinements(mt.resultType))
        case resTp => resTp
      }

      if (newBounds.isEmpty) erasedResType(tp)
      else PolyType(newNames, newVariances)(polyType => newBounds, erasedResType)
      */
    case _ => tp
  }

  /* private methods */

  private def erasePhantomRefinements(tp: Type)(implicit ctx: Context): Type = tp match {
    case RefinedType(parent, refinedName, refinedInfo)  =>
      if (refinedInfo.isPhantom) erasePhantomRefinements(parent)
      else RefinedType(erasePhantomRefinements(parent), refinedName, erasePhantomRefinements(refinedInfo))
    case _ => tp
  }

}
