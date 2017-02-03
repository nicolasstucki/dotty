package dotty.tools.dotc.transform.phantom

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.DenotTransformers._
import dotty.tools.dotc.core.Phases._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

class PhantomAnyErasure extends MiniPhaseTransform with InfoTransformer {

  import tpd._

  override def phaseName: String = "phantomAnyErasure"

  /** List of names of phases that should precede this phase */
  override def runsAfter: Set[Class[_ <: Phase]] =
    Set(classOf[PhantomParamErasure], classOf[PhantomFunctionErasure])

  /** Check what the phase achieves, to be called at any point after it is finished. */
  override def checkPostCondition(tree: tpd.Tree)(implicit ctx: Context): Unit = {
    assert(!(tree.tpe =:= defn.PhantomAnyType), "PhantomAny should be erased in " + tree)
  }

  /* Tree transform */

  override def transformTypeTree(tree: TypeTree)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val newTypeTree = TypeTree(erasePhantomAnyType(tree.tpe))
    if (newTypeTree == tree) tree else newTypeTree
  }

  /* Symbol transform */

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = erasePhantomAnyType(tp)

  /* private methods */

  private def erasePhantomAnyType(tp: Type)(implicit ctx: Context): Type = {
    val erasePhantomAnyTypeMap = new DeepTypeMap {
      private val phantomAnyClass = defn.PhantomAnyClass
      override def apply(tp: Type): Type = tp match {
        case tp: TypeRef if tp.symbol eq phantomAnyClass => defn.ErasedPhantomAnyType
        case _ => mapOver(tp)
      }
    }
    erasePhantomAnyTypeMap(tp)
  }

}
