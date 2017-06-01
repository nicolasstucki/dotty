package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types.TypeRef
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

/** Propagates the @CannotCapture from the closure to the anonymous method */
class PropagateCannotCapture extends MiniPhaseTransform {

  def phaseName = "propagateCannotCapture"

  override def transformClosure(tree: tpd.Closure)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    tree.tpe.member(nme.apply).symbol.getAnnotation(defn.CannotCaptureAnnot) match {
      case Some(annotation) => tree.meth.symbol.addAnnotation(annotation)
      case None =>
    }
    tree
  }

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree =
    propagateAnnotation(tree)

  override def transformValDef(tree: tpd.ValDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree =
    propagateAnnotation(tree)

  private def propagateAnnotation(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = {
    def overriddenIn(parent: TypeRef): Symbol = tree.symbol.overriddenSymbol(parent.symbol.asClass)

    def inferredAnnotation(sym: Symbol): Unit = {
      // TODO: should we warn?
       ctx.warning(s"Should have @CannotCapture annotation because ${sym.showLocated} has the annotation.", tree.pos)
      tree.symbol.addAnnotation(Annotation(defn.CannotCaptureAnnot))
    }

    if (tree.symbol.exists && tree.symbol.owner.isClass && !tree.symbol.cannotCapture) {
      val parents = tree.symbol.owner.asClass.classParents
      parents.iterator.map(overriddenIn).find(_.cannotCapture).foreach(inferredAnnotation)
    }
    tree
  }
}
