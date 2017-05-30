package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.transform.TreeTransforms._


/** A no-op transform to ensure that the compiled sources have no ??? */
class CheckCaptures extends MiniPhaseTransform { thisTransformer =>

  override def phaseName = "checkCaptures"

  override def transformApply(tree: tpd.Apply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = checked(tree)
  override def transformIdent(tree: tpd.Ident)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = checked(tree)
  override def transformSelect(tree: tpd.Select)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = checked(tree)

  private def checked(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = {
    lazy val top = tree.tpe.topType
    def cannotBeCaptured = top.normalizedPrefix.termSymbol.cannotBeCaptured
    def couldBeCapturedByNonCapturing(owner: Symbol) = !owner.is(Package) && owner != tree.symbol.owner
    lazy val cannotCapturedBy: Symbol =
      ctx.owner.ownersIterator.takeWhile(couldBeCapturedByNonCapturing).find(_.cannotCapture).getOrElse(NoSymbol)
    if (tree.symbol.exists && tree.symbol.owner != ctx.owner && cannotBeCaptured && cannotCapturedBy.exists)
      ctx.error(s"${tree.show} cannot be captured by ${cannotCapturedBy.showLocated}".stripMargin, tree.pos)
    tree
  }

}
