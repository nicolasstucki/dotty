package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

class PhantasmophobicChecks extends MiniPhaseTransform {
  import tpd._
  import Phantoms._

  override def phaseName: String = "phantasmophobicChecks"

  override def transformIdent(tree: Ident)(implicit ctx: Context, info: TransformerInfo): Tree = {
    checkCapture(tree)
    tree
  }

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    checkCapture(tree)
    tree
  }

  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo): Tree = {
    checkCapture(tree)
    tree
  }

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    checkCapture(tree)
    tree
  }

  private def checkCapture(tree: Tree)(implicit ctx: Context): Unit = {
    if (isPhantom(tree.tpe)) {
      val treeOwner = tree.symbol.owner
      shouldNotBeCapturedBy(treeOwner).foreach { owner =>
        val captured =
          if (tree.symbol.isConstructor) treeOwner.name + " constructor"
          else tree.symbol.name
        ctx.error(s"Cannot capture $captured in $owner.", tree.pos)
      }
    }
  }

  /** Return the symbol of the method that is capturing illegally */
  private def shouldNotBeCapturedBy(treeOwner: Symbol)(implicit ctx: Context): Option[Symbol] =
    ctx.owner.ownersIterator.takeWhile(_ != treeOwner).find(isPhantasmophobic)

  private def isPhantasmophobic(sym: Symbol)(implicit ctx: Context): Boolean =
    hasPhantasmophobicAnnotation(sym) || sym.allOverriddenSymbols.exists(hasPhantasmophobicAnnotation)

  private def hasPhantasmophobicAnnotation(sym: Symbol)(implicit ctx: Context): Boolean =
    sym.hasAnnotation(defn.PhantasmophobicAnnot)
}
