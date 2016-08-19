package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

class PhobicChecks extends MiniPhaseTransform {
  import tpd._
  import Phantoms._

  override def phaseName: String = "phobicChecks"

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

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    for (ann <- getPhobicAnnotation(tree.symbol)) {
      ann.arguments.head match {
        case TypeApply(_, arg :: _) =>
          val disallowedTpe = arg.tpe
          if (disallowedTpe.typeSymbol.is(Flags.Trait))
            ctx.error("Disallow can not be used with traits.", arg.pos)
          else if (disallowedTpe.typeSymbol.isAbstractType)
            ctx.error("Disallow can not be used with abstract types.", arg.pos)
      }
    }

    tree
  }

  private def checkCapture(tree: Tree)(implicit ctx: Context): Unit = {
    val treeOwner = tree.symbol.owner
    ctx.owner.ownersIterator.takeWhile(_ != treeOwner).find { owner =>
      getPhobic(owner) match {
        case Some(ann) =>
          ann.arguments.head match {
            case TypeApply(_, arg :: _) => checkCapture(tree, owner, arg.tpe)
          }
          false
        case None => false
      }
    }
  }

  private def checkCapture(tree: Tree, owner: Symbol, disallowedTpe: Type)(implicit ctx: Context) = {
    val tpe = tree.tpe
    if (tpe.widen <:< disallowedTpe) {
      ctx.error(s"$owner can not capture ${tree.show} of type ${tpe.widen.show}.", tree.pos)
    } else if ((!disallowedTpe.derivesFrom(defn.PhantomAnyClass) && disallowedTpe <:< tpe.widen) || tpe.widen.typeSymbol.isTypeParam) {
      ctx.error(s"$owner can not capture ${tree.show} of type ${tpe.widen.show} because it could be of type ${disallowedTpe.show}.", tree.pos)
    }
  }

  private def getPhobic(sym: Symbol)(implicit ctx: Context): Option[Annotation] = {
    getPhobicAnnotation(sym).orElse {
      sym.allOverriddenSymbols.map(getPhobicAnnotation).collectFirst { case Some(ann) => ann }
    }
  }

  private def getPhobicAnnotation(sym: Symbol)(implicit ctx: Context): Option[Annotation] =
    sym.getAnnotation(defn.PhobicAnnot)
}
