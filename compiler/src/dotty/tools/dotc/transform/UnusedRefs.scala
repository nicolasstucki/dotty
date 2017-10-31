package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Phases
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.MegaPhase.MiniPhase

/** This phase removes all references and calls to unused methods or vals
 *
 *   if `unused def f(x1,...,xn): T = ...`
 *   then `f(y1,...,yn)` --> `y1; ...; yn; (default value for T)`
 *
 *   if   `unused val x: T = ...` including parameters
 *   then `x` --> `(default value for T)`
 */
class UnusedRefs extends MiniPhase {
  import tpd._

  override def phaseName: String = "unusedRefs"

  override def runsAfterGroupsOf: Set[Class[_ <: Phases.Phase]] = Set(
    classOf[UnusedChecks]
  )

  /** Check what the phase achieves, to be called at any point after it is finished. */
  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = tree match {
    case _: Apply | _: TypeApply | _: RefTree => assert(!tree.symbol.is(Unused), tree)
    case _ =>
  }

  /* Tree transform */

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context): Tree = transformUnused(tree)
  override def transformApply(tree: Apply)(implicit ctx: Context): Tree = transformUnused(tree)
  override def transformIdent(tree: Ident)(implicit ctx: Context): Tree = transformUnused(tree)
  override def transformSelect(tree: Select)(implicit ctx: Context): Tree = transformUnused(tree)

  private def transformUnused(tree: Tree)(implicit ctx: Context): Tree = {
    if (!tree.symbol.is(Unused)) tree
    else {
      tree.tpe.widen match {
        case _: MethodOrPoly => tree // Do the transformation higher in the tree if needed
        case _ =>
          tree match {
            case _: RefTree | _: TypeApply => defaultValue(tree.tpe)
            case Apply(_ , args) =>
            def allArgs(t: Tree, acc: List[Tree]): List[Tree] = t match {
              case Apply(fun, args) => allArgs(fun, args ::: acc)
              case _ => acc
            }
            seq(allArgs(tree, Nil), defaultValue(tree.tpe))
          }
      }
    }
  }
}
