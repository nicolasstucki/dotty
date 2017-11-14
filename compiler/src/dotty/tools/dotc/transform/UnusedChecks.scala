package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.transform.MegaPhase.MiniPhase

/** This phase checks that unused `def`s, `val`s and parameters are only used in valid place.
 *  It also checks that the `unused` keyword is only used on `def`, `val` and parameters lists
 *
 *  Unused values can be used:
 *    - As parameters to an unused function
 *    - In statement position
 *    - Anywhere in the RHS of an `unused def` or `unused val`
 */
class UnusedChecks extends MiniPhase {
  import tpd._

  override def phaseName: String = "unusedChecks"


  /* Tree transform */

  override def transformApply(tree: Apply)(implicit ctx: Context): tree.type = {
    if (!isUnusedContext && !tree.symbol.is(Unused) && !tree.fun.tpe.widen.isUnusedMethod)
      tree.args.foreach(arg => checked(arg)(arg, "Cannot use `unused` value in a context that is not `unused`"))
    tree
  }

  override def transformDefDef(tree: DefDef)(implicit ctx: Context): tree.type = {
    checkedValOrDefDefRHS(tree)
  }

  override def transformValDef(tree: ValDef)(implicit ctx: Context): tree.type = {
    checkedValOrDefDefRHS(tree)
  }

  override def transformSelect(tree: Select)(implicit ctx: Context): tree.type =
    checked(tree)(tree.qualifier, "Cannot use `unused` value in a context that is not `unused`")

  override def transformMatch(tree: Match)(implicit ctx: Context): tree.type =
    checked(tree)(tree.selector, "Cannot match on `unused` value in a context that is not `unused`")

  override def transformIf(tree: If)(implicit ctx: Context): tree.type =
    checked(tree)(tree.cond, "Cannot use `unused` condition in a context that is not `unused`")

  override def transformReturn(tree: Return)(implicit ctx: Context): tree.type =
    checked(tree)(tree.expr, "Cannot return `unused` condition in a context that is not `unused`")

  override def transformAssign(tree: Assign)(implicit ctx: Context): tree.type =
    checked(tree)(tree.rhs, "Cannot assign `unused` condition in a context that is not `unused`")


  /* private methods */

  /** Check that the expression is not a reference to an unused value */
  private def checkedValOrDefDefRHS(tree: ValOrDefDef)(implicit ctx: Context): tree.type =
    if (tree.symbol.is(Unused)) tree
    else checked(tree)(tree.rhs, "Cannot return `unused` value in a def without `unused`")

  private def checked(tree0: Tree)(tree1: Tree, msg: String)(implicit ctx: Context): tree0.type = {
    def rec(t: Tree): Unit = t match {
      case Block(_, expr) => rec(expr)
      case If(_, thenp, elsep) =>
        rec(thenp)
        rec(elsep)
      case Match(_, cases) =>
        cases.foreach(c => rec(c.body))
      case Try(expr, cases, _) =>
        rec(expr)
        cases.foreach(c => rec(c.body))
      case _ if t.symbol.is(Unused) =>
        ctx.error(msg, t.pos)
      case _ =>
    }
    if (!isUnusedContext)
      rec(tree1)
    tree0
  }

  private def isUnusedContext(implicit ctx: Context): Boolean =
    ctx.owner.ownersIterator.exists(_.is(Unused)) // TODO make context mode?
}
