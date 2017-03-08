package dotty.tools.dotc
package transform

import core._
import Phases.Phase
import Contexts.Context
import Symbols._
import TreeTransforms._
import Flags._

/** Erases all ValDefs of phantom type,
 *  except for `lazy val` as the field are not yet generated.
 *
 *    <accessor> <mods> def x(): ErasedPhantom = e
 *      --> <accessor> <mods> def x(): ErasedPhantom = null
 *
 *
 *    x = e  --> e  where type of x is ErasedPhantom
 *
 *    Filed in class
 *    <private> <mods> val x: ErasedPhantom --> EmptyTree
 *
 */
 class PhantomFields extends MiniPhaseTransform { thisTransform =>
  import ast.tpd._
  import Erasure.Phantom._

  override def phaseName = "phantomFields"

  override def runsAfterGroupsOf: Set[Class[_ <: Phase]] = Set(classOf[Constructors])

  override def transformValDef(tree: ValDef)(implicit ctx: Context, info: TransformerInfo): Tree =
    if (tree.symbol.owner.isClass && !tree.symbol.is(Lazy) && isErasedPhantom(tree.tpt.tpe)) EmptyTree
    else tree

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree =
    if (!(tree.symbol.isGetter && !tree.symbol.is(Lazy) && isErasedPhantom(tree.tpt.tpe))) tree
    else cpy.DefDef(tree)(rhs = erasedPhantomTree)

  override def transformAssign(tree: Assign)(implicit ctx: Context, info: TransformerInfo): Tree =
    if (!tree.lhs.symbol.is(Lazy) && isErasedPhantom(tree.rhs.tpe)) tree.rhs
    else tree

}
