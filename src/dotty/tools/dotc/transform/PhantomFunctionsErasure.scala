package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.DenotTransformers._
import dotty.tools.dotc.core.Phases._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo, TreeTransform}

import scala.annotation.tailrec

class PhantomFunctionsErasure extends MiniPhaseTransform with InfoTransformer {
  import tpd._

  override def phaseName: String = "phantomFunctionsErasure"

  /** List of names of phases that should precede this phase */
  override def runsAfter: Set[Class[_ <: Phase]] =
    Set(classOf[InterceptedMethods], classOf[Splitter], classOf[ElimRepeated])

  /** Check what the phase achieves, to be called at any point after it is finished. */
  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    // TODO
  }

  /* Tree transform */

  override def transformTemplate(tree: Template)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val newParents = tree.parents.map {
      case parent: TypeTree if isPhantomFunctionType(parent.tpe) =>
        val newType = parent.tpe match {
          case tpe: RefinedType => RefinedType(erasedPhantomFunctionClass, tpe.refinedName, tpe.refinedInfo)
        }
        cpy.TypeTree(TypeTree(newType))(parent)
      case parent => parent
    }

    cpy.Template(tree)(parents = newParents)
  }

  override def transformTypeTree(tree: TypeTree)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (!isPhantomFunctionType(tree.tpe)) tree
    else cpy.TypeTree(TypeTree(erasedPhantomFunctionClass))(tree)
  }

  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (tree.name != nme.apply || !defn.isPhantomFunctionClass(tree.symbol.owner)) tree
    else Select(tree.qualifier, nme.apply)
  }

  /* Symbol transform */

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = tp match {
    case tp: ClassInfo =>
      val newParents = tp.classParents map {
        case parent: TypeRef if defn.isPhantomFunctionClass(parent.info.classSymbol) =>
          erasedPhantomFunctionClass

        case parent => parent
      }
      if (newParents == tp.parents) tp
      else ClassInfo(tp.prefix, tp.cls, newParents, tp.decls, tp.selfInfo)

    case tp: JavaMethodType => tp
    case tp: MethodType =>
      val erasedParamTypes = tp.paramTypes.map { tpe =>
        if (!isPhantomFunctionType(tpe)) tpe
        else erasedPhantomFunctionClass
      }

      if (erasedParamTypes == tp.paramTypes) {
        tp
      } else {
        println(tp.getClass)
        tp match {
          case tp: ImplicitMethodType => ImplicitMethodType(tp.paramNames, erasedParamTypes, tp.resultType)
          case tp => MethodType(tp.paramNames, erasedParamTypes, tp.resultType)
        }
      }

    case tp: RefinedType if isPhantomFunctionType(tp) =>
      RefinedType(erasedPhantomFunctionClass, tp.refinedName, tp.refinedInfo)

    case tp: ExprType if isPhantomFunctionType(tp) =>
      ExprType(erasedPhantomFunctionClass)

    case _ => tp
  }

  @tailrec private def isPhantomFunctionType(tpe: Type, arity: Int = -1)(implicit ctx: Context): Boolean = tpe match {
    case tpe: RefinedType => isPhantomFunctionType(tpe.parent, arity + 1)
    case _ => defn.isPhantomFunctionClass(tpe.classSymbol)
  }

  private def erasedPhantomFunctionClass(implicit ctx: Context): TypeRef = defn.FunctionType(0)

}
