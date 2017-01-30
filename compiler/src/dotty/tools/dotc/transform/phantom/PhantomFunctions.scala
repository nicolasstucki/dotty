package dotty.tools.dotc.transform.phantom

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.DenotTransformers._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.NameOps._
import dotty.tools.dotc.core.Phases._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

import scala.annotation.tailrec

/** Rewrite functions that took phantom parameters into their correspondent FunctionN */
class PhantomFunctions extends MiniPhaseTransform with InfoTransformer {
  import tpd._

  override def phaseName: String = "phantomFunctions"

  /** List of names of phases that should precede this phase */
  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[PhantomParamErasure])

  /** Check what the phase achieves, to be called at any point after it is finished. */
  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    def assertNotPhantomFunction(tpe: Type): Unit =
      assert(!isPhantomFunctionType(tpe), "All phantom functions should be erased in " + tree)

    tree match {
      case _: TypeTree       =>
      case _: TypeDef        =>
      case tree: ValOrDefDef => assertNotPhantomFunction(tree.tpt.typeOpt)
      case _                 => assertNotPhantomFunction(tree.tpe)
    }
  }

  /* Tree transform */

  override def transformTemplate(tree: Template)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val newParents = tree.parents.map {
      case parent: TypeTree if isPhantomFunctionType(parent.tpe) =>
        TypeTree(defn.FunctionType(countNonPhantomParametersOfParent(ctx.owner.asClass, parent.tpe) - 1))
      case parent => parent
    }
    cpy.Template(tree)(parents = newParents)
  }

  override def transformTypeTree(tree: TypeTree)(implicit ctx: Context, info: TransformerInfo): Tree = {
    tree.tpe match {
      case tp: RefinedType if isPhantomFunctionType(tp) =>
        TypeTree(eraseFunctionWithPhantom(tp))
      case _ =>  tree
    }
  }

  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (tree.name != nme.apply || !isSomePhantomFunction(tree.symbol.owner)) tree
    else tree.qualifier.select(nme.apply)
  }

  /* Symbol transform */

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = eraseFunctionWithPhantom(tp)

  /* private methods */

  @tailrec private def isPhantomFunctionType(tpe: Type)(implicit ctx: Context): Boolean = tpe match {
    case tpe: RefinedType => isPhantomFunctionType(tpe.parent)
    case _                => isSomePhantomFunction(tpe.classSymbol)
  }

  private def eraseFunctionWithPhantom(tpe: Type)(implicit ctx: Context): Type = {
    new DeepTypeMap {
      override def apply(tp: Type): Type = tp match {
        case RefinedType(parent, refinedName, _) if isFunctionWithPhantomsTypeParam(refinedName) =>
          apply(parent)

        case tp: TypeRef if isSomePhantomFunction(tp.classSymbol) =>
          val erasedArity = tp.name.asTypeName.functionWithPhantomsErasedArity
          val isImplicit = defn.isImplicitPhantomFunctionClass(tp.classSymbol)
          defn.FunctionType(erasedArity, isImplicit)

        case tp: ClassInfo if tp.parents.exists(p => isSomePhantomFunction(p.info.classSymbol)) =>
          val newParents = tp.classParents map {
            case parent: TypeRef if isSomePhantomFunction(parent.info.classSymbol) =>
              val arity = countNonPhantomParametersOfParent(tp.classSymbol.asClass, parent) - 1
              getErasedFunctionType(arity, parent.info.classSymbol)
            case parent => parent
          }

          val newDecls = tp.decls.filteredScope { decl => !decl.isType || !isFunctionWithPhantomsTypeParam(decl.name) }

          ClassInfo(tp.prefix, tp.cls, newParents, newDecls, tp.selfInfo)

        case tp => mapOver(tp)
      }
    }.apply(tpe)
  }

  private def countNonPhantomParametersOfParent(cls: ClassSymbol, parent: Type)(implicit ctx: Context): Int =
    parent.typeParams.count(!_.paramRef.typeSymbol.overriddenSymbol(cls).paramBounds.isPhantom)

  private def isFunctionWithPhantomsTypeParam(name: Name): Boolean =
    name.startsWith(tpnme.scala_ ++ "$" ++ tpnme.FunctionWithPhantoms)

  private def isSomePhantomFunction(sym: Symbol)(implicit ctx: Context): Boolean =
    defn.isPhantomFunctionClass(sym) || defn.isImplicitPhantomFunctionClass(sym)

  private def getErasedFunctionType(arity: Int, from: Symbol)(implicit ctx: Context): TypeRef =
    defn.FunctionType(arity, defn.isImplicitPhantomFunctionClass(from))

}
