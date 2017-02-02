package dotty.tools.dotc.transform.phantom

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.phantom.PhantomParamErasure._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

import scala.annotation.tailrec


class PhantomChecks extends MiniPhaseTransform {
  import tpd._

  override def phaseName: String = "phantomChecks"

  private var checkedDisjointLattices = false

  /** Check what the phase achieves, to be called at any point after it is finished. */
  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    if (!checkedDisjointLattices) {
      assert(defn.NothingType <:< defn.AnyType, "Nothing should not be a subtype of Any")
      assert(defn.PhantomNothingType <:< defn.PhantomAnyType, "PhantomNothing should not be a subtype of PhantomAny")
      assert(!(defn.PhantomAnyType <:< defn.AnyType), "PhantomAny should not be a subtype of Any")
      assert(!(defn.PhantomNothingType <:< defn.AnyType), "PhantomNothing should not be a subtype of Any")
      assert(!(defn.NothingType <:< defn.PhantomAnyType), "Nothing should not be a subtype of PhantomAny")
      assert(!(defn.NothingType <:< defn.PhantomNothingType), "Nothing should not be a subtype of PhantomNothing")
      checkedDisjointLattices = true
    }

    def check(tp: Type): Unit = {
      assert(!(tree.tpe.isPhantom && tree.tpe.derivesFrom(defn.AnyClass)),
          "Types can't be subtype of both PhantomAny and Any: $tp")
    }

    tree match {
      case tree: ValOrDefDef => check(tree.tpt.tpe)
      case tree: TypeDef     => check(tree.rhs.tpe)
      case _                 => check(tree.tpe)
    }
  }

  override def transformStats(trees: List[Tree])(implicit ctx: Context, info: TransformerInfo): List[Tree] = {
    ctx.owner match {
      case cls: ClassSymbol if cls.classDenot.classSymbol.isPhantomClass =>
        trees.foreach {
          case _: TypeDef     => // OK
          case _: ValOrDefDef => // checked in transformValDef and transformDefDef
          case tree =>
            ctx.error("Phantom classes can not have expressions in statement position.", tree.pos)
        }

      case _ =>
        trees.foreach {
          case _: TypeDef     => // OK
          case _: ValOrDefDef => // OK
          case Apply(Trees.Select(_: This, name), _) if name == nme.CONSTRUCTOR =>
            // avoid double error in phantom class secondary constructor
          case tree if tree.tpe.isPhantom =>
            ctx.error("Expression returning a phantom type can not be in statement position.", tree.pos)
          case _ => // OK
        }
    }

    trees
  }

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val sym = tree.symbol
    if (sym.isClass && sym.owner.isClass && !sym.owner.is(Package) && sym.owner.isClass) {
      if (sym.isPhantomClass && !(sym.owner.isPhantomClass && sym.owner.is(Module)))
        ctx.error(s"Phantom classes can only be contain phantom objects.", tree.pos)
      else if (!sym.isPhantomClass && sym.owner.isPhantomClass)
        ctx.error("Non phantom classes cannot be contain phantom classes.", tree.pos)
    }
    tree
  }

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val sym = tree.symbol
    if (!inPhantomClass(sym)) {
      if (tree.tpt.tpe.isPhantom) {
        ctx.error("Classes cannot have methods that return a phantom value. Define them in a phantom object and import its content.", tree.pos)
      } else if (sym.owner.isClass) {
        val erased = erasedPhantomParameters(sym.info)
        for (decl <- sym.owner.info.decls) {
          if ((decl ne sym) && decl.is(Method) && decl.name == sym.name && erased == erasedPhantomParameters(decl.info))
            ctx.error(em"After phantom erasure $sym${sym.info} and $decl${decl.info} will have the same signature: $sym$erased", tree.pos)
        }
      }
    } else if (!sym.isConstructor) {
      if (!sym.owner.is(Module))
        ctx.error("Can not define methods in phantom class.", tree.pos)
      else if (!tree.tpt.tpe.isPhantom)
        ctx.error("Phantom modules can only have methods that return a phantom value.", tree.pos)
    } else if (!sym.isPrimaryConstructor) {
      ctx.error("Can not have secondary constructors in phantom class.", tree.pos)
    } else if (tree.vparamss.exists(_.nonEmpty)) {
      ctx.error("Can not have parameters in constructor of a phantom class.", tree.pos)
    }

    if (tree.tpt.tpe.isPhantom) {
      @tailrec def checkForStats(t: Tree): Unit = t match {
        case Block(stats, expr) =>
          stats match {
            case TypeDef(name, _) :: Nil if name == tpnme.ANON_CLASS => // OK
            case _ =>
              for (stat <- stats)
                ctx.error("Methods returning phantom values can not have statements.", stat.pos)
          }
          checkForStats(expr)
        case _ => // Ok
      }
      checkForStats(tree.rhs)
    }

    tree
  }

  override def transformValDef(tree: tpd.ValDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val sym = tree.symbol
    def keyword =
      if (sym.is(Mutable)) "var"
      else if (sym.is(Lazy)) "lazy val"
      else "val"
    if (inPhantomClass(sym)) {
      if (!sym.is(ParamAccessor) && !sym.is(Module))
        ctx.error(s"Phantom classes can not have '$keyword' fields.", tree.pos)
    } else if (!sym.is(Param) && !sym.is(ParamAccessor) && !sym.is(Module) && tree.tpt.tpe.isPhantom) {
      ctx.error(s"Can not define '$keyword' fields with phantom values.", tree.pos)
    }

    tree
  }

  override def transformClosure(tree: Closure)(implicit ctx: Context, info: TransformerInfo): Tree = {
    tree.tpe match {
      case tp: RefinedType if tp.refinedInfo.isPhantom && isPhantomFunctionType(tp) =>
        ctx.error(s"Closures can not return a phantom value.", tree.pos)
      case _ =>
    }
    tree
  }

  private def inPhantomClass(sym: Symbol)(implicit ctx: Context): Boolean = sym.owner match {
    case cls: ClassSymbol => cls.classSymbol.derivesFrom(defn.PhantomAnyClass)
    case _                => false
  }

  //TODO duplicated in PhantomFunctionErasure
  @tailrec private def isPhantomFunctionType(tpe: Type)(implicit ctx: Context): Boolean = tpe match {
    case tpe: RefinedType => isPhantomFunctionType(tpe.parent)
    case _                => defn.isPhantomFunctionClass(tpe.classSymbol)
  }

}
