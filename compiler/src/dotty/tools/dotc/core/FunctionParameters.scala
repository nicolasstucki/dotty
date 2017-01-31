package dotty.tools.dotc.core

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Definitions._
import dotty.tools.dotc.core.NameOps._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._

trait FunctionParameters {
  def arity: Int
  def erasedArity: Int

  def isImplicit: Boolean
  def withoutImplicit: FunctionParameters

  def hasPhantoms: Boolean
  def isPhantomParam(i: Int): Boolean

  def name: TypeName
  def fullName: TypeName = tpnme.scala_ ++ "." ++ name

  def functionClass(implicit ctx: Context): Symbol = defn.FunctionClass(this)
  def functionType(implicit ctx: Context): TypeRef = defn.FunctionType(this)

  def isImplementedFunction(implicit ctx: Context): Boolean

  def paramTypeBound(i: Int)(implicit ctx: Context): TypeBounds
  def paramTypeName(i: Int)(implicit ctx: Context): TypeName

  protected def scalaFunctionPrefix: TypeName = tpnme.scala_ ++ "$" ++ tpnme.Function
}

object FunctionParameters {

  def apply(name: Name)(implicit ctx: Context): FunctionParameters = {
    val isImplicitFunctionWithPhantoms = name.startsWith(tpnme.ImplicitFunctionWithPhantoms)
    if (isImplicitFunctionWithPhantoms || name.startsWith(tpnme.FunctionWithPhantoms)) {
      new FunctionParametersWithPhantoms(phantomicity(name), isImplicitFunctionWithPhantoms)
    } else {
      val arity = name.anyFunctionArity
      assert(arity != -1)
      apply(arity, name.startsWith(tpnme.ImplicitFunction))
    }

  }

  def apply(args: List[Type], isImplicit: Boolean)(implicit ctx: Context): FunctionParameters = {
    val argsPhantomicity = phantomicity(args)
    if (argsPhantomicity.hasPhantoms) new FunctionParametersWithPhantoms(argsPhantomicity, isImplicit)
    else apply(argsPhantomicity.arity, isImplicit)
  }

  def apply(arity: Int, isImplicit: Boolean)(implicit ctx: Context): FunctionParameters =
    new FunctionParametersWithoutPhantoms(arity, isImplicit)

  private class FunctionParametersWithoutPhantoms(val arity: Int, val isImplicit: Boolean) extends FunctionParameters {
    def erasedArity: Int = arity

    def withoutImplicit: FunctionParameters = new FunctionParametersWithoutPhantoms(arity, false)

    def hasPhantoms: Boolean = false
    def isPhantomParam(i: Int): Boolean = false

    def name: TypeName = (if (isImplicit) tpnme.ImplicitFunction else tpnme.Function) ++ arity.toString

    def isImplementedFunction(implicit ctx: Context): Boolean =
      arity <= MaxImplementedFunctionArity && (!isImplicit || ctx.erasedTypes)

    def paramTypeBound(i: Int)(implicit ctx: Context): TypeBounds = TypeBounds.empty

    def paramTypeName(i: Int)(implicit ctx: Context): TypeName = {
      val paramName = if (i != arity) "T" + (i + 1) else "R"
      scalaFunctionPrefix ++ arity.toString ++ "$$" ++ paramName
    }
  }

  private class FunctionParametersWithPhantoms(phantomicity: Phantomicity, val isImplicit: Boolean) extends FunctionParameters {

    private lazy val erasedPrefix: TypeName = scalaFunctionPrefix ++ erasedArity.toString

    def arity: Int = phantomicity.arity
    def erasedArity: Int = phantomicity.erasedArity

    def hasPhantoms: Boolean = true

    def isPhantomParam(i: Int): Boolean = 0 <= i && i < arity && phantomicity(i)

    def withoutImplicit: FunctionParameters = new FunctionParametersWithPhantoms(phantomicity, false)

    def name: TypeName = {
      val prefix = if (isImplicit) tpnme.ImplicitFunctionWithPhantoms else tpnme.FunctionWithPhantoms
      prefix ++ arity.toString ++ "_" ++ phantomicity.encodedString
    }

    def isImplementedFunction(implicit ctx: Context): Boolean = false

    def paramTypeBound(i: Int)(implicit ctx: Context): TypeBounds =
      if (isPhantomParam(i)) TypeBounds.emptyPhantom else TypeBounds.empty

    def paramTypeName(i: Int)(implicit ctx: Context): TypeName = {
      if (i == arity) erasedPrefix ++ "$$R"
      else if (!isPhantomParam(i)) erasedPrefix ++ "$$T" ++ erasedParamNum(i).toString
      else tpnme.scala_ ++ "$" ++ tpnme.FunctionWithPhantoms ++ arity.toString ++ "$$P" ++ i.toString
    }

    private def erasedParamNum(i: Int): Int = (0 until i).count(j => !isPhantomParam(j)) + 1
  }

  def phantomicity(name: Name): Phantomicity =
    new Phantomicity(name.toString.substring(name.toString.indexOf('_') + 1))

  def phantomicity(args: List[Type])(implicit ctx: Context): Phantomicity =
    new Phantomicity(args.map(arg => if (arg.isPhantom) '1' else '0').mkString)

  class Phantomicity(val encodedString: String) extends AnyVal {
    def hasPhantoms: Boolean = encodedString.contains("1")
    def apply(i: Int): Boolean = encodedString.charAt(i) == '1'
    def arity: Int = encodedString.length
    def erasedArity: Int = encodedString.count(_ == '0')
  }
}
