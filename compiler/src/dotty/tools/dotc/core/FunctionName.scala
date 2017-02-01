package dotty.tools.dotc.core

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Definitions._
import dotty.tools.dotc.core.NameOps._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._

trait FunctionName {
  def arity: Int

  def isImplicit: Boolean
  def isPhantom: Boolean = false
  def isImplicitPhantom: Boolean = isImplicit && isPhantom

  def withoutImplicit: FunctionName
  def withoutPhantoms: FunctionName = this

  def name: TypeName
  def fullName: TypeName = tpnme.scala_ ++ "." ++ name

  def isImplementedFunction(implicit ctx: Context): Boolean =
    FunctionName.isImplementedFunction(arity, isImplicit, isPhantom)

  def paramTypeBound(i: Int)(implicit ctx: Context): TypeBounds = TypeBounds.empty
  def paramTypeName(i: Int)(implicit ctx: Context): TypeName

  def isFunctionName: Boolean = arity != -1

  protected def scalaFunctionPrefix: TypeName = tpnme.scala_ ++ "$" ++ tpnme.Function
}

object FunctionName {

  def apply(sym: Symbol)(implicit ctx: Context): FunctionName = apply(defn.scalaClassName(sym))

  def apply(name: TypeName)(implicit ctx: Context): FunctionName = {
    val functionArity = name.functionArity
    lazy val implicitFunctionArity = name.implicitFunctionArity
    if (functionArity != -1) {
      new SimpleFunctionName(functionArity, isImplicit = false)
    } else if (implicitFunctionArity != -1) {
      new SimpleFunctionName(implicitFunctionArity, isImplicit = true)
    } else if (name.phantomFunctionArity != -1) {
      val phantomicity = new Phantomicity(name.toString.substring(tpnme.PhantomFunction.length))
      new PhantomFunction(phantomicity, isImplicit = false)
    } else if (name.implicitPhantomFunctionArity != -1) {
      val phantomicity = new Phantomicity(name.toString.substring(tpnme.ImplicitPhantomFunction.length))
      new PhantomFunction(phantomicity, isImplicit = true)
    } else new NotAFunctionName(name)
  }

  def apply(args: List[Type], isImplicit: Boolean)(implicit ctx: Context): FunctionName = {
    val argsPhantomicity = new Phantomicity(args.map(arg => if (arg.isPhantom) phantomicityOnChar else phantomicityOffChar).mkString)
    if (argsPhantomicity.hasPhantoms) new PhantomFunction(argsPhantomicity, isImplicit)
    else new SimpleFunctionName(argsPhantomicity.arity, isImplicit)
  }

  def apply(arity: Int, isImplicit: Boolean = false): FunctionName =
    new SimpleFunctionName(arity, isImplicit)

  def isImplementedFunction(arity: Int, isImplicit: Boolean, isPhantom: Boolean)(implicit ctx: Context): Boolean =
    !isPhantom && arity <= MaxImplementedFunctionArity && (!isImplicit || ctx.erasedTypes)

  private class NotAFunctionName(val name: TypeName) extends FunctionName {
    def arity: Int = -1
    def isImplicit: Boolean = false
    def withoutImplicit: FunctionName = this
    def paramTypeName(i: Int)(implicit ctx: Context): TypeName = tpnme.error
  }

  private class SimpleFunctionName(val arity: Int, val isImplicit: Boolean) extends FunctionName {
    assert(arity != -1)
    def withoutImplicit: FunctionName = new SimpleFunctionName(arity, false)
    def name: TypeName = (if (isImplicit) tpnme.ImplicitFunction else tpnme.Function) ++ arity.toString

    def paramTypeName(i: Int)(implicit ctx: Context): TypeName = {
      val paramName = if (i != arity) "T" + (i + 1) else "R"
      scalaFunctionPrefix ++ arity.toString ++ "$$" ++ paramName
    }
  }

  private class PhantomFunction(phantomicity: Phantomicity, val isImplicit: Boolean) extends FunctionName {

    private lazy val erasedPrefix: TypeName = scalaFunctionPrefix ++ phantomicity.erasedArity.toString
    private lazy val phantomPrefix: TypeName = tpnme.scala_ ++ "$" ++ name

    def arity: Int = phantomicity.arity

    override def isPhantom: Boolean = true

    def withoutImplicit: FunctionName = new PhantomFunction(phantomicity, false)
    override def withoutPhantoms: FunctionName = new SimpleFunctionName(phantomicity.erasedArity, isImplicit)

    def name: TypeName =
      (if (isImplicit) tpnme.ImplicitPhantomFunction else tpnme.PhantomFunction) ++ phantomicity.encodedString

    override def paramTypeBound(i: Int)(implicit ctx: Context): TypeBounds =
      if (isPhantomParam(i)) TypeBounds.emptyPhantom else TypeBounds.empty

    def paramTypeName(i: Int)(implicit ctx: Context): TypeName = {
      if (i == arity) erasedPrefix ++ "$$R"
      else if (!isPhantomParam(i)) erasedPrefix ++ "$$T" ++ erasedParamNum(i).toString
      else phantomPrefix ++ "$$P" ++ i.toString
    }

    private def isPhantomParam(i: Int): Boolean = 0 <= i && i < arity && phantomicity(i)
    private def erasedParamNum(i: Int): Int = (0 until i).count(j => !isPhantomParam(j)) + 1
  }

  private class Phantomicity(val encodedString: String) extends AnyVal {
    def hasPhantoms: Boolean = encodedString.contains(phantomicityOnChar.toString)
    def apply(i: Int): Boolean = encodedString.charAt(i) == phantomicityOnChar
    def arity: Int = encodedString.length
    def erasedArity: Int = encodedString.count(_ == phantomicityOffChar)
  }

  final val phantomicityOnChar = 'X'
  final val phantomicityOffChar = '0'
}
