package dotty.tools.dotc
package interpreter

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core.Constants._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._

import scala.reflect.ClassTag
import java.net.URLClassLoader

import dotty.tools.dotc.util.Positions.Position

/** Tree interpreter that can interpret
 *   * Literal constants
 *   * Calls to static methods
 *   * New objects with explicit `new` keyword
 *   * Quoted code
 *
 *  The interpreter assumes that all calls in the trees are to code that was
 *  previously compiled and is present in the classpath of the current context.
 */
class Interpreter(implicit ctx: Context) {
  import tpd._

  private[this] val classLoader = {
    val urls = ctx.settings.classpath.value.split(':').map(cp => java.nio.file.Paths.get(cp).toUri.toURL)
    new URLClassLoader(urls, getClass.getClassLoader)
  }

  /** Returns the interpreted result of interpreting the code represented by the tree.
   *  Return Some of the result or None if some error happen during the interpretation.
   */
  def interpretTree[T](tree: Tree)(implicit ct: ClassTag[T]): Option[T] = {
    try {
      interpretTreeImpl(tree) match {
        case obj: T => Some(obj)
        case obj =>
          // TODO upgrade to a full type tag check or something similar
          ctx.error(s"Interpreted tree returned a result of an unexpected type. Expected ${ct.runtimeClass} but was ${obj.getClass}", tree.pos)
          None
      }
    } catch {
      case _: StopInterpretation => None
    }
  }

  /** Returns the interpreted result of interpreting the code represented by the tree.
   *  Returns the result of the interpreted tree.
   *
   *  If some error is encountered while interpreting a ctx.error is emited and a StopInterpretation is thrown.
   */
  private def interpretTreeImpl(tree: Tree): Object = { // TODO add environment
    try {
      tree match {
        case Apply(_, quote :: Nil) if tree.symbol eq defn.quoteMethod =>
          new RawExpr(quote)
        case TypeApply(_, quote :: Nil) if tree.symbol eq defn.typeQuoteMethod =>
          new RawType(quote)

        case Literal(Constant(c)) =>
          c.asInstanceOf[AnyRef]

        case Apply(fn, args) if fn.symbol.isConstructor =>
          val cls = fn.symbol.owner
          val clazz = classLoader.loadClass(cls.symbol.fullName.toString)
          val paramClasses = paramsSig(fn.symbol)
          val args1: List[Object] = args.map(arg => interpretTreeImpl(arg))
          clazz.getConstructor(paramClasses: _*).newInstance(args1: _*).asInstanceOf[Object]

        case Apply(fun, args) if fun.symbol.isStatic =>
          val clazz = classLoader.loadClass(fun.symbol.owner.companionModule.fullName.toString)
          val paramClasses = paramsSig(fun.symbol)
          val args1: List[Object] = args.map(arg => interpretTreeImpl(arg))
          val method = clazz.getMethod(fun.symbol.name.toString, paramClasses: _*)
          method.invoke(null, args1: _*)

        case tree: RefTree if tree.symbol.isStatic =>
          val clazz = classLoader.loadClass(tree.symbol.owner.companionModule.fullName.toString)
          val method = clazz.getMethod(tree.name.toString)
          method.invoke(null)

        // case tree: RefTree if tree.symbol.is(Module) => // TODO
        // case Block(stats, expr) => // TODO evaluate bindings add environment
        // case ValDef(_, _, rhs) =>  // TODO evaluate bindings add environment

        case Inlined(_, bindings, expansion) =>
          assert(bindings.isEmpty) // TODO evaluate bindings and add environment
          interpretTreeImpl(expansion)
        case _ =>
          throw new StopInterpretation(s"Could not interpret ${tree.show}", tree.pos)
      }
    } catch {
      case ex: NoSuchMethodException =>
        throw new StopInterpretation("Could not find interpreted method in classpath: " + ex.getMessage, tree.pos)
      case ex: ClassNotFoundException =>
        throw new StopInterpretation("Could not find interpreted class in classpath: " + ex.getMessage, tree.pos)
      case ex: RuntimeException =>
        ex.printStackTrace()
        throw new StopInterpretation("A runtime exception occurred while interpreting: " + ex.getMessage, tree.pos)
    }
  }

  /** List of classes of the parameters of the signature of `sym` */
  private def paramsSig(sym: Symbol): List[Class[_]] = {
    sym.signature.paramsSig.map { param =>
      val paramString = param.toString
      if (paramString == defn.BooleanClass.showFullName) classOf[Boolean]
      else if (paramString == defn.ByteClass.showFullName) classOf[Byte]
      else if (paramString == defn.CharClass.showFullName) classOf[Char]
      else if (paramString == defn.ShortClass.showFullName) classOf[Short]
      else if (paramString == defn.IntClass.showFullName) classOf[Int]
      else if (paramString == defn.LongClass.showFullName) classOf[Long]
      else if (paramString == defn.DoubleClass.showFullName) classOf[Float]
      else if (paramString == defn.DoubleClass.showFullName) classOf[Double]
      else classLoader.loadClass(paramString)
    }
  }

  /** Exception that stops interpretation if some issue is found */
  private class StopInterpretation(msg: String, pos: Position) extends Exception

}
