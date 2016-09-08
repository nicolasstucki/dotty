package dotty.tools.dotc.core

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types._

/** Suffix encoding M of `PhantomFunctionM` and `ImplicitPhantomFunctionM`
 *
 *  `FunctionN` and `ImplicitFunctionN` can also be encoded as
 *  `PhantomFunctionM` and `ImplicitPhantomFunctionM` where `M` has `N + 1` `O`s
 */
class Phantomicity private (val encodedString: String) extends AnyVal {
  import Phantomicity._

  /** If parameter i is a phantom parameter */
  def apply(i: Int): Boolean = encodedString.charAt(i) == phantomTypeChar

  /** If has phantom parameters or return type */
  def hasPhantoms: Boolean = encodedString.exists(_ == phantomTypeChar)

  /** If all parameters and return type are phantoms */
  def allPhantoms: Boolean = encodedString.forall(_ == phantomTypeChar)

  /** If the return type is a phantom */
  def returnsPhantom: Boolean = encodedString.last == phantomTypeChar

  /** Arity of this function. Including phantom parameters. */
  def arity: Int = if (isValid) encodedString.length - 1 else -1

  /** Erased arity of this function. Without phantom parameters. */
  def erasedArity: Int =
    if (isValid) parameters.count(_ == normalTypeChar)
    else -1


  /** Is a valid phantomicity */
  def isValid: Boolean = encodedString ne invalid.encodedString

  private def parameters = encodedString.substring(0, encodedString.length - 1)
}

object Phantomicity {
  private val phantomTypeChar = 'X'
  private val normalTypeChar = 'O'

  lazy val invalid: Phantomicity = new Phantomicity("invalid")

  def apply(types: List[Type])(implicit ctx: Context): Phantomicity = {
    def typeToChar(tp: Type) = if (tp.isPhantom) phantomTypeChar else normalTypeChar
    new Phantomicity(types.iterator.map(typeToChar).mkString)
  }

  def from(str: String): Phantomicity = {
    if (str.exists(c => c != phantomTypeChar && c != normalTypeChar)) invalid
    else new Phantomicity(str)
  }

  def noPhantoms(arity: Int): Phantomicity =
    new Phantomicity(Iterator.fill(arity + 1)(normalTypeChar).mkString)

}
