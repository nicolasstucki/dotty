package dotty.tools.dotc.transform.linker

import dotty.tools.dotc.FromTasty.TASTYCompilationUnit
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.{Flags, TypeErasure}
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.transform.SymUtils._
import dotty.tools.dotc.transform.TreeTransforms._
import dotty.tools.dotc.transform.linker.callgraph.OuterTargs
import dotty.tools.dotc.transform.linker.summaries._
import dotty.tools.dotc.transform.linker.types.{ClosureType, PreciseType}
import dotty.tools.dotc.typer.Applications._

import scala.annotation.tailrec
import scala.collection.mutable

class CollectSummaries extends MiniPhase { thisTransform =>
  import tpd._

  /** the following two members override abstract members in Transform */
  val phaseName: String = "summaries"

  val treeTransform: TreeTransform = new Collect

  private var methodSums = Map.empty[Symbol, MethodSummary]
  // private var noSummaryAvailable = Set[Symbol]()

   /*
  def getSummary(d: Symbol)(implicit ctx: Context): Option[MethodSummary] = {
    if (noSummaryAvailable(d)) None
    else methodSums.find(_.methodDef == d).orElse {
      val nw = retrieveSummary(d)
      methodSums = nw ::: methodSums
      nw.headOption
    }
  }

  private def retrieveSummary(claz: Symbol)(implicit ctx: Context): List[MethodSummary] = {
    val topDenot = claz.topLevelClass.denot.asSymDenotation
    topDenot match {
      case clsd: ClassDenotation =>
        clsd.initInfo match {
          case info: ClassfileLoader =>
            info.load(clsd) match {
              case Some(unpickler: DottyUnpickler) =>
                class STreeUnpickler(reader: TastyReader, tastyName: TastyName.Table) extends TreeUnpickler(reader, tastyName) {

                  roots = Set.empty

                  def getStartReader: Option[TreeReader] = {
                    val st = new TreeReader(reader)
                    st.skipToplevel()(ctx.addMode(Mode.AllowDependentFunctions))

                    while (true) {
                      while (reader.nextByte != TastyFormat.VALDEF && !reader.isAtEnd) st.skipTree()
                      if (reader.isAtEnd) return None // no section here
                      val tag = reader.readByte()
                      val end = reader.readEnd()
                      val name = st.readName()
                      if (name.toString == sectionName + unpickler.unpickler.uuid) return Some(st.forkAt(end))
                      st.skipTree() // skip type
                      st.skipTree() // skip rhs
                    }

                    None
                  }

               }
                class STreeSectionUnpickler extends TreeSectionUnpickler {
                  override def unpickle(reader: TastyReader, tastyName: Table): STreeUnpickler = {
                      new STreeUnpickler(reader, tastyName)
                  }
                }

                val tastySection = unpickler.unpickler.unpickle(new STreeSectionUnpickler).get
                val treeReader = tastySection.asInstanceOf[STreeUnpickler].getStartReader.get

                val unp = new SectionUnpickler[List[MethodSummary]](sectionName) {
                  def unpickle(reader: TastyReader, tastyName: Table): List[MethodSummary] = {
                    def readSymbolRef = {
                      val s = treeReader.readType()
                      s.termSymbol.orElse(s.typeSymbol).orElse(s.classSymbol)
                    }

                    def readType = treeReader.readType()

                    def readMS: MethodSummary = {
                      val sym = readSymbolRef
                      val methodsSz = reader.readInt()

                      val methodsCalled = new mutable.HashMap[Type, List[CallInfo]]()

                      for(_ <- 0 until methodsSz) {
                        val reciever = readType
                        val listSz = reader.readInt()

                        def readCallInfo: CallInfo = {
                          val t = readType
                          val targsSz = reader.readByte()
                          val targs = for(_ <- 0 until targsSz) yield readType
                          val argsSz = reader.readByte()
                          val argsPassed = for(_ <- 0 until argsSz) yield readSymbolRef
                          new CallInfo(t, targs.toList, argsPassed.toList)
                        }

                        val calls = for(_ <- 0 until listSz) yield readCallInfo
                        methodsCalled(reciever) = calls.toList
                      }

                      val accessedModulesSz = reader.readInt()

                      val accesedModules = for(_ <- 0 until accessedModulesSz) yield readSymbolRef

                      val argumentReturned = reader.readByte()

                      val bitsExtected =
                        sym.info.widenDealias.asInstanceOf[MethodType].paramTypess.foldLeft(0)(_+_.size) + 2 // this and thisAccessed
                      val bytesExpected = bitsExtected / 8 + (if(bitsExtected % 8 > 0) 1 else 0)
                      val bytes = reader.readBytes(bytesExpected)
                      val (thisAccessed :: argumentStoredToHeap) = bytes.toList.flatMap{ bt =>
                        List((bt & 1)  != 0, (bt & 2)  != 0, (bt & 4)  != 0, (bt & 8)   != 0,
                             (bt & 16) != 0, (bt & 32) != 0, (bt & 64) != 0, (bt & 128) != 0)
                      }

                      new MethodSummary(sym, thisAccessed, methodsCalled, accesedModules.toList, argumentReturned.toByte, argumentStoredToHeap.take(bitsExtected - 1))
                    }

                    val version = reader.readInt()

                    val methodsSz = reader.readInt()

                    val methods = for(_ <- 0 until methodsSz) yield readMS

                    methods.toList
                  }
                }
                unpickler.unpickler.unpickle(unp).getOrElse(Nil)
              case _ => Nil
            }
        }
    }
  }  */

  override def run(implicit ctx: Context): Unit = {
    if (CollectSummaries.isPhaseRequired)
      super.run
  }

  def methodSummaries: Map[Symbol, MethodSummary] = methodSums

  class Collect extends TreeTransform {
    def phase: CollectSummaries = thisTransform

    private var methodSummaries: Map[Symbol, MethodSummary] = Map.empty
    private var methodSummaryStack: mutable.Stack[MethodSummaryBuilder] = mutable.Stack()
    private var curMethodSummary: MethodSummaryBuilder = _

    override def prepareForUnit(tree: tpd.Tree)(implicit ctx: Context): TreeTransform = {
      if (ctx.compilationUnit.isInstanceOf[TASTYCompilationUnit])
        NoTransform // will retrieve them lazily
      else this
    }

    override def prepareForDefDef(tree: tpd.DefDef)(implicit ctx: Context): TreeTransform = {
      val sym = tree.symbol
      if (!sym.is(Label) && !sym.isPrimaryConstructor) {
        methodSummaryStack.push(curMethodSummary)
        val args = tree.vparamss.flatten.map(_.symbol) // outer param for constructors
        val argumentStoredToHeap = (0 to args.length).map(_ => true).toList
        curMethodSummary = new MethodSummaryBuilder(sym, argumentStoredToHeap)
      }
      this
    }

    override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      if (!tree.symbol.is(Label) && !tree.symbol.isPrimaryConstructor) {
        assert(curMethodSummary.methodDef eq tree.symbol)
        methodSummaries = methodSummaries.updated(curMethodSummary.methodDef, curMethodSummary.result())
        curMethodSummary = methodSummaryStack.pop()
      }
      tree
    }

    override def prepareForValDef(tree: tpd.ValDef)(implicit ctx: Context): TreeTransform = {
      val sym = tree.symbol
      if (sym.exists && ((sym.is(Lazy) &&  (sym.owner.is(Package) || sym.owner.isClass)) ||  //lazy vals and modules
          sym.owner.name.startsWith(nme.LOCALDUMMY_PREFIX) || // blocks inside constructor
          sym.owner.isClass)) { // fields
        // owner is a template
        methodSummaryStack.push(curMethodSummary)
        curMethodSummary = new MethodSummaryBuilder(sym, List(true))
      }
      this
    }

    override def transformValDef(tree: tpd.ValDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      val sym = tree.symbol
      if (sym.exists) {
        val ownerIsClass = sym.owner.isClass
        val isLazyValOrModule = sym.is(Lazy) && (ownerIsClass || sym.owner.is(Package))
        val isBockInsideConstructor = sym.owner.name.startsWith(nme.LOCALDUMMY_PREFIX)
        if (isLazyValOrModule || isBockInsideConstructor || ownerIsClass) {
          assert(curMethodSummary.methodDef eq tree.symbol)
          methodSummaries = methodSummaries.updated(curMethodSummary.methodDef, curMethodSummary.result())
          curMethodSummary = methodSummaryStack.pop()
        }
        if (!isLazyValOrModule && (isBockInsideConstructor || ownerIsClass))
          registerCall(tree)
      }
      tree
    }

    override def prepareForTemplate(tree: tpd.Template)(implicit ctx: Context): TreeTransform = {
      val sym = tree.symbol
      assert(!sym.is(Label))
      methodSummaryStack.push(curMethodSummary)
      curMethodSummary = new MethodSummaryBuilder(sym.owner.primaryConstructor, List(true))
      this
    }

    override def transformTemplate(tree: tpd.Template)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      val sym = tree.symbol
      assert(!sym.is(Label))
      assert(curMethodSummary.methodDef eq tree.symbol.owner.primaryConstructor)
      methodSummaries = methodSummaries.updated(curMethodSummary.methodDef, curMethodSummary.result())
      curMethodSummary = methodSummaryStack.pop()
      tree
    }

    /*
    override def transformTypeDef(tree: tpd.TypeDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      val sym = tree.symbol
      if (sym.isClass) {
        val isEntryPoint = dotty.tools.backend.jvm.CollectEntryPoints.isJavaEntryPoint(sym)
        /*summaries = ClassSummary(sym.asClass,
          methodSummaries
        ) :: summaries
        methodSummaries = Nil*/
      }
      tree
    }
    */

    def registerModule(sym: Symbol)(implicit ctx: Context): Unit = {
      if ((curMethodSummary ne null) && sym.is(ModuleVal)) {
        curMethodSummary.addAccessedModules(sym)
        registerModule(sym.owner)
      }
      val res = sym.info.finalResultType.termSymbol
      if ((curMethodSummary ne null) && res.is(ModuleVal)) {
        curMethodSummary.addAccessedModules(res)
        registerModule(res.owner)
      }

    }

    def registerCall(tree: Tree)(implicit ctx: Context): Unit = {

      def symbolOf(t: Tree) = {
        val s = t.symbol.orElse(t.tpe.classSymbol).orElse(TypeErasure.erasure(t.tpe).classSymbol)
        assert(s.exists)
        s
      }

      @tailrec def receiverArgumentsAndSymbol(t: Tree, accArgs: List[List[Tree]] = Nil, accT: List[Tree] = Nil):
          (Tree, Tree, List[List[Tree]], List[Tree], Type) = t match {
        case Block(stats, expr) => receiverArgumentsAndSymbol(expr, accArgs, accT)
        case TypeApply(fun, targs) if fun.symbol eq t.symbol => receiverArgumentsAndSymbol(fun, accArgs, targs)
        case Apply(fn, args) if fn.symbol == t.symbol => receiverArgumentsAndSymbol(fn, args :: accArgs, accT)
        case Select(qual, _) =>
          (qual, t, accArgs, accT, t.tpe)
        case x: This => (x, x, accArgs, accT, x.tpe)
        case x => (x, x, accArgs, accT, x.tpe)
      }
      val widenedTp = tree.tpe.widen
      if (!widenedTp.isInstanceOf[MethodicType] || (tree.symbol.exists && !tree.symbol.info.isInstanceOf[MethodicType])) {
        val (receiver, _ /*call*/ , arguments, typeArguments, method) = receiverArgumentsAndSymbol(tree)

        val storedReceiver = receiver.tpe

        assert(storedReceiver.exists)

        def wrapArrayTermRef(wrapArrayMethodName: TermName) =
          TermRef(defn.ScalaPredefModuleRef, defn.ScalaPredefModule.requiredMethod(wrapArrayMethodName))

        @tailrec def skipBlocks(s: Tree): Tree = s match {
          case s: Block => skipBlocks(s.expr)
          case _ => s
        }

        @tailrec def argType(x: Tree): Type = skipBlocks(x) match {
          case exp: Closure =>
            val SAMType(e) = exp.tpe
            new ClosureType(exp, x.tpe, e.symbol, OuterTargs.empty)
          case Select(New(tp), _) => new PreciseType(tp.tpe)
          case Apply(Select(New(tp), _), args) => new PreciseType(tp.tpe)
          case Apply(TypeApply(Select(New(tp), _), targs), args) => new PreciseType(tp.tpe)
          case Typed(expr: SeqLiteral, tpt) if x.tpe.isRepeatedParam =>
            wrapArrayTermRef(defn.wrapArrayMethodName(expr.elemtpt.tpe)).widenDealias.finalResultType
          case Typed(expr, _) => argType(expr)
          case _ =>
            x.tpe match {
              case _ if x.isInstanceOf[NamedArg] => ref(symbolOf(x.asInstanceOf[NamedArg].arg)).tpe
              case _ => x.tpe
            }
        }

        val thisCallInfo = CallInfo(method, typeArguments.map(_.tpe), arguments.flatten.map(argType))
        lazy val someThisCallInfo = Some(thisCallInfo)

        // Create calls to wrapXArray for varArgs
        val repeatedArgsCalls = tree match {
          case Apply(fun, _) if fun.symbol.info.isVarArgsMethod =>
            @tailrec def refine(tp: Type): Type = tp match {
              case tp: TypeAlias => refine(tp.alias.dealias)
              case tp: RefinedType => refine(tp.refinedInfo)
              case tp: TypeBounds => refine(tp.hi)
              case _ => tp
            }
            @tailrec def getVarArgTypes(tp: Type, acc: List[Type] = Nil): List[Type] = tp match {
              case tp: PolyType => getVarArgTypes(tp.resultType, acc)
              case tp@MethodType(_, paramTypes) if paramTypes.nonEmpty && paramTypes.last.isRepeatedParam =>
                getVarArgTypes(tp.resultType, refine(paramTypes.last) :: acc)
              case _ => acc
            }

            getVarArgTypes(fun.tpe.widenDealias).map { tp =>
              val wrapArrayName = defn.wrapArrayMethodName(tp)
              val targs = if (wrapArrayName == nme.wrapRefArray || wrapArrayName == nme.genericWrapArray) List(tp) else Nil
              val args = List(defn.ArrayOf(tp))
              CallInfo(wrapArrayTermRef(wrapArrayName), targs, args, someThisCallInfo)
            }

          case _ => Nil
        }

        val fillInStackTrace = tree match {
          case Apply(Select(newThrowable, nme.CONSTRUCTOR), _) if newThrowable.tpe.derivesFrom(defn.ThrowableClass) =>
            val throwableClass = newThrowable.tpe.widenDealias.classSymbol
            val fillInStackTrace = throwableClass.requiredMethod(nme.fillInStackTrace)
            if (fillInStackTrace.is(JavaDefined)) Nil
            else List(CallInfo(TermRef(newThrowable.tpe, fillInStackTrace), Nil, Nil, someThisCallInfo))
          case _ => Nil
        }

        val isInPredef =
          ctx.owner.ownersIterator.exists(owner => owner == defn.ScalaPredefModule || owner.companionModule == defn.ScalaPredefModule)

        val loadPredefModule = if (!isInPredef && (repeatedArgsCalls.nonEmpty || tree.tpe.normalizedPrefix == defn.ScalaPredefModuleRef)) {
          List(CallInfo(defn.ScalaPredefModuleRef, Nil, Nil, someThisCallInfo))
        } else {
          Nil
        }

        val sym = tree.symbol
        val mixinConstructors: List[CallInfo] = {
          if (!sym.isPrimaryConstructor) {
            Nil
          } else {
            sym.owner.mixins.distinct.map { mixin =>
              val decl = mixin.primaryConstructor

              decl.info match {
                case tp: PolyType =>
                  CallInfo(decl.termRef, tp.paramRefs, tp.resType.paramTypess.iterator.flatten.toList, someThisCallInfo) // TODO get precise type params
                case tp =>
                  CallInfo(decl.termRef, Nil, tp.paramTypess.iterator.flatten.toList, someThisCallInfo)
              }
            }
          }
        }

        val languageDefinedCalls = loadPredefModule ::: fillInStackTrace ::: mixinConstructors ::: repeatedArgsCalls

        curMethodSummary.addMethodsCalledBy(storedReceiver, thisCallInfo :: languageDefinedCalls)
      }
    }

    override def transformIdent(tree: tpd.Ident)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      if (!tree.symbol.is(Package)) {
        registerModule(tree.symbol)
      }
      val select = tree.tpe match {
        case TermRef(prefix: TermRef, name) =>
          Some(tpd.ref(prefix).select(tree.symbol))
        case TermRef(prefix: ThisType, name) =>
          Some(tpd.This(prefix.cls).select(tree.symbol))
        case TermRef(NoPrefix, name) =>
          if (tree.symbol is Method) { // todo: this kills dotty {
            val widenedTp = tree.tpe.widen
            if (widenedTp.isInstanceOf[MethodicType] && (!tree.symbol.exists || tree.symbol.info.isInstanceOf[MethodicType]))
              return tree
            registerCall(tree)
            return tree
            // Some(This(tree.symbol.topLevelClass.asClass).select(tree.symbol)) // workaround #342 todo: remove after fixed
          }
          else None
        case _ => None
      }

      select.map(transformSelect)

      tree
    }

    override def transformSelect(tree: tpd.Select)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      if (!tree.symbol.is(Package | Label) && !tree.symbol.isClass) {
        registerModule(tree.symbol)
        registerCall(tree)
      }
      // handle nullary methods
      tree
    }

    override def transformThis(tree: tpd.This)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      curMethodSummary.setThisAccessed(true)
      tree
    }

    override def transformApply(tree: tpd.Apply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      if (!tree.symbol.is(Label))
        registerCall(tree)
      tree
    }

    override def transformTypeApply(tree: tpd.TypeApply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      registerCall(tree)
      tree
    }

    def registerUnApply(selector: tpd.Tree, tree: tpd.UnApply)(implicit ctx: Context, info: TransformerInfo): Unit = {
      def registerNestedUnapply(nestedSelector: Tree, nestedPattern: Tree): Unit = nestedPattern match {
        case nestedUnapply: UnApply => registerUnApply(nestedSelector, nestedUnapply)
        case _ =>
      }

      def registerNestedUnapplyFromProduct(product: Tree, patterns: List[Tree]): Unit =
        for ((nestedPat, idx) <- patterns.zipWithIndex) {
          val nestedSel = product.select(nme.selectorName(idx))
          registerCall(nestedSel) // register call to Product._x
          registerNestedUnapply(nestedSel, nestedPat)
        }

      def registerNestedUnapplyFromSeq(seq: Tree, patterns: List[Tree]): Unit = {
        registerCall(seq.select(nme.lengthCompare).appliedTo(Literal(Constant(patterns.size))))

        if (patterns.size >= 1) {
          val headSel  = seq.select(nme.head)
          val tailSels = for (i <- 1 until patterns.size) yield seq.select(nme.apply).appliedTo(Literal(Constant(i)))
          val nestedSels = Seq(headSel) ++ tailSels

          for ((nestedSel, nestedPat) <- nestedSels zip patterns) {
            registerCall(nestedSel)
            registerNestedUnapply(nestedSel, nestedPat)
          }
        }
      }

      val unapplyCall = Apply(tree.fun, List(selector))
      registerCall(unapplyCall)

      val unapplyResultType = unapplyCall.tpe
      val hasIsDefined = extractorMemberType(unapplyResultType, nme.isDefined) isRef defn.BooleanClass
      val hasGet = extractorMemberType(unapplyResultType, nme.get).exists

      if (hasIsDefined && hasGet) { // if result of unapply is an Option
        val getCall = unapplyCall.select(nme.get)

        // register Option.isDefined and Option.get calls
        registerCall(unapplyCall.select(nme.isDefined))
        registerCall(getCall)

        if (tree.fun.symbol.name == nme.unapplySeq)                 // result of unapplySeq is Option[Seq[T]]
          registerNestedUnapplyFromSeq(getCall, tree.patterns)
        else if (tree.patterns.size == 1)                           // result of unapply is Option[T]
          registerNestedUnapply(getCall, tree.patterns.head)
        else                                                        // result of unapply is Option[(T1, ..., Tn)]
          registerNestedUnapplyFromProduct(getCall, tree.patterns)

      } else if (defn.isProductSubType(unapplyResultType)) {
        // if result of unapply is a Product
        registerNestedUnapplyFromProduct(unapplyCall, tree.patterns)
      }
    }

    def collectMatch(selector: tpd.Tree, cases: List[tpd.CaseDef])(implicit ctx: Context, info: TransformerInfo): Unit = {
      cases foreach { case CaseDef(pat, _, _) => pat match {
        case unapply: tpd.UnApply => registerUnApply(selector, unapply)
        case _ =>
      }}
    }

    override def transformMatch(tree: tpd.Match)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      collectMatch(tree.selector, tree.cases)

      tree
    }

    override def transformTry(tree: tpd.Try)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      // generate synthetic selector of Throwable type (from TryCatchPatters.scala)
      val exName = ctx.freshName("ex").toTermName
      val fallbackSelector = ctx.newSymbol(ctx.owner, exName, Flags.Synthetic | Flags.Case, defn.ThrowableType)
      val sel = Ident(fallbackSelector.termRef)

      collectMatch(sel, tree.cases)

      tree
    }

    override def transformUnit(tree: tpd.Tree)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {

      methodSums = methodSummaries

      methodSummaries = Map.empty
      methodSummaryStack = mutable.Stack()
      curMethodSummary = null

      /*

      for { cls <- ctx.compilationUnit.picklers.keySet} {
        def serializeCS(methods: List[MethodSummary], pickler: TastyPickler): Unit = {
          val buf = new TastyBuffer(5000)
          val treePickl = pickler.treePkl
          val anchorTree = tpd.SyntheticValDef((sectionName + pickler.uuid.toString).toTermName, Literal(Constant(sectionName)))

          treePickl.preRegister(anchorTree)
          treePickl.pickle(anchorTree :: Nil)

          pickler.newSection(sectionName, buf)
          val start = treePickl.buf.currentAddr
          buf.writeInt(version)//1

          def writeSymbolRef(sym: Symbol) = {
            treePickl.pickleType(ref(sym).tpe)
          }
          def writeTypeRef(tp: Type) = {
            treePickl.pickleType(tp)
          }


          def serializeMS(ms: MethodSummary) = {
            writeSymbolRef(ms.methodDef) //26

            buf.writeInt(ms.methodsCalled.size) //29
            for ((reciever, methods) <- ms.methodsCalled) {
              writeTypeRef(reciever) //36
              buf.writeInt(methods.size)

              def writeCallInfo(c: CallInfo): Unit = {
                writeTypeRef(c.call)
                buf.writeByte(c.targs.size)
                c.targs foreach writeTypeRef

                buf.writeByte(c.argumentsPassed.size)
                c.argumentsPassed foreach writeSymbolRef
              }
              methods foreach writeCallInfo
            }

            buf.writeInt(ms.accessedModules.length)
            ms.accessedModules foreach writeSymbolRef

            buf.writeByte(ms.argumentReturned)
            (ms.thisAccessed :: ms.argumentStoredToHeap).grouped(8).map(_.foldRight(0) { (bl: Boolean, acc: Int) =>
              (if (bl) 1 else 0) + 2 * acc
            }) foreach (buf.writeByte)
          }

          buf.writeInt(methods.length) // 19

          methods foreach serializeMS

          val sz = treePickl.buf.currentAddr.index - start.index

          ctx.debuglog("new section for " + cls + " size:"
            + sz + "/" + buf.currentAddr + "increased by " + (sz + buf.length) * 1.0 / start.index)
          // note: this is huge overestimate. This section contains a lot of refferences to already existing symbols and types
          // and will be compressed during bytecode generation by TreePickler.compactify
        }

          val s = methodSummaries.filter(_.methodDef.topLevelClass == cls)

          // println(s)

          serializeCS(s, ctx.compilationUnit.picklers(cls))
      } */



      tree
    }
  }
}

object CollectSummaries {

  def isPhaseRequired(implicit ctx: Context): Boolean = BuildCallGraph.isPhaseRequired

}
