package dotty.tools.dotc.transform.linker.callgraph

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.linker.summaries.{AbstractCallInfo, CallInfo}

import scala.collection.mutable

object GraphVisualization {

  @deprecated("replaced with outputGraphVis", "0")
  def outputGraph(mode: Int, specLimit: Int)(callGraph: CallGraph)(implicit ctx: Context): String = {
    val reachableMethods = callGraph.reachableMethods
    val reachableTypes = callGraph.reachableTypes
    val outerMethod = callGraph.outerMethods

    val outGraph = new StringBuffer()
    outGraph.append(s"digraph Gr${mode}_$specLimit {\n")
    outGraph.append("graph [fontsize=10 fontname=\"Verdana\" compound=true];\n")
    outGraph.append("label = \"" + reachableMethods.size + " nodes, " +
        reachableMethods.foldLeft(0)(_ + _.edgeCount) + " edges, " + reachableTypes.size  + " reachable types\";\n")

    // add names and subraphs
    reachableMethods.foreach { caller =>
      val subgraphColor = if (outerMethod.contains(caller.callSymbol)) "red" else "blue"

      outGraph.append("subgraph ").append(clusterName(caller)).append(" {\n")
      outGraph.append("  label = ").append(slash + csWTToName(caller) + slash).append(";\n")
      outGraph.append("  color = ").append(subgraphColor).append(";\n")
      outGraph.append("  ").append(dummyName(caller)).append(" [shape=point style=invis];\n")
      for ((call, _) <- caller.outEdgesIterator) {
        outGraph.append("  ").append(csToName(caller, call))
        outGraph.append(" [")
        outGraph.append("label=").append(slash).append(callSiteLabel(call)).append(slash)
        call.source match {
          case Some(source) =>
            outGraph.append(", color=")
            val color =
              if (source.call.widenDealias.classSymbol.is(JavaDefined)) "orange"
              else "blue"
            outGraph.append(color)
          case None =>
        }
        outGraph.append("];\n")
      }
      outGraph.append("}\n\n")
    }

    // Add edges
    reachableMethods.foreach { caller =>
      for ((callInfo, edges) <- caller.outEdgesIterator)
        edges.foreach { target =>
          val from = csToName(caller, callInfo)
          val to = dummyName(target)
          outGraph.append(from).append(" -> ").append(to)
          outGraph.append(" [")
          outGraph.append("ltail=").append(clusterName(target))
          outGraph.append("];\n")
        }
        outGraph.append("\n")
    }
    outGraph.append("}")
    outGraph.toString
  }

  def outputGraphVisToFile(callGraph: CallGraph, outFile: java.io.File)(implicit ctx: Context): Unit = {
    val p = new java.io.PrintWriter(outFile)
    try {
      outputGraphVis(callGraph, p)
    } finally {
      p.close()
    }
  }

  private def outputGraphVis(callGraph: CallGraph, pw: java.io.PrintWriter)(implicit ctx: Context): Unit = {
    val reachableMethods = callGraph.reachableMethods
    val reachableTypes = callGraph.reachableTypes
    val outerMethod = callGraph.outerMethods

    val callInfoNodeId = mutable.Map.empty[CallInfoWithContext, String]

    // add names and subraphs
    val nodes = mutable.Map.empty[String, String]
    val edges = mutable.Map.empty[String, List[String]]

    def addNode(id: String, properties: String*): Unit = {
      nodes(id) = (Iterator(s"id: '$id'") ++ properties).mkString("{ ", ", ", " }")
    }

    def addEdge(from: String, to: String, properties: String*): Unit = {
      val newEntry = (Iterator(s"from: '$from'", s"to: '$to'") ++ properties).mkString("{ ", ", ", " }")
      edges(from) = newEntry :: edges.getOrElse(from, Nil)
    }

    val red = "'rgb(255,150,150)'"
    val lightRed = "'rgb(255,200,200)'"
    val blue = "'rgb(150,150,255)'"
    val lightBlue = "'rgb(200,200,255)'"
    val green = "'rgb(150,255,150)'"
    val grey = "'rgb(200,200,200)'"

    def detailsHTML(info: CallInfoWithContext): String = {
      htmlFormattedStringLabel(
        s"""call: ${info.call.show}
           |targs: ${info.targs.map(_.show)}
           |arguments: ${info.argumentsPassed.map(_.show)}
           |
           |id: ${info.id}
         """.stripMargin)
    }

    callGraph.entryPoints.values.toSet[Int].foreach { entryPointId =>
      addNode("entry-" + entryPointId, "shape: 'diamond'", "color: " + green)
    }

    reachableMethods.map(_.call).foreach { call =>
      val widenCall = call.widenDealias
      val callId = "call-" + widenCall.uniqId

      val label = htmlFormattedStringLabel(call.termSymbol.owner.name + "." + call.termSymbol.name + (widenCall match {
        case widenCall: PolyType => widenCall.paramRefs.map(_.show).mkString("[", ",", "]") + widenCall.resType.show
        case widenCall => widenCall.show
      }))
      addNode(callId, s"label: '$label'", s"color: $grey")
    }

    reachableMethods.foreach { caller =>

      val color =
        if (outerMethod.contains(caller.callSymbol)) red
        else blue

      val callId = "call-" + caller.call.widenDealias.uniqId
      val callerId = callInfoNodeId.getOrElseUpdate(caller, caller.id.toString)

      addNode(callerId, s"label: '${csWTToName(caller)}'", s"title: '${detailsHTML(caller)}'", s"color: $color")
      addEdge(callerId, callId, s"title: 'actually calls'", s"color: $grey")

      callGraph.entryPoints.get(caller) match {
        case Some(entryPointId) =>
          addEdge("entry-" + entryPointId, callerId)
        case None =>
      }

      for ((call, callees)  <- caller.outEdgesIterator) {
        val callId = callerId + "-" + call.id
        val color =
          if (outerMethod.contains(caller.callSymbol)) lightRed
          else lightBlue
        addNode(callId, s"label: '${callSiteLabel(call)}'", s"color: $color")

        if (call.source.isEmpty)
          addEdge(callerId, callId, s"title: 'calls'")

        callees.foreach { callee =>
          val calleeId = callInfoNodeId.getOrElseUpdate(callee, callee.id.toString)
          addEdge(callId, calleeId, s"title: 'dispatches to'")

          callee.source.foreach { source =>
            val actualCallerId = callerId + "-" + source.id
            val calleeId = callee.id.toString
            addEdge(actualCallerId, calleeId, s"title: 'also dispatches call to'")
          }
        }
      }
    }

    def printNodesJSON(): Unit = {
      def printNodes(nodes: List[(String, String)]): Unit = nodes match {
        case n :: ns =>
          pw.print("'" + n._1 + "': " + n._2)
          if (ns.nonEmpty)
            pw.print(",")
          printNodes(ns)
        case Nil =>
      }
      pw.println("{")
      printNodes(nodes.toList)
      pw.println("}")
    }
    def printEdgesJSON(): Unit = {
      def printEdges(edges: List[(String, List[String])]): Unit = edges match {
        case e :: es =>
          pw.print("'" + e._1 + "': ")
          pw.print("[")
          pw.print(e._2.mkString(","))
          pw.print("]")
          if (es.nonEmpty)
            pw.print(",")
          printEdges(es)
        case Nil =>
      }
      pw.println("{")
      printEdges(edges.toList)
      pw.println("}")
    }

    pw.println("""
               |<!doctype html>
               |<html>
               |<head>
               |  <title>Callgraph</title>
               |
               |  <script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/vis/4.16.1/vis.min.js"></script>
               |  <link href="https://cdnjs.cloudflare.com/ajax/libs/vis/4.16.1/vis.min.css" rel="stylesheet" type="text/css">
               |  <style type="text/css">
               |    #mynetwork {
               |      width: 1080px;
               |      height: 720px;
               |      border: 1px solid lightgray;
               |    }
               |    p {
               |      max-width:1000px;
               |    }
               |  </style>
               |</head>
               |
               |<body>
               |<div id="wrapper">
               |  <div id="mynetwork"></div>
               |  <div id="loadingBar">
               |    <div class="outerBorder">
               |      <div id="text">0%</div>
               |      <div id="border">
               |        <div id="bar"></div>
               |      </div>
               |    </div>
               |  </div>
               |</div>
               |
               |
               |<script type="text/javascript">
               |  var container = document.getElementById('mynetwork');
               |  var options = {
               |    layout: {
               |      improvedLayout: true
               |    },
               |    edges: {
               |      smooth: true,
               |      arrows: { to: true }
               |    },
               |    interaction: {
               |      hover: true
               |    },
               |    physics: {
               |      enabled: true,
               |      solver: 'repulsion',
               |      maxVelocity: 10,
               |      minVelocity: 1,
               |      repulsion: {
               |        nodeDistance: 400
               |      }
               |    }
               |  };
               |""".stripMargin)

    pw.print("var nodesMap = ")
    printNodesJSON()

    pw.print("var edgesMap = ")
    printEdgesJSON()

    pw.println("""
               |  var data = {
               |    nodes: new vis.DataSet([]),
               |    edges: new vis.DataSet([])
               |  };
               |
               |  var expandNode = function (id) {
               |    var edgs = edgesMap[id];
               |    var node = nodesMap[id];
               |    if (!id.startsWith('entry') && !node.isLeaf) {
               |      if (node.shape != 'box') {
               |        node.shape = 'box';
               |        var edges1 = edgesMap[id];
               |        for (i in edges1)
               |          data.edges.add(edges1[i]);
               |      } else {
               |        node.shape = undefined;
               |        var edges1 = edgesMap[id];
               |        for (i in edges1)
               |          data.edges.remove(edges1[i]);
               |      }
               |      data.nodes.update(nodesMap[id]);
               |    }
               |    for (e in edgs) {
               |      var nodeId = edgs[e].to
               |      var node = nodesMap[nodeId];
               |      if (!node.added) {
               |        if (!edgesMap[nodeId]) {
               |          node.shape = 'box';
               |          node.shapeProperties = { borderDashes: [10,5] };
               |          node.isLeaf = true;
               |        }
               |        data.nodes.add(node);
               |        node.added = true;
               |      }
               |    }
               |  };
               |
               |  var doubleClickOnNode = function (id) {
               |    expandNode(id);
               |  };
               |
               |  for (i in nodesMap) {
               |    if (i.startsWith('entry')) {
               |      data.nodes.add(nodesMap[i]);
               |      data.edges.add(edgesMap[i]);
               |      expandNode(i);
               |    }
               |  }
               |
               |  var network = new vis.Network(container, data, options);
               |   network.on("stabilizationProgress", function(params) {
               |      var maxWidth = 496;
               |      var minWidth = 20;
               |      var widthFactor = params.iterations/params.total;
               |      var width = Math.max(minWidth,maxWidth * widthFactor);
               |
               |      document.getElementById('bar').style.width = width + 'px';
               |      document.getElementById('text').innerHTML = Math.round(widthFactor*100) + '%';
               |  });
               |  network.once("stabilizationIterationsDone", function() {
               |      document.getElementById('text').innerHTML = '100%';
               |      document.getElementById('bar').style.width = '496px';
               |      document.getElementById('loadingBar').style.opacity = 0;
               |      // really clean the dom element
               |      setTimeout(function () {document.getElementById('loadingBar').style.display = 'none';}, 500);
               |  });
               |  network.on("doubleClick", function (params) {
               |      params.event = "[original event]";
               |      if (params.nodes && params.nodes.length > 0)
               |        doubleClickOnNode(params.nodes[0]);
               |  });
               |</script>
               |
               |</body>
               |</html>
             """.stripMargin)
  }

  private def callSiteLabel(x: CallInfo)(implicit ctx: Context): String = {
    val prefix = x.call.normalizedPrefix
    val calleeSymbol = x.callSymbol
    val prefixString = prefix match {
      case NoPrefix => calleeSymbol.name.toString
      case t if calleeSymbol.isPrimaryConstructor => calleeSymbol.showFullName
      case st: SuperType => s"super[${st.supertpe.classSymbol.showFullName}].${calleeSymbol.name}"
      /* case t if calleeSymbol.is(SuperAccessor) =>
         val prev = t.classSymbol
         types.flatMap {
           x =>
             val s = x.baseClasses.dropWhile(_ != prev)
             if (s.nonEmpty) {
               val parent = s.find(x => x.info.decl(calleeSymbol.name).altsWith(x => x.signature == calleeSymbol.signature).nonEmpty)
               parent match {
                 case Some(p) if p.exists =>
                   val method = p.info.decl(calleeSymbol.name).altsWith(x => x.signature == calleeSymbol.signature)
                   // todo: outerTargs are here defined in terms of location of the subclass. Is this correct?
                   new CallWithContext(t.select(method.head.symbol), targs, args, outerTargs) :: Nil
                 case _ => Nil
               }
             } else Nil
         }     */

      case thisType: ThisType =>
        "this." + calleeSymbol.name
      case t =>
        typeName(t) + '.' + calleeSymbol.name
    }

    val targsString = typeArgumentsString(x.targs)
    val vargsString = argumentsString(x.argumentsPassed)

    htmlFormattedStringLabel(prefixString + targsString + vargsString)
  }

  private val slash = '"'

  private def htmlFormattedStringLabel(str: String): String =
    str.replace("\n", "<br>").replace("'", "\"")

  private def escape(s: String) = s.replace("\\", "\\\\")

  private def fullNameSeparated(symbol: Symbol)(separator: String)(implicit ctx: Context): Name = {
    var sep = separator
    val owner = symbol.owner
    var name: Name = symbol.name
    var stopAtPackage = false
    if (sep.isEmpty) {
      sep = "$"
      stopAtPackage = true
    }
    if (symbol.isAnonymousClass || symbol.isAnonymousFunction)
      name = name ++ symbol.id.toString
    if (symbol == NoSymbol ||
      owner == NoSymbol ||
      owner.isEffectiveRoot ||
      stopAtPackage && owner.is(PackageClass)) name
    else {
      var encl = owner
      while (!encl.isClass && !encl.isPackageObject) {
        encl = encl.owner
        sep += "~"
      }
      if (owner.is(ModuleClass, butNot = Package) && sep == "$") sep = "" // duplicate scalac's behavior: don't write a double '$$' for module class members.
      val fn = fullNameSeparated(encl)(separator) ++ sep ++ name
      if (symbol.isType) fn.toTypeName else fn.toTermName
    }
  }

  private def symbolName(sym: Symbol)(implicit ctx: Context): String = {
    if (!sym.is(Method)) escape(sym.name.show)
    else escape(fullNameSeparated(sym)(".").show)
  }

  private def typeName(x: Type)(implicit ctx: Context): String = {
    x match {
      case ConstantType(value) => escape(value.toString)
      case _ =>
        val t = x.termSymbol.orElse(x.typeSymbol)
        if (t.exists)
          symbolName(t)
        else escape(x.show)
    }
  }

  private def csWTToName(x: AbstractCallInfo)(implicit ctx: Context): String = {
    val targs = typeArgumentsString(x.targs)
    val vargs = typeParameterString(x.call.widenDealias.paramInfoss)
    val resultType = typeName(x.call.widenDealias.finalResultType)
    if (x.callSymbol.owner == x.call.normalizedPrefix.classSymbol) {
      val callTypeName = typeName(x.call)
      htmlFormattedStringLabel(callTypeName + targs + vargs + ": " + resultType)
    } else {
      val callTypeName = typeName(x.call.normalizedPrefix)
      val symName = symbolName(x.callSymbol)
      htmlFormattedStringLabel(callTypeName + ".super." + symName + targs + vargs + ": " + resultType)
    }
  }

  private def csWTToShortName(x: AbstractCallInfo)(implicit ctx: Context): String = {
    if (x.callSymbol.owner.name == x.call.normalizedPrefix.classSymbol.name) {
      val callTypeName = typeName(x.call)
      callTypeName
    } else {
      val callTypeName = typeName(x.call.normalizedPrefix)
      symbolName(x.callSymbol)
    }
  }

  private def csToName(parent: CallInfoWithContext, inner: CallInfo)(implicit ctx: Context): String = {
    slash + csWTToName(parent) + escape(inner.call.show) + inner.hashCode() + slash
  }

  private def dummyName(x: AbstractCallInfo)(implicit ctx: Context): String = {
    slash + csWTToName(x) + "_Dummy" + slash
  }

  private def clusterName(x: AbstractCallInfo)(implicit ctx: Context): String = {
    slash + "cluster_" + csWTToName(x) + slash
  }

  private def argumentsString(args: List[Type])(implicit ctx: Context): String = {
    if (args.isEmpty) ""
    else args.map(typeName).mkString("(", ",", ")")
  }

  private def typeArgumentsString(targs: List[Type])(implicit ctx: Context): String = {
    if (targs.isEmpty) ""
    else targs.map(t => typeName(t.widenDealias)).mkString("[", ",", "]")
  }

  private def typeParameterString(paramTypess: List[List[Type]])(implicit ctx: Context): String = {
    paramTypess.iterator.map { paramTypes =>
      paramTypes.map(x => typeName(x)).mkString("(", ",", ")")
    }.mkString("")
  }
}