// SPDX-License-Identifier: Apache-2.0

// ---------------------------------------------------------------------------
// COPIA CORRIGIDA de treadle/executable/Scheduler.scala (Treadle 1.6.0, a versao
// usada pelo chiseltest 0.6.1). A unica mudanca em relacao ao original esta em
// `executeAssigners` e nas duas chamadas a ele.
//
// Problema: no Scala 2.13, `ArrayBuffer.toSeq` converte para uma `List`
// imutavel, e `executeAssigners` indexa por posicao (`assigners(index)`), que
// numa `List` custa O(index). O laco inteiro fica O(N^2) no numero de
// atribuicoes combinacionais, e o Treadle avalia o circuito a cada poke/peek e a
// cada ciclo: designs com dezenas de milhares de atribuicoes levam segundos por
// avaliacao (um laco de 30 mil elementos: ~0,7 s via List contra ~0,4 ms via
// ArrayBuffer). Aqui o `ArrayBuffer` e percorrido direto, como no Scala 2.12.
//
// Esta copia so tem efeito com `Test / fork := true` (ver build.sbt): o
// classpath do processo bifurcado coloca as classes de teste antes do jar do
// Treadle. O teste riscv.unit.simulation.TreadleSchedulerPatchSpec falha se a
// copia nao estiver valendo. Se o Treadle for atualizado para uma versao que
// corrija isso, apague este arquivo e o teste.
// ---------------------------------------------------------------------------
package treadle.executable

import firrtl.ir.{Info, NoInfo}
import logger.LazyLogging

import scala.collection.mutable

/** The scheduler holds the ordered assignment statements of the entire circuit.
  * Clocks have magic shadow symbols "clockName/prev". These shadows are
  * used to make the circuit evaluation idempotent, i.e. evaluating the
  * circuit at the moment of an positive clock transition can be done
  * repeatedly and registers will only be advanced on the first call.
  *
  * @param symbolTable symbol table is used to find orphans
  */
class Scheduler(val symbolTable: SymbolTable) extends LazyLogging {

  var combinationalAssigns: mutable.ArrayBuffer[Assigner] = new mutable.ArrayBuffer
  val endOfCycleAssigns:    mutable.HashSet[Assigner] = new mutable.HashSet

  val orphanedAssigns: mutable.ArrayBuffer[Assigner] = new mutable.ArrayBuffer

  private val toAssigner: mutable.HashMap[Symbol, Assigner] = new mutable.HashMap()

  var executionEngineOpt: Option[ExecutionEngine] = None

  def addAssigner(
    symbol:                   Symbol,
    assigner:                 Assigner,
    excludeFromCombinational: Boolean = false
  ): Unit = {

    toAssigner(symbol) = assigner
    combinationalAssigns += assigner
  }

  def addEndOfCycleAssigner(assigner: Assigner): Unit = {
    if (!endOfCycleAssigns.exists(a => a.symbol == assigner.symbol)) {
      endOfCycleAssigns += assigner
    }
  }

  def hasAssigner(symbol: Symbol): Boolean = {
    toAssigner.contains(symbol)
  }

  def getAllAssigners: Seq[Assigner] = {
    toAssigner.values.toSeq
  }

  def getAssignerInfo(symbol: Symbol): Info = {
    getAllAssigners.find(assigner => assigner.symbol == symbol) match {
      case Some(assigner) => assigner.info
      case _              => NoInfo
    }
  }

  def getAssignerInfo(symbolName: String): Info = {
    symbolTable.get(symbolName) match {
      case Some(symbol) => getAssignerInfo(symbol)
      case _            => NoInfo
    }
  }

  def inputChildrenAssigners(): Seq[Assigner] = {
    val assigners = {
      symbolTable
        .getChildren(symbolTable.inputPortsNames.map(symbolTable.nameToSymbol(_)).toSeq)
        .flatMap { symbol =>
          toAssigner.get(symbol)
        }
        .toSeq
    }
    assigners
  }

  def getAssigners(symbols: Seq[Symbol]): Seq[Assigner] = {
    val assigners = symbols.flatMap { symbol =>
      toAssigner.get(symbol)
    }
    assigners
  }

  def organizeAssigners(): Unit = {
    val orphansAndSensitives = symbolTable.orphans.flatMap(s => toAssigner.get(s)).flatMap {
      case _: BlackBoxCycler => None
      case _: StopOp         => None
      case _: PrintfOp       => None
      case assigner => Some(assigner)
    }

    setOrphanedAssigners(orphansAndSensitives)
    sortInputSensitiveAssigns()
  }

  def setVerboseAssign(isVerbose: Boolean): Unit = {
    def setMode(assigner: Assigner): Unit = {
      assigner.setVerbose(isVerbose)
    }
    getAllAssigners.foreach { setMode }
  }

  def setLeanMode(setLean: Boolean): Unit = {
    def setMode(assigner: Assigner): Unit = {
      assigner.setLeanMode(setLean)
    }
    getAllAssigners.foreach { setMode }
  }

  /** Execute the seq of assigners
    * @param assigners list of assigners
    */
  private def executeAssigners(
      assigners: scala.collection.IndexedSeq[Assigner]
  ): Unit = {
    var index = 0
    val lastIndex = assigners.length

    while (index < lastIndex) {
      assigners(index).run()
      index += 1
    }
  }

  /**  updates signals that depend on inputs
    */
  def executeCombinationalAssigns(): Unit = {
    executeAssigners(combinationalAssigns)
  }

  /**  updates signals that depend on inputs
    */
  def executeOrphanedAssigns(): Unit = {
    executeAssigners(orphanedAssigns)
  }

  /** de-duplicates and sorts assignments that depend on top level inputs.
    */
  def sortInputSensitiveAssigns(): Unit = {
    val buf = mutable.ArrayBuffer[Assigner]()
    buf ++= combinationalAssigns
    buf --= orphanedAssigns
    val deduplicatedAssigns = buf.distinct
    combinationalAssigns = deduplicatedAssigns.sortBy { assigner: Assigner =>
      assigner.symbol.cardinalNumber
    } ++ endOfCycleAssigns
  }

  def setOrphanedAssigners(assigners: Seq[Assigner]): Unit = {
    orphanedAssigns.clear()
    orphanedAssigns ++= assigners
  }

  /** Render the assigners managed by this scheduler
    * @return
    */
  def render(engine: ExecutionEngine): String = {
    val expressionViewRenderer = new ExpressionViewRenderer(
      engine.dataStore,
      symbolTable,
      engine.expressionViews,
      maxDependencyDepth = 0
    )

    def renderAssigner(assigner: Assigner): String = {
      val expression =
        expressionViewRenderer.render(assigner.symbol, engine.wallTime.currentTime, showValues = false)

      if (expression.isEmpty) {
        s"${assigner.symbol.name} :::"
      } else {
        s"${expression.toString}"
      }
    }

    s"Static assigns (${orphanedAssigns.size})\n" +
      orphanedAssigns.map(renderAssigner).mkString("\n") + "\n\n" +
      s"Active assigns (${combinationalAssigns.size})\n" +
      combinationalAssigns.map(renderAssigner).mkString("\n") + "\n\n"
  }
}

object Scheduler {
  def apply(symbolTable: SymbolTable): Scheduler = new Scheduler(symbolTable)
}
