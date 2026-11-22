package riscv.unit.simulation

import org.scalatest.flatspec.AnyFlatSpec

/** Garante que o Scheduler do Treadle usado nos testes e a copia corrigida de
  * `src/test/scala/treadle/executable/Scheduler.scala` (O(N) por avaliacao) e
  * nao a do jar (O(N^2)). Isso so vale com `Test / fork := true`.
  */
class TreadleSchedulerPatchSpec extends AnyFlatSpec {
  behavior of "Scheduler do Treadle"

  it should "vir da copia corrigida em test-classes, nao do jar do Treadle" in {
    val location = classOf[
      treadle.executable.Scheduler
    ].getProtectionDomain.getCodeSource.getLocation.toString
    assert(
      location.contains("test-classes"),
      s"Scheduler carregado de $location; esperado test-classes. " +
        "Confira se Test / fork := true esta no build.sbt."
    )
  }
}
