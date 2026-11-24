#!/usr/bin/env bash
# Gera o Verilog do Pipeline3 e simula com Icarus Verilog.
#
# Uso: modelsim/run_icarus.sh [programa.hex] [ciclos] [memoryWords] [memoryLatency]
#
# Requer: sbt e iverilog (sudo apt install iverilog gtkwave). As ondas ficam em
# pipeline.vcd (gtkwave pipeline.vcd).
set -euo pipefail
cd "$(dirname "$0")/.."

PROGRAM="${1:-src/test/resources/diagnostics/rv32i-add.hex}"
CYCLES="${2:-200}"
WORDS="${3:-1024}"
LATENCY="${4:-0}"
OUT=generated

command -v iverilog >/dev/null || { echo "iverilog nao encontrado (sudo apt install iverilog)"; exit 1; }

sbt "runMain riscv.pipeline.GeneratePipeline3Verilog $PROGRAM $OUT $WORDS $LATENCY"
iverilog -g2012 -o "$OUT/pipeline3_sim.out" "$OUT/Pipeline3.v" modelsim/tb_pipeline3.v
vvp "$OUT/pipeline3_sim.out" "+cycles=$CYCLES"
echo "Ondas em pipeline.vcd (gtkwave pipeline.vcd)"
