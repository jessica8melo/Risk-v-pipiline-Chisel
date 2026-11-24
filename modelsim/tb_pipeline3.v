// Testbench do Pipeline3 (Verilog puro: Icarus Verilog, ModelSim, Verilator --binary).
//
// Gera clock e reset, imprime cada escrita em registrador e cada trap, cria as
// ondas em pipeline.vcd (GTKWave) e, ao fim, imprime os contadores das caches.
//
// Uso (ver modelsim/run_icarus.sh):
//   vvp generated/pipeline3_sim.out +cycles=200
//
// Com as caches frias cada instrucao leva varios ciclos (miss de ICache e latencia
// da memoria): use um numero de ciclos generoso.
`timescale 1ns / 1ps

module tb_pipeline3;
  reg clock = 1'b0;
  reg reset = 1'b1;
  reg io_perfClear = 1'b0;

  wire [31:0] io_pc;
  wire [31:0] io_instr;
  wire [31:0] io_aluResult;
  wire [31:0] io_writebackData;
  wire [4:0]  io_writebackRd;
  wire        io_writebackEnable;
  wire        io_illegal;
  wire        io_stalled;

  wire [63:0] io_iPerf_hits, io_iPerf_misses, io_iPerf_writeBacks;
  wire [63:0] io_iPerf_flushWriteBacks, io_iPerf_faults, io_iPerf_missCycles;
  wire [63:0] io_dPerf_hits, io_dPerf_misses, io_dPerf_writeBacks;
  wire [63:0] io_dPerf_flushWriteBacks, io_dPerf_faults, io_dPerf_missCycles;

  wire        io_debug_clint_trap_assert;
  wire        io_debug_clint_is_interrupt;
  wire [30:0] io_debug_clint_exception_code;
  wire [31:0] io_debug_clint_trap_address;

  Pipeline3 dut (
    .clock(clock),
    .reset(reset),
    .io_pc(io_pc),
    .io_instr(io_instr),
    .io_aluResult(io_aluResult),
    .io_writebackData(io_writebackData),
    .io_writebackRd(io_writebackRd),
    .io_writebackEnable(io_writebackEnable),
    .io_illegal(io_illegal),
    .io_stalled(io_stalled),
    .io_iPerf_hits(io_iPerf_hits),
    .io_iPerf_misses(io_iPerf_misses),
    .io_iPerf_writeBacks(io_iPerf_writeBacks),
    .io_iPerf_flushWriteBacks(io_iPerf_flushWriteBacks),
    .io_iPerf_faults(io_iPerf_faults),
    .io_iPerf_missCycles(io_iPerf_missCycles),
    .io_dPerf_hits(io_dPerf_hits),
    .io_dPerf_misses(io_dPerf_misses),
    .io_dPerf_writeBacks(io_dPerf_writeBacks),
    .io_dPerf_flushWriteBacks(io_dPerf_flushWriteBacks),
    .io_dPerf_faults(io_dPerf_faults),
    .io_dPerf_missCycles(io_dPerf_missCycles),
    .io_perfClear(io_perfClear),
    .io_debug_clint_trap_assert(io_debug_clint_trap_assert),
    .io_debug_clint_is_interrupt(io_debug_clint_is_interrupt),
    .io_debug_clint_exception_code(io_debug_clint_exception_code),
    .io_debug_clint_trap_address(io_debug_clint_trap_address)
  );

  integer max_cycles;
  integer cycle;
  integer writebacks;

  always #5 clock = ~clock; // 100 MHz

  initial begin
    max_cycles = 200;
    if ($value$plusargs("cycles=%d", max_cycles))
      $display("ciclos simulados: %0d", max_cycles);
    $dumpfile("pipeline.vcd");
    $dumpvars(0, tb_pipeline3);
  end

  initial begin
    cycle = 0;
    writebacks = 0;
    repeat (3) @(posedge clock);
    reset <= 1'b0;
    while (cycle < max_cycles) begin
      @(posedge clock);
      cycle = cycle + 1;
    end
    $display("------------------------------------------------------------");
    $display("fim apos %0d ciclos; %0d escritas em registradores", cycle, writebacks);
    $display("ICache: hits=%0d misses=%0d faults=%0d missCycles=%0d",
             io_iPerf_hits, io_iPerf_misses, io_iPerf_faults, io_iPerf_missCycles);
    $display("DCache: hits=%0d misses=%0d writeBacks=%0d faults=%0d missCycles=%0d",
             io_dPerf_hits, io_dPerf_misses, io_dPerf_writeBacks,
             io_dPerf_faults, io_dPerf_missCycles);
    $finish;
  end

  // Amostra os sinais antes da borda (os flip-flops do Chisel usam <=).
  always @(posedge clock) begin
    if (!reset) begin
      if (io_writebackEnable && io_writebackRd != 5'd0) begin
        writebacks = writebacks + 1;
        $display("[ciclo %0d] pc=0x%08h  x%0d <= 0x%08h",
                 cycle, io_pc, io_writebackRd, io_writebackData);
      end
      if (io_debug_clint_trap_assert) begin
        if (io_debug_clint_is_interrupt)
          $display("[ciclo %0d] TRAP interrupcao mcause=%0d desvio=0x%08h",
                   cycle, io_debug_clint_exception_code, io_debug_clint_trap_address);
        else
          $display("[ciclo %0d] TRAP excecao mcause=%0d desvio=0x%08h",
                   cycle, io_debug_clint_exception_code, io_debug_clint_trap_address);
      end
    end
  end
endmodule
