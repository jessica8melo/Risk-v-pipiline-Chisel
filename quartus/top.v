// Top-level da DE1-SoC (Cyclone V SE, 5CSEMA5F31C6).
// CLOCK_50 = clock de 50 MHz; KEY[0] = reset (ativo em nivel baixo);
// LEDR[7:0] = writebackData[7:0], LEDR[8] = illegal, LEDR[9] = paridade.
module top (
    input  wire       CLOCK_50,
    input  wire [3:0] KEY,
    output wire [9:0] LEDR
);
  // O botao e assincrono ao clock: sincroniza com 2 flip-flops.
  reg [1:0] rst_sync = 2'b11;
  always @(posedge CLOCK_50) rst_sync <= {rst_sync[0], ~KEY[0]};

  Pipeline3Fpga cpu (
      .clock  (CLOCK_50),
      .reset  (rst_sync[1]),
      .io_leds(LEDR)
  );
endmodule