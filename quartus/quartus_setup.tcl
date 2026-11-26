# Cria o projeto Quartus (riscv_de1soc.qpf). Rode dentro da pasta quartus/:
#   quartus_sh -t quartus_setup.tcl
# Depois abra riscv_de1soc.qpf no Quartus e compile (Ctrl+L).
# Pinos da DE1-SoC: conferir com o manual da placa antes de gravar.
project_new riscv_de1soc -overwrite

set_global_assignment -name FAMILY "Cyclone V"
set_global_assignment -name DEVICE 5CSEMA5F31C6
set_global_assignment -name TOP_LEVEL_ENTITY top
set_global_assignment -name VERILOG_FILE top.v
set_global_assignment -name VERILOG_FILE Pipeline3Fpga.v
set_global_assignment -name SDC_FILE top.sdc

set_location_assignment PIN_AF14 -to CLOCK_50
set_location_assignment PIN_AA14 -to KEY[0]
set_location_assignment PIN_AA15 -to KEY[1]
set_location_assignment PIN_W15  -to KEY[2]
set_location_assignment PIN_Y16  -to KEY[3]
set_location_assignment PIN_V16  -to LEDR[0]
set_location_assignment PIN_W16  -to LEDR[1]
set_location_assignment PIN_V17  -to LEDR[2]
set_location_assignment PIN_V18  -to LEDR[3]
set_location_assignment PIN_W17  -to LEDR[4]
set_location_assignment PIN_W19  -to LEDR[5]
set_location_assignment PIN_Y19  -to LEDR[6]
set_location_assignment PIN_W20  -to LEDR[7]
set_location_assignment PIN_W21  -to LEDR[8]
set_location_assignment PIN_Y21  -to LEDR[9]

foreach pin {CLOCK_50 KEY[0] KEY[1] KEY[2] KEY[3]} {
  set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to $pin
}
for {set i 0} {$i < 10} {incr i} {
  set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to LEDR[$i]
}

project_close
