# Projeto RISC-V em Chisel

CPU RISC-V RV32I usando Chisel (Scala), com pipeline de 3 estagios (`Pipeline3`), CSRs e
traps (CLINT), descompressor de instrucoes RVC, atomicas (LR/SC/AMO) e caches L1 separadas
de instrucoes e dados ligadas a uma memoria principal simulada. Os blocos sao modulares e
cada um tem testes unitarios.

## Requisitos

- Linux (testado em Ubuntu)
- Java 21
- sbt 1.12.8 ou compativel

Comandos uteis para conferir versoes:

```bash
java -version
sbt --version
```

## Estrutura atual

O codigo de producao fica em `src/main/scala/riscv/`, um pacote por bloco funcional. Os testes em `src/test/scala/riscv/` seguem a mesma organizacao.

| Pacote | Arquivos | Conteudo |
| --- | --- | --- |
| `riscv.decode` | `RV32I.scala`, `Controller.scala`, `ImmGen.scala` | Constantes de opcode e seletores de controle (`RV32I`, incluindo `AMOOp`/`AMOFunct5`), decodificador/controlador combinacional (`Controller`, `ControlSignals`; valida, por exemplo, `rs2 = 0` em `LR.W`) e gerador de imediatos I/S/B/U/J. |
| `riscv.execute` | `ULA.scala` | ULA de 32 bits (`ALUOp`, `ULA`): aritmetica/logica RV32I e multiplicacao. |
| `riscv.registers` | `RegisterFile.scala` | Banco de 32 registradores x 32 bits. |
| `riscv.privileged` | `CSR.scala`, `CLINT.scala` | Banco de CSRs (`mstatus`, `mtvec`, `mepc`, `mcause`, `mtval` e equivalentes do modo S, entre outros) e logica de traps/retornos (`ECALL`, `EBREAK`, `MRET`, `SRET`), excecoes sincronas vindas do pipeline e delegacao M/S via `medeleg`. |
| `riscv.compressed` | `RvcDecompressor.scala`, `GenerateRvcDecompressorVerilog.scala` | Descompressor combinacional de instrucoes compactas (RVC) para RV32I de 32 bits e gerador do seu Verilog. |
| `riscv.atomic` | `AmoAlu.scala`, `ReservationUnit.scala` | ALU dos `AMO*.W` (valor a gravar a partir do valor antigo e de `rs2`) e reserva de `LR.W`/`SC.W` (estabelece, consome e invalida a reserva). |
| `riscv.cache` | `CacheParams.scala`, `CacheIO.scala`, `CacheArrays.scala`, `CacheLru.scala`, `Cache.scala`, `L1Cache.scala`, `CacheMainMemory.scala` | Geometria e enderecamento (`CacheParams`, `CacheAddress`), interfaces CPU-cache e cache-memoria, arrays de dados/tags, LRU, a FSM da cache (`Cache`), o `L1Cache` (ICache + DCache e coordenacao do `FENCE.I`) e a memoria principal simulada, compartilhada pelas duas caches, com latencia configuravel. |
| `riscv.pipeline` | `InstructionFetch.scala`, `Pipeline3.scala`, `GeneratePipeline3Verilog.scala` | Estagio de busca com RVC (pede palavras de 32 bits a ICache com handshake por `memoryReady`, guarda a ultima palavra num buffer, monta instrucoes de 32 bits que cruzam o limite de palavra, expande as de 16 bits e informa o tamanho, 2 ou 4 bytes, para avancar o PC); top-level do pipeline RV32I (com extensoes A e C e multiplicacao) em 3 estagios, integrando `InstructionFetch`, controlador, CSR/CLINT, `AmoAlu`, `ReservationUnit`, `L1Cache` e `CacheMainMemory`; e o gerador de Verilog do `Pipeline3` (ver a secao de simulacao). |

Testes (ScalaTest + chiseltest) em `src/test/scala/riscv/`:

- `unit/`: blocos isolados, em subpastas que espelham os pacotes acima (`decode/`, `execute/`, `registers/`, `privileged/`, `compressed/`, `atomic/`) e `fetch/` para o `InstructionFetch`.
- `cache/`: cache, LRU, arrays, FSM, latencia e memoria principal.
- `integration/`: pipeline completo: `pipeline/` (programas e acesso desalinhado), `privileged/` (traps), `rv32i/`, `rv32c/`, `blackbox/`, `diagnostics/` e `support/` (helpers compartilhados).

Outros arquivos: `build.sbt` (dependencias e configuracoes do build) e `.github/workflows/ci.yml` (CI).

## Como rodar o projeto

Entre na pasta do projeto:

```bash
cd Risk-v-pipiline-Chisel
```

Compile o codigo:

```bash
sbt compile
```

Rode todos os testes:

```bash
sbt test
```

Rode apenas o teste da ULA:

```bash
sbt "testOnly riscv.unit.execute.ULATest"
```

## CI e formatacao

O projeto usa GitHub Actions para validar o codigo a cada `push` e
`pull_request`. O workflow fica em:

```text
.github/workflows/ci.yml
```

O CI possui dois jobs separados, para ficar claro onde uma falha aconteceu:

- `Lint / Scalafmt`: verifica a formatacao.
- `Tests / ChiselTest`: roda os testes do projeto.

Eles executam:

```bash
sbt scalafmtCheckAll
sbt test
```

Para rodar a mesma verificacao localmente:

```bash
sbt scalafmtCheckAll test
```

Para formatar automaticamente os arquivos Scala/Chisel:

```bash
sbt scalafmtAll
```

Sobre linter: para Chisel, o mais comum neste projeto e tratar o codigo como
Scala e usar `scalafmt` para padronizar estilo. Erros estruturais do circuito
continuam sendo pegos por `sbt compile` e pelos testes com `chiseltest`.

## Gerando Verilog e simulando com Icarus/GTKWave

Este fluxo gera o Verilog do `Pipeline3`, carrega um programa em hexadecimal,
executa a simulacao com Icarus Verilog e abre as ondas no GTKWave.

### Ferramentas

Instale as ferramentas de simulacao:

```bash
sudo apt update
sudo apt install iverilog gtkwave
```

### Arquivo de programa

Neste README o arquivo de programa fica em `generated/program.hex`, e ele e passado
explicitamente ao gerador nos comandos abaixo. Atencao: se o gerador for chamado
**sem argumentos**, `GeneratePipeline3Verilog` usa por padrao `modelsim/program.hex`
(primeiro argumento = arquivo do programa, segundo = pasta de saida, padrao `generated`).
Por isso, sempre informe o caminho do hex ao rodar:

```text
generated/program.hex
```

Cada linha desse arquivo deve conter **uma instrucao RV32I de 32 bits em
hexadecimal**, sem `0x`, com 8 digitos por linha:

```text
00500313
00a00393
007302b3
0000006f
```

Isso representa a memoria de instrucoes por palavra:

- linha 1: instrucao no endereco `0x00000000`
- linha 2: instrucao no endereco `0x00000004`
- linha 3: instrucao no endereco `0x00000008`
- linha 4: instrucao no endereco `0x0000000c`

Ou seja, o PC anda em bytes, mas a memoria usa uma instrucao de 32 bits por
linha. Internamente, a memoria usa `PC / 4` para escolher a linha do arquivo.

### Gerar o Verilog

Entre na raiz do projeto:

```bash
cd Risk-v-pipiline-Chisel
```

Gere o Verilog do pipeline usando `generated/program.hex` como memoria de
instrucoes:

```bash
sbt "runMain riscv.pipeline.GeneratePipeline3Verilog generated/program.hex generated"
```

Esse comando gera/atualiza principalmente:

```text
generated/Pipeline3.v
generated/Pipeline3.fir
generated/Pipeline3.anno.json
```

O arquivo mais importante para a simulacao com Icarus e:

```text
generated/Pipeline3.v
```

### Testbench Verilog

O testbench usado neste fluxo e:

```text
generated/tb_pipeline3.v
```

Ele instancia o modulo `Pipeline3`, gera clock/reset, imprime alguns sinais no
terminal e cria o arquivo de ondas:

```text
pipeline.vcd
```

Dentro do testbench, as linhas responsaveis pelo GTKWave sao:

```verilog
$dumpfile("pipeline.vcd");
$dumpvars(0, tb_pipeline3);
```

### Compilar e rodar a simulacao

Compile o Verilog gerado junto com o testbench:

```bash
iverilog -g2012 -o generated/pipeline3_sim.out generated/Pipeline3.v generated/tb_pipeline3.v
```

Execute a simulacao:

```bash
vvp generated/pipeline3_sim.out
```

Ao final, deve aparecer no projeto o arquivo:

```text
pipeline.vcd
```

Se aparecer um aviso parecido com este:

```text
Not enough words in the file for the requested range [0:1023]
```

isso normalmente nao e erro. Ele so indica que `generated/program.hex` tem menos
linhas do que o tamanho total da memoria de instrucoes. As posicoes nao
preenchidas ficam indefinidas.

### Abrir as ondas no GTKWave

Abra o terminal, entre na pasta do projeto e rode:

```bash
cd Risk-v-pipiline-Chisel
gtkwave pipeline.vcd &
```

No GTKWave, procure a hierarquia `tb_pipeline3 -> dut` e comece observando:

```text
clock
reset
io_pc
io_instr
io_aluResult
io_writebackEnable
io_writebackRd
io_writebackData
io_illegal
```

Dentro de `instrMem`, os sinais principais sao:

```text
io_address
io_readData
mem_io_readData_MPORT_addr
mem_io_readData_MPORT_data
mem_io_readData_MPORT_en
```

Interpretacao rapida:

- `io_address`: endereco em bytes recebido pela memoria, normalmente o PC.
- `io_readData`: instrucao de 32 bits entregue ao pipeline.
- `mem_io_readData_MPORT_addr`: indice interno da memoria, equivalente a `PC / 4`.
- `mem_io_readData_MPORT_data`: instrucao bruta lida da memoria.
- `mem_io_readData_MPORT_en`: enable da porta de leitura.

### Sequencia completa

Para repetir tudo do zero:

```bash
cd Risk-v-pipiline-Chisel
sbt "runMain riscv.pipeline.GeneratePipeline3Verilog generated/program.hex generated"
iverilog -g2012 -o generated/pipeline3_sim.out generated/Pipeline3.v generated/tb_pipeline3.v
vvp generated/pipeline3_sim.out
gtkwave pipeline.vcd &
```

## Fluxo recomendado no dia a dia

1. Editar o modulo no pacote correspondente em `src/main/scala/riscv/` (ver "Estrutura atual").
2. Rodar `sbt scalafmtAll`.
3. Rodar `sbt scalafmtCheckAll test`.

## Escopo RV32I atual

- A ULA cobre as operacoes exigidas por `OP` e `OP-IMM`: `ADD`, `SUB`, `SLL`, `SLT`, `SLTU`, `XOR`, `SRL`, `SRA`, `OR`, `AND`.
- O controlador reconhece os opcodes RV32I usados pelo pipeline inicial: `LUI`, `AUIPC`, `JAL`, `JALR`, branches, loads, stores, `OP-IMM` e `OP`.
- Loads/stores ja saem do controlador com tamanho (`byte`, `half`, `word`) e sinalizacao de extensao sem sinal para `LBU`/`LHU`.
- A `DCache` suporta `LB`, `LH`, `LW`, `LBU`, `LHU`, `SB`, `SH` e `SW` (acesso por byte, meia palavra e palavra, com extensao de sinal nos loads).
- Alem do RV32I base, o projeto tem a ULA com multiplicacao (`MUL`, `MULH`, `MULHSU`, `MULHU`), as atomicas `LR.W`, `SC.W` e `AMO*.W`, e o RVC (instrucoes de 16 bits expandidas no `InstructionFetch`).
- Acessos desalinhados nao tem suporte por hardware: geram trap precisa no estagio EX, antes da DCache (ver a secao "Pipeline inicial").

## Pipeline inicial

O modulo `Pipeline3` segue a organizacao didatica do capitulo 15:

1. IF: busca (`InstructionFetch` + ICache), descompressao RVC e atualizacao do PC (+2 ou +4).
2. ID: decode, leitura do banco de registradores e geracao de imediato.
3. EX/MEM/WB: ULA, branch/jump, acesso a memoria de dados e write-back.

Ele possui forwarding, traps/CSRs, caches L1 separadas para instrucoes e dados, e uma memoria principal simulada compartilhada. Para codigo modificado em memoria, execute `FENCE.I`: o pipeline espera o write-back de todas as linhas dirty da DCache, invalida a ICache e refaz o fetch sequencial. `FENCE` comum e tratado como no-op no modelo single-hart.

### Traps e retornos

`ECALL`, `EBREAK`, `MRET` e `SRET` sao resolvidos no estagio EX: o pipeline descarta as instrucoes mais novas e desvia para `mtvec`/`stvec` (traps) ou `mepc`/`sepc` (retornos). Para voltar de um `ECALL`/`EBREAK`, o handler deve somar 4 a `mepc` antes do `MRET`, pois `mepc` guarda o PC da propria instrucao.

### Acessos desalinhados

Nao ha suporte por hardware. O endereco e checado no EX, antes de a DCache ver o pedido, e a instrucao nao tem efeito algum (sem escrita em `rd`, na memoria ou na reserva de LR/SC):

- `LH`/`LW` e `LR.W` desalinhados: `mcause = 4` (load address misaligned).
- `SH`/`SW`, `SC.W` e `AMO*.W` desalinhados: `mcause = 6` (store/AMO address misaligned).
- `mtval` recebe o endereco efetivo que falhou e `mepc` o PC da instrucao.
- `LR.W`, `SC.W` e `AMO*.W` exigem alinhamento de 4 bytes; `SC.W` desalinhado gera trap mesmo sem reserva.

## Erros comuns e como resolver

`java: command not found`:
- Instale o Java 21 e confirme com `java -version`.

`sbt: command not found`:
- Instale o sbt e confirme com `sbt --version`.

Falha de compilacao em teste:
- Rode primeiro `sbt compile` e depois `sbt test` para ver o erro completo de forma separada.

## Dependencias principais

- Scala `2.13.14`
- Chisel `3.6.1`
- chiseltest `0.6.1`
- Scalafmt `3.10.7`
