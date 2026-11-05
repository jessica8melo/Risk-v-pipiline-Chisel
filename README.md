# Projeto RISC-V em Chisel

Implementacao inicial de blocos modulares para uma CPU RISC-V RV32I usando Chisel (Scala).
O projeto foi limpo para preparar um pipeline futuro: nao ha mais top-level uniciclo, apenas blocos reutilizaveis e testes unitarios.

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

- `src/main/scala/riscv/elementosbasicos/RV32I.scala`: constantes de opcode e seletores de controle RV32I.
- `src/main/scala/riscv/elementosbasicos/Controller.scala`: decodificador/controlador combinacional para RV32I base.
- `src/main/scala/riscv/elementosbasicos/ULA.scala`: ULA de 32 bits para operacoes aritmeticas/logicas RV32I.
- `src/main/scala/riscv/elementosbasicos/ImmGen.scala`: gerador de imediatos I/S/B/U/J.
- `src/main/scala/riscv/elementosbasicos/RegisterFile.scala`: banco de 32 registradores x 32 bits.
- `src/main/scala/riscv/elementosbasicos/Memorias.scala`: memoria de instrucao por palavra e memoria de dados por byte.
- `src/main/scala/riscv/pipeline/Pipeline3.scala`: top-level inicial de pipeline RV32I em 3 estagios.
- `src/test/scala/riscv/`: testes com ScalaTest + chiseltest
- `build.sbt`: dependencias e configuracoes do build

## Como rodar o projeto

Entre na pasta do projeto:

```bash
cd "/home/victor-costa/Documentos/Risk-v-pipiline-Chisel"
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
sbt "testOnly riscv.ULATest"
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

O arquivo de programa usado pelo fluxo atual fica em:

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
cd ~/Documentos/Risk-v-pipiline-Chisel
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

No seu caso, abra o terminal, entre na pasta do projeto e rode:

```bash
cd ~/Documentos/Risk-v-pipiline-Chisel
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
cd ~/Documentos/Risk-v-pipiline-Chisel
sbt "runMain riscv.pipeline.GeneratePipeline3Verilog generated/program.hex generated"
iverilog -g2012 -o generated/pipeline3_sim.out generated/Pipeline3.v generated/tb_pipeline3.v
vvp generated/pipeline3_sim.out
gtkwave pipeline.vcd &
```

## Fluxo recomendado no dia a dia

1. Editar o modulo em `src/main/scala/riscv/elementosbasicos/`.
2. Rodar `sbt scalafmtAll`.
3. Rodar `sbt scalafmtCheckAll test`.

## Escopo RV32I atual

- A ULA cobre as operacoes exigidas por `OP` e `OP-IMM`: `ADD`, `SUB`, `SLL`, `SLT`, `SLTU`, `XOR`, `SRL`, `SRA`, `OR`, `AND`.
- O controlador reconhece os opcodes RV32I usados pelo pipeline inicial: `LUI`, `AUIPC`, `JAL`, `JALR`, branches, loads, stores, `OP-IMM` e `OP`.
- Loads/stores ja saem do controlador com tamanho (`byte`, `half`, `word`) e sinalizacao de extensao sem sinal para `LBU`/`LHU`.
- `DataMemory` suporta `LB`, `LH`, `LW`, `LBU`, `LHU`, `SB`, `SH` e `SW` em memoria byte-addressable. O proximo passo para um core mais completo e decidir como o pipeline vai tratar acessos desalinhados e traps.

## Pipeline inicial

O modulo `Pipeline3` segue a organizacao didatica do capitulo 15:

1. IF: busca de instrucao e atualizacao do PC.
2. ID: decode, leitura do banco de registradores e geracao de imediato.
3. EX/MEM/WB: ULA, branch/jump, acesso a memoria de dados e write-back.

Ele possui forwarding, traps/CSRs, caches L1 separadas para instrucoes e dados, e uma memoria principal simulada compartilhada. Para codigo modificado em memoria, execute `FENCE.I`: o pipeline espera o write-back de todas as linhas dirty da DCache, invalida a ICache e refaz o fetch sequencial. `FENCE` comum e tratado como no-op no modelo single-hart; acessos desalinhados continuam sem suporte final.

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
