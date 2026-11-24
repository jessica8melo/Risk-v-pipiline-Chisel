# Projeto RISC-V em Chisel

CPU RISC-V RV32I usando Chisel (Scala), com pipeline de 3 estagios (`Pipeline3`), CSRs e
traps (CLINT), instrucoes compactas (RVC), atomicas (LR/SC/AMO) e caches L1 separadas de
instrucoes e dados ligadas a uma memoria principal simulada (com latencia configuravel,
canal de falha e contadores de desempenho). Os blocos sao modulares e cada um tem testes.

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
| `riscv.cache` | `CacheParams.scala` (`CacheConfig`), `CacheIO.scala`, `CacheArrays.scala`, `CacheLru.scala` (interface comum e as politicas LRU, FIFO e aleatoria), `Cache.scala`, `L1Cache.scala`, `CacheMainMemory.scala` | Geometria parametrizavel e enderecamento (`CacheConfig`, `CacheParams`, `CacheAddress`), interfaces CPU-cache e cache-memoria, arrays de dados/tags, LRU, a FSM da cache (`Cache`), o `L1Cache` (ICache + DCache e coordenacao do `FENCE.I`), os contadores de desempenho (`CachePerf`) e a memoria principal simulada, compartilhada pelas duas caches, com latencia configuravel e canal de falha (`fault`). Ver a secao "Sistema de cache". |
| `riscv.pipeline` | `InstructionFetch.scala`, `Pipeline3.scala` | Estagio de busca com RVC (pede palavras de 32 bits a ICache com handshake por `memoryReady`, guarda a ultima palavra num buffer, monta instrucoes de 32 bits que cruzam o limite de palavra, expande as de 16 bits e informa o tamanho, 2 ou 4 bytes, para avancar o PC); top-level do pipeline RV32I (com extensoes A e C e multiplicacao) em 3 estagios, integrando `InstructionFetch`, controlador, CSR/CLINT, `AmoAlu`, `ReservationUnit`, `L1Cache` e `CacheMainMemory`. Parametros do `Pipeline3`: `initialProgram`, `memoryWords` (tamanho da memoria principal, em palavras), `programFile` (hex carregado na memoria), `memoryLatency` (ciclos de resposta da memoria; 0 = combinacional) e `cacheConfig` (geometria e politica das caches). |

Testes (ScalaTest + chiseltest) em `src/test/scala/riscv/`:

- `unit/`: blocos isolados, em subpastas que espelham os pacotes acima (`decode/`, `execute/`, `registers/`, `privileged/`, `compressed/`, `atomic/`).
- `cache/`: arrays, LRU, FSM, caminho de acerto, latencia da memoria, canal de falha (`CacheFaultTest`), contadores (`CachePerfTest`), memoria principal e a varredura de geometria/politica (`CacheGeometryTest`).
- `integration/`: pipeline completo. `pipeline/` (`Pipeline3MisalignedSpec`: traps de acesso desalinhado; `Pipeline3SlowMemorySpec`: memoria lenta; `Pipeline3CounterSpec`: `mcycle`/`minstret` e contadores das caches nos CSRs), `blackbox/rv32i/` (programas RV32I), `diagnostics/` (`Rv32iHexTraceSpec`, ver a secao de programas em hex) e `support/` (helpers compartilhados).

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

Rode uma suite, ou um unico caso pelo nome:

```bash
sbt "testOnly riscv.integration.pipeline.Pipeline3MisalignedSpec"
sbt "testOnly riscv.cache.CacheMemoryLatencyTest riscv.cache.CachePerfTest"
sbt 'testOnly riscv.integration.pipeline.Pipeline3MisalignedSpec -- -z "nao gerar trap"'
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

## Programas em hex e geracao de Verilog

### Rodar um programa hex nos testes

O teste de diagnostico executa um programa em hexadecimal no `Pipeline3` e lista as
escritas em registradores. Cada linha do arquivo e **uma palavra de 32 bits em
hexadecimal**, 8 digitos, sem `0x` (comentarios com `#` sao ignorados). A linha 1 fica no
endereco `0x0`, a linha 2 em `0x4`, e assim por diante; programa e dados dividem o mesmo
espaco de enderecos. Instrucoes RVC ocupam meia palavra (little-endian):

```text
00500313
00a00393
007302b3
0000006f
```

```bash
RV32I_HEX=caminho/programa.hex RV32I_CYCLES=60 sbt "testOnly riscv.integration.diagnostics.Rv32iHexTraceSpec"
```

`RV32I_CYCLES` e o numero de ciclos simulados. Com caches frias cada instrucao leva mais
ciclos para chegar (miss de ICache), entao use um valor generoso. Um exemplo pronto esta em
`src/test/resources/diagnostics/rv32i-add.hex`.

### Gerar Verilog

Hoje existe um unico gerador, o do descompressor RVC:

```bash
sbt "runMain riscv.compressed.GenerateRvcDecompressorVerilog"
```

O Verilog sai em `output/decompressor/`. O gerador do `Pipeline3` e o fluxo com
Icarus/GTKWave que existiam antes foram removidos (o pipeline agora busca instrucoes
pela ICache, e nao mais por uma memoria de instrucoes propria). Para gerar o Verilog do
`Pipeline3`, crie um `App` parecido com o do descompressor, por exemplo:

```scala
(new ChiselStage).emitVerilog(
  new Pipeline3(programFile = "generated/program.hex", memoryLatency = 0),
  Array("--target-dir", "generated")
)
```

O `programFile` e carregado na memoria principal (`CacheMainMemory`) por
`loadMemoryFromFileInline`; o simulador precisa de um testbench proprio, que nao esta
no repositorio.

## Fluxo recomendado no dia a dia

1. Editar o modulo no pacote correspondente em `src/main/scala/riscv/` (ver "Estrutura atual").
2. Rodar `sbt scalafmtAll`.
3. Rodar `sbt scalafmtCheckAll test`.

## Escopo RV32I atual

- A ULA cobre as operacoes exigidas por `OP` e `OP-IMM`: `ADD`, `SUB`, `SLL`, `SLT`, `SLTU`, `XOR`, `SRL`, `SRA`, `OR`, `AND`.
- O controlador reconhece os opcodes RV32I usados pelo pipeline inicial: `LUI`, `AUIPC`, `JAL`, `JALR`, branches, loads, stores, `OP-IMM` e `OP`.
- Loads/stores ja saem do controlador com tamanho (`byte`, `half`, `word`) e sinalizacao de extensao sem sinal para `LBU`/`LHU`.
- A `DCache` suporta `LB`, `LH`, `LW`, `LBU`, `LHU`, `SB`, `SH` e `SW` (acesso por byte, meia palavra e palavra, com extensao de sinal nos loads).
- Alem do RV32I base, o projeto tem multiplicacao (`MUL`, `MULH`, `MULHSU`, `MULHU`; **sem** `DIV`/`REM`), as atomicas `LR.W`, `SC.W` e `AMO*.W`, e o RVC (instrucoes de 16 bits expandidas no `InstructionFetch`).
- Acessos desalinhados nao tem suporte por hardware: geram trap precisa no estagio EX, antes da DCache (ver a secao "Acessos desalinhados").

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

### Access faults e instrucao ilegal

Tres excecoes sincronas sao tomadas no estagio EX, com `mepc` = PC da instrucao (que nao tem efeito algum) e `mtval` conforme a causa:

| Causa | `mcause` | `mtval` |
|---|---|---|
| Busca recusada pela memoria (instruction access fault) | 1 | endereco que falhou (o PC, ou a palavra seguinte se a instrucao cruza palavras) |
| Instrucao ilegal | 2 | encoding original (16 bits, se comprimida) |
| Load ou `LR.W` fora da memoria | 5 | endereco efetivo |
| Store, `SC.W` ou `AMO*.W` fora da memoria | 7 | endereco efetivo |

- **Busca:** o `InstructionFetch` marca o slot com `fault` e o marcador viaja ate o EX, para que as instrucoes mais antigas terminem antes da trap (um desvio mais antigo pode descartar a busca errada). Depois de uma falha a busca para ate o proximo flush.
- **Dados:** o `fault` da DCache encerra a espera do pipeline; a trap sai assim que nao houver miss de ICache pendente. Os arrays e a memoria nao mudam.
- **Instrucao ilegal:** inclui opcode invalido, RVC ilegal, `pc[0]` impar e a palavra `0x00000000`. **Programas de teste precisam terminar em um laco** (`jal x0, 0`), porque cair na memoria vazia agora gera trap (com `mtvec = 0`, o programa reinicia).
- Prioridade quando mais de uma vale: busca (1), ilegal (2), desalinhado (4/6), dados (5/7); na pratica sao exclusivas.

### Privilegio

O processador comeca em M-mode. O CLINT calcula o privilegio seguinte (`priv_write`) e o CSR o grava junto com o resto da trap:

- trap (excecao ou interrupcao): vai a **M**, ou a **S** quando delegada por `medeleg`/`mideleg`; `mstatus.MPP` (ou `SPP`) guarda o privilegio anterior;
- `MRET` volta ao privilegio salvo em `MPP`; `SRET`, ao salvo em `SPP` (S ou U).

A checagem acontece no EX, com o privilegio vigente, e uma violacao e **instrucao ilegal** (`mcause 2`, `mtval` = encoding):

| Situacao | Ilegal quando |
|---|---|
| Acesso a CSR | o CSR nao existe, ou o privilegio atual e menor que `csr[9:8]` (U < S < M) |
| Leitura de `cycle`/`instret`/`hpmcounterN` (0xC00..) em S ou U | o bit `N` de `mcounteren` esta em 0 |
| Escrita em CSR | o CSR e somente leitura (`csr[11:10] = 11`); `CSRRS`/`CSRRC` com `rs1 = 0` (ou `zimm = 0`) so leem e sao permitidos |
| `MRET` | privilegio diferente de M |
| `SRET` | em U, ou em S com `mstatus.TSR = 1` |

Limites: `misa`, `mip` e `sip` existem e leem zero (escritas ignoradas); `sstatus` e um registrador separado de `mstatus` (nao um recorte dele); nao ha `TVM`, `TW`, `MPRV`, PMP nem MMU, entao loads e stores em U/S nao sao restringidos.

## Sistema de cache

- **Geometria padrao:** L1 dividida em ICache e DCache, 8 KiB cada, 4 vias, linha de 16 B (4 palavras), reposicao LRU, write-back com write-allocate, controlador bloqueante. A premissa e que os 16 KiB do enunciado sao a soma das duas.
- **Geometria e politica parametrizaveis** (`CacheConfig`): `capacityBytes`, `ways` (1 = mapeamento direto), `lineBytes` (potencia de 2, ao menos 8) e `policy` (`ReplacementPolicy.Lru`, `Fifo` ou `Random`). A mesma configuracao vale para a ICache e a DCache. Exemplo: `new Pipeline3(cacheConfig = CacheConfig(capacityBytes = 4096, ways = 2, lineBytes = 32, policy = ReplacementPolicy.Fifo))`. `CacheParams` continua existindo e e a geometria padrao (`CacheConfig.default`). Tamanhos devem ser potencias de 2 e o numero de conjuntos, ao menos 2.
- **Politicas de substituicao:** so atuam com o conjunto cheio (vias invalidas sao preenchidas primeiro, da menor para a maior). LRU atualiza a cada acerto; FIFO avanca um ponteiro por conjunto a cada linha alocada (e o zera quando a ICache e invalidada); a aleatoria usa um LFSR de 16 bits sempre em marcha, determinista mas dependente do ciclo.
- **Varredura para experimentos:** `CacheGeometryTest` roda, um eixo por vez e com a linha de base nos demais, capacidade 1/2/4/8/16 KiB, 1/2/4/8 vias, linhas de 8/16/32/64 B e as tres politicas. Todas as configuracoes rodam o mesmo trace (`CacheSweepTrace`: enderecos de byte fixos, semente fixa, independente da geometria), o que torna os quatro eixos comparaveis. Confere dados e contadores (LRU e FIFO) contra um modelo de referencia e grava os contadores e a taxa de acerto de cada configuracao em `target/cache-sweep.csv`, com a linha de base presente em todos os eixos: `sbt "testOnly riscv.cache.CacheGeometryTest riscv.cache.CacheSweepTraceTest"`. O `CacheSweepTraceTest` e a guarda de regressao (Scala puro, rapido): falha se duas geometrias distintas derem contadores identicos.
- **Contrato com a CPU:** enquanto `stall` esta alto, o pedido (`request`, `write`, `address`, ...) deve ficar estavel. Por isso o pipeline so desvia, trata trap ou termina `FENCE.I` quando nao ha espera de cache.
- **Bit de sujeira so na DCache:** `CacheArrays(cfg, hasDirty)` so cria o registrador `dirtyBits` quando `hasDirty = true`; a ICache usa `hasDirty = false`, entao o armazenamento nao existe no circuito (a leitura de `dirty` e a constante 0 e `write.dirty` e ignorado). Teste: `CacheDirtyBitTest`.
- **Coerencia:** ICache e DCache so se sincronizam por `FENCE.I` (grava as linhas dirty da DCache e invalida a ICache), como manda a especificacao.
- **Memoria principal** (`CacheMainMemory`): `latency = 0` responde no mesmo ciclo; com `latency >= 1` atende uma transacao por vez, com prioridade da DCache em caso de empate. O tamanho padrao do `Pipeline3` (`memoryWords = 1024`, 4 KiB) e menor que as caches; para experimentos de cache use uma memoria bem maior.
- **Canal de falha:** um pedido fora da capacidade (ou desalinhado) e recusado pela memoria com `fault` no lugar de `ready` (a leitura devolve zero e nada e escrito). A `Cache` vai ao estado `sFault` e levanta `io.cpu.fault` ate a CPU derrubar o pedido; os arrays nao mudam. A convencao prevista e: ICache = instruction access fault (`mcause 1`); DCache = load (`5`) ou store/AMO (`7`) access fault, com `mtval = address`. O `L1Cache` tambem expoe `fenceIFault` quando um write-back do `FENCE.I` e recusado. O `Pipeline3` converte esses sinais em trap (ver "Access faults e instrucao ilegal").
- **Contadores de desempenho** (`CachePerf`, 64 bits, em `L1Cache.io.iPerf` e `io.dPerf`; `io.perfClear` zera os das duas caches): `hits`, `misses` (um miss seguido de refill conta 1, e o hit que atende o pedido depois do refill nao entra em `hits`), `writeBacks` (despejo de linha suja), `flushWriteBacks` (flush do `FENCE.I`), `faults` e `missCycles` (penalidade de miss acumulada).

  O `Pipeline3` expoe os dois conjuntos em `io.iPerf` e `io.dPerf` (e tem `io.perfClear`), e os mesmos valores aparecem como CSRs (ver "Contadores como CSRs").

## Contadores como CSRs

`mcycle` e `minstret` (64 bits, em duas metades) e 10 contadores das caches (`mhpmcounter3..12`, os `CachePerf`) sao CSRs. Cada contador de 64 bits aparece em 4 enderecos de 32 bits: `0xB00 + n` (baixa, M), `0xB80 + n` (alta, M), `0xC00 + n` e `0xC80 + n` (espelhos somente leitura, legiveis em S/U conforme `mcounteren`).

| n | Contador | n | Contador |
|---|---|---|---|
| 0 | `mcycle` (todo ciclo) | 7 | DCache misses |
| 2 | `minstret` (instrucoes concluidas) | 8 | DCache miss cycles |
| 3 | ICache hits | 9 | DCache write-backs |
| 4 | ICache misses | 10 | DCache flush write-backs |
| 5 | ICache miss cycles | 11 | ICache faults |
| 6 | DCache hits | 12 | DCache faults |

- `minstret` conta cada instrucao uma unica vez, quando ela conclui no EX: esperas por miss de cache nao contam instrucoes a mais, e `ECALL`/`EBREAK` (que geram excecao) nao contam. A leitura soma a instrucao que conclui no mesmo ciclo, entao o valor lido e o de todas as instrucoes anteriores.
- `mcycle` e `minstret` sao graveis (metades baixa e alta). Os contadores das caches sao somente leitura (escrita ignorada); eles so voltam a zero por `io.perfClear` do `Pipeline3`.
- `mcounteren` (0x306) e leitura/escrita. Nao ha `mcountinhibit`, `time` nem `scounteren`.
- Os contadores das caches sao registrados no fim do ciclo do evento: uma leitura imediatamente apos o acesso que o gerou ainda nao o inclui (deixe 1 ou 2 instrucoes de folga).
- Teste: `Pipeline3CounterSpec`.

## Pendencias conhecidas

- `DIV`, `DIVU`, `REM` e `REMU` (extensao M so tem multiplicacao).
- Interrupcoes: o timer esta desligado (sem `mtime`/`mtimecmp`) e nao ha `WFI` nem interrupcao externa.
- `mcountinhibit`, `mtime`/`time` e `scounteren` (os contadores sempre contam; o acesso de S/U depende so de `mcounteren`).
- MMIO e regiao nao-cacheable (todo acesso passa pela cache).
- MMU: ha o CSR `satp`, mas nao ha traducao de enderecos nem `SFENCE.VMA`.

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
