CEREScript-Core
====

CEREScript-Core only for control some state DB
Also, we provide a basic reference interpreter.

## Features

* Manipulate State DB

## Limitation

### Current limitations

* A spool couldn't manipulate any spool

#### Interpreter's limitations

* Runs spools sequentially
  * Spool dependency decides running order of the spools
* The language provides RW flagger, but the interpreter do not use it for orchestrating concurrent spool

### Political/Strategical limitation

* Could edit only for *current* value
  * Do not manipulating past/future value
  * Also, no Historic Variable and DB
    * Historic manipulation against variables/spools is only used for HistoryGrapher/StorioGrapher
* No branching
  * Currently, branching is designed clearly for StorioGrapher. But, branching is also mentioned for the interpreter's rewinding feature of Project Chloe/Holos.

### Unsolved limitation

## Future of CEREScript-Core

* CEREScript-Core
  * For MaterialBalancer
* SGript
  * For HistoryGrapher / StorioGrapher
  * + Branching feature
  * + Historic instruction set
* CEREScript
  * For CoMPleT Engine / Project Holos
  * + Turing-Complete instruction set
* CERES / Demeter
  * For CERE / Project Chloe
  * + Integrate TEOE Model to instruction set
