CEREScript-Minimal
====

CEREScript only for control some state DB

## Features

* Manipulate State DB

## Limitation

### Current limitations

* A spool couldn't manipulate any spool
* No variable manager in interpreter
  * A programmer should manage variables by oneself

### Current limitations

* Prepares RW flagger, but not use it for orchestrating concurrent spool
* Runs spools sequentially
  * Spool dependency decides running order of the spools

### Political/Strategical limitation

* Could edit only for current data
  * Do not manipulating past/future value
  * Also, no Historical Variable and DB
  * Historical manipulation against variables/spools is only used for HistoryGrapher/StorioGrapher

#### Unfixed limitation

* No branching
  * Currently, branching is designed clearly for StorioGrapher
  * But, branching is also mentioned for the rewinding feature of CERE. Therefore

## Future of CEREScript-Minimal

* CEREScript
  * For CERE
* (H)SGript
  * For HistoryGrapher/StorioGrapher
