CEREScript-Minimal
====

CEREScript only for control some state DB

## Features

* Manipulate State DB

## Unfixed Features

* A controller couldn't manipulate any controller

## Limitation

* Prepares RW flagger, but not use it for orchestrating concurrent controller
* Runs controllers sequentially
  * Controller dependency decides running order of the controllers
* No variable manager in interpreter
  * A programmer should manage variables by oneself

### Current limitations

### Political/Strategical limitation

* Could edit only for current data
  * Do not manipulating past/future value
  * Historical manipulation against variables/controllers is only used for HistoryGrapher/StorioGrapher

#### Unfixed limitation

* No branching
  * Currently, branching is designed clearly for StorioGrapher
  * But, branching is also mentioned for the rewinding feature of CERE. Therefore

## Future of CEREScript-Minimal

* CERES
  * For CERE
* (H)SGript
  * For HistoryGrapher/StorioGrapher
