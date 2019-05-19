WorkFlow
====

## Prepositions

Given:

* Current CERESSpools
* Environment Set
* CERESSpool Dictionary
* CEREScript Dictionary

## Callgraph

* `runSimulation [CERESSpool] EnvSet` -- First start
  * `runTimeSlot [CERESSpool] EnvSet`
  * Sort and group CERESSpools by `ccPriority`
  * Synthesis CERESSpools depend on `readVP` and/or `writeVP`
    * Converts CERESSpools to CEREScripts only
    * Completely
  * `runCEREScript` with CEREScript (Synthesized Scripts) and EnvSet
    * Returns RWSet, InheritSpoolCodes

* `runX` should return random seed