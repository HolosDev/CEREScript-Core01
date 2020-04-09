Structure
====

## Definitions

* Event
  * Spool
    * Manipulator
    * Spool Metadata
  * Script
    * Instruction
    * Operator
* Environment
  * Variable(Cell)
    * Accumulative Cell
* Value

### Summary

Usually, We gives descriptions for Event or Manipulator.
Entity of event is spool or group of spool.

### Event

Conceptual/Abstract unit of event.

### Spool

Technical unit of event.
A spool is consisted of Script and Metadata.

### Manipulator

Conceptual/Abstract unit of Control about an cell
But is almost same as Spool/Event.

### Script

Real working part without metadata.

## Module Structure

### Data.CERES

Data part of CERES.

Script itself, CERES identifiers, and data storing code will be placed here.

* Data.CERES
  * Identifiers for CERES
* Data.CERES.Type
  * Common types used in CERES
* Data.CERES.Value
  * Value types defined for CERES
  * VariablePosition for abstraction

## CERES

Function part of CERES.

Script interpreter, data operators and some helper/integration functions will be placed here.

* CERES.Interpret
  * (Reference) Interpreting code of CERES
* CERES.Operate
  * (Data) Operating code of CERES

## Data Structure

### Spool

A spool is composed of the following.

* Branch information
* CEREScript
* A set of Read/Write VariablePosition list
  * One playout, one Read/Write VariablePosition list
* Priority between spools on the same time-slot

### VariablePosition

Just an variable position identifier, not variable itself.
This indicates where the variables is stored.

* Variable Location
  * Here - Not stored in Variable, but presented in instruction itself
  * LocalTemp - Shared within a Spool at a time-slot
  * LocalVars - Shared within a Spool, can inherit to its successor
  * Time - Shared within a time-slot
  * World - Global Storage
  * Dict - Dictionary
  * Var - Variable

#### Time DB

Time DB is dedicated with an time-slot only.
This is very temporal and volatile DB.
A interpreter does not preserve this when interpreting at the time-slot is ended.

### ValueContainer

A metadata bundling container for Value.

## Instruction Structure

AnInstruction VP VC
