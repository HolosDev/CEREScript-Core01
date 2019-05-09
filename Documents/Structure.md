Structure
====

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

### Controller

A controller is composed of the following.

* Branch information
* CEREScript
* A set of Read/Write VariablePosition list
  * One playout, one Read/Write VariablePosition list
* Priority between 

### VariablePosition

Just an variable position identifier, not variable itself.
This indicates where the variables is stored.

* Variable Location
  * Local - Controller
  * 

### ValueContainer

A metadata bundling container for Value.

## Instruction Structure

AnInstruction VP VC
