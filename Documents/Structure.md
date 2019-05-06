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
  * ValueContainer for abstraction

## CERES

Function part of CERES.

Script interpreter, data operators and some helper/integration functions will be placed here.

* CERES.Inteprete
  * (Reference) Intepreter code of CERES
* CERES.Operate
  * Data Operator of CERES

## Data Structure

### ValueContainer

Just an value allocation identifier, not value itself.
