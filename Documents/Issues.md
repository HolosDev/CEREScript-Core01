Issues
====

## Display

### Display Description and Spool

When we wants to display spool or description exclusively with spreadsheet style, it is not so big problem.
However, displaying description and spool together leads hard problem with spreadsheet style.

#### When the accumulative

## AtHere VariablePosition

To optimize instruction set, provides `AtHere` `VariablePosition`.

### As a `VariablePosition`

Contains `Value` with `VariablePosition`

#### Cons

* `VariablePosition` becomes too big
* Meaning of `variablePosition` may broken

### As providing additional `VariablePlace`

Contains `Value` with `VariablePlace`.

#### Cons

* Breaks `Enum` Constraint
* Very essential element
* Can't share same name between `StandardVariablePlace` and `AdvancedVariablePlace` when I defines them in the same file.
