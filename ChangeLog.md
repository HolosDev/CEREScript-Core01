Changelog for CEREScript-Core01
====

## Unreleased

### Add
* Add binary instructions and ternary instructions
  * Binary instruction: f(A,B) -> A
  * Ternary instruction: f(A,B) -> C
* Add structured data type
* Add array type

### Change

#### Parametric Data Type

Change type of `CERES` and `Value` as parametric to allow to change type of Str, etc.


## 0.13.0.0 -- 2020-04-09

### Changed
* Add more `VariablePlace` and rename of `Local*`


## 0.12.0.0 -- 2020-04-06

### Changed
* Change type of `NKey` from `Data.Text.Lazy.Text` to `Data.Text.Text`
* Split `VariablePosition` to `VariablePlace` and `VariableIndex`

## 0.11.1.0 -- 2020-03-31

### Added
* Add more type aliases for `NKey`


## 0.11.0.0 -- 2020-03-28

### Added
* Add more type aliases like `NKey`, `Str`, and `Idx`

### Changed
* Change `VariablePosition` name for unify length


## 0.10.0.0 -- 2020-03-26

### Changed
* Simplify `VariablePosition`
* Add new Constructor `AtNDict`
* `VariablePosition` handles idx by itself

### Removed
* Remove `VariablePlace`


## 0.9.0.0 -- 2020-03-12

### Added
* Add a VariablePlace data constructor `AtNull`
* Add a ValueType, Value, and data type `Atom`

## 0.8.1.0 -- 2020-03-12

### Added
* Add `getValueType`


## 0.8.0.0 -- 2020-03-12

### Added
* Add more random instructions


## 0.7.0.0 -- 2020-03-12

### Added
* Add more converting method

### Fix
* Fill missing operators in `operatorSelector`

### Removed
* Remove interpreter dependent function `modify*`


## 0.6.0.0 -- 2020-03-08

### Added
* Add more instructions

### Changed
* Add CRS prefix to existing instructions


## 0.5.3.0 -- 2020-03-08

### Added
* Add constant `blankVM`
* Add `get*` method for `Value`


## 0.5.2.0 -- 2020-03-08

### Added
* Add constant `blankVM`

### Changed
* Reorder `VariablePlace` and `VariablePosition` data constructors


## 0.5.1.0 -- 2020-03-05

### Changed
* Simplify type VariablePlace
* Refactoring `Show` method with `TextShow`


## 0.5.0.0 -- 2020-03-04

### Changed
* Remove `Advanced` layer
* Add timeIndex for `AtWord` and `AtTime` VariablePlace

## Removed
* Remove Interpreter code


## 0.4.0.0 -- 2020-03-03

### Changed
* Remove executable generation code from package.yaml
* Refactoring test generation code

### Removed
* Remove `CERESpool` for Interpreter implementation


## 0.3.0.0 -- 2020-03-01

### Added
* Implement `Advanced` instruction set and its operator
* Integrate `TextShow`

### Changed
* Make compatible with `TextShow` and `AtHere`


## 0.2.0.0 -- 2019-05-07

### Changed
* Rename `ValueContainer` to `VariablePosition`
* Rename some instructions name to distinguish Variable and Value
* Redesign interpreter as RW-Set style for measuring concurrent event process

### Added
* Implement `ValueContainer` newly


## 0.1.0.0 -- 2019-05-06

### Changed
* Move functional modules from `Data.CERES.*` to `CERES.*`
* Generalized instructions by `ValueContainer`
* Rename `CERES.Operator` to `CERES.Operate`

### Added
* Implement `ValueContainer`


## 0.0.0.1 -- 2019-02-23

### Added
* Implement Data.Type
* Implement Data.CERES.Value
