Changelog for CEREScript-Core
====

## Unreleased

### Change

#### Parametric Data Type

Change type of `CERES` and `Value` as parametric to allow to change type of Str, etc.


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
