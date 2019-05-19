Use ST Monad
====

## Why I could not utilize ST monad for interpreting?

The program can't bind string type variable name in a script with auto-generated ST monad's variable identifier.

## Where could I utilize ST monad effectively?

### For register?

When calculating arithmetic value, a variable which defined in ST is useful.
Only the problem is that how to get variable without passing the identifier.
