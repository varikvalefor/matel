# Style Guidelines for Matel and Supporting Modules
This document outlines the style guidelines which should be followed when contributing to Matel.

WARNING.  This document is a work in progress.
## Line Length
For all lines, the character-based length of a line should be less than or equal to seventy-two (72).  Exceptions are made for lines which cannot be reasonably broken.  An example of a line which cannot be reasonably broken is as follows:
```haskell
deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''DisplayNameResponse;
```
## Semicolons
Matel's source code contains some unnecessary semicolons.

For all things which can be terminated by a semicolon without being broken _k_, _k_ should be terminated by a semicolon.

An example of a poorly-placed semicolon is as follows:
```haskell
main = blahblah;
  where blahblah = and [so, so]
```
This semicolon is placed poorly because the "`where`" clause which follows the semicolon is no longer specific to `main`, which defeats the purpose of the "`where`" clause.
## Function Length
For all functions _eee_, the line length of _eee_ should be less than or equal to ten (10).  This line length does _not_ include the "`where`" clauses which may be a part of _eee_; these things are considered to be separate functions.  But note that functions which contain many "`where`" clauses may be best broken into several separate functions.