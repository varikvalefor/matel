# Style Guidelines for Matel and Supporting Modules
This document outlines the style guidelines which should be followed when contributing to Matel.
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
