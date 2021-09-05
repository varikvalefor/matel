# Style Guidelines for Matel and Supporting Modules
This document outlines the style guidelines which should be followed when contributing to Matel.
## Line Length
For all lines, the character-based length of a line should be less than or equal to seventy-two (72).  Exceptions are made for lines which cannot be reasonably broken.  An example of a line which cannot be reasonably broken is as follows:
```haskell
deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''DisplayNameResponse;
```
