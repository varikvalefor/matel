# Compiling
The `Makefile` handles the compilation and installation of Matel.

To compile and install Matel, run the following command:
```sh
make
```
# Caveats
First, this file is unfinished.
## Cross-Compiling
Let _k_ denote the set of all operating systems which support `pledge(2)` and `unveil(2)`.

Let _m_ denote the set of all operating systems which do not support `pledge(2)` or `unveil(2)`.

Let _E_ denote the set of all Matel programs which support `pledge(2)` or `unveil(2)`, e.g., MATELCLI.
### Compiling on Systems which Support `pledge(2)` et al.
Theorem.  For all _g_ in _E_, for all _a_ in _k_, for all _b_ in _m_, _b_ does not run the result of compiling _g_ on _a_.

Proof.

Let _P_ denote the set of all programs which depend upon `pledge(2)` or `unveil(2)`.

For all _a_ in _P_, for all _s_ in _m_, _s_ does not run _a_.

For all _g_ in _E_, for all _a_ in _k_, the result of compiling _g_ on _a_ is an element of _P_.

Therefore, for all _g_ in _E_, for all _a_ in _k_, for all _b_ in _m_, _b_ does not run the result of compiling _g_ on _a_.  Q.E.D.
### Compiling on Systems which do Not Support `pledge(2)` et al.
Similarly, Matel executables which are not compiled on `pledge(2)`-and-`unveil(2)`-supporting operating systems do not work on OpenBSD.
