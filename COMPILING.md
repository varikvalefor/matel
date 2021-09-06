# Compiling
The `Makefile` handles the compilation and installation of Matel.
# Caveats
As a result of Matel and MATELCLI's support of `pledge(2)` and `unveil(2)`, Matel executables which are compiled on OpenBSD do not work on most non-OpenBSD operating systems.  Similarly, Matel executables which are not compiled on `pledge(2)`-and-`unveil(2)`-supporting operating systems do not work on OpenBSD.

Additionally, this file is unfinished.
