# os-bootstrapper

Code to set up my linux box after a fresh install

This package first calls a shell script to bootstrap a Haskell encironment (`stack` and `ghc`)
and calls the installer proper then.

The installer uses `dnf`, source code repositories and various other sources to gather and 
install all required software. Afterwards, hooks can be executed. 
Also, the `PATH` variable and environment variables are set according to what is requested by the packages.
