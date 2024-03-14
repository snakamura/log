# coreutils in MacPorts

It used to be able to install GNU versions of `ls`, `find` and so on from MacPorts with their default names instead of names prefixed with 'g' by typing `sudo port install coreutils +with_default_names`. Now that this variant has been obsolete and all GNU binaries are installed by default to `/opt/local/libexec/gnubin` without 'g' prefix.

So you need to add this directory to `PATH` before `/bin` and `/usr/bin` to use GNU versions rather than the system default BSD versions.
