## owwica
Oh What Was It Called Again
===========================

This facility will select all executables that match a string criteria. 

It gathers all files from all the paths in your ${PATH} and then selects
those that match the parameter you supply.

e.g. ```owwica ch```

gives:
```
/usr/bin/chmod
/usr/bin/chgrp
/usr/bin/chown
```

```
owwica: version: 0.0.21

owwica [OPTIONS] [ITEM]
  owwica:Usage: [-?/--help] [-V/--version] [--numeric-version] [-v|--verbose]

Common flags:
  -? --help             Display help message
  -V --version          Print version information
     --numeric-version  Print just the version number
  -v --verbose          Loud verbosity
  -q --quiet            Quiet verbosity

More details on http://gitlab.com/agander/owwica
```

