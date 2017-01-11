projmanr
========

This is a R-package for internal usage at the SNP&SEQ Technology Platform at SciLifeLab. To see some basic usage examples see `tutorial.R`.


Installing from Github
-----------------------

```
library(devtools)
devtools::install_github("Molmed/projmanr", ref="master") # Change from master to release tag to install specific release
```

Building a release
------------------
To build a source release of projmanr use the `devtools::build()` command, see:

```
library(devtools)
devtools::build()
```

