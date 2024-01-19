# <u>Met</u>abolite <u>Mash</u>ing in <u>R</u> (MetMashR)

`MetMashR` is an R package that can be used to import, clean, filter, 
prioritise, combine and otherwise "mash" together metabolite annotations from 
multiple sources.

`MetMashR` extends class templates defined by the `struct` package to wrap 
annotation workflow steps into modular into easy to use components.

## Installation

To install this package:

```
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("MetMashR")
```

To install the development version:

```
if (!require("remotes", quietly = TRUE))
    install.packages("remotes")

remotes::install_github("computational-metabolomics/MetMashR")
```

<!-- badges: start -->
[![BioC version](https://img.shields.io/badge/dynamic/yaml?url=https%3A%2F%2Fbioconductor.org%2Fconfig.yaml&query=%24.release_version&label=Bioconductor)](http://www.bioconductor.org)
[![BioC status](http://www.bioconductor.org/shields/build/release/bioc/MetMasheR.svg)](https://bioconductor.org/checkResults/release/bioc-LATEST/MetMasheR)
<!-- badges: end -->
