# healthyControlsPresenceChecker

healthyControlsPresenceChecker

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

This package allows users to verify if a specific GEO dataset contains data of healthy controls amongside data of patients. 

## Installation via Bioconductor

Once this package will be available on Bioconductor, it will be possibile to install it through the following commands.

Start R (version "4.1") and enter:

```{r, eval=FALSE}
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("healthyControlsPresenceChecker")
```

It will be possible to load the package with the following command:

```{r, eval=FALSE}
library("healthyControlsPresenceChecker")
```


## Usage

The usage of healthyControlsPresenceChecker is very easy. The main function `healthyControlsCheck()` reads two input arguments: the GEO accession code of the dataset for which the user wants to verify the presence of the healthy controls, and a verbose flag.
For example, if the user wants to know if the [GSE47407](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE47407) dataset contains data of healthy controls, she/he can type on a terminal shell within the R environment:

```{r, eval=TRUE}
outcomeGSE47407 <- healthyControlsCheck("GSE47407", TRUE)
```

The function will print all the intermediate messages, and eventually the `outcomeGSE47407` variable will be true if healthy controls were found, or false otherwise.

## Contacts

This software was developed by [Davide Chicco](https://www.DavideChicco.it), who can be contacted via email at davidechicco(AT)davidechicco.it
