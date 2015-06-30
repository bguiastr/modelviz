# modelviz
Quantitative model diagrams (QMD) for NONMEM

## Rationale
To facilitate model communication and evaluation through intuitive visual representation of their structure, parameter values and uncertainty.

## Installation

```{r, eval=FALSE}
library(devtools)
install_github("guiastrennec/modelviz")
library(modelviz)
```

## Example
```{r, eval=FALSE}
modelviz(twocomp)
```

## How to use
```{r, eval=FALSE}
 # Import dataset from NONMEM
prm_list <- prm_import(dir='../models/pk/', runno='001')

# Generate QMD
modelviz(prm_list)
```
