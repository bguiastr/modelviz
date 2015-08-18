library(modelviz)

# Test parse_nonmem_model -------------------------------------------------

## No input
P1 <- parse_nonmem_model()

## Wrong input
P2 <- parse_nonmem_model(file = 'run999.mod')

## Use file
P3 <- parse_nonmem_model(file = 'inst/models/run101.mod')

## Use dir and runno
P4 <- parse_nonmem_model(dir = 'inst/models/', runno = '101')

## More complicated parsing
P5 <- parse_nonmem_model(file='inst/models/run201.mod')

P6 <- parse_nonmem_model(file='inst/models/run202.mod')

# Test read_nmtab ---------------------------------------------------------

## No input
R1 <- read_nmtab()

## Wrong input
R2 <- read_nmtab(file = 'run999.mod')

## Read tab test
R3 <- read_nmtab(file = 'inst/models/patab101')

## Read ext test
R4 <- read_nmtab(file = 'inst/models/run101.ext', nonmem_tab = FALSE)

## Multi file test
R5 <- read_nmtab(file = paste0('inst/models/',c('sdtab','patab','covtab','simtab'),'101'))

## More complicated tab test
R6 <- read_nmtab(file = paste0('/Users/bengu839/Farmbio/projects/cph/cph-ge-1/Analysis/Model/Para2/',
                               c('sdtab','patab','simtab'),'039b'))

## More complicated ext test
R7 <- read_nmtab(file = '/Users/bengu839/Farmbio/projects/cph/cph-ge-1/Analysis/Model/Para2/run039b.ext', nonmem_tab = FALSE)


# Test import_qmd_info ----------------------------------------------------

## No input
I1 <- import_qmd_info(file = NULL, dir = NULL, verbose = TRUE)

## Wrong input
I2 <- import_qmd_info(file = 'run999.mod', verbose = TRUE)

## Use file
I3 <- import_qmd_info(file = 'inst/models/run101.mod', verbose = TRUE)

## Use dir and runno
I4 <- import_qmd_info(dir = 'inst/models/', runno = '101', verbose = TRUE)

## Test verbose
I5 <- import_qmd_info(dir = 'inst/models/', runno = '101', verbose = FALSE)

## More complicated parsing
I6 <- import_qmd_info(file = '/Users/bengu839/Farmbio/projects/cph/cph-ge-1/Analysis/Model/Para2/run039b.mod', verbose = TRUE)

# Test qmd ----------------------------------------------------------------

## Using built-in examples
qmd(examples$onecomp)
qmd(examples$twocomp)
qmd(examples$threecomp)

## Scaling test
qmd(examples$threecomp, scaling = FALSE)

## Horizontal test
qmd(examples$threecomp, horizontal = FALSE)

## shiny test
qmd(examples$threecomp, shiny = TRUE)

## output test
qmd(examples$threecomp, output = 'vivagraph')

## width and height
qmd(examples$threecomp, width = 640, height = 480)

## Using example run
qmd(I3)
qmd(I4)
qmd(I5)
qmd(I6)
