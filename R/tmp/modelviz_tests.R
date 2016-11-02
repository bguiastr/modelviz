library(modelviz)

# Test parse_nonmem_model -------------------------------------------------
## No input
#P1 <- parse_nonmem_model()

## Wrong input
#P2 <- parse_nonmem_model(file = 'run999.mod')

## Use file
P3 <- parse_nonmem_model(file = 'inst/models/run101.mod')

## Use dir and runno
P4 <- parse_nonmem_model(dir = 'inst/models/', runno = '101')


# Test read_nmtab ---------------------------------------------------------
## No input
#R1 <- read_nmtab()

## Wrong input
#R2 <- read_nmtab(file = 'run999.mod')

## Read tab test
R3 <- read_nmtab(file = 'inst/models/patab101')

## Read ext test
R4 <- read_nmtab(file = 'inst/models/run101.ext', nonmem_tab = FALSE)

## Multi file test
R5 <- read_nmtab(file = paste0('inst/models/',c('sdtab','patab','covtab','simtab'),'101'))


# Test skeleton_qmd_info --------------------------------------------------
## With help
S1 <- skeleton_qmd_info()

## Without help
S2 <- skeleton_qmd_info(help = FALSE)


# Test import_qmd_info ----------------------------------------------------
## No input
#I1 <- import_qmd_info(file = NULL, dir = NULL, verbose = TRUE)

## Wrong input
#I2 <- import_qmd_info(file = 'run999.mod', verbose = TRUE)

## Use file
I3 <- import_qmd_info(file = 'inst/models/run101.mod', verbose = TRUE)

## Use dir and runno
I4 <- import_qmd_info(dir = 'inst/models/', runno = '101', verbose = TRUE)

## Test verbose
I5 <- import_qmd_info(dir = 'inst/models/', runno = '101', verbose = FALSE)


# Test format_qmd_info ----------------------------------------------------
## No input
#F1 <- format_qmd_info()

## Default test
#F2 <- format_qmd_info(qmd_info = examples$threecomp)

## DES test
#F3 <- format_qmd_info(qmd_info = I5)


# Test define_comp_layout -------------------------------------------------
## No input
#C1 <- define_comp_layout()

## Default test
C2 <- define_comp_layout(qmd_info = examples$threecomp)

## Scaling == False
C3 <- define_comp_layout(qmd_info = examples$threecomp, scaling = FALSE)

## Filled == False
C4 <- define_comp_layout(qmd_info = examples$threecomp, filled = FALSE)

## color_scaling == IIV
C5 <- define_comp_layout(qmd_info = examples$threecomp, color_scaling = 'IIV')

## More complicated example
C6 <- define_comp_layout(qmd_info = import_qmd_info(file = 'inst/models/run101.mod', verbose = TRUE), color_scaling = 'RSE')

## color_scaling == NONE
C7 <- define_comp_layout(qmd_info = examples$threecomp, color_scaling = 'NONE')

## pbpk model
C8 <- define_comp_layout(qmd_info = examples$pbpk, color_scaling = 'NONE')


# Test define_arrow_layout -----------------------------------------------
## No input
#A1 <- define_arrow_layout()

## Default test
A2 <- define_arrow_layout(qmd_info = examples$threecomp)

## Scaling == False
A3 <- define_arrow_layout(qmd_info = examples$threecomp, scaling = FALSE)

## Label == False
A4 <- define_arrow_layout(qmd_info = examples$threecomp, label = FALSE)

## color_scaling == IIV
A5 <- define_arrow_layout(qmd_info = examples$threecomp, color_scaling = 'IIV')

## More complicated example
A6 <- define_arrow_layout(qmd_info = import_qmd_info(file = 'inst/models/run101.mod', verbose = TRUE), color_scaling = 'RSE')

## color_scaling == NONE
A7 <- define_arrow_layout(qmd_info = examples$threecomp, color_scaling = 'NONE')

## pbpk model
A8 <- define_arrow_layout(qmd_info = examples$pbpk, color_scaling = 'NONE')

# Test qmd ----------------------------------------------------------------
## One compartment
qmd(examples$onecomp, scaling = FALSE, filled = FALSE)

qmd(examples$onecomp, scaling = FALSE)

qmd(examples$onecomp)

## Two compartment
qmd(examples$twocomp, scaling = FALSE, filled = FALSE)

qmd(examples$twocomp, scaling = FALSE)

qmd(examples$twocomp)

## Three compartment
qmd(examples$threecomp, scaling = FALSE, filled = FALSE)

qmd(examples$threecomp, scaling = FALSE)

qmd(examples$threecomp)

qmd(examples$threecomp, color_scaling = 'iiv', color_cutoff = c(15, 30))

## GITT model
qmd(examples$gitt, rank = c(1,2,2,2,2,2,3,4,5,5,3),
    scaling = FALSE, filled = FALSE, unscaled_color = 'dodgerblue3')

qmd(examples$gitt,
    scaling = FALSE, filled = FALSE)

qmd(examples$gitt, rank = c(1,2,2,2,2,2,3,4,5,5,3),
    scaling = FALSE)

qmd(examples$gitt, rank = c(1,2,2,2,2,2,3,4,5,5,3),
    arrow_scale_fun = function(x){sqrt(x)},
    comp_scale_fun = function(x){sqrt(x/5)})

qmd(examples$gitt, rank = c(1,2,2,2,2,2,3,4,5,5,3),
    color_scaling = 'iiv', color_cutoff = c(20,40),
    arrow_scale_fun = function(x){sqrt(x)},
    comp_scale_fun = function(x){sqrt(x/5)})

## PBPK model
qmd(examples$pbpk, pbpk_layout = TRUE, color_scaling = 'PBPK',
    scaling = FALSE, filled = FALSE)

qmd(examples$pbpk, pbpk_layout = TRUE, color_scaling = 'PBPK',
    scaling = FALSE)

qmd(examples$pbpk, pbpk_layout = TRUE, color_scaling = 'PBPK',
    arrow_scale_fun = function(x) { 1.5*x + 1 })

qmd(examples$pbpk, pbpk_layout = TRUE, color_scaling = 'PBPK',
    scaling = FALSE, flipped = TRUE)

qmd(examples$pbpk, pbpk_layout = TRUE, color_scaling = 'none',
    flipped = TRUE, arrow_scale_fun = function(x) { 1.5*x + 1 })

# Bedaquiline
qmd(examples$metabolite,
    rank = c(1,2,3,4,5,6,7,7,6,7,6,7,6),
    scaling = FALSE, filled = FALSE)

qmd(examples$metabolite,
    rank = c(1,2,3,4,5,6,7,7,6,7,6,7,6),
    scaling = FALSE)

qmd(examples$metabolite,
    rank = c(1,2,3,4,5,6,7,7,6,7,6,7,6),
    comp_scale_fun  = function(x){sqrt(x/50)},
    arrow_scale_fun = function(x){sqrt(x)},
    color_scaling = 'rse')

qmd(examples$metabolite,
    rank = c(1,2,3,4,5,6,7,7,6,7,6,7,6),
    comp_scale_fun  = function(x){sqrt(x/50)},
    arrow_scale_fun = function(x){sqrt(x)},
    color_scaling = 'iiv')

qmd(examples$metabolite,
    rank = c(1,2,3,4,5,6,7,7,6,7,6,7,6),
    alpha             = 0.6,
    comp_color_manual = c(rep('deepskyblue2',5), rep('deepskyblue2',3), rep('chartreuse3',2), rep('violetred2',2)),
    arrow_color_manual = c(rep('deepskyblue2',5), rep('deepskyblue2',3), rep('chartreuse3',2), rep('violetred2',2)),
    scaling = FALSE)

qmd(examples$metabolite,
         rank = c(1,2,3,4,5,6,7,7,6,7,6,7,6),
         comp_scale_fun    = function(x){sqrt(x/50)},
         arrow_scale_fun   = function(x){sqrt(x)},
         color_scaling     = 'none',
         alpha             = 0.6,
         comp_color_manual = c(rep('deepskyblue2',5), rep('deepskyblue2',3), rep('chartreuse3',2), rep('violetred2',2)),
         arrow_color_manual = c(rep('deepskyblue2',5), rep('deepskyblue2',3), rep('chartreuse3',2), rep('violetred2',2)),
    gv_options = 'splines = true')

## DES Parser TEST
qmd(import_qmd_info(file = 'inst/models/run300.mod'),
    color_scaling = 'IIV',
    rank = c(1,2,3,4,5,6,6,7,6,7))

## Shiny test
qmd(examples$threecomp, shiny = TRUE)

## Output test
qmd(examples$threecomp, output = 'vivagraph')
qmd(examples$threecomp, output = 'visNetwork')

## Width and height
qmd(examples$threecomp, width = 640, height = 480)


# To do -------------------------------------------------------------------
# Arrow shape
arrow_data$style     <- c('normal','tapered','tapered')
arrow_data$arrowhead <- c('normal','none','none')
arrow_data$arrowtail <- 'none'

# Test define_comp_layout -------------------------------------------------
library(modelviz)
#library(DiagrammeR)

qmd_info      = examples$gitt
rank          = NULL
pbpk_layout   = FALSE
color_scaling = 'RSE'

comp_data  <- define_comp_layout(qmd_info, color_scaling = 'none')
arrow_data <- define_arrow_layout(qmd_info, color_scaling = 'none')

sort(unique(arrow_data$from))
table(arrow_data$from)


file = 'inst/models/run101.mod'
dir         = NULL
prefix      = 'run'
runno       = NULL
ext         = '.mod'
file        = NULL
interactive = TRUE
verbose     = FALSE
source('R/msg.R')
source('R/parse_patab.R')
source('R/des_parser.R')
