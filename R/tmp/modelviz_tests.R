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

## More complicated parsing
P5 <- parse_nonmem_model(file='inst/models/run201.mod')
P6 <- parse_nonmem_model(file='inst/models/run202.mod')


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
F1 <- format_qmd_info()

## Default test
F2 <- format_qmd_info(qmd_info = examples$threecomp)

## DES test
F3 <- format_qmd_info(qmd_info = I6)


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
sink('~/Desktop/1_comp_unfilled_unscaled.svg')
cat( qmd(examples$onecomp, scaling = FALSE, filled = FALSE, output = 'SVG') )
sink()

sink('~/Desktop/1_comp_unscaled.svg')
cat( qmd(examples$onecomp, scaling = FALSE, output = 'SVG') )
sink()

sink('~/Desktop/1_comp_rse.svg')
cat( qmd(examples$onecomp, output = 'SVG') )
sink()

## Two compartment
sink('~/Desktop/2_comp_unfilled_unscaled.svg')
cat( qmd(examples$twocomp, scaling = FALSE, filled = FALSE, output = 'SVG') )
sink()

sink('~/Desktop/2_comp_unscaled.svg')
cat( qmd(examples$twocomp, scaling = FALSE, output = 'SVG') )
sink()

sink('~/Desktop/2_comp_rse.svg')
cat( qmd(examples$twocomp, output = 'SVG') )
sink()

## Three compartment
sink('~/Desktop/3_comp_unscaled_unfilled.svg')
cat( qmd(examples$threecomp, scaling = FALSE, filled = FALSE, output = 'SVG') )
sink()

sink('~/Desktop/3_comp_unscaled.svg')
cat( qmd(examples$threecomp, scaling = FALSE, output = 'SVG') )
sink()

sink('~/Desktop/3_comp_rse.svg')
cat( qmd(examples$threecomp, output = 'SVG') )
sink()

sink('~/Desktop/3_comp_iiv.svg')
cat( qmd(examples$threecomp, color_scaling = 'iiv',
         color_cutoff = c(15, 30), output = 'SVG') )
sink()

## GITT model
sink('~/Desktop/gitt_unscaled_unfilled.svg')
cat( qmd(examples$gitt, rank = c(1,2,2,2,2,2,3,4,5,5,3),
         scaling = FALSE, filled = FALSE, output = 'SVG') )
sink()

sink('~/Desktop/gitt_unscaled.svg')
cat( qmd(examples$gitt, rank = c(1,2,2,2,2,2,3,4,5,5,3),
         scaling = FALSE, output = 'SVG') )
sink()

sink('~/Desktop/gitt_rse.svg')
cat( qmd(examples$gitt, rank = c(1,2,2,2,2,2,3,4,5,5,3),
         arrow_scale_fun = function(x){sqrt(x)}, output = 'SVG') )
sink()

sink('~/Desktop/gitt_iiv.svg')
cat( qmd(examples$gitt, rank = c(1,2,2,2,2,2,3,4,5,5,3),
         color_scaling = 'iiv', color_cutoff = c(20,40),
         arrow_scale_fun = function(x){sqrt(x)}, output = 'SVG') )
sink()

## PBPK model
sink('~/Desktop/pbpk_unscaled_unfilled.svg')
cat( qmd(examples$pbpk, pbpk_layout = TRUE, color_scaling = 'PBPK',
         scaling = FALSE, filled = FALSE, output = 'SVG') )
sink()

sink('~/Desktop/pbpk_unscaled.svg')
cat( qmd(examples$pbpk, pbpk_layout = TRUE, color_scaling = 'PBPK',
         scaling = FALSE, output = 'SVG') )
sink()

sink('~/Desktop/pbpk_scaled.svg')
cat( qmd(examples$pbpk, pbpk_layout = TRUE, color_scaling = 'PBPK',
         arrow_scale_fun = function(x) { 1.5*x + 1 }, output = 'SVG') )
sink()

sink('~/Desktop/pbpk_unscaled_flipped.svg')
cat( qmd(examples$pbpk, pbpk_layout = TRUE, color_scaling = 'PBPK',
         scaling = FALSE, flipped = TRUE, output = 'SVG') )
sink()

sink('~/Desktop/pbpk_scaled_flipped.svg')
cat( qmd(examples$pbpk, pbpk_layout = TRUE, color_scaling = 'PBPK',
         flipped = TRUE, arrow_scale_fun = function(x) { 1.5*x + 1 }, output = 'SVG') )
sink()

# Bedaquiline
sink('~/Desktop/bedaquiline_unscaled_unfilled.svg')
cat( qmd(examples$metabolite,
         rank = c(1,2,3,4,5,6,7,7,6,7,6,7,6),
         scaling = FALSE, filled = FALSE, output = 'SVG') )
sink()

sink('~/Desktop/bedaquiline_unscaled.svg')
cat( qmd(examples$metabolite,
         rank = c(1,2,3,4,5,6,7,7,6,7,6,7,6),
         scaling = FALSE, output = 'SVG') )
sink()

sink('~/Desktop/bedaquiline_rse.svg')
cat( qmd(examples$metabolite,
         rank = c(1,2,3,4,5,6,7,7,6,7,6,7,6),
         comp_scale_fun  = function(x){sqrt(x/50)},
         arrow_scale_fun = function(x){sqrt(x)},
         color_scaling = 'rse', output = 'SVG') )
sink()

sink('~/Desktop/bedaquiline_iiv.svg')
cat( qmd(examples$metabolite,
         rank = c(1,2,3,4,5,6,7,7,6,7,6,7,6),
         comp_scale_fun  = function(x){sqrt(x/50)},
         arrow_scale_fun = function(x){sqrt(x)},
         color_scaling = 'iiv', output = 'SVG') )
sink()


sink('~/Desktop/bedaquiline_unscaled_manual_color.svg')
cat( qmd(examples$metabolite,
         rank = c(1,2,3,4,5,6,7,7,6,7,6,7,6),
         alpha             = 0.6,
         comp_color_manual = c(rep('coral2',5), rep('deepskyblue2',3), rep('chartreuse3',2), rep('violetred2',2)),
         arrow_color_manual = c(rep('coral2',5), rep('deepskyblue2',3), rep('chartreuse3',2), rep('violetred2',2)),
         scaling = FALSE, output = 'SVG') )
sink()

sink('~/Desktop/bedaquiline_manual_color.svg')
cat( qmd(examples$metabolite,
         rank = c(1,2,3,4,5,6,7,7,6,7,6,7,6),
         comp_scale_fun    = function(x){sqrt(x/50)},
         arrow_scale_fun   = function(x){sqrt(x)},
         color_scaling     = 'none',
         alpha             = 0.6,
         comp_color_manual = c(rep('coral2',5), rep('deepskyblue2',3), rep('chartreuse3',2), rep('violetred2',2)),
         arrow_color_manual = c(rep('coral2',5), rep('deepskyblue2',3), rep('chartreuse3',2), rep('violetred2',2)),
         output = 'SVG') )
sink()

## Shiny test
qmd(examples$threecomp, shiny = TRUE)

## Output test
qmd(examples$threecomp, output = 'vivagraph')

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







