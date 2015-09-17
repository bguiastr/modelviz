# modelviz
Quantitative model diagrams (QMD) for NONMEM

## Rationale
To facilitate model communication and evaluation through intuitive visual representation of their structure, parameter values and uncertainty.

## Installation
```r
# Install modelviz package (first time only)
devtools::install_github("guiastrennec/modelviz")

# Load modelviz package
library(modelviz)
```

## Workflow
```r
# Import dataset from a NONMEM run
qmd_info <- import_qmd_info(dir = '/nonmem/runs', runno = '101')

# Generate QMD
qmd(qmd_info)
```

## qmd_info structure
The **qmd_info** objects are lists of 7 levels:
- **descr** : model description *[character string, optional]*
- **theta** : theta typical values and RSE (%) *[data.frame, required]*
- **omega** : typical values (%) and RSE (%) *[data.frame, optional]*
- **data** : individual parameter values *[data.frame, optional]*
- **advan** : nonmem ADVAN subroutine *[integer, required]*
- **parsed_comp** : parsed compartment information *[data.frame, required]*
- **parsed_arrow** : parsed arrow information *[data.frame, required]*

``` r
$descr
[1] "Example 1-comp: Nevirapine in HIV- and TB-infected south african (day values w/o rifampicin)"

$theta
         KA   CL     V
tvprm  3.57 2.76 137.0
rse   61.00 5.70   7.2

$omega
           KA       CL
tvprm 84.2615 55.40758
rse    0.8500  0.27000

$data
NULL

$advan
[1] 2

$parsed_comp
    label  prm output
1   Depot <NA>  FALSE
2 Central    V   TRUE

$parsed_arrow
  from   to prm     dir
1   A1   A2  KA forward
2   A2 <NA>  CL forward
```
## Examples
### One-compartment model

The example dataset ```onecomp``` contains typical pharmacokinetic parameters values and uncertainty for nevirapine _(Elsherbiny et al. 2009)_

#### Unscaled
```r
qmd(examples$onecomp, scaling = FALSE, filled = FALSE)
```
<img src="inst/img/1_comp_unscaled_unfilled.png">

#### Scaled
```r
qmd(examples$onecomp, scaling = TRUE)
```
<img src="inst/img/1_comp.png">

#### Tooltip
All values *(i.e parameters, RSE and IIV)* are embedded into the graph. Just pass the cursor on the compartments or arrows of the graph and the tooltip showing the numeric values will be displayed.
<img src="inst/img/tooltip.png">

### Two-compartment model

The example dataset ```twocomp``` contains typical pharmacokinetic parameters values and uncertainty for miltefosine _(Dorlo et al. 2008)_

#### Unscaled
```r
qmd(examples$twocomp, scaling = FALSE)
```
<img src="inst/img/2_comp_unscaled_unfilled.png">


#### Scaled
```r
qmd(examples$twocomp, scaling = TRUE)
```
<img src="inst/img/2_comp.png">

### Three-compartment model

The example dataset ```threecomp``` contains typical pharmacokinetic parameters values and uncertainty for ciclosporin in paediatric renal transplant candidates _(Fanta et al. 2007)_

#### Unscaled
```r
qmd(examples$threecomp, scaling = FALSE)
```
<img src="inst/img/3_comp_unscaled_unfilled.png">


#### Scaled
##### No color scale
```r
qmd(examples$threecomp, scaling = TRUE, color_scaling = 'none', unscaled_color = 'grey80')
```
<img src="inst/img/3_comp_none.png">

##### RSE color scale [default]
```r
qmd(examples$threecomp, scaling = TRUE)
```
<img src="inst/img/3_comp.png">

##### IIV color scale
```r
qmd(examples$threecomp, scaling = TRUE, color_scaling = 'iiv', color_cutoff = c(15, 30))
```
<img src="inst/img/3_comp_iiv.png">


### Gastro-Intestinal Tansit Time (GITT) model

The example dataset ```gitt``` contains typical pharmacokinetic parameters values and uncertainty for felodipine gastro-intestinal transit time _(Hénin et al. 2012)_

#### Unscaled
```r
qmd(examples$gitt, scaling = FALSE, rank = c(1,2,2,2,2,2,3,4,5,5,3))
```
<img src="inst/img/gitt_unscaled_unfilled_ranked.png">

*Note : a ranking is manually applied to obtained a specific layout. An automated mode will come shortly*

#### Scaled
```r
qmd(examples$gitt, scaling = TRUE, rank = c(1,2,2,2,2,2,3,4,5,5,3)), arrow_scale_fun = function(x){sqrt(x)})
```
<img src="inst/img/gitt.png">

*Note : the default scaling function is sqrt(x) for volumes which scale the compartment proportionally to their surface area. For arrows the default is x. In this examples Q1 = 174 L/h a non linear scaling function has then be applied to the arrows using arrow_scale_fun*


### PBPK model

The example dataset ```pbpk``` contains typical pharmacokinetic parameters values for theophylline in adult male. _(Björkman et al. 2004)_

#### Unscaled
```r
qmd(examples$pbpk, pbpk_layout = TRUE, scaling = FALSE, filled = FALSE, unscaled_color = 'black')
```
<img src="inst/img/pbpk_unscaled_unfilled.png">


#### Scaled
```r
qmd(examples$pbpk, pbpk_layout = TRUE, scaling = TRUE, filled = FALSE, scaled_shape = 'circle', arrow_scale_fun = function(x) { 1.5*x + 1 }, unscaled_color = 'black')
```
<img src="inst/img/pbpk.png">

## In development
Modelviz automatically handles outputs from NONMEM ADVAN 1-4, 11-12. Other ADVAN require minimal user input. A differential equation parser is being implemented to make modelviz automatically compatible with any model.
