;; 1. Based on:                     na
;; 2. Description:                  Modelviz Template model
;; 3. Label:                        201
;; 4. Structural model:             1CMT-oral
;; 5. Covariate model:              na
;; 6. Inter-individual variability: na
;; 7. Inter-occasion variability:   na
;; 8. Residual variability:         Combined
;; 9. Estimation:                   FOCE-I
$PROBLEM    Modelviz Template model
$INPUT      ID TIME DOSE AMT DV MDV
$DATA      ../../data_produced/data.csv IGNORE=@ 
$ABBREVIATED DERIV2=NO
$SUBROUTINE ADVAN13 TOL=6
$MODEL      COMP=(DEPOT,DEFDOSE)      ; 1. Depot compartment
            COMP=(CENTRAL,DEFOBS)     ; 2. Central compartment
$PK 
 TVCL  = THETA(3)
 CL    = TVCL*EXP(ETA(1))             ; Apparent total clearance (L/h)

 TVVC  = THETA(4)
 VC    = TVVC*EXP(ETA(2))             ; Apparent volume of distribution (L)

 TVKA  = LOG(2)/THETA(5)              ; Change half-life into rate constant
 KA    = TVKA*EXP(ETA(3))             ; Rate constant of absorption (h-1)

$DES 
; Eq Diff
 DADT(1) = -KA*A(1)                   ; Depot compartment
 DADT(2) =  KA*A(1) -(CL/VC)*A(2)     ; Central compartment

$ERROR 
 CONC  = A(2)/VC                      ; Plasmatic concentration of rifampin (ug/mL)
 IPRED = CONC

 IRES  = DV - IPRED
 W     = SQRT((THETA(1)*IPRED)**2 + THETA(2)**2)
 IWRES = IRES/W
 Y     = IPRED+W*EPS(1)

$THETA  
 (0,0.1)      ; Th.1 Prop Error
 (0,0.5)      ; Th.2 Add Error
 (0,5.38750)  ; Th.3 CL (L/h)
 (0,2.845)    ; Th.4 VC (L)
 (0,3)        ; Th.5 Half-life KA (h)

$OMEGA  
 0  FIX       ; Om1.1  CL
 0  FIX       ; Om2.2. VC
 0  FIX       ; Om3.3. KA

$SIGMA  
 1  FIX       ; Sig1.1. Error

$ESTIMATION METHOD=1 SIGL=6 NSIG=2 INTER MAXEVAL=9999 PRINT=1 NOABORT
$COVARIANCE UNCONDITIONAL
$TABLE      ID TIME DOSE MDV IPRED IWRES CWRES NOPRINT ONEHEADER
            FILE=sdtab201
$TABLE      ID TIME CL VC KA ETAS(1:3) NOPRINT ONEHEADER FILE=patab201
