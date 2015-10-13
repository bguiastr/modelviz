$PROBLEM    DES parsing test with transit comp
$INPUT      ID TIME AMT DV EVID
$DATA       data.csv IGNORE=@
$ABBREVIATED DERIV2=NO
$SUBROUTINE ADVAN13 TOL=6
$MODEL      COMP=(TRANS1,DEFDOSE)
            COMP=(TRANS2)
            COMP=(TRANS3)
            COMP=(TRANS4)
            COMP=(TRANS5)
            COMP=(CENTRAL,DEFOBS)
            COMP=(PERIPH)
            COMP=(EFFECT)

$PK   
 TVCL  = THETA(1)
 CL    = TVCL*EXP(ETA(1))

 TVVC  = THETA(2)
 VC    = TVVC*EXP(ETA(2))

 TVQ  = THETA(3)
 Q    = TVQ*EXP(ETA(3))

 TVVP  = THETA(4)
 VP    = TVVP*EXP(ETA(4))

 TVKA  = THETA(5)
 KA    = TVKA*EXP(ETA(5))

 TVKT  = THETA(6)
 KT    = TVKT*EXP(ETA(6))

 TVEMAX = THETA(7)
 EMAX   = TVEMAX*EXP(ETA(7))

 TVEC50 = THETA(8)
 EC50   = TVEC50*EXP(ETA(8))

 TVKDIS = THETA(9)
 KDIS   = TVKDIS*EXP(ETA(9))

 TVF1   = THETA(10)
 F1     = TVF1*EXP(ETA(10))

$DES   
 DADT(1) = -KT*A(1)
 DADT(2) =  KT*A(1) -KT*A(2)
 DADT(3) =  KT*A(2) -KT*A(3)
 DADT(4) =  KT*A(3) -KT*A(4)
 DADT(5) =  KT*A(4) -KA*A(5)
 DADT(6) =  KA*A(5) -(CL/VC)*A(6) -(Q/VC)*A(6) +(Q/VP)*A(7)
 DADT(7) =  (Q/VC)*A(6) -(Q/VP)*A(7)
 DADT(8) =  EMAX*A(6)/(EC50+A(6)) - KDIS*A(8)

$ERROR   
 IPRED = A(2)/VC
 IRES  = DV - IPRED
 Y     = IPRED*(1+EPS(1))

$THETA  
 (0,5)     ; CL 
 (0,20)    ; VC
 (0,1)     ; Q 
 (0,50)    ; VP
 (0,1)     ; KA
 (0,3)     ; KT
 (0,5)     ; EMAX
 (0,3)     ; EC50
 (0,1)     ; KDIS
 (0,0.5,1) ; F1
 
$OMEGA  
 0.1       ; CL
 1.5       ; VC
 0.4       ; Q
 0.05      ; VP
 0.4       ; KA
 0.4       ; KT
 0 FIX     ; EMAX
 0 FIX     ; EC50
 0 FIX     ; KDIS
 0.01      ; F1 

$SIGMA  
 0.01      ; Error

$ESTIMATION METHOD=1 SIGL=6 NSIG=2 INTER MAXEVAL=0 PRINT=1 NOABORT
$COVARIANCE UNCONDITIONAL
$TABLE      ID TIME MDV IPRED CWRES NOPRINT ONEHEADER FILE=sdtab300
$TABLE      ID TIME CL VC VP Q KA KT EMAX EC50 KDIS F1 ETAS(1:10) 
            NOPRINT ONEHEADER FILE=patab300
