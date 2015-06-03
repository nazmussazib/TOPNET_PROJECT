        !COMPILER-GENERATED INTERFACE MODULE: Wed Jun 26 16:19:22 2013
        MODULE TOPMOD__genmod
          INTERFACE 
            SUBROUTINE TOPMOD(SI,SP,ISUB,NKA,LAMBDA,ATB,PKA,ND,CL,PD,   &
     &UNITS,IRR,MODWRT,IPSUB,IPATB,STIM,SPEAK,R,PET,INTERV,ART_DRAINAGE,&
     &RATE_IRRIG,IMONTH,NDATA,MPS,MPE,QINST_OUT,DR_OUT,NDUMP,NTDH,ISTEP,&
     &MAXC,ZBM,MAXA,MAXSLP,MAXINT,SUMR,SUMQ,SUMAE,S0,Q0,SR,CV,ACIEM,    &
     &ACSEM,SUMPE,SUMIE,SUMQB,SUMCE,SUMSLE,SUMR1,QB,QINST,DR,SUMQV,SUMSE&
     &,ZBAR,TDH,ZR,AK0FZRDT,LOGOQM,QVMIN,DTH,SUMAD,EVAP_MM,QLAT_MM,     &
     &IPFLAG,RIRR)
              INTEGER(KIND=4) :: MAXSLP
              INTEGER(KIND=4) :: MAXA
              INTEGER(KIND=4) :: MAXC
              REAL(KIND=8) :: SI(3,MAXSLP)
              REAL(KIND=8) :: SP(39,MAXSLP)
              INTEGER(KIND=4) :: ISUB
              INTEGER(KIND=4) :: NKA(MAXSLP)
              REAL(KIND=8) :: LAMBDA
              REAL(KIND=8) :: ATB(MAXA,MAXSLP)
              REAL(KIND=8) :: PKA(MAXA,MAXSLP)
              INTEGER(KIND=4) :: ND(MAXSLP)
              REAL(KIND=8) :: CL(MAXC,MAXSLP)
              REAL(KIND=8) :: PD(MAXC,MAXSLP)
              REAL(KIND=8) :: UNITS
              INTEGER(KIND=4) :: IRR(15,1)
              LOGICAL(KIND=1) :: MODWRT
              INTEGER(KIND=4) :: IPSUB
              INTEGER(KIND=4) :: IPATB
              INTEGER(KIND=4) :: STIM
              LOGICAL(KIND=1) :: SPEAK
              REAL(KIND=8) :: R
              REAL(KIND=8) :: PET
              INTEGER(KIND=8) :: INTERV
              REAL(KIND=8) :: ART_DRAINAGE
              REAL(KIND=8) :: RATE_IRRIG
              INTEGER(KIND=4) :: IMONTH
              INTEGER(KIND=4) :: NDATA
              INTEGER(KIND=4) :: MPS
              INTEGER(KIND=4) :: MPE
              REAL(KIND=8) :: QINST_OUT
              REAL(KIND=8) :: DR_OUT
              INTEGER(KIND=4) :: NDUMP
              INTEGER(KIND=4) :: NTDH(MAXSLP)
              INTEGER(KIND=4) :: ISTEP
              REAL(KIND=8) :: ZBM
              INTEGER(KIND=4) :: MAXINT
              REAL(KIND=8) :: SUMR
              REAL(KIND=8) :: SUMQ
              REAL(KIND=8) :: SUMAE
              REAL(KIND=8) :: S0
              REAL(KIND=8) :: Q0
              REAL(KIND=8) :: SR
              REAL(KIND=8) :: CV
              REAL(KIND=8) :: ACIEM
              REAL(KIND=8) :: ACSEM
              REAL(KIND=8) :: SUMPE
              REAL(KIND=8) :: SUMIE
              REAL(KIND=8) :: SUMQB
              REAL(KIND=8) :: SUMCE
              REAL(KIND=8) :: SUMSLE
              REAL(KIND=8) :: SUMR1
              REAL(KIND=8) :: QB
              REAL(KIND=8) :: QINST(450)
              REAL(KIND=8) :: DR(450)
              REAL(KIND=8) :: SUMQV
              REAL(KIND=8) :: SUMSE
              REAL(KIND=8) :: ZBAR
              REAL(KIND=8) :: TDH(450,MAXSLP)
              REAL(KIND=8) :: ZR
              REAL(KIND=8) :: AK0FZRDT
              REAL(KIND=8) :: LOGOQM
              REAL(KIND=8) :: QVMIN
              REAL(KIND=8) :: DTH
              REAL(KIND=8) :: SUMAD
              REAL(KIND=8) :: EVAP_MM
              REAL(KIND=8) :: QLAT_MM
              INTEGER(KIND=4) :: IPFLAG
              REAL(KIND=8) :: RIRR(15,1)
            END SUBROUTINE TOPMOD
          END INTERFACE 
        END MODULE TOPMOD__genmod
