        !COMPILER-GENERATED INTERFACE MODULE: Wed Jun 26 16:19:21 2013
        MODULE CORRECT__genmod
          INTERFACE 
            SUBROUTINE CORRECT(QMODEL,N,QMEAS,M,T_NOW,QCORR,NFORE,      &
     &BESTSHIFT,BESTSCALE,MAX_SHIFT,MAXDIM)
              INTEGER(KIND=4) :: MAXDIM
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: QMODEL(N)
              REAL(KIND=8) :: QMEAS(MAXDIM)
              INTEGER(KIND=4) :: M
              INTEGER(KIND=4) :: T_NOW
              REAL(KIND=8) :: QCORR(MAXDIM)
              INTEGER(KIND=4) :: NFORE
              INTEGER(KIND=4) :: BESTSHIFT
              REAL(KIND=8) :: BESTSCALE
              INTEGER(KIND=4) :: MAX_SHIFT
            END SUBROUTINE CORRECT
          END INTERFACE 
        END MODULE CORRECT__genmod
