        !COMPILER-GENERATED INTERFACE MODULE: Sun Jul 21 11:10:43 2013
        MODULE RAINFILL_DOIT__genmod
          INTERFACE 
            SUBROUTINE RAINFILL_DOIT(TRAIN,IT,NGAUGE,RCOEFF,FSITE,NFILL,&
     &I_RESET_FLAG,IT_SAVE_OLD,MAXGAUGE)
              INTEGER(KIND=4) :: MAXGAUGE
              REAL(KIND=8) :: TRAIN(MAXGAUGE)
              INTEGER(KIND=4) :: IT
              INTEGER(KIND=4) :: NGAUGE
              REAL(KIND=4) :: RCOEFF(MAXGAUGE,MAXGAUGE)
              INTEGER(KIND=4) :: FSITE(MAXGAUGE,MAXGAUGE)
              INTEGER(KIND=4) :: NFILL(MAXGAUGE)
              LOGICAL(KIND=4) :: I_RESET_FLAG
              INTEGER(KIND=4) :: IT_SAVE_OLD
            END SUBROUTINE RAINFILL_DOIT
          END INTERFACE 
        END MODULE RAINFILL_DOIT__genmod
