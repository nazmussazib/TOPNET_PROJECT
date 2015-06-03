        !COMPILER-GENERATED INTERFACE MODULE: Sun Jul 21 11:10:45 2013
        MODULE SNOWLSUB__genmod
          INTERFACE 
            SUBROUTINE SNOWLSUB(YEAR,MONTH,DAY,HOUR,DT,NSTEP,INPT,SITEV,&
     &STATEV,PARAM,IFLAG,DTBAR,NSTEPDAY,CUMP,CUME,CUMMR,OUTV,TSBACKUP,  &
     &TAVEBACKUP,NDEPLETIONPOINTS,DFC,MODELELEMENT)
              INTEGER(KIND=4) :: NDEPLETIONPOINTS
              INTEGER(KIND=4) :: YEAR
              INTEGER(KIND=4) :: MONTH
              INTEGER(KIND=4) :: DAY
              REAL(KIND=4) :: HOUR
              REAL(KIND=4) :: DT
              INTEGER(KIND=4) :: NSTEP
              REAL(KIND=4) :: INPT(7,1)
              REAL(KIND=4) :: SITEV(*)
              REAL(KIND=4) :: STATEV(*)
              REAL(KIND=4) :: PARAM(*)
              INTEGER(KIND=4) :: IFLAG(*)
              REAL(KIND=4) :: DTBAR(12)
              INTEGER(KIND=4) :: NSTEPDAY
              REAL(KIND=4) :: CUMP
              REAL(KIND=4) :: CUME
              REAL(KIND=4) :: CUMMR
              REAL(KIND=4) :: OUTV(*)
              REAL(KIND=4) :: TSBACKUP(*)
              REAL(KIND=4) :: TAVEBACKUP(*)
              REAL(KIND=4) :: DFC(NDEPLETIONPOINTS,2)
              INTEGER(KIND=4) :: MODELELEMENT
            END SUBROUTINE SNOWLSUB
          END INTERFACE 
        END MODULE SNOWLSUB__genmod
