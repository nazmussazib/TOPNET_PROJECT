        !COMPILER-GENERATED INTERFACE MODULE: Sun Jul 21 11:10:43 2013
        MODULE SNOWUEB2__genmod
          INTERFACE 
            SUBROUTINE SNOWUEB2(DT,NT,INPUT,SITEV,STATEV,TSPREVDAY,     &
     &TAVEPREVDAY,NSTEPDAY,PARAM,IFLAG,CUMP,CUME,CUMMR,OUTV,MTIME)
              INTEGER(KIND=4) :: NT
              REAL(KIND=4) :: DT
              REAL(KIND=4) :: INPUT(7,NT)
              REAL(KIND=4) :: SITEV(*)
              REAL(KIND=4) :: STATEV(*)
              REAL(KIND=4) :: TSPREVDAY(*)
              REAL(KIND=4) :: TAVEPREVDAY(*)
              INTEGER(KIND=4) :: NSTEPDAY
              REAL(KIND=4) :: PARAM(*)
              INTEGER(KIND=4) :: IFLAG(*)
              REAL(KIND=4) :: CUMP
              REAL(KIND=4) :: CUME
              REAL(KIND=4) :: CUMMR
              REAL(KIND=4) :: OUTV(*)
              REAL(KIND=4) :: MTIME(*)
            END SUBROUTINE SNOWUEB2
          END INTERFACE 
        END MODULE SNOWUEB2__genmod
