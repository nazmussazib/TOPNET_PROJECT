        !COMPILER-GENERATED INTERFACE MODULE: Wed Jun 26 16:19:24 2013
        MODULE TIMECALCS__genmod
          INTERFACE 
            SUBROUTINE TIMECALCS(TIMESTEP,DT,I8STARTSECS,DOY,THISMONTH, &
     &THISDAY,YYYYMMDD,HHMMSS)
              INTEGER(KIND=4) :: TIMESTEP
              INTEGER(KIND=4) :: DT
              INTEGER(KIND=8) :: I8STARTSECS
              INTEGER(KIND=4) :: DOY
              INTEGER(KIND=4) :: THISMONTH
              INTEGER(KIND=4) :: THISDAY
              INTEGER(KIND=4) :: YYYYMMDD
              INTEGER(KIND=4) :: HHMMSS
            END SUBROUTINE TIMECALCS
          END INTERFACE 
        END MODULE TIMECALCS__genmod