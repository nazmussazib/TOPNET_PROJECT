        !COMPILER-GENERATED INTERFACE MODULE: Sun Jul 21 11:10:45 2013
        MODULE SNOW__genmod
          INTERFACE 
            SUBROUTINE SNOW(TEMPER,ELEVTG,ELEVSB,RLAPSE,BRAIN,DDF,SNOWST&
     &,DT,M,NSUB,JS,IT,MAXSLP,MAXINT)
              INTEGER(KIND=4) :: MAXINT
              REAL(KIND=8) :: TEMPER(MAXINT)
              REAL(KIND=8) :: ELEVTG
              REAL(KIND=8) :: ELEVSB
              REAL(KIND=8) :: RLAPSE
              REAL(KIND=8) :: BRAIN
              REAL(KIND=8) :: DDF
              REAL(KIND=8) :: SNOWST
              INTEGER(KIND=8) :: DT
              INTEGER(KIND=4) :: M
              INTEGER(KIND=4) :: NSUB
              INTEGER(KIND=4) :: JS
              INTEGER(KIND=4) :: IT
              INTEGER(KIND=4) :: MAXSLP
            END SUBROUTINE SNOW
          END INTERFACE 
        END MODULE SNOW__genmod
