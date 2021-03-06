        !COMPILER-GENERATED INTERFACE MODULE: Wed Jun 26 16:19:21 2013
        MODULE LAKE_ROUTE__genmod
          INTERFACE 
            SUBROUTINE LAKE_ROUTE(ILAKE,HQI,Q4,N6,Q5,DEL_T,LAKE_REACH,  &
     &LZERO,LAKE_AREAS,LAKE_BEACH_SLPS,LK_LINE,NUM_RAT_VALS,LHEADS,     &
     &LOFLOWS,INI_LEVELS,MAX_LAKES,MAX_LHEADS)
              INTEGER(KIND=4) :: MAX_LHEADS
              INTEGER(KIND=4) :: MAX_LAKES
              INTEGER(KIND=4) :: N6
              INTEGER(KIND=4) :: ILAKE
              REAL(KIND=8) :: HQI(MAX_LAKES)
              REAL(KIND=8) :: Q4(N6)
              REAL(KIND=8) :: Q5(N6+1)
              INTEGER(KIND=4) :: DEL_T
              INTEGER(KIND=4) :: LAKE_REACH(MAX_LAKES)
              INTEGER(KIND=4) :: LZERO(MAX_LAKES)
              REAL(KIND=4) :: LAKE_AREAS(MAX_LAKES)
              INTEGER(KIND=4) :: LAKE_BEACH_SLPS(MAX_LAKES)
              INTEGER(KIND=4) :: LK_LINE(MAX_LAKES)
              INTEGER(KIND=4) :: NUM_RAT_VALS(MAX_LAKES)
              INTEGER(KIND=4) :: LHEADS(MAX_LAKES,MAX_LHEADS)
              INTEGER(KIND=4) :: LOFLOWS(MAX_LAKES,MAX_LHEADS)
              REAL(KIND=4) :: INI_LEVELS(MAX_LAKES)
            END SUBROUTINE LAKE_ROUTE
          END INTERFACE 
        END MODULE LAKE_ROUTE__genmod
