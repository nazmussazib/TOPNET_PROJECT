        !COMPILER-GENERATED INTERFACE MODULE: Wed Jun 26 16:19:21 2013
        MODULE INDEPTH__genmod
          INTERFACE 
            SUBROUTINE INDEPTH(NRCH,N0RCH,LL,LINKR,SP,RP,SI,BIGN,HI,QI, &
     &SQI,LAKE_REACH,LZERO,LAKE_AREAS,LAKE_BEACH_SLPS,LK_LINE,          &
     &NUM_RAT_VALS,LHEADS,LOFLOWS,INI_LEVELS,MAX_LAKES,MAX_LHEADS,MAXCHN&
     &,MAXSLP)
              INTEGER(KIND=4) :: MAXSLP
              INTEGER(KIND=4) :: MAXCHN
              INTEGER(KIND=4) :: MAX_LHEADS
              INTEGER(KIND=4) :: MAX_LAKES
              INTEGER(KIND=4) :: NRCH
              INTEGER(KIND=4) :: N0RCH
              INTEGER(KIND=4) :: LL(MAXCHN)
              INTEGER(KIND=4) :: LINKR(4,MAXCHN)
              REAL(KIND=8) :: SP(39,MAXSLP)
              REAL(KIND=8) :: RP(4,MAXCHN)
              REAL(KIND=8) :: SI(3,MAXSLP)
              REAL(KIND=8) :: BIGN
              REAL(KIND=8) :: HI(MAXCHN)
              REAL(KIND=8) :: QI(MAXSLP)
              REAL(KIND=8) :: SQI(MAX_LAKES)
              INTEGER(KIND=4) :: LAKE_REACH(MAX_LAKES)
              INTEGER(KIND=4) :: LZERO(MAX_LAKES)
              REAL(KIND=4) :: LAKE_AREAS(MAX_LAKES)
              INTEGER(KIND=4) :: LAKE_BEACH_SLPS(MAX_LAKES)
              INTEGER(KIND=4) :: LK_LINE(MAX_LAKES)
              INTEGER(KIND=4) :: NUM_RAT_VALS(MAX_LAKES)
              INTEGER(KIND=4) :: LHEADS(MAX_LAKES,MAX_LHEADS)
              INTEGER(KIND=4) :: LOFLOWS(MAX_LAKES,MAX_LHEADS)
              REAL(KIND=4) :: INI_LEVELS(MAX_LAKES)
            END SUBROUTINE INDEPTH
          END INTERFACE 
        END MODULE INDEPTH__genmod