        !COMPILER-GENERATED INTERFACE MODULE: Wed Jun 26 16:19:24 2013
        MODULE BALANCEFLOWSATSTREAMNODES__genmod
          INTERFACE 
            SUBROUTINE BALANCEFLOWSATSTREAMNODES(NUMNODE,NUMLINK,       &
     &DRAINAGEORDER,NUMDRAINAGE,DRAINAGEOUTFLOW)
              INTEGER(KIND=4) :: NUMDRAINAGE
              INTEGER(KIND=4) :: NUMNODE
              INTEGER(KIND=4) :: NUMLINK
              INTEGER(KIND=4) :: DRAINAGEORDER(NUMDRAINAGE)
              REAL(KIND=4) :: DRAINAGEOUTFLOW(NUMDRAINAGE)
            END SUBROUTINE BALANCEFLOWSATSTREAMNODES
          END INTERFACE 
        END MODULE BALANCEFLOWSATSTREAMNODES__genmod
