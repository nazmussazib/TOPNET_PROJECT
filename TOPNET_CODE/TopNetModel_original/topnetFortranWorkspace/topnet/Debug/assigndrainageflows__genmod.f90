        !COMPILER-GENERATED INTERFACE MODULE: Wed Jun 26 16:19:25 2013
        MODULE ASSIGNDRAINAGEFLOWS__genmod
          INTERFACE 
            SUBROUTINE ASSIGNDRAINAGEFLOWS(TIMESTEP,NUMDRAINAGE,NUMNODE,&
     &NUMLINK,DRAINAGEORDER,NUMRUNOFF,NUMBASEFLOW,DRAINAGEOUTFLOW)
              INTEGER(KIND=4) :: NUMDRAINAGE
              INTEGER(KIND=4) :: TIMESTEP
              INTEGER(KIND=4) :: NUMNODE
              INTEGER(KIND=4) :: NUMLINK
              INTEGER(KIND=4) :: DRAINAGEORDER(NUMDRAINAGE)
              INTEGER(KIND=4) :: NUMRUNOFF
              INTEGER(KIND=4) :: NUMBASEFLOW
              REAL(KIND=4) :: DRAINAGEOUTFLOW(NUMDRAINAGE)
            END SUBROUTINE ASSIGNDRAINAGEFLOWS
          END INTERFACE 
        END MODULE ASSIGNDRAINAGEFLOWS__genmod
