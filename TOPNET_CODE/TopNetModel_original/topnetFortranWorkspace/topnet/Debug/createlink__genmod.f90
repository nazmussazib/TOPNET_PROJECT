        !COMPILER-GENERATED INTERFACE MODULE: Wed Jun 26 16:19:25 2013
        MODULE CREATELINK__genmod
          INTERFACE 
            SUBROUTINE CREATELINK(NUMLINK,TITLE,LINKCODE,INTEXTCODE,    &
     &USNODE,DSNODE,FLOW,RETURNFLOWID)
              INTEGER(KIND=4) :: NUMLINK
              CHARACTER(LEN=6) :: TITLE
              INTEGER(KIND=4) :: LINKCODE
              INTEGER(KIND=4) :: INTEXTCODE
              INTEGER(KIND=4) :: USNODE
              INTEGER(KIND=4) :: DSNODE
              REAL(KIND=4) :: FLOW
              INTEGER(KIND=4) :: RETURNFLOWID
            END SUBROUTINE CREATELINK
          END INTERFACE 
        END MODULE CREATELINK__genmod
