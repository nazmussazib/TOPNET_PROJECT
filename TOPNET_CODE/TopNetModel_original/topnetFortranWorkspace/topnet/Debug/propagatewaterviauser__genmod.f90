        !COMPILER-GENERATED INTERFACE MODULE: Wed Jun 26 16:19:24 2013
        MODULE PROPAGATEWATERVIAUSER__genmod
          INTERFACE 
            SUBROUTINE PROPAGATEWATERVIAUSER(I,J,QTRY,NUMNODE,NUMLINK,  &
     &NUMUSER,NUMRESERVOIR,NUMSOURCE,DRAINAGEORDER,NUMDRAINAGE,         &
     &NUMRETURNFLOW,IFEASIBLE,CAPACITY)
              INTEGER(KIND=4) :: NUMDRAINAGE
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: J
              REAL(KIND=4) :: QTRY
              INTEGER(KIND=4) :: NUMNODE
              INTEGER(KIND=4) :: NUMLINK
              INTEGER(KIND=4) :: NUMUSER
              INTEGER(KIND=4) :: NUMRESERVOIR
              INTEGER(KIND=4) :: NUMSOURCE
              INTEGER(KIND=4) :: DRAINAGEORDER(NUMDRAINAGE)
              INTEGER(KIND=4) :: NUMRETURNFLOW
              INTEGER(KIND=4) :: IFEASIBLE
              REAL(KIND=4) :: CAPACITY
            END SUBROUTINE PROPAGATEWATERVIAUSER
          END INTERFACE 
        END MODULE PROPAGATEWATERVIAUSER__genmod
