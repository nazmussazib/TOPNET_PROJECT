        !COMPILER-GENERATED INTERFACE MODULE: Wed Jun 26 16:19:24 2013
        MODULE BALANCEFLOWSATRESERVOIRS__genmod
          INTERFACE 
            SUBROUTINE BALANCEFLOWSATRESERVOIRS(NUMNODE,NUMLINK,NUMUSER,&
     &NUMRESERVOIR,RESERVOIRNETSTORAGE)
              INTEGER(KIND=4) :: NUMRESERVOIR
              INTEGER(KIND=4) :: NUMNODE
              INTEGER(KIND=4) :: NUMLINK
              INTEGER(KIND=4) :: NUMUSER
              REAL(KIND=4) :: RESERVOIRNETSTORAGE(NUMRESERVOIR)
            END SUBROUTINE BALANCEFLOWSATRESERVOIRS
          END INTERFACE 
        END MODULE BALANCEFLOWSATRESERVOIRS__genmod
