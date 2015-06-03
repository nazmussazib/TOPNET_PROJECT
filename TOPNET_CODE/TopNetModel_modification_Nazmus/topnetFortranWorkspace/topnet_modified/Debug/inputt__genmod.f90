        !COMPILER-GENERATED INTERFACE MODULE: Sun Jul 21 11:10:45 2013
        MODULE INPUTT__genmod
          INTERFACE 
            SUBROUTINE INPUTT(INITT,IEND,NEQ,QACT,ACTIME,MODELID,NPAR,  &
     &NRX,IEX)
              INTEGER(KIND=4) :: IEX
              INTEGER(KIND=4) :: NRX
              INTEGER(KIND=4) :: INITT(IEX)
              INTEGER(KIND=4) :: IEND(IEX)
              INTEGER(KIND=4) :: NEQ
              REAL(KIND=8) :: QACT(NRX,IEX)
              REAL(KIND=8) :: ACTIME(NRX)
              CHARACTER(LEN=30) :: MODELID
              INTEGER(KIND=4) :: NPAR
            END SUBROUTINE INPUTT
          END INTERFACE 
        END MODULE INPUTT__genmod
