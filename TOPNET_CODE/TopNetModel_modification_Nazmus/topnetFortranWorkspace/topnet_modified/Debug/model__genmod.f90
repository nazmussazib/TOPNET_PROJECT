        !COMPILER-GENERATED INTERFACE MODULE: Sun Jul 21 11:10:45 2013
        MODULE MODEL__genmod
          INTERFACE 
            SUBROUTINE MODEL(IT,IFLAG,IOPT,PRT,NEQ,NPAR,NPX,IEX,NFOR,   &
     &QFIT,DFIT,PAR,MFIT,IFIT,IBEALE)
              INTEGER(KIND=4) :: IEX
              INTEGER(KIND=4) :: NPX
              INTEGER(KIND=4) :: IT
              INTEGER(KIND=4) :: IFLAG
              INTEGER(KIND=4) :: IOPT
              CHARACTER(LEN=1) :: PRT
              INTEGER(KIND=4) :: NEQ
              INTEGER(KIND=4) :: NPAR
              INTEGER(KIND=4) :: NFOR
              REAL(KIND=8) :: QFIT(IEX)
              REAL(KIND=8) :: DFIT(IEX,NPX)
              REAL(KIND=8) :: PAR(NPX)
              INTEGER(KIND=4) :: MFIT(IEX)
              INTEGER(KIND=4) :: IFIT(NPX)
              INTEGER(KIND=4) :: IBEALE
            END SUBROUTINE MODEL
          END INTERFACE 
        END MODULE MODEL__genmod
