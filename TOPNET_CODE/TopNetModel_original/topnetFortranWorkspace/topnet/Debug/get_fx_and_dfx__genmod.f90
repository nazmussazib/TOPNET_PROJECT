        !COMPILER-GENERATED INTERFACE MODULE: Wed Jun 26 16:19:22 2013
        MODULE GET_FX_AND_DFX__genmod
          INTERFACE 
            SUBROUTINE GET_FX_AND_DFX(ZBAR0,AREA,F,K,LAMBDA,Q,NS,FX,DFX,&
     &DT)
              INTEGER(KIND=4) :: NS
              REAL(KIND=8) :: ZBAR0
              REAL(KIND=8) :: AREA(NS)
              REAL(KIND=8) :: F(NS)
              REAL(KIND=8) :: K(NS)
              REAL(KIND=8) :: LAMBDA(NS)
              REAL(KIND=8) :: Q
              REAL(KIND=8) :: FX
              REAL(KIND=8) :: DFX
              INTEGER(KIND=8) :: DT
            END SUBROUTINE GET_FX_AND_DFX
          END INTERFACE 
        END MODULE GET_FX_AND_DFX__genmod
