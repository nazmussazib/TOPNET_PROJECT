        !COMPILER-GENERATED INTERFACE MODULE: Wed Jun 26 16:19:21 2013
        MODULE CLIPARAM__genmod
          INTERFACE 
            SUBROUTINE CLIPARAM(XLAT,XLONG,STDLON,ELEVTG,DTBAR,NS_TEMPER&
     &,TEMPER_ID)
              REAL(KIND=8) :: XLAT(*)
              REAL(KIND=8) :: XLONG(*)
              REAL(KIND=8) :: STDLON
              REAL(KIND=8) :: ELEVTG(*)
              REAL(KIND=8) :: DTBAR(12,*)
              INTEGER(KIND=4) :: NS_TEMPER
              INTEGER(KIND=4) :: TEMPER_ID(*)
            END SUBROUTINE CLIPARAM
          END INTERFACE 
        END MODULE CLIPARAM__genmod
