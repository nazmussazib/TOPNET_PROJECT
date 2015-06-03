        !COMPILER-GENERATED INTERFACE MODULE: Sun Jul 21 11:10:44 2013
        MODULE ET__genmod
          INTERFACE 
            SUBROUTINE ET(IYEAR,MONTH,IDAY,HOUR1,DT,ALBEDO,ELEVSB,ELEVTG&
     &,RLAPSE,TEMP,DEWP,TRANGE,XLAT,XLONG,STDLON,DTBAR,RET,TMIN,TMAX,   &
     &WIND2M,METHOD)
              INTEGER(KIND=4) :: IYEAR
              INTEGER(KIND=4) :: MONTH
              INTEGER(KIND=4) :: IDAY
              REAL(KIND=4) :: HOUR1
              REAL(KIND=4) :: DT
              REAL(KIND=4) :: ALBEDO
              REAL(KIND=4) :: ELEVSB
              REAL(KIND=4) :: ELEVTG
              REAL(KIND=4) :: RLAPSE
              REAL(KIND=4) :: TEMP
              REAL(KIND=4) :: DEWP
              REAL(KIND=4) :: TRANGE
              REAL(KIND=4) :: XLAT
              REAL(KIND=4) :: XLONG
              REAL(KIND=4) :: STDLON
              REAL(KIND=4) :: DTBAR(12)
              REAL(KIND=4) :: RET
              REAL(KIND=4) :: TMIN
              REAL(KIND=4) :: TMAX
              REAL(KIND=4) :: WIND2M
              INTEGER(KIND=4) :: METHOD
            END SUBROUTINE ET
          END INTERFACE 
        END MODULE ET__genmod
