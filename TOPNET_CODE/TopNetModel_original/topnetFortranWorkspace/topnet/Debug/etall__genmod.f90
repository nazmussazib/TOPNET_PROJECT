        !COMPILER-GENERATED INTERFACE MODULE: Wed Jun 26 16:19:21 2013
        MODULE ETALL__genmod
          INTERFACE 
            SUBROUTINE ETALL(XLAT,XLONG,STDLON,ELEVTG,DTBAR,EVAP,TEMP,  &
     &DEWP,TRANGE,ELEVSB,ALBEDO,RLAPSE,SDATE,SHOUR,DTSEC,M,ISTEP,IYEAR, &
     &MONTH,IDAY,IHR,IMM,ISEC,HOUR1,TMIN,TMAX,WIND2M,METHOD)
              REAL(KIND=4) :: XLAT
              REAL(KIND=4) :: XLONG
              REAL(KIND=4) :: STDLON
              REAL(KIND=4) :: ELEVTG
              REAL(KIND=4) :: DTBAR(12)
              REAL(KIND=4) :: EVAP
              REAL(KIND=4) :: TEMP
              REAL(KIND=4) :: DEWP
              REAL(KIND=4) :: TRANGE
              REAL(KIND=4) :: ELEVSB
              REAL(KIND=4) :: ALBEDO
              REAL(KIND=4) :: RLAPSE
              INTEGER(KIND=4) :: SDATE
              INTEGER(KIND=4) :: SHOUR
              INTEGER(KIND=8) :: DTSEC
              INTEGER(KIND=4) :: M
              INTEGER(KIND=4) :: ISTEP
              INTEGER(KIND=4) :: IYEAR
              INTEGER(KIND=4) :: MONTH
              INTEGER(KIND=4) :: IDAY
              INTEGER(KIND=4) :: IHR
              INTEGER(KIND=4) :: IMM
              INTEGER(KIND=4) :: ISEC
              REAL(KIND=4) :: HOUR1
              REAL(KIND=4) :: TMIN
              REAL(KIND=4) :: TMAX
              REAL(KIND=4) :: WIND2M
              INTEGER(KIND=4) :: METHOD
            END SUBROUTINE ETALL
          END INTERFACE 
        END MODULE ETALL__genmod
