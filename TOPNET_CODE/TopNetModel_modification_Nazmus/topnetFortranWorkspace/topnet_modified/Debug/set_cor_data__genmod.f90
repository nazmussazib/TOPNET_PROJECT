        !COMPILER-GENERATED INTERFACE MODULE: Sun Jul 21 11:10:45 2013
        MODULE SET_COR_DATA__genmod
          INTERFACE 
            SUBROUTINE SET_COR_DATA(LINKR,NEQ,NOUT,NRCH,NS,KLLOUT,FLOW, &
     &SPD,BAREA,LL,LLOUT,DFLOW,SUMA,KNT,IREACH,LOUT,MAXINT,MAXSLP,MAXCHN&
     &,MAXRESPONSE)
              INTEGER(KIND=4) :: MAXRESPONSE
              INTEGER(KIND=4) :: MAXCHN
              INTEGER(KIND=4) :: MAXSLP
              INTEGER(KIND=4) :: MAXINT
              INTEGER(KIND=4) :: LINKR(3,MAXCHN)
              INTEGER(KIND=4) :: NEQ
              INTEGER(KIND=4) :: NOUT
              INTEGER(KIND=4) :: NRCH
              INTEGER(KIND=4) :: NS
              INTEGER(KIND=4) :: KLLOUT(MAXRESPONSE)
              REAL(KIND=8) :: FLOW(MAXRESPONSE,MAXINT)
              REAL(KIND=8) :: SPD(39,MAXSLP)
              REAL(KIND=8) :: BAREA(MAXCHN)
              INTEGER(KIND=4) :: LL(MAXCHN)
              INTEGER(KIND=4) :: LLOUT(MAXCHN)
              REAL(KIND=8) :: DFLOW(MAXRESPONSE)
              REAL(KIND=8) :: SUMA(MAXRESPONSE)
              INTEGER(KIND=4) :: KNT(MAXRESPONSE)
              INTEGER(KIND=4) :: IREACH(MAXSLP)
              INTEGER(KIND=4) :: LOUT(MAXRESPONSE)
            END SUBROUTINE SET_COR_DATA
          END INTERFACE 
        END MODULE SET_COR_DATA__genmod
