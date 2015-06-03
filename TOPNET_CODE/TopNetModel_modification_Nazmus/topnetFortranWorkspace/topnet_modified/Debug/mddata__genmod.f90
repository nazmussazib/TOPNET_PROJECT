        !COMPILER-GENERATED INTERFACE MODULE: Sun Jul 21 11:10:44 2013
        MODULE MDDATA__genmod
          INTERFACE 
            SUBROUTINE MDDATA(NGAUGE,NS,NRCH,NKA,TL,ATB,PKA,ND,CL2,PD2, &
     &UNITS,LL,NTR,NTS,LINKS,LINKR,SI,SP,RP,IRET,PMAP,NPAR,LRG,WRG,IEX, &
     &LLOUT,NEQ,NOUT,NBOUT,IBOUT,NRCHSAV,QMAP,REL,RELFLAG,MINSP,MAXSP,  &
     &MINSI,MAXSI,MINRP,MAXRP,LIMITC,CLINKR,KLLOUT,ISHIFT0,MAXINT,      &
     &MAXGAUGE,MAXSLP,MAXRESPONSE,MAXA,MAXC,MAXCHN,MAXRCHAREAS,MAXSITES,&
     &BP,BXLAT,BXLON,WRG1)
              INTEGER(KIND=4) :: MAXSITES
              INTEGER(KIND=4) :: MAXRCHAREAS
              INTEGER(KIND=4) :: MAXCHN
              INTEGER(KIND=4) :: MAXC
              INTEGER(KIND=4) :: MAXA
              INTEGER(KIND=4) :: MAXRESPONSE
              INTEGER(KIND=4) :: MAXSLP
              INTEGER(KIND=4) :: MAXGAUGE
              INTEGER(KIND=4) :: NGAUGE
              INTEGER(KIND=4) :: NS
              INTEGER(KIND=4) :: NRCH
              INTEGER(KIND=4) :: NKA(MAXSLP)
              REAL(KIND=8) :: TL(MAXSLP)
              REAL(KIND=8) :: ATB(MAXA,MAXSLP)
              REAL(KIND=8) :: PKA(MAXA,MAXSLP)
              INTEGER(KIND=4) :: ND(MAXSLP)
              REAL(KIND=8) :: CL2(MAXC,MAXSLP)
              REAL(KIND=8) :: PD2(MAXC,MAXSLP)
              REAL(KIND=8) :: UNITS
              INTEGER(KIND=4) :: LL(MAXCHN)
              INTEGER(KIND=4) :: NTR(MAXCHN)
              INTEGER(KIND=4) :: NTS(MAXSLP)
              INTEGER(KIND=4) :: LINKS(2,MAXSLP)
              INTEGER(KIND=4) :: LINKR(4,MAXCHN)
              REAL(KIND=8) :: SI(3,MAXSLP)
              REAL(KIND=8) :: SP(39,MAXSLP)
              REAL(KIND=8) :: RP(4,MAXCHN)
              INTEGER(KIND=4) :: IRET
              INTEGER(KIND=4) :: PMAP(4,17)
              INTEGER(KIND=4) :: NPAR
              INTEGER(KIND=4) :: LRG(MAXSLP,MAXGAUGE)
              REAL(KIND=8) :: WRG(MAXSLP,MAXGAUGE)
              INTEGER(KIND=4) :: IEX
              INTEGER(KIND=4) :: LLOUT(MAXCHN)
              INTEGER(KIND=4) :: NEQ
              INTEGER(KIND=4) :: NOUT
              INTEGER(KIND=4) :: NBOUT
              INTEGER(KIND=4) :: IBOUT(MAXSLP)
              INTEGER(KIND=4) :: NRCHSAV
              INTEGER(KIND=4) :: QMAP(MAXRESPONSE)
              REAL(KIND=4) :: REL(MAXRCHAREAS)
              INTEGER(KIND=4) :: RELFLAG
              REAL(KIND=8) :: MINSP(39)
              REAL(KIND=8) :: MAXSP(39)
              REAL(KIND=8) :: MINSI(3)
              REAL(KIND=8) :: MAXSI(3)
              REAL(KIND=8) :: MINRP(4)
              REAL(KIND=8) :: MAXRP(4)
              LOGICAL(KIND=4) :: LIMITC
              INTEGER(KIND=4) :: CLINKR(3,MAXCHN)
              INTEGER(KIND=4) :: KLLOUT(MAXRESPONSE)
              INTEGER(KIND=4) :: ISHIFT0(MAXRCHAREAS)
              INTEGER(KIND=4) :: MAXINT
              REAL(KIND=8) :: BP(46,MAXSLP)
              REAL(KIND=8) :: BXLAT(MAXSLP)
              REAL(KIND=8) :: BXLON(MAXSLP)
              REAL(KIND=8) :: WRG1(MAXSLP,MAXGAUGE)
            END SUBROUTINE MDDATA
          END INTERFACE 
        END MODULE MDDATA__genmod
