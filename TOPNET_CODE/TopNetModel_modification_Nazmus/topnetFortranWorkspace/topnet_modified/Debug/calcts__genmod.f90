        !COMPILER-GENERATED INTERFACE MODULE: Sun Jul 21 11:10:45 2013
        MODULE CALCTS__genmod
          INTERFACE 
            SUBROUTINE CALCTS(SI,SP,RP,LINKS,LINKR,LL,NSUB,NCHN,LRG,WRG,&
     &NKA,TL,ATB,PKA,ND,CL,PD,UNITS,IPSUB,IPATB,NTS,REINIT,QM,MODWRT,   &
     &IPRCH,STIM,ISITE,SPEAK,BRAIN,INTERV,M,MI,MPS,MPE,OK,Q4SAVE,XLAT,  &
     &XLONG,STDLON,ELEVTG,BDTBAR,SDATE,SHOUR,TEMPER,DEWP,TRANGE,LLOUT,  &
     &NEQ,NOUT,NBOUT,IBOUT,LAKE_REACH,LZERO,LAKE_AREAS,LAKE_BEACH_SLPS, &
     &LK_LINE,NUM_RAT_VALS,LHEADS,LOFLOWS,INI_LEVELS,UBI,WAI,FRACI,     &
     &WIND2M,BTMIN,BTMAX,BTDEW,BP,BXLAT,BXLON,NTDH,NDUMP,REL,RELFLAG,   &
     &ISHIFT0,MAXINT,MAXGAUGE,MAXSLP,MAXRESPONSE,MAXA,MAXC,MAXCHN,      &
     &MAXRCHAREAS,MAX_LAKES,MAX_LHEADS,MAX_OF_SHIFTS,IDEBUGOUTPUT,      &
     &IDEBUGBASIN,IDEBUGCASE)
              INTEGER(KIND=4) :: MAX_LHEADS
              INTEGER(KIND=4) :: MAX_LAKES
              INTEGER(KIND=4) :: MAXRCHAREAS
              INTEGER(KIND=4) :: MAXCHN
              INTEGER(KIND=4) :: MAXC
              INTEGER(KIND=4) :: MAXA
              INTEGER(KIND=4) :: MAXRESPONSE
              INTEGER(KIND=4) :: MAXSLP
              INTEGER(KIND=4) :: MAXGAUGE
              INTEGER(KIND=4) :: MAXINT
              REAL(KIND=8) :: SI(3,MAXSLP)
              REAL(KIND=8) :: SP(39,MAXSLP)
              REAL(KIND=8) :: RP(4,MAXCHN)
              INTEGER(KIND=4) :: LINKS(2,MAXSLP)
              INTEGER(KIND=4) :: LINKR(4,MAXCHN)
              INTEGER(KIND=4) :: LL(MAXCHN)
              INTEGER(KIND=4) :: NSUB
              INTEGER(KIND=4) :: NCHN
              INTEGER(KIND=4) :: LRG(MAXSLP,MAXGAUGE)
              REAL(KIND=8) :: WRG(MAXSLP,MAXGAUGE)
              INTEGER(KIND=4) :: NKA(MAXSLP)
              REAL(KIND=8) :: TL(MAXSLP)
              REAL(KIND=8) :: ATB(MAXA,MAXSLP)
              REAL(KIND=8) :: PKA(MAXA,MAXSLP)
              INTEGER(KIND=4) :: ND(MAXSLP)
              REAL(KIND=8) :: CL(MAXC,MAXSLP)
              REAL(KIND=8) :: PD(MAXC,MAXSLP)
              REAL(KIND=8) :: UNITS
              INTEGER(KIND=4) :: IPSUB
              INTEGER(KIND=4) :: IPATB
              INTEGER(KIND=4) :: NTS(MAXSLP)
              LOGICAL(KIND=1) :: REINIT
              REAL(KIND=8) :: QM(MAXRESPONSE,MAXINT)
              LOGICAL(KIND=1) :: MODWRT
              INTEGER(KIND=4) :: IPRCH
              INTEGER(KIND=4) :: STIM
              INTEGER(KIND=4) :: ISITE
              LOGICAL(KIND=1) :: SPEAK
              REAL(KIND=8) :: BRAIN(MAXSLP,MAXINT)
              INTEGER(KIND=8) :: INTERV
              INTEGER(KIND=4) :: M
              INTEGER(KIND=4) :: MI
              INTEGER(KIND=4) :: MPS
              INTEGER(KIND=4) :: MPE
              LOGICAL(KIND=1) :: OK
              REAL(KIND=8) :: Q4SAVE(MAXINT,MAXCHN)
              REAL(KIND=8) :: XLAT(*)
              REAL(KIND=8) :: XLONG(*)
              REAL(KIND=8) :: STDLON
              REAL(KIND=8) :: ELEVTG(*)
              REAL(KIND=8) :: BDTBAR(12,MAXSLP)
              INTEGER(KIND=4) :: SDATE
              INTEGER(KIND=4) :: SHOUR
              REAL(KIND=8) :: TEMPER(MAXINT)
              REAL(KIND=8) :: DEWP(MAXINT)
              REAL(KIND=8) :: TRANGE(MAXINT)
              INTEGER(KIND=4) :: LLOUT(MAXCHN)
              INTEGER(KIND=4) :: NEQ
              INTEGER(KIND=4) :: NOUT
              INTEGER(KIND=4) :: NBOUT
              INTEGER(KIND=4) :: IBOUT(MAXSLP)
              INTEGER(KIND=4) :: LAKE_REACH(MAX_LAKES)
              INTEGER(KIND=4) :: LZERO(MAX_LAKES)
              REAL(KIND=4) :: LAKE_AREAS(MAX_LAKES)
              INTEGER(KIND=4) :: LAKE_BEACH_SLPS(MAX_LAKES)
              INTEGER(KIND=4) :: LK_LINE(MAX_LAKES)
              INTEGER(KIND=4) :: NUM_RAT_VALS(MAX_LAKES)
              INTEGER(KIND=4) :: LHEADS(MAX_LAKES,MAX_LHEADS)
              INTEGER(KIND=4) :: LOFLOWS(MAX_LAKES,MAX_LHEADS)
              REAL(KIND=4) :: INI_LEVELS(MAX_LAKES)
              REAL(KIND=4) :: UBI(MAXSLP)
              REAL(KIND=4) :: WAI(MAXSLP)
              REAL(KIND=4) :: FRACI(MAXSLP)
              REAL(KIND=8) :: WIND2M(MAXINT)
              REAL(KIND=8) :: BTMIN(MAXSLP,MAXINT)
              REAL(KIND=8) :: BTMAX(MAXSLP,MAXINT)
              REAL(KIND=8) :: BTDEW(MAXSLP,MAXINT)
              REAL(KIND=8) :: BP(46,MAXSLP)
              REAL(KIND=8) :: BXLAT(*)
              REAL(KIND=8) :: BXLON(*)
              INTEGER(KIND=4) :: NTDH(MAXSLP)
              INTEGER(KIND=4) :: NDUMP
              REAL(KIND=4) :: REL(MAXRCHAREAS)
              INTEGER(KIND=4) :: RELFLAG
              INTEGER(KIND=4) :: ISHIFT0(MAXRCHAREAS)
              INTEGER(KIND=4) :: MAX_OF_SHIFTS
              INTEGER(KIND=4) :: IDEBUGOUTPUT
              INTEGER(KIND=4) :: IDEBUGBASIN
              INTEGER(KIND=4) :: IDEBUGCASE
            END SUBROUTINE CALCTS
          END INTERFACE 
        END MODULE CALCTS__genmod
