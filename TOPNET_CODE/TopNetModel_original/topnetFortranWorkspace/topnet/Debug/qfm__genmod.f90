        !COMPILER-GENERATED INTERFACE MODULE: Wed Jun 26 16:19:22 2013
        MODULE QFM__genmod
          INTERFACE 
            SUBROUTINE QFM(UB,W,A,TA,PRAIN,PS,WS,RH,QSI,QLI,RKN,IRADFL, &
     &QNETOB,CDH,CDE,RID,PARAM,SITEV,ITSMETHOD,MTIME,FM,Q,QM,MR,QE,E,   &
     &TSURF,TAVE,QH,QNET,DT,REFDEPTH,TOTALREFDEPTH,SMELT)
              REAL(KIND=4) :: UB
              REAL(KIND=4) :: W
              REAL(KIND=4) :: A
              REAL(KIND=4) :: TA
              REAL(KIND=4) :: PRAIN
              REAL(KIND=4) :: PS
              REAL(KIND=4) :: WS
              REAL(KIND=4) :: RH
              REAL(KIND=4) :: QSI
              REAL(KIND=4) :: QLI
              REAL(KIND=4) :: RKN
              INTEGER(KIND=4) :: IRADFL
              REAL(KIND=4) :: QNETOB
              REAL(KIND=4) :: CDH
              REAL(KIND=4) :: CDE
              REAL(KIND=4) :: RID
              REAL(KIND=4) :: PARAM(*)
              REAL(KIND=4) :: SITEV(*)
              INTEGER(KIND=4) :: ITSMETHOD
              REAL(KIND=4) :: MTIME(*)
              REAL(KIND=4) :: FM
              REAL(KIND=4) :: Q
              REAL(KIND=4) :: QM
              REAL(KIND=4) :: MR
              REAL(KIND=4) :: QE
              REAL(KIND=4) :: E
              REAL(KIND=4) :: TSURF
              REAL(KIND=4) :: TAVE
              REAL(KIND=4) :: QH
              REAL(KIND=4) :: QNET
              REAL(KIND=4) :: DT
              REAL(KIND=4) :: REFDEPTH
              REAL(KIND=4) :: TOTALREFDEPTH
              REAL(KIND=4) :: SMELT
            END SUBROUTINE QFM
          END INTERFACE 
        END MODULE QFM__genmod
