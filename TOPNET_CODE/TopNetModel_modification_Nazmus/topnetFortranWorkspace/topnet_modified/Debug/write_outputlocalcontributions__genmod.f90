        !COMPILER-GENERATED INTERFACE MODULE: Sun Jul 21 11:10:47 2013
        MODULE WRITE_OUTPUTLOCALCONTRIBUTIONS__genmod
          INTERFACE 
            SUBROUTINE WRITE_OUTPUTLOCALCONTRIBUTIONS(NUMSTREAMNODE,    &
     &NUMDRAINAGE,BASEFLOWTOPNET,RUNOFFTOPNET,TIMESTEP,SCALEFACTOR)
              INTEGER(KIND=4) :: NUMDRAINAGE
              INTEGER(KIND=4) :: NUMSTREAMNODE
              REAL(KIND=8) :: BASEFLOWTOPNET(NUMDRAINAGE)
              REAL(KIND=8) :: RUNOFFTOPNET(NUMDRAINAGE)
              INTEGER(KIND=4) :: TIMESTEP
              REAL(KIND=8) :: SCALEFACTOR
            END SUBROUTINE WRITE_OUTPUTLOCALCONTRIBUTIONS
          END INTERFACE 
        END MODULE WRITE_OUTPUTLOCALCONTRIBUTIONS__genmod
