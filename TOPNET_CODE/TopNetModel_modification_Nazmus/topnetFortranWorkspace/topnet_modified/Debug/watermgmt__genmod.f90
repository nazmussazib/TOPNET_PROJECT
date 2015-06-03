        !COMPILER-GENERATED INTERFACE MODULE: Sun Jul 21 11:10:47 2013
        MODULE WATERMGMT__genmod
          INTERFACE 
            SUBROUTINE WATERMGMT(STARTDATETOPNET,STARTHOURTOPNET,       &
     &TIMESTEP,NSTEPS,RUNOFFTOPNET,BASEFLOWTOPNET,ARTDRAINAGETOPNET,    &
     &VOL_IRRIG_DEMAND,MAXSLP,EVAPORATION,PRECIPITATION,VOLUME_IRRIG_SUP&
     &,GROUNDWATER_TO_TAKE)
              INTEGER(KIND=4) :: MAXSLP
              INTEGER(KIND=4) :: STARTDATETOPNET
              INTEGER(KIND=4) :: STARTHOURTOPNET
              INTEGER(KIND=4) :: TIMESTEP
              INTEGER(KIND=4) :: NSTEPS
              REAL(KIND=8) :: RUNOFFTOPNET(MAXSLP)
              REAL(KIND=8) :: BASEFLOWTOPNET(MAXSLP)
              REAL(KIND=8) :: ARTDRAINAGETOPNET(MAXSLP)
              REAL(KIND=8) :: VOL_IRRIG_DEMAND(MAXSLP)
              REAL(KIND=8) :: EVAPORATION(MAXSLP)
              REAL(KIND=8) :: PRECIPITATION(MAXSLP)
              REAL(KIND=8) :: VOLUME_IRRIG_SUP(MAXSLP)
              REAL(KIND=8) :: GROUNDWATER_TO_TAKE(MAXSLP)
            END SUBROUTINE WATERMGMT
          END INTERFACE 
        END MODULE WATERMGMT__genmod
