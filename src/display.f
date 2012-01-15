      SUBROUTINE DISPLAY
      IMPLICIT NONE
      CHARACTER*4 ST0
      INTEGER I
c      INCLUDE 'model.txt'
c      INCLUDE 'series.txt'
c      INCLUDE 'conpar.txt'
C*****model.txt
      INTEGER PFI,ARP,VRI,CCV,LYAP,SCC,fct
      DOUBLE PRECISION SCALE,VR,CONST,PHI(20),PRDG
      COMMON/MODEL/SCALE,VR,CONST,PHI,PRDG,PFI,ARP,VRI,CCV,LYAP,SCC,fct
      integer for, fty
      common/model/for, fty
C*****series.txt
      CHARACTER*40 NAME
      INTEGER LEN
      DOUBLE PRECISION TIM(500),SER(500),TDIF(500)
c      COMMON/SERIES/NAME,LEN,TIM,SER,TDIF
      COMMON/SERIES/TIM,SER,TDIF,NAME,LEN
C*****conpar.txt    
      INTEGER NIT,OPM,RGM,KUP,KSP,KST
      DOUBLE PRECISION REQ,CONCRIT,RPERT,IVLAM,FAC,STLAM,SMLAM,GTLAM
c      COMMON/CONPAR/NIT,OPM,RGM,REQ,CONCRIT,RPERT,IVLAM,FAC,STLAM,
c     * SMLAM,GTLAM,KUP,KSP,KST
      COMMON/CONPAR/REQ,CONCRIT,RPERT,IVLAM,FAC,STLAM,
     * SMLAM,GTLAM,NIT,OPM,RGM,KUP,KSP,KST

      integer ARI, tra
      common/model/ari,tra
      
C      CALL NEWLINE
C      CALL NEWLINE
C  DISPLAY MODEL SETUP
C      PRINT *,'INPUT FROM MODEL FILE IS:'
C      CALL NEWLINE
      IF(PFI.EQ.1)THEN
        ST0='QLFA'
      ELSE IF(PFI.EQ.2)THEN
        ST0='QLFS'
      ELSE IF(PFI.EQ.3)THEN
        ST0='DIRA'
      ELSE IF(PFI.EQ.4)THEN
        ST0='DIRS'
      ELSE IF(PFI.EQ.5)THEN
        ST0='MAPA'
      ELSE
        ST0='MAPS'
      END IF
C      PRINT *,'FORM OF PARAMETERIZATION: ',ST0
C      PRINT *,'SCALE: ',SCALE
C      PRINT *,'ORDER OF AUTOREGRESSION: ',ARP
      IF(ARI.EQ.0)THEN
        ST0='NO  '
      ELSE
        ST0='YES '
      END IF
C      PRINT *,'INITIAL VALUES SUPPLIED? ',ST0

      IF(ARI.EQ.1)THEN
C        PRINT *,'VALUES SUPPLIED ARE:'
        DO 100 I=1,ARP
C        PRINT *,I,PHI(I)
  100   CONTINUE
      ELSE
C        PRINT *,'INITIAL VALUES ARE ZERO'
      END IF
      IF(VRI.EQ.0)THEN
        ST0='NO  '
      ELSE
        ST0='YES '
      END IF
C      PRINT *,'OBSERVATION VARIANCE RATIO INCLUDED? ',ST0
      IF(VRI.EQ.1)THEN
C        PRINT *,'OBSERVATION VARIANCE RATIO: ',VR
      END IF
      IF(CCV.EQ.0)THEN
        ST0='NULL'
      ELSE IF(CCV.EQ.1)THEN
        ST0='MNCT'
      ELSE
        ST0='CTES'
      END IF
C      PRINT *,'METHOD OF MEAN ESTIMATION: ',ST0
      IF(CCV.GT.0)THEN
C        PRINT *,'CONSTANT: ',CONST
      END IF        
      IF(LYAP.EQ.0)THEN
        ST0='NO  '
      ELSE
        ST0='YES '
      END IF
C      PRINT *,'STATIONARY STATE ASSUMED AT START ? ',ST0
      IF(LYAP.EQ.0)THEN
C        PRINT *,'STATE VARIANCE AT START SET TO DIAGONAL ',VR
      END IF
      IF(SCC.EQ.0)THEN
        ST0='NO'
      ELSE
        ST0='YES'
      ENDIF
C      PRINT *,'STATIONARITY CONSTRAINT CHECKING ? ',ST0
C  DISPLAY SERIES SETUP
C      CALL NEWLINE
C      CALL NEWLINE
C      PRINT *,'INPUT FROM SERIES FILE IS:'
C      CALL NEWLINE
C      PRINT *,'SERIES NAME: ',NAME
C      PRINT *,'SERIES LENGTH: ',LEN
C  DISPLAY CONTROL PARAMETER SETUP
C      CALL NEWLINE
C      CALL NEWLINE
C      PRINT *,'INPUT FROM CONTROL PARAMETER FILE IS:'
C      CALL NEWLINE
C      PRINT *,'NUMBER OF ITERATIONS: ',NIT
C      PRINT *,'OPTIMISATION METHOD: ',OPM
C      PRINT *,'RESIDUAL GENERATION METHOD: ',RGM
C      PRINT *,'ROOT EQUALITY CRITERION: ',REQ
C      PRINT *,'SUM OF SQUARES CONVERGENCE CRITERION: ',CONCRIT
C      PRINT *,'PARAMETER PERTURBATION MODIFICATION RATIO: ',RPERT
C      PRINT *,'STEP-SIZE CONSTRAINT PARAMETER INITIAL VALUE: ',IVLAM
C      PRINT *,'CONSTRAINED MODIFICATION FACTOR STEP-SIZE: ',FAC
C      PRINT *,'SMALLEST LAMBDA VALUE: ',STLAM
C      PRINT *,'SMALL LAMBDA VALUE: ',SMLAM
C      PRINT *,'GREATEST LAMBDA VALUE: ',GTLAM
C      IF (KST.EQ.1) PRINT *,'STATE ESTIMATE FILE REQUESTED'
C      IF (KSP.EQ.1) PRINT *,'MODEL SPECTRUM FILE REQUESTED'
C      IF (KUP.EQ.1) PRINT *,'UPDATED MODEL FILE REQUESTED'
      CALL NEWLINE
C      PRINT *,'END OF SET-UP PROCESS'
      RETURN
      END
 
