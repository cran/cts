      SUBROUTINE LOOP(ss, bit, ERRNO)
      IMPLICIT NONE
      LOGICAL ADV
      INTEGER I, errno
      DOUBLE PRECISION U,PU,SSNEW
c      INCLUDE 'model.txt'
c      INCLUDE 'series.txt'
c      INCLUDE 'conpar.txt'
c      INCLUDE 'setcon.txt'
c      INCLUDE 'veccom.txt'
c      INCLUDE 'repcom.txt'
C*****model.txt
      INTEGER PFI,ARP,VRI,CCV,LYAP,SCC,fct
      DOUBLE PRECISION SCALE,VR,CONST,PHI(20),PRDG
      COMMON/MODEL/SCALE,VR,CONST,PHI,PRDG,PFI,ARP,VRI,CCV,LYAP,SCC,fct
      integer for, fty
      common/model/for, fty
      integer ARI, tra
      common/model/ari,tra

C*****repcom.txt
      LOGICAL PRPI
      INTEGER REQSW,KFSW
      DOUBLE PRECISION VOB
c      COMMON/REPCOM/PRPI,REQSW,VOB,KFSW
      COMMON/REPCOM/VOB,REQSW,KFSW,PRPI	
C*****conpar.txt    
      INTEGER NIT,OPM,RGM,KUP,KSP,KST
      DOUBLE PRECISION REQ,CONCRIT,RPERT,IVLAM,FAC,STLAM,SMLAM,GTLAM
c      COMMON/CONPAR/NIT,OPM,RGM,REQ,CONCRIT,RPERT,IVLAM,FAC,STLAM,
c     * SMLAM,GTLAM,KUP,KSP,KST
      COMMON/CONPAR/REQ,CONCRIT,RPERT,IVLAM,FAC,STLAM,
     * SMLAM,GTLAM,NIT,OPM,RGM,KUP,KSP,KST

C*****setcon.txt
      LOGICAL CONV,FAIL
      INTEGER NP,ITCT,PPIND
      DOUBLE PRECISION CSO,CSZ,LAM,SSOLD,GMOLD,GMNEW,SIGSQ,OLDB(22)
c      COMMON/SETCON/CONV,FAIL,NP,ITCT,PPIND,CSO,CSZ,LAM,SSOLD,
c     *GMOLD,GMNEW,SIGSQ,OLDB
      COMMON/SETCON/CSO,CSZ,LAM,SSOLD,
     *GMOLD,GMNEW,SIGSQ,OLDB,CONV,FAIL,NP,ITCT,PPIND
C*****veccom.txt
      DOUBLE PRECISION B(22),NEWB(22),PERB(22),DELB(22),
     * ERR(500),RES(500),PRES(22,500),sres(500)
      COMMON/VECCOM/B,NEWB,PERB,DELB,ERR,RES,PRES,sres
C*****series.txt
      CHARACTER*40 NAME
      INTEGER LEN
      DOUBLE PRECISION TIM(500),SER(500),TDIF(500)
c      COMMON/SERIES/NAME,LEN,TIM,SER,TDIF
      COMMON/SERIES/TIM,SER,TDIF,NAME,LEN
C sum of squares and parameters in each iteration      
      DOUBLE PRECISION ss(NIT+1), BIT(NIT+1,22)
      DOUBLE PRECISION WK(20),VT(500),BI(2,20,20),R(2,20,20),
     *RI(2,20,20)
      INTEGER ERRNO1
      COMMON/RESGN2/WK,VT,BI,R,RI,ERRNO1

C  FOR USE IN PERTURBATION CALCULATIONS
      ERRNO=0
      IF(PFI.LT.3)THEN
        PU=CSO
      ELSE
        PU=RPERT*1.0D-3
      END IF
C  START OF MAIN LOOP
      DO 100 ITCT=1,NIT
C  COPY OLDB TO B
      DO 200 I=1,NP
      B(I)=OLDB(I)
  200 CONTINUE
      IF(ITCT.EQ.1)THEN
        CALL REPAR
        IF(PRPI)THEN
C          PRINT *,'ITERATION 0: ROOT WITH POSITIVE REAL PART'
           call rexit('ITERATION 0: ROOT WITH POSITIVE REAL PART')
C          STOP
        ERRNO=1
        RETURN
        END IF
        CALL RESGEN
        SSOLD=CSZ
        DO 201 I=1,LEN
        SSOLD=SSOLD+RES(I)*RES(I)
  201   CONTINUE
        GMOLD=GMNEW
        ss(1)=SSOLD
        IF(tra.EQ.1)THEN
           call intpr('ITERATION 0:', 12, 1, 0)
           call dblepr('LAMBDA = ', 9, LAM, 1)
           call dblepr('   INITIAL SUM OF SQUARES = ', 28, SSOLD, 1)
           call dblepr('   INITIAL PARAMETER VALUES',  27, B, ARP)
C        PRINT *,'ITERATION 0:'
C        PRINT *,'   LAMBDA = ',LAM
C        PRINT *,'   INITIAL SUM OF SQUARES = ',SSOLD
C        PRINT *,'   INITIAL PARAMETER VALUES'
c           call dblepr('  ',  2, B, ARP)
C        DO 202 I=1,ARP
C           call dblepr('  ',  2, B(I), 1)
C        PRINT *,'   ',I,'  ',B(I)
C  202   CONTINUE
        IF(VRI.EQ.1)THEN
          call dblepr('   INITIAL OBSERVATION VARIANCE RATIO = ', 40, 
     * B(ARP+1), 1)
C          PRINT *,'   INITIAL OBSERVATION VARIANCE RATIO = ',B(ARP+1)
C        END IF
        END IF
        ENDIF
        IF(CCV.EQ.2)THEN
          IF(VRI.EQ.1)THEN
            U=B(ARP+2)
          ELSE
            U=B(ARP+1)
          ENDIF
          IF(tra.EQ.1)THEN
           call dblepr('   INITIAL VALUE OF CONSTANT TERM = ', 36, U, 1)
C           PRINT *,'   INITIAL VALUE OF CONSTANT TERM = ',U
          ENDIF
        ENDIF
      END IF
C  CALCULATE PERTURBATION VECTOR
      DO 300 I=1,NP
      PERB(I)=PU
  300 CONTINUE
      IF(PFI.EQ.3.OR.PFI.EQ.4)THEN
        DO 301 I=1,ARP
        PERB(I)=PERB(I)*(SCALE+DABS(B(I)))
  301   CONTINUE
      END IF
      DO 303 PPIND=1,NP
      U=PERB(PPIND)
      B(PPIND)=B(PPIND)+U
      CALL REPAR
      IF(PRPI)THEN
         call NEWLINE
         call intpr('ITERATION ',9, ITCT, 1)
         call rexit('ROOT WITH POSITIVE REAL PART')
C        PRINT *,'ITERATION ',ITCT,': ROOT WITH POSITIVE REAL PART'
        ERRNO=1
        RETURN
C        STOP
      END IF
      CALL RESGEN
      ERRNO = ERRNO1
      DO 302 I=1,LEN
      PRES(PPIND,I)=(PRES(PPIND,I)-RES(I))/U
  302 CONTINUE
      B(PPIND)=OLDB(PPIND)
  303 CONTINUE
      PPIND=0
      CALL REJUCE
  304 CALL SEARCH
      DO 305 I=1,NP
      B(I)=NEWB(I)
  305 CONTINUE
      CALL REPAR
      IF(PRPI)THEN
        ADV=.FALSE.
      ELSE
        CALL RESGEN
        SSNEW=CSZ
        DO 306 I=1,LEN
        SSNEW=SSNEW+RES(I)*RES(I)
  306   CONTINUE
        ADV=SSNEW.LT.SSOLD
      END IF
      IF(ADV)THEN
        LAM=LAM/FAC
        IF(LAM.LT.STLAM)LAM=CSZ
        CONV=LAM.LT.SMLAM.AND.SSNEW.GT.(SSOLD*(CSO-CONCRIT))
        SSOLD=SSNEW
        GMOLD=GMNEW
        DO 307 I=1,NP
        OLDB(I)=NEWB(I)
  307   CONTINUE
        IF(CONV)THEN
          RETURN
        ELSE
          IF(tra.EQ.1)THEN
            call NEWLINE
            call intpr('ITERATION :', 11, ITCT, 1)
            call dblepr('   LAMBDA: ',11, LAM, 1)
           call dblepr('   SUM OF SQUARES = ', 20, SSOLD, 1)
           call dblepr('   PARAMETER VALUES',  19, OLDB, NP)
C          PRINT *,'ITERATION :',ITCT
C          PRINT *,'   LAMBDA: ',LAM
C          PRINT *,'   SUM OF SQUARES: ',SSOLD
C          PRINT *,'   PARAMETER VALUES:'
c           call dblepr('  ',  2, OLDB, NP)
C          DO 308 I=1,NP
C           call dblepr('  ',  2, OLDB(I), 1)
C          PRINT *,'  ',I,'  ',OLDB(I)
C  308     CONTINUE
          END IF
      ss(ITCT+1)=SSOLD
      DO 309 I=1,NP
      BIT(ITCT+1, I)=OLDB(I)
  309 CONTINUE    
          GOTO 100
        END IF
      ELSE
        LAM=LAM*FAC
        FAIL=LAM.GT.GTLAM
        IF(FAIL)THEN
          RETURN
        ELSE
          IF(LAM.LT.IVLAM)LAM=IVLAM
          GOTO 304
        END IF
      END IF
  100 CONTINUE
      RETURN
      END
