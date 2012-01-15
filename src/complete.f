      SUBROUTINE COMPLETE(SS, BIT)
      IMPLICIT NONE
      INTEGER I,J,K
      DOUBLE PRECISION U
c      INCLUDE 'model.txt'
c      INCLUDE 'conpar.txt'
c      INCLUDE 'setcon.txt'
c      INCLUDE 'series.txt'
c      INCLUDE 'veccom.txt'
c      INCLUDE 'redcom.txt'
c      INCLUDE 'seacom.txt'
c      INCLUDE 'repar3.txt'
C*****model.txt
      INTEGER PFI,ARP,VRI,CCV,LYAP,SCC,fct
      DOUBLE PRECISION SCALE,VR,CONST,PHI(20),PRDG
      COMMON/MODEL/SCALE,VR,CONST,PHI,PRDG,PFI,ARP,VRI,CCV,LYAP,SCC,fct
      integer for, fty
      common/model/for, fty
      integer ARI, tra
      common/model/ari,tra

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
C*****series.txt
      CHARACTER*40 NAME
      INTEGER LEN
      DOUBLE PRECISION TIM(500),SER(500),TDIF(500)
c      COMMON/SERIES/NAME,LEN,TIM,SER,TDIF
      COMMON/SERIES/TIM,SER,TDIF,NAME,LEN
C*****veccom.txt
      DOUBLE PRECISION B(22),NEWB(22),PERB(22),DELB(22),
     * ERR(500),RES(500),PRES(22,500),sres(500)
      COMMON/VECCOM/B,NEWB,PERB,DELB,ERR,RES,PRES,sres
C*****redcom.txt
      DOUBLE PRECISION GRAD(22),SSP(22,22)
      COMMON/REDCOM/GRAD,SSP
C*****seacom.txt
      LOGICAL SOLV
      DOUBLE PRECISION ESSP(22,22),ECOV(22,22)
c      COMMON/SEACOM/SOLV,ESSP,ECOV
      COMMON/SEACOM/ESSP,ECOV,SOLV

C*****repar3.txt
      DOUBLE PRECISION ALPHA(21),ROOTR(20),ROOTI(20),XVECR(20),
     *XVECI(20)
      COMMON/REPAR3/ALPHA,ROOTR,ROOTI,XVECR,XVECI
      DOUBLE PRECISION SS, BIT(22) 

C      CALL NEWLINE
      if(tra.EQ.1)then
        call NEWLINE
        call intpr('PROCESSING COMPLETED', 20, 1, 0) 
      end if
      IF(ITCT.GE.NIT)THEN
        I=NIT
      ELSE
        I=ITCT
      END IF
      if(tra.EQ.1)then
       call intpr('ITERATIONS COMPLETED:', 21, I, 1)
C      end if
      IF(CONV)THEN
        call intpr('CONVERGENCE ACHIEVED', 20, 1, 0)
      ELSE
        call intpr('CONVERGENCE NOT ACHIEVED', 24, 1, 0)
      END IF
      IF(FAIL)THEN
        call intpr('SEARCH FAILED TO PROGRESS', 25, 1, 0)
      ELSE
C       if(tra.EQ.1)then
        call intpr('SEARCH PROGRESSED', 17, 1, 0) 
C       end if
      END IF
      END IF
      DO 100 I=1,NP
      DELB(I)=0.0D0
      DO 100 J=1,NP
      ESSP(I,J)=SSP(I,J)
  100 CONTINUE
      CALL SIMI
      K=LEN-ARP
      IF(VRI.EQ.1)K=K-1
      IF(CCV.EQ.2)K=K-1
      U=SSOLD/K
      SIGSQ=U/GMOLD
      DO 101 I=1,NP
      DO 101 J=1,NP
      ESSP(I,J)=ESSP(I,J)*U  
  101 CONTINUE
c added by Z.W.
c      OPEN(UNIT=4,FILE='cov.dat',STATUS='NEW')
c      OPEN(UNIT=4,FILE='cov.dat',STATUS='unknown')
c      write(4,95) ((ESSP(I,J),J=1,NP-1),I=1,NP-1)
c   95 FORMAT(5E16.8)
c Note: NP-th parameter is the constant estimator in the last line
c      close(unit=4)
C     for model selection purpose, find covariance matrix ECOV
      DO 303 I=1,ARP
      DO 303 J=1,ARP
      ECOV(I,J)=ESSP(I,J)
  303 CONTINUE                                  
c end by Z.W.   
      DO 102 I=1,NP
      DELB(I)=DSQRT(ESSP(I,I))
  102 CONTINUE
      DO 103 I=1,NP
      U=DELB(I)
      DO 103 J=1,NP
      ESSP(I,J)=ESSP(I,J)/(U*DELB(J))
  103 CONTINUE
      if(tra.EQ.1)then
      CALL NEWLINE
       call dblepr('FINAL SUM OF SQUARES: ', 22, SSOLD, 1)
       call dblepr('MEAN SUM OF SQUARES : ', 22, SIGSQ*GMOLD, 1)
       call dblepr('INNOVATION PROCESS VARIANCE ESTIMATE: ',38, SIGSQ,
     *1) 
       call dblepr('GEOMETRIC MEAN VARIANCE MULTIPLIER:   ',38, GMOLD,
     *1) 
       call dblepr('FINAL PARAMETER VALUES:',23,  OLDB, NP)
      end if
      DO 104 I=1,NP
      B(I)=OLDB(I)
C      if(tra.EQ.1)then
C      PRINT *,'   ',I,'  ',B(I),'   +/- ',DELB(I)
C      end if
  104 CONTINUE
      if(tra.EQ.1)then
      CALL NEWLINE
C      PRINT *,'CORRELATION MATRIX:'
c      DO 106 I=1,NP
c      WRITE(6,105)(ESSP(I,J),J=1,I)
c  105 FORMAT(1H ,7(D10.3,2X))
c  106 CONTINUE
      end if
      SS = SSOLD
      DO 107 I=1,NP
      BIT(I)=OLDB(I)
  107 CONTINUE
c      OPEN(UNIT=4,FILE='parest.dat',STATUS='NEW')
c      open(unit=4,file='parest.dat',status='unknown')
c      WRITE(4,195)
c  195 FORMAT('MOD.PAR.IND., ORDER, NO.PAR., VAR.RAT.IND., CON.IND.')
c      WRITE(4,196) PFI,ARP,NP,VRI,CCV
c  196 FORMAT(5I10)
c      WRITE(4,197)
c  197 FORMAT('SCALE , INNOVATION VARIANCE')
c      WRITE(4,198) SCALE,SIGSQ
c  198 FORMAT(5E16.8)
c      WRITE(4,199)
c  199 FORMAT('PARAMETERS')
c      WRITE(4,198) (B(I),I=1,NP)
c      WRITE(4,200)
c  200 FORMAT('STANDARD ERRORS')
c      WRITE(4,198) (DELB(I),I=1,NP)
c      WRITE(4,201)
c  201 FORMAT('CORRELATIONS')
c      WRITE(4,198) ((ESSP(I,J),J=1,I),I=1,NP)
c      WRITE(4,202)
      CALL ROOTS
c  202 FORMAT('ROOTS:REAL PARTS THEN IMAGINARY')
c      WRITE(4,198) (ROOTR(I),I=1,ARP)
c      WRITE(4,198) (ROOTI(I),I=1,ARP)
c      CLOSE(UNIT=4)
c added by Z.W.
c     the last element of phi.dat is the estimate of the mean
c      OPEN(UNIT=4,FILE='phi.dat',STATUS='NEW')
c      OPEN(UNIT=4,FILE='phi.dat',STATUS='unknown')
c      WRITE(4,300) (B(I),I=1,NP)
c  300 FORMAT(5E16.8)
c      CLOSE(UNIT=4)
c end by Z.W.
      RETURN
      END
