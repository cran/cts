c     SUBROUTINE RESG1
      SUBROUTINE RESG1pre1
      IMPLICIT NONE
      INTEGER E,I,J,K,L,M,T
      DOUBLE PRECISION DEL
      DOUBLE PRECISION U,PRED,PEV
c      double precision forser
c      INCLUDE 'model.txt'
c      INCLUDE 'series.txt'
c      INCLUDE 'setcon.txt'
c      INCLUDE 'veccom.txt'
c      INCLUDE 'repcom.txt'
c      INCLUDE 'repar3.txt'
c      INCLUDE 'resgn2.txt'
c      INCLUDE 'resgd3.txt'
c      INCLUDE 'kfasav.txt'
C*****model.txt
      INTEGER PFI,ARP,VRI,CCV,LYAP,SCC,fct
      DOUBLE PRECISION SCALE,VR,CONST,PHI(20),PRDG
      COMMON/MODEL/SCALE,VR,CONST,PHI,PRDG,PFI,ARP,VRI,CCV,LYAP,SCC,fct
      integer for, fty
      common/model/for, fty
      integer ARI, tra
      common/model/ari,tra

C*****series.txt
      CHARACTER*40 NAME
      INTEGER LEN
      DOUBLE PRECISION TIM(5000),SER(5000),TDIF(5000)
c      COMMON/SERIES/NAME,LEN,TIM,SER,TDIF
      COMMON/SERIES/TIM,SER,TDIF,NAME,LEN
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
     * ERR(5000),RES(5000),PRES(22,5000),sres(5000)
      COMMON/VECCOM/B,NEWB,PERB,DELB,ERR,RES,PRES,sres
C*****repcom.txt
      LOGICAL PRPI
      INTEGER REQSW,KFSW
      DOUBLE PRECISION VOB
c      COMMON/REPCOM/PRPI,REQSW,VOB,KFSW
      COMMON/REPCOM/VOB,REQSW,KFSW,PRPI	
C*****repar3.txt
      DOUBLE PRECISION ALPHA(21),ROOTR(20),ROOTI(20),XVECR(20),
     *XVECI(20)
      COMMON/REPAR3/ALPHA,ROOTR,ROOTI,XVECR,XVECI

C*****resgn2.txt
      DOUBLE PRECISION WK(20),VT(5000),BI(2,20,20),R(2,20,20),
     *RI(2,20,20)
c       DOUBLE PRECISION WK(20),VT(5000),BI(2,20,20),R(20,20),RI(2,20,20)
      INTEGER ERRNO1
      COMMON/RESGN2/WK,VT,BI,R,RI,ERRNO1
C*****resgd3.txt
      INTEGER IW1(20)
      DOUBLE PRECISION W1(20),W2(20),AL(20,20),BL(20,20),
     * CL(20,20),DL(20,20)
      DOUBLE PRECISION MS(20),NS(20),WS(20),WPS(20),AS(20,20),
     * CC(20,20),CS(20,20),CPS(20,20),AW0(20,20),AW1(20,20)
c      COMMON/RESGN3/IW1,W1,W2,AL,BL,CL,DL,MS,NS,WS,WPS,AS,CC,CS,
c     * CPS,AW0,AW1
      COMMON/RESGN3/W1,W2,AL,BL,CL,DL,MS,NS,WS,WPS,AS,CC,CS,
     * CPS,AW0,AW1,IW1

C*****kfasav.txt
      REAL PRE(5000),PRV(5000),FSER(5000),FVAR(5000),SSER(5000)
     1,SVAR(5000)
      DOUBLE PRECISION FSES(5000,20),FSCV(5000,20,20),SSES(5000,20)
     1,SSCV(5000,20,20),PSES(5000,20),PSCV(5000,20,20),TRAN(5000,20,20)
c      COMMON/KFASAV/PRE,PRV,FSER,FVAR,SSER,SVAR,FSES,FSCV,SSES,SSCV
c     1,PSES,PSCV,TRAN
      COMMON/KFASAV/FSES,FSCV,SSES,SSCV
     1,PSES,PSCV,TRAN,PRE,PRV,FSER,FVAR,SSER,SVAR

c      DO 110 I=1,ARP
c      WS(I)=CSZ
c      DO 110 J=1,ARP
c      CC(I,J)=CSZ
c      AS(I,J)=CSZ
c  110 CONTINUE
c      DO 111 J=2,ARP
c      AS(J-1,J)=CSO
c      AS(ARP,J-1)=-ALPHA(ARP+2-J)
c  111 CONTINUE
c      AS(ARP,ARP)=-ALPHA(1)
c      CC(ARP,ARP)=-CSO
c      DO 112 I=1,ARP
c      DO 112 J=1,ARP
c      AL(I,J)=AS(J,I)
c      CL(I,J)=CC(I,J)
c  112 CONTINUE
c      J=20
c      T=0
c      CALL LYBSC(ARP,AL,J,CL,J,BL,J,DL,W1,W2,T,I)
c      IF(I.NE.0)THEN
c        PRINT *,'PROGRAM FAILS IN SLICE ROUTINE LYBSC: ',I
c        STOP
c      END IF
c      DO 113 I=1,ARP
c      DO 113 J=1,ARP
c      U=BL(I,J)
c      CC(I,J)=U
c      CS(I,J)=U
c      BL(I,J)=AS(I,J)
c  113 CONTINUE
      E=0
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     print *,'main loop for predicting begins'
c      DO 150 T=1,LEN
c      if(fty.EQ.1)then
c           print *,'Length of data used to fit is ',LEN
c           print *,'fty=1, forecast past the end'
c      else
c      changed the next line 6/6/06
c           if(fct.EQ.1.AND.fty.EQ.2)then
c           print *,'fty=2, forecast last L-steps'
c           LEN=LEN-FOR
c           endif
c      endif  
      do 150 T=LEN+1,LEN+FOR
c     changed by Z.W. for predicting 20 steps temporially
      DEL=TDIF(T)
      J=20
      K=9
C      IF (KFSW.EQ.1) THEN
C        PRINT *,'JUST BEFORE MEPAD CALLED'
C        PRINT *,(ALPHA(KK),KK=1,ARP)
C      ENDIF
C     **** USE OF MEPAD ****
C     DEL IS THE TIME INTERVAL MULTIPLIER OF MATRIX BL IN THE EXP
C     DL IS THE RESULT EXP(DEL*BL)
C     AL AND CL ARE WORKING ARRAYS
C     *****             ****
      CALL MEPAD(ARP,K,DEL,BL,J,DL,J,L,M,IW1,AL,CL,W1,W2,I)
C      IF (KFSW.EQ.1)PRINT *,'JUST AFTER MEPAD CALLED'
      IF(I.EQ.4)THEN
        call intpr('WITH ERROR', 10, I, 1)
        call rexit('PROGRAM FAILS IN SLICE ROUTINE MEPAD')
C        PRINT *,'PROGRAM FAILS IN SLICE ROUTINE MEPAD: ',I
C        STOP
        ERRNO1=31
        RETURN

      ELSE IF(I.NE.0)THEN
        E=I
      END IF
C     **** BL IS RESTORED HAVING BEEN MODIFIED BY MEPAD
      DO 114 I=1,ARP
      DO 114 J=1,ARP
      BL(I,J)=AS(I,J)
      AW0(I,J)=CC(I,J)-CS(I,J)
      AW1(I,J)=DL(I,J)
  114 CONTINUE
      DO 116 I=1,ARP
      DO 116 J=1,ARP
      U=CSZ
      DO 115 K=1,ARP
      U=U+AW0(I,K)*AW1(J,K)
  115 CONTINUE
      CC(I,J)=U
  116 CONTINUE
      DO 118 I=1,ARP
      DO 118 J=1,ARP
      U=CSZ
      DO 117 K=1,ARP
      U=U+AW1(I,K)*CC(K,J)
  117 CONTINUE
      CPS(I,J)=U+CS(I,J)
  118 CONTINUE
C     **** AW1 IS TRANSITION MATRIX
C          CS IS STATIONARY STATE COVARIANCE
C          CC IS OLD STATE COVARIANCE
C     **** CPS IS PREDICTED STATE COVARIANCE
      DO 120 I=1,ARP
      U=CSZ
      DO 119 J=1,ARP
      U=U+AW1(I,J)*WS(J)
  119 CONTINUE
      WPS(I)=U
  120 CONTINUE
C     **** WS IS OLD STATE ESTIMATE
C     **** WPS IS PREDICTED STATE ESTIMATE
      DO 122 I=1,ARP
      U=CSZ
      DO 121 J=1,ARP
      U=U+CPS(I,J)*XVECR(J)
  121 CONTINUE
      MS(I)=U
  122 CONTINUE
      PEV=CSZ
      PRED=CSZ
      DO 123 I=1,ARP
      U=XVECR(I)
      PEV=PEV+U*MS(I)
c     PEV is Jones (1981), (40), predicted error variance?
      PRED=PRED+U*WPS(I)
c      print *,'prediction',PRED
c     PRED is Jones (1981), (38), predict the obs. at time t_k
  123 CONTINUE
c     VOB is R in Jones (40)
      PEV=PEV+VOB
      VT(T)=PEV
c     VT(T) is Jones (43)?
c      U=SER(T)-CONST-PRED
c     for predicting, set U=0
      U=0
c     U is the innovation, Jones (42)
      ERR(T)=U
      DO 124 I=1,ARP
      NS(I)=MS(I)/PEV
      WS(I)=WPS(I)+NS(I)*U
c     WS(I) is the gain K_t, for predicting, set to 0
  124 CONTINUE
      DO 125 I=1,ARP
      U=MS(I)
      DO 125 J=1,ARP
      CC(I,J)=CPS(I,J)-U*NS(J)
c     CC is Jones (41)
  125 CONTINUE
C     **** CC IS NOW PRESENT (UPDATED) STATE COVARIANCE 
C     **** WS IS NOW PRESENT (UPDATED) STATE ESTIMATE
      IF (KFSW.EQ.1) THEN
        PRE(T)=CONST+PRED
        PRV(T)=PEV
        U=CSZ
        DO 130 I=1,ARP
        FSES(T,I)=WS(I)
c     FSES is the filtered state
        PSES(T,I)=WPS(I) 
  130   U=U+XVECR(I)*WS(I)
        FSER(T)=U+CONST
c     FSER is the filtered series
c        print *, 'U:',U
        U=CSZ
        DO 135 I=1,ARP
        DO 135 J=1,ARP
        FSCV(T,I,J)=CC(I,J)
        PSCV(T,I,J)=CPS(I,J)
        TRAN(T,I,J)=AW1(I,J)
  135   U=U+CC(I,J)*XVECR(I)*XVECR(J)
c     U is Jones (43) without R
        FVAR(T)=U
      ENDIF
  150 CONTINUE
C      PRINT *,'MAIN LOOP IN RESG1 COMPLETED'
c      print *,'main loop for predicting completed'
      IF(E.NE.0)THEN
C        PRINT *,'WARNING IN SLICE ROUTINE MEPAD: ',E
      END IF
      RETURN
      END
