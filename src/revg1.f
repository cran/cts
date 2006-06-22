      SUBROUTINE REVG1
      INTEGER I,J,T,TR,IA
      DOUBLE PRECISION CSO,U,V,W,X,TWM1(20,20),TWM2(20,20)
     *,AQ(20,20),AQI(20,20),APH(20,20),AP(20,20),AJ(20,20),SIG(20,20)
c      INCLUDE 'kfasav.txt'
c      INCLUDE 'series.txt'
c      INCLUDE 'model.txt'
c      INCLUDE 'repar3.txt'
C*****kfasav.txt
      REAL PRE(500),PRV(500),FSER(500),FVAR(500),SSER(500),SVAR(500)
      DOUBLE PRECISION FSES(500,20),FSCV(500,20,20),SSES(500,20)
     1,SSCV(500,20,20),PSES(500,20),PSCV(500,20,20),TRAN(500,20,20)
c      COMMON/KFASAV/PRE,PRV,FSER,FVAR,SSER,SVAR,FSES,FSCV,SSES,SSCV
c     1,PSES,PSCV,TRAN
      COMMON/KFASAV/FSES,FSCV,SSES,SSCV
     1,PSES,PSCV,TRAN,PRE,PRV,FSER,FVAR,SSER,SVAR
C*****series.txt
      CHARACTER*40 NAME
      INTEGER LEN
      DOUBLE PRECISION TIM(500),SER(500),TDIF(500)
c      COMMON/SERIES/NAME,LEN,TIM,SER,TDIF
      COMMON/SERIES/TIM,SER,TDIF,NAME,LEN
C*****model.txt
      INTEGER PFI,ARP,VRI,CCV,LYAP,SCC,fct
      DOUBLE PRECISION SCALE,VR,CONST,PHI(20),PRDG
      COMMON/MODEL/SCALE,VR,CONST,PHI,PRDG,PFI,ARP,VRI,CCV,LYAP,SCC,fct
      integer for, fty
      common/model/for, fty
C*****repar3.txt
      DOUBLE PRECISION ALPHA(21),ROOTR(20),ROOTI(20),XVECR(20),
     *XVECI(20)
      COMMON/REPAR3/ALPHA,ROOTR,ROOTI,XVECR,XVECI

      CSO=1.D0
      CSZ=0.D0
      IA=20
      DO 100 T=1,LEN
c      do 100 t=1,3
        TR=1+LEN-T
        IF (T.GT.1) THEN
          DO 10 I=1,ARP
          DO 10 J=1,ARP
            V=PSCV(TR+1,I,J)
            AQ(I,J)=V
            AQI(I,J)=V
            APH(I,J)=TRAN(TR+1,I,J)
            SIG(I,J)=SSCV(TR+1,I,J)
   10       AP(I,J)=FSCV(TR,I,J) 
          IFAIL=0
C          PRINT *,'BEFORE CALL OF F01ADF'
C          IF (TR.EQ.101) CALL MPRINT(ARP,AQI,IA,'AQIBEFORE')
c         added ZW
      CALL DPOTRF ('UPPER TRIANGLE OF A STORED', ARP, AQI, IA, IFAIL)

      IF (IFAIL .NE. 0) THEN

        PRINT 1030, IFAIL

        STOP 1

      END IF

C

C     Use the factored form of A to compute the inverse of A and

C     print the inverse thus computed.

C

      CALL DPOTRI ('UPPER TRIANGLE OF A STORED', ARP, AQI, IA, IFAIL)

      IF (IFAIL .NE. 0) THEN

        PRINT 1040, ABS(IFAIL)

        STOP 2

      END IF
 1030 FORMAT (1X, 'Error factoring A, INFO = ', I5)
c          print *,'AQI'
c          do 30 i=1,arp
c             do 30 j=1,arp
c                print *,'aqi',aqi(i,j)
c 30             continue
 1040 FORMAT (1X, 'Error computing inverse of A, INFO = ', I5)
c      print *,'AQI',AQI
c     added ZW
c          CALL F01ADF(ARP,AQI,IA,IFAIL)
C          PRINT *,'AFTER CALL OF F01ADF'
          IF (IFAIL.EQ.1) PRINT *,'SMOOTHING ERROR: NOT POS.DEF.'
c          DO 20 I=1,ARP
c          DO 20 J=I,ARP
c          AQI(I,J)=AQI(J+1,I)
c   20     AQI(J,I)=AQI(I,J)

          do 20 i=1,arp-1
             do 20 j=i,arp-1
                aqi(j+1,i)=aqi(i,j+1)
 20          continue
c             print *,'AQI'
c          do 30 i=1,arp
c             do 30 j=1,arp
c                print *,'aqi',aqi(i,j)
c 30             continue
C          IF (TR.EQ.101) CALL MPRINT(ARP,AQI,IA,'AQIAFTER')
          CALL MTPROD(ARP,TWM1,APH,AQI,IA)
          CALL MPROD(ARP,AJ,AP,TWM1,IA) 
          CALL MDIFF(ARP,TWM1,SIG,AQ,IA)
          CALL MSPROD(ARP,TWM2,AJ,TWM1,IA)
        ENDIF
        W=CSZ
        X=CSZ
        DO 70 I=1,ARP
          U=FSES(TR,I)
          DO 60 J=1,ARP
            V=FSCV(TR,I,J)
            IF (T.GT.1) THEN
              V=V+TWM2(I,J)
              U=U+AJ(I,J)*(SSES(TR+1,J)-PSES(TR+1,J))
            ENDIF
            X=X+XVECR(I)*XVECR(J)*V
   60     SSCV(TR,I,J)=V
          W=W+XVECR(I)*U
   70   SSES(TR,I)=U
        SSER(TR)=W+CONST
        SVAR(TR)=X
C      PRINT *,'END OF REVG1 LOOP WITH TR=',TR
  100 CONTINUE   
      END
