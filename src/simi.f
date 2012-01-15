      SUBROUTINE SIMI
      IMPLICIT NONE
      INTEGER I,I1,J,K,L
      DOUBLE PRECISION V,W,Y,Z
c      INCLUDE 'setcon.txt'
c      INCLUDE 'veccom.txt'
c      INCLUDE 'seacom.txt'
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
C*****seacom.txt
      LOGICAL SOLV
      DOUBLE PRECISION ESSP(22,22),ECOV(22,22)
c      COMMON/SEACOM/SOLV,ESSP,ECOV
      COMMON/SEACOM/ESSP,ECOV,SOLV


      SOLV=.TRUE.
      DO 3 L=1,NP
      K=NP+1-L
      IF(ESSP(K,K).LT.1.0D-9)THEN
        SOLV=.FALSE.
        RETURN
      END IF
      IF(K.GT.1)THEN
        I1=K-1
        Y=ESSP(K,K)
        DO 2 I=1,I1
        Z=ESSP(I,K)
        DO 1 J=1,I1
        ESSP(I,J)=ESSP(I,J)-Z*ESSP(K,J)/Y
    1   CONTINUE
        DELB(I)=DELB(I)-Z*DELB(K)/Y
    2   CONTINUE
      END IF
    3 CONTINUE
      DO 7 K=1,NP
      Y=ESSP(K,K)
      V=1.0D0
      IF(K.GT.1)THEN
        I1=K-1
        DO 5 I=1,I1
        Z=0.0D0
        DO 4 J=1,I1
        Z=Z-ESSP(J,I)*ESSP(K,J)
    4   CONTINUE
        W=ESSP(K,I)
        Z=Z/Y
        V=V-W*Z
        ESSP(I,K)=Z
        DELB(K)=DELB(K)-W*DELB(I)
    5   CONTINUE
        I1=K-1
        DO 6 I=1,I1
        ESSP(K,I)=ESSP(I,K)
    6   CONTINUE
      END IF
      ESSP(K,K)=V/Y
      DELB(K)=DELB(K)/Y
    7 CONTINUE
      RETURN
      END
