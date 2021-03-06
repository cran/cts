      SUBROUTINE REJUCE
      IMPLICIT NONE
      INTEGER I,J,K 
      DOUBLE PRECISION U
c      INCLUDE 'series.txt'
c      INCLUDE 'setcon.txt'
c      INCLUDE 'veccom.txt'
c      INCLUDE 'redcom.txt'

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
C*****series.txt
      CHARACTER*40 NAME
      INTEGER LEN
      DOUBLE PRECISION TIM(5000),SER(5000),TDIF(5000)
c      COMMON/SERIES/NAME,LEN,TIM,SER,TDIF
      COMMON/SERIES/TIM,SER,TDIF,NAME,LEN
C*****redcom.txt
      DOUBLE PRECISION GRAD(22),SSP(22,22)
      COMMON/REDCOM/GRAD,SSP

      DO 101 I=1,NP
      U=CSZ
      DO 100 J=1,LEN
      U=U+RES(J)*PRES(I,J)
  100 CONTINUE
      GRAD(I)=U
  101 CONTINUE
      DO 104 I=1,NP
      DO 104 J=1,NP
      U=CSZ
      DO 103 K=1,LEN
      U=U+PRES(I,K)*PRES(J,K)
  103 CONTINUE
      SSP(I,J)=U
  104 CONTINUE
      RETURN
      END
