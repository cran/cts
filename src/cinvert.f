      SUBROUTINE CINVERT
      IMPLICIT NONE
      INTEGER I,J
c      INCLUDE 'model.txt'
c      INCLUDE 'setcon.txt'
c      INCLUDE 'resgn2.txt'
C*****model.txt
      INTEGER PFI,ARP,VRI,CCV,LYAP,SCC,fct
      DOUBLE PRECISION SCALE,VR,CONST,PHI(20),PRDG
      COMMON/MODEL/SCALE,VR,CONST,PHI,PRDG,PFI,ARP,VRI,CCV,LYAP,SCC,fct
      integer for, fty
      common/model/for, fty
      integer ARI, tra
      common/model/ari,tra

C*****setcon.txt
      LOGICAL CONV,FAIL
      INTEGER NP,ITCT,PPIND
      DOUBLE PRECISION CSO,CSZ,LAM,SSOLD,GMOLD,GMNEW,SIGSQ,OLDB(22)
c      COMMON/SETCON/CONV,FAIL,NP,ITCT,PPIND,CSO,CSZ,LAM,SSOLD,
c     *GMOLD,GMNEW,SIGSQ,OLDB
      COMMON/SETCON/CSO,CSZ,LAM,SSOLD,
     *GMOLD,GMNEW,SIGSQ,OLDB,CONV,FAIL,NP,ITCT,PPIND
C*****resgn2.txt
      DOUBLE PRECISION WK(20),VT(500),BI(2,20,20),R(2,20,20),
     *RI(2,20,20)
c       DOUBLE PRECISION WK(20),VT(500),BI(2,20,20),R(20,20),RI(2,20,20)
      INTEGER ERRNO1
      COMMON/RESGN2/WK,VT,BI,R,RI,ERRNO1


c     added ZW
      integer IPIVOT(ARP)
      DO 100 J=1,ARP
      DO 100 I=1,ARP
      IF(I.EQ.J)THEN
        BI(1,I,J)=CSO
      ELSE
        BI(1,I,J)=CSZ
      END IF
      BI(2,I,J)=CSZ
  100 CONTINUE
      I=1
      J=20
c      CALL F04ADF(R,J,BI,J,ARP,ARP,RI,J,WK,I) changed ZW
      call zgesv(ARP,ARP,R,J,IPIVOT,BI,J,I)
      ERRNO1 = 0
      IF(I.NE.0)THEN
C      IF(I.NE.0)THEN
c        PRINT *,'PROGRAM FAILS IN F04ADF WITH ERROR: ',I
C         PRINT *,'PROGRAM FAILS IN ZGESV WITH ERROR: ',I
        call intpr('WITH ERROR', 10, I, 1)
        call rexit('PROGRAM FAILS IN ZGESV')
C         STOP
      ERRNO1 = 2
      RETURN
      END IF
      RETURN
      END
