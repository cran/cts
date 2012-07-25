      SUBROUTINE ROOTS
      IMPLICIT NONE
      INTEGER I,J
      DOUBLE PRECISION T
C     DOUBLE PRECISION ROOTC(40),WORK(42)    CHANGED ZW
C     LOGICAL RSCAL  
c      double precision alpha1(3)                        
c      INCLUDE 'model.txt'
c      INCLUDE 'setcon.txt'
c      INCLUDE 'veccom.txt'
c      INCLUDE 'repar3.txt'
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
C*****veccom.txt
      DOUBLE PRECISION B(22),NEWB(22),PERB(22),DELB(22),
     * ERR(5000),RES(5000),PRES(22,5000),sres(5000)
      COMMON/VECCOM/B,NEWB,PERB,DELB,ERR,RES,PRES,sres
C*****repar3.txt
      DOUBLE PRECISION ALPHA(21),ROOTR(20),ROOTI(20),XVECR(20),
     *XVECI(20)
      COMMON/REPAR3/ALPHA,ROOTR,ROOTI,XVECR,XVECI

      J=ARP+1
      ALPHA(1)=CSO
      DO 100 I=2,J
         ALPHA(I)=B(I-1)
 100  CONTINUE
      I=0
      T=1.110D-16
C     CALL C02AEF(ALPHA,J,ROOTR,ROOTI,T,I)
C     RSCAL=.TRUE.                            CHANGED ZW
Cc     CALL C02AGF(ALPHA,J,RSCAL,ROOTC,WORK,I) CHANGED ZW
C     PRINT *,  ROOTC
c     the original call for c02agf where J is the degree of polynomial, should J be J-1?
c      print *,'alpha= ',alpha
c      print *,'alpha(1)=',alpha(1)
c      print *,'alpha(2)=',alpha(2)
c      print *,'alpha(3)=',alpha(3)
c      print *,'order J= ',J-1
c      call rpoly(alpha,J-1,rootr,rooti,fail)  
c     call rpoly using netlib subroutines      ZW 3/26/05
 
c      alpha1(1)=alpha(1)
c      alpha1(2)=alpha(2)
c      alpha1(3)=alpha(3)
c      print *,'alpha1= ',alpha1
c      print *,'alpha1(1)=',alpha1(1)
c      print *,'alpha1(2)=',alpha1(2)
c      print *,'alpha1(3)=',alpha1(3)
      call drpoly(J-1,ALPHA,rootr,rooti)
c      call drpoly(2,alpha1,rootr,rooti)  3/26/05
      if (fail.EQV..TRUE.) then
         call rexit('program fails in rpoly')
      endif
c         print *,'program fails in rpoly'  3/26/05
c         print *,'program fails in rpoly'  3/26/05
c         stop                              3/26/05
c      else                                 3/26/05
c      print *,'rootr= ',rootr
c      print *,'rooti= ',rooti
C     added ZW
c     IF(I.NE.0)THEN
c     PRINT *,'PROGRAM FAILS IN C02AGF WITH ERROR: ',FAIL
c     STOP
c     ELSE
C     K=0
C     DO 105 I=1,ARP
C     K=K+1
C     ROOTR(I)=ROOTC(K)
C     K=K+1
C     ROOTI(I)=ROOTC(K)
C     105   CONTINUE 
            DO 101 I=1,ARP
               ALPHA(I)=B(I)
 101           CONTINUE
            ALPHA(ARP+1)=CSZ
c         END IF                3/26/05
         RETURN
         END
