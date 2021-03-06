      subroutine setcom(PFI1,ARP1,NP1,VRI1,CCV1,LEN1,SCALE1,VR1,
     * SIGSQ1,ESSP1,ECOV1,B1,DELB1,ROOTR1,ROOTI1)
      IMPLICIT NONE 
      INTEGER I,J
c      INCLUDE 'model.txt'
c      INCLUDE 'conpar.txt'
c      INCLUDE 'setcon.txt'
c      INCLUDE 'series.txt'
c      INCLUDE 'veccom.txt'
c      INCLUDE 'redcom.txt'
c      INCLUDE 'seacom.txt'
c      INCLUDE 'repar3.txt'
c      INCLUDE 'seacom1.txt'
c      INCLUDE 'repar31.txt'
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
      DOUBLE PRECISION TIM(5000),SER(5000),TDIF(5000)
c      COMMON/SERIES/NAME,LEN,TIM,SER,TDIF
      COMMON/SERIES/TIM,SER,TDIF,NAME,LEN
C*****veccom.txt
      DOUBLE PRECISION B(22),NEWB(22),PERB(22),DELB(22),
     * ERR(5000),RES(5000),PRES(22,5000),sres(5000)
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
C*****seacom1.txt
      DOUBLE PRECISION ESSP1(22,22),ECOV1(22,22)
C*****repar31.txt
      DOUBLE PRECISION ROOTR1(20),ROOTI1(20)
c      COMMON/REPAR3/ALPHA,ROOTR,ROOTI,XVECR,XVECI

C*****repcom.txt
      LOGICAL PRPI
      INTEGER REQSW,KFSW
      DOUBLE PRECISION VOB
c      COMMON/REPCOM/PRPI,REQSW,VOB,KFSW
      COMMON/REPCOM/VOB,REQSW,KFSW,PRPI	
C*****veccom1.txt
      DOUBLE PRECISION B1(22),DELB1(22)
C*****setcon1.txt
      INTEGER NP1
      DOUBLE PRECISION SIGSQ1
C*****model1.txt
      INTEGER LEN1, PFI1,ARP1,VRI1,CCV1
      DOUBLE PRECISION SCALE1,VR1

      PFI1=PFI
      ARP1=ARP
      NP1=NP
      VRI1=VRI
      CCV1=CCV
      LEN1=LEN
      SCALE1=SCALE
      VR1=VR
      SIGSQ1=SIGSQ
      DO 100 I=1,NP
         DO 100 J=1,NP
            ESSP1(I,J)=ESSP(I,J)
            ECOV1(I,J)=ECOV(I,J)
 100     continue
         DO 101 I=1,NP
            B1(I)=B(I)
 101     continue
         DO 102 I=1,NP
            DELB1(I)=DELB(I)
 102     continue
         DO 103 I=1,ARP
            ROOTR1(I)=ROOTR(I)
            ROOTI1(I)=ROOTI(I)
 103     continue
         RETURN
         END
