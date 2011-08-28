      SUBROUTINE SETUP (PFI1,ARP1,VRI1,CCV1,SCALE1,ARI1,VR1,
     *PHI1,LYAP1,PRDG1,SCC1,FCT1,FTY1,
     *LEN1,FOR1,TIM1,SER1,
     *NIT1,OPM1,RGM1,REQ1,CONCRIT1,RPERT1,IVLAM1,
     *FAC1,STLAM1,SMLAM1,GTLAM1,KST1,NP1, tra1)
      IMPLICIT NONE
c      CHARACTER*1 ST9
c      CHARACTER*4 ST0,ST1
      INTEGER I,J,K,L,M
      DOUBLE PRECISION S
c      INCLUDE 'model.txt'
c      INCLUDE 'series.txt'
c      INCLUDE 'conpar.txt'
c      INCLUDE 'setcon.txt'
c      INCLUDE 'repar2.txt'
c      INCLUDE 'model2.txt'
c      INCLUDE 'veccom.txt'
c      INCLUDE 'repcom.txt'
c      INCLUDE 'repar3.txt'
c      INCLUDE 'resgn1.txt'
c      INCLUDE 'resgn2.txt'
c      INCLUDE 'resgd3.txt'
c      INCLUDE 'redcom.txt'
c      INCLUDE 'seacom.txt'
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

C*****setcon.txt
      LOGICAL CONV,FAIL
      INTEGER NP,ITCT,PPIND
      DOUBLE PRECISION CSO,CSZ,LAM,SSOLD,GMOLD,GMNEW,SIGSQ,OLDB(22)
c      COMMON/SETCON/CONV,FAIL,NP,ITCT,PPIND,CSO,CSZ,LAM,SSOLD,
c     *GMOLD,GMNEW,SIGSQ,OLDB
      COMMON/SETCON/CSO,CSZ,LAM,SSOLD,
     *GMOLD,GMNEW,SIGSQ,OLDB,CONV,FAIL,NP,ITCT,PPIND
C*****repar2.txt
      DOUBLE PRECISION BCT(21),CHT(21,21)
      COMMON/REPAR2/BCT,CHT
C*****model2.txt
      INTEGER PFI1,ARP1,ARI1,VRI1,CCV1,LYAP1,SCC1,fct1,fty1
      DOUBLE PRECISION SCALE1,VR1,PHI1(20),PRDG1
      integer for1

C*****veccom.txt
      DOUBLE PRECISION B(22),NEWB(22),PERB(22),DELB(22),
     * ERR(500),RES(500),PRES(22,500),sres(500)
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
C*****resgn1.txt
      DOUBLE PRECISION DR(20),DI(20),MR(20),MI(20),NR(20),NI(20),
     * UR(20),UI(20),WR(20),WI(20),WPR(20),WPI(20),CR(20,20),CI(20,20),
     * CPR(20,20),CPI(20,20),CSR(20,20),CSI(20,20)
      COMMON/RESGN1/DR,DI,MR,MI,NR,NI,UR,UI,WR,WI,WPR,WPI,CR,CI,CPR,CPI,
     * CSR,CSI
C*****resgn2.txt
      DOUBLE PRECISION WK(20),VT(500),BI(2,20,20),R(2,20,20),
     *RI(2,20,20)
c       DOUBLE PRECISION WK(20),VT(500),BI(2,20,20),R(20,20),RI(2,20,20)
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
C*****redcom.txt
      DOUBLE PRECISION GRAD(22),SSP(22,22)
      COMMON/REDCOM/GRAD,SSP
C*****seacom.txt
      LOGICAL SOLV
      DOUBLE PRECISION ESSP(22,22),ECOV(22,22)
c      COMMON/SEACOM/SOLV,ESSP,ECOV
      COMMON/SEACOM/ESSP,ECOV,SOLV

C*****kfasav.txt
      REAL PRE(500),PRV(500),FSER(500),FVAR(500),SSER(500),SVAR(500)
      DOUBLE PRECISION FSES(500,20),FSCV(500,20,20),SSES(500,20)
     1,SSCV(500,20,20),PSES(500,20),PSCV(500,20,20),TRAN(500,20,20)
c      COMMON/KFASAV/PRE,PRV,FSER,FVAR,SSER,SVAR,FSES,FSCV,SSES,SSCV
c     1,PSES,PSCV,TRAN
      COMMON/KFASAV/FSES,FSCV,SSES,SSCV
     1,PSES,PSCV,TRAN,PRE,PRV,FSER,FVAR,SSER,SVAR
      integer tra1

      INTEGER LEN1, NP1
      DOUBLE PRECISION TIM1(500),SER1(500)
C*****conpar1.txt    
      INTEGER NIT1,OPM1,RGM1,KST1
      DOUBLE PRECISION REQ1,CONCRIT1,RPERT1,IVLAM1,FAC1,STLAM1,SMLAM1,
     *GTLAM1
      Do 100 I=1, LEN1
         TIM(I)=TIM1(I)
         SER(I)=SER1(I)
 100     continue

 
      tra=tra1   
      PFI=PFI1
      ARP=ARP1
      ARI=ARI1
      VRI=VRI1
      CCV=CCV1
      SCALE=SCALE1
      VR=VR1
      Do 105 I=1, ARP1
         PHI(I)=PHI1(I)
 105  continue
      LYAP=LYAP1
      PRDG=PRDG1
      SCC=SCC1
      FCT=FCT1
      FTY=FTY1
      LEN=LEN1
      FOR=FOR1
      NIT=NIT1
      OPM=OPM1
      RGM=RGM1
      REQ=REQ1
      CONCRIT=CONCRIT1
      RPERT=RPERT1
      IVLAM=IVLAM1
      FAC=FAC1
      STLAM=STLAM1
      SMLAM=SMLAM1
      GTLAM=GTLAM1
      KST=KST1 

C  INITIALISE CONST
      CONST=CSZ
      IF(CCV.GT.0)THEN
        DO 202 I=1,LEN
        CONST=CONST+SER(I)
  202   CONTINUE
        CONST=CONST/LEN
      END IF
C  INITIALISE TDIF ARRAY
      TDIF(1)=CSZ
      DO 203 I=2,LEN
      TDIF(I)=TIM(I)-TIM(I-1)
  203 CONTINUE
c initialise the length of data, to use part of data if fct=0
      if(fct.EQ.0)then
         len=len-for
      endif
C  INITIALISE NP
      I=0
      IF(VRI.EQ.1)I=I+1
      IF(CCV.EQ.2)I=I+1
      NP=ARP+I
      NP1=NP
C  INITIALISE OLDB ARRAY
      DO 204 I=1,ARP
      OLDB(I)=PHI(I)
  204 CONTINUE
      M=ARP+1
      IF(VRI.EQ.1)OLDB(M)=DSQRT(VR)
      IF(CCV.EQ.2)THEN
        IF(VRI.EQ.0)THEN
          OLDB(M)=CONST
        ELSE
          OLDB(M+1)=CONST
        END IF
      END IF
C  SET INITIAL VALUES
      CONV=.FALSE.
      FAIL=.FALSE.
      ITCT=0
      PPIND=0
      CSO=1.0D0
      LAM=IVLAM
      SSOLD=CSZ
      KFSW=0
      DO 304 J=1,M
      BCT(1)=CSO
      DO 301 I=2,M
      BCT(I)=CSZ
  301 CONTINUE
      DO 302 K=2,M
      IF(K.GT.J)THEN
        S=CSO
      ELSE
        S=-CSO
      END IF
      DO 302 L=2,K
      I=K-L+2
      BCT(I)=BCT(I)+S*BCT(I-1)/SCALE
  302 CONTINUE
      DO 303 I=1,M
      CHT(I,J)=BCT(M+1-I)
  303 CONTINUE
  304 CONTINUE
      BCT(1)=CSO
      DO 400 I=2,M
      BCT(I)=CSZ
  400 CONTINUE
      DO 401 K=2,ARP
      DO 401 I=2,K
      J=K-I+2
      BCT(J)=BCT(J)+BCT(J-1)/SCALE
  401 CONTINUE
      RETURN
      END
