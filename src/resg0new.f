c      SUBROUTINE RESG0
c     compare to resg1d.f file. The relationship is below;
c     In this file (resg0new.f), CPR (real part of CP), CPI (imaginary part c     of CP), they are corresponding to CPS (resg1d.f)
c     MR and MI corresponding to MS (resg1d.f)
c     NR and NI corresponding to NS (resg1d.f)
c     WPR and WPI corresponding to WPS (resg1d.f)
c     CSR and CSI corresponding to CS (resg1d.f)
c     CR and CI corresponding to CC (resg1d.f)

      subroutine resg0new
      IMPLICIT NONE 
      INTEGER I,J,T
      DOUBLE PRECISION U,V,W,X,Y,Z,PRED,PEV
      DOUBLE PRECISION FSESR(500,20),FSCVR(500,20,20)
      DOUBLE PRECISION FSESI(500,20),FSCVI(500,20,20)
      DOUBLE PRECISION FSER0(500,20),TRANR(500,20),TRANI(500,20)
      DOUBLE PRECISION PSESR(500,20),PSESI(500,20)
      DOUBLE PRECISION PSCVR(500,20,20),PSCVI(500,20,20)
c      INCLUDE 'model.txt'
c      INCLUDE 'series.txt'
c      INCLUDE 'setcon.txt'
c      INCLUDE 'veccom.txt'
c      INCLUDE 'repcom.txt'
c      INCLUDE 'repar3.txt'
c      INCLUDE 'resgn1.txt'
c      INCLUDE 'resgn2.txt'
c      include 'kfasav.txt'
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
      COMMON/RESGN2/WK,VT,BI,R,RI
C*****kfasav.txt
      REAL PRE(500),PRV(500),FSER(500),FVAR(500),SSER(500),SVAR(500)
      DOUBLE PRECISION FSES(500,20),FSCV(500,20,20),SSES(500,20)
     1,SSCV(500,20,20),PSES(500,20),PSCV(500,20,20),TRAN(500,20,20)
c      COMMON/KFASAV/PRE,PRV,FSER,FVAR,SSER,SVAR,FSES,FSCV,SSES,SSCV
c     1,PSES,PSCV,TRAN
      COMMON/KFASAV/FSES,FSCV,SSES,SSCV
     1,PSES,PSCV,TRAN,PRE,PRV,FSER,FVAR,SSER,SVAR


      DO 100 J=1,ARP
      DO 100 I=1,ARP
      IF(I.EQ.1)THEN
        U=CSO
        V=CSZ
      ELSE IF(I.EQ.2)THEN
        U=ROOTR(J)
        V=ROOTI(J)
        IF(ARP.GT.2)THEN
          W=U
          X=V
        END IF
      ELSE IF(I.GT.2)THEN
        CALL MULC(U,V,W,X,Y,Z)
        U=Y
        V=Z
      END IF
      R(1,I,J)=U
      R(2,I,J)=V
  100 CONTINUE
      CALL CINVERT
      DO 102 I=1,ARP
      UR(I)=BI(1,I,ARP)
      UI(I)=BI(2,I,ARP)
c     in correspondence to the change of F04ADF to zgesv in LAPACK, the resuc     lts are in BI, not RI
c      UR(I)=RI(1,I,ARP)
c      UI(I)=RI(2,I,ARP)
c     end of change by Z.W.
  102 CONTINUE
      DO 104 I=1,ARP
      WR(I)=CSZ
      WI(I)=CSZ
      DO 103 J=1,ARP
      CALL ADDC(ROOTR(I),ROOTI(I),ROOTR(J),-ROOTI(J),U,V)
      CALL MULC(-UR(I),-UI(I),UR(J),-UI(J),Y,Z)
      CALL DIVC(Y,Z,U,V,W,X)
      CSR(I,J)=W
      CSI(I,J)=X
      CR(I,J)=W
      CI(I,J)=X
  103 CONTINUE
  104 CONTINUE
C      DO 150 T=1,LEN
      do 150 t=1,2
      W=TDIF(T)
      DO 105 I=1,ARP
      CALL MULR(ROOTR(I),ROOTI(I),W,U,V)
      U=DEXP(U)
      DR(I)=U*DCOS(V)
      DI(I)=U*DSIN(V)
  105 CONTINUE
      DO 107 I=1,ARP
      CALL MULC(DR(I),DI(I),WR(I),WI(I),WPR(I),WPI(I))
c      print *,'T=',t
c      print *,'CALL MULC(DR(I),DI(I),WR(I),WI(I),WPR(I),WPI(I))'
c      print *,'WPR(I)=',WPR(I)
c      print *,'WPI(I)=',WPI(I)
      DO 106 J=1,ARP
      CALL SUBC(CR(I,J),CI(I,J),CSR(I,J),CSI(I,J),U,V)
c      print *,'SUBC,U=',U
c      print *,'SUBC,V=',V
      CALL MULC(U,V,DR(J),-DI(J),Y,Z)
c      print *,'MULC(U,V,DR(J),-DI(J),Y,Z)' 
c      print *,'MULC,Y',Y
c      print *,'MULC,Z=',Z
      CALL MULC(DR(I),DI(I),Y,Z,U,V)
c      print *,' MULC(DR(I),DI(I),Y,Z,U,V)'
c      print *,'U=',U
c      print *,'V=',V
      CALL ADDC(U,V,CSR(I,J),CSI(I,J),CPR(I,J),CPI(I,J))
c      print *,'CPR=',CPR(I,J)
c      print *,'CPI=',CPI(I,J)
  106 CONTINUE
  107 CONTINUE
      DO 109 I=1,ARP
      Y=CSZ
      Z=CSZ
      DO 108 J=1,ARP
      CALL MULC(CPR(I,J),CPI(I,J),XVECR(J),-XVECI(J),W,X)
c      print *,'MULC(CPR(I,J),CPI(I,J),XVECR(J),-XVECI(J),W,X)'
c      print *,'XVECR(J)=',XVECR(J)
c      print *,'XVECI(J)=',XVECI(J)
c      print *,'W=',W
c      print *,'X=',X
      Y=Y+W
      Z=Z+X
  108 CONTINUE
      MR(I)=Y
      MI(I)=Z
c      print *,'MR(I)=',Y
c      print *,'MI(I)=',Z
  109 CONTINUE
      PEV=CSZ
      PRED=CSZ
      DO 110 I=1,ARP
      U=XVECR(I)
      V=XVECI(I)
      CALL MULC(U,V,MR(I),MI(I),W,X)
      PEV=PEV+W
c      print *,'PEV=',PEV
      CALL MULC(U,V,WPR(I),WPI(I),W,X)
      PRED=PRED+W
c      print *,'PRED=',PRED
  110 CONTINUE
      PEV=PEV+VOB
      VT(T)=PEV
      U=SER(T)-CONST-PRED
      ERR(T)=U
c      print *,'ERR=',U
      DO 111 I=1,ARP
      NR(I)=MR(I)/PEV
      NI(I)=MI(I)/PEV
      CALL MULR(NR(I),NI(I),U,W,X)
      CALL ADDC(WPR(I),WPI(I),W,X,WR(I),WI(I))
  111 CONTINUE
      DO 112 I=1,ARP
      DO 112 J=1,ARP
      CALL MULC(MR(I),MI(I),NR(J),-NI(J),W,X)
c      print *,'CALL MULC(MR(I),MI(I),NR(J),-NI(J),W,X)'
c      print *,'W=',W
c      print *,'X=',X
      CALL SUBC(CPR(I,J),CPI(I,J),W,X,CR(I,J),CI(I,J))
c      print *,'CALL SUBC(CPR(I,J),CPI(I,J),W,X,CR(I,J),CI(I,J))'
c      print *,'CR(I,J)=',CR(I,J)
c      print *,'CI(I,J)=',CI(I,J)
 112  CONTINUE
c     add the following lines to store results in order to calculate smoothic     ng states and covariance matrix, smoothing series and variances
      if (KFSW.EQ.0) then
         pre(T)=const+pred
         prv(T)=pev
         u=csz
         do 130 I=1, ARP
            FSESR(T,I)=WR(I)
            FSESI(T,I)=WI(I)
            PSESR(T,I)=WPR(I)
            PSESI(T,I)=WPI(I)
 130        U=U+XVECR(I)*WR(I)-XVECI(I)*WI(I)
            FSER0(T,I)=U+CONST
            U=CSZ
            do 135 I=1, ARP
               TRANR(T,I)=DR(I)
               TRANI(T,I)=DI(I)
               do 135 J=1, ARP
                  FSCVR(T,I,J)=CR(I,J)
                  FSCVI(T,I,J)=CI(I,J)
                  PSCVR(T,I,J)=CPR(I,J)
 135              PSCVI(T,I,J)=CPI(I,J)
c 135              U=U+
               ENDIF
 150        CONTINUE
            RETURN
            END
      
