      SUBROUTINE CSPEC(B1,ARP1,SCALE1,FRMULT,NFREQ,F1,S1)
      IMPLICIT NONE
      INTEGER I,J,M,NF,NF1,NFREQ,ARP1
      DOUBLE PRECISION B1(ARP1+1),F1(NFREQ),S1(NFREQ)
      DOUBLE PRECISION FAC,P2,S2,F,S,U,V,W,X,Y,Z,SCALE1
c      INCLUDE 'model.txt'
c      INCLUDE 'setcon.txt'
c      INCLUDE 'repar3.txt'
c      INCLUDE 'veccom.txt'
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
C*****repar3.txt
      DOUBLE PRECISION ALPHA(21),ROOTR(20),ROOTI(20),XVECR(20),
     *XVECI(20)
      COMMON/REPAR3/ALPHA,ROOTR,ROOTI,XVECR,XVECI
C*****veccom.txt
      DOUBLE PRECISION B(22),NEWB(22),PERB(22),DELB(22),
     * ERR(5000),RES(5000),PRES(22,5000),sres(5000)
      COMMON/VECCOM/B,NEWB,PERB,DELB,ERR,RES,PRES,sres
      DOUBLE PRECISION FRMULT

c      PRINT *, 'SAVING OF MODEL SPECTRUM IN PROGRESS'
      P2=6.2831853071795864D0
c     added by ZW
      ARP=ARP1
      DO 10 J=1,ARP+1
         B(J)=B1(J)
 10   CONTINUE
      SCALE=SCALE1
      S2=SCALE*SCALE
C      FRMULT=1.0
      M=ARP-1
c      NF=200
      NF=NFREQ-1
      NF1=NF+1
c      OPEN(UNIT=4,FILE='frqspc.dat',STATUS='NEW')
c      IF(PFI.EQ.1)THEN
c        PRINT *,'NOT YET IMPLEMENTED'
c      ELSE IF(PFI.EQ.2)THEN
c        PRINT *,'NOT YET IMPLEMENTED'
c      ELSE IF(PFI.GE.3)THEN
        IF(PFI.GE.3)THEN
        CALL ROOTS
        IF(PFI.GE.5)THEN
          DO 50 J=1,ARP
          U=ROOTR(J)
          Z=ROOTI(J)
          W=CSO-U
          X=-Z
          Y=CSO+U
          CALL DIVC(W,X,Y,Z,U,V)
          CALL MULR(U,V,-SCALE,ROOTR(J),ROOTI(J))
   50     CONTINUE
        END IF
c        PRINT *,'FINAL ROOTS: '
c        DO 100 I=1,ARP
c        PRINT *,'   ',I,' ( ',ROOTR(I),' , ',ROOTI(I),' I)'
c  100   CONTINUE
        DO 102 I=1,NF1
        F=FRMULT*DBLE(FLOAT(I-1)/NF)*SCALE
        Y=P2*F
        FAC=CSO
        DO 101 J=1,ARP
        W=ROOTR(J)
        V=ROOTI(J)+Y
        FAC=FAC*(W*W+V*V)
  101   CONTINUE
        S=CSO/FAC
        IF(PFI.EQ.4.OR.PFI.EQ.6)THEN
C          FAC=SCALE*SCALE+Y*Y
C  July 20, 2012. This is not consistent with the factor in the numerator of your equation (6) taken from Belcher et al equation (13). To be consistent the statement should be
          FAC=1+Y*Y/(SCALE*SCALE)
          S=S*(FAC**M)
        END IF
        F1(I)=F
C        S1(I)=S, changed July 20, 2012, ZW. Spectrum should be
C        multiplied by 2\pi \sigma^2
        S1(I)=P2*SIGSQ*S
  102   CONTINUE
      END IF
      RETURN
      END
