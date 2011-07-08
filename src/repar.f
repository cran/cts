      SUBROUTINE REPAR
      IMPLICIT NONE
      INTEGER I,J,K,K1,K2,M
      DOUBLE PRECISION REV,S,T,U,V,W,X,Y,Z
c      INCLUDE 'model.txt'
c      INCLUDE 'conpar.txt'
c      INCLUDE 'setcon.txt'
c      INCLUDE 'veccom.txt'
c      INCLUDE 'repcom.txt'
c      INCLUDE 'repar2.txt'
c      INCLUDE 'repar3.txt'
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
C*****repar2.txt
      DOUBLE PRECISION BCT(21),CHT(21,21)
      COMMON/REPAR2/BCT,CHT
C*****repar3.txt
      DOUBLE PRECISION ALPHA(21),ROOTR(20),ROOTI(20),XVECR(20),
     *XVECI(20)
      COMMON/REPAR3/ALPHA,ROOTR,ROOTI,XVECR,XVECI


      M=ARP+1
      IF(PFI.EQ.1)THEN
C        PRINT *,'NOT YET IMPLEMENTED'
      ELSE IF(PFI.EQ.2)THEN
C        PRINT *,'NOT YET IMPLEMENTED'
      ELSE IF(PFI.GE.3)THEN
        CALL ROOTS
        J=ARP+1
        VOB=CSZ
        IF(VRI.EQ.1)VOB=B(J)*B(J)
        IF(CCV.EQ.2)THEN
          IF(VRI.EQ.0)THEN
            CONST=B(J)
          ELSE
            CONST=B(J+1)
          END IF
        END IF
        IF(PFI.GE.5)THEN
          DO 500 K=1,ARP
          U=ROOTR(K)
          Z=ROOTI(K)
          W=CSO-U
          X=-Z
          Y=CSO+U
          CALL DIVC(W,X,Y,Z,U,V)
          CALL MULR(U,V,-SCALE,ROOTR(K),ROOTI(K))
  500     CONTINUE
        END IF
        PRPI=.FALSE.
        IF (SCC.EQ.1) THEN
          DO 310 K=1,ARP
          IF(ROOTR(K).GE.CSZ)THEN
            PRPI=.TRUE.
            RETURN
          END IF
  310     CONTINUE
        ENDIF
c        print *,'KFSW',kfsw
        IF(KFSW.EQ.0) THEN
          REV=CSO
          IF(ARP.GT.1)THEN
            DO 400 I=2,ARP
            K=I-1
            S=ROOTR(I)
            T=ROOTI(I)
c            print *,'S',S
c            print *,'T',T
            DO 400 J=1,K
            Y=ROOTR(J)
            Z=ROOTI(J)
            CALL SUBC(S,T,Y,Z,U,V)
            CALL ADDC(S,T,Y,Z,W,X)
            U=(U*U+V*V)/(W*W+X*X)
            IF(REV.GT.U)REV=U
c            print *,'REV',REV
  400       CONTINUE
          END IF
          REV=REV*1.0D2
          REV=REV/(CSO+REV)
          IF(REV.GT.REQ)THEN
            REQSW=0
          ELSE
            REQSW=1
          ENDIF
        ELSE
          REQSW=1
        ENDIF
        IF(REQSW.EQ.0)THEN
          K1=ARP-1
          DO 321 K=1,ARP
          U=CSO
          V=CSZ
          IF((PFI.EQ.4.OR.PFI.EQ.6).AND.ARP.GT.1)THEN
            S=CSO+ROOTR(K)/SCALE
            T=ROOTI(K)/SCALE
            Y=CSO
            Z=CSZ
            DO 320 K2=1,K1
            CALL MULC(S,T,Y,Z,W,X)
            Y=W
            Z=X
  320       CONTINUE
            U=W
            V=X
          END IF
          XVECR(K)=U
          XVECI(K)=V
  321     CONTINUE
        ELSE
          IF(PFI.GE.3)THEN
            IF(PFI.EQ.3.OR.PFI.EQ.5)THEN
              DO 330 K=1,ARP
              XVECR(K)=CSZ
  330         CONTINUE
              XVECR(1)=CSO
            ELSE
              DO 331 K=1,ARP
              XVECR(K)=BCT(K)
  331         CONTINUE
            END IF
            IF(PFI.GE.5)THEN
              DO 334 K=1,M
              S=CSZ
              U=CSO 
              DO 333 J=1,M
              S=S+CHT(K,J)*U
              U=B(J)
  333         CONTINUE
              IF(K.EQ.1)THEN
                V=S
              ELSE 
                ALPHA(K-1)=S/V
              END IF
  334         CONTINUE
            END IF
          END IF            
        END IF
      END IF
      RETURN
      END

