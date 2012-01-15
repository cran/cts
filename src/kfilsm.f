      SUBROUTINE KFILSM
      IMPLICIT NONE
      INTEGER I, T
c      INCLUDE 'model.txt'
c      INCLUDE 'repcom.txt'
c      INCLUDE 'kfasav.txt'
c      INCLUDE 'setcon.txt'
c      INCLUDE 'veccom.txt'
c      INCLUDE 'series.txt'
c      INCLUDE 'repar3.txt'
C*****model.txt
      INTEGER PFI,ARP,VRI,CCV,LYAP,SCC,fct
      DOUBLE PRECISION SCALE,VR,CONST,PHI(20),PRDG
      COMMON/MODEL/SCALE,VR,CONST,PHI,PRDG,PFI,ARP,VRI,CCV,LYAP,SCC,fct
      integer for, fty
      common/model/for, fty
      integer ARI, tra
      common/model/ari,tra

C*****repcom.txt
      LOGICAL PRPI
      INTEGER REQSW,KFSW
      DOUBLE PRECISION VOB
c      COMMON/REPCOM/PRPI,REQSW,VOB,KFSW
      COMMON/REPCOM/VOB,REQSW,KFSW,PRPI	
C*****kfasav.txt
      REAL PRE(500),PRV(500),FSER(500),FVAR(500),SSER(500),SVAR(500)
      DOUBLE PRECISION FSES(500,20),FSCV(500,20,20),SSES(500,20)
     1,SSCV(500,20,20),PSES(500,20),PSCV(500,20,20),TRAN(500,20,20)
c      COMMON/KFASAV/PRE,PRV,FSER,FVAR,SSER,SVAR,FSES,FSCV,SSES,SSCV
c     1,PSES,PSCV,TRAN
      COMMON/KFASAV/FSES,FSCV,SSES,SSCV
     1,PSES,PSCV,TRAN,PRE,PRV,FSER,FVAR,SSER,SVAR
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
C*****series.txt
      CHARACTER*40 NAME
      INTEGER LEN
      DOUBLE PRECISION TIM(500),SER(500),TDIF(500)
c      COMMON/SERIES/NAME,LEN,TIM,SER,TDIF
      COMMON/SERIES/TIM,SER,TDIF,NAME,LEN
C*****repar3.txt
      DOUBLE PRECISION ALPHA(21),ROOTR(20),ROOTI(20),XVECR(20),
     *XVECI(20)
      COMMON/REPAR3/ALPHA,ROOTR,ROOTI,XVECR,XVECI

      if(tra.EQ.1)then
C      PRINT *,'SAVING OF FILTERED AND SMOOTHED STATES IN PROGRESS' 
      end if
      KFSW=1
      DO 10 I=1,NP
  10  B(I)=OLDB(I)
c      print *,'B',(B(I),I=1,NP)
      CALL REPAR
      CALL RESG1    
C      RETURN
      CALL REVG1
c     calculate standardised residuals (innovations)
c     SIGSQ is the innovation variance
      do 11 T=1,LEN
         sres(T)=err(T)/sqrt(prv(T))/sqrt(SIGSQ)
 11   continue
C     STORE FILTERED AND SMOOTHED VALUES, TOGETHER WITH ORIGINAL DATA
c      OPEN(UNIT=4,FILE='filser.dat',STATUS='unknown')
c      WRITE(4,195)
c  195 FORMAT(7H LENGTH)
c      WRITE(4,196) LEN
c  196 FORMAT(I10)
c      WRITE(4,201) 
c  201 FORMAT(54H  TIME SERIES FILSER FILVAR ERROR ERRVAR SMOSER SMOVAR)
c  201 FORMAT(68H  TIME SERIES FILSER FILVAR ERROR ERRVAR SMOSER SMOVAR I
c     +NNSER STDINN)
c      DO 20 T=1,LEN
c      WRITE(4,202) TIM(T),SER(T),FSER(T),FVAR(T)
c      WRITE(4,202) PRE(T),PRV(T),SSER(T),SVAR(T)
c   20 write(4,202) err(T),sres(T)
c  202 FORMAT(4E13.5)
c      CLOSE(UNIT=4)
c      OPEN(UNIT=4,FILE='fsest.dat',STATUS='unknown')
c      WRITE(4,295)
c  295 FORMAT(18H  DIMENSION LENGTH )
c      WRITE(4,296) ARP,LEN
c  296 FORMAT(2I10)
c      WRITE(4,300)
c  300 FORMAT(20H  OBSERVATION VECTOR  )
c      WRITE(4,302) (XVECR(I),I=1,ARP)
c      WRITE(4,301)
c  301 FORMAT(35H  FILSTATE SMOOSTATE FILVAR SMOOVAR  )
c      DO 30 T=1,LEN
c      WRITE(4,302) (FSES(T,I),I=1,ARP)
c      WRITE(4,302) (SSES(T,I),I=1,ARP)
c      WRITE(4,302) (FSCV(T,I,I),I=1,ARP)
c   30 WRITE(4,302) (SSCV(T,I,I),I=1,ARP)
c  302 FORMAT(4E13.5)
c      CLOSE(UNIT=4)
      RETURN
      END

