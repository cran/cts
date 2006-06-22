c     SUBROUTINE KFILSM
      SUBROUTINE FORECAST
      INTEGER T
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


      print *,'FORECASTING IN PROGRESS'
      KFSW=1
      DO 10 I=1,NP
  10  B(I)=OLDB(I)

      CALL REPAR
c     resg1pre and resg1pre1 together are predicting
c fct=0 means to use part (len-for) of data to fit a model, then forecast. So the data length will be changed.
      if(fct.EQ.0)then
         print *,'  THE LENGTH OF THE DATA USED TO MODEL IS'
     *,LEN 
         print *,'  FCT=0, FORECAST LAST L-STEP'  
c note: for fct=0, LEN has been reduced to LEN-FOR in setup subroutine      
         call resg1pre1
c fct=1 means to use all data to fit a model, then forecast.
c subroutine resg1pre uses len-for of data to estimate optimal state and covariance matrices.
      else
         print *,'  THE LENGTH OF THE DATA USED TO MODEL IS'
     *,LEN         
         if(fty.EQ.1)then
            print *,'  FCT=1 AND FTY=1, FORECAST PAST THE END'
c     construct equally spaced time points to forecast
c     this may be changed, for example, from input file
            do 15 T=LEN+1,LEN+FOR
               TIM(T)=TIM(T-1)+1
c changed 6/6/06
               TDIF(T)=TIM(T)-TIM(T-1)  
 15         CONTINUE
            call resg1pre1
         else
            if(fty.EQ.2)then
               print *,'  FCT=1 AND FTY=2, FORECAST LAST L-STEP'
c               estimate state and convariance matrix up to time LEN-FOR
               call resg1pre
               LEN=LEN-FOR
               call resg1pre1
             else
                print *,'  FCT=1 AND FTY=3, FORECAST LAST L-STEP UPDATE
     *D(FILTERING)' 
              endif
         endif
      endif

ccccccccccc
      open(unit=4,file='forecast.dat',status='UNKNOWN')
      WRITE(4,195)
  195 FORMAT(7H LENGTH)
      WRITE(4,196) for
  196 FORMAT(I10)
      WRITE(4,201) 
  201 format(27H  TIME SERIES PRESER PREVAR) 
      if(fct.EQ.0)then
         DO 20 T=LEN+1,LEN+FOR
 20         write(4,202) TIM(T),SER(T),PRE(T),PRV(T)
 202        FORMAT(4E13.5)
         else
            if(fty.EQ.3)then
               LEN=LEN-FOR
            endif
            do 30 T=LEN+1,LEN+for
 30            write(4,203) TIM(T),SER(T),PRE(T),PRV(T)
 203           FORMAT(4E13.5)
            endif
            CLOSE(UNIT=4)
            RETURN
            END
