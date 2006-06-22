      subroutine setfor (PRE1,PRV1,TIM1)

      INTEGER I
c      INCLUDE 'model.txt'
c      INCLUDE 'series.txt'
c      INCLUDE 'kfasav.txt'
C*****model.txt
      INTEGER PFI,ARP,VRI,CCV,LYAP,SCC,fct
      DOUBLE PRECISION SCALE,VR,CONST,PHI(20),PRDG
      COMMON/MODEL/SCALE,VR,CONST,PHI,PRDG,PFI,ARP,VRI,CCV,LYAP,SCC,fct
      integer for, fty
      common/model/for, fty
C*****series.txt
      CHARACTER*40 NAME
      INTEGER LEN
      DOUBLE PRECISION TIM(500),SER(500),TDIF(500)
c      COMMON/SERIES/NAME,LEN,TIM,SER,TDIF
      COMMON/SERIES/TIM,SER,TDIF,NAME,LEN
C*****kfasav.txt
      REAL PRE(500),PRV(500),FSER(500),FVAR(500),SSER(500),SVAR(500)
      DOUBLE PRECISION FSES(500,20),FSCV(500,20,20),SSES(500,20)
     1,SSCV(500,20,20),PSES(500,20),PSCV(500,20,20),TRAN(500,20,20)
c      COMMON/KFASAV/PRE,PRV,FSER,FVAR,SSER,SVAR,FSES,FSCV,SSES,SSCV
c     1,PSES,PSCV,TRAN
      COMMON/KFASAV/FSES,FSCV,SSES,SSCV
     1,PSES,PSCV,TRAN,PRE,PRV,FSER,FVAR,SSER,SVAR

C*****kfasav1.txt
      DOUBLE PRECISION PRE1(500),PRV1(500),TIM1(500)
C*****series1.txt

      DO 90 I=1,LEN+FOR
         TIM1(I)=TIM(I)
 90   CONTINUE
      
      IF(fct.EQ.0)then
      DO 102 I=1,for
         PRE1(I)=PRE(LEN+I)
c changed next line 6/7/06
         PRV1(I)=PRV(LEN+I)
         TIM1(I)=TIM(LEN+I)
 102     CONTINUE
      ELSE
        if(fty.EQ.1)then
c          LEN=LEN-FOR, changed 6/7/06
          do 103 I=1,for
          PRE1(I)=PRE(LEN+I)
c changed the next line 6/7/06
          PRV1(I)=PRV(LEN+I)
 103      Continue
        else 
          if(fty.EQ.2)then
           DO 104 I=1,for
c           PRE1(I)=PRE(LEN-for+I) changed 6/6/06
c           PRV1(I)=PRV(LEN-for+I)
           PRE1(I)=PRE(LEN+I)
           PRV1(I)=PRV(LEN+I)
 104       CONTINUE
          else
           do 105 I=1,for
           pre1(I)=PRE(LEN+I)
           prv1(I)=PRV(LEN+I)
 105       continue
          endif
        endif
      ENDIF
      RETURN
      END
