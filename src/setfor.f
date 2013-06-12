      subroutine setfor (PRE1,PRV1,TIM1)
      IMPLICIT NONE
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
      integer ARI, tra
      common/model/ari,tra      
C*****series.txt
      CHARACTER*40 NAME
      INTEGER LEN
      DOUBLE PRECISION TIM(5000),SER(5000),TDIF(5000)
c      COMMON/SERIES/NAME,LEN,TIM,SER,TDIF
      COMMON/SERIES/TIM,SER,TDIF,NAME,LEN
C*****kfasav.txt
      DOUBLE PRECISION PRE(5000),PRV(5000),FSER(5000),FVAR(5000)
     1,SSER(5000),SVAR(5000)
      DOUBLE PRECISION FSES(5000,20),FSCV(5000,20,20),SSES(5000,20)
     1,SSCV(5000,20,20),PSES(5000,20),PSCV(5000,20,20),TRAN(5000,20,20)
c      COMMON/KFASAV/PRE,PRV,FSER,FVAR,SSER,SVAR,FSES,FSCV,SSES,SSCV
c     1,PSES,PSCV,TRAN
      COMMON/KFASAV/FSES,FSCV,SSES,SSCV
     1,PSES,PSCV,TRAN,PRE,PRV,FSER,FVAR,SSER,SVAR

C*****kfasav1.txt
      DOUBLE PRECISION PRE1(5000),PRV1(5000),TIM1(5000)
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
C changed 6/6/06
C           PRE1(I)=PRE(LEN-for+I) 
C           PRV1(I)=PRV(LEN-for+I)
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
