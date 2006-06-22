      SUBROUTINE UPDATE
      CHARACTER*1 ST9
      CHARACTER*4 ST0,ST1
      INTEGER I,J,M,N
c      INCLUDE 'model.txt'
c      INCLUDE 'setcon.txt'
C*****model.txt
      INTEGER PFI,ARP,VRI,CCV,LYAP,SCC,fct
      DOUBLE PRECISION SCALE,VR,CONST,PHI(20),PRDG
      COMMON/MODEL/SCALE,VR,CONST,PHI,PRDG,PFI,ARP,VRI,CCV,LYAP,SCC,fct
      integer for, fty
      common/model/for, fty

C*****setcon.txt
      LOGICAL CONV,FAIL
      INTEGER NP,ITCT,PPIND
      DOUBLE PRECISION CSO,CSZ,LAM,SSOLD,GMOLD,GMNEW,SIGSQ,OLDB(22)
c      COMMON/SETCON/CONV,FAIL,NP,ITCT,PPIND,CSO,CSZ,LAM,SSOLD,
c     *GMOLD,GMNEW,SIGSQ,OLDB
      COMMON/SETCON/CSO,CSZ,LAM,SSOLD,
     *GMOLD,GMNEW,SIGSQ,OLDB,CONV,FAIL,NP,ITCT,PPIND

      PRINT *,'MODEL UPDATE IN PROGRESS'
        I=ARP+1
        IF(I.GT.20)THEN
          PRINT *,'PROGRAM FAILS: MAXIMUM ORDER EXCEEDED IN UPDATE'
          STOP
        END IF
        OPEN(UNIT=4,FILE='newmodel.dat',STATUS='unknown')
        ST0='PFI='
        IF(PFI.EQ.1)THEN
          ST1='QLFA'
        ELSE IF(PFI.EQ.2)THEN
          ST1='QLFS'
        ELSE IF(PFI.EQ.3)THEN
          ST1='DIRA'
        ELSE IF(PFI.EQ.4)THEN
          ST1='DIRS'
        ELSE IF(PFI.EQ.5)THEN
          ST1='MAPA'
        ELSE
          ST1='MAPS'
        END IF
        WRITE(4,100)ST0,ST1
  100   FORMAT(2A4)
        ST0='SCA='
        WRITE(4,101)ST0,SCALE
  101   FORMAT(A4,D14.6)
        ST0='ARP='
        WRITE(4,102)ST0,I
  102   FORMAT(A4,I3)
        ST0='ARI='
        ST9='Y'
        WRITE(4,103)ST0,ST9
  103   FORMAT(A4,A1)
        DO 200 J=1,ARP
        PHI(J)=OLDB(J)
  200   CONTINUE
        IF(PFI.EQ.1)THEN
C         TO BE DEFINED LATER
        ELSE IF(PFI.EQ.2)THEN
C         TO BE DEFINED LATER
        ELSE IF(PFI.EQ.3.OR.PFI.EQ.4)THEN
          PHI(I)=PHI(ARP)*SCALE
          DO 201 J=2,ARP
          M=ARP-J
          N=M+2
          PHI(N)=PHI(N)+PHI(M+1)*SCALE
  201     CONTINUE
          PHI(1)=PHI(1)+SCALE
        ELSE
          PHI(I)=CSZ
        END IF
        DO 202 J=1,I
        WRITE(4,104)PHI(J)
  104   FORMAT(D14.6)
  202   CONTINUE
        ST0='VRI='
        IF(VRI.EQ.0)THEN
          ST9='N'
        ELSE
          ST9='Y'
        END IF
        WRITE(4,103)ST0,ST9
        IF(VRI.EQ.1)THEN
          WRITE(4,104)VR
        END IF
        ST0='CCV='
        IF(CCV.EQ.0)THEN
          ST1='NULL'
        ELSE IF(CCV.EQ.1)THEN
          ST1='MNCT'
        ELSE
          ST1='CTES'
        END IF
        WRITE(4,100)ST0,ST1
        CLOSE(UNIT=4)
      RETURN
      END
