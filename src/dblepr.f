      SUBROUTINE DBLEPR(MESAGE,LMESS,VALUE,N)
C
C  Print a message and double precision values.
C
      INTEGER I, LMESS, NMESS
      DOUBLE PRECISION VALUE(*)
      CHARACTER*(*) MESAGE
      INTEGER N
      INTRINSIC MIN, LEN
C
      NMESS = LEN(MESAGE)
      IF (LMESS .GE. 0) NMESS = MIN(LMESS, NMESS)
C     IF (NMESS .GT. 0) WRITE(*,'(A)') MESAGE(1:NMESS)
C     IF (N .GT. 0) WRITE(*,*) (VALUE(I), I=1,N)
      RETURN
      END
