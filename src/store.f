*UPTODATE STORETEXT
      DOUBLE PRECISION FUNCTION STORE(A,B)
C
C        MARK 2.2 RELEASE.  NAG COPYRIGHT 1987.
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION A,B
C     .. Executable Statements ..
C
C     STORE is intended to force A and B to be stored prior to doing the
C     addition of A and B. For use in situations where optimizers might
C     hold one of these in a register.
C
C
C  Nag Fortran 77 O( 1 ) basic linear algebra routine.
C
C  -- Written on 28-November-1984.
C     Sven Hammarling, Nag Central Office.
C
      STORE = A + B
C
      RETURN
C
C     End of STORE .
C
      END
