*UPTODATE GETMINTEXT
      SUBROUTINE GETMIN(EMIN,START,BASE)
C
C        MARK 2.2 RELEASE.  NAG COPYRIGHT 1987.
C
C     Service routine for ENVIRN.
C
C
C  Nag Fortran 77 O( 1 ) basic linear algebra routine.
C
C  -- Written on 2-June-1987.
C     Mick Pont, Nag Central Office.
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION START
      INTEGER BASE,EMIN
C     .. Local Scalars ..
      DOUBLE PRECISION A,B1,B2,C1,C2,D1,D2,ONE,RBASE,ZERO
      INTEGER I
C     .. External Functions ..
      DOUBLE PRECISION STORE
      EXTERNAL STORE
C     .. Executable Statements ..
      A = START
      ONE = 1
      RBASE = ONE/BASE
      ZERO = 0
      EMIN = 1
      B1 = STORE(A*RBASE,ZERO)
      C1 = A
      C2 = A
      D1 = A
      D2 = A
   20 IF ((C1.EQ.A) .AND. (C2.EQ.A) .AND. (D1.EQ.A) .AND.
     +    (D2.EQ.A)) THEN
          EMIN = EMIN - 1
          A = B1
          B1 = STORE(A/BASE,ZERO)
          C1 = STORE(B1*BASE,ZERO)
          D1 = ZERO
          DO 40 I = 1,BASE
              D1 = D1 + B1
   40     CONTINUE
          B2 = STORE(A*RBASE,ZERO)
          C2 = STORE(B2/RBASE,ZERO)
          D2 = ZERO
          DO 60 I = 1,BASE
              D2 = D2 + B2
   60     CONTINUE
          GO TO 20
      END IF
      RETURN
      END
