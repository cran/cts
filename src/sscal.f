*UPTODATE SSCALTEXT
      SUBROUTINE SSCAL(N,SA,SX,INCX)
C
C     SCALES A VECTOR BY A CONSTANT.
C     USES UNROLLED LOOPS FOR INCREMENT EQUAL TO 1.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION SA
      INTEGER INCX,N
C     .. Array Arguments ..
      DOUBLE PRECISION SX(*)
C     .. Local Scalars ..
      INTEGER I,M,MP1,NINCX
C     .. Intrinsic Functions ..
      INTRINSIC MOD
C     .. Executable Statements ..
C
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1) GO TO 40
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      NINCX = N*INCX
      DO 20 I = 1,NINCX,INCX
          SX(I) = SA*SX(I)
   20 CONTINUE
      RETURN
C
C        CODE FOR INCREMENT EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   40 M = MOD(N,5)
      IF (M.EQ.0) GO TO 80
      DO 60 I = 1,M
          SX(I) = SA*SX(I)
   60 CONTINUE
      IF (N.LT.5) RETURN
   80 MP1 = M + 1
      DO 100 I = MP1,N,5
          SX(I) = SA*SX(I)
          SX(I+1) = SA*SX(I+1)
          SX(I+2) = SA*SX(I+2)
          SX(I+3) = SA*SX(I+3)
          SX(I+4) = SA*SX(I+4)
  100 CONTINUE
      RETURN
      END
