*UPTODATE SAXPYTEXT
      SUBROUTINE SAXPY(N,SA,SX,INCX,SY,INCY)
C
C     CONSTANT TIMES A VECTOR PLUS A VECTOR.
C     USES UNROLLED LOOP FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION SA
      INTEGER INCX,INCY,N
C     .. Array Arguments ..
      DOUBLE PRECISION SX(*),SY(*)
C     .. Local Scalars ..
      INTEGER I,IX,IY,M,MP1
C     .. Intrinsic Functions ..
      INTRINSIC MOD
C     .. Executable Statements ..
C
      IF (N.LE.0) RETURN
      IF (SA.EQ.0.0D0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) GO TO 40
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
      IX = 1
      IY = 1
      IF (INCX.LT.0) IX = (-N+1)*INCX + 1
      IF (INCY.LT.0) IY = (-N+1)*INCY + 1
      DO 20 I = 1,N
          SY(IY) = SY(IY) + SA*SX(IX)
          IX = IX + INCX
          IY = IY + INCY
   20 CONTINUE
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   40 M = MOD(N,4)
      IF (M.EQ.0) GO TO 80
      DO 60 I = 1,M
          SY(I) = SY(I) + SA*SX(I)
   60 CONTINUE
      IF (N.LT.4) RETURN
   80 MP1 = M + 1
      DO 100 I = MP1,N,4
          SY(I) = SY(I) + SA*SX(I)
          SY(I+1) = SY(I+1) + SA*SX(I+1)
          SY(I+2) = SY(I+2) + SA*SX(I+2)
          SY(I+3) = SY(I+3) + SA*SX(I+3)
  100 CONTINUE
      RETURN
      END
