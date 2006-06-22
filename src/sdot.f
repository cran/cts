*UPTODATE SDOTTEXT
      DOUBLE PRECISION FUNCTION SDOT(N,SX,INCX,SY,INCY)
C
C     FORMS THE DOT PRODUCT OF TWO VECTORS.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
C     .. Scalar Arguments ..
      INTEGER INCX,INCY,N
C     .. Array Arguments ..
      DOUBLE PRECISION SX(*),SY(*)
C     .. Local Scalars ..
      DOUBLE PRECISION STEMP
      INTEGER I,IX,IY,M,MP1
C     .. Intrinsic Functions ..
      INTRINSIC MOD
C     .. Executable Statements ..
C
      STEMP = 0.0D0
      SDOT = 0.0D0
      IF (N.LE.0) RETURN
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
          STEMP = STEMP + SX(IX)*SY(IY)
          IX = IX + INCX
          IY = IY + INCY
   20 CONTINUE
      SDOT = STEMP
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   40 M = MOD(N,5)
      IF (M.EQ.0) GO TO 80
      DO 60 I = 1,M
          STEMP = STEMP + SX(I)*SY(I)
   60 CONTINUE
      IF (N.LT.5) GO TO 120
   80 MP1 = M + 1
      DO 100 I = MP1,N,5
          STEMP = STEMP + SX(I)*SY(I) + SX(I+1)*SY(I+1) +
     +            SX(I+2)*SY(I+2) + SX(I+3)*SY(I+3) + SX(I+4)*SY(I+4)
  100 CONTINUE
  120 SDOT = STEMP
      RETURN
      END
