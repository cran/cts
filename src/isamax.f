*UPTODATE ISAMAXTEXT
      INTEGER FUNCTION ISAMAX(N,SX,INCX)
C
C     FINDS THE INDEX OF ELEMENT HAVING MAX. ABSOLUTE VALUE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
C     .. Scalar Arguments ..
      INTEGER INCX,N
C     .. Array Arguments ..
      DOUBLE PRECISION SX(*)
C     .. Local Scalars ..
      DOUBLE PRECISION SMAX
      INTEGER I,IX
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C     .. Executable Statements ..
C
      ISAMAX = 0
      IF (N.LT.1) RETURN
      ISAMAX = 1
      IF (N.EQ.1) RETURN
      IF (INCX.EQ.1) GO TO 60
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      IX = 1
      SMAX = ABS(SX(1))
      IX = IX + INCX
      DO 40 I = 2,N
          IF (ABS(SX(IX)).LE.SMAX) GO TO 20
          ISAMAX = I
          SMAX = ABS(SX(IX))
   20     IX = IX + INCX
   40 CONTINUE
      RETURN
C
C        CODE FOR INCREMENT EQUAL TO 1
C
   60 SMAX = ABS(SX(1))
      DO 80 I = 2,N
          IF (ABS(SX(I)).LE.SMAX) GO TO 80
          ISAMAX = I
          SMAX = ABS(SX(I))
   80 CONTINUE
      RETURN
      END
