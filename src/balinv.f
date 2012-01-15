*UPTODATE BALINVTEXT
      SUBROUTINE BALINV(NM,N,LOW,IGH,SCALE,A)
C
C        MARK 2.2 RELEASE.  NAG COPYRIGHT 1987.
C
C     .. Scalar Arguments ..
      INTEGER IGH,LOW,N,NM
C     .. Array Arguments ..
      DOUBLE PRECISION A(NM,N),SCALE(N)
C     .. Local Scalars ..
      DOUBLE PRECISION S
      INTEGER I,II,J,K
C     .. Executable Statements ..
      IF (IGH.EQ.LOW) GO TO 100
      DO 40 I = LOW,IGH
          S = SCALE(I)
          DO 20 J = 1,N
              A(I,J) = S*A(I,J)
   20     CONTINUE
   40 CONTINUE
      DO 80 J = LOW,IGH
          S = 1.0D0/SCALE(J)
          DO 60 I = 1,N
              A(I,J) = A(I,J)*S
   60     CONTINUE
   80 CONTINUE
  100 DO 160 II = 1,N
          I = II
          IF (I.GE.LOW .AND. I.LE.IGH) GO TO 160
          IF (I.LT.LOW) I = LOW - II
          K = SCALE(I)
          IF (K.EQ.I) GO TO 160
          DO 120 J = 1,N
              S = A(I,J)
              A(I,J) = A(K,J)
              A(K,J) = S
  120     CONTINUE
          DO 140 J = 1,N
              S = A(J,I)
              A(J,I) = A(J,K)
              A(J,K) = S
  140     CONTINUE
  160 CONTINUE
      RETURN
      END
