*UPTODATE BALANCTEXT
      SUBROUTINE BALANC(NM,N,A,LOW,IGH,SCALE)
C
C     .. Scalar Arguments ..
      INTEGER IGH,LOW,N,NM
C     .. Array Arguments ..
      DOUBLE PRECISION A(NM,N),SCALE(N)
C     .. Scalars in Common ..
      DOUBLE PRECISION BASE,BIG,EPS,SMALL
C     .. Local Scalars ..
      DOUBLE PRECISION B2,C,F,G,R,RADIX,S
      INTEGER I,IEXC,J,JJ,K,L,M
      LOGICAL NOCONV
C     .. External Subroutines ..
      EXTERNAL SDATA
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C     .. Common blocks ..
      COMMON /MDEPS/BASE,BIG,SMALL,EPS
C     .. Executable Statements ..
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE BALANCE,
C     NUM. MATH. 13, 293-304(1969) BY PARLETT AND REINSCH.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 315-326(1971).
C
C     THIS SUBROUTINE BALANCES A REAL MATRIX AND ISOLATES
C     EIGENVALUES WHENEVER POSSIBLE.
C
C     ON INPUT-
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT,
C
C        N IS THE ORDER OF THE MATRIX,
C
C        A CONTAINS THE INPUT MATRIX TO BE BALANCED.
C
C     ON OUTPUT-
C
C        A CONTAINS THE BALANCED MATRIX,
C
C        LOW AND IGH ARE TWO INTEGERS SUCH THAT A(I,J)
C          IS EQUAL TO ZERO IF
C           (1) I IS GREATER THAN J AND
C           (2) J=1,...,LOW-1 OR I=IGH+1,...,N,
C
C        SCALE CONTAINS INFORMATION DETERMINING THE
C           PERMUTATIONS AND SCALING FACTORS USED.
C
C     SUPPOSE THAT THE PRINCIPAL SUBMATRIX IN ROWS LOW THROUGH IGH
C     HAS BEEN BALANCED, THAT P(J) DENOTES THE INDEX INTERCHANGED
C     WITH J DURING THE PERMUTATION STEP, AND THAT THE ELEMENTS
C     OF THE DIAGONAL MATRIX USED ARE DENOTED BY D(I,J).  THEN
C        SCALE(J) = P(J),    FOR J = 1,...,LOW-1
C                 = D(J,J),      J = LOW,...,IGH
C                 = P(J)         J = IGH+1,...,N.
C     THE ORDER IN WHICH THE INTERCHANGES ARE MADE IS N TO IGH+1,
C     THEN 1 TO LOW-1.
C
C     NOTE THAT 1 IS RETURNED FOR IGH IF IGH IS ZERO FORMALLY.
C
C     THE ALGOL PROCEDURE EXC CONTAINED IN BALANCE APPEARS IN
C     BALANC  IN LINE.  (NOTE THAT THE ALGOL ROLES OF IDENTIFIERS
C     K,L HAVE BEEN REVERSED.)
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
      CALL SDATA
      RADIX = BASE
C
      B2 = RADIX*RADIX
      K = 1
      L = N
      GO TO 120
C     ********** IN-LINE PROCEDURE FOR ROW AND
C                COLUMN EXCHANGE **********
   20 SCALE(M) = J
      IF (J.EQ.M) GO TO 80
C
      DO 40 I = 1,L
          F = A(I,J)
          A(I,J) = A(I,M)
          A(I,M) = F
   40 CONTINUE
C
      DO 60 I = K,N
          F = A(J,I)
          A(J,I) = A(M,I)
          A(M,I) = F
   60 CONTINUE
C
   80 GO TO (100,180) IEXC
C     ********** SEARCH FOR ROWS ISOLATING AN EIGENVALUE
C                AND PUSH THEM DOWN **********
  100 IF (L.EQ.1) GO TO 460
      L = L - 1
C     ********** FOR J=L STEP -1 UNTIL 1 DO -- **********
  120 DO 160 JJ = 1,L
          J = L + 1 - JJ
C
          DO 140 I = 1,L
              IF (I.EQ.J) GO TO 140
              IF (A(J,I).NE.0.0D0) GO TO 160
  140     CONTINUE
C
          M = L
          IEXC = 1
          GO TO 20
  160 CONTINUE
C
      GO TO 200
C     ********** SEARCH FOR COLUMNS ISOLATING AN EIGENVALUE
C                AND PUSH THEM LEFT **********
  180 K = K + 1
C
  200 DO 240 J = K,L
C
          DO 220 I = K,L
              IF (I.EQ.J) GO TO 220
              IF (A(I,J).NE.0.0D0) GO TO 240
  220     CONTINUE
C
          M = K
          IEXC = 2
          GO TO 20
  240 CONTINUE
C     ********** NOW BALANCE THE SUBMATRIX IN ROWS K TO L **********
      DO 260 I = K,L
          SCALE(I) = 1.0D0
  260 CONTINUE
C     ********** ITERATIVE LOOP FOR NORM REDUCTION **********
  280 NOCONV = .FALSE.
C
      DO 440 I = K,L
          C = 0.0D0
          R = 0.0D0
C
          DO 300 J = K,L
              IF (J.EQ.I) GO TO 300
              C = C + ABS(A(J,I))
              R = R + ABS(A(I,J))
  300     CONTINUE
C     ********** GUARD AGAINST ZERO C OR R DUE TO UNDERFLOW **********
          IF (C.EQ.0.0D0 .OR. R.EQ.0.0D0) GO TO 440
          G = R/RADIX
          F = 1.0D0
          S = C + R
  320     IF (C.GE.G) GO TO 340
          F = F*RADIX
          C = C*B2
          GO TO 320
  340     G = R*RADIX
  360     IF (C.LT.G) GO TO 380
          F = F/RADIX
          C = C/B2
          GO TO 360
C     ********** NOW BALANCE **********
  380     IF ((C+R)/F.GE.0.95D0*S) GO TO 440
          G = 1.0D0/F
          SCALE(I) = SCALE(I)*F
          NOCONV = .TRUE.
C
          DO 400 J = K,N
              A(I,J) = A(I,J)*G
  400     CONTINUE
C
          DO 420 J = 1,L
              A(J,I) = A(J,I)*F
  420     CONTINUE
C
  440 CONTINUE
C
      IF (NOCONV) GO TO 280
C
  460 LOW = K
      IGH = L
      RETURN
C     ********** LAST CARD OF BALANC **********
      END
