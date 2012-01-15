*UPTODATE ORTHESTEXT
      SUBROUTINE ORTHES(NM,N,LOW,IGH,A,ORT)
C
C     .. Scalar Arguments ..
      INTEGER IGH,LOW,N,NM
C     .. Array Arguments ..
      DOUBLE PRECISION A(NM,N),ORT(IGH)
C     .. Local Scalars ..
      DOUBLE PRECISION F,G,H,SCALE
      INTEGER I,II,J,JJ,KP1,LA,M,MP
C     .. Intrinsic Functions ..
      INTRINSIC ABS,SIGN,SQRT
C     .. Executable Statements ..
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE ORTHES,
C     NUM. MATH. 12, 349-368(1968) BY MARTIN AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971).
C
C     GIVEN A REAL GENERAL MATRIX, THIS SUBROUTINE
C     REDUCES A SUBMATRIX SITUATED IN ROWS AND COLUMNS
C     LOW THROUGH IGH TO UPPER HESSENBERG FORM BY
C     ORTHOGONAL SIMILARITY TRANSFORMATIONS.
C
C     ON INPUT-
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT,
C
C        N IS THE ORDER OF THE MATRIX,
C
C        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING
C          SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED,
C          SET LOW=1, IGH=N,
C
C        A CONTAINS THE INPUT MATRIX.
C
C     ON OUTPUT-
C
C        A CONTAINS THE HESSENBERG MATRIX.  INFORMATION ABOUT
C          THE ORTHOGONAL TRANSFORMATIONS USED IN THE REDUCTION
C          IS STORED IN THE REMAINING TRIANGLE UNDER THE
C          HESSENBERG MATRIX,
C
C        ORT CONTAINS FURTHER INFORMATION ABOUT THE TRANSFORMATIONS.
C          ONLY ELEMENTS LOW THROUGH IGH ARE USED.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
      LA = IGH - 1
      KP1 = LOW + 1
      IF (LA.LT.KP1) GO TO 200
C
      DO 180 M = KP1,LA
          H = 0.0D0
          ORT(M) = 0.0D0
          SCALE = 0.0D0
C     ********** SCALE COLUMN (ALGOL TOL THEN NOT NEEDED) **********
          DO 20 I = M,IGH
              SCALE = SCALE + ABS(A(I,M-1))
   20     CONTINUE
C
          IF (SCALE.EQ.0.0D0) GO TO 180
          MP = M + IGH
C     ********** FOR I=IGH STEP -1 UNTIL M DO -- **********
          DO 40 II = M,IGH
              I = MP - II
              ORT(I) = A(I,M-1)/SCALE
              H = H + ORT(I)*ORT(I)
   40     CONTINUE
C
          G = -SIGN(SQRT(H),ORT(M))
          H = H - ORT(M)*G
          ORT(M) = ORT(M) - G
C     ********** FORM (I-(U*UT)/H) * A **********
          DO 100 J = M,N
              F = 0.0D0
C     ********** FOR I=IGH STEP -1 UNTIL M DO -- **********
              DO 60 II = M,IGH
                  I = MP - II
                  F = F + ORT(I)*A(I,J)
   60         CONTINUE
C
              F = F/H
C
              DO 80 I = M,IGH
                  A(I,J) = A(I,J) - F*ORT(I)
   80         CONTINUE
C
  100     CONTINUE
C     ********** FORM (I-(U*UT)/H)*A*(I-(U*UT)/H) **********
          DO 160 I = 1,IGH
              F = 0.0D0
C     ********** FOR J=IGH STEP -1 UNTIL M DO -- **********
              DO 120 JJ = M,IGH
                  J = MP - JJ
                  F = F + ORT(J)*A(I,J)
  120         CONTINUE
C
              F = F/H
C
              DO 140 J = M,IGH
                  A(I,J) = A(I,J) - F*ORT(J)
  140         CONTINUE
C
  160     CONTINUE
C
          ORT(M) = SCALE*ORT(M)
          A(M,M-1) = SCALE*G
  180 CONTINUE
C
  200 RETURN
C     ********** LAST CARD OF ORTHES **********
      END
