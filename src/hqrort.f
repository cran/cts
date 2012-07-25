*UPTODATE HQRORTTEXT
      SUBROUTINE HQRORT(NM,N,LOW,IGH,H,WR,WI,Z,IERR)
C
C     .. Scalar Arguments ..
      INTEGER IERR,IGH,LOW,N,NM
C     .. Array Arguments ..
      DOUBLE PRECISION H(NM,N),WI(N),WR(N),Z(NM,N)
C     .. Local Scalars ..
      DOUBLE PRECISION NORM,P,Q,R,S,T,TST1,TST2,W,X,Y,ZZ
      INTEGER EN,ENM2,I,ITN,ITS,J,K,L,LL,M,MM,MP2,NA
      LOGICAL NOTLAS
C     .. Intrinsic Functions ..
      INTRINSIC ABS,MIN,SIGN,SQRT
C     .. Executable Statements ..
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE HQR2,
C     NUM. MATH. 16, 181-204(1970) BY PETERS AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971).
C
C     THIS VERSION INCORPORATES THE SUGGESTED EISPACK MODIFICATION
C     OF HQR2 TO PRODUCE HQRORT.
C
C     THIS SUBROUTINE FINDS THE EIGENVALUES AND THE SCHUR FACTOR Q
C     OF A REAL UPPER HESSENBERG MATRIX BY THE QR METHOD.  THE
C     THE SCHUR FACTOR OF A REAL GENERAL MATRIX CAN ALSO BE FOUND
C     IF  ORTHES  AND  ORTRAN  HAVE
C     BEEN USED TO REDUCE THIS GENERAL MATRIX TO HESSENBERG FORM
C     AND TO ACCUMULATE THE SIMILARITY TRANSFORMATIONS.
C
C     ON INPUT
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT.
C
C        N IS THE ORDER OF THE MATRIX.
C
C        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING
C          SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED,
C          SET LOW=1, IGH=N.
C
C        H CONTAINS THE UPPER HESSENBERG MATRIX.
C
C        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED BY  ELTRAN
C          AFTER THE REDUCTION BY  ELMHES, OR BY  ORTRAN  AFTER THE
C          REDUCTION BY  ORTHES, IF PERFORMED.  IF THE SCHUR FACTOR
C          OF THE HESSENBERG MATRIX IS DESIRED, Z MUST CONTAIN THE
C          IDENTITY MATRIX.
C
C     ON OUTPUT
C
C        H CONTAINS THE QUASI-TRIANGULAR SCHUR FACTOR.
C
C        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS,
C          RESPECTIVELY, OF THE EIGENVALUES.  THE EIGENVALUES
C          ARE UNORDERED EXCEPT THAT COMPLEX CONJUGATE PAIRS
C          OF VALUES APPEAR CONSECUTIVELY WITH THE EIGENVALUE
C          HAVING THE POSITIVE IMAGINARY PART FIRST.  IF AN
C          ERROR EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT
C          FOR INDICES IERR+1,...,N.
C
C        Z CONTAINS THE TRANSFORMED MATRIX Q*Z.
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          J          IF THE LIMIT OF 30*N ITERATIONS IS EXHAUSTED
C                     WHILE THE J-TH EIGENVALUE IS BEING SOUGHT.
C
C     CALLS CDIV FOR COMPLEX DIVISION.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
C
C     THIS VERSION DATED AUGUST 1983.
C
C     ------------------------------------------------------------------
C
      IERR = 0
      NORM = 0.0D0
      K = 1
C     .......... STORE ROOTS ISOLATED BY BALANC
C                AND COMPUTE MATRIX NORM ..........
      DO 40 I = 1,N
C
          DO 20 J = K,N
              NORM = NORM + ABS(H(I,J))
   20     CONTINUE
C
          K = I
          IF (I.GE.LOW .AND. I.LE.IGH) GO TO 40
          WR(I) = H(I,I)
          WI(I) = 0.0D0
   40 CONTINUE
C
      EN = IGH
      T = 0.0D0
      ITN = 30*N
C     .......... SEARCH FOR NEXT EIGENVALUES ..........
   60 IF (EN.LT.LOW) GO TO 640
      ITS = 0
      NA = EN - 1
      ENM2 = NA - 1
C     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT
C                FOR L=EN STEP -1 UNTIL LOW DO -- ..........
   80 DO 100 LL = LOW,EN
          L = EN + LOW - LL
          IF (L.EQ.LOW) GO TO 120
          S = ABS(H(L-1,L-1)) + ABS(H(L,L))
          IF (S.EQ.0.0D0) S = NORM
          TST1 = S
          TST2 = TST1 + ABS(H(L,L-1))
          IF (TST2.EQ.TST1) GO TO 120
  100 CONTINUE
C     .......... FORM SHIFT ..........
  120 X = H(EN,EN)
      IF (L.EQ.EN) GO TO 480
      Y = H(NA,NA)
      W = H(EN,NA)*H(NA,EN)
      IF (L.EQ.NA) GO TO 5000
      IF (ITN.EQ.0) GO TO 620
      IF (ITS.NE.10 .AND. ITS.NE.20) GO TO 160
C     .......... FORM EXCEPTIONAL SHIFT ..........
      T = T + X
C
      DO 140 I = LOW,EN
          H(I,I) = H(I,I) - X
  140 CONTINUE
C
      S = ABS(H(EN,NA)) + ABS(H(NA,ENM2))
      X = 0.75D0*S
      Y = X
      W = -0.4375D0*S*S
  160 ITS = ITS + 1
      ITN = ITN - 1
C     .......... LOOK FOR TWO CONSECUTIVE SMALL
C                SUB-DIAGONAL ELEMENTS.
C                FOR M=EN-2 STEP -1 UNTIL L DO -- ..........
      DO 180 MM = L,ENM2
          M = ENM2 + L - MM
          ZZ = H(M,M)
          R = X - ZZ
          S = Y - ZZ
          P = (R*S-W)/H(M+1,M) + H(M,M+1)
          Q = H(M+1,M+1) - ZZ - R - S
          R = H(M+2,M+1)
          S = ABS(P) + ABS(Q) + ABS(R)
          P = P/S
          Q = Q/S
          R = R/S
          IF (M.EQ.L) GO TO 200
          TST1 = ABS(P)* (ABS(H(M-1,M-1))+ABS(ZZ)+ABS(H(M+1,M+1)))
          TST2 = TST1 + ABS(H(M,M-1))* (ABS(Q)+ABS(R))
          IF (TST2.EQ.TST1) GO TO 200
  180 CONTINUE
C
  200 MP2 = M + 2
C
      DO 220 I = MP2,EN
          H(I,I-2) = 0.0D0
          IF (I.EQ.MP2) GO TO 220
          H(I,I-3) = 0.0D0
  220 CONTINUE
C     .......... DOUBLE QR STEP INVOLVING ROWS L TO EN AND
C                COLUMNS M TO EN ..........
      DO 460 K = M,NA
          NOTLAS = K .NE. NA
          IF (K.EQ.M) GO TO 240
          P = H(K,K-1)
          Q = H(K+1,K-1)
          R = 0.0D0
          IF (NOTLAS) R = H(K+2,K-1)
          X = ABS(P) + ABS(Q) + ABS(R)
          IF (X.EQ.0.0D0) GO TO 460
          P = P/X
          Q = Q/X
          R = R/X
  240     S = SIGN(SQRT(P*P+Q*Q+R*R),P)
          IF (K.EQ.M) GO TO 260
          H(K,K-1) = -S*X
          GO TO 280
  260     IF (L.NE.M) H(K,K-1) = -H(K,K-1)
  280     P = P + S
          X = P/S
          Y = Q/S
          ZZ = R/S
          Q = Q/P
          R = R/P
          IF (NOTLAS) GO TO 360
C     .......... ROW MODIFICATION ..........
          DO 300 J = K,N
              P = H(K,J) + Q*H(K+1,J)
              H(K,J) = H(K,J) - P*X
              H(K+1,J) = H(K+1,J) - P*Y
  300     CONTINUE
C
          J = MIN(EN,K+3)
C     .......... COLUMN MODIFICATION ..........
          DO 320 I = 1,J
              P = X*H(I,K) + Y*H(I,K+1)
              H(I,K) = H(I,K) - P
              H(I,K+1) = H(I,K+1) - P*Q
  320     CONTINUE
C     .......... ACCUMULATE TRANSFORMATIONS ..........
          DO 340 I = LOW,IGH
              P = X*Z(I,K) + Y*Z(I,K+1)
              Z(I,K) = Z(I,K) - P
              Z(I,K+1) = Z(I,K+1) - P*Q
  340     CONTINUE
          GO TO 440
  360     CONTINUE
C     .......... ROW MODIFICATION ..........
          DO 380 J = K,N
              P = H(K,J) + Q*H(K+1,J) + R*H(K+2,J)
              H(K,J) = H(K,J) - P*X
              H(K+1,J) = H(K+1,J) - P*Y
              H(K+2,J) = H(K+2,J) - P*ZZ
  380     CONTINUE
C
          J = MIN(EN,K+3)
C     .......... COLUMN MODIFICATION ..........
          DO 400 I = 1,J
              P = X*H(I,K) + Y*H(I,K+1) + ZZ*H(I,K+2)
              H(I,K) = H(I,K) - P
              H(I,K+1) = H(I,K+1) - P*Q
              H(I,K+2) = H(I,K+2) - P*R
  400     CONTINUE
C     .......... ACCUMULATE TRANSFORMATIONS ..........
          DO 420 I = LOW,IGH
              P = X*Z(I,K) + Y*Z(I,K+1) + ZZ*Z(I,K+2)
              Z(I,K) = Z(I,K) - P
              Z(I,K+1) = Z(I,K+1) - P*Q
              Z(I,K+2) = Z(I,K+2) - P*R
  420     CONTINUE
  440     CONTINUE
C
  460 CONTINUE
C
      GO TO 80
C     .......... ONE ROOT FOUND ..........
  480 H(EN,EN) = X + T
      WR(EN) = H(EN,EN)
      WI(EN) = 0.0D0
      EN = NA
      GO TO 60
C     .......... TWO ROOTS FOUND ..........
 5000 P = (Y-X)/2.0D0
      Q = P*P + W
      ZZ = SQRT(ABS(Q))
      H(EN,EN) = X + T
      X = H(EN,EN)
      H(NA,NA) = Y + T
      IF (Q.LT.0.0D0) GO TO 580
C     .......... REAL PAIR ..........
      ZZ = P + SIGN(ZZ,P)
      WR(NA) = X + ZZ
      WR(EN) = WR(NA)
      IF (ZZ.NE.0.0D0) WR(EN) = X - W/ZZ
      WI(NA) = 0.0D0
      WI(EN) = 0.0D0
      X = H(EN,NA)
      S = ABS(X) + ABS(ZZ)
      P = X/S
      Q = ZZ/S
      R = SQRT(P*P+Q*Q)
      P = P/R
      Q = Q/R
C     .......... ROW MODIFICATION ..........
      DO 520 J = NA,N
          ZZ = H(NA,J)
          H(NA,J) = Q*ZZ + P*H(EN,J)
          H(EN,J) = Q*H(EN,J) - P*ZZ
  520 CONTINUE
C     .......... COLUMN MODIFICATION ..........
      DO 540 I = 1,EN
          ZZ = H(I,NA)
          H(I,NA) = Q*ZZ + P*H(I,EN)
          H(I,EN) = Q*H(I,EN) - P*ZZ
  540 CONTINUE
C     .......... ACCUMULATE TRANSFORMATIONS ..........
      DO 560 I = LOW,IGH
          ZZ = Z(I,NA)
          Z(I,NA) = Q*ZZ + P*Z(I,EN)
          Z(I,EN) = Q*Z(I,EN) - P*ZZ
  560 CONTINUE
C
      GO TO 600
C     .......... COMPLEX PAIR ..........
  580 WR(NA) = X + P
      WR(EN) = X + P
      WI(NA) = ZZ
      WI(EN) = -ZZ
  600 EN = ENM2
      GO TO 60
  620 IERR = EN
  640 RETURN
      END
