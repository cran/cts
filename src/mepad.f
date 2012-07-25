*UPTODATE MEPADTEXT
      SUBROUTINE MEPAD(N,NDIAG,DELTA,A,NA,E,NE,MDIG,IDIG,IWRK,WRKA1,
     +                 WRKA2,WRKV1,WRKV2,IERR)
C
C        MARK 2.2 RELEASE.  NAG COPYRIGHT 1987.
C
C PURPOSE
C
C        To compute Exp( A * delta ) , where A is a real N by N
C        matrix and delta is a scalar value. The routine also returns
C        the minimal number of accurate digits in the one norm of
C        Exp( A * delta ) at the 95 per cent confidence level.
C
C METHOD
C
C        The exponential of the matrix is evaluated from a
C        diagonal Pade approximant. This routine is a modification
C        of the subroutine PADE, written and discussed by R.C.Ward
C        (see reference).
C
C REFERENCE
C
C       WARD, R.C.
C       "Numerical computation of the matrix exponential
C       with accuracy estimate",
C       SIAM J. NUMER. ANAL. ,VOL. 14,1977, pp. 600-610
C
C AUXILIARY ROUTINES
C
C       BALANC (EISPACK)
C       SGEFA,SGESL (LINPACK)
C       SDATA,BALINV  (SLICE)
C
C ARGUMENTS IN
C
C        N       INTEGER
C                -The order of the matrix  A.
C                 N .GE. 1
C
C        NDIAG   INTEGER
C                -The specified order of the diagonal Pade approximant.
C                 In the absence of further information NDIAG should
C                 be set to 9
C
C        DELTA   REAL
C                -The scalar value delta of the problem.
C
C        A       REAL(NA,N)
C                -The leading N by N part of this array must contain
C                 the matrix A of the problem.
C
C        NA      INTEGER
C                -The first dimension of array A as declared in the
C                 calling program
C                 NA .GE. N
C
C        NE      INTEGER
C                -The first dimension of array E as declared in the
C                 calling program
C                 NE .GE. N
C
C ARGUMENTS OUT
C
C        E       REAL(NE,N)
C                -The leading N by N part of this array will contain
C                 the solution matrix  exp( A*delta )
C
C        MDIG    INTEGER
C                -The minimal number of accurate digits
C                in the norm of  E .
C
C        IDIG    INTEGER
C                -The number of accurate digits in the norm
C                of  E  at  95% confidence level.
C
C        IERR    INTEGER
C                -Error indicator
C                IERR = 0        Successful return
C
C                IERR = 1        The norm of A is too large to obtain
C                                an accurate result
C
C                IERR = 2        If MDIG = 0 and IDIG .GT.  0, warning
C                                for possible inaccuracy
C
C                IERR = 3        If MDIG = 0 and IDIG = 0, severe
C                                warning for inaccuracy
C
C                IERR = 4        Either NA .LT. N or NE .LT. N or
C                                       N .LT. 1
C
C WORKING SPACE
C
C        IWRK      INTEGER(N)
C
C        WRKA1     REAL(N,N)
C
C        WRKA2     REAL(N,NDIAG)
C
C        WRKV1     REAL(N)
C
C        WRKV2     REAL(NDIAG)
C
C ORIGINATOR
C
C                Control Systems Research Group, School of EECS,
C                Kingston Polytechnic, Kingston-upon-Thames, England.
C
C VERSION        MEPAD 2.2 ( 20TH JANUARY 1987 )
C
C LIBRARY INDEX
C                SLICE 2.2 ( 20TH JANUARY 1987 )
C                Section  2.  Mathematical Routines.
C                Subsection  2.1  Matrix Manipulation.
C
C COMMENTS
C
C                none
C
C*******************************************************************
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION DELTA
      INTEGER IDIG,IERR,MDIG,N,NA,NDIAG,NE
C     .. Array Arguments ..
      DOUBLE PRECISION A(NA,N),E(NE,N),WRKA1(N,N),WRKA2(N,NDIAG),
     +                 WRKV1(N),WRKV2(NDIAG)
      INTEGER IWRK(N)
C     .. Scalars in Common ..
      DOUBLE PRECISION BASE,D1,D2,EPS
C     .. Local Scalars ..
      DOUBLE PRECISION ANORM,AVGEV,BD,BIG,EABS,EAVGEV,EMNORM,ENORM,
     +                 FACTOR,FN,GN,P,RERL,RERR,S,SD2,SIZE,SIZEJ,SMALL,
     +                 SS,SUM2D,TEMP,TR,U,VAR,VAREPS,XN
      INTEGER I,IGH,IM1,J,JOB,K,KEO,KP1,L,LOW,M,MPOWER,NDAGM1,NDAGM2,
     +        NDEC,NDECM1
C     .. External Subroutines ..
C      EXTERNAL BALANC,BALINV,SDATA,SGEFA,SGESL
      EXTERNAL BALANC,BALINV,SDATA,DGETRF,DGETRS
C     .. Intrinsic Functions ..
      INTRINSIC ABS,EXP,INT,LOG,LOG10,MOD,DBLE,SQRT
C     .. Common blocks ..
      COMMON /MDEPS/BASE,D1,D2,EPS
C     .. Executable Statements ..
C
C        COMMON block MDEPS  is shared with  SDATA , which provides
C        values for the machine dependent parameters  EPS, which
C        specifies the relative precision of floating point arithmetic,
C        and  BASE  which specifies the number base used for machine
C        representation.
C
      CALL SDATA
      IERR = 0
      IF (N.GT.NA .OR. N.GT.NE) IERR = 4
      IF (N.LT.1) IERR = 4
      IF (IERR.NE.0) RETURN
      JOB = 0
      NDEC = INT(LOG10(1.0D0/EPS)+1.0D0)
      NDECM1 = NDEC - 1
      IF (N.EQ.1) GO TO 800
      WRKV2(1) = 0.5D0
      DO 20 I = 2,NDIAG
          IM1 = I - 1
          WRKV2(I) = WRKV2(IM1)*DBLE(NDIAG-IM1)/DBLE(I* (2*NDIAG-IM1))
   20 CONTINUE
      VAREPS = ((BASE**2-1.0D0)/ (24.0D0*LOG(BASE)))*EPS**2
      XN = DBLE(N)
      TR = 0.0D0
      DO 60 I = 1,N
          DO 40 J = 1,N
              A(I,J) = A(I,J)*DELTA
   40     CONTINUE
          TR = TR + A(I,I)
   60 CONTINUE
      AVGEV = TR/XN
      DO 80 I = 1,N
          A(I,I) = A(I,I) - AVGEV
   80 CONTINUE
      CALL BALANC(NA,N,A,LOW,IGH,WRKV1)
      ANORM = 0.0D0
      DO 120 J = 1,N
          SS = 0.0D0
          DO 100 I = 1,N
              SS = SS + ABS(A(I,J))
  100     CONTINUE
          IF (SS.GT.ANORM) ANORM = SS
  120 CONTINUE
      M = 0
      IF (ANORM.LE.1.0D0) GO TO 220
      MPOWER = INT(LOG(1.0D0/EPS)/LOG(2.0D0))
      FACTOR = 2.0D0
      DO 140 I = 1,MPOWER
          M = I
          IF (ANORM.LE.FACTOR) GO TO 160
          FACTOR = FACTOR*2.0D0
  140 CONTINUE
      IERR = 1
      GO TO 820
  160 DO 200 I = 1,N
          DO 180 J = 1,N
              A(I,J) = A(I,J)/FACTOR
  180     CONTINUE
  200 CONTINUE
  220 NDAGM1 = NDIAG - 1
      NDAGM2 = NDAGM1 - 1
      DO 420 J = 1,N
          DO 260 I = 1,N
              S = 0.0D0
              DO 240 L = 1,N
                  S = S + A(I,L)*A(L,J)
  240         CONTINUE
              WRKA2(I,1) = S
  260     CONTINUE
          DO 320 K = 1,NDAGM2
              KP1 = K + 1
              DO 300 I = 1,N
                  S = 0.0D0
                  DO 280 L = 1,N
                      S = S + A(I,L)*WRKA2(L,K)
  280             CONTINUE
                  WRKA2(I,KP1) = S
  300         CONTINUE
  320     CONTINUE
          DO 400 I = 1,N
              S = 0.0D0
              U = 0.0D0
              DO 360 L = 1,NDAGM1
                  K = NDIAG - L
                  KP1 = K + 1
                  P = WRKV2(KP1)*WRKA2(I,K)
                  S = S + P
                  KEO = MOD(KP1,2)
                  IF (KEO.EQ.0) GO TO 340
                  U = U - P
                  GO TO 360
  340             U = U + P
  360         CONTINUE
              P = WRKV2(1)*A(I,J)
              S = S + P
              U = U - P
              IF (I.NE.J) GO TO 380
              S = S + 1.0D0
              U = U + 1.0D0
  380         E(I,J) = S
              WRKA1(I,J) = U
  400     CONTINUE
  420 CONTINUE
C      CALL SGEFA(WRKA1,N,N,IWRK,IERR)
      CALL DGETRF(N, N, WRKA1,N, IWRK,IERR)
      DO 440 J = 1,N
C          CALL SGESL(WRKA1,N,N,IWRK,E(1,J),JOB)
      CALL DGETRS('N',N, 1, WRKA1, N, IWRK, E(1,J), N, JOB)
C      CALL SGESL(T,LDIM,NSYS,IPVT,P,JOB)

  440 CONTINUE
      ENORM = 0.0D0
      DO 480 J = 1,N
          SS = 0.0D0
          DO 460 I = 1,N
              SS = SS + ABS(E(I,J))
  460     CONTINUE
          IF (SS.GT.ENORM) ENORM = SS
  480 CONTINUE
      EABS = (19.0D0*XN+47.0D0)*EPS*ENORM
      VAR = (12.0D0*XN)*VAREPS
      IF (M.EQ.0) GO TO 660
      VAR = XN*VAREPS
      FN = (4.0D0*XN)/ ((XN+2.0D0)* (XN+1.0D0))
      GN = (2.0D0* (XN**2)+10.0D0*XN-4.0D0)/
     +     (((XN+2.0D0)**2)* ((XN+1.0D0)**2))
      DO 600 K = 1,M
          DO 540 I = 1,N
              DO 520 J = 1,N
                  S = 0.0D0
                  DO 5000 L = 1,N
                      S = S + E(I,L)*E(L,J)
 5000             CONTINUE
                  WRKA1(I,J) = S
  520         CONTINUE
  540     CONTINUE
          TEMP = ENORM**2
          EABS = 2.0D0*EABS*ENORM + XN*EPS*TEMP + EABS**2
          VAR = (FN*VAR+GN*TEMP*VAREPS)*TEMP
          ENORM = 0.0D0
          DO 580 J = 1,N
              SS = 0.0D0
              DO 560 I = 1,N
                  E(I,J) = WRKA1(I,J)
                  SS = SS + ABS(E(I,J))
  560         CONTINUE
              IF (SS.GT.ENORM) ENORM = SS
  580     CONTINUE
  600 CONTINUE
      DO 640 I = 1,N
          DO 620 J = 1,N
              A(I,J) = A(I,J)*FACTOR
  620     CONTINUE
  640 CONTINUE
  660 CALL BALINV(NA,N,LOW,IGH,WRKV1,A)
      DO 680 I = 1,N
          A(I,I) = A(I,I) + AVGEV
  680 CONTINUE
      BIG = 1.0D0
      SMALL = 1.0D0
      SUM2D = 0.0D0
      IF (LOW.EQ.IGH) GO TO 740
      SUM2D = -1.0D0
      DO 720 I = LOW,IGH
          U = WRKV1(I)
          SUM2D = SUM2D + U**2
          IF (BIG.GE.U) GO TO 700
          BIG = U
  700     IF (SMALL.LE.U) GO TO 720
          SMALL = U
  720 CONTINUE
  740 SUM2D = SUM2D + XN + DBLE(LOW-IGH)
      SD2 = 2.0D0*SQRT(SUM2D*VAR+ (8.0D0*DBLE(IGH-
     +      LOW)* (ENORM**2)*SUM2D*VAREPS)/ (XN**2))
      CALL BALINV(NE,N,LOW,IGH,WRKV1,E)
      EAVGEV = EXP(AVGEV)
      EMNORM = 0.0D0
      SIZE = 0.0D0
      DO 780 J = 1,N
          SS = 0.0D0
          DO 760 I = 1,N
              SS = SS + ABS(E(I,J))
              E(I,J) = E(I,J)*EAVGEV
  760     CONTINUE
          IF (SS.GT.EMNORM) EMNORM = SS
          IF (LOW.EQ.IGH) GO TO 780
          BD = WRKV1(J)
          IF (J.LT.LOW .OR. IGH.LT.J) BD = 1.0D0
          SIZEJ = SS + SD2/BD
          IF (SIZEJ.GT.SIZE) SIZE = SIZEJ
  780 CONTINUE
      RERR = (BIG*EABS)/ (SMALL*EMNORM*EPS)
      IF (LOW.EQ.IGH) SIZE = EMNORM + SD2
      RERL = (SIZE-EMNORM)/ (EMNORM*EPS)
      MDIG = NDEC - INT(LOG10(RERR)+0.5D0)
      IDIG = NDEC - INT(LOG10(RERL)+0.5D0)
      IF (IDIG.GT.NDECM1) IDIG = NDECM1
      IF (MDIG.GT.0) GO TO 820
      MDIG = 0
      IERR = 2
      IF (IDIG.GT.0) GO TO 820
      IDIG = 0
      IERR = 3
      GO TO 820
  800 WRKA1(1,1) = A(1,1)*DELTA
      E(1,1) = EXP(WRKA1(1,1))
      MDIG = NDECM1
      IDIG = NDECM1
  820 CONTINUE
      RETURN
      END
