*UPTODATE LYBSCTEXT
      SUBROUTINE LYBSC(N,A,NA,C,NC,X,NX,U,WRK,WIK,MODE,IERR)
      IMPLICIT NONE
C
C        MARK 2.2 RELEASE.  NAG COPYRIGHT 1987.
C
C PURPOSE
C
C        Solves for X the continuous-time Lyapunov equation
C
C               TRANS(A)*X + X*A = C
C
C        where A is an n by n matrix and C is an n by n symmetric
C        matrix.
C
C METHOD
C
C        This routine is a modification of the subroutine  ATXXAC,
C        written and discussed by  R.H.Bartels & G.W.Stewart
C        (see reference)
C
C REFERENCE
C
C         BARTELS, R.H. & STEWART, G.W
C            "Solution of the matrix equation  A'X + XB = C  ",
C            Commun. A.C.M., Vol 15, 1972, pp. 820-826 .
C
C         HAMMARLING, S.J.
C         "Numerical solution of the stable, non-negative definite
C          Lyapunov equation."
C          IMA Journal of Numerical Analysis, Vol. 2, No. 3, July 1982
C          pp 303-325
C
C AUXILIARY ROUTINES
C
C       ORTHES,ORTRAN,HQRORT (EISPACK)
C       SGEFA,SGESL          (LINPACK)
C       LYCSR,SDATA          (SLICE)
C
C ARGUMENTS IN
C
C
C       N        INTEGER
C                -The order of matrix A.
C                 N .GE. 1
C
C       A        REAL(NA,N)
C                -The leading N by N part of this array must contain
C                 the matrix A of the equation.
C                 On exit the N by N part of this array is overwritten
C                 but see COMMENTS below.
C
C       NA       INTEGER
C                -The first dimension of arrays A and U as declared
C                 in the calling program.
C                 NA .GE. N
C
C       C        REAL(NC,N)
C                -The leading N by N part of this array must contain
C                 the matrix C of the equation.
C
C       NC       INTEGER
C                -The first dimension of array C as declared in the
C                 calling program
C                 NC .GE. N
C
C       NX       INTEGER
C                -The first dimension of array X as declared in the
C                 calling program
C                 NX .GE. N
C
C       MODE     INTEGER
C                -Indicates the form of matrix A on input.
C                 MODE = 0  if  A  has not already been reduced to
C                                upper triangular form
C                 MODE = 1  if  A  has been reduced to triangular form
C                                (e.g) by a previous call to LYBSC
C
C ARGUMENTS OUT
C
C       A        REAL(NA,N)
C                -The leading N by N part of this array contains the
C                 upper triangular form of A.
C
C       X        REAL(NX,N)
C                -The leading N by N part of this array contains the
C                 solution matrix X.
C
C       U        REAL(NA,N)
C                -The leading N by N part of this array contains the
C                 orthogonal matrix which was used to reduce A to upper
C                 triangular form.
C
C       IERR     INTEGER
C                -Error indicator
C
C                IERR = 0     Successful return
C
C                IERR = 1     A  has a degenerate pair of eigenvalues
C
C                IERR = 2     A  cannot be reduced to triangular form
C
C                IERR = 3     Either N .LT. 1  or N .GT. NA  or
C                                    N .GT. NC or N .GT. NX or
C                                    0 .GT. MODE .GT. 1
C
C WORKING SPACE
C
C        WRK     REAL(N)
C
C        WIK     REAL(N)
C
C ORIGINATOR
C
C                Control Systems Research Group, Dept EECS, Kingston
C                Polytechnic, Penrhyn Rd.,Kingston-upon-Thames, England.
C
C VERSION        LYBSC 2.2 ( 20TH JANUARY 1987 )
C
C LIBRARY INDEX
C                SLICE 2.2 ( 20TH JANUARY 1987 )
C                Section 10. Synthesis of Controllers for
C                                        State Space Models.
C                Subsection 10.3. Lyapunov Equation Solvers.
C
C COMMENTS   The input coefficient matrix  A is overwritten by the
C            triangularized form. If required the matrix A can be
C            reformed from the matrix product UAU' (where U and A
C            are the matrices output in arrays U and A respectively)
C
C            If there is a shortage of storage space, the matrices
C            C  and  X  can share the same locations, but this will
C            of course, result in the loss of the matrix  C.
C
C******************************************************************
C
C     .. Scalar Arguments ..
      INTEGER IERR,MODE,N,NA,NC,NX
C     .. Array Arguments ..
      DOUBLE PRECISION A(NA,N),C(NC,N),U(NA,N),WIK(N),WRK(N),X(NX,N)
C*PT*WARNING* Already double-precision
C     .. Local Scalars ..
      DOUBLE PRECISION DPREC
      INTEGER I,IGH,J,K,LOW
C     .. External Subroutines ..
      EXTERNAL HQRORT,LYCSR,ORTHES,ORTRAN
C     .. Executable Statements ..
C
      IERR = 0
      IF (N.LT.1) IERR = 3
      IF (0.GT.MODE .OR. MODE.GT.1) IERR = 3
      IF (NA.LT.N .OR. NC.LT.N .OR. NX.LT.N) IERR = 3
      IF (IERR.EQ.3) RETURN
      IF (MODE.EQ.1) GO TO 40
      LOW = 1
      IGH = N
      CALL ORTHES(NA,N,LOW,IGH,A,WRK)
      CALL ORTRAN(NA,N,LOW,IGH,A,WRK,U)
      CALL HQRORT(NA,N,LOW,IGH,A,WRK,WIK,U,IERR)
C
C       SET THE ELEMENTS ON THE FIRST SUB-DIAGONAL OF THE BLOCK
C       UPPER TRIANGULAR MATRIX TO ZERO
C
      K = 1
   20 IF (K.GE.N) GO TO 40
      IF (WIK(K).EQ.0.0D0) THEN
          A(K+1,K) = 0.0D0
          K = K + 1
      ELSE
          K = K + 2
          IF (K.LE.N) A(K,K-1) = 0.D0
      END IF
      GO TO 20
   40 CONTINUE
      IF (IERR.NE.0) GO TO 520
C
      DO 80 I = 1,N
          DO 60 J = 1,N
              X(I,J) = C(I,J)
   60     CONTINUE
          X(I,I) = X(I,I)*0.5D0
   80 CONTINUE
C
      DO 160 I = 1,N
          DO 120 J = 1,N
C*PT*WARNING* Constant already double-precision
              DPREC = 0.0D0
              DO 100 K = I,N
                  DPREC = DPREC + X(I,K)*U(K,J)
  100         CONTINUE
              WRK(J) = DPREC
  120     CONTINUE
          DO 140 J = 1,N
              X(I,J) = WRK(J)
  140     CONTINUE
  160 CONTINUE
      DO 240 J = 1,N
          DO 200 I = 1,N
C*PT*WARNING* Constant already double-precision
              DPREC = 0.0D0
              DO 180 K = 1,N
                  DPREC = DPREC + U(K,I)*X(K,J)
  180         CONTINUE
              WRK(I) = DPREC
  200     CONTINUE
          DO 220 I = 1,N
              X(I,J) = WRK(I)
  220     CONTINUE
  240 CONTINUE
C
      DO 280 I = 1,N
          DO 260 J = I,N
              X(I,J) = X(I,J) + X(J,I)
              X(J,I) = X(I,J)
  260     CONTINUE
  280 CONTINUE
C
      CALL LYCSR(N,A,NA,X,NX,IERR)
      IF (IERR.NE.0) RETURN
C
      DO 300 I = 1,N
          X(I,I) = X(I,I)*0.5D0
  300 CONTINUE
C
      DO 380 I = 1,N
          DO 340 J = 1,N
C*PT*WARNING* Constant already double-precision
              DPREC = 0.0D0
              DO 320 K = I,N
                  DPREC = DPREC + X(I,K)*U(J,K)
  320         CONTINUE
              WRK(J) = DPREC
  340     CONTINUE
          DO 360 J = 1,N
              X(I,J) = WRK(J)
  360     CONTINUE
  380 CONTINUE
C
      DO 460 J = 1,N
          DO 420 I = 1,N
C*PT*WARNING* Constant already double-precision
              DPREC = 0.0D0
              DO 400 K = 1,N
                  DPREC = DPREC + U(I,K)*X(K,J)
  400         CONTINUE
              WRK(I) = DPREC
  420     CONTINUE
          DO 440 I = 1,N
              X(I,J) = WRK(I)
  440     CONTINUE
  460 CONTINUE
C
      DO 5000 I = 1,N
          DO 480 J = I,N
              X(I,J) = X(I,J) + X(J,I)
              X(J,I) = X(I,J)
  480     CONTINUE
 5000 CONTINUE
C
      GO TO 540
  520 IERR = 2
  540 RETURN
      END
