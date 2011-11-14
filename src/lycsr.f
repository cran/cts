*UPTODATE LYCSRTEXT
      SUBROUTINE LYCSR(N,A,NA,C,NC,IERR)
C
C        MARK 2.2 RELEASE.  NAG COPYRIGHT 1987.
C
C  WARNING  -This routine is intended to be called only from
C            SLICE   routine  LYBSC .
C
C     .. Scalar Arguments ..
      IMPLICIT NONE
      INTEGER IERR,N,NA,NC
C     .. Array Arguments ..
      DOUBLE PRECISION A(NA,N),C(NC,N)
C*PT*WARNING* Already double-precision
C     .. Local Scalars ..
      DOUBLE PRECISION DPREC
      INTEGER DK,DL,I,IA,INFO,J,JOB,K,KK,KM1,L,LDIM,LDL,LL,NSYS
C     .. Local Arrays ..
      DOUBLE PRECISION P(4),T(4,4)
      INTEGER IPVT(4)
C     .. External Subroutines ..
C      EXTERNAL SGEFA,SGESL
C      EXTERNAL DGETRF, DGETRS
C     .. Executable Statements ..
C
C     This routine solves the continuous Lyapunov equation where
C     the matrix  A  has been transformed to quasi-triangular form.
C

      IERR = 0
      LDIM = 4
      JOB = 0
      L = 1
   20 DL = 1
      IF (L.EQ.N) GO TO 40
      IF (A(L+1,L).NE.0.0D0) DL = 2
   40 LL = L + DL - 1
      K = L
   60 KM1 = K - 1
      DK = 1
      IF (K.EQ.N) GO TO 80
      IF (A(K+1,K).NE.0.0D0) DK = 2
   80 KK = K + DK - 1
      IF (K.EQ.L) GO TO 160
C
      DO 140 I = K,KK
C
          DO 120 J = L,LL
C*PT*WARNING* Constant already double-precision
              DPREC = 0.0D0
              DO 100 IA = L,KM1
                  DPREC = DPREC + A(IA,I)*C(IA,J)
  100         CONTINUE
              C(I,J) = C(I,J) - DPREC
  120     CONTINUE
  140 CONTINUE
C  
  160 IF (DL.EQ.2) GO TO 200
      IF (DK.EQ.2) GO TO 180
      T(1,1) = A(K,K) + A(L,L)
      IF (T(1,1).EQ.0.0D0) GO TO 360
      C(K,L) = C(K,L)/T(1,1)
      GO TO 260
  180 T(1,1) = A(K,K) + A(L,L)
      T(1,2) = A(KK,K)
      T(2,1) = A(K,KK)
      T(2,2) = A(KK,KK) + A(L,L)
      P(1) = C(K,L)
      P(2) = C(KK,L)
      NSYS = 2
      CALL DGETRF(NSYS, NSYS, T, LDIM,IPVT,INFO)
C      CALL SGEFA(T,LDIM,NSYS,IPVT,INFO)
      IF (INFO.NE.0) GO TO 360
C      CALL SGESL(T,LDIM,NSYS,IPVT,P,JOB)
      CALL DGETRS('N',NSYS, 1, T,LDIM, IPVT,P, LDIM, INFO)

      C(K,L) = P(1)
      C(KK,L) = P(2)
      GO TO 260
  200 IF (DK.EQ.2) GO TO 220
      T(1,1) = A(K,K) + A(L,L)
      T(1,2) = A(LL,L)
      T(2,1) = A(L,LL)
      T(2,2) = A(K,K) + A(LL,LL)
      P(1) = C(K,L)
      P(2) = C(K,LL)
      NSYS = 2
      CALL DGETRF(NSYS, NSYS, T, LDIM,IPVT,INFO)
C      CALL SGEFA(T,LDIM,NSYS,IPVT,INFO)
      IF (INFO.NE.0) GO TO 360
      CALL DGETRS('N',NSYS, 1, T,LDIM, IPVT,P, LDIM, INFO)
C      CALL SGESL(T,LDIM,NSYS,IPVT,P,JOB)
      C(K,L) = P(1)
      C(K,LL) = P(2)
      GO TO 260
  220 IF (K.NE.L) GO TO 240
      T(1,1) = A(L,L)
      T(1,2) = A(LL,L)
      T(1,3) = 0.0D0
      T(2,1) = A(L,LL)
      T(2,2) = A(L,L) + A(LL,LL)
      T(2,3) = T(1,2)
      T(3,1) = 0.0D0
      T(3,2) = T(2,1)
      T(3,3) = A(LL,LL)
      P(1) = C(L,L)*0.5D0
      P(2) = C(LL,L)
      P(3) = C(LL,LL)*0.5D0
      NSYS = 3
      CALL DGETRF(NSYS, NSYS, T, LDIM,IPVT,INFO)
C      CALL SGEFA(T,LDIM,NSYS,IPVT,INFO)
      IF (INFO.NE.0) GO TO 360
C      CALL SGESL(T,LDIM,NSYS,IPVT,P,JOB)
      CALL DGETRS('N',NSYS, 1, T,LDIM, IPVT,P, LDIM, INFO)
      C(L,L) = P(1)
      C(LL,L) = P(2)
      C(L,LL) = P(2)
      C(LL,LL) = P(3)
      GO TO 260
  240 T(1,1) = A(K,K) + A(L,L)
      T(1,2) = A(KK,K)
      T(1,3) = A(LL,L)
      T(1,4) = 0.0D0
      T(2,1) = A(K,KK)
      T(2,2) = A(KK,KK) + A(L,L)
      T(2,3) = 0.0D0
      T(2,4) = T(1,3)
      T(3,1) = A(L,LL)
C      call intpr('L', 1, L, 1)
C      call intpr('LL', 2, LL, 1)
C      call dblepr('T(3,1)', 6, T(3,1), 1)
      T(3,2) = 0.0D0
      T(3,3) = A(K,K) + A(LL,LL)
      T(3,4) = T(1,2)
      T(4,1) = 0.0D0
      T(4,2) = T(3,1)
C      call dblepr('T(4,2)', 6, T(4,2), 1)
      T(4,3) = T(2,1)
      T(4,4) = A(KK,KK) + A(LL,LL)
      P(1) = C(K,L)
      P(2) = C(KK,L)
      P(3) = C(K,LL)
      P(4) = C(KK,LL)
      NSYS = 4
      
      CALL DGETRF(NSYS, NSYS, T, LDIM,IPVT,INFO)
C      CALL SGEFA(T,LDIM,NSYS,IPVT,INFO)
      IF (INFO.NE.0) GO TO 360
C      CALL SGESL(T,LDIM,NSYS,IPVT,P,JOB)
      CALL DGETRS('N',NSYS, 1, T,LDIM, IPVT,P, LDIM, INFO)
      C(K,L) = P(1)
      C(KK,L) = P(2)
      C(K,LL) = P(3)
      C(KK,LL) = P(4)
  260 K = K + DK
      IF (K.LE.N) GO TO 60
      LDL = L + DL
      IF (LDL.GT.N) RETURN
C
      DO 340 J = LDL,N
C
          DO 280 I = L,LL
              C(I,J) = C(J,I)
  280     CONTINUE
C
          DO 320 I = J,N
C*PT*WARNING* Constant already double-precision
              DPREC = 0.0D0
              DO 300 K = L,LL
                  DPREC = DPREC + C(I,K)*A(K,J)
                  DPREC = DPREC + A(K,I)*C(K,J)
  300         CONTINUE
              C(I,J) = C(I,J) - DPREC
              C(J,I) = C(I,J)
  320     CONTINUE
  340 CONTINUE
C
      L = LDL
      GO TO 20
C
  360 IERR = 1
      RETURN
      END
