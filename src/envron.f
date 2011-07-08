*UPTODATE ENVRONTEXT
      SUBROUTINE ENVRON(BETA,T,RND)
      IMPLICIT NONE
C
C        MARK 2.2 RELEASE.  NAG COPYRIGHT 1987.
C
C     ENVRON returns the machine parameters given by:
C
C        BETA - INTEGER.
C               The base of the machine.
C
C        T    - INTEGER.
C               The number of ( BETA ) digits in the mantissa.
C
C        RND  - LOGICAL.
C               Whether proper rounding ( RND = .TRUE. ) or chopping
C               ( RND = .FALSE. ) occurs in addition. This may not be a
C               reliable guide to the way in which the machine perfoms
C               its arithmetic.
C
C     The routine is based on the routine of the same name by Malcolm
C     and incorporates suggestions by Gentleman and Marovich. See
C
C        Malcolm M. A. (1972) Algorithms to reveal properties of
C           floating-point arithmetic. Comms. of the ACM, 15, 949-951.
C
C        Gentleman W. M. and Marovich S. B. (1974) More on algorithms
C           that reveal properties of floating point arithmetic units.
C           Comms. of the ACM, 17, 276-277.
C
C
C  Nag Fortran 77 O( 1 ) basic linear algebra routine.
C
C  -- Written on 26-November-1984.
C     Sven Hammarling and Mick Pont, Nag Central Office.
C
C
C     .. Scalar Arguments ..
      INTEGER BETA,T
      LOGICAL RND
C     .. Local Scalars ..
      DOUBLE PRECISION A,B,C,C1,C2,F,ONE,QTR,SAVA
      INTEGER LBETA,LT
      LOGICAL FIRST,LRND
C     .. External Functions ..
      DOUBLE PRECISION STORE
      EXTERNAL STORE
C     .. Save statement ..
      SAVE FIRST,LBETA,LRND,LT
C     .. Data statements ..
      DATA FIRST/.TRUE./
C     .. Executable Statements ..
C
      IF (FIRST) THEN
          FIRST = .FALSE.
          ONE = 1
C
C        LBETA, LT and LRND are the local values of BETA, T and RND.
C
C        Throughout this routine we use the function STORE to ensure
C        that relevant values are stored and not held in registers, or
C        are not affected by optimizers.
C
C        Compute  a = 2.0**m  with the smallest positive integer m such
C        that
C
C           fl( a + 1.0 ) = a.
C
          A = 1
          C = 1
C
C+       WHILE( C.EQ.ONE )LOOP
   20     IF (C.EQ.ONE) THEN
              A = 2*A
              C = STORE(A,ONE)
              C = STORE(C,-A)
              GO TO 20
          END IF
         call dblepr("20", 2, C, 1)
C+       END WHILE
C
C        Now compute  b = 2.0**m  with the smallest positive integer m
C        such that
C
C           fl( a + b ) .gt. a.
C
          B = 1
          C = STORE(A,B)
C
C+       WHILE( C.EQ.A )LOOP
   40     IF (C.EQ.A) THEN
              B = 2*B
              C = STORE(A,B)
              GO TO 40
          END IF
         call dblepr("40", 2, C, 1)
C+       END WHILE
C
C        Now compute the base. a and b are neighbouring floating point
C        numbers in the interval ( beta**t, beta**( t + 1 ) ) and so
C        their difference is beta. Adding 0.25 to c is to ensure that it
C        is truncated to beta and not ( beta - 1 ).
C
C
          QTR = ONE/4
          C = STORE(C,-A)
          LBETA = C + QTR
C
C        Now determine whether rounding or chopping occurs, by adding
C        a bit less than beta/2 and a bit more than beta/2 to a.
C
          B = LBETA
          F = STORE(B/2,-B/100)
          C1 = STORE(F,A)
          F = STORE(B/2,B/100)
          C2 = STORE(F,A)
          SAVA = A
C
C        Now find the mantissa, t. It should be the integer part of
C        log to the base beta of a, however it is safer to determine t
C        by powering. So we find t as the smallest positive integer
C        for which
C
C           fl( beta**t + 1.0 ) = 1.0.
C
          LT = 0
          A = 1
          C = 1
C
C+       WHILE( C.EQ.ONE )LOOP
   60     IF (C.EQ.ONE) THEN
              LT = LT + 1
         call dblepr("LBETA", 5, LBETA, 1)
         call dblepr("60 A", 4, A, 1)
              A = A*LBETA
              C = STORE(A,ONE)
         call dblepr("ONE", 3, ONE, 1)
         call dblepr("60 A", 4, A, 1)
         call dblepr("60C1", 4, C, 1)
              C = STORE(C,-A)
         call dblepr("60C2", 4, C, 1)
              GO TO 60
          END IF
C+       END WHILE
C
          LRND = C1 .EQ. SAVA .AND. C2 .NE. SAVA
      END IF
C
      BETA = LBETA
      T = LT
      RND = LRND
      RETURN
C
C     End of ENVRON.
C
      END
