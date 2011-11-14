*This is a modified version and the machine dependent information was
*passed using R .Machine
      SUBROUTINE ENVIRN(BETA,T,EPS,EMIN,RMIN)
C
C     ENVIRN returns the machine parameters given by:
C
C        BETA - INTEGER.
C               The base of the machine.
C
C        T    - INTEGER.
C               The number of ( BETA ) digits in the mantissa.
C
C        EPS  - REAL.
C               The smallest positive number such that
C
C                  fl( 1.0 - EPS ) .lt. 1.0,
C
C               where fl denotes the computed value.
C
C        EMIN - INTEGER.
C               The minimum exponent before (gradual) underflow occurs.
C
C        RMIN - REAL.
C               The smallest normalized number for the machine given by
C               BASE**( EMIN - 1 ), where BASE is the floating point
C               value of BETA.
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION EPS,RMIN
      INTEGER BETA,EMIN,T
C     .. Local Scalars ..
      DOUBLE PRECISION A,B,LEPS,LRMIN,ONE,RBASE,SMALL,TWO,ZERO
      INTEGER GNMIN,GPMIN,I,LBETA,LEMIN,LT,NGNMIN,NGPMIN,NOUT
      LOGICAL FIRST,IWARN,LRND
C     .. Save statement ..
      SAVE FIRST,IWARN,LBETA,LEMIN,LEPS,LRMIN,LT
C     .. Data statements ..
      DATA FIRST/.TRUE./,IWARN/.FALSE./
      DATA NOUT/6/
C
      IF (FIRST) THEN
          FIRST = .FALSE.
      ENDIF
C
C        LBETA, LT, LEPS, LEMIN and LRMIN are the local values of BETA,
C        T, EPS, EMIN and RMIN.
C
      CALL MACHINE(LEPS, LBETA, LT, LEMIN, LRMIN)
      BETA = LBETA
      T = LT
      EPS = LEPS
      EMIN = LEMIN
      RMIN = LRMIN
      RETURN
      END
