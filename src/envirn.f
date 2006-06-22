*UPTODATE ENVIRNTEXT
      SUBROUTINE ENVIRN(BETA,T,EPS,EMIN,RMIN)
C
C        MARK 2.2 RELEASE.  NAG COPYRIGHT 1987.
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
C
C     The computation of EPS, EMIN and RMIN is based on a routine,
C     PARANOIA by W. Kahan of the University of California at Berkeley.
C
C
C  Nag Fortran 77 O( 1 ) basic linear algebra routine.
C
C  -- Written on 2-June-1987.
C     Sven Hammarling, Mick Pont and Janet Welding, Nag Central Office.
C
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION EPS,RMIN
      INTEGER BETA,EMIN,T
C     .. Local Scalars ..
      DOUBLE PRECISION A,B,LEPS,LRMIN,ONE,RBASE,SMALL,TWO,ZERO
      INTEGER GNMIN,GPMIN,I,LBETA,LEMIN,LT,NGNMIN,NGPMIN,NOUT
      LOGICAL FIRST,IWARN,LRND
C     .. External Functions ..
      DOUBLE PRECISION STORE
      EXTERNAL STORE
C     .. External Subroutines ..
      EXTERNAL ENVRON,GETMIN
C     .. Intrinsic Functions ..
      INTRINSIC ABS,MAX,MIN
C     .. Save statement ..
      SAVE FIRST,IWARN,LBETA,LEMIN,LEPS,LRMIN,LT
C     .. Data statements ..
      DATA FIRST/.TRUE./,IWARN/.FALSE./
      DATA NOUT/6/
C     .. Executable Statements ..
C
      IF (FIRST) THEN
          FIRST = .FALSE.
          ZERO = 0
          ONE = 1
          TWO = 2
C
C        LBETA, LT, LEPS, LEMIN and LRMIN are the local values of BETA,
C        T, EPS, EMIN and RMIN.
C
C        Throughout this routine we use the function STORE to ensure
C        that relevant values are stored and not held in registers, or
C        are not affected by optimizers.
C
C        ENVRON returns the parameters LBETA and LT. ( LRND is not used
C        here. )
C
          CALL ENVRON(LBETA,LT,LRND)
C
C        Start to find EPS.
C
          B = LBETA
          IF (LRND) THEN
              LEPS = (B** (1-LT))/TWO
          ELSE
              LEPS = B** (1-LT)
          END IF
C
C        Computation of EPS complete. Now find EMIN.
C        Let a = + or - 1, and + or - (1 + base**(-3)).
C        Keep dividing a by BETA until (gradual) underflow occurs.
C        This is detected when we cannot recover the previous a.
C
          RBASE = ONE/LBETA
          SMALL = ONE
          DO 20 I = 1,3
              SMALL = STORE(SMALL*RBASE,ZERO)
   20     CONTINUE
          A = STORE(ONE,SMALL)
          CALL GETMIN(NGPMIN,ONE,LBETA)
          CALL GETMIN(NGNMIN,-ONE,LBETA)
          CALL GETMIN(GPMIN,A,LBETA)
          CALL GETMIN(GNMIN,-A,LBETA)
C
          IF (NGPMIN.EQ.NGNMIN .AND. GPMIN.EQ.GNMIN) THEN
              IF (NGPMIN.EQ.GPMIN) THEN
                  LEMIN = NGPMIN
C            ( Non twos-complement machines, no gradual underflow;
C              eg VAX )
              ELSE IF (GPMIN-NGPMIN.EQ.3) THEN
                  LEMIN = NGPMIN - 1 + LT
C            ( Non twos-complement machines, with gradual underflow;
C              eg IEEE standard followers )
              ELSE
                  LEMIN = MIN(NGPMIN,GPMIN)
C            ( A guess; no known machine )
                  IWARN = .TRUE.
              END IF
          ELSE IF (NGPMIN.EQ.GPMIN .AND. NGNMIN.EQ.GNMIN) THEN
              IF (ABS(NGPMIN-NGNMIN).EQ.1) THEN
                  LEMIN = MAX(NGPMIN,NGNMIN)
C            ( Twos-complement machines, no gradual underflow;
C              eg Cyber 205 )
              ELSE
                  LEMIN = MIN(NGPMIN,NGNMIN)
C            ( A guess; no known machine )
                  IWARN = .TRUE.
              END IF
          ELSE IF (ABS(NGPMIN-NGNMIN).EQ.1 .AND. GPMIN.EQ.GNMIN) THEN
              IF (GPMIN-MIN(NGPMIN,NGNMIN).EQ.3) THEN
                  LEMIN = MAX(NGPMIN,NGNMIN) - 1 + LT
C            ( Twos-complement machines with gradual underflow;
C              no known machine )
              ELSE
                  LEMIN = MAX(NGPMIN,NGNMIN)
C            ( A guess; no known machine )
                  IWARN = .TRUE.
              END IF
          ELSE
              LEMIN = MIN(NGPMIN,NGNMIN,GPMIN,GNMIN)
C         ( A guess; no known machine )
              IWARN = .TRUE.
          END IF
C**
C Comment out this IF block if Emin is ok
          IF (IWARN) THEN
              FIRST = .TRUE.
              WRITE (NOUT,FMT=99999) LEMIN
          END IF
C**
C
C        Finally compute RMIN by successive division by BETA.
C        We could compute RMIN as base**( EMIN - 1 ), but some machines
C        underflow during this computation.
C
          LRMIN = 1
          DO 40 I = 1,1 - LEMIN
              LRMIN = LRMIN/LBETA
   40     CONTINUE
      END IF
C
      BETA = LBETA
      T = LT
      EPS = LEPS
      EMIN = LEMIN
      RMIN = LRMIN
      RETURN
C
C
C     End of ENVIRN.
C
99999 FORMAT (/,/,' WARNING. The value Emin may be incorrect:-  Emin = '
     +       ,I8,/,
     +       ' If after inspection the value Emin looks acceptable',
     +       ' please comment out ',/,' the IF block as marked within t'
     +       ,'he code of routine envirn,',/,
     +       ' otherwise contact NAG Cen','tral Office. ',/)
      END
