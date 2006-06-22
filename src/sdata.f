*UPTODATE SDATATEXT
      SUBROUTINE SDATA
C
C        MARK 2.2 RELEASE.  NAG COPYRIGHT 1987.
C
C PURPOSE
C
C        This subroutine initialises the values of machine dependent
C        constants, for use by other sub-programs.
C
C METHOD
C
C        Pre-determined values assigned to common blocks.
C
C COMMON BLOCK
C
C        MDEPS        length 4 units ( 4*REAL )
C
C                BASE    REAL
C                        -The base of the machine representation of
C                        numerical values.
C
C                BIG     REAL
C                        -A large number that can be represented
C                        in the machine without causing overflow.
C
C                SMALL   REAL
C                        -A small number for which the machine
C                        can evaluate the expression  1.0 / SMALL
C                        in without causing overflow.
C
C                EPS     REAL
C                        -The relative accuracy of machine arithmetic.
C                        Namely, approximately the smallest positive
C                        value for which the logical expression
C
C                                1.0 + EPS  .GT.  1.0
C
C                        is true. Note that this is sometimes referred
C                        to as  MACHEP , MACHEPS , EPS or MPREC.
C
C        SMPREC       length  1  unit  ( 1*REAL )
C
C                EPSS    REAL
C                        -Equivalent to  EPS  above.
C
C*** NOTE that the common block ADDEPS is not used within SLICE
C    but is used to output other useful machine dependent values.
C
C        ADDEPS       length  3  units ( 2*INTEGER, 1*REAL )
C
C                T       INTEGER
C                        -The number of digits in the mantissa.
C
C                EMIN    INTEGER
C                        -The minimum exponent before (gradual)
C                         underflow occurs.
C
C                RMIN    REAL
C                        -The smallest normalized number for the machine
C
C
C VERSION        SDATA 2.2  A Fortran 77 version.
C
C LIBRARY INDEX
C
C                SLICE 2.2
C                Section  1.  Utility Routines
C                Subsection  1.0  General Routines
C
C COMMENTS
C
C                None
C
C******************************************************************
C
C     .. Scalars in Common ..
      DOUBLE PRECISION BASE,BIG,EPS,EPSS,RMIN,SMALL
      INTEGER EMIN,T
C     .. Local Scalars ..
      DOUBLE PRECISION FLMIN,MEPS
      INTEGER BETA
      LOGICAL FIRST
C     .. External Subroutines ..
      EXTERNAL ENVIRN
C     .. Common blocks ..
      COMMON /ADDEPS/RMIN,EMIN,T
      COMMON /MDEPS/BASE,BIG,SMALL,EPS
      COMMON /SMPREC/EPSS
C     .. Save statement ..
      SAVE FIRST
C     .. Data statements ..
C
      DATA FIRST/.TRUE./
C     .. Executable Statements ..
C
      IF (FIRST) THEN
          FIRST = .FALSE.
C
          CALL ENVIRN(BETA,T,MEPS,EMIN,RMIN)
C
          FLMIN = RMIN*BETA**4
C
          BASE = BETA
          SMALL = FLMIN
          BIG = 1/FLMIN
          EPS = MEPS
          EPSS = EPS
      END IF
      RETURN
      END
