      SUBROUTINE ADDC(A,B,C,D,E,F)
      IMPLICIT NONE
      DOUBLE PRECISION A,B,C,D,E,F
      DOUBLE PRECISION WK(20),VT(5000),BI(2,20,20),R(2,20,20),
     *RI(2,20,20)
      INTEGER ERRNO1
      COMMON/RESGN2/WK,VT,BI,R,RI,ERRNO1
                        
      E=A+C
      F=B+D
      RETURN
      END
      SUBROUTINE SUBC(A,B,C,D,E,F)
      DOUBLE PRECISION A,B,C,D,E,F
      E=A-C
      F=B-D
      RETURN
      END
      SUBROUTINE DIVC(A,B,C,D,E,F)
      DOUBLE PRECISION A,B,C,D,E,F,R
      R=C*C+D*D
      IF(R.EQ.0.0D0)THEN
C        PRINT *,'FAILURE IN COMPLEX DIVISION'
         call rexit('FAILURE IN COMPLEX DIVISION')
C        STOP
      ERRNO1 = 6
      RETURN

      END IF
      E=(C*A+D*B)/R
      F=(C*B-D*A)/R
      RETURN
      END
      SUBROUTINE MULC(A,B,C,D,E,F)
      DOUBLE PRECISION A,B,C,D,E,F
      E=A*C-B*D
      F=A*D+B*C
      RETURN
      END
      SUBROUTINE MULR(A,B,C,E,F)
      DOUBLE PRECISION A,B,C,E,F
      E=A*C
      F=B*C
      RETURN
      END
