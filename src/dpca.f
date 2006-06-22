      SUBROUTINE ADDC(A,B,C,D,E,F)
      DOUBLE PRECISION A,B,C,D,E,F
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
        PRINT *,'FAILURE IN COMPLEX DIVISION'
        STOP
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
