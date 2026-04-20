      SUBROUTINE MPRINT(N,A,K,TX)
      IMPLICIT NONE
      INTEGER N,K,I
      DOUBLE PRECISION A(K,*)
      CHARACTER(LEN=*) TX
C     WRITE(*,*) 'MATRIX ', TX
      DO 10 I=1,N
C        WRITE(*,*) A(I,1:N)
   10 CONTINUE
      END
C***********   
      SUBROUTINE MPROD(N,A,B,C,K)
      INTEGER N,K,I,J,L
      DOUBLE PRECISION A(K,*),B(K,*),C(K,*),U
      DO I=1,N
        DO J=1,N
          U=0.D0
          DO L=1,N
            U=U+B(I,L)*C(L,J)
          END DO
          A(I,J)=U
        END DO
      END DO
      END
C**********
      SUBROUTINE MTPROD(N,A,B,C,K)
      INTEGER N,K,J,L
      DOUBLE PRECISION A(K,*),B(K,*),C(K,*),U
      DO I=1,N
        DO J=1,N
          U=0.D0
          DO L=1,N
            U=U+B(L,I)*C(L,J)
          END DO
          A(I,J)=U
        END DO
      END DO
      END
C***********
      SUBROUTINE MDIFF(N,A,B,C,K)
      INTEGER N,K,I,J
      DOUBLE PRECISION A(K,*),B(K,*),C(K,*)
      DO I=1,N
        DO J=1,N
          A(I,J)=B(I,J)-C(I,J)
        END DO
      END DO
      END
C*********
      SUBROUTINE MSPROD(N,A,B,C,K)
      INTEGER N,K,I,J,L
      DOUBLE PRECISION A(K,*),B(K,*),C(K,*),U
      DO I=1,N
        DO J=1,N
          U=0.D0
          DO L=1,N
            U=U+C(I,L)*B(J,L)
          END DO
          A(I,J)=U
        END DO
      END DO
      DO I=1,N
        DO J=I,N
          U=0.D0
          DO L=1,N
            U=U+B(I,L)*A(L,J)
          END DO
          A(J,I)=U
        END DO
      END DO
      IF (N.GT.1)THEN
        M=N-1
        DO J=2,N
          DO I=1,J-1
            A(I,J)=A(J,I)
          END DO
        END DO
      ENDIF 
      END
