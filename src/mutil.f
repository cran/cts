      SUBROUTINE MPRINT(N,A,K,TX)
      IMPLICIT NONE
      INTEGER N,K,I,J
      DOUBLE PRECISION A(K,*)
      CHARACTER*(*) TX
C      PRINT *,'MATRIX',TX
C      DO 10 I=1,N
C      DO 10 J=1,N
C   10 PRINT *,'I=',I,'J=',J,'ELEM(I,J)=',A(I,J)
      END
C***********   
      SUBROUTINE MPROD(N,A,B,C,K)
      INTEGER N,K,I,J,L
      DOUBLE PRECISION A(K,*),B(K,*),C(K,*),U
      DO 10 I=1,N
      DO 10 J=1,N
      U=0.D0
      DO 5 L=1,N
    5 U=U+B(I,L)*C(L,J)
   10 A(I,J)=U
      END
C**********
      SUBROUTINE MTPROD(N,A,B,C,K)
      INTEGER N,K,J,L
      DOUBLE PRECISION A(K,*),B(K,*),C(K,*),U
      DO 10 I=1,N
      DO 10 J=1,N
      U=0.D0
      DO 5 L=1,N
    5 U=U+B(L,I)*C(L,J)
   10 A(I,J)=U
      END
C***********
      SUBROUTINE MDIFF(N,A,B,C,K)
      INTEGER N,K,I,J
      DOUBLE PRECISION A(K,*),B(K,*),C(K,*)
      DO 10 I=1,N
      DO 10 J=1,N
   10 A(I,J)=B(I,J)-C(I,J)
      END
C*********
      SUBROUTINE MSPROD(N,A,B,C,K)
      INTEGER N,K,I,J,L
      DOUBLE PRECISION A(K,*),B(K,*),C(K,*),U
      DO 10 I=1,N
      DO 10 J=1,N
      U=0.D0
      DO 5 L=1,N
    5 U=U+C(I,L)*B(J,L)
   10 A(I,J)=U
      DO 20 I=1,N
      DO 20 J=I,N
      U=0.D0
      DO 15 L=1,N
   15 U=U+B(I,L)*A(L,J)
   20 A(J,I)=U
      IF (N.GT.1)THEN
        M=N-1
        DO 30 J=2,N
        DO 30 I=1,J-1
   30   A(I,J)=A(J,I)
      ENDIF 
      END
