CCC READ MACHINE INFORMATION SAVED WITH R CODE
      SUBROUTINE MACHINE(EPS, BASE, T, EMIN, RMIN)
      DOUBLE PRECISION EPS, RMIN
      INTEGER BASE, T, EMIN
      OPEN(UNIT=3,FILE='machine.txt',STATUS='OLD')
      READ(3,*)EPS      
      READ(3,*)BASE
      READ(3,*)T
      READ(3,*)EMIN
      READ(3,*)RMIN
      CLOSE(UNIT=3)
      RETURN
      END
