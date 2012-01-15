      SUBROUTINE FRMATI(IWIDTH)
C
C  THIS SUBROUTINE COMPUTES THE WIDTH, W=IWIDTH, IN THE FORMAT
C  SPECIFICATION FOR INTEGER VARIABLES.
C
C  FRMATI SETS IWIDTH TO THE NUMBER OF CHARACTER POSITIONS NEEDED
C  FOR WRITING OUT THE LARGEST INTEGER PLUS ONE POSITION FOR THE SIGN.
C
C  I1MACH(7) IS THE BASE, A, FOR INTEGER REPRESENTATION IN THE MACHINE.
C  I1MACH(8) IS THE (MAXIMUM) NUMBER OF BASE A DIGITS.
C
      INTEGER I1MACH, ICEIL, IWIDTH
C
      IWIDTH = ICEIL( ALOG10(FLOAT(I1MACH(7)))*FLOAT(I1MACH(8)) ) + 1
C
      RETURN
      END
