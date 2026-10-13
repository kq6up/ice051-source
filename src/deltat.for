C=======================================================================
C  ICE 0.51  --  recovered source: Delta T (001C:3480)
C  The original 1988 polynomials.  An up-to-date replacement (table of
C  observed values to 2026, extrapolated to 2050) is in
C  alt/deltat26.for; build with  ./build.sh --deltat2026  to use it.
C=======================================================================
C
C-----------------------------------------------------------------------
C  DELTAT   [001C:3480]  TDT - UT in seconds for a decimal year
C           (original 1988 fit; alt/deltat26.for has the 2026 table)
C-----------------------------------------------------------------------
      SUBROUTINE DELTAT (YEAR, DT)
      DOUBLE PRECISION YEAR, DT, T
      SAVE
      T = YEAR - 1920.D0
      IF (YEAR .GE. 1800.D0 .AND. YEAR .LT. 1920.D0) THEN
         DT = (((((-1.7466294D-9*T - 6.2784728D-7)*T - 8.3015088D-5)*T
     &        - 4.7416012D-3)*T - 9.1297366D-2)*T + 0.71582603D0)*T
     &        + 22.264143D0
      ELSE IF (YEAR .GE. 1920.D0 .AND. YEAR .LE. 2050.D0) THEN
         DT = (((((((((-4.4923383D-16*T + 2.9397795D-13)*T
     &        - 8.1695181D-11)*T + 1.255968D-8)*T - 1.1668619D-6)*T
     &        + 6.7262386D-5)*T - 2.3815278D-3)*T + 4.9569969D-2)*T
     &        - 0.54514611D0)*T + 2.665183D0)*T + 19.755494D0
      ELSE IF (YEAR .GE. 1800.D0 .AND. YEAR .LE. 2050.D0) THEN
         CONTINUE
      END IF
      RETURN
      END
