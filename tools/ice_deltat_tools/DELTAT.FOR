C=======================================================================
C  ICE (Interactive Computer Ephemeris) Beta 0.51, USNO NAO, 1989
C  Delta T code recovered from ICE.EXE (MS FORTRAN 4.x, EXEPACKed).
C  There is no original source: names below are mine, the logic,
C  constants and order of evaluation are exactly what the binary does.
C=======================================================================
C
C  ---------------------------------------------------------------------
C  1. ORIGINAL ROUTINE AS COMPILED            (code 001C:3480-3626)
C  ---------------------------------------------------------------------
C  Called once, from the F7 Navigation routine.  YEAR is a decimal year,
C  DT is TDT - UT in seconds.  T is a static (DGROUP:584E) local.
C  Constants live in DGROUP:43A0-4447.
C
C  (renamed DTOLD here so both versions can live in one file)
      SUBROUTINE DTOLD (YEAR, DT)
      DOUBLE PRECISION YEAR, DT, T
      SAVE T
      T = YEAR - 1920.D0
      IF (YEAR .GE. 1800.D0 .AND. YEAR .LT. 1920.D0) THEN
         DT = ((((( -1.7466294D-09 *T
     &              -6.2784728D-07)*T
     &              -8.3015088D-05)*T
     &              -4.7416012D-03)*T
     &              -9.1297366D-02)*T
     &              +7.1582603D-01)*T
     &              +22.264143D0
      ELSE IF (YEAR .GE. 1920.D0 .AND. YEAR .LE. 2050.D0) THEN
         DT = ((((((((( -4.4923383D-16 *T
     &                  +2.9397795D-13)*T
     &                  -8.1695181D-11)*T
     &                  +1.2559680D-08)*T
     &                  -1.1668619D-06)*T
     &                  +6.7262386D-05)*T
     &                  -2.3815278D-03)*T
     &                  +4.9569969D-02)*T
     &                  -5.4514611D-01)*T
     &                  +2.6651830D0  )*T
     &                  +19.755494D0
      ELSE IF (YEAR .GE. 1800.D0 .AND. YEAR .LE. 2050.D0) THEN
C        (the binary tests this third range but the branch is empty;
C         outside 1800-2050 DT is simply left unchanged)
         CONTINUE
      END IF
      RETURN
      END
C
C  ---------------------------------------------------------------------
C  2. THE CALL SITE (F7 Navigation, code 001C:202A-20D0), original
C  ---------------------------------------------------------------------
C     CALL JDCAL (UTJD, IY, IM, ID, CMONTH, ...)     ! 06B6:1B17
C     YEAR = DBLE(IM)/12.D0 + DBLE(ID)/30.D0 + IY    ! <-- bug: ID/30
C     CALL DELTAT (YEAR, DT)                         ! years, not months
C     TDTJD = DT/86400.D0 + UTJD
C     WRITE (*, '(... ''Delta T = '', F5.1, '' seconds'' ...)') ... DT
C
C  So YEAR is IY + IM/12 + ID/30: biased late by 0.1 yr on 1 January,
C  rising to +2.0 yr on 31 December.  The patch changes the 30.D0
C  constant (DGROUP:42E8) to 365.25D0 and removes the remaining
C  1/12 + 1/365.25 offset inside DELTAT, giving the true decimal year.
C
C  ---------------------------------------------------------------------
C  3. REPLACEMENT ROUTINE  (what the patched ICE.EXE now executes)
C  ---------------------------------------------------------------------
C  Linear interpolation in a 2-year table 1800.0-2050.0 (0.01 s units)
C    1800-1972  Astron. Almanac / Stephenson & Morrison (Swiss Eph.)
C    1974-2026  IERS: 32.184 + (TAI-UTC) - (UT1-UTC) at 1 January
C    2028-2050  extrapolated from the latest IERS value
C  Latest IERS value used:
C    last IERS observed MJD 61314 (~2026.74) dT=69.207s
C    then BullA slope +0.139 s/yr; accel 0.0031 s/yr^2
C  Beyond either end the end interval is extrapolated linearly.
C
      SUBROUTINE DELTAT (YEAR, DT)
      DOUBLE PRECISION YEAR, DT, X, F, Y0
      PARAMETER (Y0 = 1800.D0 + 1.D0/12.D0 + 1.D0/365.25D0)
      INTEGER*2 ITAB(126)
      INTEGER I
      DATA (ITAB(I), I =   1,  63) /
     &  1400, 1370, 1310, 1270, 1250, 1250, 1250, 1250, 1250,
     &  1250, 1230, 1200, 1140, 1060, 960, 860, 750, 660, 600,
     &  570, 560, 570, 590, 620, 650, 680, 710, 730, 750, 770,
     &  780, 788, 754, 640, 541, 292, 161, -128, -324, -454, -511,
     &  -542, -546, -579, -564, -566, -601, -664, -647, -576,
     &  -374, -154, 264, 537, 775, 1046, 1336, 1601, 1824, 2025,
     &  2116, 2241, 2349 /
      DATA (ITAB(I), I =  64, 126) /
     &  2386, 2434, 2402, 2387, 2386, 2373, 2396, 2433, 2530,
     &  2624, 2728, 2825, 2915, 2997, 3072, 3135, 3218, 3315,
     &  3400, 3503, 3654, 3829, 4018, 4223, 4448, 4646, 4853,
     &  5054, 5217, 5379, 5487, 5582, 5686, 5831, 5998, 6163,
     &  6297, 6383, 6430, 6457, 6484, 6546, 6607, 6660, 6728,
     &  6810, 6897, 6936, 6929, 6918, 6911, 6939, 6969, 7002,
     &  7038, 7076, 7117, 7160, 7205, 7253, 7304, 7356, 7412 /
      X = (YEAR - Y0) * 0.5D0
      I = INT(X)
      IF (X .LT. 0.D0) I = 0
      IF (I .GT. 124) I = 124
      F = X - DBLE(I)
      DT = (DBLE(ITAB(I+1)) + F*DBLE(ITAB(I+2)-ITAB(I+1))) * 0.01D0
      RETURN
      END
C
C  New call site:   YEAR = DBLE(IM)/12.D0 + DBLE(ID)/365.25D0 + IY
