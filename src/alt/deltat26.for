C=======================================================================
C  ICE 0.51  --  replacement Delta T (same as in the patched ICE_DT26.EXE)
C  Used instead of deltat.for by  ./build.sh --deltat2026 , which also
C  corrects the call site in NAVIG to  IY + IM/12 + ID/365.25 .
C=======================================================================
C
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
