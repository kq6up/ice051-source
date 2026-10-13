C=======================================================================
C  ICE 0.51  --  recovered source: F6 Rise & Set Times (0A09:0000)
C  This file is kept in UTF-8; the build converts it to code page 437.
C=======================================================================
C
C-----------------------------------------------------------------------
C  RISSET   [0A09:0000]  F6: daily times of rising, transit and setting
C           (with azimuths and transit altitude) for the location of
C           the initial values; for the Sun also the times of civil,
C           nautical and astronomical twilight.
C           The transit is found by iteration on the Greenwich meridian
C           (one day before the first date is computed to obtain the
C           motion), then interpolated to the local meridian; rising
C           and setting follow from the semidiurnal arc, refined with
C           new places for Mercury, the Moon, and for Venus and the Sun
C           at latitudes beyond 48 degrees.
C           Symbols: '-- --' object does not rise, '** **' it does not
C           set; for twilight '// //', '## ##' no such twilight.
C-----------------------------------------------------------------------
      SUBROUTINE RISSET
      DOUBLE PRECISION DATE0, ALAT, ALONG, STEP
      INTEGER*4 NPOS, IZONE
      COMMON /SITE/ DATE0, ALAT, ALONG, STEP, NPOS, IZONE
      INTEGER*4 IACC
      LOGICAL LFILE
      COMMON /ACCUR/ IACC, LFILE
      DOUBLE PRECISION ONE, DSAVE, XLONG, XLAT, TJD, TJD0, DAY, RLAT,
     &     SINLAT, COSLAT, ZONE, FLON, TOL, T, TMID, TNEXT, RAPREV,
     &     DECPREV, RARATE, DECRATE, RATE, RATE0, TT, GAST, RA, DEC,
     &     DIS, HP, SD, VMAG, H, DT, DTT, DRA, DDEC, TREF, TLOC, HRS,
     &     RAT, DECT, ALT, SINDT, COSDT, TANDT, HDEP, H0, COSH, TR, TS,
     &     RAR, DECR, RAS, DECS, HP2, SD2, TK, RAK, DECK, DRK, X, SEMIR,
     &     SEMIS, SINH, AZ, DTP, REFR
      DOUBLE PRECISION TWD(3), TWA(3), STW(3), RATK(3), CTW(3),
     &     HTW(3), DTW(3), TWM(3), TWE(3), DTWM(3), DTWE(3), SDK(3),
     &     CDK(3), TDK(3), DTWM2(3), DTWE2(3), TWH(3)
      INTEGER*4 NUM, IERR, IZ, ICOUNT, NLOOP, J, ITER, K, I, M, IPRT1,
     &     IY, IM, ID, IASC, ISCAN, IYS(400), IDS(400)
      LOGICAL LFIRST, LPREC, LUP
      CHARACTER*28 TWMENU(5)
      CHARACTER*10 CLONG, CLAT
      CHARACTER*8 OBJ, ONAME
      CHARACTER*6 STR, STR6, SRISE, SSET, STRT(2), TWS(3,2,400)
      CHARACTER*4 SALT, SAZR, SAZS
      CHARACTER*3 CMON, CMS(400)
      CHARACTER*1 CTR, CNS, CRI, CSE
      SAVE
      DATA TWD / -6.D0, -12.D0, -18.D0 /
      DATA TWMENU /
     & 'ÉÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»',
     & 'º F1 ³ Twilight times      º',
     & 'ÇÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶',
     & 'º F2 ³ Return to Main Menu º',
     & 'ÈÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼' /
      IACC = 2
      ONE = 1.D0
      IPRT1 = 2
      DSAVE = DATE0
      XLONG = ALONG
      XLAT = ALAT
   10 LFIRST = .TRUE.
      WRITE (*, 9000)
      CALL CURON
      WRITE (*, 9010) ' Enter object name: '
      READ (*, 9020) OBJ
      CALL CUROFF
      IF (OBJ .EQ. ' ') GO TO 999
      CALL LOOKUP (OBJ, NUM, ONAME)
      IF (NUM .EQ. 99) THEN
         WRITE (*,*) 'Name not found.  Try again.'
         CALL HITKEY
         GO TO 10
      END IF
C     start two (sidereal) days early to get the motion of the object
      CALL DATJD (DATE0, TJD)
      TJD0 = TJD
      CALL DATJD (ONE, DAY)
      WRITE (*, 9000)
      TJD = TJD - DAY * 2.D0 / 1.002737909350795D0
      RLAT = XLAT * 0.017453292519943295D0
      SINLAT = DSIN (RLAT)
      COSLAT = DCOS (RLAT)
      IF (XLONG .GT. 180.D0) XLONG = XLONG - 360.D0
      IF (XLONG .LT. -180.D0) XLONG = 360.D0 + XLONG
      ZONE = IZONE
      IZ = ZONE
C     fraction of a day from Greenwich to the local meridian
      FLON = -(XLONG / 360.D0)
      IF (FLON .LT. 0.D0) FLON = 1.D0 + FLON
      TOL = 0.0001D0
      IF (NUM .EQ. 11) TOL = 1.D-5
      LPREC = NUM .EQ. 1 .OR. NUM .EQ. 11 .OR.
     &        (NUM .EQ. 2 .AND. DABS (XLAT) .GT. 48.D0) .OR.
     &        (NUM .EQ. 10 .AND. DABS (XLAT) .GT. 48.D0)
      CALL LATLON (XLONG, XLAT, CLONG, CLAT)
      IF (IZ .EQ. 0) THEN
         WRITE (*, 9030) ONAME, CLONG, CLAT
         IF (LFILE) WRITE (10, 9030) ONAME, CLONG, CLAT
      ELSE
         WRITE (*, 9030) ONAME, CLONG, CLAT, IZ
         IF (LFILE) WRITE (10, 9030) ONAME, CLONG, CLAT, IZ
      END IF
      T = TJD
      TMID = DINT (T - 0.5D0) + 0.5D0
      RAPREV = 0.D0
      DECPREV = 0.D0
      RARATE = 0.D0
      DECRATE = 0.D0
      RATE = 1.002737909350795D0
      RATE0 = 1.002737909350795D0
      LUP = .FALSE.
      ICOUNT = 0
      NLOOP = NPOS
      IF (LFIRST) NLOOP = NPOS + 2
      WRITE (*, 9040)
      WRITE (*, 9050) 'h  m   s', 'h  m    ø', 'h  m    ø', 'h  m    ø'
      IF (LFILE) THEN
         WRITE (10, 9040)
         WRITE (10, 9050) 'h  m   s', 'h  m    ø', 'h  m    ø',
     &      'h  m    ø'
      END IF
C
C     ---- one day per pass ----
      J = 1
  100 IF (J .GT. NLOOP) GO TO 800
      ICOUNT = ICOUNT + 1
      ITER = 0
C     transit at Greenwich, by iteration
      TT = DAY / RATE + T
  110 ITER = ITER + 1
      CALL SIDTIM (TT, 0.D0, 1, GAST)
      CALL PLACE (TT, 1, NUM, RA, DEC, DIS, HP, SD, VMAG, IERR)
      IF (IERR .NE. 0) GO TO 990
      H = GAST - RA
      IF (H .LT. -12.D0) H = 24.D0 + H
      IF (H .GE. 12.D0) H = H - 24.D0
      DT = H / 24.D0 / RATE
      TT = TT - DT
      IF (DABS (DT) .GT. TOL) GO TO 110
      IF (ICOUNT .EQ. 1) GO TO 700
C     daily motion
      DTT = TT - T
      DRA = RA - RAPREV
      IF (DRA .LT. -12.D0) DRA = 24.D0 + DRA
      IF (DRA .GE. 12.D0) DRA = DRA - 24.D0
      DDEC = DEC - DECPREV
      RARATE = DRA / DTT
      DECRATE = DDEC / DTT
      RATE = 1.002737909350795D0 - RARATE / 24.D0
      RATE0 = RATE
      TREF = T
      IF (FLON .GT. 0.5D0) TREF = TT
      TMID = DINT (TREF - 0.5D0) - ZONE / 24.D0 + 0.5D0
      TNEXT = TMID + 1.D0
C     transit at the local meridian
      TLOC = (1.D0 - FLON) * T + FLON * TT
      HRS = (TLOC - TMID) * 24.D0
      CALL DAYSYM (HRS, CTR)
      CALL ANGSTR (HRS, 3, 0, 2, -1, 5, STR)
      IF (LPREC) THEN
         CALL PLACE (TLOC, 1, NUM, RAT, DECT, DIS, HP, SD, VMAG, IERR)
      ELSE
         DECT = (1.D0 - FLON) * DECPREV + FLON * DEC
      END IF
      ALT = (DECT - XLAT) + 90.D0
      CNS = 's'
      IF (ALT .GT. 90.D0) THEN
         ALT = 180.D0 - ALT
         CNS = 'n'
      END IF
      IF (ALT .LE. 30.D0) ALT = REFR (ALT) + ALT
      SALT = '--  '
      IF (ALT .GT. -2.D0) WRITE (SALT, 9060) ALT
      DECT = 0.017453292519943295D0 * DECT
      SINDT = DSIN (DECT)
      COSDT = DCOS (DECT)
C     altitude of the upper limb at rising: parallax - semidiameter
C     - 34' refraction
      HDEP = -SD + HP - 2040.D0
      H0 = (-SD + HP - 2040.D0) / 3600.D0 * 0.017453292519943295D0
      DO 120 K = 1, 3
         TWA(K) = TWD(K) * 0.017453292519943295D0
         STW(K) = DSIN (TWA(K))
  120 CONTINUE
C     first estimates of the semidiurnal arcs
      COSH = (H0 - SINLAT*SINDT) / (COSLAT*COSDT)
      IF (COSH .GE. 0.999D0) COSH = 0.999D0
      IF (COSH .LE. -0.999D0) COSH = -0.999D0
      H = DACOS (COSH) / 6.283185307179586D0
      DT = H / RATE
      DO 130 K = 1, 3
         RATK(K) = RATE
         CTW(K) = (STW(K) - SINLAT*SINDT) / (COSLAT*COSDT)
         IF (CTW(K) .GE. 0.999D0) CTW(K) = 0.999D0
         IF (CTW(K) .LE. -0.999D0) CTW(K) = -0.999D0
         HTW(K) = DACOS (CTW(K)) / 6.283185307179586D0
         DTW(K) = HTW(K) / RATK(K)
  130 CONTINUE
      IF (LUP) GO TO 400
C
C     ---- rising (and morning twilight) ----
      TR = TLOC - DT
      DO 140 K = 1, 3
         TWM(K) = TLOC - DTW(K)
  140 CONTINUE
      IF (.NOT. LPREC) GO TO 160
      CALL PLACE (TR, 1, NUM, RAR, DECR, DIS, HP2, SD2, VMAG, IERR)
      DRA = RAR - RAT
      IF (DRA .LT. -12.D0) DRA = 24.D0 + DRA
      IF (DRA .GE. 12.D0) DRA = DRA - 24.D0
      RARATE = DRA / (TR - TLOC)
      RATE = 1.002737909350795D0 - RARATE / 24.D0
      DO 150 K = 1, 3
         TK = TWM(K)
         CALL PLACE (TK, 1, NUM, RAK, DECK, DIS, HP2, SD2, VMAG, IERR)
         DTWM(K) = DECK
         DRK = RAK - RAT
         IF (DRK .LT. -12.D0) DRK = 24.D0 + DRK
         IF (DRK .GE. 12.D0) DRK = DRK - 24.D0
         RATK(K) = 1.002737909350795D0 - DRK / (TWM(K) - TLOC) / 24.D0
  150 CONTINUE
      GO TO 170
  160 DECR = (TR - T) * DECRATE + DECPREV
      DO 165 K = 1, 3
         DTWM(K) = (TWM(K) - T) * DECRATE + DECPREV
  165 CONTINUE
  170 DECR = 0.017453292519943295D0 * DECR
      SINDT = DSIN (DECR)
      COSDT = DCOS (DECR)
      TANDT = SINDT / DCOS (DECR)
      COSH = (H0 - SINLAT*SINDT) / (COSLAT*COSDT)
      IF (NUM .NE. 10) GO TO 200
C     morning twilight
      DO 190 K = 1, 3
         DTWM(K) = 0.017453292519943295D0 * DTWM(K)
         SDK(K) = DSIN (DTWM(K))
         CDK(K) = DCOS (DTWM(K))
         TDK(K) = SDK(K) / CDK(K)
         CTW(K) = (STW(K) - SDK(K)*SINLAT) / (CDK(K)*COSLAT)
         IF (DABS (COSH) .GT. 1.D0) GO TO 180
         IF (DABS (CTW(K)) .LE. 1.D0) GO TO 185
         TWM(K) = TMID
         TWS(K,1,J) = '// //'
         GO TO 190
  180    IF (DABS (CTW(K)) .LE. 1.D0) GO TO 185
         IF (CTW(K) .LT. -1.D0) THEN
            TWM(K) = TMID
            TWS(K,1,J) = '-- --'
         ELSE
            TWM(K) = TMID
            TWS(K,1,J) = '## ##'
         END IF
         GO TO 190
  185    HTW(K) = DACOS (CTW(K)) / 6.283185307179586D0
         DTWM2(K) = HTW(K) / RATK(K)
         TWM(K) = TLOC - DTWM2(K)
         TWH(K) = (TWM(K) - TMID) * 24.D0
         X = TWH(K)
         CALL ANGSTR (X, 3, 0, 2, -1, 5, STR6)
         TWS(K,1,J) = STR6
  190 CONTINUE
C     rising time and azimuth
  200 IF (DABS (COSH) .GT. 1.D0) GO TO 220
      H = DACOS (COSH) / 6.283185307179586D0
      SEMIR = H / RATE
      TR = TLOC - SEMIR
      HRS = (TR - TMID) * 24.D0
      CALL DAYSYM (HRS, CRI)
      CALL ANGSTR (HRS, 3, 0, 2, -1, 5, SRISE)
      SINH = -DSQRT (1.D0 - COSH*COSH)
      AZ = DATAN2 (-SINH, TANDT*COSLAT - COSH*SINLAT)
     &     / 0.017453292519943295D0
      IF (AZ .LT. 0.D0) AZ = 360.D0 + AZ
      WRITE (SAZR, 9070) AZ
      GO TO 260
C     never rises
  220 IF (COSH .LE. 1.D0) GO TO 240
      TR = TMID
      CRI = ' '
      SRISE = '-- -- '
      SAZR = '--- '
      LUP = .FALSE.
  230 IF (.NOT. LUP) GO TO 600
      GO TO 410
C     never sets
  240 IF (ICOUNT .GT. 2) GO TO 250
      TR = TMID
      CRI = ' '
      SRISE = '** ** '
      SAZR = '*** '
      GO TO 260
  250 TR = TS
      HRS = (TR - TMID) * 24.D0
      CALL DAYSYM (HRS, CRI)
      CALL ANGSTR (HRS, 3, 0, 2, -1, 5, SRISE)
      SAZR = SAZS
  260 LUP = .TRUE.
      GO TO 230
C     the object stayed above the horizon the previous day
  400 TR = TMID
      CRI = ' '
      SRISE = '** ** '
      SAZR = '*** '
      DO 405 K = 1, 3
         TWM(K) = TMID
         TWS(K,1,J) = '** **'
  405 CONTINUE
      GO TO 230
C
C     ---- setting (and evening twilight) ----
  410 TS = TLOC + DT
      DO 415 K = 1, 3
         TWE(K) = DTW(K) + TLOC
  415 CONTINUE
      IF (.NOT. LPREC) GO TO 430
      CALL PLACE (TS, 1, NUM, RAS, DECS, DIS, HP2, SD2, VMAG, IERR)
      DRA = RAS - RAT
      IF (DRA .LT. -12.D0) DRA = 24.D0 + DRA
      IF (DRA .GE. 12.D0) DRA = DRA - 24.D0
      RARATE = DRA / (TS - TLOC)
      RATE = 1.002737909350795D0 - RARATE / 24.D0
      DO 420 K = 1, 3
         TK = TWE(K)
         CALL PLACE (TK, 1, NUM, RAK, DECK, DIS, HP2, SD2, VMAG, IERR)
         DTWE(K) = DECK
         DRK = RAK - RAT
         IF (DRK .LT. -12.D0) DRK = 24.D0 + DRK
         IF (DRK .GE. 12.D0) DRK = DRK - 24.D0
         RATK(K) = 1.002737909350795D0 - DRK / (TWE(K) - TLOC) / 24.D0
  420 CONTINUE
      GO TO 440
  430 DECS = (TS - T) * DECRATE + DECPREV
      DO 435 K = 1, 3
         DTWE(K) = (TWE(K) - T) * DECRATE + DECPREV
  435 CONTINUE
  440 DECS = 0.017453292519943295D0 * DECS
      SINDT = DSIN (DECS)
      COSDT = DCOS (DECS)
      TANDT = SINDT / DCOS (DECS)
      COSH = (H0 - SINLAT*SINDT) / (COSLAT*COSDT)
      IF (NUM .NE. 10) GO TO 500
C     evening twilight
      DO 490 K = 1, 3
         DTWE(K) = 0.017453292519943295D0 * DTWE(K)
         SDK(K) = DSIN (DTWE(K))
         CDK(K) = DCOS (DTWE(K))
         TDK(K) = SDK(K) / CDK(K)
         CTW(K) = (STW(K) - SDK(K)*SINLAT) / (CDK(K)*COSLAT)
         IF (DABS (COSH) .GT. 1.D0) GO TO 480
         IF (DABS (CTW(K)) .LE. 1.D0) GO TO 485
         IF (CTW(K) .GT. 1.D0) THEN
            TWE(K) = TNEXT
            TWS(K,2,J) = '-- --'
         ELSE
            TWE(K) = TNEXT
            TWS(K,2,J) = '// //'
         END IF
         GO TO 490
  480    TWE(K) = TNEXT
         TWS(K,2,J) = '** **'
         GO TO 490
  485    HTW(K) = DACOS (CTW(K)) / 6.283185307179586D0
         DTWE2(K) = HTW(K) / RATK(K)
         TWE(K) = DTWE2(K) + TLOC
         TWH(K) = (TWE(K) - TMID) * 24.D0
         X = TWH(K)
         CALL ANGSTR (X, 3, 0, 2, -1, 5, STR6)
         TWS(K,2,J) = STR6
  490 CONTINUE
C     setting time and azimuth
  500 IF (DABS (COSH) .GT. 1.D0) GO TO 520
      H = DACOS (COSH) / 6.283185307179586D0
      SEMIS = H / RATE
      TS = SEMIS + TLOC
      HRS = (TS - TMID) * 24.D0
      CALL DAYSYM (HRS, CSE)
      CALL ANGSTR (HRS, 3, 0, 2, -1, 5, SSET)
      SINH = DSQRT (1.D0 - COSH*COSH)
      AZ = DATAN2 (-SINH, TANDT*COSLAT - COSH*SINLAT)
     &     / 0.017453292519943295D0
      IF (AZ .LT. 0.D0) AZ = 360.D0 + AZ
      WRITE (SAZS, 9070) AZ
  510 LUP = .FALSE.
      GO TO 650
C     does not set
  520 IF (COSH .GE. -1.D0) GO TO 530
      TS = TNEXT
      CSE = ' '
      SSET = '** **'
      SAZS = '***'
      LUP = .TRUE.
      GO TO 650
C     sets before it rises
  530 TS = TR
      HRS = (TS - TMID) * 24.D0
      CALL DAYSYM (HRS, CSE)
      CALL ANGSTR (HRS, 3, 0, 2, -1, 5, SSET)
      SAZS = SAZR
      GO TO 510
C     does not rise
  600 TS = TNEXT
      CSE = ' '
      SSET = '-- -- '
      SAZS = '--- '
      IF (NUM .NE. 0) GO TO 650
      DO 640 K = 1, 3
         IF (DABS (CTW(K)) .LE. 1.D0) GO TO 630
         IF (CTW(K) .LT. -1.D0) THEN
            TWE(K) = TNEXT
            TWS(K,2,J) = '-- --'
         ELSE
            TWE(K) = TNEXT
            TWS(K,2,J) = '## ##'
         END IF
         GO TO 640
  630    HTW(K) = DACOS (CTW(K)) / 6.283185307179586D0
         DTWE2(K) = HTW(K) / RATK(K)
         TWE(K) = DTWE2(K) + TLOC
         TWH(K) = (TWE(K) - TMID) * 24.D0
         X = TWH(K)
         CALL ANGSTR (X, 3, 0, 2, -1, 5, STR6)
         TWS(K,2,J) = STR6
  640 CONTINUE
C
C     ---- the first two passes only prepare; skip days before the
C          starting date ----
  650 IF (ICOUNT .LE. 2) GO TO 690
      DTP = TJD0 - TREF
      IF (DTP .LE. 0.D0) GO TO 750
      NLOOP = NLOOP + 1
  690 IPRT1 = IPRT1 + 1
  700 T = TT
      RAPREV = RA
      DECPREV = DEC
      RATE = RATE0
      J = J + 1
      GO TO 100
C
C     ---- print one day ----
  750 IF (ICOUNT .EQ. 3 .AND. FLON .GT. 0.5D0) THEN
         CALL CALDAT (T, IY, IM, ID, CMON, HRS)
         CALL ANGSTR (HRS, 3, 0, 2, 1, 10, STRT)
         WRITE (*, 9080) IY, CMON, ID, T, STRT
         IF (LFILE) WRITE (10, 9080) IY, CMON, ID, T, STRT
      END IF
      CALL CALDAT (TREF, IY, IM, ID, CMON, HRS)
      CALL ANGSTR (HRS, 3, 0, 2, 1, 10, STRT)
      WRITE (*, 9080) IY, CMON, ID, TREF, STRT, CRI, SRISE, SAZR, CTR,
     &   STR, CNS, SALT, CSE, SSET, SAZS
      IF (LFILE) WRITE (10, 9080) IY, CMON, ID, TREF, STRT, CRI, SRISE,
     &   SAZR, CTR, STR, CNS, SALT, CSE, SSET, SAZS
      IYS(J) = IY
      WRITE (CMS(J), 9090) CMON
      IDS(J) = ID
      GO TO 700
C
C     ---- twilight table for the Sun ----
  800 IF (NUM .NE. 10) GO TO 990
      WRITE (*, 9100)
      WRITE (*, 9110) TWMENU
  810 CALL GETKEY (IASC, ISCAN)
      IF (ISCAN .LT. 59 .OR. ISCAN .GT. 60) GO TO 810
      IF (ISCAN .EQ. 60) GO TO 999
      WRITE (*, 9000)
      WRITE (*, 9120)
      WRITE (*, 9130) CLONG, CLAT, IZ
      WRITE (*, 9140)
      WRITE (*, 9150)
      WRITE (*, 9160)
      WRITE (*, 9170) (IYS(M), CMS(M), IDS(M),
     &   ((TWS(I,K,M), K = 1, 2), I = 1, 3), M = IPRT1, NLOOP)
      IF (.NOT. LFILE) GO TO 990
      WRITE (10, 9120)
      WRITE (10, 9130) CLONG, CLAT, IZ
      WRITE (10, 9140)
      WRITE (10, 9150)
      WRITE (10, 9160)
      WRITE (10, 9170) (IYS(M), CMS(M), IDS(M),
     &   ((TWS(I,K,M), K = 1, 2), I = 1, 3), M = IPRT1, NLOOP)
  990 CALL HITKEY
  999 RETURN
 9000 FORMAT (///////////////////////,/)
 9010 FORMAT (//A,$)
 9020 FORMAT (A)
 9030 FORMAT (26X,'Topocentric Ephemeris of ',A8//,
     &   12X,'³   for Greenwich Meridian  ³',SP,
     &   '   for location:  Longitude ',A/
     &   12X,'³',27X,'³',18X,'Latitude  ',A/
     &   12X,'³',27X,'³',18X,'Time Zone   UT',I3)
 9040 FORMAT (12X,'³      Ephemeris Transit    ³'/
     &   '    Date    ³    TDT JD       TDT Time  ³',
     &   '  Rise (Azm)  Trans (Alt)   Set  (Azm)')
 9050 FORMAT (T32,A,T45,A,T58,A,T71,A)
 9060 FORMAT (F3.0)
 9070 FORMAT (F4.0)
 9080 FORMAT (1X,I4,1X,A3,1X,I2,2X,F14.6,2X,A6,A4,1X,1X,A1,A5,' (',A3,
     &   ')',1X,A1,A5,' (',A1,A2,')',1X,A1,A5,' (',A3,')')
 9090 FORMAT (A3)
 9100 FORMAT (/)
 9110 FORMAT (T28,A)
 9120 FORMAT (//T33,'Times of Twilight')
 9130 FORMAT (T40,'for'/1X,'Longitude:',A,9X,'Latitude:',A,8X,
     &   'Time Zone: UT',I3,/)
 9140 FORMAT (T24,'Civil',T43,'Nautical',T61,'Astronomical')
 9150 FORMAT (T5,'Date',T18,'Morning',T28,'Evening',T38,'Morning',
     &   T48,'Evening',T58,'Morning',T68,'Evening')
 9160 FORMAT (T20,'h',T23,'m',T30,'h',T33,'m',T40,'h',T43,'m',
     &   T50,'h',T53,'m',T60,'h',T63,'m',T70,'h',T73,'m')
 9170 FORMAT (400(1X,I4,1X,A3,1X,I2,1X,6(5X,A5),/))
      END
