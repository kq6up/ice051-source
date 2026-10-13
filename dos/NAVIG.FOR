C=======================================================================
C  ICE 0.51  --  recovered source: F7 Navigation (001C:1DDF)
C  This file is kept in UTF-8; the build converts it to code page 437.
C=======================================================================
C
C-----------------------------------------------------------------------
C  NAVIG    [001C:1DDF]  F7: almanac data (GHA, declination) and the
C           computed altitude and azimuth with altitude corrections for
C           one object (plus the Sun) or, with 'ALL', for the Sun, Moon,
C           navigational planets and the 57 navigational stars
C-----------------------------------------------------------------------
      SUBROUTINE NAVIG
      DOUBLE PRECISION DATE0, ALAT, ALONG, STEP
      INTEGER*4 NPOS, IZONE
      COMMON /SITE/ DATE0, ALAT, ALONG, STEP, NPOS, IZONE
      INTEGER*4 IACC
      LOGICAL LFILE
      COMMON /ACCUR/ IACC, LFILE
$INCLUDE:'catlg.inc'
      DOUBLE PRECISION XLONG, XLAT, RLAT, SINLAT, COSLAT, TJD, HOUR,
     &     YEAR, DTSEC, TDT, GAST, GHAA, RA, DEC, DIS, HP, SD, VMAG,
     &     GHA, HLOC, SINDEC, COSDEC, TANDEC, SINLHA, COSLHA, HC, HCD,
     &     ZN, ZND, PA, HA, REF, SUM, REFR
      INTEGER*4 NUM, IERR, I, NOBJ, IY, IM, ID
      LOGICAL LALL, LSUN, LMOON
      CHARACTER*10 CLONG, CLAT
      CHARACTER*8 OBJ, ONAME, SGHA, SDEC, SHC, STIME
      CHARACTER*3 CMON
      SAVE
      IACC = 2
      SD = 0.D0
      XLONG = ALONG
      XLAT = ALAT
   10 WRITE (*, 9000)
      CALL CURON
      WRITE (*, 9010) ' Enter object name: '
      READ (*, 9020) OBJ
      CALL CUROFF
      IF (OBJ .EQ. ' ') RETURN
      IF (OBJ .EQ. 'ALL' .OR. OBJ .EQ. 'all') THEN
         LALL = .TRUE.
      ELSE
         LALL = .FALSE.
         CALL LOOKUP (OBJ, NUM, ONAME)
      END IF
      IF (LALL .AND. ICAT .NE. 1) THEN
         WRITE (*,*) '''ALL'' option requires Catalog 1.'
         GO TO 90
      END IF
      IF (.NOT. LALL .AND. NUM .EQ. 99) THEN
         WRITE (*,*) 'Name not found. Try again.'
         CALL HITKEY
         GO TO 10
      END IF
      CALL DATJD (DATE0, TJD)
      RLAT = XLAT * 0.017453292519943295D0
      SINLAT = DSIN (XLAT * 0.017453292519943295D0)
      COSLAT = DCOS (RLAT)
      IF (XLONG .GT. 180.D0) XLONG = XLONG - 360.D0
      IF (XLONG .LT. -180.D0) XLONG = 360.D0 + XLONG
      WRITE (*, 9000)
      CALL LATLON (XLONG, XLAT, CLONG, CLAT)
      CALL CALDAT (TJD, IY, IM, ID, CMON, HOUR)
      CALL ANGSTR (HOUR, 3, 0, 2, 0, 8, STIME)
C     (sic: month and day are not combined with the year correctly)
      YEAR = DBLE (IM) / 12.D0 + DBLE (ID) / 30.D0 + DBLE (IY)
      CALL DELTAT (YEAR, DTSEC)
      TDT = DTSEC / 86400.D0 + TJD
      WRITE (*, 9030) IY, CMON, ID, STIME, DTSEC, CLONG, CLAT
      WRITE (*, 9040) 'Almanac Data', '³', 'Altitude Corrections'
      WRITE (*, 9050)
      WRITE (*, 9060) 'ø', '''', 'ø', '''', 'ø', '''', 'ø', '³', '''',
     &   '''', '''', ''''
      IF (LFILE) THEN
         WRITE (10, 9030) IY, CMON, ID, STIME, DTSEC, CLONG, CLAT
         WRITE (10, 9040) 'Almanac Data', '³', 'Altitude Corrections'
         WRITE (10, 9050)
         WRITE (10, 9060) 'ø', '''', 'ø', '''', 'ø', '''', 'ø', '³',
     &      '''', '''', '''', ''''
      END IF
      CALL SIDTIM (TJD, 0.D0, 1, GAST)
      GHAA = GAST * 15.D0
      NOBJ = 2
      IF (LALL) NOBJ = 63
      DO 50 I = 1, NOBJ
         IF (LALL) THEN
            CALL GETNAM (I, OBJ)
         ELSE IF (I .EQ. 2) THEN
C           the Sun is always listed after the object asked for
            IF (LSUN) GO TO 50
            OBJ = 'SUN'
         END IF
         CALL LOOKUP (OBJ, NUM, ONAME)
         LSUN = ONAME .EQ. 'SUN'
         LMOON = ONAME .EQ. 'MOON'
         CALL PLACE (TDT, 1, NUM, RA, DEC, DIS, HP, SD, VMAG, IERR)
         IF (IERR .NE. 0) GO TO 90
C        Greenwich and local hour angle (degrees)
         GHA = (GAST - RA) * 15.D0
         IF (GHA .LT. 0.D0) GHA = 360.D0 + GHA
         IF (GHA .GE. 360.D0) GHA = GHA - 360.D0
         HLOC = GHA + XLONG
         SINDEC = DSIN (DEC * 0.017453292519943295D0)
         COSDEC = DCOS (DEC * 0.017453292519943295D0)
         TANDEC = SINDEC / DCOS (DEC * 0.017453292519943295D0)
         SINLHA = DSIN (HLOC * 0.017453292519943295D0)
         COSLHA = DCOS (HLOC * 0.017453292519943295D0)
C        computed altitude and azimuth
         HC = 1.5707963267948966D0
     &      - DACOS (COSLAT*COSDEC*COSLHA + SINLAT*SINDEC)
         HCD = HC / 0.017453292519943295D0
         ZN = DATAN2 (-SINLHA, TANDEC*COSLAT - COSLHA*SINLAT)
         IF (ZN .LT. 0.D0) ZN = 6.283185307179586D0 + ZN
         ZND = ZN / 0.017453292519943295D0
         CALL DMSTR (GHA, 1, 8, SGHA)
         CALL DMSTR (DEC, 2, 8, SDEC)
         CALL DMSTR (HCD, 3, 8, SHC)
         IF (HCD .GT. -0.5D0) THEN
C           parallax in altitude, semidiameter, refraction (arcmin)
            PA = DCOS (HC) * (HP / 206264.8D0)
            PA = DCOS (HC - PA) * (HP / 60.D0)
            IF (LSUN) SD = 1.55D0 + SD
            SD = SD / 60.D0
            IF (LMOON) SD = (DSIN (HC) / 60.27D0 + 1.D0) * SD
            HA = HCD - PA / 60.D0 - SD / 60.D0
            REF = -(REFR (HA) * 60.D0)
            SUM = REF + SD + PA
            WRITE (*, 9070) ONAME, SGHA, SDEC, SHC, ZND, REF, SD, PA,
     &         SUM
            IF (LFILE) WRITE (10, 9070) ONAME, SGHA, SDEC, SHC, ZND,
     &         REF, SD, PA, SUM
         ELSE IF ((.NOT. LALL .AND. I .EQ. 1) .OR.
     &            (LSUN .AND. HCD .GE. -12.D0)) THEN
            WRITE (*, 9080) ONAME, SGHA, SDEC, SHC, ZND
            IF (LFILE) WRITE (10, 9080) ONAME, SGHA, SDEC, SHC, ZND
         END IF
         SD = 0.D0
   50 CONTINUE
C     GHA of the first point of Aries
      ONAME = 'ARIES'
      CALL DMSTR (GHAA, 1, 8, SGHA)
      WRITE (*, 9070) ONAME, SGHA
      IF (LFILE) WRITE (10, 9070) ONAME, SGHA
   90 CALL HITKEY
      RETURN
 9000 FORMAT (///////////////////////,/)
 9010 FORMAT (//A,$)
 9020 FORMAT (A)
 9030 FORMAT (/,9X,'Celestial Navigation Data for ',I4,1X,A3,1X,I2,
     &   ' at ',A8,' UT (GMT)'/30X,'Delta T = ',F5.1,' seconds '/
     &   17X,'For Assumed Location:  Longitude   ',A/
     &   40X,'Latitude    ',A,/)
 9040 FORMAT (T22,A,T53,A,T58,A)
 9050 FORMAT (' Object ',3X,'   GHA  ',3X,'   Dec  ',3X,'    Hc  ',3X,
     &   '  Zn ',2X,' ³ Refr','    SD ','    PA ','    Sum')
 9060 FORMAT (T15,A,T19,A,T26,A,T30,A,T37,A,T41,A,T49,A,T53,A,T57,A,
     &   T64,A,T71,A,T78,A)
 9070 FORMAT (1X,A,3(3X,A),3X,F5.1,T53,'³',F5.1,3F7.1)
 9080 FORMAT (1X,A,3(3X,A),3X,F5.1,T53,'³')
      END
