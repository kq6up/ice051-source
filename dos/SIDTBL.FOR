C=======================================================================
C  ICE 0.51  --  recovered source: F5 Sidereal Time (001C:197A)
C=======================================================================
C
C-----------------------------------------------------------------------
C  SIDTBL   [001C:197A]  F5: table of Greenwich apparent and mean and
C           local apparent sidereal time
C-----------------------------------------------------------------------
      SUBROUTINE SIDTBL
      DOUBLE PRECISION DATE0, ALAT, ALONG, STEP
      INTEGER*4 NPOS, IZONE
      COMMON /SITE/ DATE0, ALAT, ALONG, STEP, NPOS, IZONE
      INTEGER*4 IACC
      LOGICAL LFILE
      COMMON /ACCUR/ IACC, LFILE
      DOUBLE PRECISION TJD, DT, TJDH, TJDL, GMST, GAST, LAST, HOUR
      INTEGER*4 I, IY, IM, ID
      CHARACTER*14 SGAST, SLAST
      CHARACTER*13 SGMST
      CHARACTER*10 CLONG, CLAT
      CHARACTER*9 STIME
      CHARACTER*3 CMON
      SAVE
      IACC = 1
      CALL DATJD (DATE0, TJD)
      CALL DATJD (STEP, DT)
      CALL LATLON (ALONG, 0.D0, CLONG, CLAT)
      WRITE (*, 9000)
      WRITE (*, 9010) '* * Sidereal Times for Longitude ', CLONG, ' * *'
      WRITE (*, 9020) 'Greenwich Sidereal Times', 'Local Apparent'
      WRITE (*, 9030) 'Date', 'Time', 'Apparent', 'Mean',
     &   'Sidereal Time'
      WRITE (*, 9040) 'Julian Date', 'Year', 'Mon', 'Da', 'h  m  s',
     &   'h  m   s', 'h  m   s', 'h  m   s'
      IF (LFILE) THEN
         WRITE (10, 9010) '* * Sidereal Times for Longitude ', CLONG,
     &      ' * *'
         WRITE (10, 9020) 'Greenwich Sidereal Times', 'Local Apparent'
         WRITE (10, 9030) 'Date', 'Time', 'Apparent', 'Mean',
     &      'Sidereal Time'
         WRITE (10, 9040) 'Julian Date', 'Year', 'Mon', 'Da', 'h  m  s',
     &      'h  m   s', 'h  m   s', 'h  m   s'
      END IF
      DO 10 I = 1, NPOS
         CALL CALDAT (TJD, IY, IM, ID, CMON, HOUR)
         TJDH = DINT (TJD)
         TJDL = TJD - DINT (TJD)
         CALL SIDTIM (TJDH, TJDL, 0, GMST)
         CALL SIDTIM (TJDH, TJDL, 1, GAST)
         LAST = ALONG / 15.D0 + GAST
         CALL ANGSTR (GMST, 3, 0, 2, 4, 13, SGMST)
         CALL ANGSTR (GAST, 3, 0, 2, 4, 14, SGAST)
         CALL ANGSTR (LAST, 3, 0, 2, 4, 14, SLAST)
         CALL ANGSTR (HOUR, 3, 0, 2, 0, 9, STIME)
         WRITE (*, 9050) TJD, IY, CMON, ID, STIME, SGAST, SGMST, SLAST
         IF (LFILE)
     &      WRITE (10, 9050) TJD, IY, CMON, ID, STIME, SGAST, SGMST,
     &                       SLAST
         TJD = DT + TJD
   10 CONTINUE
      CALL HITKEY
      RETURN
 9000 FORMAT (///////////////////////,/)
 9010 FORMAT (T18,A,A,A)
 9020 FORMAT (/,T41,A,T67,A)
 9030 FORMAT (T13,A,T31,A,T42,A,T56,A,T68,A)
 9040 FORMAT (T3,A,T17,A,T22,A,T26,A,T30,A,T40,A,T54,A,T69,A)
 9050 FORMAT (1X,F13.5,I6,1X,A,I3,A,1X,A,1X,A,1X,A)
      END
