C=======================================================================
C  ICE 0.51  --  recovered source: F4 Physical Ephemeris (001C:1176)
C  This file is kept in UTF-8; the build converts it to code page 437.
C=======================================================================
C
C-----------------------------------------------------------------------
C  PHYSIC   [001C:1176]  F4: illumination (diameter, phase, defect,
C           magnitude) or rotation data (central meridians, sub-solar
C           and sub-earth points, pole) of a planet or the Moon
C-----------------------------------------------------------------------
      SUBROUTINE PHYSIC
      DOUBLE PRECISION DATE0, ALAT, ALONG, STEP
      INTEGER*4 NPOS, IZONE
      COMMON /SITE/ DATE0, ALAT, ALONG, STEP, NPOS, IZONE
      INTEGER*4 IACC
      LOGICAL LFILE
      COMMON /ACCUR/ IACC, LFILE
      DOUBLE PRECISION TJD, DT, TLT, VMAG, SBR, DE, DP, FILL, PHASE,
     &     DEFECT, CMLE, BE, CMLS, BS, DSUN, PASUN, DPOLE, PAPOLE, HOUR
      INTEGER*4 NUM, IERR, I, IY, IM, ID, IPASUN, IPAPOL
      CHARACTER*9 STIME
      CHARACTER*8 OBJ, ONAME
      CHARACTER*3 CMON
      CHARACTER*1 ANS
      SAVE
      IACC = 2
   10 WRITE (*, 9000)
      CALL CURON
      WRITE (*, 9010) ' Enter object name: '
      READ (*, 9020) OBJ
      CALL CUROFF
      IF (OBJ .EQ. ' ') RETURN
      CALL LOOKUP (OBJ, NUM, ONAME)
      IF (NUM .LE. 0 .OR. NUM .EQ. 10) THEN
         WRITE (*,*) 'Object must be a planet.'
         CALL HITKEY
         GO TO 10
      ELSE IF (NUM .EQ. 3) THEN
         WRITE (*,*) 'Cannot compute Physical Ephemeris of Earth.'
         CALL HITKEY
         GO TO 10
      ELSE IF (NUM .EQ. 99) THEN
         WRITE (*,*) 'Name not found.  Try again.'
         CALL HITKEY
         GO TO 10
      END IF
      CALL DATJD (DATE0, TJD)
      CALL DATJD (STEP, DT)
      WRITE (*, 9030) ' Choose: Illumination or Rotation: '
      CALL CURON
      READ (*, 9020) ANS
      CALL CUROFF
      WRITE (*, 9000)
      WRITE (*, 9040) 'Physical Ephemeris of ', ONAME
      IF (LFILE) WRITE (10, 9040) 'Physical Ephemeris of ', ONAME
      IF (ANS .EQ. 'i' .OR. ANS .EQ. 'I') THEN
         IF (NUM .EQ. 11) THEN
            WRITE (*, 9100) 'Date', 'Time', 'Diam', 'Phase', 'Angle',
     &         'Defect'
            WRITE (*, 9110) 'Julian Date', 'Year Mon Da', 'h  m  s',
     &         '"', 'ø', '"'
            IF (LFILE) THEN
               WRITE (10, 9100) 'Date', 'Time', 'Diam', 'Phase',
     &            'Angle', 'Defect'
               WRITE (10, 9110) 'Julian Date', 'Year Mon Da',
     &            'h  m  s', '"', 'ø', '"'
            END IF
         ELSE
            WRITE (*, 9120) 'Date', 'Time', 'Light', 'Vis Sq"',
     &         'Diam Phase Angle', 'Dfect'
            WRITE (*, 9130) 'Julian Date', 'Year Mon Da', 'h  m  s',
     &         'Min', 'Mag Mag', '"', 'ø', '"'
            IF (LFILE) THEN
               WRITE (10, 9120) 'Date', 'Time', 'Light', 'Vis Sq"',
     &            'Diam Phase Angle', 'Dfect'
               WRITE (10, 9130) 'Julian Date', 'Year Mon Da',
     &            'h  m  s', 'Min', 'Mag Mag', '"', 'ø', '"'
            END IF
         END IF
      ELSE
         WRITE (*, 9140) '³', 'Sub-Earth', '³', 'Sub-Solar', '³',
     &      'North Pole'
         WRITE (*, 9150) 'Date', 'Time', '³', 'Long.', 'Lat.', '³',
     &      'Long.', 'Lat.', 'Dist.', 'PA', '³', 'Dist.', 'PA'
         WRITE (*, 9160) 'Julian Date', 'Mon', 'Da', 'h  m  s', '³',
     &      'ø', 'ø', '³', 'ø', 'ø', '"', 'ø', '³', '"', 'ø'
         IF (LFILE) THEN
            WRITE (10, 9140) '³', 'Sub-Earth', '³', 'Sub-Solar', '³',
     &         'North Pole'
            WRITE (10, 9150) 'Date', 'Time', '³', 'Long.', 'Lat.',
     &         '³', 'Long.', 'Lat.', 'Dist.', 'PA', '³', 'Dist.', 'PA'
            WRITE (10, 9160) 'Julian Date', 'Mon', 'Da', 'h  m  s',
     &         '³', 'ø', 'ø', '³', 'ø', 'ø', '"', 'ø', '³', '"', 'ø'
         END IF
      END IF
C
      DO 50 I = 1, NPOS
         CALL PHYSEP (TJD, NUM, TLT, VMAG, SBR, DE, DP, FILL, PHASE,
     &      DEFECT, CMLE, BE, CMLS, BS, DSUN, PASUN, DPOLE, PAPOLE,
     &      IERR)
         IF (IERR .NE. 0) GO TO 90
         CALL CALDAT (TJD, IY, IM, ID, CMON, HOUR)
         CALL ANGSTR (HOUR, 3, 0, 2, 0, 9, STIME)
         IPAPOL = IDNINT (PAPOLE)
         IPASUN = IDNINT (PASUN)
         IF (ANS .EQ. 'i' .OR. ANS .EQ. 'I') THEN
            IF (NUM .EQ. 11) THEN
               WRITE (*, 9200) TJD, IY, CMON, ID, STIME, DE, FILL,
     &            PHASE, DEFECT
               IF (LFILE) WRITE (10, 9200) TJD, IY, CMON, ID, STIME,
     &            DE, FILL, PHASE, DEFECT
            ELSE
               WRITE (*, 9210) TJD, IY, CMON, ID, STIME, TLT, VMAG,
     &            SBR, DE, FILL, PHASE, DEFECT
               IF (LFILE) WRITE (10, 9210) TJD, IY, CMON, ID, STIME,
     &            TLT, VMAG, SBR, DE, FILL, PHASE, DEFECT
            END IF
         ELSE
            WRITE (*, 9220) TJD, CMON, ID, STIME, CMLE, BE, CMLS, BS,
     &         DSUN, IPASUN, DPOLE, IPAPOL
            IF (LFILE) WRITE (10, 9220) TJD, CMON, ID, STIME, CMLE, BE,
     &         CMLS, BS, DSUN, IPASUN, DPOLE, IPAPOL
         END IF
         TJD = TJD + DT
   50 CONTINUE
   90 CALL HITKEY
      RETURN
 9000 FORMAT (///////////////////////,/)
 9010 FORMAT (//A,$)
 9020 FORMAT (A)
 9030 FORMAT (A,$)
 9040 FORMAT (T25,A,A)
 9100 FORMAT (T13,A,T32,A,T42,A,T50,A,T57,A,T64,A)
 9110 FORMAT (T3,A,T17,A,T31,A,T45,A,T60,A,T68,A)
 9120 FORMAT (T13,A,T31,A,T39,A,T46,A,T55,A,T72,A)
 9130 FORMAT (T3,A,T17,A,T30,A,T40,A,T46,A,T56,A,T69,A,T74,A)
 9140 FORMAT (/,T31,A,T35,A,T45,A,T52,A,T70,A,A)
 9150 FORMAT (T13,A,T25,A,T31,A,T33,A,T41,A,A,T47,A,T55,A,T61,A,T68,A,
     &   A,T72,A,T79,A)
 9160 FORMAT (T3,A,T16,A,T20,A,T24,A,T31,A,T35,A,T42,A,T45,A,T49,A,
     &   T56,A,T63,A,T69,A,A,T74,A,T80,A)
 9200 FORMAT (1X,F13.5,I6,1X,A,I3,1X,A,F10.2,F7.3,F7.1,F9.2)
 9210 FORMAT (1X,F13.5,I6,1X,A,I3,A,F7.2,F5.1,F4.1,F6.2,F6.3,F6.1,F6.2)
 9220 FORMAT (1X,F13.5,1X,A,I3,A,5F7.2,I4,F7.2,I4)
      END
