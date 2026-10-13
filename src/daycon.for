C=======================================================================
C  ICE 0.51  --  recovered source: F8 Daily Configuration (001C:27AB)
C  This file is kept in UTF-8; the build converts it to code page 437.
C=======================================================================
C
C-----------------------------------------------------------------------
C  DAYCON   [001C:27AB]  F8: geocentric configuration of the Sun, Moon
C           and planets for one instant, sorted by elongation:
C           position, hour-angle difference and elongation from the
C           Sun, magnitude, diameter, illuminated fraction, Moon phase
C-----------------------------------------------------------------------
      SUBROUTINE DAYCON
      DOUBLE PRECISION DATE0, ALAT, ALONG, STEP
      INTEGER*4 NPOS, IZONE
      COMMON /SITE/ DATE0, ALAT, ALONG, STEP, NPOS, IZONE
      INTEGER*4 IACC
      LOGICAL LFILE
      COMMON /ACCUR/ IACC, LFILE
      DOUBLE PRECISION SINEPS, COSEPS, TJD, HOUR, RAH, DECD, DIS, HP,
     &     SD, VM, SINRA, SINDEC, COSDEC, SINDS, COSDS, DDEC, DRA, CE,
     &     SINB, COSB, C, T, TLT, SBR, DP, PHASE, DEFECT, CMLE, BE,
     &     CMLS, BS, DSUN, PASUN, DPOLE, PAPOLE
      DOUBLE PRECISION RA(9), DEC(9), DIST(9), ELONG(9), HADIF(9),
     &     PHMOON(9), VMAGS(9), DIAMS(9), FILLS(9)
      INTEGER*4 I, J, K1, K2, NUM, IERR, IY, IM, ID, ISORT(9), IEL,
     &     IDIA, IFILL
      LOGICAL LSORT
      CHARACTER*11 PHTXT
      CHARACTER*8 OBJ, ONAME, NAMES(9), STIME, SRA, SDEC, SHA
      CHARACTER*3 CMON
      CHARACTER*2 BODY(9), FLAG(9)
      SAVE
      DATA BODY / 'SU', 'MO', 'ME', 'VE', 'MA', 'JU', 'SA', 'UR', 'NE' /
      IACC = 2
      SINEPS = DSIN (0.40909280420293637D0)
      COSEPS = DCOS (0.40909280420293637D0)
      WRITE (*, 9000)
      CALL DATJD (DATE0, TJD)
      CALL CALDAT (TJD, IY, IM, ID, CMON, HOUR)
      CALL ANGSTR (HOUR, 3, 0, 2, 0, 8, STIME)
      WRITE (*, 9010) IY, CMON, ID, STIME
      WRITE (*, 9020)
      WRITE (*, 9030) 'h  m  s', '°', '''', 'AU', 'h  m', '°', '"'
      IF (LFILE) THEN
         WRITE (10, 9010) IY, CMON, ID, STIME
         WRITE (10, 9020)
         WRITE (10, 9030) 'h  m  s', '°', '''', 'AU', 'h  m', '°', '"'
      END IF
C
      DO 20 I = 1, 9
         OBJ = BODY(I)
         CALL LOOKUP (OBJ, NUM, ONAME)
         IF (NUM .EQ. 99) GO TO 20
         CALL PLACE (TJD, 1, NUM, RAH, DECD, DIS, HP, SD, VM, IERR)
         IF (IERR .NE. 0) GO TO 90
         NAMES(I) = ONAME
         RA(I) = RAH * 0.2617993877991494D0
         DEC(I) = DECD * 0.017453292519943295D0
         DIST(I) = DIS
         SINRA = DSIN (RA(I))
         SINDEC = DSIN (DEC(I))
         COSDEC = DCOS (DEC(I))
         IF (I .LE. 1) THEN
C           the Sun
            SINDS = SINDEC
            COSDS = COSDEC
            ELONG(I) = 0.D0
            HADIF(I) = 0.D0
            PHMOON(I) = 0.D0
            VMAGS(I) = 0.D0
            DIAMS(I) = 0.D0
            FILLS(I) = 0.D0
            GO TO 20
         END IF
C        elongation from the Sun, east or west (north or south if
C        close to the Sun and mostly in declination)
         DDEC = DEC(1) - DEC(I)
         DRA = RA(1) - RA(I)
         IF (DRA .GT. 3.141592653589793D0)
     &      DRA = DRA - 6.283185307179586D0
         IF (DRA .LT. -3.141592653589793D0)
     &      DRA = 6.283185307179586D0 + DRA
         CE = DCOS (DRA) * COSDEC * COSDS + SINDEC * SINDS
         ELONG(I) = DACOS (CE) * 57.29577951308232D0
         FLAG(I) = 'W'
         IF (DRA .LT. 0.D0) FLAG(I) = 'E'
         IF (ELONG(I) .LT. 10.D0 .AND.
     &       DABS (DRA) * COSDS .LT. DABS (DDEC)) THEN
            FLAG(I) = 'S'
            IF (DDEC .LT. 0.D0) FLAG(I) = 'N'
         END IF
         IF (DRA .LT. 0.D0) ELONG(I) = -ELONG(I)
         HADIF(I) = DRA * 57.29577951308232D0 / 15.D0
         PHMOON(I) = 0.D0
         IF (I .EQ. 2) THEN
C           Moon: elongation in ecliptic longitude
            SINB = -(COSDEC * SINRA * SINEPS) + SINDEC * COSEPS
            COSB = DSQRT (1.D0 - SINB * SINB)
            C = CE / COSB
            IF (C .LT. -1.D0) C = -1.D0
            IF (C .GT. 1.D0) C = 1.D0
            PHMOON(I) = DACOS (C) * 57.29577951308232D0
            IF (DRA .LT. 0.D0) PHMOON(I) = -PHMOON(I)
         END IF
         CALL PHYSEP (TJD, NUM, TLT, VMAGS(I), SBR, DIAMS(I), DP,
     &      FILLS(I), PHASE, DEFECT, CMLE, BE, CMLS, BS, DSUN, PASUN,
     &      DPOLE, PAPOLE, IERR)
   20 CONTINUE
C
C     sort by elongation, east to west
      DO 30 J = 1, 9
         ISORT(J) = J
   30 CONTINUE
   40 LSORT = .TRUE.
      DO 50 J = 1, 8
         K1 = ISORT(J)
         K2 = ISORT(J+1)
         IF (ELONG(K1) .LT. ELONG(K2)) THEN
            LSORT = .FALSE.
            ISORT(J) = K2
            ISORT(J+1) = K1
         END IF
   50 CONTINUE
      IF (.NOT. LSORT) GO TO 40
C
      DO 80 J = 1, 9
         I = ISORT(J)
         CALL ANGSTR (RA(I), 1, 0, 2, 0, 8, SRA)
         T = DEC(I) * 57.29577951308232D0
         CALL DMSTR (T, 3, 8, SDEC)
         CALL ANGSTR (HADIF(I), 3, 2, 2, -1, 8, SHA)
         IEL = INT (DABS (ELONG(I)) + 0.5D0)
         IF (IEL .LT. 1 .OR. IEL .GT. 179) FLAG(I) = ' '
         IDIA = INT (DIAMS(I) + 0.5D0)
         IFILL = IDNINT (FILLS(I) * 100.D0)
         IF (NAMES(I) .EQ. 'SUN') THEN
            WRITE (*, 9040) NAMES(I), SRA, SDEC, DIST(I)
            IF (LFILE) WRITE (10, 9040) NAMES(I), SRA, SDEC, DIST(I)
         ELSE IF (NAMES(I) .EQ. 'MOON') THEN
            PHTXT = '   (FULL)  '
            IF (DABS (PHMOON(I)) .LT. 177.D0) PHTXT = ' (GIBBOUS) '
            IF (DABS (PHMOON(I)) .LT. 90.D0) PHTXT = ' (CRESCENT)'
            IF (DABS (PHMOON(I)) .LE. 3.D0) PHTXT = '   (NEW)   '
            IF (DABS (PHMOON(I) - 90.D0) .LE. 3.D0)
     &         PHTXT = ' (LAST QTR)'
            IF (DABS (PHMOON(I) + 90.D0) .LE. 3.D0)
     &         PHTXT = '(FIRST QTR)'
            WRITE (*, 9040) NAMES(I), SRA, SDEC, DIST(I), SHA, FLAG(I),
     &         IEL, VMAGS(I), IDIA, IFILL, PHTXT
            IF (LFILE) WRITE (10, 9040) NAMES(I), SRA, SDEC, DIST(I),
     &         SHA, FLAG(I), IEL, VMAGS(I), IDIA, IFILL, PHTXT
         ELSE
            WRITE (*, 9040) NAMES(I), SRA, SDEC, DIST(I), SHA, FLAG(I),
     &         IEL, VMAGS(I), IDIA, IFILL
            IF (LFILE) WRITE (10, 9040) NAMES(I), SRA, SDEC, DIST(I),
     &         SHA, FLAG(I), IEL, VMAGS(I), IDIA, IFILL
         END IF
   80 CONTINUE
   90 CALL HITKEY
      RETURN
 9000 FORMAT (///////////////////////,/)
 9010 FORMAT (11X,'Geocentric Configurations for ',I4,1X,A3,1X,I2,
     &   ' AT ',A8,' TDT')
 9020 FORMAT (44X,'Position wrt Sun',3X,'(Moon Phase)'/' Object ',6X,
     &   'RA',9X,'Dec',7X,'Dist',6X,'HA Diff',3X,'Elong',5X,'Mag',2X,
     &   'Dia',3X,'%Il')
 9030 FORMAT (T14,A,T26,A,T30,A,T37,A,T49,A,T60,A,T73,A)
 9040 FORMAT (1X,A8,2(3X,A8),3X,F7.4,:,3X,A8,3X,A1,I4,4X,F4.1,3X,I2,3X,
     &   I3,T63,A11)
      END
