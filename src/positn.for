C=======================================================================
C  ICE 0.51  --  recovered source: F3 Positions (001C:03A7) and the
C  Chebyshev coefficient listing POLYLS (06B6:23B9)
C  This file is kept in UTF-8; the build converts it to code page 437.
C=======================================================================
C
C-----------------------------------------------------------------------
C  POSITN   [001C:03A7]  F3: tables of apparent / astrometric places,
C           heliocentric ecliptic and barycentric rectangular
C           coordinates; Chebyshev coefficients to a file (unit 11)
C-----------------------------------------------------------------------
      SUBROUTINE POSITN
      DOUBLE PRECISION DATE0, ALAT, ALONG, STEP
      INTEGER*4 NPOS, IZONE
      COMMON /SITE/ DATE0, ALAT, ALONG, STEP, NPOS, IZONE
      INTEGER*4 IACC
      LOGICAL LFILE
      COMMON /ACCUR/ IACC, LFILE
      DOUBLE PRECISION TJD, DT, RA, DEC, DIS, HP, SD, VMAG, HOUR,
     &     POS(3), VEL(3), T, EPS, ELON, BLAT, DATE, TJD2
      INTEGER*4 I, ISCAN, NUM, IERR, IY, IM, ID, NUM2, N
      LOGICAL APPAR, LOPEN, LEX
      CHARACTER*44 PMENU(13)
      CHARACTER*40 FNAME
      CHARACTER*14 SRA, SDEC
      CHARACTER*9 STIME
      CHARACTER*8 OBJ, ONAME, NAME2, ONAM2
      CHARACTER*3 CMON, TSCALE
      SAVE
      DATA PMENU /
     & '╔════╤═════════════════════════════════════╗',
     & '║ F1 │ Apparent geocentric coordinates     ║',
     & '╟────┼─────────────────────────────────────╢',
     & '║ F2 │ Astrometric geocentric coordinates  ║',
     & '╟────┼─────────────────────────────────────╢',
     & '║ F3 │ Heliocentric coordinates            ║',
     & '╟────┼─────────────────────────────────────╢',
     & '║ F4 │ Barycentric rectangular coordinates ║',
     & '╟────┼─────────────────────────────────────╢',
     & '║ F5 │ Chebyshev coefficient output        ║',
     & '╟────┼─────────────────────────────────────╢',
     & '║ F6 │ Leave Position calculation          ║',
     & '╚════╧═════════════════════════════════════╝' /
      IACC = 1
   10 WRITE (*, 9010) PMENU
      WRITE (*, 9020)
   20 CALL GETKEY (I, ISCAN)
      IF (ISCAN .LT. 59 .OR. ISCAN .GT. 64) GO TO 20
      IF (ISCAN .EQ. 63) GO TO 500
      IF (ISCAN .EQ. 64) GO TO 900
C     object to be computed
   30 WRITE (*, 9000)
      CALL CURON
      WRITE (*, 9030) ' Enter object name: '
      READ (*, 9040) OBJ
      CALL CUROFF
      IF (OBJ .EQ. ' ') GO TO 900
      CALL LOOKUP (OBJ, NUM, ONAME)
      IF (NUM .EQ. 99) THEN
         WRITE (*,*) 'Name not found.  Try again.'
         CALL HITKEY
         GO TO 30
      END IF
      IF ((NUM .EQ. 3 .AND. (ISCAN .EQ. 59 .OR. ISCAN .EQ. 60)) .OR.
     &    (NUM .EQ. 10 .AND. ISCAN .EQ. 61)) THEN
         WRITE (*,*) 'Impossible coordinate request!  Try again.'
         CALL HITKEY
         WRITE (*, 9000)
         GO TO 10
      END IF
      IF (NUM .LE. 0 .AND. (ISCAN .EQ. 61 .OR. ISCAN .EQ. 62)) THEN
         WRITE (*,*) 'Choice is for solar system objects only.'
         CALL HITKEY
         GO TO 10
      END IF
      IF (ISCAN .LE. 60) THEN
         TSCALE = 'TDT'
      ELSE
         TSCALE = 'TDB'
      END IF
      CALL DATJD (DATE0, TJD)
      CALL DATJD (STEP, DT)
      GO TO (110, 120, 300, 400, 500), ISCAN - 58
C
C     F1, F2: apparent or astrometric geocentric places
  110 APPAR = .TRUE.
      GO TO 130
  120 APPAR = .FALSE.
  130 WRITE (*, 9000)
      IF (APPAR) THEN
         WRITE (*, 9100) 'Apparent Places of ', ONAME
         IF (LFILE) WRITE (10, 9100) 'Apparent Places of ', ONAME
      ELSE
         WRITE (*, 9110) 'Astrometric Places of ', ONAME
         IF (LFILE) WRITE (10, 9100) 'Astrometric Places of ', ONAME
      END IF
      IF (NUM .GE. 1) THEN
         WRITE (*, 9120)
         WRITE (*, 9130)
         IF (LFILE) THEN
            WRITE (10, 9120)
            WRITE (10, 9130)
         END IF
      ELSE
         WRITE (*, 9140)
         WRITE (*, 9150)
         IF (LFILE) THEN
            WRITE (10, 9140)
            WRITE (10, 9150)
         END IF
      END IF
      DO 150 I = 1, NPOS
         CALL PLACE (TJD, ISCAN - 58, NUM, RA, DEC, DIS, HP, SD, VMAG,
     &               IERR)
         IF (IERR .NE. 0) GO TO 800
         SRA = ' '
         SDEC = ' '
         STIME = ' '
         CALL ANGSTR (RA, 3, 0, 2, 3, 14, SRA)
         CALL ANGSTR (DEC, 4, 2, 3, 2, 14, SDEC)
         CALL CALDAT (TJD, IY, IM, ID, CMON, HOUR)
         CALL ANGSTR (HOUR, 3, 0, 2, 0, 9, STIME)
         IF (NUM .GT. 0) THEN
            WRITE (*, 9160) TJD, IY, CMON, ID, STIME, SRA, SDEC, DIS
         ELSE
            WRITE (*, 9160) TJD, IY, CMON, ID, STIME, SRA, SDEC
         END IF
         IF (LFILE) THEN
            IF (NUM .GT. 0) THEN
               WRITE (10, 9160) TJD, IY, CMON, ID, STIME, SRA, SDEC, DIS
            ELSE
               WRITE (10, 9160) TJD, IY, CMON, ID, STIME, SRA, SDEC
            END IF
         END IF
         TJD = TJD + DT
  150 CONTINUE
      GO TO 800
C
C     F3: heliocentric ecliptic coordinates of date
  300 WRITE (*, 9000)
      WRITE (*, 9300) 'Heliocentric Coordinates of ', ONAME
      WRITE (*, 9310)
      IF (LFILE) THEN
         WRITE (10, 9300) 'Heliocentric Coordinates of ', ONAME
         WRITE (10, 9310)
      END IF
      DO 350 I = 1, NPOS
         CALL SOLSYS (TJD, NUM, 1, POS, VEL, IERR)
         IF (IERR .NE. 0) GO TO 800
         CALL PRECES (2451545.D0, POS, TJD, VEL)
         T = (TJD - 2451545.D0) / 36525.D0
         EPS = (((0.001813D0*T - 0.00059D0)*T - 46.815D0)*T
     &         + 84381.448D0) / 3600.D0 * 0.017453292519943295D0
         POS(1) = VEL(1)
         POS(2) = DCOS (EPS) * VEL(2) + DSIN (EPS) * VEL(3)
         POS(3) = DCOS (EPS) * VEL(3) - DSIN (EPS) * VEL(2)
         DIS = DSQRT (POS(1)*POS(1) + POS(2)*POS(2) + POS(3)*POS(3))
         BLAT = DASIN (POS(3) / DIS)
         ELON = DATAN2 (POS(2), POS(1))
         SRA = ' '
         SDEC = ' '
         STIME = ' '
         CALL ANGSTR (ELON, 2, 0, 3, 1, 14, SRA)
         CALL ANGSTR (BLAT, 2, 2, 3, 1, 14, SDEC)
         CALL CALDAT (TJD, IY, IM, ID, CMON, HOUR)
         CALL ANGSTR (HOUR, 3, 0, 2, 0, 9, STIME)
         WRITE (*, 9160) TJD, IY, CMON, ID, STIME, SRA, SDEC, DIS
         IF (LFILE)
     &      WRITE (10, 9160) TJD, IY, CMON, ID, STIME, SRA, SDEC, DIS
         TJD = TJD + DT
  350 CONTINUE
      GO TO 800
C
C     F4: barycentric rectangular coordinates (J2000.0)
  400 WRITE (*, 9000)
      WRITE (*, 9400) 'Barycentric Coordinates of ', ONAME
      WRITE (*, 9410)
      IF (LFILE) THEN
         WRITE (10, 9400) 'Barycentric Coordinates of ', ONAME
         WRITE (10, 9410)
      END IF
      DO 450 I = 1, NPOS
         CALL SOLSYS (TJD, NUM, 0, POS, VEL, IERR)
         IF (IERR .NE. 0) GO TO 800
         CALL CALDAT (TJD, IY, IM, ID, CMON, HOUR)
         CALL ANGSTR (HOUR, 3, 0, 2, 0, 9, STIME)
         WRITE (*, 9420) TJD, IY, CMON, ID, STIME, POS
         IF (LFILE) WRITE (10, 9420) TJD, IY, CMON, ID, STIME, POS
         TJD = TJD + DT
  450 CONTINUE
      GO TO 800
C
C     F5: Chebyshev coefficients of one body to a file on unit 11
  500 WRITE (*, 9000)
      CALL CURON
  510 WRITE (*,*) 'Enter JD or YYYYMMDD.hhmmss you want coefficients',
     &            ' for'
      READ (*, *, ERR=530) DATE
      CALL DATJD (DATE, TJD2)
      WRITE (*, 9000)
  520 WRITE (*,*) 'Enter object name: '
      READ (*, 9040) NAME2
      IF (NAME2 .NE. ' ') GO TO 540
      GO TO 900
  530 CALL BADCHR
      GO TO 510
  540 CALL LOOKUP (NAME2, NUM2, ONAM2)
      IF (NUM2 .EQ. 99) THEN
         WRITE (*,*) 'Name not found.  Try again.'
         GO TO 520
      END IF
      WRITE (*, 9000)
      INQUIRE (11, OPENED=LOPEN, NAME=FNAME)
      IF (LOPEN) GO TO 590
      WRITE (*, 9030) ' Enter name of output file: '
      CALL CURON
      READ (*, 9040) FNAME
      CALL CUROFF
      IF (FNAME .EQ. ' ') GO TO 900
      INQUIRE (FILE=FNAME, EXIST=LEX)
      IF (LEX) THEN
         N = INDEX (FNAME, '  ')
         WRITE (*,*) 'Output file ', FNAME(1:N), 're - opened.'
         WRITE (*,*) 'Data will be appended to bottom of file.'
         OPEN (11, FILE=FNAME, STATUS='OLD')
  550    READ (11, 9500, END=560)
         GO TO 550
  560    BACKSPACE 11
      ELSE
         IF (LEX) GO TO 590
         N = INDEX (FNAME, '  ')
         WRITE (*,*) 'Output file ', FNAME(1:N), 'created.'
         OPEN (11, FILE=FNAME, STATUS='NEW')
      END IF
  590 CALL POLYLS (NUM2, TJD2)
      CLOSE (11)
      WRITE (*, 9510)
  800 CALL HITKEY
  900 RETURN
 9000 FORMAT (///////////////////////,/)
 9010 FORMAT (T19,A)
 9020 FORMAT (///////)
 9030 FORMAT (//A,$)
 9040 FORMAT (A)
 9100 FORMAT (T28,A,A)
 9110 FORMAT (T26,A,A)
 9120 FORMAT (T13,'Date',T31,'Time',T44,'RA',T59,'Dec',T71,'Dist')
 9130 FORMAT (T3,'Julian Date   Year Mon Da  h  m  s    h  m   s',
     &   T57,'°  ''   "       AU')
 9140 FORMAT (T13,'Date',T31,'Time',T43,'RA',T59,'Dec')
 9150 FORMAT (T3,'Julian Date   Year Mon Da  h  m  s    h  m   s',
     &   T57,'°  ''   "')
 9160 FORMAT (1X,F13.5,I6,1X,A,I3,A,1X,A,1X,A,F12.7)
 9300 FORMAT (T23,A,A)
 9310 FORMAT (T13,'Date',T31,'Time',T43,'Long',T59,'Lat',T71,'Dist'/
     &   T3,'Julian Date   Year Mon Da  h  m  s',T43,'°  ''   "',
     &   T58,'°  ''   "',T72,'AU')
 9400 FORMAT (T23,A,A)
 9410 FORMAT (T13,'Date',T31,'Time',T46,'X',T60,'Y',T74,'Z'/
     &   T3,'Julian Date   Year Mon Da  h  m  s',T46,'au',T60,'au',
     &   T74,'au')
 9420 FORMAT (1X,F13.5,I6,1X,A,I3,A,3F14.10)
 9500 FORMAT (1X)
 9510 FORMAT (/)
      END
C
C-----------------------------------------------------------------------
C  POLYLS   [06B6:23B9]  list the Chebyshev coefficients of body NB for
C           the ephemeris interval containing TJD on unit 11
C-----------------------------------------------------------------------
      SUBROUTINE POLYLS (NB, TJD)
      INTEGER*4 NB
      DOUBLE PRECISION TJD
      DOUBLE PRECISION T0, PER(11), PB(11), PC(11), E1(4), E2(4),
     &     SC(11), E3(5), F1(5), F2(5), CA(42,5), CB(42,5), START(11),
     &     SPAN(11), XM1, XM2, XM3, XM4, CE, SE
      INTEGER*4 NCREF(5), NX(11), IREC(11)
      COMMON /EPHPAR/ T0, PER, PB, PC, E1, E2, SC, E3, F1, F2, CA, CB,
     &     START, SPAN, XM1, XM2, XM3, XM4, CE, SE, NCREF, NX, IREC
      DOUBLE PRECISION TB(2), TE(2), XMU, EMRAT, CC(41,3,11)
      INTEGER*4 IDATE(30), JHDR(11,2), IDAT(10000,2), IPTR(400,2),
     &     IHDR(21,2), NLAST(11), NREF, IB
      LOGICAL FIRST
      COMMON /EPHBUF/ TB, TE, XMU, EMRAT, CC, IDATE, JHDR, IDAT, IPTR,
     &     IHDR, NLAST, NREF, IB, FIRST
      DOUBLE PRECISION POS(3), VEL(3), TFIRST, TLAST, TMID, HALF
      INTEGER*4 IERR, I, N
      CHARACTER*25 BNAME(11)
      SAVE
      DATA BNAME / ' MERCURY.', ' VENUS.', ' EARTH - MOON BARYCENTER.',
     &     ' MARS.', ' JUPITER.', ' SATURN.', ' URANUS.', ' NEPTUNE.',
     &     ' PLUTO.', ' SUN.', ' MOON RELATIVE TO EARTH.' /
      CALL SOLSYS (TJD, NB, 0, POS, VEL, IERR)
      TFIRST = START(NB)
      HALF = SPAN(NB) * 0.5D0
      TMID = HALF + TFIRST
      TLAST = TMID + HALF
      IF (NB .NE. 11) HALF = 1.1D0 * HALF
      WRITE (11, 9000) BNAME(NB)
      WRITE (11, 9010)
      WRITE (11, 9020) TFIRST
      WRITE (11, 9030) TLAST
      WRITE (11, 9040) TMID
      WRITE (11, 9050) HALF
      WRITE (11, 9060)
      N = NLAST(NB)
      WRITE (11, 9070) (I-1, CC(I,1,NB), CC(I,2,NB), CC(I,3,NB),
     &                  I = 1, N)
      IF (NB .EQ. 3 .OR. NB .EQ. 11) WRITE (11, 9080)
      RETURN
 9000 FORMAT (//1X,'CHEBYSHEV POLYNOMIAL EXPANSION OF POSITION OF',A25)
 9010 FORMAT (/,1X,'POSITIONS OF OBJECTS ARE J2000.0 EQUATORIAL ',
     &   'RECTANGULAR COORDINATES'/1X,'RELATIVE TO THE SOLAR SYSTEM ',
     &   'BARYCENTER, AS USED IN ','JPL EPHEMERIS DE200.')
 9020 FORMAT (//1X,'FIRST JULIAN DATE FOR WHICH POLYNOMIAL IS TO BE ',
     &   'USED=',D25.15)
 9030 FORMAT (1X,'LAST JULIAN DATE FOR WHICH POLYNOMIAL IS TO BE USED=',
     &   D25.15)
 9040 FORMAT (1X,'MIDDLE JULIAN DATE OF EXPANSION INTERVAL=',D25.15)
 9050 FORMAT (1X,'HALF WIDTH OF EXPANSION INTERVAL=',D25.15,' DAYS')
 9060 FORMAT (//22X,'J2000.0 EQUATORIAL RECTANGULAR COORDINATE'/
     &   '   DEGREE  ',11X,'X',22X,'Y',22X,'Z')
 9070 FORMAT (1X,I5,4X,3D23.15)
 9080 FORMAT (//' RATIO OF MASS OF MOON TO MASS OF EARTH IS 0.01230002')
      END
