C=======================================================================
C  ICE 0.51  --  recovered source: star catalogs (segment 0917 and
C  helpers in 06B6)
C=======================================================================
C
C-----------------------------------------------------------------------
C  CATCHG   [0917:0001]  F9: open a catalog file (unit 11), load the
C           first page of up to 200 objects, optionally list it
C           IERR = 0 ok, 5 open error, 7 bad header, 8 read error
C-----------------------------------------------------------------------
      SUBROUTINE CATCHG (IERR, LFILE)
      INTEGER*4 IERR
      LOGICAL LFILE
      INCLUDE 'catlg.inc'
      CHARACTER*16 CFILE, CATFIL(10), SRA, SDEC
      CHARACTER*8 CCNT, C1, C2, C3
      CHARACTER*60 CATID
      CHARACTER*1 ANS
      DOUBLE PRECISION RA, DEC
      INTEGER*4 IVAL, NREC, IOS, N, I, IR
      SAVE
      DATA CATFIL / 'STAR1.CAT', 'STAR1.CAT', 'RADIO1.CAT',
     &     'MESSIER.CAT', 6*'$USER.CAT' /
      IERR = 0
      CALL CURON
      WRITE (*, 9000) ' Enter catalog number or file name of catalog: '
      READ (*, 9010) CFILE
      CALL CUROFF
      IF (CFILE .EQ. ' ') RETURN
      ICAT = IVAL (CFILE)
      IF (ICAT .GT. 9) ICAT = 10
      IF (ICAT .GE. 1) CFILE = CATFIL(ICAT)
      IF (ICAT .LE. 0) ICAT = 99
      OPEN (11, FILE=CFILE, ACCESS='DIRECT', FORM='BINARY', RECL=68,
     &      STATUS='OLD', IOSTAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE (*, 9020)
         WRITE (*, 9030) ' Error opening catalog file: ', CFILE
         WRITE (*, 9020)
         IERR = 5
         RETURN
      END IF
      WRITE (*, 9040) ' Successfully opened catalog file: ', CFILE
      READ (11, REC=1, IOSTAT=IOS) CCNT, CATID
      NREC = IVAL (CCNT)
      IF (IOS .NE. 0 .OR. NREC .LT. 1 .OR. CATID .EQ. ' ') THEN
         WRITE (*, 9020)
         WRITE (*, 9030) ' Error in header record in catalog file: ',
     &                   CFILE
         WRITE (*, 9020)
         IERR = 7
         RETURN
      END IF
C     catalog 1 is the 200 brightest-star subset of STAR1.CAT
      IF (ICAT .EQ. 1) NREC = 200
      WRITE (*, 9050) ' Catalog ID is: ', CATID,
     &   ' Number of data records to be processed is: ', NREC
      N = NREC
      IF (N .GT. 200) N = 200
      DO 10 I = 1, N
         IR = I + 1
         READ (11, REC=IR, IOSTAT=IOS) CNAM(I,1), CNAM(I,2), CNAM(I,3),
     &      CRA(I), CDEC(I), CPMRA(I), CPMDEC(I), CPARX(I), CRV(I),
     &      CMAG(I)
         IF (IOS .NE. 0) GO TO 20
   10 CONTINUE
      IFIRST = 1
      NSTAR = NREC
      ILOW = 1
      IHIGH = IR - 1
      WRITE (*, 9060) ' Data for ', IHIGH, ' objects now in memory.'
      GO TO 30
   20 WRITE (*, 9020)
      WRITE (*, 9070) 'Error at record ', IR, ' in catalog file ', CFILE
      WRITE (*, 9020)
      IFIRST = 1
      NSTAR = NREC
      ILOW = 1
      IHIGH = IR - 2
      IF (IHIGH .LT. 1) IHIGH = 1
      WRITE (*, 9060) ' Data for ', IHIGH, ' Objects now in memory.'
      IERR = 8
   30 CALL CURON
      WRITE (*, 9000)
     &   ' Display list of all objects in catalog? [Yes / No]: '
      READ (*, 9010) ANS
      CALL CUROFF
      IF (ANS .NE. 'Y' .AND. ANS .NE. 'y') RETURN
      WRITE (*, 9080) CFILE
      N = NSTAR - IFIRST + 1
      IF (LFILE) WRITE (10, 9010) CFILE
      DO 40 I = 1, N
         IR = I + 1
         READ (11, REC=IR, IOSTAT=IOS) C1, C2, C3, RA, DEC
         CALL ANGSTR (RA, 3, 0, 2, 4, 16, SRA)
         CALL ANGSTR (DEC, 4, 2, 2, 3, 16, SDEC)
         WRITE (*, 9090) I, C1, C2, C3, SRA, SDEC
         IF (LFILE) WRITE (10, 9090) I, C1, C2, C3, SRA, SDEC
   40 CONTINUE
      CALL HITKEY
      RETURN
 9000 FORMAT (A,$)
 9010 FORMAT (A)
 9020 FORMAT (15('****'))
 9030 FORMAT (5X,A,A)
 9040 FORMAT (A,A)
 9050 FORMAT (A,A/A,I5)
 9060 FORMAT (A,I3,A,/)
 9070 FORMAT (6X,A,I3,A,A)
 9080 FORMAT (23X,'Catalog File ',A16//,1X,'Number',3X,'Name 1',5X,
     &   'Name 2',5X,'Name 3',6X,'RA (J2000.0)',3X,'DEC (J2000.0)',/)
 9090 FORMAT (I6,3(3X,A),2A)
      END
C
C-----------------------------------------------------------------------
C  NXTCAT   [06B6:1D77]  load the next page (200 objects) of a large
C           catalog, wrapping round to the start; IERR = 8 on read error
C-----------------------------------------------------------------------
      SUBROUTINE NXTCAT (IERR)
      INTEGER*4 IERR
      INCLUDE 'catlg.inc'
      INTEGER*4 N, M, I, IR, IOS
      SAVE
      IERR = 0
      N = NSTAR - IFIRST + 1
      IF (N .LE. 200) RETURN
      M = NSTAR - IHIGH
      IF (M .GT. 200) M = 200
      IF (M .LE. 0) THEN
         M = 200
         IHIGH = 0
      END IF
      DO 10 I = 1, M
         IR = IHIGH + I + 1
         READ (11, REC=IR, IOSTAT=IOS) CNAM(I,1), CNAM(I,2), CNAM(I,3),
     &      CRA(I), CDEC(I), CPMRA(I), CPMDEC(I), CPARX(I), CRV(I),
     &      CMAG(I)
         IF (IOS .NE. 0) GO TO 20
   10 CONTINUE
      ILOW = IHIGH + 1
      IHIGH = IR - 1
      RETURN
   20 WRITE (*, 9000) IR
      ILOW = IHIGH + 1
      IHIGH = IR - 2
      IF (IHIGH .LT. ILOW) IHIGH = ILOW
      IERR = 8
      RETURN
 9000 FORMAT (15('****')/5X,'Error at record ',I5,' in catalog file'/
     &   15('****'))
      END
C
C-----------------------------------------------------------------------
C  LOOKUP   [0917:08CF]  identify an object typed by the user: a catalog
C           number, a planet name or a catalog name (abbreviations
C           allowed).  NUM = 1..11 body number, -K = K-th object of the
C           page in memory, 99 = not found; OUTNAM = name found
C-----------------------------------------------------------------------
      SUBROUTINE LOOKUP (NAME, NUM, OUTNAM)
      CHARACTER*8 NAME, OUTNAM
      INTEGER*4 NUM
      INCLUDE 'catlg.inc'
      CHARACTER*8 PLNAM(11)
      INTEGER*4 IPLNUM(11)
      INTEGER*4 NBLANK, IVAL, MATCH, NC, N, I, J, K, M, NN, NPAGE,
     &     IPAGE, IERR
      SAVE
      DATA PLNAM / 'SUN', 'MOON', 'VENUS', 'JUPITER', 'SATURN',
     &     'MERCURY', 'MARS', 'URANUS', 'NEPTUNE', 'PLUTO', 'EARTH' /
      DATA IPLNUM / 10, 11, 2, 5, 6, 1, 4, 7, 8, 9, 3 /
      NUM = 99
      OUTNAM = ' '
      NC = NBLANK (NAME)
      IF (NC .EQ. 0) RETURN
      N = IVAL (NAME)
      NPAGE = INT ((FLOAT (NSTAR - IFIRST) + 1.0) / 200.0 + 0.999)
C     object number: page through the catalog until it is in memory
      IF (N .GE. IFIRST .AND. N .LE. NSTAR) THEN
         IPAGE = 1
   10    IF (N .GE. ILOW .AND. N .LE. IHIGH) THEN
            K = N - ILOW + 1
            NUM = -K
            OUTNAM = CNAM(K,1)
            RETURN
         END IF
         IF (IPAGE .NE. NPAGE) THEN
            CALL NXTCAT (IERR)
            IPAGE = IPAGE + 1
            GO TO 10
         END IF
      END IF
C     planet names
      DO 20 I = 1, 11
         IF (MATCH (NAME, PLNAM(I)) .EQ. NC) THEN
            NUM = IPLNUM(I)
            OUTNAM = PLNAM(I)
            RETURN
         END IF
   20 CONTINUE
C     catalog names, page by page
      IPAGE = 1
   30 NN = IHIGH - ILOW + 1
      DO 50 I = 1, NN
         DO 40 J = 1, 3
            M = MATCH (NAME, CNAM(I,J))
            IF (M .EQ. NC) GO TO 60
            IF (M .GE. 6 .AND. NC .GE. 7) THEN
               IF (M .EQ. NBLANK (CNAM(I,J))) GO TO 60
            END IF
   40    CONTINUE
   50 CONTINUE
      IF (IPAGE .EQ. NPAGE) RETURN
      CALL NXTCAT (IERR)
      IPAGE = IPAGE + 1
      GO TO 30
   60 NUM = -I
      OUTNAM = CNAM(I,J)
      RETURN
      END
C
C-----------------------------------------------------------------------
C  DMSTR    [06B6:2015]  edit an angle X (degrees) as 'ddd mm.m' into
C           the last 8 characters of STR(1:LEN), blank filled.
C           MODE 1: no sign, X reduced to 0..360;  MODE 2: n/s;
C           MODE 3: +/-.   LEN outside 8..40 gives asterisks.
C-----------------------------------------------------------------------
      SUBROUTINE DMSTR (X, MODE, LEN, STR)
      DOUBLE PRECISION X
      INTEGER*4 MODE, LEN
      CHARACTER*(*) STR
      DOUBLE PRECISION V, FRAC
      INTEGER*4 IDEG, IMIN, N, I
      CHARACTER*20 FMT
      CHARACTER*8 WORK
      CHARACTER*1 SGN
      SAVE
      IF (LEN .LT. 8 .OR. LEN .GT. 40) GO TO 90
      V = X * 60.D0
      IF (MODE .LT. 2) THEN
         V = DMOD (V, 21600.D0)
         IF (V .LT. 0.D0) V = 21600.D0 + V
      END IF
      V = DNINT (V * 10.D0) / 10.D0 * 1.000000000000001D0
      SGN = ' '
      IF (MODE .EQ. 1) GO TO 10
      IF (V .EQ. 0.D0) GO TO 10
      IF (MODE .EQ. 2) THEN
         SGN = 'n'
         IF (V .LT. 0.D0) SGN = 's'
      ELSE IF (MODE .EQ. 3) THEN
         SGN = '+'
         IF (V .LT. 0.D0) SGN = '-'
      END IF
   10 V = DABS (V)
      IDEG = INT (V / 60.D0)
      FRAC = V - DBLE (IDEG) * 60.D0
      IMIN = INT (FRAC)
      FRAC = FRAC - DBLE (IMIN)
      IF (MODE .EQ. 1) FMT = '(   i3,1x,i2.2,f2.1)'
      IF (MODE .GE. 2) FMT = '(a1,i2,1x,i2.2,f2.1)'
      IF (MODE .EQ. 1) THEN
         WRITE (WORK, FMT) IDEG, IMIN, FRAC
      ELSE
         WRITE (WORK, FMT) SGN, IDEG, IMIN, FRAC
      END IF
      N = LEN - 8
      DO 20 I = 1, N
         STR(I:I) = ' '
   20 CONTINUE
      DO 30 I = 1, 8
         STR(N+I:N+I) = WORK(I:I)
   30 CONTINUE
      RETURN
   90 DO 95 I = 1, LEN
         STR(I:I) = '*'
   95 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C  LATLON   [0917:066B]  edit longitude and latitude as 'E ddd mm.m',
C           'N  dd mm.m'; ALONG is first reduced to -180..+180
C-----------------------------------------------------------------------
      SUBROUTINE LATLON (ALONG, ALAT, CLONG, CLAT)
      DOUBLE PRECISION ALONG, ALAT
      CHARACTER*10 CLONG, CLAT
      SAVE
      IF (ALONG .GT. 180.D0) ALONG = ALONG - 360.D0
      IF (ALONG .LT. -180.D0) ALONG = 360.D0 + ALONG
      CLONG(1:2) = 'E '
      IF (ALONG .LT. 0.D0) CLONG(1:2) = 'W '
      CALL DMSTR (DABS (ALONG), 1, 8, CLONG(3:))
      IF (CLONG(3:10) .EQ. '  0 00.0') CLONG(1:2) = '  '
      CLAT(1:2) = 'N '
      IF (ALAT .LT. 0.D0) CLAT(1:2) = 'S '
      CALL DMSTR (DABS (ALAT), 1, 8, CLAT(3:))
      IF (CLAT(3:10) .EQ. '  0 00.0') CLAT(1:2) = '  '
      RETURN
      END
C
C-----------------------------------------------------------------------
C  GETNAM   [06B6:1FF1]  name of navigational body I (1..6 Sun, Moon,
C           planets; 7..63 the 57 navigational stars)
C-----------------------------------------------------------------------
      SUBROUTINE GETNAM (I, NAME)
      INTEGER*4 I
      CHARACTER*8 NAME
      CHARACTER*8 NAVNAM(63)
      SAVE
      DATA NAVNAM /
     & 'SUN', 'MOON', 'VENUS', 'MARS', 'JUPITER', 'SATURN',
     & 'ACAMAR', 'ACHERNAR', 'ACRUX', 'ADHARA', 'ALDEBA', 'ALIOTH',
     & 'ALKAID', 'AL NA', 'ALNILAM', 'ALPHARD', 'ALPHECCA', 'ALPHER',
     & 'ALTAIR', 'ANKAA', 'ANTARES', 'ARCTURUS', 'ATRIA', 'AVIOR',
     & 'BELLATR', 'BETELG', 'CANOPUS', 'CAPELLA', 'DENEB_', 'DENEBOLA',
     & 'DIPHDA', 'DUBHE', 'ELNATH', 'ELTANI', 'ENIF', 'FOMALH',
     & 'GACRUX', 'GIENAH', 'HADAR', 'HAMAL', 'KAUS A', 'KOCHAB',
     & 'MARKAB', 'MENKAR', 'MENKENT', 'MIAPLAC', 'MIRFAK', 'NUNKI',
     & 'PEACOCK', 'POLLUX', 'PROCYON', 'RASALHA', 'REGULUS', 'RIGEL__',
     & 'RIGIL K', 'SABIK', 'SCHEDAR', 'SHAULA', 'SIRIUS', 'SPICA',
     & 'SUHAIL', 'VEGA', 'ZUBENE' /
      NAME = NAVNAM(I)
      RETURN
      END
