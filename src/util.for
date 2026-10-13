C=======================================================================
C  ICE 0.51  --  recovered source: utilities
C  037E:2944  ANGSTR   angle/time -> text (runtime-built FORMAT)
C  037E:304F  DATJD    date input (JD, YYYYMMDD.HHMMSS, YYMMDD) -> JD
C  06B6:1B17  CALDAT   JD -> calendar date
C  06B6:17C8 .. 06B6:1FF1  small character utilities
C=======================================================================
C
C-----------------------------------------------------------------------
C  ANGSTR   [037E:2944]  format angle X into STR(1:LEN), right-justified
C           KIND  1 radians -> h m s, 2 radians -> deg ' ",
C                 3 hours -> h m s, otherwise degrees -> deg ' "
C           ISIGN 0 unsigned (normalised to 0..24h / 0..360 deg),
C                 1 '-' when negative, 2 also '+' when positive
C           NDIG  width of the leading (hours/degrees) field
C           NDEC  decimals of the seconds; < 0: no seconds field
C           A field that cannot hold the value is filled with '*'.
C-----------------------------------------------------------------------
      SUBROUTINE ANGSTR (X, KIND, ISIGN, NDIG, NDEC, LEN, STR)
      DOUBLE PRECISION X
      INTEGER*4 KIND, ISIGN, NDIG, NDEC, LEN
      CHARACTER*(*) STR
      DOUBLE PRECISION SECRAD, SECRAH, V, P, FRAC
      INTEGER*4 I, IDEG, IMIN, ISEC, NW, ISTART
      CHARACTER*40 FMT, WORK, FMT0
      CHARACTER*1 SGN
      CHARACTER*2 NUM(24)
      SAVE
      DATA FMT0 / '(T  , A1,  I  ,1X,I2.2,1X,I2.2,F  .    )' /
      DATA NUM / ' 1', ' 2', ' 3', ' 4', ' 5', ' 6', ' 7', ' 8', ' 9',
     &           '10', '11', '12', '13', '14', '15', '16', '17', '18',
     &           '19', '20', '21', '22', '23', '24' /
      SECRAD = 206264.80624709636D0
      SECRAH = 13750.987083139758D0
      NW = 0
      IF (NDIG .LT. 1) GO TO 90
      IF (LEN .LT. 4 .OR. LEN .GT. 40) GO TO 90
      IF (NDIG + NDEC + 4 .GT. LEN) GO TO 90
      IF (KIND .EQ. 1) GO TO 20
      IF (KIND .EQ. 2) GO TO 30
      IF (KIND .EQ. 3) GO TO 40
C     degrees -> arcseconds
      V = X * 3600.D0
      IF (ISIGN .EQ. 0) THEN
         V = DMOD (V, 1296000.D0)
         IF (V .LT. 0.D0) V = 1296000.D0 + V
      END IF
      GO TO 50
C     radians -> seconds of time
   20 V = X * SECRAH
   25 IF (ISIGN .EQ. 0) THEN
         V = DMOD (V, 86400.D0)
         IF (V .LT. 0.D0) V = 86400.D0 + V
      END IF
      GO TO 50
C     radians -> arcseconds
   30 V = X * SECRAD
      IF (ISIGN .EQ. 0) THEN
         V = DMOD (V, 1296000.D0)
         IF (V .LT. 0.D0) V = 1296000.D0 + V
      END IF
      GO TO 50
C     hours -> seconds of time
   40 V = X * 3600.D0
      GO TO 25
C     round to the last digit shown
   50 P = 10.D0 ** DBLE (NDEC)
      IF (NDEC .LT. 0) P = 1.D0 / 60.D0
      V = DNINT (V * P) / P * 1.000000000000001D0
      SGN = ' '
      IF (ISIGN .NE. 0 .AND. V .LT. 0.D0) SGN = '-'
      IF (ISIGN .GE. 2 .AND. V .GT. 0.D0) SGN = '+'
      V = DABS (V)
      IDEG = V / 3600.D0
      IMIN = (V - IDEG*3600.D0) / 60.D0
      FRAC = V - IMIN*60.D0 - IDEG*3600.D0
      ISEC = FRAC
      FRAC = FRAC - ISEC
C     width of the result
      NW = NDIG + 3
      IF (ISIGN .NE. 0) NW = NW + 1
      IF (NDEC .GE. 0) NW = NW + 3
      IF (NDEC .GT. 0) NW = NDEC + NW + 1
      IF (LEN .LT. NW) GO TO 90
      ISTART = LEN - NW + 1
C     build the format
      FMT = FMT0
      FMT(3:4) = NUM(ISTART)
      IF (ISIGN .EQ. 0) THEN
         FMT(7:8) = '  '
         FMT(9:10) = '  '
      END IF
      FMT(13:14) = NUM(NDIG)
      IF (NDEC .GT. 0) THEN
         FMT(33:34) = NUM(NDEC+1)
         FMT(37:38) = NUM(NDEC)
      ELSE
         FMT(31:32) = '  '
         FMT(35:36) = '  '
      END IF
      IF (NDEC .LT. 0) THEN
         FMT(23:24) = '  '
         FMT(25:26) = '  '
         FMT(27:28) = '  '
         FMT(29:30) = '  '
      END IF
      IF (ISIGN .NE. 0) THEN
         IF (NDEC .GT. 0) WRITE (WORK, FMT) SGN, IDEG, IMIN, ISEC, FRAC
         IF (NDEC .EQ. 0) WRITE (WORK, FMT) SGN, IDEG, IMIN, ISEC
         IF (NDEC .LT. 0) WRITE (WORK, FMT) SGN, IDEG, IMIN
      ELSE
         IF (NDEC .GT. 0) WRITE (WORK, FMT) IDEG, IMIN, ISEC, FRAC
         IF (NDEC .EQ. 0) WRITE (WORK, FMT) IDEG, IMIN, ISEC
         IF (NDEC .LT. 0) WRITE (WORK, FMT) IDEG, IMIN
      END IF
      DO 60 I = 1, LEN
         STR(I:I) = WORK(I:I)
   60 CONTINUE
      RETURN
   90 DO 95 I = 1, LEN
         STR(I:I) = '*'
   95 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C  DATJD    [037E:304F]  interpret a date typed by the user:
C           1e6 <= D <= 1e7           Julian date
C           D <= 999 after a JD       Julian date as well
C           1000 < D < 1e6            YYMMDD.HHMMSS (19YY; D is changed)
C           D >= 1e6                  YYYYMMDD.HHMMSS
C           D <= 1000                 day number + fraction
C           A fraction above 0.9 or below 1e-7 is taken as a fraction
C           of a day, otherwise as .HHMMSS.
C-----------------------------------------------------------------------
      SUBROUTINE DATJD (D, TJD)
      DOUBLE PRECISION D, TJD
      DOUBLE PRECISION FR, S
      INTEGER*4 IY, IM, ID, JD, IH, MI
      LOGICAL LASTJD
      SAVE
      DATA LASTJD / .FALSE. /
      IF ((D .GE. 1.D6 .AND. D .LE. 1.D7) .OR.
     &    (D .LE. 999.D0 .AND. LASTJD)) THEN
         TJD = D
         LASTJD = .TRUE.
         RETURN
      END IF
      IF (D .LE. 1000.D0) GO TO 10
      IF (D .LT. 1.D6) D = 19000000.D0 + D
      IY = D * 0.0001D0
      IM = DMOD (D, 10000.D0) * 0.01D0
      ID = DMOD (D, 100.D0)
      JD = 367*IY - 7*(IY + (IM+9)/12)/4 - 3*((IY + (IM-9)/7)/100 + 1)/4
     &     + 275*IM/9 + ID + 1721029
      TJD = DBLE (JD) - 0.5D0
      GO TO 20
   10 TJD = DINT (D)
   20 FR = DMOD (D, 1.D0)
      IF (FR .GT. 0.9D0 .OR. FR .LT. 1.D-7) THEN
         TJD = FR + TJD
      ELSE
         FR = DNINT (FR * 1.D6)
         IH = FR * 0.0001D0
         MI = DMOD (FR, 10000.D0) * 0.01D0
         S = DMOD (FR, 100.D0)
         TJD = TJD + DBLE(IH)/24.D0 + DBLE(MI)/1440.D0 + S/86400.D0
      END IF
      LASTJD = .FALSE.
      RETURN
      END
C
C-----------------------------------------------------------------------
C  CALDAT   [06B6:1B17]  Julian date -> year, month, day, month name,
C           hour (Fliegel & Van Flandern)
C-----------------------------------------------------------------------
      SUBROUTINE CALDAT (TJD, IY, IM, ID, CMON, HOUR)
      DOUBLE PRECISION TJD, HOUR
      INTEGER*4 IY, IM, ID
      CHARACTER*3 CMON
      DOUBLE PRECISION DJD
      INTEGER*4 JD, L, N, I, J, K
      CHARACTER*3 MONTHS(12)
      SAVE
      DATA MONTHS / 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul',
     &              'Aug', 'Sep', 'Oct', 'Nov', 'Dec' /
      DJD = TJD + 0.5D0
      JD = DJD
      HOUR = DMOD (DJD, 1.D0) * 24.D0
      L = JD + 68569
      N = 4*L/146097
      L = L - (146097*N + 3)/4
      I = 4000*(L + 1)/1461001
      L = L - 1461*I/4 + 31
      J = 80*L/2447
      K = L - 2447*J/80
      L = J/11
      J = J + 2 - 12*L
      I = 100*(N - 49) + I + L
      CMON = MONTHS(J)
      IY = I
      IM = J
      ID = K
      RETURN
      END
C
C-----------------------------------------------------------------------
C  NBLANK   [06B6:17C8]  number of non-blank characters in an 8-char name
C-----------------------------------------------------------------------
      INTEGER*4 FUNCTION NBLANK (NAME)
      CHARACTER*8 NAME
      INTEGER*4 I, N
      SAVE
      N = 0
      DO 10 I = 1, 8
         IF (NAME(I:I) .NE. ' ') N = N + 1
   10 CONTINUE
      NBLANK = N
      RETURN
      END
C
C-----------------------------------------------------------------------
C  MATCH    [06B6:183D]  number of characters of PAT found, in order,
C           in NAME ('?' matches anything, '_' a blank; letters of PAT
C           are also tried in upper case; blanks in NAME are skipped)
C-----------------------------------------------------------------------
      INTEGER*4 FUNCTION MATCH (PAT, NAME)
      CHARACTER*8 PAT, NAME
      CHARACTER*1 C, D, UPCASE
      INTEGER*4 I, J, N
      SAVE
      N = 0
      J = 0
      DO 30 I = 1, 8
         C = PAT(I:I)
         IF (C .EQ. ' ') GO TO 30
   10    J = J + 1
         IF (J .GT. 8) GO TO 40
         D = NAME(J:J)
         IF (C .EQ. '_' .AND. D .EQ. ' ') GO TO 20
         IF (C .EQ. '?') GO TO 20
         IF (C .EQ. D) GO TO 20
         C = UPCASE (C)
         IF (C .EQ. D) GO TO 20
         IF (D .NE. ' ') GO TO 40
         GO TO 10
   20    N = N + 1
   30 CONTINUE
   40 MATCH = N
      RETURN
      END
C
C-----------------------------------------------------------------------
C  UPCASE   [06B6:197C]  upper case of one character
C-----------------------------------------------------------------------
      CHARACTER*1 FUNCTION UPCASE (C)
      CHARACTER*1 C
      INTEGER*4 I
      SAVE
      I = ICHAR (C)
      IF (I .GE. 97 .AND. I .LE. 122) I = I - 32
      UPCASE = CHAR (I)
      RETURN
      END
C
C-----------------------------------------------------------------------
C  IVAL     [06B6:19CA]  value of a right-justified integer in an 8-char
C           field (leading sign allowed); 0 if invalid or > 32767
C-----------------------------------------------------------------------
      INTEGER*4 FUNCTION IVAL (STR)
      CHARACTER*8 STR
      CHARACTER*1 C
      INTEGER*4 I, IZERO, N, K, IDIG
      LOGICAL SIGN
      SAVE
      SIGN = .FALSE.
      IZERO = ICHAR ('0')
      N = 0
      K = -1
      DO 10 I = 1, 8
         C = STR(9-I:9-I)
         IF (C .EQ. ' ') GO TO 10
         IF (SIGN) GO TO 20
         IF (C .EQ. '-') THEN
            N = -N
            SIGN = .TRUE.
            GO TO 10
         END IF
         IF (C .EQ. '+') THEN
            SIGN = .TRUE.
            GO TO 10
         END IF
         IDIG = ICHAR (C) - IZERO
         IF (IDIG .LT. 0 .OR. IDIG .GT. 9) GO TO 20
         K = K + 1
         N = N + 10**K * IDIG
   10 CONTINUE
      IF (IABS (N) .LE. 32767) GO TO 30
   20 N = 0
   30 IVAL = N
      RETURN
      END
C
C-----------------------------------------------------------------------
C  DAYSYM   [06B6:1D43]  marker for an event time H (hours from the
C           start of the day): '<' two days before ... '>' two after
C-----------------------------------------------------------------------
      SUBROUTINE DAYSYM (H, C)
      DOUBLE PRECISION H
      CHARACTER*1 C
      CHARACTER*5 SYMS
      INTEGER*4 K
      SAVE
      DATA SYMS / '<p f>' /
      K = H / 24.D0 + 3.D0
      C = SYMS(K:K)
      RETURN
      END
