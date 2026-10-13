C=======================================================================
C  ICE 0.51  --  recovered source, module 0C4D
C  USNO Compressed Ephemeris: parameter file, ephemeris files,
C  decompression of the packed Chebyshev residuals and evaluation.
C
C  Model: each body's barycentric position is a Chebyshev series over
C  an interval equal to (about) its orbital period.  For Mercury..Mars
C  and the Moon a reference orbit (PARAM.DAT, CA/CB) is rotated into
C  place and the residual series from EPHnn.DAT is added; the outer
C  planets and the Sun use the residual series alone.  The residual
C  coefficients are stored as integers in a variable-width packing
C  (32, 24, 16, 8, 4, 2 and 1 bit fields), see UNPACK.
C
C  Two far COMMON blocks hold the data (segments 2000 and 2109 of the
C  original); names are invented, layout is the original one.
C=======================================================================
C
C-----------------------------------------------------------------------
C  Block data for the ephemeris commons
C-----------------------------------------------------------------------
      BLOCK DATA EPHBD
      DOUBLE PRECISION TB(2), TE(2), XMU, EMRAT, CC(41,3,11)
      INTEGER*4 IDATE(30), JHDR(11,2), IDAT(10000,2), IPTR(400,2),
     &          IHDR(21,2), NLAST(11), NREF, IB
      LOGICAL FIRST
      COMMON /EPHBUF/ TB, TE, XMU, EMRAT, CC, IDATE, JHDR, IDAT, IPTR,
     &                IHDR, NLAST, NREF, IB, FIRST
      DATA EMRAT / 0.01230002D0 /
      DATA FIRST / .TRUE. /
      END
C
C-----------------------------------------------------------------------
C  SOLSYS   [0C4D:0008]  position and velocity of a body
C           TJD    TDB Julian date
C           NB     body: 1 Mercury .. 9 Pluto, 10 Sun, 11 Moon
C           IORIG  0 = solar-system barycentre, 1 = centre of the Sun
C           POS    position, AU;  VEL velocity, AU/day
C           IERR   0 ok, 1 date before / 2 date after the ephemeris
C                  files, 3 bad argument
C-----------------------------------------------------------------------
      SUBROUTINE SOLSYS (TJD, NB, IORIG, POS, VEL, IERR)
      DOUBLE PRECISION TJD, POS(3), VEL(3)
      INTEGER*4 NB, IORIG, IERR
      DOUBLE PRECISION T0, PER(11), PB(11), PC(11), E1(4), E2(4),
     &     SC(11), E3(5), F1(5), F2(5), CA(42,5), CB(42,5),
     &     START(11), SPAN(11), XM1, XM2, XM3, XM4, CE, SE
      INTEGER*4 NCREF(5), NX(11), IREC(11)
      COMMON /EPHPAR/ T0, PER, PB, PC, E1, E2, SC, E3, F1, F2, CA, CB,
     &     START, SPAN, XM1, XM2, XM3, XM4, CE, SE, NCREF, NX, IREC
      DOUBLE PRECISION TB(2), TE(2), XMU, EMRAT, CC(41,3,11)
      INTEGER*4 IDATE(30), JHDR(11,2), IDAT(10000,2), IPTR(400,2),
     &          IHDR(21,2), NLAST(11), NREF, IB
      LOGICAL FIRST
      COMMON /EPHBUF/ TB, TE, XMU, EMRAT, CC, IDATE, JHDR, IDAT, IPTR,
     &                IHDR, NLAST, NREF, IB, FIRST
      DOUBLE PRECISION FM
      INTEGER*4 IEARTH, IMOON, ISUN, K0, K1, NCOF, I, IPOS
      LOGICAL LFLAG
      SAVE
      IEARTH = 3
      IMOON = 11
      ISUN = 10
      K0 = 0
      NCOF = 41
      K1 = 1
      IF (NB .LE. 0 .OR. NB .GE. 12) GO TO 90
      IF (IORIG .NE. 0 .AND. IORIG .NE. 1) GO TO 90
      IERR = 0
C
C     first call: read PARAM.DAT, invalidate everything
      IF (FIRST) THEN
         XMU = 1.D0 / (1.D0/EMRAT + 1.D0)
         IB = 2
         TB(1) = 1.D10
         TB(2) = 1.D10
         DO 10 I = 1, 11
            IREC(I) = 0
            START(I) = 1.D10
            SPAN(I) = 1.D10
   10    CONTINUE
         CALL PARAMS (NREF, IDATE)
         FIRST = .FALSE.
      END IF
C
C     coefficients for body NB current?  if not, make sure one of the
C     two ephemeris-file buffers covers TJD, then decode the interval
      IF (START(NB) .GT. TJD .OR. START(NB)+SPAN(NB) .LT. TJD) THEN
         IF (TB(IB) .GT. TJD .OR. TE(IB) .LT. TJD) THEN
            IB = 3.2 - IB
            IF (TB(IB) .GT. TJD .OR. TE(IB) .LT. TJD) THEN
               CALL EPHRD (TJD, IDATE, IHDR(1,IB), IPTR(1,IB),
     &              IDAT(1,IB), TB(IB), TE(IB), JHDR(1,IB), IERR)
               IF (IERR .NE. 0) THEN
                  IB = 3.2 - IB
                  RETURN
               END IF
            END IF
         END IF
         CALL LOCATE (NB, NREF, TJD, LFLAG, IPOS, IHDR(1,IB),
     &        IPTR(1,IB), TB(IB), TE(IB), JHDR(1,IB))
         CALL COEFS (NB, NREF, IDAT(IPOS,IB), LFLAG, CC(1,1,NB),
     &        NLAST(NB))
      END IF
C
C     the Earth needs the Moon and the Moon needs the Earth-Moon
C     barycentre (series 3 is the barycentre)
      IF (NB .EQ. 3 .OR. NB .EQ. 11) THEN
         IF (NB .NE. 3) THEN
            IF (START(3) .GT. TJD .OR. START(3)+SPAN(3) .LT. TJD) THEN
               CALL LOCATE (IEARTH, NREF, TJD, LFLAG, IPOS, IHDR(1,IB),
     &              IPTR(1,IB), TB(IB), TE(IB), JHDR(1,IB))
               CALL COEFS (IEARTH, NREF, IDAT(IPOS,IB), LFLAG,
     &              CC(1,1,3), NLAST(3))
            END IF
         END IF
         IF (START(11) .GT. TJD .OR. START(11)+SPAN(11) .LT. TJD) THEN
            CALL LOCATE (IMOON, NREF, TJD, LFLAG, IPOS, IHDR(1,IB),
     &           IPTR(1,IB), TB(IB), TE(IB), JHDR(1,IB))
            CALL COEFS (IMOON, NREF, IDAT(IPOS,IB), LFLAG,
     &           CC(1,1,11), NLAST(11))
         END IF
      END IF
      IF (IORIG .NE. 0) THEN
         IF (START(10) .GT. TJD .OR. START(10)+SPAN(10) .LT. TJD) THEN
            CALL LOCATE (ISUN, NREF, TJD, LFLAG, IPOS, IHDR(1,IB),
     &           IPTR(1,IB), TB(IB), TE(IB), JHDR(1,IB))
            CALL COEFS (ISUN, NREF, IDAT(IPOS,IB), LFLAG,
     &           CC(1,1,10), NLAST(10))
         END IF
      END IF
C
C     position
      CALL CHEBEV (NB, NREF, K0, NCOF, NLAST(NB), TJD, 0.D0, POS,
     &     1.D0, CC(1,1,NB), POS)
      IF (NB .EQ. 3 .OR. NB .EQ. 11) THEN
         IF (NB .NE. 3) THEN
            FM = 1.D0 - XMU
            CALL CHEBEV (IEARTH, NREF, K0, NCOF, NLAST(3), TJD, FM,
     &           POS, 1.D0, CC(1,1,3), POS)
         ELSE
            FM = -XMU
            CALL CHEBEV (IMOON, NREF, K0, NCOF, NLAST(11), TJD, 1.D0,
     &           POS, FM, CC(1,1,11), POS)
         END IF
      END IF
      IF (IORIG .NE. 0) CALL CHEBEV (ISUN, NREF, K0, NCOF, NLAST(10),
     &     TJD, 1.D0, POS, -1.D0, CC(1,1,10), POS)
C
C     velocity
      CALL CHEBEV (NB, NREF, K1, NCOF, NLAST(NB), TJD, 0.D0, VEL,
     &     1.D0, CC(1,1,NB), VEL)
      IF (NB .EQ. 3 .OR. NB .EQ. 11) THEN
         IF (NB .NE. 3) THEN
            FM = 1.D0 - XMU
            CALL CHEBEV (IEARTH, NREF, K1, NCOF, NLAST(3), TJD, FM,
     &           VEL, 1.D0, CC(1,1,3), VEL)
         ELSE
            FM = -XMU
            CALL CHEBEV (IMOON, NREF, K1, NCOF, NLAST(11), TJD, 1.D0,
     &           VEL, FM, CC(1,1,11), VEL)
         END IF
      END IF
      IF (IORIG .NE. 0) CALL CHEBEV (ISUN, NREF, K1, NCOF, NLAST(10),
     &     TJD, 1.D0, VEL, -1.D0, CC(1,1,10), VEL)
      CLOSE (15)
      RETURN
   90 IERR = 3
      RETURN
      END
C
C-----------------------------------------------------------------------
C  PARAMS   [0C4D:1465]  read PARAM.DAT (unit 20): file date table,
C           per-body interval lengths, orientation elements and the
C           reference-orbit series.  NREF returns the number of bodies
C           (1..NREF) that have a reference orbit (4).
C-----------------------------------------------------------------------
      SUBROUTINE PARAMS (NREF, IDATE)
      INTEGER*4 NREF, IDATE(30)
      DOUBLE PRECISION T0, PER(11), PB(11), PC(11), E1(4), E2(4),
     &     SC(11), E3(5), F1(5), F2(5), CA(42,5), CB(42,5),
     &     START(11), SPAN(11), XM1, XM2, XM3, XM4, CE, SE
      INTEGER*4 NCREF(5), NX(11), IREC(11)
      COMMON /EPHPAR/ T0, PER, PB, PC, E1, E2, SC, E3, F1, F2, CA, CB,
     &     START, SPAN, XM1, XM2, XM3, XM4, CE, SE, NCREF, NX, IREC
      DOUBLE PRECISION OBL
      CHARACTER*30 NAME
      INTEGER*4 I, J, NB, NC, NXX
      LOGICAL LEX
      SAVE
      OBL = 0.4090928D0
      CE = DCOS (OBL)
      SE = DSIN (OBL)
      GO TO 20
   10 WRITE (*,*) 'File PARAM.DAT not found on active disk.'
      PAUSE 'Insert correct disk and press "Enter" when ready.'
   20 INQUIRE (FILE='PARAM.DAT', EXIST=LEX)
      IF (.NOT. LEX) GO TO 10
      OPEN (20, FILE='PARAM.DAT', FORM='UNFORMATTED')
      DO 50 I = 1, 11
         IF (I .EQ. 1) READ (20) IDATE
         READ (20, END=99) NAME, NB, NREF, NC, NXX
         IF (NREF .GE. NB) NCREF(NB) = NC
         IF (NB .EQ. 11) NCREF(5) = NC
         NX(NB) = NXX
         IREC(NB) = I
         READ (20) T0, PER(NB)
         READ (20) PB(NB), PC(NB), SC(NB)
         IF (NB .LT. 11 .AND. NREF .LT. NB) GO TO 50
         IF (NB .EQ. 11) GO TO 40
         READ (20) E1(NB), E2(NB), E3(NB)
   30    READ (20) F1(NB), F2(NB)
         READ (20) (CA(J,NB), J = 1, NC+1)
         READ (20) (CB(J,NB), J = 1, NC+1)
         GO TO 50
C        the Moon's reference orbit is kept in slot 5
   40    READ (20) XM1, XM2, XM3
         READ (20) XM4, E3(5)
         NB = 5
         GO TO 30
   50 CONTINUE
   99 RETURN
      END
C
C-----------------------------------------------------------------------
C  EPHRD    [0C4D:17EB]  load the ephemeris file EPHnn.DAT that covers
C           TJD (units 21..44): header, pointer table, packed data.
C           IDATE(K)/100 is the starting Julian date of file K.
C-----------------------------------------------------------------------
      SUBROUTINE EPHRD (TJD, IDATE, IHDR, IPTR, IDAT, TBEG, TEND,
     &                  JHDR, IERR)
      DOUBLE PRECISION TJD, TBEG, TEND
      INTEGER*4 IDATE(30), IHDR(21), IPTR(*), IDAT(*), JHDR(11), IERR
      DOUBLE PRECISION D1, D2
      INTEGER*4 I, J, K, NUP, NDAT
      LOGICAL LEX
      CHARACTER*9 FNAME(24)
      SAVE
      DATA FNAME / 'EPH01.DAT', 'EPH02.DAT', 'EPH03.DAT', 'EPH04.DAT',
     &             'EPH05.DAT', 'EPH06.DAT', 'EPH07.DAT', 'EPH08.DAT',
     &             'EPH09.DAT', 'EPH10.DAT', 'EPH11.DAT', 'EPH12.DAT',
     &             'EPH13.DAT', 'EPH14.DAT', 'EPH15.DAT', 'EPH16.DAT',
     &             'EPH17.DAT', 'EPH18.DAT', 'EPH19.DAT', 'EPH20.DAT',
     &             'EPH21.DAT', 'EPH22.DAT', 'EPH23.DAT', 'EPH24.DAT' /
      D1 = IDATE(1) * 0.01D0
      D2 = IDATE(30) * 0.01D0
      IF (D1 .GT. TJD .OR. D2 .LT. TJD) THEN
         IERR = 2
         IF (TJD .LT. D1 + 0.01D0) IERR = 1
         RETURN
      END IF
      DO 10 I = 1, 29
         K = I
         TEND = IDATE(I+1) * 0.01D0
         IF (TEND .GE. TJD) GO TO 20
   10 CONTINUE
   20 TBEG = IDATE(K) * 0.01D0
      GO TO 40
   30 WRITE (*,*) 'Ephemeris file ', FNAME(K), ' not found on ',
     &            'active disk.'
      PAUSE 'Insert correct disk and press "Enter" when ready.'
   40 INQUIRE (FILE=FNAME(K), EXIST=LEX)
      IF (.NOT. LEX) GO TO 30
      OPEN (K+20, FILE=FNAME(K), FORM='UNFORMATTED')
      READ (K+20) IHDR, JHDR
      NUP = IHDR(IHDR(1)) + 1
      DO 50 I = 1, NUP, 64
         READ (K+20) (IPTR(J), J = I, I+63)
   50 CONTINUE
      NDAT = IPTR(NUP) - 1
      DO 60 I = 1, NDAT, 64
         READ (K+20) (IDAT(J), J = I, I+63)
   60 CONTINUE
      CLOSE (K+20)
      RETURN
      END
C
C-----------------------------------------------------------------------
C  LOCATE   [0C4D:1C87]  find the interval of body NB that contains
C           TJD in the loaded ephemeris file; sets START(NB), SPAN(NB),
C           IPOS (index of its packed record in IDAT) and LFLAG
C           (.TRUE. if the record is a residual to a reference orbit)
C-----------------------------------------------------------------------
      SUBROUTINE LOCATE (NB, NREF, TJD, LFLAG, IPOS, IHDR, IPTR,
     &                   TBEG, TEND, JHDR)
      INTEGER*4 NB, NREF, IPOS, IHDR(*), IPTR(*), JHDR(*)
      DOUBLE PRECISION TJD, TBEG, TEND
      LOGICAL LFLAG
      DOUBLE PRECISION T0, PER(11), PB(11), PC(11), E1(4), E2(4),
     &     SC(11), E3(5), F1(5), F2(5), CA(42,5), CB(42,5),
     &     START(11), SPAN(11), XM1, XM2, XM3, XM4, CE, SE
      INTEGER*4 NCREF(5), NX(11), IREC(11)
      COMMON /EPHPAR/ T0, PER, PB, PC, E1, E2, SC, E3, F1, F2, CA, CB,
     &     START, SPAN, XM1, XM2, XM3, XM4, CE, SE, NCREF, NX, IREC
      DOUBLE PRECISION TS
      INTEGER*4 K, N
      SAVE
      IF (IREC(NB) .LE. 1) THEN
         K = 1
      ELSE
         K = IHDR(IREC(NB)) + 1
      END IF
      IF (NB .GT. NREF .AND. NB .NE. 11) THEN
C        intervals start at TBEG
         LFLAG = .FALSE.
         N = (TJD - TBEG) / PER(NB)
         IF (K + N .GE. IHDR(IREC(NB)+1)) N = IHDR(IREC(NB)+1) - K
         K = K + N
         START(NB) = PER(NB) * N + TBEG
      ELSE
C        first (partial) interval ends at TS
         TS = JHDR(NB) * 1.D-5 + TBEG
         IF (TJD .LT. TS) THEN
            LFLAG = .FALSE.
            START(NB) = TBEG
            SPAN(NB) = TS - TBEG
            GO TO 10
         END IF
         N = (TJD - TS) / PER(NB)
         LFLAG = .TRUE.
         IF (K + N + 1 .GE. IHDR(IREC(NB)+1)) THEN
            N = IHDR(IREC(NB)+1) - K - 1
            LFLAG = .FALSE.
         END IF
         K = K + N + 1
         START(NB) = PER(NB) * N + TS
      END IF
      SPAN(NB) = DMIN1 (TEND - START(NB), PER(NB))
   10 IPOS = IPTR(K)
      RETURN
      END
C
C-----------------------------------------------------------------------
C  COEFS    [0C4D:2030]  decode the Chebyshev coefficients of body NB
C           for the current interval into CC(41,3) (equatorial frame).
C           Residual series are unpacked from IDAT; when LFLAG is set
C           the reference orbit is added in its orbital plane, then
C           the plane (P,Q = tan(i/2) sin/cos node) is rotated into
C           place.  NLAST = last significant coefficient.
C-----------------------------------------------------------------------
      SUBROUTINE COEFS (NB, NREF, IDAT, LFLAG, CC, NLAST)
      INTEGER*4 NB, NREF, IDAT(*), NLAST
      LOGICAL LFLAG
      DOUBLE PRECISION CC(41,3)
      DOUBLE PRECISION T0, PER(11), PB(11), PC(11), E1(4), E2(4),
     &     SC(11), E3(5), F1(5), F2(5), CA(42,5), CB(42,5),
     &     START(11), SPAN(11), XM1, XM2, XM3, XM4, CE, SE
      INTEGER*4 NCREF(5), NX(11), IREC(11)
      COMMON /EPHPAR/ T0, PER, PB, PC, E1, E2, SC, E3, F1, F2, CA, CB,
     &     START, SPAN, XM1, XM2, XM3, XM4, CE, SE, NCREF, NX, IREC
      DOUBLE PRECISION ANG, CA1, SA1, Y(3), P, Q, R(3,3), TC, DEN,
     &     S, PP, QQ, X1, X2, X3
      INTEGER*4 IX, I, KMAX, N41
      SAVE
      N41 = 41
      IF (.NOT. LFLAG) THEN
         KMAX = 0
         P = PB(NB)
         Q = PC(NB)
         CALL UNPACK (IDAT, N41, SC(NB), CC)
      ELSE
C        time of mid-interval, thousands of Julian years from T0
         TC = (PER(NB)*0.5D0 + START(NB) - T0) / 365250.D0
         IF (NB .EQ. 11) THEN
            P = DCOS (XM4*TC + XM2) * (XM3*TC + XM1)
            Q = DSIN (XM4*TC + XM2) * (XM3*TC + XM1)
            IX = 5
         ELSE
            P = E1(NB)*TC + PB(NB)
            Q = E2(NB)*TC + PC(NB)
            IX = NB
         END IF
         ANG = F2(IX)*TC + F1(IX)
         CA1 = DCOS (ANG)
         SA1 = DSIN (ANG)
         CALL UNPACK (IDAT, N41, E3(IX), CC)
         KMAX = NCREF(IX) + 1
         DO 10 I = 1, KMAX
            CC(I,1) = CC(I,1) + CA1*CA(I,IX) - SA1*CB(I,IX)
            CC(I,2) = CC(I,2) + CA1*CB(I,IX) + SA1*CA(I,IX)
   10    CONTINUE
      END IF
C     rotation matrix from the orbital plane
      QQ = Q*Q
      PP = P*P
      S = PP + QQ
      DEN = 1.D0 / (S + 1.D0)
      R(1,3) = DEN*Q*2.D0
      R(2,3) = -(P*DEN*2.D0)
      R(3,3) = (1.D0 - S)*DEN
      R(1,1) = (PP - QQ + 1.D0)*DEN
      R(2,1) = P*Q*DEN*2.D0
      R(3,1) = -R(1,3)
      R(1,2) = R(2,1)
      R(2,2) = (QQ - PP + 1.D0)*DEN
      R(3,2) = P*DEN*2.D0
      NLAST = 0
      KMAX = MAX0 (KMAX, NX(NB)+1)
      DO 20 I = 1, KMAX
         X3 = CC(I,3)
         X2 = CC(I,2)
         X1 = CC(I,1)
         Y(1) = R(1,1)*X1 + R(1,2)*X2 + R(1,3)*X3
         Y(2) = R(2,1)*X1 + R(2,2)*X2 + R(2,3)*X3
         Y(3) = R(3,1)*X1 + R(3,2)*X2 + R(3,3)*X3
         IF (DABS(Y(1)) + DABS(Y(2)) + DABS(Y(3)) .GE. 1.D-14)
     &      NLAST = I
         CC(I,1) = Y(1)
         IF (NB .EQ. 11) THEN
C           the Moon's series are ecliptic
            CC(I,2) = CE*Y(2) - SE*Y(3)
            CC(I,3) = SE*Y(2) + CE*Y(3)
         ELSE
            CC(I,2) = Y(2)
            CC(I,3) = Y(3)
         END IF
   20 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C  CHEBEV   [0C4D:275B]  evaluate the series of body NB at TJD:
C           K = 0 position, K = 1 velocity (d/dt);
C           R(J) = S*Y(J) + F*series(J), J = 1..3
C-----------------------------------------------------------------------
      SUBROUTINE CHEBEV (NB, NREF, K, NC, N, TJD, S, Y, F, C, R)
      INTEGER*4 NB, NREF, K, NC, N
      DOUBLE PRECISION TJD, S, Y(3), F, C(NC,3), R(3)
      DOUBLE PRECISION T0, PER(11), PB(11), PC(11), E1(4), E2(4),
     &     SC(11), E3(5), F1(5), F2(5), CA(42,5), CB(42,5),
     &     START(11), SPAN(11), XM1, XM2, XM3, XM4, CE, SE
      INTEGER*4 NCREF(5), NX(11), IREC(11)
      COMMON /EPHPAR/ T0, PER, PB, PC, E1, E2, SC, E3, F1, F2, CA, CB,
     &     START, SPAN, XM1, XM2, XM3, XM4, CE, SE, NCREF, NX, IREC
      DOUBLE PRECISION HALF, TMID, X, V, FAC0, FAC1
      INTEGER*4 J
      LOGICAL LFONE, LFMONE, LSZERO, LSONE
      SAVE
      HALF = SPAN(NB) * 0.5D0
      TMID = START(NB) + HALF
C     the series are fitted over 1.1 times the interval (not the Moon)
      IF (NB .NE. 11) HALF = 1.1D0 * HALF
      X = -((TJD - TMID) / HALF)
      IF (K .EQ. 0) FAC0 = F
      IF (K .EQ. 1) FAC1 = -(F / HALF)
      LFONE = .FALSE.
      LFMONE = .FALSE.
      LSONE = .FALSE.
      LSZERO = .FALSE.
      IF (DABS(F - 1.D0) .LT. 1.D-13) LFONE = .TRUE.
      IF (DABS(F + 1.D0) .LT. 1.D-13) LFMONE = .TRUE.
      IF (DABS(S) .LT. 1.D-13) LSZERO = .TRUE.
      IF (DABS(S - 1.D0) .LT. 1.D-13) LSONE = .TRUE.
      DO 10 J = 1, 3
         IF (K .EQ. 1) THEN
            CALL CHEBD (X, C(1,J), N, V)
            V = FAC1 * V
         ELSE
            CALL CHEB (X, C(1,J), N, V)
            IF (.NOT. LFONE) THEN
               IF (LFMONE) THEN
                  V = -V
               ELSE
                  V = FAC0 * V
               END IF
            END IF
         END IF
         IF (LSONE) THEN
            R(J) = Y(J) + V
         ELSE IF (LSZERO) THEN
            R(J) = V
         ELSE
            R(J) = Y(J)*S + V
         END IF
   10 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C  UNPACK   [0C4D:2AD9]  unpack the residual coefficients of one
C           interval: IDAT -> C(NC,3) * SCALE.
C           Per component a header word holds six base-40 digits:
C           KF(6) 32-bit words, KF(5) 24-bit values (M of them paired
C           with an 8-bit value in the low byte), then KF(4), KF(3),
C           KF(2), KF(1) values of 16, 8, 4 and 2 bits packed into
C           16-bit fields, two per word, after all three components.
C-----------------------------------------------------------------------
      SUBROUTINE UNPACK (IDAT, NC, SCALE, C)
      INTEGER*4 IDAT(*), NC
      DOUBLE PRECISION SCALE, C(NC,3)
      INTEGER*4 NBW, IR4, IR3, IR2, IR1, IA, I, L, J, IS, I2, KK, IP,
     &     LO(3), KF(6,3), IBIAS, NBITS, M, NG(4,3), HI(3), IB(200),
     &     IPT(16), K1
      SAVE
      DATA IPT / 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048,
     &           4096, 8192, 16384, 32768 /
      DO 10 J = 1, 3
         DO 10 I = 1, NC
            C(I,J) = 0.D0
   10 CONTINUE
      IP = 0
      DO 40 J = 1, 3
C        header word: six base-40 counts
         IP = IP + 1
         IA = IABS (IDAT(IP))
         IR1 = MOD (IA, 102400000)
         KF(6,J) = IA / 102400000
         IF (IDAT(IP) .LT. 0) KF(6,J) = KF(6,J) + 20
         IR2 = MOD (IR1, 2560000)
         KF(5,J) = IR1 / 2560000
         IR3 = MOD (IR2, 64000)
         KF(4,J) = IR2 / 64000
         IR4 = MOD (IR3, 1600)
         KF(3,J) = IR3 / 1600
         KF(1,J) = MOD (IR4, 40)
         KF(2,J) = IR4 / 40
C        full 32-bit coefficients
         LO(J) = 1
         HI(J) = KF(6,J)
         DO 20 I = 1, HI(J)
            IP = IP + 1
            C(I,J) = IDAT(IP) * SCALE
   20    CONTINUE
C        24-bit coefficients; the first M carry an 8-bit one as well
         M = MIN0 (KF(5,J), KF(3,J) + KF(2,J) + KF(1,J))
         NG(4,J) = KF(4,J)
         NG(3,J) = MAX0 (KF(3,J) - M, 0)
         NG(2,J) = MAX0 (KF(2,J) - MAX0 (M - KF(3,J), 0), 0)
         NG(1,J) = MAX0 (KF(1,J) - MAX0 (M - KF(3,J) - KF(2,J), 0), 0)
         I2 = KF(6,J) + KF(5,J) + KF(4,J)
         LO(J) = HI(J) + 1
         HI(J) = HI(J) + M
         DO 30 I = LO(J), HI(J)
            IP = IP + 1
            I2 = I2 + 1
            IA = IABS (IDAT(IP))
            IF (IDAT(IP) .GT. 0) THEN
               K1 = IA / 256 - 8388608
            ELSE
               K1 = IA / 256
            END IF
            C(I,J) = K1 * SCALE
            C(I2,J) = (MOD (IA, 256) - 128) * SCALE
   30    CONTINUE
         LO(J) = HI(J) + 1
         HI(J) = HI(J) + KF(5,J) - M
         DO 35 I = LO(J), HI(J)
            IP = IP + 1
            C(I,J) = IDAT(IP) * SCALE
   35    CONTINUE
   40 CONTINUE
C     the remaining small coefficients: 16-bit fields, two per word
      NBITS = 0
      DO 50 J = 1, 3
         NBITS = NBITS + NG(4,J)*8 + NG(3,J)*4 + NG(2,J)*2 + NG(1,J)
   50 CONTINUE
      NBITS = (NBITS + 15) / 16 * 2
      DO 60 L = 2, NBITS, 2
         IP = IP + 1
         IA = IABS (IDAT(IP))
         IF (IDAT(IP) .GE. 0) THEN
            IB(L-1) = IA / 65536
         ELSE
            IB(L-1) = IA / 65536 + 32768
         END IF
         IB(L) = MOD (IA, 65536)
   60 CONTINUE
C     peel off fields of 16, 8, 4, 2 bits
      IS = 17
      L = 1
      DO 90 KK = 1, 4
         NBW = 2 ** (5 - KK)
         IBIAS = 2 ** (NBW - 1)
         DO 80 J = 1, 3
            HI(J) = HI(J) + KF(5-KK,J)
            LO(J) = HI(J) - NG(5-KK,J) + 1
            DO 70 I = LO(J), HI(J)
               IF (NBW + 1 .LT. IS) THEN
                  IS = IS - NBW
                  C(I,J) = (IB(L) / IPT(IS) - IBIAS) * SCALE
                  IB(L) = MOD (IB(L), IPT(IS))
               ELSE
                  C(I,J) = (IB(L) - IBIAS) * SCALE
                  IS = 17
                  L = L + 1
               END IF
   70       CONTINUE
   80    CONTINUE
   90 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C  CHEB     [0C4D:37BA]  Chebyshev series by Clenshaw's recurrence
C           F = C(1)/2 + SUM C(K+1)*T(K,X),  K = 1..N-1
C-----------------------------------------------------------------------
      SUBROUTINE CHEB (X, C, N, F)
      DOUBLE PRECISION X, C(*), F, TWOX, B0, B1, B2
      INTEGER*4 N, J, K
      SAVE
      TWOX = X * 2.D0
      B0 = 0.D0
      B1 = 0.D0
      DO 10 J = 1, N
         K = N - J + 1
         B2 = B1
         B1 = B0
         B0 = C(K) + TWOX * B1 - B2
   10 CONTINUE
      F = (B0 - B2) * 0.5D0
      RETURN
      END
C
C-----------------------------------------------------------------------
C  CHEBD    [0C4D:38ED]  derivative (with respect to X) of the series
C           evaluated by CHEB
C-----------------------------------------------------------------------
      SUBROUTINE CHEBD (X, C, N, F)
      DOUBLE PRECISION X, C(*), F, TWOX, D, D1, D2, B0, B1, B2, B2S, TK
      INTEGER*4 N, J, K, NM1
      SAVE
      TWOX = X * 2.D0
      D1 = 0.D0
      D2 = 0.D0
      B1 = 0.D0
      B2 = 0.D0
      NM1 = N - 1
      DO 10 J = 1, NM1
         K = N - J
         TK = 2 * K
         D = TK * C(K+1) + D1
         B0 = D + TWOX * B1 - B2
         B2S = B2
         B2 = B1
         B1 = B0
         D1 = D2
         D2 = D
   10 CONTINUE
      F = (B0 - B2S) * 0.5D0
      RETURN
      END
