C=======================================================================
C  ICE 0.51  --  recovered source, module 037E (USNO astrometry, NOVAS)
C  Recovered from ICE.EXE (MS FORTRAN 4.x).  Routine names follow the
C  NOVAS conventions where the binary is clearly a NOVAS routine;
C  addresses of the original code are given in each header.
C  Every routine here is verified against the original binary by
C  differential tests (see tests/).
C=======================================================================
C
C-----------------------------------------------------------------------
C  ANGLES   [037E:0F88]  vector -> right ascension (hours), dec (deg)
C-----------------------------------------------------------------------
      SUBROUTINE ANGLES (POS, RA, DEC)
      DOUBLE PRECISION POS(3), RA, DEC, SECCON, XYPROJ, R, D
      SAVE
      DATA SECCON / 206264.8062470964D0 /
      XYPROJ = DSQRT (POS(2)*POS(2) + POS(1)*POS(1))
      R = DATAN2 (POS(2), POS(1))
      D = DATAN2 (POS(3), XYPROJ)
      RA = R * SECCON / 54000.D0
      DEC = D * SECCON / 3600.D0
      IF (RA .LT. 0.D0) RA = RA + 24.D0
      RETURN
      END
C
C-----------------------------------------------------------------------
C  TIMES    [037E:107A]  TDB Julian date -> TDT Julian date
C           (TDB-TDT periodic terms, seconds, returned in SECDIF)
C-----------------------------------------------------------------------
      SUBROUTINE TIMES (TDBJD, TDTJD, SECDIF)
      DOUBLE PRECISION TDBJD, TDTJD, SECDIF, T, M, LJ, E
      SAVE
      T = (TDBJD - 2433282.5D0) / 36525.D0
      M = (TDBJD - 2433282.5D0) / 36525.D0 * 628.3019415D0 + 6.248291D0
      LJ = T * 575.3380832D0 + 5.652593D0
      E = DSIN (M) * 0.01671D0 + M
      SECDIF = DSIN (E) * 0.001658D0 + DSIN (LJ) * 2.073D-5
      TDTJD = TDBJD - SECDIF / 86400.D0
      RETURN
      END
C
C-----------------------------------------------------------------------
C  PROPMO   [037E:1124]  apply space motion: POS2 = POS1 + VEL1*(T2-T1)
C-----------------------------------------------------------------------
      SUBROUTINE PROPMO (TJD1, POS1, VEL1, TJD2, POS2)
      DOUBLE PRECISION TJD1, POS1(3), VEL1(3), TJD2, POS2(3)
      INTEGER J
      SAVE
      DO 10 J = 1, 3
         POS2(J) = VEL1(J) * (TJD2 - TJD1) + POS1(J)
   10 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C  GEOCEN   [037E:11D8]  POS2 = POS1 - PE ;  light time in days
C-----------------------------------------------------------------------
      SUBROUTINE GEOCEN (POS1, PE, POS2, TLIGHT)
      DOUBLE PRECISION POS1(3), PE(3), POS2(3), TLIGHT, C
      INTEGER J
      SAVE
      DATA C / 173.1446333D0 /
      DO 10 J = 1, 3
         POS2(J) = POS1(J) - PE(J)
   10 CONTINUE
      TLIGHT = DSQRT (POS2(2)*POS2(2) + POS2(3)*POS2(3)
     &              + POS2(1)*POS2(1)) / C
      RETURN
      END
C
C-----------------------------------------------------------------------
C  VECTRS   [037E:0D4B]  catalog data -> position and velocity vectors
C           RA hours, DEC deg, PMRA s/century, PMDEC "/century,
C           PARLAX arcsec, RADVEL km/s; POS in AU, VEL in AU/day
C-----------------------------------------------------------------------
      SUBROUTINE VECTRS (RA, DEC, PMRA, PMDEC, PARLAX, RADVEL,
     &                   POS, VEL)
      DOUBLE PRECISION RA, DEC, PMRA, PMDEC, PARLAX, RADVEL,
     &                 POS(3), VEL(3), SECCON, PARAL, DIST, R, D,
     &                 CRA, SRA, CDC, SDC, PMR, PMD, RVL
      SAVE
      DATA SECCON / 206264.8062470964D0 /
      PARAL = PARLAX
      IF (PARAL .LE. 0.D0) PARAL = 1.D-7
      DIST = SECCON / PARAL
      R = RA * 54000.D0 / SECCON
      D = DEC * 3600.D0 / SECCON
      CRA = DCOS (R)
      SRA = DSIN (R)
      CDC = DCOS (D)
      SDC = DSIN (D)
      POS(1) = CRA * (DIST * CDC)
      POS(2) = SRA * (DIST * CDC)
      POS(3) = DIST * SDC
      PMR = PMRA * CDC * 15.D0 / (PARAL * 36525.D0)
      PMD = PMDEC / (PARAL * 36525.D0)
      RVL = RADVEL * 86400.D0 / 149597870.D0
      VEL(1) = - (PMR * SRA) - CRA * (PMD * SDC)
     &         + CRA * (RADVEL * 86400.D0 / 149597870.D0 * CDC)
      VEL(2) =   (PMR * CRA) - SRA * (PMD * SDC)
     &         + SRA * (RADVEL * 86400.D0 / 149597870.D0 * CDC)
      VEL(3) =   PMD * CDC + RVL * SDC
      RETURN
      END
C
C-----------------------------------------------------------------------
C  ABERAT   [037E:12F3]  aberration of light (relativistic form)
C           POS1 geocentric position, VE earth velocity (AU/day),
C           TLIGHT light time (days; 0 = compute it), POS2 apparent
C-----------------------------------------------------------------------
      SUBROUTINE ABERAT (POS1, VE, TLIGHT, POS2)
      DOUBLE PRECISION POS1(3), VE(3), TLIGHT, POS2(3), C, TL,
     &                 P1MAG, VEMAG, BETA, DOT, COSD, GAMMAI, P, Q, R,
     &                 VX, VY, VZ
      INTEGER J
      SAVE
      DATA C / 173.1446333D0 /
      TL = TLIGHT
      P1MAG = TL * C
      IF (TL .NE. 0.D0) GOTO 10
      P1MAG = DSQRT (POS1(2)*POS1(2) + POS1(3)*POS1(3)
     &             + POS1(1)*POS1(1))
      TL = P1MAG / C
   10 CONTINUE
      VX = VE(1)
      VZ = VE(3)
      VY = VE(2)
      VEMAG = DSQRT (VY*VY + VZ*VZ + VX*VX)
      BETA = VEMAG / C
      DOT = POS1(2)*VY + POS1(3)*VZ + POS1(1)*VX
      COSD = DOT / (P1MAG * VEMAG)
      GAMMAI = DSQRT (1.D0 - BETA * BETA)
      P = BETA * COSD
      Q = (P / (GAMMAI + 1.D0) + 1.D0) * TL
      R = P + 1.D0
      DO 20 J = 1, 3
         POS2(J) = (POS1(J) * GAMMAI + VE(J) * Q) / R
   20 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C  SIDTIM   [037E:0BC5]  Greenwich sidereal time (hours)
C           K = 0 mean, K .NE. 0 apparent (adds equation of equinoxes)
C-----------------------------------------------------------------------
      SUBROUTINE SIDTIM (TJDH, TJDL, K, GST)
      DOUBLE PRECISION TJDH, TJDL, GST, SECCON, TH, TL, T, T2, T3,
     &                 EQEQ, EPS0, DPSI, DEPS, EPS, ST
      INTEGER*4 K
      SAVE
      DATA SECCON / 206264.8062470964D0 /
      TH = (TJDH - 2451545.D0) / 36525.D0
      TL = TJDL / 36525.D0
      T = TL + TH
      T2 = T * T
      T3 = T2 * T
      EQEQ = 0.D0
      IF (K .EQ. 0) GOTO 10
      EPS0 = (84381.448D0 - 46.8150D0*T - 0.00059D0*T2
     &       + 0.001813D0*T3) / SECCON
      CALL NOD (T, DPSI, DEPS)
      DEPS = DEPS / SECCON
      EPS = DEPS + EPS0
      EQEQ = DCOS (EPS) * (DPSI / 15.D0)
   10 CONTINUE
      ST = EQEQ - 6.2D-6*T3 + 0.093104D0*T2
     &   + 8640184.812866D0*TL + 3155760000.D0*TL
     &   + 8640184.812866D0*TH + 3155760000.D0*TH + 67310.54841D0
      GST = DMOD (ST / 3600.D0, 24.D0)
      IF (GST .LT. 0.D0) GST = GST + 24.D0
      RETURN
      END
C
C-----------------------------------------------------------------------
C  NOD      [037E:2174]  nutation in longitude and obliquity (arcsec),
C           IAU 1980 theory.  Single-precision series summed from the
C           smallest term upward; multiple-angle tables built from the
C           cosines/sines of the five fundamental arguments.
C           In low-accuracy mode (IACC .GE. 2) only 4 terms are used.
C-----------------------------------------------------------------------
      SUBROUTINE NOD (T, DPSI, DEPS)
      DOUBLE PRECISION T, DPSI, DEPS, SECCON, EL, ELP, F, D, OM
      REAL CA(5), SA(5), CC(5,-4:4), SS(5,-4:4), X, Y, X2, Y2, XY,
     &     TT, A, B, CS, SN, CC1, SC1
      REAL XC(212), XT(40)
      INTEGER*1 MULT(530)
      INTEGER*4 I, J, K, M, NTERMS, N1
      INTEGER*4 IACC
      LOGICAL LFILE
      COMMON /ACCUR/ IACC, LFILE
      SAVE
      DATA SECCON / 206264.8062470964D0 /
$INCLUDE:'nod_data.inc'
C
      EL = ((0.064D0*T + 31.310D0)*T + 715922.633D0)*T
     &   + DMOD (1325.D0*T, 1.D0) * 1296000.D0 + 485866.733D0
      EL = DMOD (EL, 1296000.D0) / SECCON
      CA(1) = DCOS (EL)
      SA(1) = DSIN (EL)
      ELP = ((-0.012D0*T - 0.577D0)*T + 1292581.224D0)*T
     &   + DMOD (99.D0*T, 1.D0) * 1296000.D0 + 1287099.804D0
      ELP = DMOD (ELP, 1296000.D0) / SECCON
      CA(2) = DCOS (ELP)
      SA(2) = DSIN (ELP)
      F = ((0.011D0*T - 13.257D0)*T + 295263.137D0)*T
     &   + DMOD (1342.D0*T, 1.D0) * 1296000.D0 + 335778.877D0
      F = DMOD (F, 1296000.D0) / SECCON
      CA(3) = DCOS (F)
      SA(3) = DSIN (F)
      D = ((0.019D0*T - 6.891D0)*T + 1105601.328D0)*T
     &   + DMOD (1236.D0*T, 1.D0) * 1296000.D0 + 1072261.307D0
      D = DMOD (D, 1296000.D0) / SECCON
      CA(4) = DCOS (D)
      SA(4) = DSIN (D)
      OM = ((0.008D0*T + 7.455D0)*T - 482890.539D0)*T
     &   - DMOD (5.D0*T, 1.D0) * 1296000.D0 + 450160.28D0
      OM = DMOD (OM, 1296000.D0) / SECCON
      CA(5) = DCOS (OM)
      SA(5) = DSIN (OM)
C
C     multiple-angle tables  CC(I,M) = COS(M*ARG(I)), SS(I,M) = SIN(..)
      DO 10 I = 1, 5
         X = CA(I)
         Y = SA(I)
         X2 = X * X
         Y2 = Y * Y
         XY = Y * X
         CC(I,0) = 1.0
         SS(I,0) = 0.0
         CC(I,1) = X
         SS(I,1) = Y
         CC(I,-1) = X
         SS(I,-1) = -Y
         CC(I,2) = 1.0 - Y2 * 2.0
         SS(I,2) = XY * 2.0
         CC(I,-2) = 1.0 - Y2 * 2.0
         SS(I,-2) = -SS(I,2)
         IF (I .EQ. 1) THEN
            CC(I,3) = (X2 * 4.0 + (-3.0)) * X
            SS(I,3) = (3.0 - Y2 * 4.0) * Y
         END IF
         IF (I .LE. 2) GOTO 10
         CC(I,4) = (X2 + (-1.0)) * X2 * 8.0 + 1.0
         SS(I,4) = (1.0 - Y2 * 2.0) * XY * 4.0
         CC(I,-4) = CC(I,4)
         SS(I,-4) = -SS(I,4)
   10 CONTINUE
C
      NTERMS = 106
      IF (IACC .GE. 2) NTERMS = 4
      N1 = NTERMS + 1
      TT = T
      DPSI = 0.D0
      DEPS = 0.D0
      DO 50 J = 1, NTERMS
         K = N1 - J
         SN = 0.0
         CS = 1.0
         DO 40 I = 1, 5
            M = MULT(5*(K-1)+I)
            IF (M .EQ. 0) GOTO 40
            CC1 = CC(I,M) * CS - SS(I,M) * SN
            SC1 = CC(I,M) * SN + SS(I,M) * CS
            CS = CC1
            SN = SC1
   40    CONTINUE
         A = XC(2*K-1)
         B = XC(2*K)
         IF (K .LE. 20) THEN
            A = XT(2*K-1) * TT + A
            B = XT(2*K) * TT + B
         END IF
         DEPS = A * CS + DEPS
         DPSI = B * SN + DPSI
   50 CONTINUE
      DPSI = 1.0D-4 * DPSI
      DEPS = 1.0D-4 * DEPS
      RETURN
      END
C
C-----------------------------------------------------------------------
C  PRECES   [037E:1590]  precession J2000-based (IAU 1976, Lieske)
C           from epoch TJD1 to epoch TJD2.  The rotation matrix is kept
C           and reused while the epochs are unchanged (to 1.D-6 day),
C           or within one day when the low-accuracy option is set.
C-----------------------------------------------------------------------
      SUBROUTINE PRECES (TJD1, POS1, TJD2, POS2)
      DOUBLE PRECISION TJD1, POS1(3), TJD2, POS2(3), TLAST1, TLAST2,
     &     T0, T, T02, T2, T3, ZETA0, ZEE, THETA, CZETA0, SZETA0,
     &     CZEE, SZEE, CTHETA, STHETA,
     &     XX, YX, ZX, XY, YY, ZY, XZ, YZ, ZZ
      INTEGER*4 IACC
      LOGICAL LFILE
      COMMON /ACCUR/ IACC, LFILE
      SAVE
      DATA TLAST1, TLAST2 / 0.D0, 0.D0 /
      IF (DABS (TJD1 - TLAST1) .LT. 1.D-6 .AND.
     &    DABS (TJD2 - TLAST2) .LT. 1.D-6) GOTO 20
      IF (IACC .GE. 2 .AND. DABS (TJD1 - TLAST1) .LT. 1.D0 .AND.
     &    DABS (TJD2 - TLAST2) .LT. 1.D0) GOTO 20
      T0 = (TJD1 - 2451545.D0) / 36525.D0
      T = (TJD2 - TJD1) / 36525.D0
      T02 = T0 * T0
      T2 = T * T
      T3 = T2 * T
      ZETA0 = (2306.2181D0 + 1.39656D0*T0 - 0.000139D0*T02) * T
     &      + (0.30188D0 - 0.000344D0*T0) * T2 + 0.017998D0 * T3
      ZEE   = (2306.2181D0 + 1.39656D0*T0 - 0.000139D0*T02) * T
     &      + (1.09468D0 + 0.000066D0*T0) * T2 + 0.018203D0 * T3
      THETA = (2004.3109D0 - 0.85330D0*T0 - 0.000217D0*T02) * T
     &      + (-0.42665D0 - 0.000217D0*T0) * T2 - 0.041833D0 * T3
      ZETA0 = ZETA0 / 206264.8062470964D0
      ZEE = ZEE / 206264.8062470964D0
      THETA = THETA / 206264.8062470964D0
      CZETA0 = DCOS (ZETA0)
      SZETA0 = DSIN (ZETA0)
      CZEE = DCOS (ZEE)
      SZEE = DSIN (ZEE)
      CTHETA = DCOS (THETA)
      STHETA = DSIN (THETA)
      XX =  CZETA0 * CTHETA * CZEE - SZETA0 * SZEE
      YX = -SZETA0 * CTHETA * CZEE - CZETA0 * SZEE
      ZX = -STHETA * CZEE
      XY =  CZETA0 * CTHETA * SZEE + SZETA0 * CZEE
      YY = -SZETA0 * CTHETA * SZEE + CZETA0 * CZEE
      ZY = -STHETA * SZEE
      XZ =  CZETA0 * STHETA
      YZ = -SZETA0 * STHETA
      ZZ =  CTHETA
      TLAST1 = TJD1
      TLAST2 = TJD2
   20 POS2(1) = XX * POS1(1) + YX * POS1(2) + ZX * POS1(3)
      POS2(2) = XY * POS1(1) + YY * POS1(2) + ZY * POS1(3)
      POS2(3) = XZ * POS1(1) + YZ * POS1(2) + ZZ * POS1(3)
      RETURN
      END
C
C-----------------------------------------------------------------------
C  NUTATE   [037E:19E4]  mean equator & equinox of date -> true
C-----------------------------------------------------------------------
      SUBROUTINE NUTATE (TJD, POS1, POS2)
      DOUBLE PRECISION TJD, POS1(3), POS2(3), TLAST, SECCON, T, T2, T3,
     &     OBLM, OBLT, DPSI, DEPS, COBM, SOBM, COBT, SOBT, CPSI, SPSI,
     &     XX, YX, ZX, XY, YY, ZY, XZ, YZ, ZZ
      INTEGER*4 IACC
      LOGICAL LFILE
      COMMON /ACCUR/ IACC, LFILE
      SAVE
      DATA TLAST, SECCON / 0.D0, 206264.8062470964D0 /
      IF (IACC .GE. 2 .AND. DABS (TJD - TLAST) .LT. 1.D0) GOTO 20
      T = (TJD - 2451545.D0) / 36525.D0
      T2 = T * T
      T3 = T2 * T
      OBLM = (84381.448D0 - 46.8150D0*T - 0.00059D0*T2
     &     + 0.001813D0*T3) / SECCON
      CALL NOD (T, DPSI, DEPS)
      DPSI = DPSI / SECCON
      DEPS = DEPS / SECCON
      OBLT = OBLM + DEPS
      COBM = DCOS (OBLM)
      SOBM = DSIN (OBLM)
      COBT = DCOS (OBLT)
      SOBT = DSIN (OBLT)
      CPSI = DCOS (DPSI)
      SPSI = DSIN (DPSI)
      XX =  CPSI
      YX = -SPSI * COBM
      ZX = -SPSI * SOBM
      XY =  SPSI * COBT
      YY =  CPSI * COBM * COBT + SOBM * SOBT
      ZY =  CPSI * SOBM * COBT - COBM * SOBT
      XZ =  SPSI * SOBT
      YZ =  CPSI * COBM * SOBT - SOBM * COBT
      ZZ =  CPSI * SOBM * SOBT + COBM * COBT
      TLAST = TJD
   20 POS2(1) = XX * POS1(1) + YX * POS1(2) + ZX * POS1(3)
      POS2(2) = XY * POS1(1) + YY * POS1(2) + ZY * POS1(3)
      POS2(3) = XZ * POS1(1) + YZ * POS1(2) + ZZ * POS1(3)
      RETURN
      END
C
C-----------------------------------------------------------------------
C  SUNFLD   [037E:1CFF]  gravitational light deflection by the Sun
C           POS1 geocentric position, PE heliocentric earth, POS2 out.
C           Skipped (POS2 = POS1) in low-accuracy mode.
C-----------------------------------------------------------------------
      SUBROUTINE SUNFLD (POS1, PE, POS2)
      DOUBLE PRECISION POS1(3), PE(3), POS2(3), Q(3), PHAT(3), EHAT(3),
     &     QHAT(3), PMAG, EMAG, QMAG, PDOTQ, PDOTE, QDOTE, FAC1, FAC2,
     &     P2J, AU, GS, C
      INTEGER*4 J, IACC
      LOGICAL LFILE
      COMMON /ACCUR/ IACC, LFILE
      SAVE
      DATA AU, GS, C / 1.49597870D11, 1.32712438D20, 299792458.D0 /
      IF (IACC .GE. 2) GOTO 50
      DO 10 J = 1, 3
         Q(J) = PE(J) + POS1(J)
   10 CONTINUE
      PMAG = DSQRT (POS1(1)**2 + POS1(2)**2 + POS1(3)**2)
      EMAG = DSQRT (PE(1)**2 + PE(2)**2 + PE(3)**2)
      QMAG = DSQRT (Q(1)**2 + Q(2)**2 + Q(3)**2)
      IF (QMAG .LT. 1.D-6) GOTO 50
      DO 20 J = 1, 3
         PHAT(J) = POS1(J) / PMAG
         EHAT(J) = PE(J) / EMAG
         QHAT(J) = Q(J) / QMAG
   20 CONTINUE
      PDOTQ = PHAT(1)*QHAT(1) + PHAT(2)*QHAT(2) + PHAT(3)*QHAT(3)
      PDOTE = EHAT(1)*PHAT(1) + EHAT(2)*PHAT(2) + EHAT(3)*PHAT(3)
      QDOTE = QHAT(1)*EHAT(1) + QHAT(2)*EHAT(2) + QHAT(3)*EHAT(3)
      IF (QDOTE .LT. -0.9999999999D0) GOTO 50
      FAC1 = GS * 2.D0 / (C * C * EMAG * AU)
      FAC2 = QDOTE + 1.D0
      DO 30 J = 1, 3
         P2J = (EHAT(J) * PDOTQ - QHAT(J) * PDOTE) * FAC1 / FAC2
     &       + PHAT(J)
         POS2(J) = P2J * PMAG
   30 CONTINUE
      GOTO 60
   50 POS2(1) = POS1(1)
      POS2(2) = POS1(2)
      POS2(3) = POS1(3)
   60 RETURN
      END
C
C-----------------------------------------------------------------------
C  REFR     [037E:281C]  refraction (degrees) for altitude H (degrees)
C           Bennett's formula, iterated from the true altitude.
C-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION REFR (H)
      DOUBLE PRECISION H
      REAL HA, R
      SAVE
      HA = H
      R = 0.0
      IF (HA .LT. 4.) THEN
         R = 32.5 - HA * 6.5
         HA = H + R / 60.
      END IF
      IF (HA .LT. 16.) THEN
         R = 1. / ((7.31 / (HA + 4.4) + HA) * 0.01745329)
         HA = H + R / 60.
      END IF
      IF (HA .LT. 87.) R = 1. / TAN ((7.31 / (HA + 4.4) + HA)
     &                                * 0.01745329)
      REFR = R / 60.
      RETURN
      END
C
C-----------------------------------------------------------------------
C  APSTAR   [037E:039B]  apparent place of a star
C           TJD  TDT Julian date, N = body number of the Earth,
C           catalog J2000 data (RAM,DECM,PMRA,PMDEC,PARLAX,RADVEL),
C           RA, DEC apparent place (hours, degrees); 0,0 on error.
C-----------------------------------------------------------------------
      SUBROUTINE APSTAR (TJD, N, RAM, DECM, PMRA, PMDEC, PARLAX,
     &                   RADVEL, RA, DEC)
      DOUBLE PRECISION TJD, RAM, DECM, PMRA, PMDEC, PARLAX, RADVEL,
     &     RA, DEC, T0, TJD2, PEB(3), VEB(3), PES(3), VES(3), POS1(3),
     &     VEL1(3), POS2(3), POS3(3), POS4(3), POS5(3), POS6(3),
     &     POS7(3), TLIGHT, R, D, PMR, PMD, PRL, RVL
      INTEGER*4 N, IERR, K, IACC
      LOGICAL LFILE
      COMMON /ACCUR/ IACC, LFILE
      SAVE
      DATA T0 / 2451545.D0 /
      TJD2 = TJD
      K = 0
      CALL SOLSYS (TJD2, N, K, PEB, VEB, IERR)
      IF (IERR .NE. 0) GOTO 90
      IF (IACC .LT. 2) THEN
         K = 1
         CALL SOLSYS (TJD2, N, K, PES, VES, IERR)
         IF (IERR .NE. 0) GOTO 90
      END IF
      R = RAM
      D = DECM
      PMR = PMRA
      PMD = PMDEC
      PRL = PARLAX
      RVL = RADVEL
      CALL VECTRS (R, D, PMR, PMD, PRL, RVL, POS1, VEL1)
      CALL PROPMO (T0, POS1, VEL1, TJD2, POS2)
      CALL GEOCEN (POS2, PEB, POS3, TLIGHT)
      CALL SUNFLD (POS3, PES, POS4)
      CALL ABERAT (POS4, VEB, TLIGHT, POS5)
      CALL PRECES (T0, POS5, TJD2, POS6)
      CALL NUTATE (TJD2, POS6, POS7)
      CALL ANGLES (POS7, R, D)
      RA = R
      DEC = D
      GOTO 99
   90 RA = 0.D0
      DEC = 0.D0
   99 RETURN
      END
C
C-----------------------------------------------------------------------
C  APPLAN   [037E:05CA]  apparent place of a planet (or Sun, Moon)
C           TJD TDT Julian date, L body, N earth; RA, DEC, DIS (AU)
C-----------------------------------------------------------------------
      SUBROUTINE APPLAN (TJD, L, N, RA, DEC, DIS)
      DOUBLE PRECISION TJD, RA, DEC, DIS, T0, C, TDB, T2, T3, TDTJD,
     &     SECDIF, PEB(3), VEB(3), PES(3), VES(3), POS1(3), VEL1(3),
     &     POS2(3), POS3(3), POS4(3), POS5(3), POS6(3), TLIGHT, R, D,
     &     DS
      INTEGER*4 L, N, IERR, K, IACC
      LOGICAL LFILE
      COMMON /ACCUR/ IACC, LFILE
      SAVE
      DATA T0, C / 2451545.D0, 173.1446333D0 /
      CALL TIMES (TJD, TDTJD, SECDIF)
      TDB = TJD + SECDIF / 86400.D0
      K = 0
      CALL SOLSYS (TDB, N, K, PEB, VEB, IERR)
      IF (IERR .NE. 0) GOTO 90
      IF (IACC .LT. 2) THEN
         K = 1
         CALL SOLSYS (TDB, N, K, PES, VES, IERR)
         IF (IERR .NE. 0) GOTO 90
      END IF
      K = 0
      CALL SOLSYS (TDB, L, K, POS1, VEL1, IERR)
      IF (IERR .NE. 0) GOTO 90
      CALL GEOCEN (POS1, PEB, POS2, TLIGHT)
      DS = TLIGHT * C
      T3 = TDB - TLIGHT
C     iterate for light time (single pass in low-accuracy mode)
   10 K = 0
      CALL SOLSYS (T3, L, K, POS1, VEL1, IERR)
      IF (IERR .NE. 0) GOTO 90
      CALL GEOCEN (POS1, PEB, POS2, TLIGHT)
      T2 = TDB - TLIGHT
      IF (DABS (T2 - T3) .LT. 1.D-8) GOTO 20
      IF (IACC .EQ. 2) GOTO 20
      T3 = T2
      GOTO 10
   20 CALL SUNFLD (POS2, PES, POS3)
      CALL ABERAT (POS3, VEB, TLIGHT, POS4)
      CALL PRECES (T0, POS4, TDB, POS5)
      CALL NUTATE (TDB, POS5, POS6)
      CALL ANGLES (POS6, R, D)
      RA = R
      DEC = D
      DIS = DS
      GOTO 99
   90 RA = 0.D0
      DEC = 0.D0
      DIS = 0.D0
   99 RETURN
      END
C
C-----------------------------------------------------------------------
C  ASSTAR   [037E:085A]  astrometric place of a star (J2000 frame)
C-----------------------------------------------------------------------
      SUBROUTINE ASSTAR (TJD, N, RAM, DECM, PMRA, PMDEC, PARLAX,
     &                   RADVEL, RA, DEC)
      DOUBLE PRECISION TJD, RAM, DECM, PMRA, PMDEC, PARLAX, RADVEL,
     &     RA, DEC, T0, TJD2, PEB(3), VEB(3), POS1(3), VEL1(3),
     &     POS2(3), POS3(3), TLIGHT, R, D, PMR, PMD, PRL, RVL
      INTEGER*4 N, IERR, K
      SAVE
      DATA T0 / 2451545.D0 /
      TJD2 = TJD
      K = 0
      CALL SOLSYS (TJD2, N, K, PEB, VEB, IERR)
      IF (IERR .NE. 0) GOTO 90
      R = RAM
      D = DECM
      PMR = PMRA
      PMD = PMDEC
      PRL = PARLAX
      RVL = RADVEL
      CALL VECTRS (R, D, PMR, PMD, PRL, RVL, POS1, VEL1)
      CALL PROPMO (T0, POS1, VEL1, TJD2, POS2)
      CALL GEOCEN (POS2, PEB, POS3, TLIGHT)
      CALL ANGLES (POS3, R, D)
      RA = R
      DEC = D
      GOTO 99
   90 RA = 0.D0
      DEC = 0.D0
   99 RETURN
      END
C
C-----------------------------------------------------------------------
C  ASPLAN   [037E:09DF]  astrometric place of a planet (J2000 frame)
C-----------------------------------------------------------------------
      SUBROUTINE ASPLAN (TJD, L, N, RA, DEC, DIS)
      DOUBLE PRECISION TJD, RA, DEC, DIS, C, TDB, T2, T3, TDTJD,
     &     SECDIF, PEB(3), VEB(3), POS1(3), VEL1(3), POS2(3), TLIGHT,
     &     R, D, DS
      INTEGER*4 L, N, IERR, K, IACC
      LOGICAL LFILE
      COMMON /ACCUR/ IACC, LFILE
      SAVE
      DATA C / 173.1446333D0 /
      CALL TIMES (TJD, TDTJD, SECDIF)
      TDB = TJD + SECDIF / 86400.D0
      K = 0
      CALL SOLSYS (TDB, N, K, PEB, VEB, IERR)
      IF (IERR .NE. 0) GOTO 90
      K = 0
      CALL SOLSYS (TDB, L, K, POS1, VEL1, IERR)
      IF (IERR .NE. 0) GOTO 90
      CALL GEOCEN (POS1, PEB, POS2, TLIGHT)
      DS = TLIGHT * C
      T3 = TDB - TLIGHT
   10 K = 0
      CALL SOLSYS (T3, L, K, POS1, VEL1, IERR)
      IF (IERR .NE. 0) GOTO 90
      CALL GEOCEN (POS1, PEB, POS2, TLIGHT)
      T2 = TDB - TLIGHT
      IF (DABS (T2 - T3) .LT. 1.D-8) GOTO 20
      IF (IACC .EQ. 2) GOTO 20
      T3 = T2
      GOTO 10
   20 CALL ANGLES (POS2, R, D)
      RA = R
      DEC = D
      DIS = DS
      GOTO 99
   90 RA = 0.D0
      DEC = 0.D0
      DIS = 0.D0
   99 RETURN
      END
C
C-----------------------------------------------------------------------
C  PLACE    [037E:0007]  position of body NUM at TDT date TJD
C           NUM = 1..11  Mercury..Pluto, Sun, Moon (ephemeris body)
C           NUM .LT. 0   star number -NUM of the current catalog
C           KIND = 1 apparent place, 2 astrometric (J2000) place
C           Returns RA (h), DEC (deg), DIS (AU; parallax for stars),
C           HP horizontal parallax and SD semidiameter (arcsec),
C           VMAG visual magnitude (stars), IERR = 1 if no data.
C-----------------------------------------------------------------------
      SUBROUTINE PLACE (TJD, KIND, NUM, RA, DEC, DIS, HP, SD, VMAG,
     &                  IERR)
      DOUBLE PRECISION TJD, RA, DEC, DIS, HP, SD, VMAG, RADIUS(11),
     &     D, RD, X, PRL, RVL
      INTEGER*4 KIND, NUM, IERR, NEARTH, NB, IACC
      LOGICAL LFILE
      COMMON /ACCUR/ IACC, LFILE
$INCLUDE:'catlg.inc'
      SAVE
      DATA NEARTH / 3 /
C     semidiameters at unit distance (arcsec): Mercury..Pluto, Sun, Moon
      DATA RADIUS / 3.3629D0, 8.3445D0, 8.7941D0, 4.6788D0, 98.4432D0,
     &     82.7277D0, 35.0214D0, 33.5047D0, 2.0682D0, 959.6414D0,
     &     2.39634584D0 /
      IERR = 0
      IF (NUM .LT. 1 .OR. NUM .GT. 11) GOTO 50
      NB = NUM
      IF (KIND .LE. 1) CALL APPLAN (TJD, NB, NEARTH, RA, DEC, DIS)
      IF (KIND .GE. 2) CALL ASPLAN (TJD, NB, NEARTH, RA, DEC, DIS)
      IF (RA .EQ. 0.D0 .AND. DEC .EQ. 0.D0) THEN
         IERR = 1
         RETURN
      END IF
      D = DIS
      HP = 8.794148D0 / D
      RD = RADIUS(NB)
      SD = RD / D
      VMAG = 0.D0
      IF (IACC .NE. 1) RETURN
      IF (D .GE. 1.1D0) RETURN
C     rigorous formulae for nearby bodies, full-accuracy mode
      X = D * 206264.8062471D0
      HP = DASIN (8.794148D0 / X) * 206264.8062471D0
      SD = DASIN (RD / X) * 206264.8062471D0
      RETURN
C
   50 IF (NUM .GT. 0) GOTO 80
      NB = -NUM
      PRL = CPARX(NB)
      RVL = CRV(NB)
      VMAG = CMAG(NB)
      IF (KIND .LE. 1) CALL APSTAR (TJD, NEARTH, CRA(NB), CDEC(NB),
     &   CPMRA(NB), CPMDEC(NB), PRL, RVL, RA, DEC)
      IF (KIND .GE. 2) CALL ASSTAR (TJD, NEARTH, CRA(NB), CDEC(NB),
     &   CPMRA(NB), CPMDEC(NB), PRL, RVL, RA, DEC)
      DIS = PRL
      HP = 0.D0
      SD = 0.D0
      RETURN
   80 RA = 0.D0
      DEC = 0.D0
      DIS = 0.D0
      HP = 0.D0
      SD = 0.D0
      VMAG = 0.D0
      RETURN
      END
