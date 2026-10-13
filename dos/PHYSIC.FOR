C=======================================================================
C  ICE 0.51  --  recovered source, module 06B6 (part 1)
C  Physical ephemeris of the planets and the Moon: apparent diameter,
C  phase, illuminated fraction, defect of illumination, magnitude,
C  central meridian, sub-earth and sub-solar points, position angles.
C  Rotation elements are those of the IAU working group (1985).
C=======================================================================
C
      BLOCK DATA PHYBD
      DOUBLE PRECISION PI, TWOPI, DEGRAD, RADDEG, SECRAD, T0, AUKM, CKMS
      COMMON /CONST/ PI, TWOPI, DEGRAD, RADDEG, SECRAD, T0, AUKM, CKMS
      DATA PI, TWOPI / 3.141592653589793D0, 6.283185307179586D0 /
      DATA DEGRAD, RADDEG / 0.01745329252D0, 57.29577951D0 /
      DATA SECRAD, T0 / 206264.8062D0, 2451545.D0 /
      DATA AUKM, CKMS / 149597870.D0, 299792.458D0 /
      END
C
C-----------------------------------------------------------------------
C  PHYSEP   [06B6:000E]  physical ephemeris of body NB at TDB date TJD
C           TLT    light time (minutes)
C           VMAG   visual magnitude      SBR   surface brightness
C           DE,DP  equatorial and polar diameter (arcsec)
C           FILL   illuminated fraction  PHASE phase angle (deg)
C           DEFECT defect of illumination (arcsec)
C           CMLE,BE  longitude (central meridian) and latitude of the
C                  sub-earth point; CMLS,BS  of the sub-solar point
C           DSUN,PASUN   distance (arcsec) and position angle of the
C                  sub-solar point; DPOLE,PAPOLE  same for the N pole
C-----------------------------------------------------------------------
      SUBROUTINE PHYSEP (TJD, NB, TLT, VMAG, SBR, DE, DP, FILL, PHASE,
     &     DEFECT, CMLE, BE, CMLS, BS, DSUN, PASUN, DPOLE, PAPOLE,
     &     IERR)
      DOUBLE PRECISION TJD, TLT, VMAG, SBR, DE, DP, FILL, PHASE,
     &     DEFECT, CMLE, BE, CMLS, BS, DSUN, PASUN, DPOLE, PAPOLE
      INTEGER*4 NB, IERR
      DOUBLE PRECISION PI, TWOPI, DEGRAD, RADDEG, SECRAD, T0, AUKM, CKMS
      COMMON /CONST/ PI, TWOPI, DEGRAD, RADDEG, SECRAD, T0, AUKM, CKMS
      DOUBLE PRECISION PE(3), VE(3), PP(3), VP(3), G(3), DIST, DKM,
     &     TL, P1(3), PPD(3), PED(3), GKM(3), DGEO, UH(3), UG(3), RN,
     &     COSP, RE, RP, F, A0, D0, W, SGN, POLE0(3), POLE(3), X, Y,
     &     Z, CW, SD, CD, ALPM, DELM, PM0(3), PM(3), SINS, SINE,
     &     PV(3), PVP(3), RS, SV(3), SVP(3), RG, GV(3), GVP(3), Q1, Q2,
     &     ELL, ANG, RH, RGA
      DOUBLE PRECISION DOT, VNORM
      SAVE
      CALL SOLSYS (TJD, 3, 1, PE, VE, IERR)
      CALL SOLSYS (TJD, NB, 1, PP, VP, IERR)
      IF (IERR .NE. 0) GO TO 99
C     light time, first approximation
      CALL VSUB (PP, PE, G)
      DIST = VNORM (G)
      DKM = DIST * AUKM
      TLT = DKM / CKMS / 60.D0
      TL = TJD - TLT / 1440.D0
      CALL SOLSYS (TL, 3, 1, PE, VE, IERR)
      CALL SOLSYS (TL, NB, 1, PP, VP, IERR)
C     heliocentric vectors of date, km
      CALL PRECES (T0, PP, TJD, P1)
      CALL NUTATE (TJD, P1, PPD)
      CALL PRECES (T0, PE, TJD, P1)
      CALL NUTATE (TJD, P1, PED)
      CALL VSCALE (AUKM, PPD, PPD)
      CALL VSCALE (AUKM, PED, PED)
      CALL VSUB (PPD, PED, GKM)
      DGEO = VNORM (GKM)
      TLT = DGEO / CKMS / 60.D0
      TL = TJD - TLT / 1440.D0
C     unit vectors Sun->planet and Earth->planet; phase
      RN = 1.D0 / VNORM (PPD)
      CALL VSCALE (RN, PPD, UH)
      RN = 1.D0 / VNORM (GKM)
      CALL VSCALE (RN, GKM, UG)
      COSP = DOT (UH, UG)
      PHASE = DACOS (COSP) * RADDEG
      FILL = (COSP + 1.D0) * 0.5D0
C     north pole and prime meridian, true equator and equinox of date
      CALL ROTEL (TL, NB, RE, RP, F, A0, D0, W, SGN)
      A0 = DEGRAD * A0
      D0 = DEGRAD * D0
      X = DCOS (A0) * DCOS (D0)
      Y = DSIN (A0) * DCOS (D0)
      Z = DSIN (D0)
      CALL VSET (X, Y, Z, POLE0)
      CALL PRECES (T0, POLE0, TJD, P1)
      CALL NUTATE (TJD, P1, POLE)
      W = DMOD (DEGRAD * W, TWOPI)
      SD = DCOS (D0) * DSIN (W)
      CD = DSQRT (1.D0 - SD*SD)
      X = DSIN (D0) * DSIN (W) / CD
      CW = DCOS (W) / CD
      ALPM = PI / 2.D0 + DATAN2 (X, CW) + A0
      DELM = DATAN2 (SD, CD)
      X = DCOS (ALPM) * DCOS (DELM)
      Y = DSIN (ALPM) * DCOS (DELM)
      Z = DSIN (DELM)
      CALL VSET (X, Y, Z, PM0)
      CALL PRECES (T0, PM0, TJD, P1)
      CALL NUTATE (TJD, P1, PM)
C     planetocentric declinations of the Sun and of the Earth
      SINS = -DOT (UH, POLE)
      SINE = -DOT (UG, POLE)
C     semidiameters
      DE = DASIN (RE / DGEO) * SECRAD
      DP = (1.D0 - (1.D0 - SINE*SINE) * F) * DE
C     projections on the sky (frame of the line of sight)
      CALL FRAME (UG)
      CALL VSCALE (RP, POLE, PV)
      CALL PROJ (DGEO, PV, PVP, PAPOLE, DPOLE)
      RS = (1.D0 - SINS*SINS*F) * RE
      X = -RS
      CALL VSCALE (X, UH, SV)
      CALL PROJ (DGEO, SV, SVP, PASUN, DSUN)
      RG = (1.D0 - SINE*SINE*F) * RE
      X = -RG
      CALL VSCALE (X, UG, GV)
      CALL PROJ (DGEO, GV, GVP, Q1, Q2)
C     defect of illumination
      ELL = 1.D0 - DP / DE
      ANG = (PASUN - PAPOLE + 90.D0) * DEGRAD
      DEFECT = (1.D0 - ELL*DSIN(ANG)*DSIN(ANG)) * (1.D0 - FILL) * DE
     &         * 2.D0
C     sub-solar and sub-earth points
      CALL VSCALE (-1.D0, UH, UH)
      CALL VSCALE (-1.D0, UG, UG)
      CALL PLCOOR (PM, POLE, UH, F, CMLS, BS)
      CALL PLCOOR (PM, POLE, UG, F, CMLE, BE)
      IF (CMLS .LT. 0.D0) CMLS = 360.D0 + CMLS
      IF (CMLE .LT. 0.D0) CMLE = 360.D0 + CMLE
      IF (SGN .GT. 0.D0) CMLS = 360.D0 - CMLS
      IF (SGN .GT. 0.D0) CMLE = 360.D0 - CMLE
C     magnitude
      RH = VNORM (PPD) / AUKM
      RGA = VNORM (GKM) / AUKM
      CALL PMAG (NB, RH, RGA, PHASE, DE, DP, FILL, BE, VMAG, SBR)
      DGEO = DGEO / AUKM
      DE = 2.D0 * DE
      DP = 2.D0 * DP
   99 RETURN
      END
C
C-----------------------------------------------------------------------
C  ROTEL    [06B6:0906]  IAU rotation elements of body NB at TJD:
C           RE, RP equatorial/polar radius (km), F flattening,
C           A0, D0 north pole (deg), W prime meridian (deg),
C           SGN +1 direct, -1 retrograde rotation
C-----------------------------------------------------------------------
      SUBROUTINE ROTEL (TJD, NB, RE, RP, F, A0, D0, W, SGN)
      DOUBLE PRECISION TJD, RE, RP, F, A0, D0, W, SGN
      INTEGER*4 NB
      DOUBLE PRECISION PI, TWOPI, DEGRAD, RADDEG, SECRAD, T0, AUKM, CKMS
      COMMON /CONST/ PI, TWOPI, DEGRAD, RADDEG, SECRAD, T0, AUKM, CKMS
      DOUBLE PRECISION D, T, E1, E2, E3, E4, E5
      SAVE
      D = TJD - 2451545.D0
      T = D / 36525.D0
      GO TO (10, 20, 30, 30, 50, 60, 70, 80, 90, 210, 110), NB
C     Mercury
   10 A0 = 281.0225D0 - T*0.0326D0
      D0 = 61.4505D0 - T*0.0052D0
      F = 0.D0
      RE = 2439.D0
      W = D*6.1385025D0 + 329.7132D0
      GO TO 200
C     Venus
   20 A0 = 272.7791D0
      D0 = 67.2135D0
      F = 0.D0
      RE = 6051.D0
      W = 159.9058D0 - D*1.4814205D0
      GO TO 205
C     Mars (also for NB = 3)
   30 A0 = 317.681D0 - T*0.1075D0
      D0 = 52.8858D0 - T*0.0615D0
      F = 0.0051865D0
      RE = 3393.4D0
      W = D*350.891983D0 + 176.6505D0
      GO TO 200
C     Jupiter (System III)
   50 A0 = 268.0536D0 - T*0.0078D0
      D0 = T*0.003D0 + 64.4919D0
      F = 0.0648088D0
      RE = 71398.D0
      W = D*870.536D0 + 284.9461D0
      GO TO 200
C     Saturn
   60 A0 = 40.6609D0 - T*0.0366D0
      D0 = 83.5225D0 - T*0.004D0
      F = 0.1076209D0
      RE = 60000.D0
      W = D*810.7939023635D0 + 38.9016D0
      GO TO 200
C     Uranus
   70 A0 = 257.4338D0
      D0 = -15.1023D0
      F = 0.03D0
      RE = 25400.D0
      W = 261.6185D0 - D*554.913D0
      GO TO 205
C     Neptune
   80 A0 = 295.3348D0
      D0 = 40.6482D0
      F = 0.0259D0
      RE = 24300.D0
      W = D*468.75D0 + 107.2069D0
      GO TO 200
C     Pluto
   90 A0 = 311.6257D0
      D0 = 4.1838D0
      W = 252.6596D0 - D*56.364D0
      F = 0.D0
      RE = 1500.D0
      GO TO 205
C     Moon
  110 E1 = (125.045D0 - D*0.052992D0) * DEGRAD
      E2 = (249.39D0 - D*0.105984D0) * DEGRAD
      E3 = (D*13.012D0 + 196.694D0) * DEGRAD
      E4 = (D*13.340716D0 + 176.63D0) * DEGRAD
      E5 = (D*0.9856D0 + 358.219D0) * DEGRAD
      A0 = DSIN(E3)*0.07D0 - (DSIN(E1)*3.878D0 + DSIN(E2)*0.12D0)
     &     - DSIN(E4)*0.017D0 + 270.D0
      D0 = DCOS(E1)*1.543D0 + DCOS(E2)*0.024D0 - DCOS(E3)*0.028D0
     &     + DCOS(E4)*0.007D0 + 66.534D0
      W = DSIN(E5)*0.025D0 + D*13.1763581D0 + DSIN(E1)*3.558D0
     &     + DSIN(E2)*0.121D0 - DSIN(E3)*0.064D0 + DSIN(E4)*0.016D0
     &     + 38.314D0
      F = 0.D0
      RE = 1738.D0
  200 SGN = 1.D0
      GO TO 210
  205 SGN = -1.D0
  210 RP = (1.D0 - F) * RE
      RETURN
      END
C
C-----------------------------------------------------------------------
C  PMAG     [06B6:0E22]  visual magnitude (Harris 1961 phase laws) and
C           surface brightness; R, DEL heliocentric and geocentric
C           distance (AU), PH phase angle (deg), B Saturnicentric
C           latitude of the Earth (ring correction)
C-----------------------------------------------------------------------
      SUBROUTINE PMAG (NB, R, DEL, PH, DE, DP, FILL, B, VMAG, SBR)
      INTEGER*4 NB
      DOUBLE PRECISION R, DEL, PH, DE, DP, FILL, B, VMAG, SBR
      DOUBLE PRECISION PI, TWOPI, DEGRAD, RADDEG, SECRAD, T0, AUKM, CKMS
      COMMON /CONST/ PI, TWOPI, DEGRAD, RADDEG, SECRAD, T0, AUKM, CKMS
      DOUBLE PRECISION X, DM, V0, FL, BP, SB
      SAVE
      IF (NB .EQ. 11) THEN
         VMAG = 1.D8
         SBR = 1.D8
         RETURN
      END IF
      GO TO (10, 20, 30, 30, 50, 60, 70, 80, 90, 99), NB
C     Mercury
   10 X = PH / 100.D0
      DM = ((X*2.D0 - 2.73D0)*X + 3.8D0)*X
      V0 = -0.42D0
      GO TO 95
C     Venus
   20 X = PH / 100.D0
      DM = ((-(X*0.65D0) + 2.39D0)*X + 0.09D0)*X
      V0 = -4.4D0
      GO TO 95
C     Mars
   30 DM = PH * 0.016D0
      V0 = -1.52D0
      GO TO 95
C     Jupiter
   50 DM = PH * 0.005D0
      V0 = -9.4D0
      GO TO 95
C     Saturn: the rings are seen at elevation BP
   60 DM = PH * 0.043D0
      V0 = -8.88D0
      FL = 0.1076209D0
      BP = DATAN (DTAN (DABS (B*DEGRAD)) * 0.79634045811681D0)
      SB = DSIN (BP)
      GO TO 96
C     Uranus
   70 DM = PH * 0.002D0
      V0 = -7.19D0
      GO TO 95
C     Neptune
   80 DM = 0.D0
      V0 = -6.87D0
      GO TO 95
C     Pluto
   90 DM = 0.D0
      V0 = -1.D0
   95 CONTINUE
   96 VMAG = DLOG10 (R*DEL)*5.D0 + V0 + DM
      SBR = DLOG10 (DE*DP*FILL*PI)*2.5D0 + VMAG
      IF (NB .EQ. 6) VMAG = VMAG + (SB*1.25D0 - 2.6D0)*SB
   99 RETURN
      END
C
C-----------------------------------------------------------------------
C  FRAME    [06B6:107A]  sky frame for line of sight U (unit vector):
C           ROT(.,3) toward the observer, ROT(.,2) north, ROT(.,1) east
C-----------------------------------------------------------------------
      SUBROUTINE FRAME (U)
      DOUBLE PRECISION U(3)
      DOUBLE PRECISION ROT(3,3)
      COMMON /SKYFR/ ROT
      DOUBLE PRECISION ZP(3), V(3), RN
      DOUBLE PRECISION VNORM
      SAVE
      CALL VSET (0.D0, 0.D0, 1.D0, ZP)
      CALL VSCALE (-1.D0, U, ROT(1,3))
      CALL CROSS (ROT(1,3), ZP, V)
      RN = 1.D0 / VNORM (V)
      CALL VSCALE (RN, V, ROT(1,2))
      CALL CROSS (ROT(1,2), ROT(1,3), ROT(1,1))
      RETURN
      END
C
C-----------------------------------------------------------------------
C  PROJ     [06B6:1120]  project vector V (km) at distance D (km) on
C           the sky frame: VP components, PA position angle (deg),
C           SEP distance from the disk centre (arcsec)
C-----------------------------------------------------------------------
      SUBROUTINE PROJ (D, V, VP, PA, SEP)
      DOUBLE PRECISION D, V(3), VP(3), PA, SEP
      DOUBLE PRECISION ROT(3,3)
      COMMON /SKYFR/ ROT
      DOUBLE PRECISION PI, TWOPI, DEGRAD, RADDEG, SECRAD, T0, AUKM, CKMS
      COMMON /CONST/ PI, TWOPI, DEGRAD, RADDEG, SECRAD, T0, AUKM, CKMS
      DOUBLE PRECISION X, Y, Z, RXY, DZ
      DOUBLE PRECISION DOT
      SAVE
      X = DOT (V, ROT(1,1))
      Y = DOT (V, ROT(1,2))
      Z = DOT (V, ROT(1,3))
      CALL VSET (X, Y, Z, VP)
      PA = 0.D0
      SEP = 0.D0
      RXY = DSQRT (X*X + Y*Y)
      DZ = D - Z
      IF (DZ .LT. 1.D-10) RETURN
      IF (RXY / DZ .LT. 1.D-10) RETURN
      PA = DATAN2 (Y, X) * RADDEG
      IF (PA .LT. 0.D0) PA = 360.D0 + PA
      SEP = RXY / DZ * SECRAD
      IF (Z .LT. 0.D0) SEP = -SEP
      RETURN
      END
C
C-----------------------------------------------------------------------
C  PLCOOR   [06B6:12B4]  planetographic longitude and latitude (deg)
C           of direction U; PM prime-meridian and POLE unit vectors,
C           F flattening
C-----------------------------------------------------------------------
      SUBROUTINE PLCOOR (PM, POLE, U, F, ALON, ALAT)
      DOUBLE PRECISION PM(3), POLE(3), U(3), F, ALON, ALAT
      DOUBLE PRECISION PI, TWOPI, DEGRAD, RADDEG, SECRAD, T0, AUKM, CKMS
      COMMON /CONST/ PI, TWOPI, DEGRAD, RADDEG, SECRAD, T0, AUKM, CKMS
      DOUBLE PRECISION Q(3), X, Y, Z, V(3), B
      DOUBLE PRECISION DOT, VNORM
      SAVE
      CALL CROSS (POLE, PM, Q)
      X = DOT (U, PM)
      Y = DOT (U, Q)
      Z = DOT (U, POLE)
      CALL VSET (X, Y, Z, V)
      ALON = DATAN2 (Y, X) * RADDEG
      B = DASIN (Z / VNORM (V))
      ALAT = DATAN (DTAN (B) / ((1.D0 - F)*(1.D0 - F))) * RADDEG
      RETURN
      END
C
C-----------------------------------------------------------------------
C  Vector utilities  [06B6:13D8 .. 06B6:16A1]
C-----------------------------------------------------------------------
C  DOT      [06B6:13D8]
      DOUBLE PRECISION FUNCTION DOT (A, B)
      DOUBLE PRECISION A(3), B(3)
      DOT = A(2)*B(2) + A(3)*B(3) + A(1)*B(1)
      RETURN
      END
C
C  VNORM    [06B6:145E]
      DOUBLE PRECISION FUNCTION VNORM (A)
      DOUBLE PRECISION A(3)
      VNORM = DSQRT (A(2)*A(2) + A(3)*A(3) + A(1)*A(1))
      RETURN
      END
C
C  VSET     [06B6:14EA]
      SUBROUTINE VSET (X, Y, Z, A)
      DOUBLE PRECISION X, Y, Z, A(3)
      A(1) = X
      A(2) = Y
      A(3) = Z
      RETURN
      END
C
C  VSUB     [06B6:1559]
      SUBROUTINE VSUB (A, B, C)
      DOUBLE PRECISION A(3), B(3), C(3)
      C(1) = A(1) - B(1)
      C(2) = A(2) - B(2)
      C(3) = A(3) - B(3)
      RETURN
      END
C
C  VSCALE   [06B6:1608]
      SUBROUTINE VSCALE (S, A, B)
      DOUBLE PRECISION S, A(3), B(3)
      B(1) = A(1) * S
      B(2) = A(2) * S
      B(3) = A(3) * S
      RETURN
      END
C
C  CROSS    [06B6:16A1]
      SUBROUTINE CROSS (A, B, C)
      DOUBLE PRECISION A(3), B(3), C(3)
      C(1) = A(2)*B(3) - A(3)*B(2)
      C(2) = A(3)*B(1) - B(3)*A(1)
      C(3) = B(2)*A(1) - A(2)*B(1)
      RETURN
      END
