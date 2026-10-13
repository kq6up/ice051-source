import os
here=os.path.dirname(os.path.abspath(__file__))
grid=[(int(l.split()[0]),float(l.split()[1])) for l in open(os.path.join(here,'data/dt_grid.txt')) if not l.startswith('#')]
hdr=open(os.path.join(here,'data/dt_grid.txt')).readline().strip('# \n')
hdr_a,hdr_b=hdr.split('; ',1)
cs=[round(v*100) for y,v in grid]
def data_stmt(lo,hi):
    vals=[f'{x}' for x in cs[lo-1:hi]]
    lines=[];cur=''
    for i,v in enumerate(vals):
        piece=v+(',' if i<len(vals)-1 else ' /')
        if len(cur)+len(piece)+1>58: lines.append(cur); cur=''
        cur+=(' ' if cur else '')+piece
    lines.append(cur)
    out=[f'      DATA (ITAB(I), I = {lo:3d}, {hi:3d}) /']
    out+=['     &  '+l for l in lines]
    return '\n'.join(out)
src=f"""C=======================================================================
C  ICE (Interactive Computer Ephemeris) Beta 0.51, USNO NAO, 1989
C  Delta T code recovered from ICE.EXE (MS FORTRAN 4.x, EXEPACKed).
C  There is no original source: names below are mine, the logic,
C  constants and order of evaluation are exactly what the binary does.
C=======================================================================
C
C  ---------------------------------------------------------------------
C  1. ORIGINAL ROUTINE AS COMPILED            (code 001C:3480-3626)
C  ---------------------------------------------------------------------
C  Called once, from the F7 Navigation routine.  YEAR is a decimal year,
C  DT is TDT - UT in seconds.  T is a static (DGROUP:584E) local.
C  Constants live in DGROUP:43A0-4447.
C
C  (renamed DTOLD here so both versions can live in one file)
      SUBROUTINE DTOLD (YEAR, DT)
      DOUBLE PRECISION YEAR, DT, T
      SAVE T
      T = YEAR - 1920.D0
      IF (YEAR .GE. 1800.D0 .AND. YEAR .LT. 1920.D0) THEN
         DT = ((((( -1.7466294D-09 *T
     &              -6.2784728D-07)*T
     &              -8.3015088D-05)*T
     &              -4.7416012D-03)*T
     &              -9.1297366D-02)*T
     &              +7.1582603D-01)*T
     &              +22.264143D0
      ELSE IF (YEAR .GE. 1920.D0 .AND. YEAR .LE. 2050.D0) THEN
         DT = ((((((((( -4.4923383D-16 *T
     &                  +2.9397795D-13)*T
     &                  -8.1695181D-11)*T
     &                  +1.2559680D-08)*T
     &                  -1.1668619D-06)*T
     &                  +6.7262386D-05)*T
     &                  -2.3815278D-03)*T
     &                  +4.9569969D-02)*T
     &                  -5.4514611D-01)*T
     &                  +2.6651830D0  )*T
     &                  +19.755494D0
      ELSE IF (YEAR .GE. 1800.D0 .AND. YEAR .LE. 2050.D0) THEN
C        (the binary tests this third range but the branch is empty;
C         outside 1800-2050 DT is simply left unchanged)
         CONTINUE
      END IF
      RETURN
      END
C
C  ---------------------------------------------------------------------
C  2. THE CALL SITE (F7 Navigation, code 001C:202A-20D0), original
C  ---------------------------------------------------------------------
C     CALL JDCAL (UTJD, IY, IM, ID, CMONTH, ...)     ! 06B6:1B17
C     YEAR = DBLE(IM)/12.D0 + DBLE(ID)/30.D0 + IY    ! <-- bug: ID/30
C     CALL DELTAT (YEAR, DT)                         ! years, not months
C     TDTJD = DT/86400.D0 + UTJD
C     WRITE (*, '(... ''Delta T = '', F5.1, '' seconds'' ...)') ... DT
C
C  So YEAR is IY + IM/12 + ID/30: biased late by 0.1 yr on 1 January,
C  rising to +2.0 yr on 31 December.  The patch changes the 30.D0
C  constant (DGROUP:42E8) to 365.25D0 and removes the remaining
C  1/12 + 1/365.25 offset inside DELTAT, giving the true decimal year.
C
C  ---------------------------------------------------------------------
C  3. REPLACEMENT ROUTINE  (what the patched ICE.EXE now executes)
C  ---------------------------------------------------------------------
C  Linear interpolation in a 2-year table 1800.0-2050.0 (0.01 s units)
C    1800-1972  Astron. Almanac / Stephenson & Morrison (Swiss Eph.)
C    1974-2026  IERS: 32.184 + (TAI-UTC) - (UT1-UTC) at 1 January
C    2028-2050  extrapolated from the latest IERS value
C  Latest IERS value used:
C    {hdr_a}
C    then {hdr_b}
C  Beyond either end the end interval is extrapolated linearly.
C
      SUBROUTINE DELTAT (YEAR, DT)
      DOUBLE PRECISION YEAR, DT, X, F, Y0
      PARAMETER (Y0 = 1800.D0 + 1.D0/12.D0 + 1.D0/365.25D0)
      INTEGER*2 ITAB(126)
      INTEGER I
{data_stmt(1,63)}
{data_stmt(64,126)}
      X = (YEAR - Y0) * 0.5D0
      I = INT(X)
      IF (X .LT. 0.D0) I = 0
      IF (I .GT. 124) I = 124
      F = X - DBLE(I)
      DT = (DBLE(ITAB(I+1)) + F*DBLE(ITAB(I+2)-ITAB(I+1))) * 0.01D0
      RETURN
      END
C
C  New call site:   YEAR = DBLE(IM)/12.D0 + DBLE(ID)/365.25D0 + IY
"""
open(os.path.join(here,'DELTAT.FOR'),'w').write(src.replace('\n','\r\n'))
print(src[-2600:])
