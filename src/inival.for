C=======================================================================
C  ICE 0.51  --  recovered source: F1 Initial Values (0917:0CCB)
C=======================================================================
C
C-----------------------------------------------------------------------
C  INIVAL   [0917:0CCB]  show and change the initial values (COMMON SITE)
C-----------------------------------------------------------------------
      SUBROUTINE INIVAL
      DOUBLE PRECISION DATE0, ALAT, ALONG, STEP
      INTEGER*4 NPOS, IZONE
      COMMON /SITE/ DATE0, ALAT, ALONG, STEP, NPOS, IZONE
      INTEGER*2 IASC
      INTEGER*4 ISCAN
      CHARACTER*42 BOX(10)
      SAVE
      DATA BOX /
     & '╔═══════════════════════╤════════════════╗',
     & '║ F1 Date               │                ║',
     & '║ F2 Latitude           │                ║',
     & '║ F3 Longitude          │                ║',
     & '║ F4 Tabulation Interval│                ║',
     & '║ F5 Number of lines    │                ║',
     & '║ F6 Time Zone          │                ║',
     & '╟───────────────────────┼────────────────╢',
     & '║ F7 Accept values      │                ║',
     & '╚═══════════════════════╧════════════════╝' /
   10 CALL CUROFF
      WRITE (*, 9000)
      WRITE (*, 9010) '* * Initial Values * *'
      WRITE (*, 9020) 'Press F key to modify:'
      WRITE (*, 9030) BOX(1)
      WRITE (*, 9040) BOX(2), DATE0
      WRITE (*, 9050) BOX(3), ALAT
      WRITE (*, 9060) BOX(4), ALONG
      WRITE (*, 9070) BOX(5), STEP
      WRITE (*, 9080) BOX(6), NPOS
      WRITE (*, 9090) BOX(7), IZONE
      WRITE (*, 9030) BOX(8)
      WRITE (*, 9030) BOX(9)
      WRITE (*, 9030) BOX(10)
      WRITE (*, 9100)
   20 CALL GETKEY (IASC, ISCAN)
      IF (ISCAN .LT. 59 .OR. ISCAN .GT. 65) GO TO 20
      CALL CURON
      GO TO (110, 120, 130, 140, 150, 160, 170), ISCAN - 58
  110 WRITE (*, 9110) ' Enter starting date [JD or yyyymmdd.hhmmss]: '
      READ (*, *, ERR=115) DATE0
      GO TO 10
  115 CALL BADCHR
      GO TO 110
  120 WRITE (*, 9110) ' Enter latitude [±dd.ddd]: '
      READ (*, *, ERR=125) ALAT
      GO TO 10
  125 CALL BADCHR
      GO TO 120
  130 WRITE (*, 9110) ' Enter longitude [±ddd.ddd]: '
      READ (*, *, ERR=135) ALONG
      GO TO 10
  135 CALL BADCHR
      GO TO 130
  140 WRITE (*, 9110) ' Enter interval [JD or yymmdd.hhmmss]: '
      READ (*, *, ERR=145) STEP
      GO TO 10
  145 CALL BADCHR
      GO TO 140
  150 WRITE (*, 9110) ' Enter number of lines [nnnn]: '
      READ (*, *, ERR=155) NPOS
      GO TO 10
  155 CALL BADCHR
      GO TO 150
  160 WRITE (*, 9110) ' Enter time zone (zone - UT) [±nn]: '
      READ (*, *, ERR=165) IZONE
      GO TO 10
  165 CALL BADCHR
      GO TO 160
  170 CALL CUROFF
      RETURN
 9000 FORMAT (///////////////////////,/)
 9010 FORMAT (T30,A,/)
 9020 FORMAT (T22,A)
 9030 FORMAT (T20,A)
 9040 FORMAT (T20,A,T45,F15.6)
 9050 FORMAT (T20,A,SP,T50,F6.2)
 9060 FORMAT (T20,A,SP,T49,F7.2)
 9070 FORMAT (T20,A,SP,T49,F11.6)
 9080 FORMAT (T20,A,T48,I5)
 9090 FORMAT (T20,A,SP,T50,I3)
 9100 FORMAT (///////)
 9110 FORMAT (//A,$)
      END
