C=======================================================================
C  ICE 0.51  --  recovered source: main program and menu (001C, 0000)
C  The screen/keyboard primitives SETCUR, GETKEY, CLRWIN, CUROFF,
C  CURON and DOSXIT are the assembler routines of segment 0013
C  (see scrio.asm / scrio.c).
C  This file is kept in UTF-8; the build converts it to code page 437.
C=======================================================================
C
      BLOCK DATA ICEBD
      DOUBLE PRECISION DATE0, ALAT, ALONG, STEP
      INTEGER*4 NPOS, IZONE
      COMMON /SITE/ DATE0, ALAT, ALONG, STEP, NPOS, IZONE
      DATA DATE0, ALAT, ALONG, STEP / 880101.D0, 40.D0, -90.D0, 1.D0 /
      DATA NPOS, IZONE / 5, -6 /
      END
C
C-----------------------------------------------------------------------
C  ICE      [001C:000A]  main program: menu, function keys F1..F10
C-----------------------------------------------------------------------
      PROGRAM ICE
      INTEGER*4 IACC
      LOGICAL LFILE
      COMMON /ACCUR/ IACC, LFILE
      INTEGER*4 IOS
      LOGICAL LMENU
      COMMON /MENUC/ IOS, LMENU
      INTEGER*4 IASC
      INTEGER*4 ISCAN
      SAVE
      LMENU = .FALSE.
      LFILE = .FALSE.
      CALL ICEINIT
      CALL CUROFF
   10 CALL MENU
   20 CALL GETKEY (IASC, ISCAN)
C     Alt-F1 redraws the menu
      IF (ISCAN .EQ. 104) THEN
         CALL MENU
         CALL CUROFF
         GO TO 20
      END IF
      IF (ISCAN .LT. 59 .OR. ISCAN .GT. 68) GO TO 20
      WRITE (*, 9000)
      GO TO (110, 120, 130, 140, 150, 160, 170, 180, 190, 200),
     &      ISCAN - 58
  110 CALL INIVAL
      GO TO 10
  120 CALL FILOUT
      GO TO 10
  130 CALL POSITN
      GO TO 10
  140 CALL PHYSIC
      GO TO 10
  150 CALL SIDTBL
      GO TO 10
  160 CALL RISSET
      GO TO 10
  170 CALL NAVIG
      GO TO 10
  180 CALL DAYCON
      GO TO 10
  190 CALL CATCHG (IASC, LFILE)
      GO TO 10
C     F10: quit
  200 CLOSE (10)
      WRITE (*, 9000)
      WRITE (*, 9010) '╔═══════════════════════════════════════╗'
      WRITE (*, 9010) '║ End of Interactive Computer Ephemeris ║'
      WRITE (*, 9010) '╚═══════════════════════════════════════╝'
      WRITE (*, 9020)
      CALL CURON
      CALL DOSXIT
      STOP
 9000 FORMAT (///////////////////////,/)
 9010 FORMAT (T20,A)
 9020 FORMAT (/////////,/)
      END
C
C-----------------------------------------------------------------------
C  MENU     [0000:0000]  title and function-key menu; on the first call
C           the defaults file ICE.DFT is read (unit 1)
C-----------------------------------------------------------------------
      SUBROUTINE MENU
      DOUBLE PRECISION DATE0, ALAT, ALONG, STEP
      INTEGER*4 NPOS, IZONE
      COMMON /SITE/ DATE0, ALAT, ALONG, STEP, NPOS, IZONE
      INTEGER*4 IOS
      LOGICAL LMENU
      COMMON /MENUC/ IOS, LMENU
      DOUBLE PRECISION D, A, B, S
      INTEGER*4 N, IZ
      LOGICAL LEX
      CHARACTER*4096 ICEFILE
      CHARACTER*36 TITLE(7)
      CHARACTER*55 MENUT(11)
      SAVE
      DATA TITLE /
     & '╔══════════════════════════════════╗',
     & '║      U. S. Naval Observatory     ║',
     & '║      Nautical Almanac Office     ║',
     & '╟──────────────────────────────────╢',
     & '║  Interactive Computer Ephemeris  ║',
     & '╚══════════════════════════════════╝',
     & '            Version 0.51            ' /
      DATA MENUT /
     & '╔════╤═════════════════════╦════╤═════════════════════╗',
     & '║ F1 │ Initial Values      ║ F2 │ File Output         ║',
     & '╟────┼─────────────────────╫────┼─────────────────────╢',
     & '║ F3 │ Positions           ║ F4 │ Physical Ephemeris  ║',
     & '╟────┼─────────────────────╫────┼─────────────────────╢',
     & '║ F5 │ Sidereal Time       ║ F6 │ Rise & Set Times    ║',
     & '╟────┼─────────────────────╫────┼─────────────────────╢',
     & '║ F7 │ Navigation          ║ F8 │ Daily Configuration ║',
     & '╟────┼─────────────────────╫────┼─────────────────────╢',
     & '║ F9 │ Change Catalog      ║ F10│ Quit                ║',
     & '╚════╧═════════════════════╩════╧═════════════════════╝' /
      WRITE (*, 9000)
      WRITE (*, 9010) TITLE
      IF (LMENU) GO TO 30
      INQUIRE (FILE=ICEFILE('ICE.DFT'), EXIST=LEX)
      CALL DFTMSG (LEX)
      IF (LEX) THEN
         OPEN (1, FILE=ICEFILE('ICE.DFT'))
         READ (1, 9020, IOSTAT=IOS) D, A, B, S, N, IZ
      END IF
      IF (IOS .EQ. 0) THEN
         DATE0 = D
         ALONG = B
         ALAT = A
         NPOS = N
         STEP = S
         IZONE = IZ
      ELSE
         WRITE (*,*) 'Error reading file ICE.DFT!!!'
         WRITE (*,*) 'Built in default values will be used.'
      END IF
      GO TO 40
   30 WRITE (*, 9030)
   40 WRITE (*, 9040) MENUT
      IF (LMENU) WRITE (*, 9050)
      LMENU = .TRUE.
      RETURN
 9000 FORMAT (///////////////////////,/)
 9010 FORMAT (T22,A)
 9020 FORMAT (4(T18,F15.6,/),2(T24,I3,:,/))
 9030 FORMAT (///)
 9040 FORMAT (T12,A)
 9050 FORMAT (/)
      END
C
C-----------------------------------------------------------------------
C  DFTMSG   [001C:0184]  report whether ICE.DFT was found
C-----------------------------------------------------------------------
      SUBROUTINE DFTMSG (LEX)
      LOGICAL LEX
      CHARACTER*38 BOX(8)
      SAVE
      DATA BOX /
     & '┌────────────────────────────────────┐',
     & '│ The file of default initial values │',
     & '│              ICE.DFT               │',
     & '│           has been read.           │',
     & '└────────────────────────────────────┘',
     & '│       * * *  ICE.DFT  * * *        │',
     & '│    not found in this directory!    │',
     & '│   Internal defaults will be used.  │' /
      IF (LEX) THEN
         WRITE (*, 9000) BOX(1), BOX(2), BOX(3), BOX(4), BOX(5)
      ELSE
         WRITE (*, 9000) BOX(1), BOX(6), BOX(7), BOX(8), BOX(5)
         WRITE (*, 9010) CHAR(7)
      END IF
      RETURN
 9000 FORMAT (T21,A)
 9010 FORMAT (T27,A)
      END
C
C-----------------------------------------------------------------------
C  BADCHR   [001C:01C1]  complain about an invalid input character
C-----------------------------------------------------------------------
      SUBROUTINE BADCHR
      WRITE (*, 9000)
      WRITE (*, 9010) ' Invalid character in input!  Try again.'
      RETURN
 9000 FORMAT (///////////////////////,/)
 9010 FORMAT (//A)
      END
C
C-----------------------------------------------------------------------
C  HITKEY   [001C:346B]  wait for Enter
C-----------------------------------------------------------------------
      SUBROUTINE HITKEY
      PAUSE '                       Hit [Enter] to return to menu'
      RETURN
      END
C
C-----------------------------------------------------------------------
C  FILOUT   [001C:01E2]  F2: open (append/create) or close the output
C           file, unit 10
C-----------------------------------------------------------------------
      SUBROUTINE FILOUT
      INTEGER*4 IACC
      LOGICAL LFILE
      COMMON /ACCUR/ IACC, LFILE
      CHARACTER*40 FNAME
      INTEGER*4 N
      LOGICAL LOPEN, LEX
      SAVE
      WRITE (*, 9000)
      INQUIRE (10, OPENED=LOPEN, NAME=FNAME)
      IF (LOPEN) THEN
         N = INDEX (FNAME, '  ')
         WRITE (*,*) 'Output file ', FNAME(1:N), 'closed.'
         WRITE (*,*)
     &   'No more data will be added until file is re - opened.'
         CLOSE (10)
         LFILE = .FALSE.
         GO TO 90
      END IF
      IF (LOPEN) GO TO 90
      WRITE (*, 9010) ' Enter name of output file: '
      CALL CURON
      READ (*, 9020) FNAME
      CALL CUROFF
      IF (FNAME .EQ. ' ') RETURN
      INQUIRE (FILE=FNAME, EXIST=LEX)
      IF (LEX) THEN
         N = INDEX (FNAME, '  ')
         WRITE (*,*) 'Output file ', FNAME(1:N), 're - opened.'
         WRITE (*,*) 'Data will be appended to bottom of file.'
         OPEN (10, FILE=FNAME, STATUS='OLD')
   10    READ (10, 9030, END=20)
         GO TO 10
   20    BACKSPACE 10
      ELSE
         N = INDEX (FNAME, '  ')
         WRITE (*,*) 'Output file ', FNAME(1:N), 'created.'
         OPEN (10, FILE=FNAME, STATUS='NEW')
      END IF
      LFILE = .TRUE.
   90 WRITE (*,*)
      CALL HITKEY
      RETURN
 9000 FORMAT (///////////////////////,/)
 9010 FORMAT (//A,$)
 9020 FORMAT (A)
 9030 FORMAT (1X)
      END
