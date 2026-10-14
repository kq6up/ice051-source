@ECHO OFF
CD \ORIGINAL
AUTOTYPE -w 3 -p 1 f2 s u n period o u t enter , enter , f7 s u n enter , , , , , , , , , enter , f10
ICE.EXE > C:\LOGS\ORIGINAL.OUT
C:\DELAY.COM
ECHO COMPLETE > C:\LOGS\ORIGINAL.DON
