C     Native runtime paths only; no changes to astronomical calculations.
      CHARACTER*4096 FUNCTION ICEFILE (NAME)
      CHARACTER*(*) NAME
      CALL ICEPATH (NAME, ICEFILE)
      END
