C     star catalog in memory (one page of up to 200 objects)
C       COMMON /CATLG/  far segment 1C49, /CATNAM/ far segment 1ED4
C       ICAT    catalog number (1..10, 99 = file given by name)
C       IFIRST  first object number in catalog (always 1)
C       NSTAR   number of objects in catalog file
C       ILOW, IHIGH  object numbers of the page now in memory
      DOUBLE PRECISION CRA(200), CDEC(200), CPMRA(200), CPMDEC(200)
      REAL CPARX(200), CRV(200), CMAG(200)
      INTEGER*4 ICAT, IFIRST, NSTAR, ILOW, IHIGH
      CHARACTER*8 CNAM(200,3)
      COMMON /CATLG/ CRA, CDEC, CPMRA, CPMDEC, CPARX, CRV, CMAG,
     &     ICAT, IFIRST, NSTAR, ILOW, IHIGH
      COMMON /CATNAM/ CNAM
