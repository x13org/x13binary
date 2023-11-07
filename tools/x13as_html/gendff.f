C     Last change:  BCM  19 May 2003    9:29 am
      SUBROUTINE gendff(Srs,Pos1ob,Posfob,Srdiff,Df1ob,Taklog,Logten,
     &                  Thisd)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
c     ------------------------------------------------------------------
      DOUBLE PRECISION Srs,Srdiff
      LOGICAL Taklog,Logten
      INTEGER Pos1ob,Posfob,Df1ob,Thisd,i,j
      DIMENSION Srs(PLEN),Srdiff(PLEN)
c     ------------------------------------------------------------------
      DO i=Pos1ob,Posfob
       IF(Taklog)THEN
        IF(Logten)THEN
         Srdiff(i)=log10(Srs(i))
        ELSE
         Srdiff(i)=log(Srs(i))
        END IF
       ELSE
        Srdiff(i)=Srs(i)
       END IF
      END DO
c     ------------------------------------------------------------------
      Df1ob=Pos1ob
      IF(Thisd.eq.0)RETURN
      DO i=1,Thisd
       Df1ob=Df1ob+1
       DO j=Posfob,Df1ob,-1
        Srdiff(j)=Srdiff(j)-Srdiff(j-1)
       END DO
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
      