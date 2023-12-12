      SUBROUTINE setmv(Srs,Mvind,Mvval,Pos1ob,Posfob)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Check to see if prior adjustments have brought down missing
c     value code, and reset missing value code if this has occurred.
c-----------------------------------------------------------------------
      DOUBLE PRECISION Srs,Mvval
      LOGICAL Mvind
      INTEGER Pos1ob,Posfob,i
      DIMENSION Srs(*),Mvind(*)
c-----------------------------------------------------------------------
      DO i=Pos1ob,Posfob
       IF(Mvind(i))Srs(i)=Mvval
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
      