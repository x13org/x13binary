      SUBROUTINE dlrgrw(Xy,Ncxy,Nrxy,Rgxcld)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Removes from the regression matrix Xy the values indicated in the
c     logical vector Rgxcld
c-----------------------------------------------------------------------
      DOUBLE PRECISION Xy
      LOGICAL Rgxcld
      INTEGER disp1,disp2,i,i2,j,Ncxy,Nrxy
      DIMENSION Xy(*),Rgxcld(*)
c-----------------------------------------------------------------------
c     Initialize index for matrix with rows excluded
c-----------------------------------------------------------------------
      i2=1
c-----------------------------------------------------------------------
c     Check to see if observation not to be excluded
c-----------------------------------------------------------------------
      DO i=1,Nrxy
       IF(.not.Rgxcld(i))THEN
        disp1=(i-1)*Ncxy
        disp2=(i2-1)*Ncxy
        DO j=1,Ncxy
         Xy(disp2+j)=Xy(disp1+j)
        END DO
c-----------------------------------------------------------------------
c     Update index for matrix with rows excluded
c-----------------------------------------------------------------------
        i2=i2+1
       END IF
      END DO
c-----------------------------------------------------------------------
      RETURN
      END

