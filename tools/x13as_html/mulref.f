      SUBROUTINE mulref(Nrxy,Fac,Tmp,Xdev,Xvec,Xval,Same)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     adjusts X-11 Regression factors by dividing either by the 
c     scalar Xval or the array Xvec.
c-----------------------------------------------------------------------
      DOUBLE PRECISION Fac,Tmp,Xvec,Xval
      LOGICAL Same
      INTEGER irow,Nrxy,j,Xdev
      DIMENSION Fac(*),Tmp(*),Xvec(*)
c-----------------------------------------------------------------------
      IF(Xval.gt.0)THEN
       DO irow=1,Nrxy
        IF(Same)THEN
         Fac(irow)=Tmp(irow)/Xval
        ELSE
         Fac(irow)=Fac(irow)+Tmp(irow)/Xval
        END IF
       END DO
      ELSE
       DO irow=1,Nrxy
        j=irow+Xdev-1
        IF(Same)THEN
         Fac(irow)=Tmp(irow)/Xvec(j)
        ELSE
         Fac(irow)=Fac(irow)+Tmp(irow)/Xvec(j)
        END IF
       END DO
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
