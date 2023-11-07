      SUBROUTINE upespm(Estprm)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     upespm.f, Release 1, Subroutine Version 1.1, Modified 01 Feb 1995.
c-----------------------------------------------------------------------
c	This routine takes the vector of nonlinear estimated parameters
c and stores them in the data structures needed to run the ARIMA
c filter.
c-----------------------------------------------------------------------
c Name  Type Description
c-----------------------------------------------------------------------
c begptr  i  Local pointer to the first row in opr of the current
c             difference, AR, or MA filter
c estprm  d  Input Nestpm long vector of estimated parameters from the
c             nonlinear routine.  Nestpm is found in model.cmn
c estptr  i  Local pointer in either estprm or arimap for the first operator
c             to be expanded.
c iflt    i  Local index for the current filter type, DIFF, AR, or MA.
c ilag    i  Local index for the current lag, pointer to the current
c             element in lag,arimap, and arimaf.
c iopr    i  Local index for the current operator, it is the pointer to the
c             current row in the operator specfication matrix, opr.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
c     ------------------------------------------------------------------
      INTEGER beglag,begopr,endlag,endopr,estptr,iflt,ilag,iopr
      DOUBLE PRECISION Estprm
      DIMENSION Estprm(Nestpm)
c-----------------------------------------------------------------------
c     For each operator insert the estimated parameters in the model
c information vectors
c-----------------------------------------------------------------------
      estptr=0
      DO iflt=DIFF,MA
       begopr=Mdl(iflt-1)
       endopr=Mdl(iflt)-1
c     ------------------------------------------------------------------
       DO iopr=begopr,endopr
        beglag=Opr(iopr-1)
        endlag=Opr(iopr)-1
c     ------------------------------------------------------------------
        DO ilag=beglag,endlag
         IF(.not.Arimaf(ilag))THEN
          estptr=estptr+1
          Arimap(ilag)=Estprm(estptr)
         END IF
        END DO
       END DO
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
