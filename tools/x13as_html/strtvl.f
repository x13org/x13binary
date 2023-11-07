C     Last change:  BCM  29 Sep 97    8:45 am
      SUBROUTINE strtvl()
      INCLUDE 'notset.prm'
c-----------------------------------------------------------------------
c     Calculates starting values.  If the parameter vector is notset
c then the regression parameters are calculated by OLS and stored in b.
c The ARMA parameters are set to .1
c-----------------------------------------------------------------------
c Name  Type  Description
c-----------------------------------------------------------------------
c beglag  i  Local begining lag of the lag operator
c begopr  i  Local begining operator of the (Difference, AR, MA) filter
c endlag  i  Local last lag of the lag operator
c endopr  i  Local last operator of the (Difference, AR, MA) filter
c i       i  Local do loop index
c iflt    i  Local do loop index for the current filter
c iopr    i  Local do loop index for the current lag operator
c lag     i  Local do loop index for the current lag
c pnt1    d  Local PARAMETER for .1d0
c-----------------------------------------------------------------------
c     Variable typing and initialization
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION PNT1
      PARAMETER(PNT1=.1D0)
c     ------------------------------------------------------------------
      INTEGER beglag,begopr,endlag,endopr,iflt,iopr,lag
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
c     Set the ARMA parameters if they are not fixed
c-----------------------------------------------------------------------
      DO iflt=DIFF,MA
       begopr=Mdl(iflt-1)
       endopr=Mdl(iflt)-1
c     ------------------------------------------------------------------
       DO iopr=begopr,endopr
        beglag=Opr(iopr-1)
        endlag=Opr(iopr)-1
c     ------------------------------------------------------------------
        DO lag=beglag,endlag
         IF(dpeq(Arimap(lag),DNOTST).AND.(.not.Arimaf(lag)))
     &      Arimap(lag)=PNT1
        END DO
       END DO
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
