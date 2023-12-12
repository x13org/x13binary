**==isfixd.f    processed by SPAG 4.03F  at 09:50 on  1 Mar 1994
      SUBROUTINE isfixd(Level,Arimaf,Beglag,Endlag,Cfix)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Reports "(fixed)" for a component, operator, lag, or variance
c depending on what level it is fixed at.
c-----------------------------------------------------------------------
c Name   Type Description
c-----------------------------------------------------------------------
c arimaf  l  Input parima long vector to tell if a parameter is
c             fixed or not
c beglag  i  Input begining lag of the current set of lags and
c             estimate/parameters in arimal and arimap
c cfix    c  Output 7 long character string reporting whether a
c             component, operator, lag, or variance is fixed or
c             estimated
c cpntfx  l  Local saved switch for a fixed component
c endlag  i  Input end lag of the current set of lags and
c             estimate/parameters in arimal and arimap
c fixed   l  Local switch for a fixed or estimated lag set
c ilag    i  Local index to the current lag
c LAGS    i  Local PARAMETER defining the lag level
c level   i  Input indicator if the routine is working with something
c             fixed at the component, operator, or lag level
c MODEL   i  Local PARAMETER defining the component level
c oprfix  l  Local saved switch for a not fixed component but fixed
c             operator withing the component
c OPRS    i  Local PARAMETER defining the operator level
c-----------------------------------------------------------------------
      INTEGER MODEL,LAGS,OPRS
      PARAMETER(MODEL=1,LAGS=3,OPRS=2)
c     ------------------------------------------------------------------
      CHARACTER Cfix*7
      LOGICAL Arimaf,cpntfx,fixed,oprfix
      INTEGER Beglag,Endlag,ilag,Level
      DIMENSION Arimaf(*)
      SAVE cpntfx,oprfix
c     ------------------------------------------------------------------
      fixed=.true.
      DO ilag=Beglag,Endlag
       fixed=fixed.and.Arimaf(ilag)
      END DO
c     ------------------------------------------------------------------
      IF(Level.eq.MODEL)THEN
       cpntfx=fixed
c     ------------------------------------------------------------------
      ELSE IF(Level.eq.OPRS)THEN
       oprfix=.not.cpntfx.and.fixed.and.(Endlag.gt.Beglag)
       fixed=oprfix
c     ------------------------------------------------------------------
      ELSE IF(Level.eq.LAGS)THEN
       fixed=.not.cpntfx.and..not.oprfix.and.fixed
      END IF
c     ------------------------------------------------------------------
      IF(fixed)THEN
       Cfix='(fixed)'
      ELSE
       Cfix=' &nbsp;'
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
