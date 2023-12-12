C     Last change:  BCM  25 Sep 2008    9:23 am
      SUBROUTINE chkuhg(Iuhl,Nguhl,Herror)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     check input for groups of user defined holiday regressors to see
c     if correct sequence of codes.  Will also compute number of groups
c     specified.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
c-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c-----------------------------------------------------------------------
      INTEGER Iuhl,Nguhl,i
      LOGICAL Herror,hzero
      DIMENSION Iuhl(PUHLGP)
c-----------------------------------------------------------------------
c     Initialize variables
c-----------------------------------------------------------------------
      herror=F
      hzero=F
      Nguhl=0
c-----------------------------------------------------------------------
c     Loop through holiday groups, checking to see if there are
c     inconsistencies
c-----------------------------------------------------------------------
      DO i=1,PUHLGP
       IF(Iuhl(i).eq.0)THEN
        IF(.not.hzero)hzero=T
       ELSE
        IF(hzero)THEN
         Herror=T
        ELSE IF(.not.Herror)THEN
         Nguhl=Nguhl+1
        END IF
       END IF
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
