C     Last change:  BCM  28 Sep 1998    8:55 am
      SUBROUTINE gtrgpt(Begdat,Rgdate,Rgzero,Rgdtvc,Nobs)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Generate a pointer to the change of regime date of a change-of-
c     regime regressor
c-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
c-----------------------------------------------------------------------
      LOGICAL Rgdtvc
      INTEGER Begdat,i,Nobs,Rgzero,rgmidx,Rgdate
      DIMENSION Begdat(2),Rgdate(2),Rgdtvc(PLEN)
c-----------------------------------------------------------------------
      CALL setlg(F,PLEN,Rgdtvc)
      CALL dfdate(Rgdate,Begdat,Sp,rgmidx)
      rgmidx=rgmidx+1
c-----------------------------------------------------------------------
      IF(Rgzero.eq.1)THEN
c-----------------------------------------------------------------------
       IF(rgmidx.gt.0)THEN
        DO i=1,rgmidx-1
         IF(.not.Rgdtvc(i))Rgdtvc(i)=T
        END DO
       END IF
      ELSE
c-----------------------------------------------------------------------
       IF(rgmidx.le.0)rgmidx=1
c-----------------------------------------------------------------------
       DO i=rgmidx,Nobs
        IF(.not.Rgdtvc(i))Rgdtvc(i)=T
       END DO
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
