C     Last change:  BCM  25 Nov 1998   12:27 pm
      SUBROUTINE rmlnvr(Priadj,Kfulsm,Nspobs)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Remove length of month or leap year regressor
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      LOGICAL T
      PARAMETER(T=.true.)
c-----------------------------------------------------------------------
      INTEGER Nspobs,Priadj,Kfulsm,icol,strinx
      EXTERNAL strinx
c-----------------------------------------------------------------------
c     Reset prior adjustment variable
c     if type=trend specified in the x11 spec, then use lom or loq
c     adjustment (BCM, July 2005)
c-----------------------------------------------------------------------
      IF(Priadj.eq.0)THEN
       IF(Kfulsm.eq.2)THEN
        IF(Sp.eq.12)THEN
         Priadj=2
        ELSE IF(Sp.eq.4)THEN
         Priadj=3
        END IF
       ELSE
        Priadj=4
       END IF
      END IF
c     ------------------------------------------------------------------
c     Check for length-of-period or leap year regressor and remove it.
c     ------------------------------------------------------------------
      icol=1
      DO WHILE (icol.gt.0)
       icol=strinx(T,Colttl,Colptr,1,Ncoltl,'Length-of-')
       IF(icol.eq.0)icol=strinx(T,Colttl,Colptr,1,Ncoltl,'Leap Year')
       IF(icol.gt.0)THEN
        CALL dlrgef(icol,Nspobs,1)
        IF(Lfatal)RETURN
       END IF
      END DO
c     ------------------------------------------------------------------
      IF(Lndate(1).ne.NOTSET)THEN
       Lnzero=0
       CALL setint(NOTSET,2,Lndate)
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
