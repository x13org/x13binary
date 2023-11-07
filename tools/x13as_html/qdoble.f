C     Last change:  BCM  14 Oct 97    9:05 am
**==qdoble.f    processed by SPAG 4.03F  at 09:52 on  1 Mar 1994
      LOGICAL FUNCTION qdoble(Str,Nchr,Alsoin)
      IMPLICIT NONE
c     -----------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      CHARACTER decchr*1,dmychr*1,expchr*1,sgnchr*1,Str*(*)
      LOGICAL Alsoin,havsgn
      INTEGER Nchr,nchr2
c     -----------------------------------------------------------------
      CHARACTER getchr*1
      LOGICAL qintgr
      INTEGER indx
      EXTERNAL getchr,qintgr,indx
c     -----------------------------------------------------------------
      qdoble=F
      Alsoin=F
c     -----------------------------------------------------------------
      sgnchr=getchr(dmychr)
      IF(sgnchr.eq.'+'.or.sgnchr.eq.'-')THEN
       havsgn=T
       Nchr=1
       Str(Nchr:Nchr)=sgnchr
      ELSE
       havsgn=F
       CALL putbak(sgnchr)
       Nchr=0
      END IF
c     -----------------------------------------------------------------
      qdoble=qintgr(Str(Nchr+1:),nchr2)
      Alsoin=qdoble
      IF(qdoble)Nchr=Nchr+nchr2
c     -----------------------------------------------------------------
      IF(getchr(decchr).eq.'.')THEN
       Alsoin=F
       Nchr=Nchr+1
       Str(Nchr:Nchr)=decchr
c     -----------------------------------------------------------------
       IF(qintgr(Str(Nchr+1:),nchr2))THEN
        qdoble=T
        Nchr=Nchr+nchr2
       ELSE IF(.not.qdoble)THEN
        CALL putbak(decchr)
        Nchr=Nchr-1
       END IF
c     -----------------------------------------------------------------
      ELSE
       CALL putbak(decchr)
      END IF
c     -----------------------------------------------------------------
      IF(.not.qdoble.and.havsgn)THEN
       CALL putbak(sgnchr)
       Nchr=0
      END IF
c     -----------------------------------------------------------------
      IF(qdoble)THEN
       IF(indx('eEdD^',getchr(expchr)).eq.0)THEN
        CALL putbak(expchr)
c     -----------------------------------------------------------------
       ELSE
        Nchr=Nchr+1
        Str(Nchr:Nchr)=expchr
c     -----------------------------------------------------------------
        sgnchr=getchr(dmychr)
        IF(sgnchr.eq.'+'.or.sgnchr.eq.'-')THEN
         havsgn=T
         Nchr=Nchr+1
         Str(Nchr:Nchr)=sgnchr
c     -----------------------------------------------------------------
        ELSE
         havsgn=F
         CALL putbak(sgnchr)
        END IF
c     -----------------------------------------------------------------
        IF(qintgr(Str(Nchr+1:),nchr2))THEN
         Alsoin=F
         Nchr=Nchr+nchr2
c     -----------------------------------------------------------------
        ELSE
         IF(havsgn)THEN
          CALL putbak(sgnchr)
          Nchr=Nchr-1
         END IF
         CALL putbak(expchr)
         Nchr=Nchr-1
        END IF
       END IF
      END IF
c     -----------------------------------------------------------------
      RETURN
      END
