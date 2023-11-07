C     Last change:  BCM  22 Sep 1998   11:00 am
      SUBROUTINE rdregm(Rgmttl,Begspn,Sp,Zeroz,Rgmidx,Locok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c   Reads change of regime information from the group title of a
c   set of change of regime regressors
c-----------------------------------------------------------------------
      CHARACTER Rgmttl*(*)
      LOGICAL Locok
      INTEGER begrgm,Begspn,ipos,nchr,Rgmidx,Sp,strinx,Zeroz
      DIMENSION begrgm(2),Begspn(2)
      EXTERNAL strinx
c-----------------------------------------------------------------------
c     check to see if regressor has zeros before or after the date.
c-----------------------------------------------------------------------
      Locok=.true.
      Rgmidx=0
      Zeroz=0
      nchr=LEN(Rgmttl)
c-----------------------------------------------------------------------
      ipos=index(Rgmttl(1:nchr),'(starting ')
      IF(ipos.eq.0)THEN
       ipos=index(Rgmttl(1:nchr),'(before ')
       IF(ipos.eq.0)THEN
        ipos=index(Rgmttl(1:nchr),'(change from before ')
        RETURN
       END IF
       Zeroz=1
      ELSE
       Zeroz=-1
      END IF
c-----------------------------------------------------------------------
c     Read regime date from group title string
c-----------------------------------------------------------------------
      IF(Zeroz.eq.1)THEN
       ipos=ipos+8
      ELSE
       ipos=ipos+10
      END IF
      CALL ctodat(Rgmttl(1:(nchr-1)),Sp,ipos,begrgm,Locok)
c-----------------------------------------------------------------------
c     if ok, compute displacement from Begspn
c-----------------------------------------------------------------------
      IF(Locok)THEN
       CALL dfdate(begrgm,Begspn,Sp,Rgmidx)
       Rgmidx=Rgmidx+1
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
