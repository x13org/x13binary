C     Last change:  BCM  24 Nov 97   12:07 pm
      SUBROUTINE wrtdat(Idate,Sp,Str,Nchr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Puts the date in character format for outlier variables and
c printouts.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      CHARACTER cmo*3
      DIMENSION cmo(12)
c     ------------------------------------------------------------------
c  moved by Bob Fay
      CHARACTER Str*(*)
      INTEGER Idate,Nchr,Sp
      DIMENSION Idate(2)
c     ------------------------------------------------------------------
      DATA cmo/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep',
     &     'Oct','Nov','Dec'/
c     ------------------------------------------------------------------
      Nchr=1
      CALL itoc(Idate(YR),Str,Nchr)
      IF(Lfatal)RETURN
c     ------------------------------------------------------------------
      IF(Sp.gt.1)THEN
       Str(Nchr:Nchr)='.'
       Nchr=Nchr+1
c     ------------------------------------------------------------------
       IF(Sp.eq.12)THEN
        Str(Nchr:Nchr+2)=cmo(Idate(MO))
        Nchr=Nchr+3
c     ------------------------------------------------------------------
       ELSE
        CALL itoc(Idate(MO),Str,Nchr)
        IF(Lfatal)RETURN
       END IF
      END IF
      Nchr=Nchr-1
c     ------------------------------------------------------------------
      RETURN
      END
