      SUBROUTINE setrvp(Begspn,Ny,Lfda,Llda,Lmodel)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c ---  Set pointer for beginning, end of adjustment loop for revisions
c      analysis
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'rev.prm'
      INCLUDE 'rev.cmn'
      INCLUDE 'revtrg.cmn'
c-----------------------------------------------------------------------
      INTEGER MO,YR
      PARAMETER(MO=2,YR=1)
c-----------------------------------------------------------------------
      INTEGER Begspn,Ny,Lfda,Llda,i,mxrlag
      LOGICAL Lmodel
      DIMENSION Begspn(2)
c-----------------------------------------------------------------------
      CALL dfdate(Rvstrt,Begspn,Ny,Begrev)
      Begrev=Begrev+Lfda
      CALL dfdate(Rvend,Begspn,Ny,Endsa)
      Endsa=Endsa+Lfda
      Endrev=Llda
      IF(Endsa.ne.Endrev)Endsa=Endsa+1
      Endtbl=Endsa
      Revnum=Endtbl-Begrev
      IF(Ntarsa.gt.0.or.Ntartr.gt.0)THEN
       mxrlag=0
       IF(Ntarsa.gt.0)THEN
        DO i=1,Ntarsa
         IF(mxrlag.lt.Targsa(i))mxrlag=Targsa(i)
        END DO
       END IF
       IF(Ntartr.gt.0)THEN
        DO i=1,Ntartr
         IF(mxrlag.lt.Targtr(i))mxrlag=Targtr(i)
        END DO
       END IF
       Endsa=Endsa+mxrlag
       IF(Endsa.gt.Llda)Endsa=Llda
      END IF
c      Revptr=Endrev-Begrev+1
c-----------------------------------------------------------------------
c     Set Beglup, the first observation in the loop.  Will be different
c     from Begrev only if revisions history of the projected seasonal 
c     factors are requested.
c-----------------------------------------------------------------------
      Beglup=Begrev
      CALL cpyint(Rvstrt,2,1,Lupbeg)
      IF(Lrvsf)THEN
       Lupbeg(YR)=Rvstrt(YR)-1
       Lupbeg(MO)=Ny
       CALL dfdate(Lupbeg,Begspn,Ny,Beglup)
       Beglup=Beglup+Lfda
       Frstsa=Beglup
      ELSE
       Frstsa=Begrev
      END IF
c-----------------------------------------------------------------------
c     If Fixper > 0, set the beginning of the loop so that the first 
c     model estimation occurs at the first occurance of the period 
c     Fixper before the revision history loop.
c-----------------------------------------------------------------------
      IF(Fixper.gt.0.and.Lmodel)THEN
       IF(Lupbeg(MO).gt.Fixper)THEN
        Beglup=Beglup-(Lupbeg(MO)-Fixper)
        Lupbeg(MO)=Fixper
       ELSE IF(Lupbeg(MO).lt.Fixper)THEN
        Beglup=Beglup-(Ny-(Fixper-Lupbeg(MO)))
        Lupbeg(MO)=Fixper
        Lupbeg(YR)=Lupbeg(YR)-1
       END IF
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
