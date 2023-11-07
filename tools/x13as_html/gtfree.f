      SUBROUTINE gtfree(Plen,Datfil,Nfil,Y,Chnl,Freq,Nobs,Hvfreq,Hvstrt,
     &                  Argok)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     Reads in free formatted data.
c     ------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      CHARACTER Datfil*(PFILCR)
      DOUBLE PRECISION Y
      LOGICAL Hvfreq,Hvstrt,Argok
      INTEGER Freq,Nobs,Chnl,Plen,i,Nfil
      DIMENSION Y(Plen)
c     ------------------------------------------------------------------
      IF(.not.Hvfreq.and.Hvstrt)THEN
       Freq=12
       Hvfreq=T
      END IF
      READ(Chnl,*,END=30,ERR=20)(Y(i),i=1,Plen)
      GO TO 30
c     ------------------------------------------------------------------
   20 CALL eWritln('Problem reading '//Datfil(1:Nfil)//'.',
     &             STDERR,Mt2,T,F)
      CALL writln('        Check that file has only correctly '//
     &            'formatted real numbers.',STDERR,Mt2,F,T)
      Argok=F
      Nobs=0
c     ------------------------------------------------------------------
   30 RETURN
      END
      
