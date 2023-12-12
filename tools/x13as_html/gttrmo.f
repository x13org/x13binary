C     Last change:  BCM  11 Jun 1998    4:20 pm
      SUBROUTINE gttrmo(Plen,Trfile,Nfil,Y,Start,Chnl,Nobs,Freq,Havttl,
     &                  Title,Nttlcr,Havnam,Srsnam,Nser,Argok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Read the Edit data file format
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
      INCLUDE 'x11msc.cmn'
c-----------------------------------------------------------------------
      LOGICAL T,F
      INTEGER PCUT2K,YR,MO
      PARAMETER(YR=1,MO=2,PCUT2K=45,T=.true.,F=.false.)
c-----------------------------------------------------------------------
      CHARACTER Trfile*(PFILCR),Title*(*),Srsnam*(64),tmpttl*80
      DOUBLE PRECISION Y
      LOGICAL Argok,Havnam,Havttl
      INTEGER Freq,i,itmp1,itmp2,Plen,Start,Chnl,Nobs,Nttlcr,Nser,isp,
     &        Nfil
      DIMENSION Y(Plen),Start(2)
c-----------------------------------------------------------------------
      INTEGER nblank
      EXTERNAL nblank
c-----------------------------------------------------------------------
c     Read the series name, title from file (only use if series name
c     and title are not already given).
c-----------------------------------------------------------------------
      READ(Chnl,*,END=20,ERR=10)tmpttl
      IF(.not.Havttl)THEN
       Nttlcr=nblank(tmpttl)
       Title(1:Nttlcr)=tmpttl(1:Nttlcr)
       Havttl=T
      END IF
      IF(.not.Havnam)THEN
       isp = index(tmpttl,' ') - 1
       IF (isp.gt.0) THEN
        Nser=MIN(isp,16)
        Srsnam(1:Nser)=tmpttl(1:Nser)
        Havnam=T
       END IF
      END IF
c-----------------------------------------------------------------------
c     Read number of observations, starting day, frequency
c-----------------------------------------------------------------------
      READ(Chnl,*,END=20,ERR=10)Nobs,itmp1,itmp2,Freq
      IF(itmp1.lt.100)THEN
       IF(Yr2000.and.(yr.le.PCUT2K))THEN
        itmp1=itmp1+2000
       ELSE
        itmp1=itmp1+1900
       END IF
      END IF
c-----------------------------------------------------------------------
c     Set the starting date.
c-----------------------------------------------------------------------
      Start(YR)=itmp1
      Start(MO)=itmp2
c-----------------------------------------------------------------------
c     Check to see if number of observations exceeds program limit
c-----------------------------------------------------------------------
      IF(Nobs.gt.Plen)THEN
       CALL eWritln('Problem reading '//Trfile(1:Nfil)//'.',
     &              STDERR,Mt2,T,F)
       CALL writln('        Too many observations in file.',
     &             STDERR,Mt2,F,T)
       Argok=.false.
       Nobs=0
c-----------------------------------------------------------------------
c     Else, read in observations
c-----------------------------------------------------------------------
      ELSE
       READ(Chnl,*,END=20,ERR=10)(Y(i),i=1,Nobs)
      END IF
      RETURN
c-----------------------------------------------------------------------
   10 CALL eWritln('Problem reading '//Trfile(1:Nfil)//'.',
     &             STDERR,Mt2,T,F)
      CALL writln('        Check your input file and format.',
     &            STDERR,Mt2,F,T)
      Argok=.false.
      Nobs=0
      RETURN
c-----------------------------------------------------------------------
   20 CALL eWritln('End of file encountered while reading '//
     &             Trfile(1:Nfil)//'.',STDERR,Mt2,T,F)
      CALL writln('        Check your input file and format.',
     &            STDERR,Mt2,F,T)
      Argok=.false.
      Nobs=0
      RETURN
      END
