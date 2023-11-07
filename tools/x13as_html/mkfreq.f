C     Last change:  BCM  29 Feb 2008    9:46 am
*      SUBROUTINE mkfreq(Ny,Peakwd,Lfqalt,Lprsfq)
      SUBROUTINE mkfreq(Peakwd,Lfqalt,Lprsfq)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C     Generate two sets of frequencies to generate the spectral
c     estimates - one to determine peaks (Frqpk), the other to provide
c     frequencies for the line printer spectral plots (Frq)
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'spcidx.cmn'
c-----------------------------------------------------------------------
      LOGICAL Lfqalt,Lprsfq
*      INTEGER Ny,Peakwd,i,i2
      INTEGER Peakwd,i,i2
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
c     Generate base frequencies over which the spectrum will be computed
c-----------------------------------------------------------------------
      DO i=1,61
       Frq(i)=dble(i-1)/120D0
      END DO
c-----------------------------------------------------------------------
c     Create set of frequencies to be used to find spectral peaks.
c-----------------------------------------------------------------------
c     Initialize frequencies to DNOTST 
c-----------------------------------------------------------------------
      CALL setdp(DNOTST,76,Frqpk)
c-----------------------------------------------------------------------
c     Insert trading day peak, upper and lower limits into Frqpk
c-----------------------------------------------------------------------
      DO i=1,nTfreq
       Frqpk(Tpeak(i)) = Tfreq(i)
       Frqpk(Tlow(i)) = Tfreq(i) - dble(Peakwd)*Frq(2)
       Frqpk(Tup(i)) = Tfreq(i) + dble(Peakwd)*Frq(2)
      END DO
c-----------------------------------------------------------------------
c     Insert Seasonal Frequencies into Frqpk
c-----------------------------------------------------------------------
      i2=1
      DO i=1,nFreq
       IF(dpeq(Frqpk(i),DNOTST))THEN
        Frqpk(i)=Frq(i2)
        i2=i2+1
       END IF
      END DO
c-----------------------------------------------------------------------
c     Create set of frequencies to be plotted.
c-----------------------------------------------------------------------
c     Include frequencies for trading day peak limits, except where they
c     would overwrite seasonal frequencies.
c-----------------------------------------------------------------------
      IF(.not.Lprsfq)THEN
*      IF(Ny.eq.12)THEN
       IF(Lfqalt)THEN
        Frq(37-Peakwd)=.3036D0-Frq(2)*dble(Peakwd)
        Frq(37)=.3036D0
        IF(Peakwd.lt.4)Frq(37+Peakwd)=.3036D0+Frq(2)*dble(Peakwd)
       END IF
c-----------------------------------------------------------------------
       IF(Peakwd.ne.2)Frq(43-Peakwd)=.3482D0-Frq(2)*dble(Peakwd)
       Frq(43)=.3482D0
       Frq(43+Peakwd)=.3482D0+Frq(2)*dble(Peakwd)
c-----------------------------------------------------------------------
       IF(Peakwd.ne.2)Frq(53-Peakwd)=.432D0-Frq(2)*dble(Peakwd)
       Frq(53)=.432D0
       Frq(53+Peakwd)=.432D0+Frq(2)*dble(Peakwd)
c-----------------------------------------------------------------------
*      ELSE
*       IF(Lfqalt)THEN
*        Frq(36-Peakwd)=.294375D0-Frq(2)*dble(Peakwd)
*        Frq(36)=.294375D0
*        Frq(36+Peakwd)=.294375D0+Frq(2)*dble(Peakwd)
*c-----------------------------------------------------------------------
*        Frq(42-Peakwd)=.33875D0-Frq(2)*dble(Peakwd)
*        Frq(42)=.33875D0
*        Frq(42+Peakwd)=.33875D0+Frq(2)*dble(Peakwd)
*c-----------------------------------------------------------------------
*        Frq(47-Peakwd)=.383125D0-Frq(2)*dble(Peakwd)
*        Frq(47)=.383125D0
*        Frq(47+Peakwd)=.383125D0+Frq(2)*dble(Peakwd)
*       END IF
*c-----------------------------------------------------------------------
*       Frq(6-Peakwd)=.0446D0-Frq(2)*dble(Peakwd)
*       Frq(6)=.0446D0
*       Frq(6+Peakwd)=.0446D0+Frq(2)*dble(Peakwd)
*c-----------------------------------------------------------------------
*       Frq(12-Peakwd)=.0893D0-Frq(2)*dble(Peakwd)
*       Frq(12)=.0893D0
*       Frq(12+Peakwd)=.0893D0+Frq(2)*dble(Peakwd)
*      END IF
      END IF
c-----------------------------------------------------------------------
      RETURN
      END