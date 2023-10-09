C     Last change:  BCM  4 Mar 2008    3:46 pm
      SUBROUTINE idpeak(Sxx,Sxx2,Spclim,Ny,Tpeak,Tlow,Tup,Ntfreq,Speak,
     &                  Slow,Sup,Nsfreq,Ltdpk,Lsapk,Freq,Plocal,Ldecbl,
     &                  Ltdfrq)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
      LOGICAL F,T
      DOUBLE PRECISION FIVETO
      PARAMETER(F=.false.,T=.true.,FIVETO=52D0)
c-----------------------------------------------------------------------
      DOUBLE PRECISION Sxx,Sxx2,tmpsxx,domfqt,domfqs,medsxx,pklim,
     &                 Spclim,Freq,Plocal,star1
      INTEGER nspstr,Tpeak,Tlow,Tup,Ntfreq,Speak,Slow,Sup,Nsfreq,Ny,
     &        Ltdpk,Lsapk,i
      LOGICAL Ldecbl,Ltdfrq
      CHARACTER spcstr*(10)
      DIMENSION Sxx(61),Sxx2(*),Tpeak(*),Tlow(*),Tup(*),Speak(*),
     &          Slow(*),Sup(*),Freq(*),tmpsxx(61)
c-----------------------------------------------------------------------
      INTEGER ispeak
      DOUBLE PRECISION mkmdsx
      EXTERNAL ispeak,mkmdsx
c-----------------------------------------------------------------------
      CALL copy(Sxx,61,1,tmpsxx)
      CALL shlsrt(61,tmpsxx)
      medsxx=mkmdsx(tmpsxx,61,Ldecbl)
c-----------------------------------------------------------------------
      IF(Ldecbl)THEN
       pklim=(tmpsxx(61)-tmpsxx(1))*(Spclim/FIVETO)
      ELSE
       pklim=(tmpsxx(61)/tmpsxx(1))**(Spclim/FIVETO)
      END IF
      IF(Ltdfrq)Ltdpk=ispeak(Sxx2,F,Tpeak,Tlow,Tup,Ntfreq,pklim,medsxx,
     &                       Ny,Freq,Plocal,Ldecbl)
      Lsapk=ispeak(Sxx2,T,Speak,Slow,Sup,Nsfreq,pklim,medsxx,Ny,Freq,
     &             Plocal,Ldecbl)
c-----------------------------------------------------------------------
      RETURN
      END
