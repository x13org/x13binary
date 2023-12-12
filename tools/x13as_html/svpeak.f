C     Last change:  BCM  4 Mar 2008    3:46 pm
      SUBROUTINE svpeak(Sxx,Sxx2,Itbl,Iagr,Tpeak,Tlow,Tup,Ntfreq,Speak,
     &                  Slow,Sup,Nsfreq,Lseats,Ldecbl,Ltdfrq)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      LOGICAL F,T
      DOUBLE PRECISION ONE,TEN,FIVETO
      PARAMETER(F=.false.,T=.true.,ONE=1D0,TEN=10D0,FIVETO=52D0)
c-----------------------------------------------------------------------
      DOUBLE PRECISION Sxx,Sxx2,tmpsxx,medsxx,pklim,star1,sprng
      INTEGER Itbl,Nspstr,Iagr,Lsumm,Tpeak,Tlow,Tup,Ntfreq,i,
     &        domfqt,domfqs,Speak,Slow,Sup,Nsfreq,Ltdpk,Lsapk
      LOGICAL Lseats,Ldecbl,Ltdfrq
      CHARACTER spcstr*(10)
      DIMENSION Sxx(61),Sxx2(*),Tpeak(*),Tlow(*),Tup(*),Speak(*),
     &          Slow(*),Sup(*),tmpsxx(61)
c-----------------------------------------------------------------------
      INTEGER smpeak
      DOUBLE PRECISION mkmdsx
      EXTERNAL smpeak,mkmdsx
c-----------------------------------------------------------------------
c   Save information to .udg file
c-----------------------------------------------------------------------
      CALL copy(Sxx,61,1,tmpsxx)
      CALL shlsrt(61,tmpsxx)
      medsxx=mkmdsx(tmpsxx,61,Ldecbl)
      sprng=tmpsxx(61)-tmpsxx(1)
c-----------------------------------------------------------------------
      CALL mkspky(Itbl,spcstr,nspstr,Iagr,Lseats)
      write(Nform,1010)spcstr(1:nspstr),'median',medsxx
      write(Nform,1010)spcstr(1:nspstr),'range',sprng
c-----------------------------------------------------------------------
      star1=sprng/FIVETO
      domfqt=NOTSET
      IF(Ltdfrq)domfqt=smpeak(Sxx2,F,Tpeak,Tlow,Tup,Ntfreq,star1,
     &                        medsxx,Nform,spcstr(1:nspstr))
      domfqs=smpeak(Sxx2,T,Speak,Slow,Sup,Nsfreq,star1,medsxx,Nform,
     &              spcstr(1:nspstr))
      CALL mxpeak(Sxx2,Tpeak,domfqt,Ntfreq,Speak,domfqs,Nsfreq,
     &            tmpsxx(61),Nform,spcstr(1:nspstr))
c-----------------------------------------------------------------------
 1010 FORMAT(a,'.',a,': ',e20.10)
c-----------------------------------------------------------------------
      RETURN
      END

