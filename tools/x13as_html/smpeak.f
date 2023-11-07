C     Last change:  BCM  12 Nov 1998   10:53 am
**==ispeak.f    processed by SPAG 4.03F  at 14:16 on 28 Sep 1994
      INTEGER FUNCTION smpeak(Sxx,Lsa,Peaks,Lowlim,Uplim,Npeaks,Star1,
     &                        Mlimit,Nform,Spclab)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Function that flags possible trading day or seasonal peaks in a
c     given set of spectral estimates.  Peak must be greater than the
c     median of the spectral estimates computed (Mlimit).  The peaks of
c     interest are defined in the vector pkvec.
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
c-----------------------------------------------------------------------
      CHARACTER labvec*(2),domfrq*(2),frqlab*(2),Spclab*(*)
      LOGICAL Lsa
      DOUBLE PRECISION Mlimit,Sxx,sbase,Star1,domsxx,starz
      INTEGER i,freq,Nform,pkidx,Peaks,Lowlim,Uplim,Npeaks
      DIMENSION Sxx(*),Peaks(*),Lowlim(*),Uplim(*),labvec(10)
c-----------------------------------------------------------------------
      DATA labvec/'t1','t2','t3','t4','t5','s1','s2','s3','s4','s5'/
c-----------------------------------------------------------------------
      domfrq = 'no'
      domsxx = DNOTST
      smpeak = NOTSET
c-----------------------------------------------------------------------
      DO i=1,Npeaks
       freq=Peaks(i)
       sbase=max(Sxx(Lowlim(i)),Sxx(Uplim(i)))
       starz=(Sxx(freq)-sbase)/Star1
       pkidx=i
       IF(Lsa)pkidx=i+5
       frqlab=labvec(pkidx)
       IF(Sxx(freq).gt.domsxx.and.Sxx(freq).gt.Mlimit.and.
     &    starz.gt.0D0)THEN
        domfrq=frqlab
        domsxx=Sxx(freq)
        smpeak=freq
       END IF
       IF(starz.le.0D0)THEN
        WRITE(Nform,1010)Spclab,frqlab,'nopeak'
       ELSE IF(Sxx(freq).gt.Mlimit)THEN
        WRITE(Nform,1020)Spclab,frqlab,starz,'+'
       ELSE
        WRITE(Nform,1020)Spclab,frqlab,starz,' '
       END IF
      END DO
      WRITE(Nform,1010)Spclab,frqlab(1:1)//'.dom',domfrq
c-----------------------------------------------------------------------
 1010 FORMAT(a,'.',a,': ',a)
 1020 FORMAT(a,'.',a,': ',f6.1,' ',a)
      RETURN
      END
