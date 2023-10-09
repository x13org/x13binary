C     Last change:  BCM  12 Nov 1998   10:53 am
**==ispeak.f    processed by SPAG 4.03F  at 14:16 on 28 Sep 1994
      INTEGER FUNCTION ispeak(Sxx,Lsa,Peaks,Lowlim,Uplim,Npeaks,Plimit,
     &                        Mlimit,Ny,Freq,Plocal,Ldecbl)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Function that flags possible trading day or seasonal peaks in a
c     given set of spectral estimates.  Peak must be greater than the
c     median of the spectral estimates computed (Mlimit).  The peaks of
c     interest are defined in the vector pkvec.
c-----------------------------------------------------------------------
      DOUBLE PRECISION Mlimit,Sxx,slimit,Plimit,Freq,f0,f1,f2,Plocal
      LOGICAL Lsa,Ldecbl
      INTEGER i,ifreq,Peaks,Lowlim,Uplim,Peakwd,Npeaks,i2,Ny,k,k0,k1,k2
      DIMENSION Sxx(*),Freq(*),Peaks(*),Lowlim(*),Uplim(*)
c-----------------------------------------------------------------------
      ispeak=0
      i2=Npeaks
      IF(Lsa.and.Ny.eq.12)i2=i2-1
c-----------------------------------------------------------------------
      DO i=1,i2
       ifreq=Peaks(i)
       IF(Sxx(ifreq).gt.Mlimit)THEN
c-----------------------------------------------------------------------
        k=0
        k1=Lowlim(i)+1
        IF(Lsa.and.Ny.eq.12)THEN
         k2=ifreq-1
        ELSE
         k2=Uplim(i)-1
        END IF
        IF(k2.gt.k1)THEN
         f1=Freq(ifreq)-Plocal
         f2=Freq(ifreq)+Plocal
         DO k0=k1,k2
          IF(k0.ne.ifreq)THEN
           f0=Freq(k0)
           IF((f0.lt.f1.or.f0.gt.f2).and.(Sxx(k0).gt.Sxx(ifreq)))k=k+1
          END IF
         END DO
        END IF
c-----------------------------------------------------------------------
        IF(k.eq.0)THEN
         IF(Ldecbl)THEN
          slimit=Sxx(ifreq)-Plimit
          IF(Sxx(Lowlim(i)).lt.slimit)THEN
           IF(Lsa.and.(i.eq.Npeaks))THEN
            ispeak=ispeak+1
           ELSE
            IF(Sxx(Uplim(i)).lt.slimit)ispeak=ispeak+1
           END IF
          END IF
         ELSE
          slimit=Sxx(ifreq)/Sxx(Lowlim(i))
          IF(slimit.ge.Plimit)THEN
           IF(Lsa.and.(i.eq.Npeaks))THEN
            ispeak=ispeak+1
           ELSE
            slimit=Sxx(ifreq)/Sxx(Uplim(i))
            IF(slimit.ge.Plimit)ispeak=ispeak+1
           END IF
          END IF
         END IF
        END IF
       END IF
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
