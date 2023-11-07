C     Last change:  BCM   2 Oct 97    7:47 am
**==sdxtrm.f    processed by SPAG 4.03F  at 17:02 on 16 May 1994
      DOUBLE PRECISION FUNCTION sdxtrm(Xi,Xbar,L,M,Nsp,Imad,Istep,Ny,
     &                                 Lgrp)
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'xtrm.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0D0)
c-----------------------------------------------------------------------
      LOGICAL Lgrp,lselec,lsig
      DOUBLE PRECISION Xi,Xbar,stau,xn,abdev,median
      INTEGER L,M,Nsp,Imad,Istep,n,ixn,i2,Ny,nper
      DIMENSION Xi(*),abdev(PLEN)
c-----------------------------------------------------------------------
      LOGICAL dpeq,istrue
      DOUBLE PRECISION rho2
      EXTERNAL rho2,istrue,dpeq
c-----------------------------------------------------------------------
C --- COMPUTE FIVE YEAR STANDARD DEVIATION (OR MEDIAN ABSOLUTE
C     DEVIATION) OF THE IRREGULARS.
c-----------------------------------------------------------------------
      sdxtrm=ZERO
      xn=ZERO
      ixn=0
      lsig=istrue(Csigvc,1,Ny).and.Ksdev.eq.4
      DO n=L,M,Nsp
       lselec=.true.
       IF(lsig)THEN
        nper=mod(n,Ny)
        IF(nper.eq.0)nper=Ny
        lselec=Csigvc(nper).and.Lgrp
        IF(.not.lselec)lselec=(.not.Csigvc(nper)).and.(.not.Lgrp)
       END IF
       IF((.not.(Istep.eq.2.and.dpeq(Stwt(n),ZERO))).and.lselec)THEN
c-----------------------------------------------------------------------
C --- OMIT EXTREMES FROM THE CALCULATION OF THE FIVE YEAR SD OR MAD
c-----------------------------------------------------------------------
        xn=xn+1D0
        ixn=ixn+1
        IF(Imad.eq.0)sdxtrm=sdxtrm+(Xi(n)-Xbar)*(Xi(n)-Xbar)
        IF(Imad.eq.1.or.Imad.eq.3)abdev(ixn)=abs(Xi(n)-Xbar)
        IF(Imad.eq.2.or.Imad.eq.4)abdev(ixn)=abs(log(Xi(n)))
       END IF
      END DO
      IF(Imad.eq.0)sdxtrm=sqrt(sdxtrm/xn)
      IF(Imad.ge.1)THEN
       ixn=int(xn)
       CALL shlsrt(ixn,abdev)
c-----------------------------------------------------------------------
c --- calculate median absolute difference
c-----------------------------------------------------------------------
       IF(mod(ixn,2).eq.0)THEN
        median=(abdev(ixn/2)+abdev(ixn/2+1))/2D0
       ELSE
        median=abdev((ixn+1)/2)
       END IF
c-----------------------------------------------------------------------
c --- calculate median absolute deviation
c-----------------------------------------------------------------------
       sdxtrm=median/0.6745D0
       IF(Imad.eq.2.or.Imad.eq.4)sdxtrm=sqrt(exp(sdxtrm*sdxtrm)*(exp(
     &                                  sdxtrm*sdxtrm)-1))
c-----------------------------------------------------------------------
c --- derive tau adjustment for mad standard error
c-----------------------------------------------------------------------
       IF(Imad.ge.3)THEN
        stau=ZERO
        DO i2=1,ixn
         stau=stau+rho2(abdev(i2)/sdxtrm)
        END DO
        sdxtrm=sqrt(sdxtrm*sdxtrm*stau/n)
       END IF
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
