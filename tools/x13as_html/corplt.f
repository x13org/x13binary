C     Last change:  BCM  21 Aug 1998   11:18 am
      SUBROUTINE corplt(R,Se,Nr,Nsp,Iflag)
c-----------------------------------------------------------------------
c     corplt.f, Release 1, Subroutine Version 1.5, Modified 15 Feb 1995.
c-----------------------------------------------------------------------
c subroutine to plot sample ACF and PACF functions
c-----------------------------------------------------------------------
C        modified 28 Mar 1996 to use =s instead of Xs for spikes,
C        use a | at 0.0, and indicate seasonal lags with dashes.
C        -Matt Kramer
c-----------------------------------------------------------------------
c        modified 21 August 1998 to produce ACF plot for squared
c	 residuals
c-----------------------------------------------------------------------
c Name   Type Description
c-----------------------------------------------------------------------
c r       r   vector of autorelations or partial autorelations
c nr      i   number of relations and standard errors
c nsp     i   length of the seasonal period
c iflag   i   indicator for PACF and ACF, i = 1 PACF, i = 2 ACF,
c               i=3, ACF of squared residuals
c se      r   vector of standard errors
C ep      i   even value of p if = 0
C sl      i   seasonal lag if = 0
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
      LOGICAL F,T
      DOUBLE PRECISION ONE,MONE,ZERO
      PARAMETER(F=.false.,T=.true.,ONE=1D0,MONE=-1D0,ZERO=0D0)
c-----------------------------------------------------------------------
C--- modified following line -MK
      INTEGER i,j,k,imax,imin,Nr,nlag,Nsp,Iflag,pin,plb,pub,sl,ep
      DOUBLE PRECISION rmax,rmin,rmid,R(Nr),Se(Nr),sqv
C--- modified following line -MK
      CHARACTER p(51),x,cblnk,cdot,bar,dash
      CHARACTER*2 pmar
*      LOGICAL pinout,plbout,pubout
      LOGICAL plbout,pubout
C--- following line modified -MK
      DATA x,cblnk,cdot,bar,dash/'X',' ','.','|','-'/
c-----------------------------------------------------------------------
c     Set the number of lags to be plotted
c-----------------------------------------------------------------------
      nlag=Nr
c-----------------------------------------------------------------------
c     Set the Plot Range
c-----------------------------------------------------------------------
      rmin=MONE
      rmax=ONE
      rmid=ZERO
      sqv=(rmax-rmin)/50D0
c-----------------------------------------------------------------------
c     Write out header for the plot
c-----------------------------------------------------------------------
*      IF(Iflag.eq.1)THEN
*       WRITE(Mt1,1010)
* 1010  FORMAT(' Sample Partial Autocorrelations of the Residuals')
*      ELSE IF(Iflag.eq.2)THEN
*       WRITE(Mt1,1020)'Residuals'
* 1020  FORMAT('  Sample Autocorrelations of the ',a)
*      ELSE IF(Iflag.eq.3)THEN
*       WRITE(Mt1,1020)'Squared Residuals'
*      END IF
c-----------------------------------------------------------------------
c     Write out plot axes
c-----------------------------------------------------------------------
      CALL writTag(Mt1,'<pre>')
      WRITE(Mt1,1030)
 1030 FORMAT(17x,
     &       '-1.0 -0.8 -0.6 -0.4 -0.2  0.0  0.2  0.4  0.6  0.8  1.0')
      WRITE(Mt1,1040)
 1040 FORMAT(17x,'  +',10('----+'))
c-----------------------------------------------------------------------
c     plot sample autorelations one lag at a time
c     by filling print buffer character vector p
c-----------------------------------------------------------------------
      DO i=1,nlag
c-----------------------------------------------------------------------
c     initialize print buffer vector p and margin buffer pmar
c-----------------------------------------------------------------------
       pmar='  '
       DO j=1,51
        p(j)=cblnk
       END DO
c-----------------------------------------------------------------------
       IF(R(i).lt.MONE.or.R(i).ge.ONE)THEN
        CALL writTag(Mt1,'</pre>')
        CALL eWritln('Sample autocorrelation or partial '//
     &               'autocorrelation computations',STDERR,Mt2,T,F)
        CALL writln('        produced values greater than one in norm.',
     &              STDERR,Mt2,F,T)
        CALL abend
        RETURN
       END IF
c-----------------------------------------------------------------------
c     Calculate plot point indices for the autorelation value,
c and cplus and minus two standard error bounds
c-----------------------------------------------------------------------
       pin=nint((R(i)-rmin)/sqv)+1
       pub=nint((2D0*Se(i)-rmin)/sqv)+1
       plb=52-pub
c-----------------------------------------------------------------------
c     Check to see if plot indices are within bounds
c-----------------------------------------------------------------------
c       pinout=F
       plbout=F
       pubout=F
c       IF((pin.gt.51).or.(pin.lt.1))pinout=T
       IF((plb.gt.51).or.(plb.lt.1))plbout=T
       IF((pub.gt.51).or.(pub.lt.1))pubout=T
c     ------------------------------------------------------------------
       imin=pin
       imax=pin
c-----------------------------------------------------------------------
c     Set up marker for the zero line, and standard error bounds
c-----------------------------------------------------------------------
       IF(.not.plbout)p(plb)=cdot
       IF(.not.pubout)p(pub)=cdot
C---  added lines to insert dashes at seasonal period -MK
       sl=mod(i,Nsp)
       IF((sl.eq.0).and.(Nsp.gt.1))THEN
        DO j=1,49
         ep=mod(j,2)
         IF(((j.lt.(plb-2)).or.(j.gt.(pub+2))).and.(ep.eq.0))p(j)=dash
        END DO
       END IF
C---END OF ADDED LINES
c-----------------------------------------------------------------------
c     Set up bars
c-----------------------------------------------------------------------
       IF(R(i).lt.rmid)imax=26
       IF(R(i).ge.rmid)imin=26
       DO k=imin,imax
        p(k)=x
       END DO
C---  blank out seasonal dashes near ends of ACF or PACF spikes  -MK
       IF((sl.eq.0).and.(imin.gt.3).and.(imin.lt.plb))THEN
        DO k=(imin-3),(imin-1)
         p(k)=cblnk
        END DO
       END IF
       IF((sl.eq.0).and.(imax.lt.48).and.(imax.gt.pub))THEN
        DO k=(imax+1),(imax+3)
         p(k)=cblnk
        END DO
       END IF
       p(26)=bar
C---       IF(sl.eq.0)p(26)=cplus  this wasn't being used anyway  -MK
c-----------------------------------------------------------------------
c     Write out print buffer for the autorelation at this lag
c-----------------------------------------------------------------------
       WRITE(Mt1,1060)i,pmar,p,R(i)
 1060  FORMAT(10x,i3,4x,a2,51A1,f6.3)
      END DO
      CALL writTag(Mt1,'</pre>')
c     ------------------------------------------------------------------
      RETURN
      END
