C     Last change:  BCM  28 Apr 1998   11:04 am
      DOUBLE PRECISION FUNCTION setcv(Nspobs,Cvalfa)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'stdio.i'
c     ------------------------------------------------------------------
      LOGICAL F,T
      DOUBLE PRECISION PI,ONE,TWO
      PARAMETER(F=.false.,T=.true.,PI=3.14159265358979d0,ONE=1D0,
     &          TWO=2D0)
c     ------------------------------------------------------------------
      INTEGER Nspobs,i,iflag
      DOUBLE PRECISION Cvalfa,dnobs,y,xmat,beta,x,acv,bcv
c----------------------------------------------------------------------
      DOUBLE PRECISION setcvl,ppnd
      EXTERNAL setcvl,ppnd
c----------------------------------------------------------------------
      DIMENSION x(3),xmat(3,3),y(3),beta(3)
c----------------------------------------------------------------------
      DATA x / 2.0D0,100.0D0,200.0D0 /
c----------------------------------------------------------------------
c     Compute critical value based on length of series (see Ljung)
c----------------------------------------------------------------------
      setcv=DNOTST
      IF(Nspobs.eq.1)THEN
c----------------------------------------------------------------------
c     If only one observation in the outlier span, set the critical
c     value based on the normal deviate corresponding to alpha.
c----------------------------------------------------------------------
       setcv=ppnd(ONE-(Cvalfa/TWO),iflag)
       IF(iflag.eq.1)THEN
        CALL eWritln('Default outlier critical value cannot be '//
     &               'derived due to an',STDERR,Mt2,T,F)
        CALL writln('       internal error.  Use the critical '//
     &              'argument to set the outlier',STDERR,Mt2,F,F)
        CALL writln('       critical value.',STDERR,Mt2,F,T)
        setcv=DNOTST
        RETURN
       END IF
c     ------------------------------------------------------------------
c     Else, set up equation to solve to get approximation formula for
c     this value of alpha.
c     ------------------------------------------------------------------
      ELSE
       dnobs=DBLE(Nspobs)
       do i=1,3
        if(i.eq.1)THEN
         y(1)=ppnd((ONE+sqrt(ONE-Cvalfa))/TWO,iflag)
         IF(iflag.eq.1)THEN
          CALL eWritln('Default outlier critical value cannot be '//
     &                 'derived due to an',STDERR,Mt2,T,F)
          CALL writln('       internal error.  Use the critical '//
     &                'argument to set the outlier',STDERR,Mt2,F,F)
          CALL writln('       critical value.',STDERR,Mt2,F,T)
          RETURN
         END IF
        ELSE
         y(i)=setcvl(int(x(i)),Cvalfa)
        END IF
        xmat(i,1)=ONE
        xmat(i,3)=sqrt(TWO*log(x(i)))
        xmat(i,2)=(LOG(LOG(x(i)))+LOG(TWO*TWO*PI))/(TWO*xmat(i,3))
       END DO      
c     ------------------------------------------------------------------
c     solve equations...
c     ------------------------------------------------------------------
       call lassol(3,xmat,y,3,beta,iflag)
       IF(iflag.eq.2)THEN
        CALL eWritln('Default outlier critical value cannot be '//
     &               'derived due to an',STDERR,Mt2,T,F)
        CALL writln('       internal error.  Use the critical '//
     &              'argument to set the outlier',STDERR,Mt2,F,F)
        CALL writln('       critical value.',STDERR,Mt2,F,T)
        RETURN
       END IF
c----------------------------------------------------------------------
c     Use coefficients to derive critical value for outlier span length
c     dnobs.
c----------------------------------------------------------------------
       acv=SQRT(TWO * LOG(dnobs))
       bcv=(LOG(LOG(dnobs))+LOG(TWO*TWO*PI))/(TWO*acv)
       setcv=beta(1) + beta(2)*bcv + beta(3)*acv
      END IF
c----------------------------------------------------------------------
      RETURN
      END
