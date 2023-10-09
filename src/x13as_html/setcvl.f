C     Last change:  BCM  28 Apr 1998   11:04 am
      DOUBLE PRECISION FUNCTION setcvl(Nspobs,Cvalfa)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'stdio.i'
c     ------------------------------------------------------------------
      LOGICAL F,T
      DOUBLE PRECISION PI,ONE,TWO,MINPT5
      PARAMETER(F=.false.,T=.true.,PI=3.14159265358979d0,ONE=1D0,
     &          TWO=2D0,MINPT5=-0.5D0)
c     ------------------------------------------------------------------
      INTEGER Nspobs
      DOUBLE PRECISION dnobs,acv,bcv,xcv,Cvalfa,pmod
c----------------------------------------------------------------------
c     Compute critical value based on length of series (see Ljung)
c----------------------------------------------------------------------
      IF(Nspobs.eq.1)THEN
       CALL eWritln('Default outlier critical value cannot be '//
     &              'derived for an outlier',STDERR,Mt2,T,F)
       CALL writln('       span of one observation.  Either use the '//
     &             'critical argument to',STDERR,Mt2,F,F)
       CALL writln('       set the outlier critical value, or '//
     &             'change the setting of the',STDERR,Mt2,T,F)
       CALL writln('       defaultcritical argument.',STDERR,Mt2,F,T)
       Cvalfa=DNOTST
       setcvl=DNOTST
       RETURN
      END IF
c----------------------------------------------------------------------
      pmod=TWO-SQRT(ONE+Cvalfa)
      dnobs=DBLE(Nspobs)
      acv=SQRT(TWO * LOG(dnobs))
      bcv=acv-(LOG(LOG(dnobs))+LOG(TWO*TWO*PI))/(TWO*acv)
      xcv=-LOG(MINPT5 * LOG(pmod))
      setcvl=(xcv/acv) + bcv
c----------------------------------------------------------------------
      RETURN
      END
