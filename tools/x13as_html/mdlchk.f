C     Last change:  SRD  31 Jan 2000    6:58 am
      SUBROUTINE mdlchk(A,Na,Nefobs,Blpct,Blq,Bldf,Rvr,Rtval)
      IMPLICIT NONE
c-----------------------------------------------------------------------
*      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
*      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
      INTEGER PR
      LOGICAL F,T
      PARAMETER(PR=PLEN/4,F=.false.,T=.true.)
c     ------------------------------------------------------------------
      DOUBLE PRECISION A,smpac,seacf,Blpct,Blq,Rvr,Rtval,rm,rv,an,rstd
      INTEGER Bldf,i,i1,Na,Nefobs,np,endlag,ilag
      DIMENSION A(PLEN+2*PORDER),seacf(PR),smpac(PR)
c-----------------------------------------------------------------------
      INCLUDE 'autoq.cmn'
c-----------------------------------------------------------------------
c      Get Ljung-Box Chi-Square results - first derive number of ACFs
c      to generate
c-----------------------------------------------------------------------
      IF(Sp.eq.12)THEN
       Bldf=24
      ELSE IF(Sp.eq.1)THEN
       Bldf=8
      ELSE
       Bldf=4*Sp
       IF(Sp.eq.4.and.Nefobs.le.22.and.Nefobs.ge.18)Bldf=6
      END IF
c-----------------------------------------------------------------------
c     check to see if there are enough observations to compute
c     LB Chi-Square results
c-----------------------------------------------------------------------
      IF(Bldf.ge.Nefobs)Bldf = Nefobs / 2
c-----------------------------------------------------------------------
      i1=Na-Nefobs+1
      np=0
      endlag=Opr(Nopr)-1
      DO ilag=1,endlag
       IF(.not.Arimaf(ilag))np=np+1
      END DO
      CALL acf(A(i1),Nefobs,Nefobs,smpac,seacf,Bldf,np,Sp,0,T,F,F)
      Blpct=1D0 - Qpv(Bldf)
      Blq=Qs(Bldf)
c-----------------------------------------------------------------------
c     Compute t-value of the residuals
c-----------------------------------------------------------------------
      rm=0.D0
      rv=0.D0
      DO i=i1,Na
       rm=rm+A(i)
       rv=rv+A(i)*A(i)
      END DO
      an=DBLE(Na-i1+1)
      rm=rm/an
      rv=rv/an-rm**2
      rstd=DSQRT(rv/an)
      Rtval=rm/rstd
c-----------------------------------------------------------------------
      Rvr=DSQRT(Var)
c-----------------------------------------------------------------------
      RETURN
      END
