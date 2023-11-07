C     Last change:  BCM  21 Nov 97   10:11 pm
**==hinge.f    processed by SPAG 4.03F  at 17:22 on 11 Mar 1994
      SUBROUTINE hinge(Xo,N,Ts,Tsxtra,Ic)
      IMPLICIT NONE
c----------------------------------------------------------------------
      DOUBLE PRECISION Xo,Ts,Tsxtra
      INTEGER Ic,lxtra,N,n1,n2,n3
      DIMENSION Ts(5),Xo(*)
c----------------------------------------------------------------------
c     Sort the series
c-----------------------------------------------------------------------
      CALL shlsrt(N,Xo)
c-----------------------------------------------------------------------
c     Store the maximum and the minimum of the series.
c----------------------------------------------------------------------
      Ts(1)=Xo(1)
      Ts(5)=Xo(N)
c----------------------------------------------------------------------
c     Compute the median
c----------------------------------------------------------------------
      IF(mod(N,2).eq.1)THEN
       Ts(3)=Xo((N+1)/2)
      ELSE
       Ts(3)=(Xo(N/2)+Xo((N/2)+1))/2
      END IF
c----------------------------------------------------------------------
c     Compute the 25th and 75th Pecentiles
c----------------------------------------------------------------------
      n2=(N+1)/2
      IF(mod(n2,2).eq.1)THEN
       n1=(n2+1)/2
       n3=N-n1+1
       Ts(2)=Xo(n1)
       Ts(4)=Xo(n3)
      ELSE
       n1=n2/2
       n3=n1+1
       Ts(2)=(Xo(n1)+Xo(n3))/2
       n1=N-n1+1
       n3=N-n3+1
       Ts(4)=(Xo(n1)+Xo(n3))/2
      END IF
c----------------------------------------------------------------------
c     For the sliding spans analysis, compute the 60th and 85th
c     percentiles.
c----------------------------------------------------------------------
      IF(Ic.gt.0)THEN
       IF(Ic.le.3)THEN
        lxtra=N-int(dble(N)*0.15D0+0.5D0)
       ELSE IF(Ic.eq.4)THEN
        lxtra=N-int(dble(N)*0.40D0+0.5D0)
       ELSE
        lxtra=N-int(dble(N)*0.10D0+0.5D0)
       END IF
       Tsxtra=Xo(lxtra)
      END IF
c----------------------------------------------------------------------
      RETURN
      END

