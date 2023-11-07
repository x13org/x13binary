C     Last change:  BCM   3 Feb 1999    9:10 am
      SUBROUTINE estrmu(Begdat,Nrxy,Sp,Ndays,Hlong,Hmean,Hstock)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This subroutine computes a function that removes the easter mean
c     effect from combined calendar runs.
c-----------------------------------------------------------------------
c     BCM April, 2016 - add mean for easter(0) regressor
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
c-----------------------------------------------------------------------
      LOGICAL F
      INTEGER MO
      DOUBLE PRECISION ZERO
      PARAMETER(MO=2,ZERO=0D0,F=.false.)
c-----------------------------------------------------------------------
      LOGICAL Hlong,Hstock
      INTEGER Begdat,Ndays,Nrxy,i,i1,mnindx,n,Sp,n2,n3,tdat
      DOUBLE PRECISION txy,Hmean
      DIMENSION Begdat(2),txy(1,PLEN),Hmean(*),tdat(2)
c-----------------------------------------------------------------------
c     Means of Easter regressors from 500 year span (1600-2099)
c     Source : Bednarek (http://home.swiftdsl.com.au/~mbednarek//easter.php)
c-----------------------------------------------------------------------
      DOUBLE PRECISION emeans
      DIMENSION emeans(26,2:4)
c-----------------------------------------------------------------------
      DATA(emeans(i,2),i=1,26)/
     & 0.00368D0,0.002083333D0,0.001130435D0,0.0002727273D0,0D0,
     & 0D0,0D0,0D0,0D0,0D0,
     & 0D0,0D0,0D0,0D0,0D0,
     & 0D0,0D0,0D0,0D0,0D0,
     & 0D0,0D0,0D0,0D0,0D0,0D0/
      DATA(emeans(i,3),i=1,26)/
     & 0.6576D0,0.6450833D0,0.6311304D0,0.6162727D0,0.5999048D0,
     & 0.583D0,0.5661053D0,0.549D0,0.5318824D0,0.514625D0,
     & 0.4973333D0,0.4807143D0,0.4643077D0,0.4476667D0,0.4305455D0,
     & 0.4136D0,0.3975556D0,0.382D0,0.3654286D0,0.3483333D0,
     & 0.3304D0,0.3125D0,0.2966667D0,0.281D0,0.266D0,0.232D0/
      DATA(emeans(i,4),i=1,26)/
     & 0.33872D0,0.3528333D0,0.3677391D0,0.3834545D0,0.4000952D0,
     & 0.417D0,0.4338947D0,0.451D0,0.4681176D0,0.485375D0,
     & 0.5026667D0,0.5192857D0,0.5356923D0,0.5523333D0,0.5694545D0,
     & 0.5864D0,0.6024444D0,0.618D0,0.6345714D0,0.6516667D0,
     & 0.6696D0,0.6875D0,0.7033333D0,0.719D0,0.734D0,0.768D0/
c-----------------------------------------------------------------------
c     IF long term means requested, store means from previous version
c     of X-13ARIMA-SEATS into vector
c-----------------------------------------------------------------------
      IF(Hlong)THEN
c-----------------------------------------------------------------------
c     Set index for holiday means 
c-----------------------------------------------------------------------
       mnindx=25-Ndays+1
c-----------------------------------------------------------------------
c     Compute monthly means for Easter
c-----------------------------------------------------------------------
       IF(Sp.eq.4)THEN
        DO i=1,Sp
         IF(i.eq.1)THEN
          Hmean(i)=emeans(mnindx,2)+emeans(mnindx,3)
         ELSE IF(i.eq.2)THEN
          Hmean(i)=emeans(mnindx,4)
         ELSE
          Hmean(i)=ZERO
         END IF
        END DO
       ELSE
        DO i=1,Sp
         IF(i.ge.2.and.i.le.4)THEN
          Hmean(i)=emeans(mnindx,i)
         ELSE
          Hmean(i)=ZERO
         END IF
        END DO
       END IF
c-----------------------------------------------------------------------
      ELSE
c-----------------------------------------------------------------------
c       Ensure that full years are used in computing Hmean
c       Update : BCM 2-1999
c-----------------------------------------------------------------------
       CALL cpyint(Begdat,2,1,tdat)
       n2=Nrxy
       IF(tdat(MO).gt.1)THEN
        n2=n2+tdat(MO)-1
        tdat(MO)=1
       END IF
       n3=MOD(n2,Sp)
       IF(n3.GT.0)n2=n2+Sp-n3
c-----------------------------------------------------------------------
       CALL adestr(tdat,n2,1,Sp,1,Ndays,0,txy,F,Hmean,Hstock)
c-----------------------------------------------------------------------
       DO i=1,Sp
        Hmean(i)=ZERO
        IF((Sp.eq.12.AND.(i.ge.2.and.i.le.4)).OR.
     &     (Sp.eq.4.AND.(i.eq.1.or.i.eq.2)))THEN
         i1=i
         n=1
         DO WHILE(n.le.29.and.i1.le.n2)
          Hmean(i)=Hmean(i)+txy(1,i1)
          i1=i1+Sp
          n=n+1
         END DO
         Hmean(i)=Hmean(i)/dble(n-1)
        END IF
       END DO
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
