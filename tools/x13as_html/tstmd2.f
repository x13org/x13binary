c     Last change:Mar. 2021- if there are fatal errors when calling
c                 armats,return
      SUBROUTINE tstmd2(Nnsig,Nz,Ipr,Iqr,Ips,Iqs)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      LOGICAL inptok
      DOUBLE PRECISION bmin,cval,tval,cmin,cv
      INTEGER i,ardsp,iurpr,iurps,iurqr,iurqs,Nnsig,Ipr,Ips,idr,ids,Iqr,
     &        Iqs,id,ip,iq,iprs,iqrs,n,icpr,icps,icqr,icqs,Nz
      DIMENSION tval(PARIMA)
c     ------------------------------------------------------------------
c     Initialize variables
c     ------------------------------------------------------------------
      Nnsig=0
      cval=Tsig
      IF (Nz.LE.150) THEN
       cmin=.15D0
      ELSE
       cmin=.1D0
      END IF
      ardsp=Nnsedf+Nseadf
c     ------------------------------------------------------------------
c       Convert X-13A-S model variables into variables compatable with
c       TRAMO/SEATS model data structure
c     ------------------------------------------------------------------
      CALL cnvmdl(Ipr,Ips,idr,ids,Iqr,Iqs,id,ip,iq,iprs,iqrs,n)
      IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     Check for unit roots.  Do not reduce order of time series model
c     if unit roots are present.
c     ------------------------------------------------------------------
      CALL chkurt(iurpr,iurps,iurqr,iurqs)
c     ------------------------------------------------------------------
c     Generate t-values for ARMA parameters
c     ------------------------------------------------------------------
      CALL armats(tval)
      IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     Check to see if t-values for the ARMA parameters are insignificant
c     ------------------------------------------------------------------
      icpr=0
      icps=0
      icqr=0
      icqs=0
      bmin=-DNOTST
c     ------------------------------------------------------------------
c     If there are insignificant paramters, reduce model order.
c     ------------------------------------------------------------------
      IF(Iurpr.eq.0.and.Ipr.gt.icpr)THEN
       i=Ipr-icpr
       cv=DABS(tval(i))
       IF (cv.LT.cval.AND.DABS(Arimap(i+ardsp)).LT.cmin) THEN
        icpr=icpr+1
        IF (bmin.GT.cv) bmin=cv
       END IF
      END IF
      IF (Ips.GT.icps.AND.Iurps.EQ.0) THEN
       i=iprs-icps
       cv=DABS(tval(i))
       IF (cv.LT.cval.AND.DABS(Arimap(i+ardsp)).LT.cmin) THEN
        icps=icps+1
        IF (bmin.GT.cv) THEN
         bmin=cv
         icpr=0
        END IF
       END IF
      END IF
      IF (Iqr.GT.icqr.AND.Iurqr.EQ.0) THEN
       i=iprs+Iqr-icqr
       cv=DABS(tval(i))
       IF (cv.LT.cval.AND.DABS(Arimap(i+ardsp)).LT.cmin) THEN
        icqr=icqr+1
        IF (bmin.GT.cv) THEN
         bmin=cv
         icpr=0
         icps=0
        END IF
       END IF
      END IF
      IF (Iqs.gt.icqs.AND.Iurqs.EQ.0) THEN
       i=iprs+iqrs-icqs
       cv=DABS(tval(i))
       IF (cv.LT.cval.AND.DABS(Arimap(i+ardsp)).LT.cmin) THEN
        icqs=icqs+1
        IF (bmin.GT.cv) THEN
         bmin=cv
         icpr=0
         icps=0
         icqr=0
        END IF
       END IF
      END IF
      Nnsig=Nnsig+icpr+icps+icqr+icqs
      IF(((iprs+iqrs).EQ.1).OR.((iurpr+iurps+iurqr+iurqs).GT.0))
     &   Nnsig=0
      IF (Nnsig.GE.1) THEN
       IF (icpr.GE.1) THEN
        DO WHILE (.TRUE.)
         DO i=Ipr,n-1
          Arimap(i+ardsp)=Arimap(i+1+ardsp)
         END DO
         Ipr=Ipr-1
         icpr=icpr-1
         IF (icpr.LE.0) GO TO 10
        END DO
       ELSE IF (icps.GE.1) THEN
        DO WHILE (.TRUE.)
         DO i=iprs,N-1
          Arimap(i+ardsp)=Arimap(i+1+ardsp)
         END DO
         Ips=Ips-1
         icps=icps-1
         IF (icps.LE.0) GO TO 10
        END DO
       ELSE IF (icqr.GE.1) THEN
        DO WHILE (.TRUE.)
         DO i=iprs+Iqr,N-1
          Arimap(i+ardsp)=Arimap(i+1+ardsp)
         END DO
         Iqr=Iqr-1
         icqr=icqr-1
         IF (icqr.LE.0) GO TO 10
        END DO
       ELSE
        DO WHILE (.TRUE.)
         DO i=iprs+iqrs,N-1
          Arimap(i+ardsp)=Arimap(i+1+ardsp)
         END DO
         Iqs=Iqs-1
         icqs=icqs-1
         IF (icqs.LE.0) GO TO 10
        END DO
       END IF
   10  CALL mdlint()
       CALL mdlset(Ipr,idr,Iqr,Ips,ids,Iqs,inptok)
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
