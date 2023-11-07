C     Last change:  BCM  30 Sep 2005    3:39 pm
      SUBROUTINE nmlmdl(Nn,Ipr,Ips,Idr,Ids,Iqr,Iqs,Th,Bth,Phi,BPhi,
     &                  Xl,Nfixed)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c       Converts X-13ARIMA-SEATS ARIMA modeling data structures to 
c       variables used by TRAMO/SEATS program.
c     ------------------------------------------------------------------
c     Changed by REG, on 2 Jun 2005, to add Nfixed output variable
c     that identifies the number of parameters fixed by the user.
c     ------------------------------------------------------------------
      LOGICAL T
      INTEGER N10
      PARAMETER(N10=64,T=.true.)
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'stdio.i'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      CHARACTER tmpttl*(PGRPCR)
      DOUBLE PRECISION ur,Xl,Th,Bth,Phi,BPhi,xmin,xmax
c     Modified by REG on 2 Jun 2005 to add Nfixed output variable
      INTEGER ardsp,Ipr,Ips,Idr,Ids,Iqr,Iqs,iparma,iflt,Nn,
     &        begopr,endopr,nlag,iopr,ntmpcr,i,iprs,iqrs,Nfixed
      DIMENSION Th(Nn),Bth(Nn),Phi(Nn),BPhi(Nn),xmin(N10),xmax(N10)
      DOUBLE PRECISION x,x2
c      INTEGER id,ip,iq,pbp,ps,qbq
      DIMENSION x(N10),x2(N10)
c     ------------------------------------------------------------------
c       Set up values for difference orders from variables on hand.
c     ------------------------------------------------------------------
      ur = 1.0d0
      CALL setdp(Xl,N10,xmax)
      CALL setdp(-Xl,N10,xmin)
      Idr=Nnsedf
      Ids=Nseadf
      ardsp=Nnsedf+Nseadf
c     ------------------------------------------------------------------
c       Initialize terms for nonseasonal and seasonal AR, MA
c     ------------------------------------------------------------------
      Ipr=0
      Ips=0
      Iqr=0
      Iqs=0
c-----------------------------------------------------------------------
c       Loop through other operators, getting number of lags in each
c     ------------------------------------------------------------------
      iparma=ardsp+1
c     Added by REG on 2 Jun 2005 to initialize Nfixed
      Nfixed=0
      DO iflt=AR,MA
       begopr=Mdl(iflt-1)
       endopr=Mdl(iflt)-1
       DO iopr=begopr,endopr
        nlag=Opr(iopr)-Opr(iopr-1)
        CALL getstr(Oprttl,Oprptr,Noprtl,iopr,tmpttl,ntmpcr)
c     Added by BCM on 29 Sep 2005 to allow program to exit when
c     limit of ARMA parameters reached by program.
        IF((.not.Lfatal).and.nlag.gt.Nn)THEN
         WRITE(Mt1,1011)Nn,tmpttl(1:ntmpcr)
         WRITE(Mt2,1011)Nn,tmpttl(1:ntmpcr)
         WRITE(STDERR,1010)Nn,tmpttl(1:ntmpcr)
 1010    FORMAT('  NOTE: The SEATS signal extraction routines cannot',
     &          ' process more than ',i3,/,'        ',a,' terms.',/,
     &          '        The program will stop executing; try ',
     &          'specifying another ARIMA model.',/)
 1011    FORMAT(/,' <p><strong>NOTE:</strong> The SEATS signal ',
     &       ' extraction routines cannot process more than ',i3,/,
     &       ' ',a,' terms.</p>',/,
     &       ' <p>The program will stop executing; try specifying',
     &       ' another ARIMA model.</p>',/)
         CALL abend
        END IF
        IF(Lfatal)RETURN
        IF(tmpttl(1:ntmpcr).eq.'Nonseasonal AR')THEN
         Ipr=nlag
         DO i=1,nlag
          IF(Arimal(iparma).ne.i)THEN
           CALL nWritln('The SEATS signal extraction routines cannot'//
     &                  ' process missing lag models.',Mt1,Mt2,T,T)
           CALL writln('The program will stop executing; '//
     &                 'try specifying another ARIMA model.',
     &                 Mt1,Mt2,T,T)
           WRITE(STDERR,1012)
 1012      FORMAT('  NOTE: The SEATS signal extraction routines cannot',
     &            ' process missing lag models.',/,
     &            '        The program will stop executing; try ',
     &            'specifying another ARIMA model.',/)
           CALL abend
          END IF
          IF(Lfatal)RETURN
          Phi(i)=0D0-Arimap(iparma)
          x2(iparma-ardsp)=Phi(i)
c     Added by REG on 2 Jun 2005 to increment Nfixed
          IF(Arimaf(iparma))Nfixed=Nfixed+1
          iparma=iparma+1
         END DO
        ELSE IF(tmpttl(1:ntmpcr).eq.'Seasonal AR')THEN
         Ips=nlag
         DO i=1,nlag
          IF(Arimal(iparma).ne.i*Sp)THEN
           CALL nWritln('The SEATS signal extraction routines cannot'//
     &                  ' process missing lag models.',Mt1,Mt2,T,T)
           CALL writln('The program will stop executing; '//
     &                 'try specifying another ARIMA model.',
     &                 Mt1,Mt2,T,T)
           WRITE(STDERR,1012)
           CALL abend
          END IF
          IF(Lfatal)RETURN
          BPhi(i)=0D0-Arimap(iparma)
          x2(iparma-ardsp)=Bphi(i)
c     Added by REG on 2 Jun 2005 to increment Nfixed
          IF(Arimaf(iparma))Nfixed=Nfixed+1
          iparma=iparma+1
         END DO
        ELSE IF(tmpttl(1:ntmpcr).eq.'Nonseasonal MA')THEN
         Iqr=nlag
         DO i=1,nlag
          IF(Arimal(iparma).ne.i)THEN
           CALL nWritln('The SEATS signal extraction routines cannot'//
     &                  ' process missing lag models.',Mt1,Mt2,T,T)
           CALL writln('The program will stop executing; '//
     &                 'try specifying another ARIMA model.',
     &                 Mt1,Mt2,T,T)
           WRITE(STDERR,1012)
           CALL abend
          END IF
          IF(Lfatal)RETURN
          Th(i)=0D0-Arimap(iparma)
          x2(iparma-ardsp)=Th(i)
c     Added by REG on 2 Jun 2005 to increment Nfixed
          IF(Arimaf(iparma))Nfixed=Nfixed+1
          iparma=iparma+1
         END DO
        ELSE IF(tmpttl(1:ntmpcr).eq.'Seasonal MA')THEN
         Iqs=nlag
         DO i=1,nlag
          IF(Arimal(iparma).ne.i*Sp)THEN
           CALL nWritln('The SEATS signal extraction routines cannot'//
     &                  ' process missing lag models.',Mt1,Mt2,T,T)
           CALL writln('The program will stop executing; '//
     &                 'try specifying another ARIMA model.',
     &                 Mt1,Mt2,T,T)
           WRITE(STDERR,1012)
           CALL abend
          END IF
          IF(Lfatal)RETURN
          BTh(i)=0D0-Arimap(iparma)
          x2(iparma-ardsp)=Bth(i)
c     Added by REG on 2 Jun 2005 to increment Nfixed
          IF(Arimaf(iparma))Nfixed=Nfixed+1
          iparma=iparma+1
         END DO
c         IF(nlag.eq.1.and.Bth(1).lt.-0.995)Bth(1)=-0.995
        END IF
       END DO
      END DO
      Iprs=Ipr+Ips
      IQRS=IQR+IQS
c      pq=Ipr+Ips+Iqr
c      qbq=pq+iqs
      IF(Ipr.gt.0)THEN
       CALL TRANS0(x2,N10,x,1,Ipr,Iprs,Ur,Xl)
       CALL TRANS2(x2,N10,x,0,Ipr)
       DO i=1,Ipr
        Phi(i)=x2(i)
       END DO
      END IF
      IF(Ips.gt.0)THEN
       CALL TRANS0(x2,N10,X,Ipr+1,Iprs,Iprs,Ur,Xl)
       CALL TRANS2(x2,N10,x,Ipr,Iprs)
       DO i=1,Ips
        BPhi(i)=x2(i+Ipr)
       END DO
      END IF
      IF(Iqr.gt.0)THEN
       CALL TRANS0(x2,N10,X,Iprs+1,Iprs+Iqr,Iprs,Ur,Xl)
       CALL TRANS2(x2,N10,x,Iprs,Iprs+Iqr)
       DO i=1,Iqr
        Th(i)=x2(IPRS+I)
       END DO
      END IF
      IF(Iqs.gt.0)THEN
       CALL TRANS0(x2,N10,X,Iprs+Iqr+1,IPRS+IQRS,Iprs,Ur,Xl)
       CALL TRANS2(x2,N10,x,Iprs+Iqr,IPRS+IQRS)
       DO i=1,Iqs
        BTh(i)=x2(IPRS+IQR+I)
       END DO
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
