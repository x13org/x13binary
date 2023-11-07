C     Last change:  BCM  25 Feb 1999    9:36 am
      SUBROUTINE cnvmdl(Ipr,Ips,Idr,Ids,Iqr,Iqs,Id,Ip,Iq,Iprs,Iqrs,N)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c       Converts X-13ARIMA-SEATS ARIMA modeling data structures to 
c       variables used by TRAMO/SEATS program.
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      CHARACTER tmpttl*(PGRPCR)
      INTEGER Ipr,Ips,Idr,Ids,Iqr,Iqs,Id,Ip,Iq,Iprs,Iqrs,N,iflt,begopr,
     &        endopr,nlag,iopr,ntmpcr
c     ------------------------------------------------------------------
c       Set up values for difference orders from variables on hand.
c     ------------------------------------------------------------------
      Idr=Nnsedf
      Ids=Nseadf
      Id=Idr+Sp*Ids
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
      DO iflt=AR,MA
       begopr=Mdl(iflt-1)
       endopr=Mdl(iflt)-1
       DO iopr=begopr,endopr
        nlag=Opr(iopr)-Opr(iopr-1)
        CALL getstr(Oprttl,Oprptr,Noprtl,iopr,tmpttl,ntmpcr)
        IF(Lfatal)RETURN
        IF(tmpttl(1:ntmpcr).eq.'Nonseasonal AR')THEN
         Ipr=nlag
        ELSE IF(tmpttl(1:ntmpcr).eq.'Seasonal AR')THEN
         Ips=nlag
        ELSE IF(tmpttl(1:ntmpcr).eq.'Nonseasonal MA')THEN
         Iqr=nlag
        ELSE IF(tmpttl(1:ntmpcr).eq.'Seasonal MA')THEN
         Iqs=nlag
        END IF
       END DO
      END DO
c     ------------------------------------------------------------------
c       Finish setting up other variables relating to AR and MA
c     ------------------------------------------------------------------
      Ip=Ipr+Sp*Ips
      Iq=Iqr+Sp*Iqs
      Iprs=Ipr+Ips
      Iqrs=Iqr+Iqs
      N=Iprs+Iqrs
c     ------------------------------------------------------------------
      RETURN
      END
