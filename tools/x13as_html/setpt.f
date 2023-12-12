C     Last change:  BCM  24 Nov 97   12:47 pm
      SUBROUTINE setpt(Mt1,Arma,Str)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     Set up variable used to print out ARIMA model parameters in
c     automatic modeling.
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      CHARACTER tmpttl*(POPRCR),Str*(*)
      INTEGER Mt1,begopr,endopr,iopr,beglag,endlag,ntmpcr,ilag,Arma,i,
     &        i2,npt
      DOUBLE PRECISION pt
      DIMENSION pt(PARIMA)
c     ------------------------------------------------------------------
      npt=0
      begopr=Mdl(Arma-1)
      endopr=Mdl(Arma)-1
      DO iopr=begopr,endopr
       beglag=Opr(iopr-1)
       endlag=Opr(iopr)-1
       CALL getstr(Oprttl,Oprptr,Noprtl,iopr,tmpttl,ntmpcr)
       IF(Lfatal)RETURN
       IF(tmpttl(1:ntmpcr).eq.Str)THEN
        DO ilag=beglag,endlag
         npt=npt+1
         pt(npt)=Arimap(ilag)
        END DO
       END IF
      END DO
c     ------------------------------------------------------------------
c     If npt > 0, print out parameter estimates
c     ------------------------------------------------------------------
      IF(npt.gt.0)THEN
       i2=npt
       IF(npt.gt.5)i2=5
       WRITE(Mt1,1010)Str,(pt(i),i=1,i2)
 1010  FORMAT('<p>',a,' parameter estimates: ',5f8.3)
       IF(i2.lt.npt)WRITE(Mt1,1020)(pt(i),i=i2+1,npt)
 1020  FORMAT(t40,5f8.3)
       CALL writTag(Mt1,'</p>')
      END IF
      RETURN
c     ------------------------------------------------------------------
      END
