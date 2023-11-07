C     Last change:  BCM  25 Feb 1999    9:36 am
      SUBROUTINE chksmd(Nn)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c       Checks X-13ARIMA-SEATS ARIMA modeling data structures to see if 
c       the model can be used by the SEATS seasonal adjustment routines.
c     ------------------------------------------------------------------
      LOGICAL T
      PARAMETER(T=.true.)
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'stdio.i'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'units.cmn'
c     ------------------------------------------------------------------
      CHARACTER tmpttl*(PGRPCR)
      INTEGER Nn,iflt,begopr,endopr,nlag,iopr,ntmpcr,iparma,ardsp,i
c-----------------------------------------------------------------------
c set counter for first AR/MA coefficient
c-----------------------------------------------------------------------
      ardsp=Nnsedf+Nseadf
      iparma=ardsp+1
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
c     ------------------------------------------------------------------
c     check to see if there are too many lags for SEATS to handle
c     ------------------------------------------------------------------
        IF(nlag.gt.Nn)THEN
         WRITE(Mt1,1011)Nn,tmpttl(1:ntmpcr)
         WRITE(Mt2,1011)Nn,tmpttl(1:ntmpcr)
         WRITE(STDERR,1010)Nn,tmpttl(1:ntmpcr)
         CALL abend
        END IF
c     ------------------------------------------------------------------
c     check to see if there are missing lags in the model
c     ------------------------------------------------------------------
        IF(tmpttl(1:ntmpcr).eq.'Nonseasonal AR'.or.
     &     tmpttl(1:ntmpcr).eq.'Nonseasonal MA')THEN
         DO i=1,nlag
          IF(Arimal(iparma).ne.i)THEN
           CALL nWritln('The SEATS signal extraction routines cannot'//
     &                  ' process missing lag models.',Mt1,Mt2,T,T)
           CALL writln('The program will stop executing; '//
     &                 'try specifying another ARIMA model.',
     &                 Mt1,Mt2,T,T)
           WRITE(STDERR,1020)
           CALL abend
          END IF
          IF(Lfatal)RETURN
          iparma=iparma+1
         END DO
        ELSE IF(tmpttl(1:ntmpcr).eq.'Seasonal AR'.or.
     &          tmpttl(1:ntmpcr).eq.'Seasonal MA')THEN
         DO i=1,nlag
          IF(Arimal(iparma).ne.i*Sp)THEN
           CALL nWritln('The SEATS signal extraction routines cannot'//
     &                  ' process missing lag models.',Mt1,Mt2,T,T)
           CALL writln('The program will stop executing; '//
     &                 'try specifying another ARIMA model.',
     &                 Mt1,Mt2,T,T)
           WRITE(STDERR,1020)
           CALL abend
          END IF
          IF(Lfatal)RETURN
          iparma=iparma+1
         END DO
        END IF
       END DO
      END DO
c     ------------------------------------------------------------------
 1010 FORMAT(/,'  NOTE: The SEATS signal extraction routines cannot',
     &       ' process more than ',i3,/,'        ',a,' terms.',/,
     &       '        The program will stop executing; try specifying',
     &       ' another ARIMA model.',/)
 1011 FORMAT(/,' <p><strong>NOTE:</strong> The SEATS signal ',
     &       ' extraction routines cannot process more than ',i3,/,
     &       ' ',a,' terms.</p>',/,
     &       ' <p>The program will stop executing; try specifying',
     &       ' another ARIMA model.</p>',/)
 1020 FORMAT(/,'  NOTE: The SEATS signal extraction routines cannot',
     &       ' process missing lag models.',/,
     &       '        The program will stop executing; try specifying',
     &       ' another ARIMA model.',/)
      RETURN
      END
      
