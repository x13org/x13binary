C     Last change:  BCM  23 Sep 1998   10:14 am
      SUBROUTINE prafce(Mt1,Mape,Outfer,Lfcst)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Print out average fcst std. error for the last three years
c-----------------------------------------------------------------------
      INCLUDE 'title.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      CHARACTER cfcst*(9)
      DOUBLE PRECISION Mape
      INTEGER i,Mt1
      LOGICAL Lfcst,Outfer
      DIMENSION Mape(4)
c-----------------------------------------------------------------------
      IF(Lfcst)THEN
       cfcst='forecasts'
      ELSE
       cfcst='backcasts'
      END IF
c-----------------------------------------------------------------------
      IF(.not.Lcmpaq)WRITE(Mt1,'()')
      IF(Outfer)THEN
       CALL mkPOneLine(Mt1,'@',
     &           'Average absolute percentage error in out-of-sample '//
     &                 cfcst//':')
      ELSE
       CALL mkPOneLine(Mt1,'@',
     &           'Average absolute percentage error in within-sample '//
     &                 cfcst//':')
      END IF
c-----------------------------------------------------------------------
      CALL mkPClass(Mt1,'indent')
      WRITE(Mt1,1020)(Mape(i),Cbr,i=1,3)
      WRITE(Mt1,1030)Mape(4)
 1020 FORMAT(' Last year: ',f6.2,a,' Last-1 year: ',f6.2,a,
     &      '     Last-2 year: ',f6.2,a)
 1030 FORMAT('  Last three years:  ',f6.2,'</p>')
c-----------------------------------------------------------------------
      RETURN
      END
