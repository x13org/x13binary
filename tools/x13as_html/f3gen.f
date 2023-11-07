C     Last change:  BCM  16 Feb 1999   11:15 am
      SUBROUTINE f3gen(Nw,Ny,Kfulsm,Lcmpaq)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- THIS SUBROUTINE PRINTS THE F3 TABLE.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'work2.cmn'
      INCLUDE 'mq3.cmn'
c-----------------------------------------------------------------------
      LOGICAL Lcmpaq
      INTEGER k,l,Nw,Ny,Kfulsm
*      CHARACTER span*7,blank*(30)
      CHARACTER span*7
      DIMENSION span(4)
c-----------------------------------------------------------------------
      INTEGER nblank
      EXTERNAL nblank
c-----------------------------------------------------------------------
      DATA span/'three  ','months ','one    ','quarter'/
*      DATA blank/'                              '/
c-----------------------------------------------------------------------
c     Print out individual quality control statistics
c-----------------------------------------------------------------------
      k=1
      IF(Ny.eq.4)k=3
      l=k+1
      IF(Lcmpaq)THEN
       CALL mkPOneLine(Nw,'center',
     &      'The measures below are between 0 and 3; acceptance '//
     &      'region from 0 to 1.')
      ELSE
       CALL mkPOneLine(Nw,'center',
     &      'All the measures below are in the range from 0 to 3 '//
     &      'with an acceptance region from 0 to 1.')
      END IF
      CALL mkTableTag(Nw,'x11',
     &                'Monitoring and Quality Assessment Statistics')
      CALL mkCaption(Nw,'Monitoring and Quality Assessment Statistics')
      CALL writTag(Nw,'<tr>')
      CALL mkTableCell(Nw,'headleft',
     &           '1. The relative contribution of the irregular over '//
     &          span(k)(1:nblank(span(k)))//span(l)(1:nblank(span(l)))//
     &                 ' span (from Table F 2.B).')
      WRITE(Nw,2010)'M1',Qu(1)
      CALL writTag(Nw,'</tr>')
      CALL writTag(Nw,'<tr>')
      CALL mkTableCell(Nw,'headleft','2. The relative contribution '//
     &                 'of the irregular component to the stationary '//
     &                 'portion of the variance (from Table F 2.F).')
      WRITE(Nw,2010)'M2',Qu(2)
      CALL writTag(Nw,'</tr>')
      CALL writTag(Nw,'<tr>')
      CALL mkTableCell(Nw,'headleft','3. The amount of '//
     &                 Moqu(1:nblank(Moqu))//' to '//
     &                 Moqu(1:nblank(Moqu))//
     &                 ' change in the irregular component '//
     &                 'as compared to the amount of '//
     &                 Moqu(1:nblank(Moqu))//' to '//
     &                 Moqu(1:nblank(Moqu))//
     &                 'change in the trend-cycle (from Table F2.H).')
      WRITE(Nw,2010)'M3',Qu(3)
      CALL writTag(Nw,'</tr>')
      CALL writTag(Nw,'<tr>')
      CALL mkTableCell(Nw,'headleft','4. The amount of '//
     &                 'autocorrelation in the irregular as '//
     &                 'described by the average duration of run '//
     &                 '(Table F 2.D).')
      WRITE(Nw,2010)'M4',Qu(4)
      CALL writTag(Nw,'</tr>')
      CALL writTag(Nw,'<tr>')
      CALL mkTableCell(Nw,'headleft','5. The number of '//
     &                 Moqu(1:nblank(Moqu))//
     &                 's it takes the change in the trend-cycle'//
     &                 'to surpass the amount of change in the '//
     &                 'irregular (from Table F 2.E).')
      WRITE(Nw,2010)'M5',Qu(5)
      CALL writTag(Nw,'</tr>')
      CALL writTag(Nw,'<tr>')
      IF(Kfulsm.lt.2)THEN
       CALL mkTableCell(Nw,'headleft',
     &                  '6. The amount of year to year change in the '//
     &                  'irregular as compared to the amount of year '//
     &                  'to year change in the seasonal (from Table '//
     &                  'F 2.H).')
       WRITE(Nw,2010)'M6',Qu(6)
       CALL writTag(Nw,'</tr>')
       CALL writTag(Nw,'<tr>')
      END IF
      CALL mkTableCell(Nw,'headleft',
     &                 '7. The amount of moving seasonality present '//
     &                 'relative to the amount of stable seasonality '//
     &                 '(from Table F 2.I).')
      WRITE(Nw,2010)'M7',Qu(7)
      CALL writTag(Nw,'</tr>')
      IF(Nn.ne.7)THEN
       CALL writTag(Nw,'<tr>')
       CALL mkTableCell(Nw,'headleft',
     &                  '8. The size of the fluctuations in the '//
     &                  'seasonal component throughout the whole '//
     &                  'series.')
       WRITE(Nw,2010)'M8',Qu(8)
       CALL writTag(Nw,'</tr>')
       CALL writTag(Nw,'<tr>')
       CALL mkTableCell(Nw,'headleft',
     &                  '9. The average linear movement in the '//
     &                  'seasonal component throughout the whole '//
     &                  'series.')
       WRITE(Nw,2010)'M9',Qu(9)
       CALL writTag(Nw,'</tr>')
       CALL writTag(Nw,'<tr>')
       CALL mkTableCell(Nw,'headleft',
     &               '10. Same as 8, calculated for recent years only.')
       WRITE(Nw,2010)'M10',Qu(10)
       CALL writTag(Nw,'</tr>')
       CALL writTag(Nw,'<tr>')
       CALL mkTableCell(Nw,'headleft',
     &               '11. Same as 9, calculated for recent years only.')
       WRITE(Nw,2010)'M11',Qu(11)
       CALL writTag(Nw,'</tr>')
      END IF
      CALL writTag(Nw,'</table>')
      IF(.not.Lcmpaq)CALL mkPOneLine(Nw,'@','&nbsp;')
c-----------------------------------------------------------------------
c     Print out Q values, acceptance/rejection information
c-----------------------------------------------------------------------
      IF(Qual.lt.0.8D0)THEN
       WRITE(Nw,3010)'<p class="center">',Qual,'</p>'
      ELSE IF(Qual.lt.1.0D0)THEN
       WRITE(Nw,3020)'<p class="center">',Qual,'</p>'
      ELSE IF(Qual.lt.1.2D0)THEN
       WRITE(Nw,3030)'<p class="center">',Qual,'</p>'
      ELSE
       WRITE(Nw,3040)'<p class="center">',Qual,'</p>'
      END IF
      IF(Kfail.gt.0)THEN
       WRITE(Nw,3050)'<p class="center">',Kfail,'</p>'
      END IF
      IF(Q2m2.lt.0.8D0)THEN
       WRITE(Nw,3060)'<p class="center">',Q2m2,'</p>'
      ELSE IF(Q2m2.lt.1.0D0)THEN
       WRITE(Nw,3070)'<p class="center">',Q2m2,'</p>'
      ELSE IF(Q2m2.lt.1.2D0)THEN
       WRITE(Nw,3080)'<p class="center">',Q2m2,'</p>'
      ELSE
       WRITE(Nw,3090)'<p class="center">',Q2m2,'</p>'
      END IF
      IF(.not.Lcmpaq)CALL mkPOneLine(Nw,'@','&nbsp;')
c-----------------------------------------------------------------------
      RETURN
c-----------------------------------------------------------------------
c     Formats for standard printout
c-----------------------------------------------------------------------
 2010 FORMAT('<td class="nowrap">',a,'  = ',F6.3,'</td>')
c-----------------------------------------------------------------------
c     Other output formats
c-----------------------------------------------------------------------
 3010 FORMAT(a,'<strong>ACCEPTED</strong> at the level  ',F5.2,a)
 3020 FORMAT(a,'<strong>CONDITIONALLY ACCEPTED</strong> at the level ',
     &         F5.2,a)
 3030 FORMAT(a,'<strong>CONDITIONALLY REJECTED</strong> at the level ',
     &         F5.2,a)
 3040 FORMAT(a,'<strong>REJECTED</strong> at the level ',F5.2,a)
 3050 FORMAT(a,'Check the ',i2,' above measures which failed.',a)
 3060 FORMAT(a,'Q (without M2) = ',f5.2,' <strong>ACCEPTED</strong>',a)
 3070 FORMAT(a,'Q (without M2) = ',f5.2,
     &         ' <strong>CONDITIONALLY ACCEPTED.</strong>',a)
 3080 FORMAT(a,'Q (without M2) = ',f5.2,
     &         ' <strong>CONDITIONALLY REJECTED</strong>',a)
 3090 FORMAT(a,'Q (without M2) = ',f5.2,' <strong>REJECTED</strong>',a)
      END
