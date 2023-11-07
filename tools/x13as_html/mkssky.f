      SUBROUTINE mkssky(Fnotky,Nssky,Nopt,Nop2)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Generate key for sliding spans table
c-----------------------------------------------------------------------
      INTEGER PSSFL1,PSSFL3,PSSFL4,PSSTP,PSSSC,PSSNT
      PARAMETER(PSSFL1=1,PSSFL3=3,PSSFL4=4,PSSTP=5,PSSSC=6,PSSNT=7)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'ssap.prm'
      INCLUDE 'ssap.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'title.cmn'
c-----------------------------------------------------------------------
      INTEGER Fnotky,Nssky,Nopt,Nop2,i
      DIMENSION Fnotky(7)
c-----------------------------------------------------------------------
c    Print an explanation for each footnote
c-----------------------------------------------------------------------
      IF(Fnotky(PSSNT).eq.1)THEN
       CALL writTag(Mt1,'<tr>')
       CALL mkTableCell(Mt1,'head','NT')
       CALL mkTableCell(Mt1,'@','Observation not included in sliding'//
     &                  ' spans comparisons.')
       CALL writTag(Mt1,'</tr>')
      END IF
c-----------------------------------------------------------------------
      IF(Fnotky(PSSSC).eq.1)THEN
       CALL writTag(Mt1,'<tr>')
       IF(Nop2.gt.0)THEN
        CALL mkTableCell(Mt1,'head','SC')
        CALL mkTableCell(Mt1,'@','A sign change can be found for '//
     &                   'this observation.')
       ELSE
        CALL mkTableCell(Mt1,'center','IE')
        WRITE(Mt1,1071)
 1071   FORMAT('<td> The estimates of this effect are inconsistent',
     &         ' for this observation;',
     &       /,' one span indicates that the effect causes an ',
     &         'increase in the ',
     &       /,' observed value, another that it causes a decrease.',
     &         '</td>')
       END IF
       CALL writTag(Mt1,'</tr>')
      END IF
c-----------------------------------------------------------------------
      IF(Fnotky(PSSTP).eq.1)THEN
       CALL writTag(Mt1,'<tr>')
       CALL mkTableCell(Mt1,'head','TP')
       CALL mkTableCell(Mt1,'@','Span values for this observation '//
     &                  'have a turning point.')
       CALL writTag(Mt1,'</tr>')
      END IF
c-----------------------------------------------------------------------
      DO i=PSSFL1,PSSFL3
       IF(Fnotky(i).gt.0)THEN
        CALL writTag(Mt1,'<tr>')
        WRITE(Mt1,1020)i,Ch(Nopt),Cut(Nopt,i),Cut(Nopt,i+1)
 1020   FORMAT('<td class="head">',i1,a1,'</td>',/,
     &         '<td>The maximum percentage difference is greater than ',
     &         'or equal to ',f4.1,'%',/,' but less than ',f4.1,
     &         '%.</td>')
        CALL writTag(Mt1,'</tr>')
       END IF
      END DO
      IF(Fnotky(PSSFL4).eq.1)THEN
       CALL writTag(Mt1,'<tr>')
       WRITE(Mt1,1030)PSSFL4,Ch(Nopt),Cut(Nopt,PSSFL4)
 1030  FORMAT('<td class="head">',i1,a1,'</td>',/,
     &        '<td> The maximum percentage difference is greater than ',
     &        'or equal to ',f4.1,'%.</td>')
       CALL writTag(Mt1,'</tr>')
      END IF
c-----------------------------------------------------------------------
      CALL writTag(Mt1,'</table>')
      CALL mkPOneLine(Mt1,'@','&nbsp;')
c-----------------------------------------------------------------------
      RETURN
      END
      