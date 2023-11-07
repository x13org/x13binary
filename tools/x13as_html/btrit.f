C     Last change:  BCM  20 May 1998   11:27 am
**==btrit.f    processed by SPAG 4.03F  at 14:07 on 24 Aug 1994
      SUBROUTINE btrit(Nyearz,Nopt,No2,Iagr,Ext,Eststr,Nstr,Cpobs,
     &                 Lrange,Ssdiff,Lp,Ls)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C  *****  PRINTS OUT SUMMARY BREAKDOWN TABLES FOR TOTAL, MONTHS, YEARS.
C  *****  CALCULATES PERCENTAGE OF MONTHS FLAGGED (FPER), PERCENTAGE OF
C  *****  MONTHS WITH CHANGE IN DIRECTION (SPER), PERCENTAGE OF MONTHS
C  *****  WITH CHANGE OF DIRECTION THAT WERE FLAGGED (SFPER).
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'ssap.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'tfmts.cmn'
      INCLUDE 'ssap.cmn'
      INCLUDE 'sspvec.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      LOGICAL Ls,Lp,Lrange,Ssdiff
      CHARACTER Eststr*(45),Ext*(2),Cpobs*(9),fmt*(35)
      INTEGER Iagr,ij,iy,j,j1,Nstr,next,No2,Nopt,Nyearz,lfmt,nfmt
      DIMENSION Cpobs(20)
c-----------------------------------------------------------------------
      INTEGER nblank
      EXTERNAL nblank
c----------------------------------------------------------------------
      IF(.not.(Lp.or.Ls))RETURN
c----------------------------------------------------------------------
c     Set number of nonblank characters in Ext
c----------------------------------------------------------------------
      next=1
      IF(Iagr.eq.6)next=2
c----------------------------------------------------------------------
c     Print out table header
c----------------------------------------------------------------------
      IF(Lp)THEN
       IF(Ssdiff)THEN
        WRITE(Mt1,1006)Ext,Eststr(1:Nstr),Serno(1:Nser)
 1006   FORMAT('<h3>S  3.',a2,'  Breakdown of the Average Maximum ',
     &         'Absolute Differences across spans for ',a,' of ',
     &         a,'.</h3>',/)
*        END IF
       ELSE IF(Lrange)THEN
        WRITE(Mt1,1011)Ext,Eststr(1:Nstr),Eststr(1:Nstr),Serno(1:Nser)
 1011   FORMAT(/,'<h3>S  3.',a2,'  Breakdowns of unstable ',a,/,10x,
     &           'and Average Maximum Percent Differences across ',
     &           'spans for ',a,' of ',a,'.</h3>',/)
*        END IF
       ELSE
        WRITE(Mt1,1016)Ext,Eststr(1:Nstr),Serno(1:Nser)
 1016   FORMAT('<h3>S  3.',a2,'  Breakdown of the Average Maximum ',
     &         'Percent Differences across spans for ',a,' of ',a,
     &         '.</h3>',/)
       END IF
       IF(Iagr.eq.6)
     &    CALL mkPOneLine(Mt1,'@','Indirect seasonal adjustment')
      END IF
c----------------------------------------------------------------------
c     Print out summaries for each period and year
c----------------------------------------------------------------------
      ij=Icyr-Iyr
      IF(Icyr.eq.Iyr)THEN
       IF(No2.eq.3)ij=ij+1
       IF((No2.eq.1.or.No2.eq.2).and.Im.eq.Nsea)ij=ij+1
      END IF
c----------------------------------------------------------------------
      IF(Lp)THEN
       CALL mkTableTag(Mt1,'w40','@')
       IF(Nsea.eq.12)THEN
        CALL mkCaption(Mt1,'Breakdown by Month')
       ELSE
        CALL mkCaption(Mt1,'Breakdown by Quarter')
       END IF
       CALL writTag(Mt1,'<tr>')
       CALL mkTableCell(Mt1,'head','&nbsp;')
       IF((.not.Ssdiff).and.Lrange)THEN
        CALL mkHeaderCellScope(Mt1,0,0,'col','@',
     &                         'Observations'//Cbr//'Flagged')
       END IF
       IF(Ssdiff)THEN
        CALL mkHeaderCellScope(Mt1,0,0,'col','@',
     &                   'Average Maximum'//Cbr//'Absolute Differences')
       ELSE
        CALL mkHeaderCellScope(Mt1,0,0,'col','@',
     &                    'Average Maximum'//Cbr//'Percent Differences')
       END IF
       CALL writTag(Mt1,'</tr>')
       
       j1=0
       IF(Nsea.eq.4)j1=12
       DO j=1,Nsea
        CALL writTag(Mt1,'<tr>')
        CALL mkHeaderCellScope(Mt1,0,0,'row','@',Cpobs(j+j1))
        IF((.not.Ssdiff).and.Lrange)THEN
         WRITE(Mt1,1020)SSnobs(j,Nopt),Aobs(j,Nopt)
        ELSE
         IF(Tblwid.gt.6)THEN
          WRITE(Mt1,1031)Aobs(j,Nopt)
         ELSE
          WRITE(Mt1,1030)Aobs(j,Nopt)
         END IF
        END IF
        CALL writTag(Mt1,'</tr>')
       END DO
 1020  FORMAT('<td>',i6,'</td><td> ',F10.2,' </td>')
 1030  FORMAT('<td> ',F10.2,' </td>')
 1031  FORMAT('<td> ',G17.10,' </td>')
c-----------------------------------------------------------------------
       CALL writTag(Mt1,'</table>')
       CALL mkPOneLine(Mt1,'@','&nbsp;')
c-----------------------------------------------------------------------
       CALL mkTableTag(Mt1,'w40','@')
       CALL mkCaption(Mt1,'Breakdown by Year')
       CALL writTag(Mt1,'<tr>')
       CALL mkTableCell(Mt1,'head','&nbsp;')
       IF((.not.Ssdiff).and.Lrange)THEN
        CALL mkHeaderCellScope(Mt1,0,0,'col','@',
     &                         'Observations'//Cbr//'Flagged')
       END IF
       IF(Ssdiff)THEN
        CALL mkHeaderCellScope(Mt1,0,0,'col','@',
     &                   'Average Maximum'//Cbr//'Absolute Differences')
       ELSE
        CALL mkHeaderCellScope(Mt1,0,0,'col','@',
     &                    'Average Maximum'//Cbr//'Percent Differences')
       END IF
       CALL writTag(Mt1,'</tr>')
       DO j=ij,Nyearz
        iy=Iyr+j
        CALL writTag(Mt1,'<tr>')
        WRITE(Mt1,1040)iy
 1040   FORMAT('<th scope="row">',i6,'</th>')
        IF((.not.Ssdiff).and.Lrange)THEN
         WRITE(Mt1,1020)SSnyr(j,Nopt),Ayr(j,Nopt)
        ELSE
         IF(Tblwid.gt.6)THEN
          WRITE(Mt1,1031)Ayr(j,Nopt)
         ELSE
          WRITE(Mt1,1030)Ayr(j,Nopt)
         END IF
        END IF
        CALL writTag(Mt1,'</tr>')
       END DO
       CALL writTag(Mt1,'</table>')
       CALL mkPOneLine(Mt1,'@','&nbsp;')
c-----------------------------------------------------------------------
      END IF
c----------------------------------------------------------------------
c     Save summaries for each period and year
c----------------------------------------------------------------------
      IF(Ls)THEN
       j1=0
       IF(Nsea.eq.4)j1=16
       DO j=1,Nsea
        IF((.not.Ssdiff).and.Lrange)THEN
         WRITE(Nform,1070)Ext(1:next),j,Cpobs(j+j1)(1:3),SSnobs(j,Nopt),
     &                    Aobs(j,Nopt)
        ELSE
         WRITE(Nform,1070)Ext(1:next),j,Cpobs(j+j1)(1:3),0,Aobs(j,Nopt)
        END IF
 1070   FORMAT('s3.',a,'.brk.p',i2.2,': ',A3,1x,I3,2X,E17.10)
       END DO
       DO j=ij,Nyearz
        iy=Iyr+j
        IF((.not.Ssdiff).and.Lrange)THEN
         WRITE(Nform,1080)Ext(1:next),j,iy,SSnyr(j,Nopt),Ayr(j,Nopt)
        ELSE
         WRITE(Nform,1080)Ext(1:next),j,iy,0,Ayr(j,Nopt)
        END IF
 1080   FORMAT('s3.',a,'.brk.y',i2.2,': ',I4,1x,I3,2X,E17.10)
       END DO
      END IF
c----------------------------------------------------------------------
      RETURN
      END
