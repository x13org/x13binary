C     Last change:  BCM   6 May 2003    1:33 pm
      SUBROUTINE tblhdr(Ktabl,Itype,Ixreg,Nobs,Begtbl,Nny,Y,Tbltit,
     &                  Ntbttl)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This subroutine produces a header for the tabular output produced
c     by the suboutine TABLE.
c-----------------------------------------------------------------------
c     Kpart is the iteration of the table
c     Ktabl is the table number
c     Nobs is the number of observations
c     Begtbl is the beginning date
c     Lyr is year of first observation
c     Nny is seasonal frequency (12 for monthly, 4 for quarterly)
C     Y is an additional array to be printed on table.
c     Tbltit is the title for the table
c-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'tbltitle.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'x11reg.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11msc.cmn'
      INCLUDE 'priusr.cmn'
c      INCLUDE 'prior.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'force.cmn'
      INCLUDE 'mq3.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      LOGICAL lsdiff,subhdr
      DOUBLE PRECISION Y
      INTEGER Ktabl,Begtbl,Nobs,Nny,i,Itype,Ixreg,nttl,n,m,Ntbttl
      CHARACTER Tbltit*(PTTLEN),avgs*(8),ttl2*(80)
      DIMENSION Begtbl(2),Y(*),avgs(7)
c-----------------------------------------------------------------------
      INTEGER nblank
      LOGICAL dpeq
      EXTERNAL nblank,dpeq
c-----------------------------------------------------------------------
      DATA avgs/'Default ','3 x 3   ','3 x 5   ','3 x 9   ','3 x 15  ',
     &          'Stable  ','3 x 1   '/
c-----------------------------------------------------------------------
c     Call header routine to print title and date information
c-----------------------------------------------------------------------
*      lpttl=T
*      IF(Kpart.eq.1.and.Ktabl.eq.2.and.Itype.gt.1)THEN
*       IF(.not.Lcmpaq)THEN
*        IF(Itype.eq.2)THEN
*         WRITE(Mt1,Ttlfmt)Newpg,Title(1:Ntitle),Kpage,Tmpser(1:Ntser)
*        ELSE IF(Itype.eq.3)THEN
*         WRITE(Mt1,Ttlfmt)Newpg,Title(1:Ntitle),Kpage,Prmser(1:Npser)
*        END IF
*       END IF
*       Kpage=Kpage+1
*       lpttl=F
*      END IF
c-----------------------------------------------------------------------
c     Generate subtitles for selected tables.
c-----------------------------------------------------------------------
      CALL mkshdr(ttl2,nttl,Ktabl,Itype,subhdr)
      IF(subhdr)THEN
       CALL prshd2(Tbltit,Ntbttl,ttl2,nttl,Begtbl,Nny,Nobs)
      ELSE
       CALL prtshd(Tbltit(1:Ntbttl),Begtbl,Nny,Nobs)
      END IF
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Print additional information for specific tables
c-----------------------------------------------------------------------
      IF((.not.(Kpart.eq.1.and.Ktabl.eq.1)).AND.(Ixreg.eq.2.OR.
     &  (Khol.eq.1.OR.(Kpart.eq.0.and.Ktabl.eq.1))))THEN
       IF(Ixreg.eq.2.AND.(Khol.eq.1.OR.(Kpart.eq.0.and.Ktabl.eq.1)))THEN
        CALL mkPOneLine(Mt1,'@','First pass - Estimating '//
     &                  'irregular regression and X-11 Easter effects')
       ELSE IF(Ixreg.eq.2)THEN
        CALL mkPOneLine(Mt1,'@','First pass - Estimating '//
     &                  'irregular regression effects')
       ELSE
        CALL mkPOneLine(Mt1,'@','First pass - Estimating '//
     &                  'X-11 Easter effects')
       END IF
      END IF
c-----------------------------------------------------------------------
      IF((Kpart.eq.1.and.Ktabl.eq.4).or.(Kpart.eq.3.and.Ktabl.eq.16)
     &   .or.(Kpart.eq.3.and.Ktabl.eq.18))THEN
       IF(.not.dpeq(Y(1),DNOTST))THEN
        CALL mkTableTag(Mt1,'x11','@')
        IF(Kpart.eq.1.and.Ktabl.eq.4)THEN
         CALL mkCaption(Mt1,'Prior daily weights')
        ELSE IF(Kpart.eq.3.and.Ktabl.eq.16)THEN
         CALL mkCaption(Mt1,'Daily weights')
        ELSE
         CALL mkCaption(Mt1,'Combined daily weights')
        END IF
        CALL writTag(Mt1,'<tr>')
        CALL mkTableCell(Mt1,'head','&nbsp;')
        CALL mkHeaderCellScope(Mt1,0,0,'col','Monday','Mon')
        CALL mkHeaderCellScope(Mt1,0,0,'col','Tuesday','Tue')
        CALL mkHeaderCellScope(Mt1,0,0,'col','Wednesday','Wed')
        CALL mkHeaderCellScope(Mt1,0,0,'col','Thursday','Thu')
        CALL mkHeaderCellScope(Mt1,0,0,'col','Friday','Fri')
        CALL mkHeaderCellScope(Mt1,0,0,'col','Saturday','Sat')
        CALL mkHeaderCellScope(Mt1,0,0,'col','Sunday','Sun')
        CALL writTag(Mt1,'</tr>')
        CALL writTag(Mt1,'<tr>')
        CALL mkHeaderCellScope(Mt1,0,0,'row','Trading Day Daily Weight',
     &                         'TD Daily Wt.')
        DO i=1,7
         WRITE(Mt1,1010)Y(i)
        END DO
        CALL writTag(Mt1,'</tr>')
        CALL writTag(Mt1,'</table>')
        CALL mkPOneLine(Mt1,'@','&nbsp;')
       END IF
 1010  FORMAT('<td>',F8.3,'</td>')
c-----------------------------------------------------------------------
      ELSE IF((Kpart.ge.2.and.Kpart.le.4).and.Ktabl.eq.2)THEN
c       nyy=(Iwt+1)*Nny
c       WRITE(Mt1,1040)nyy
       WRITE(Mt1,1040)Nny
 1040  FORMAT('<p>Trend filter : Centered ',i3,
     &        '-term moving average</p>')
      ELSE IF((Kpart.ge.2.and.Kpart.le.4).and.
     &        (Ktabl.eq.7.or.Ktabl.eq.12))THEN
       WRITE(Mt1,1050)Nterm,Cbr,Ratic
 1050  FORMAT('<p>Trend filter : ',i3,'-term Henderson moving average',
     &        a,/,'  I/C ratio : ',F6.2,'</p>')
c      ELSE IF(Kpart.eq.5.and.Ktabl.eq.12)THEN
c       WRITE(Mt1,1060)Adjtrn
c 1060  FORMAT('  Trend filter   ',i3,'-term Henderson moving average')
c-----------------------------------------------------------------------
      ELSE IF((Ktabl.eq.5.and.(Kpart.eq.2.or.Kpart.eq.3.or.Kpart.eq.4))
     &       .or.(Ktabl.eq.10.and.(Kpart.eq.2.or.Kpart.eq.3.or.
     &       (Kpart.eq.4.and.Itype.eq.1))))THEN
       lsdiff=.false.
       DO i=2,Nny
        IF(Lter(i).ne.0.and.Lter(i).ne.Lterm)lsdiff=.true.
       END DO
       IF(lsdiff)THEN
        CALL mkPOneLine(Mt1,'@',
     &             'Seasonal filter : Different moving averages used '//
     &                  Moqu(1:nblank(Moqu)))
       ELSE
        CALL mkPOneLine(Mt1,'@','Seasonal filter : '//
     &                  avgs(Mtype)(1:nblank(avgs(Mtype)))//
     &                  ' moving average')
       END IF
       IF(Kpart.eq.4.and.Ishrnk.gt.0)THEN
        IF(Ishrnk.eq.1)THEN
         CALL mkPOneLine(Mt1,'@',
     &                'Global shrinkage technique applied to seasonal.')
        ELSE IF(Ishrnk.eq.2)THEN
         CALL mkPOneLine(Mt1,'@',
     &                'Local shrinkage technique applied to seasonal.')
        END IF
       END IF
c-----------------------------------------------------------------------
      ELSE IF((Kpart.eq.2.and.Ktabl.eq.1).and.Nbcst.gt.0)THEN
       WRITE(Mt1,1090)Nbcst
 1090  FORMAT('<p>Includes ',i2,' backcasts.</p>')
c-----------------------------------------------------------------------
      ELSE IF((Kpart.eq.2.or.Kpart.eq.3).and.Ktabl.eq.14)THEN
       WRITE(Mt1,1091)Sigxrg
 1091  FORMAT('<p>Irregular component regression sigma limit   ',f5.2,
     &        '</p>')
c-----------------------------------------------------------------------
      ELSE IF((Kpart.eq.2.or.Kpart.eq.3).and.Ktabl.eq.17)THEN
       WRITE(Mt1,1100)Sigml,Cbr,Sigmu
 1100  FORMAT('<p>Lower sigma limit : ',f5.2,a,/,
     &        '  Upper sigma limit : ',f5.2,'</p>')
      ELSE IF(Kpart.eq.5.and.(Ktabl.ge.1.and.Ktabl.le.3))THEN
       IF(Adjao.eq.1.and.Adjtc.eq.1)THEN
        CALL mkPOneLine(Mt1,'@','AO &amp; TC outliers removed')
       ELSE IF(Adjao.eq.1)THEN
        CALL mkPOneLine(Mt1,'@','AO outliers removed')
       ELSE IF(Adjtc.eq.1)THEN
        CALL mkPOneLine(Mt1,'@','TC outliers removed')
       END IF
c-----------------------------------------------------------------------
      ELSE IF((Kpart.eq.4.or.Kpart.eq.-1).and.Ktabl.eq.11)THEN
       IF(Itype.eq.2)THEN
        IF(Iyrt.eq.1)THEN
         CALL mkPOneLine(Mt1,'@','Denton method used.')
        ELSE IF(Iyrt.eq.2)THEN
         WRITE(Mt1,1131)Lamda,Rol
 1131    FORMAT('<p>Regression method used, with lambda = ',f10.7,
     &          ', rho = ',f10.7,'.</p>')
        END IF
       END IF
       IF(Nustad.gt.0)THEN
        CALL mkPOneLine(Mt1,'@','Temporary prior adjustments included.')
       END IF
c-----------------------------------------------------------------------
      ELSE IF(Kpart.eq.7)THEN
       IF(Rvper)THEN
        CALL mkPOneLine(Mt1,'@','Type of revision: Percent')
       ELSE
        CALL mkPOneLine(Mt1,'@','Type of revision: Difference')
       END IF
       IF(Ktabl.eq.1.and.(Lrndsa.or.Iyrt.gt.0))THEN
        IF(Lrndsa.and.Iyrt.gt.0)THEN
         WRITE(Mt1,1140)'Rounded s','with revised yearly totals u'
        ELSE IF(Lrndsa)THEN
         WRITE(Mt1,1140)'Rounded s','u'
        ELSE IF(Iyrt.gt.0)THEN
         WRITE(Mt1,1140)'s','with revised yearly totals u'
        END IF
c-----------------------------------------------------------------------
       ELSE IF(Ktabl.eq.9.and.(Lrndsa.or.Iyrt.gt.0))THEN
        IF(Lrndsa.and.Iyrt.gt.0)THEN
         WRITE(Mt1,1140)'Rounded indirect s',
     &                  'with revised yearly totals u'
        ELSE IF(Lrndsa)THEN
         WRITE(Mt1,1140)'Rounded indirect s','u'
        ELSE IF(Iyrt.gt.0)THEN
         WRITE(Mt1,1140)'Indirect s','with revised yearly totals u'
        END IF
       END IF
      ELSE IF(Kpart.eq.6.and.Ktabl.eq.1)THEN
       n=Mcd
       IF(n.gt.6)n=6
       m=2-n+n/2*2
       WRITE(Mt1,1180)n,m
 1180  FORMAT('<p>MCD filter         ',i1,'&nbsp;x&nbsp;',i1,
     &        ' moving average</p>')
      END IF
 1140 FORMAT('<p>',a,'easonally adjusted series ',a,
     &       'sed in this analysis.</p>')
c-----------------------------------------------------------------------
      RETURN
      END
