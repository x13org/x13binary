C     Last change:  BCM  16 Feb 1999   11:16 am
**==histx.f    processed by SPAG 4.03F  at 14:08 on 24 Aug 1994
      SUBROUTINE histx(Y,Nobs,Muladd,Ny,Lyr,Begsrs,Itbl,Ldiff,Lprt,Lsav,
     &                 Label,Nlbl)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Calculates the histogram of the ny long revisions vector, y.
c-----------------------------------------------------------------------
c bin     i  Local vector of counts of observations between cut points
c             i.e. bins
c cutpnt  d  Local vector of cut points where observations counted in
c             bin(i) are cutpnt(i-1) < y <= cutpnt(i)
c d       i  Input diffence between the number of observations in the
c             series and number of effective observations in the series,
c             nobs-nefobs
c i       i  Local do loop index
c ibin    i  Local index for the current bin, or column of the
c             histogram
c irow    i  Local index for the current row of the output
c lowbnd  d  Local scalar for the low bound of the observations
c nbin    i  Local number of bins or columns in the histogram
c notlr   i  Local number of outliers (in otlr)
c nobs    i  Input number of observations
c nrow    i  Local number of rows in the histogram
c otlr    i  Local notlr long list of the values of residuals
c             greater than 3.25
c otlrt0  i  Local notlr long list of residuals greater than 3.25
c             standard deviations from the median
c xscale  d  Local scale factor to make the number of rows be 40
c stddev  d  Local standard deviation of the y's
c sum     d  Local scalar sum of y
c sumsq   d  Local scalar sum of squares of y
c tmp     d  Local temporary scalar
c width   d  Local width between cut points
c y       d  Input nobs long vector of revisions
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'ssap.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'tfmts.cmn'
      INCLUDE 'htmlout.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION ONE
      PARAMETER(ONE=1D0)
      INTEGER MO,YR
      PARAMETER(MO=2,YR=1)
      LOGICAL T
      PARAMETER(T=.true.)
c-----------------------------------------------------------------------
      LOGICAL Lprt,Lsav,Ldiff
      CHARACTER Label*(41),dash*50,ex*(2)
      INTEGER bin,xbin,i,ibin,irow,nbin,Nobs,notlr,nrow,otlrt0,Muladd,
     &        otlobs,Ny,Lyr,Begsrs,iobs,Itbl,i2,ic,Nlbl
      DOUBLE PRECISION cutpnt,lowbnd,otlr,stddev,tmp,width,Y,ts,tsxtra,
     &                 temp,ceilng,xscale
      DIMENSION bin(15),xbin(15),cutpnt(15),otlr(50),otlrt0(50),ts(5),
     &          Y(*),otlobs(2),temp(PLEN),ex(2*NEST)
      EXTERNAL ceilng
c-----------------------------------------------------------------------
      DATA dash/'  ------------------------------------------------'/
      DATA ex/'a ','ai','b ','bi','c ','ci','d ','di','e ','ei'/
c-----------------------------------------------------------------------
      CALL copy(Y,Nobs,1,temp)
      ic=(Itbl-1)/2+1
      CALL hinge(temp,Nobs,ts,tsxtra,ic)
c-----------------------------------------------------------------------
c     Find standard deviation of the observations.
c-----------------------------------------------------------------------
      CALL medabs(Y,Nobs,stddev)
      IF(Lfatal)RETURN
      stddev=stddev/0.6745D0
c      IF(stddev.eq.0)RETURN
c     ------------------------------------------------------------------
c     Print Summary statistics
c     ------------------------------------------------------------------
      IF(Lprt)THEN
       CALL mkTableTag(Mt1,'w50','@')
       CALL mkCaption(Mt1,' Summary Statistics for the '//Label(1:Nlbl))
       IF(Ldiff)THEN
        CALL writTag(Mt1,'<tr>')
        CALL mkTableCell(Mt1,'head','&nbsp;')
        CALL mkHeaderCellScope(Mt1,0,0,'col','@',
     &                         'Maximum Absolute'//Cbr//'Differences')
        CALL writTag(Mt1,'</tr>')
        DO i=1,5
         CALL writTag(Mt1,'<tr>')
         IF(i.eq.1)THEN
          CALL mkHeaderCellScope(Mt1,0,0,'row','@','Minimum')
         ELSE IF(i.eq.2)THEN
          CALL mkHeaderCellScope(Mt1,0,0,'row','@','25th Percentile')
         ELSE IF(i.eq.3)THEN
          CALL mkHeaderCellScope(Mt1,0,0,'row','@','Median')
         ELSE IF(i.eq.4)THEN
          CALL mkHeaderCellScope(Mt1,0,0,'row','@','75th Percentile')
         ELSE
          CALL mkHeaderCellScope(Mt1,0,0,'row','@','Maximum')
         END IF
         IF(Tblwid.gt.7)THEN
          WRITE(Mt1,9010)ts(i)
         ELSE
          WRITE(Mt1,9020)ts(i)
         END IF
         CALL writTag(Mt1,'</tr>')
        END DO
        CALL writTag(Mt1,'<tr>')
        CALL mkHeaderCellScope(Mt1,0,0,'row','@','Standard Deviation')
        IF(Tblwid.gt.7)THEN
         WRITE(Mt1,9010)stddev
        ELSE
         WRITE(Mt1,9020)stddev
        END IF
        CALL writTag(Mt1,'</tr>')
        CALL writTag(Mt1,'</table>')
       ELSE
c     ------------------------------------------------------------------
c     Print hinge values for sliding spans analysis.
c     ------------------------------------------------------------------
        CALL writTag(Mt1,'<tr>')
        CALL mkTableCell(Mt1,'head','&nbsp;')
        CALL mkHeaderCellScope(Mt1,0,0,'col','@',
     &                         'Maximum Percent'//Cbr//'Differences')
        CALL mkTableCell(Mt1,'head','&nbsp;')
        CALL writTag(Mt1,'</tr>')
c     ------------------------------------------------------------------
        DO i=1,3
         CALL writTag(Mt1,'<tr>')
         IF(i.eq.1)THEN
          CALL mkHeaderCellScope(Mt1,0,0,'row','@','Minimum')
         ELSE IF(i.eq.2)THEN
          CALL mkHeaderCellScope(Mt1,0,0,'row','@','25th Percentile')
         ELSE IF(i.eq.3)THEN
          CALL mkHeaderCellScope(Mt1,0,0,'row','@','Median')
         END IF
         WRITE(Mt1,9030)ts(i)
         CALL mkTableCell(Mt1,'@','&nbsp;')
         CALL writTag(Mt1,'</tr>')
        END DO
c     ------------------------------------------------------------------
        IF(ic.le.3)THEN
         CALL writTag(Mt1,'<tr>')
         CALL mkHeaderCellScope(Mt1,0,0,'row','@','75th Percentile')
         WRITE(Mt1,9030)ts(4)
         CALL mkTableCell(Mt1,'@','&nbsp;')
         CALL writTag(Mt1,'</tr>')
         CALL writTag(Mt1,'<tr>')
         CALL mkHeaderCellScope(Mt1,0,0,'row','@','85th Percentile')
         WRITE(Mt1,9030)tsxtra
         CALL mkTableCell(Mt1,'@','&lt;&nbsp;')
         CALL writTag(Mt1,'</tr>')
c     ------------------------------------------------------------------
        ELSE IF(ic.eq.4)THEN
         CALL writTag(Mt1,'<tr>')
         CALL mkHeaderCellScope(Mt1,0,0,'row','@','60th Percentile')
         WRITE(Mt1,9030)tsxtra
         CALL mkTableCell(Mt1,'@','&lt;&nbsp;')
         CALL writTag(Mt1,'</tr>')
         CALL writTag(Mt1,'<tr>')
         CALL mkHeaderCellScope(Mt1,0,0,'row','@','75th Percentile')
         WRITE(Mt1,9030)ts(3)
         CALL mkTableCell(Mt1,'@','&nbsp;')
         CALL writTag(Mt1,'</tr>')
c     ------------------------------------------------------------------
        ELSE
         CALL writTag(Mt1,'<tr>')
         CALL mkHeaderCellScope(Mt1,0,0,'row','@','75th Percentile')
         WRITE(Mt1,9030)ts(3)
         CALL mkTableCell(Mt1,'@','&nbsp;')
         CALL writTag(Mt1,'</tr>')
         CALL writTag(Mt1,'<tr>')
         CALL mkHeaderCellScope(Mt1,0,0,'row','@','90th Percentile')
         WRITE(Mt1,9030)tsxtra
         CALL mkTableCell(Mt1,'@','&lt;&nbsp;')
         CALL writTag(Mt1,'</tr>')
        END IF
c     ------------------------------------------------------------------
        CALL writTag(Mt1,'<tr>')
        CALL mkHeaderCellScope(Mt1,0,0,'row','@','Maximum')
        WRITE(Mt1,9030)ts(5)
        CALL mkTableCell(Mt1,'@','&nbsp;')
        CALL writTag(Mt1,'</tr>')
        CALL writTag(Mt1,'<tr>')
        CALL mkHeaderCellScope(Mt1,0,0,'row','@','Standard Deviation')
        WRITE(Mt1,9030)stddev
        CALL mkTableCell(Mt1,'@','&nbsp;')
        CALL writTag(Mt1,'</tr>')
        CALL writTag(Mt1,'</table>')
        CALL mkPOneLine(Mt1,'@','&nbsp;')
        CALL mkPOneLine(Mt1,'center',
     &        '&lt; - hinge value associated with default cutoff value')
       END IF
      END IF
c-----------------------------------------------------------------------
c     Find the range and lower bound of the histogram.
c-----------------------------------------------------------------------
      width=.25D0*stddev
      lowbnd=0
      IF(Ldiff)THEN
       nbin=15
      ELSE
       nbin=12
      END IF
c-----------------------------------------------------------------------
c     Calculate the cut points.
c-----------------------------------------------------------------------
      tmp=lowbnd
      DO i=1,nbin
       cutpnt(i)=tmp
       tmp=tmp+width
      END DO
c-----------------------------------------------------------------------
c     Sort the observations into bins.
c-----------------------------------------------------------------------
      nrow=0
      notlr=0
      CALL setint(0,nbin,bin)
c     ------------------------------------------------------------------
      DO i=1,Nobs
       tmp=Y(i)
c     ------------------------------------------------------------------
       IF(tmp.lt.lowbnd)THEN
        notlr=notlr+1
        IF (notlr.le.50) THEN
         otlrt0(notlr)=i
         otlr(notlr)=tmp
        END IF
        bin(1)=bin(1)+1
        nrow=max(nrow,bin(1))
c     ------------------------------------------------------------------
       ELSE
        DO ibin=2,nbin-1
         IF(tmp.lt.cutpnt(ibin))THEN
          bin(ibin)=bin(ibin)+1
          nrow=max(nrow,bin(ibin))
          GO TO 10
         END IF
        END DO
c     ------------------------------------------------------------------
        notlr=notlr+1
        IF(notlr.le.50)THEN
         otlrt0(notlr)=i
         otlr(notlr)=tmp
        END IF
        bin(nbin)=bin(nbin)+1
        nrow=max(nrow,bin(nbin))
       END IF
   10  CONTINUE
      END DO
c     ------------------------------------------------------------------
      xscale=ceilng(dble(nrow)/69D0)
      IF(xscale.gt.ONE)THEN
       DO ibin=1,nbin
        xbin(ibin)=int(bin(ibin)/xscale)
       END DO
      ELSE
       DO ibin=1,nbin
        xbin(ibin)=bin(ibin)
       END DO
      END IF
c-----------------------------------------------------------------------
c     Print the histogram sideways with negative values on top.
c-----------------------------------------------------------------------
      IF(Lprt)THEN
       CALL makeSkipLink(Mt1,Idxtab,
     &                   'ASCII plot of Histogram of the '//
     &                   Label(1:Nlbl),T)
       CALL writTagOneLine(Mt1,'h3','@',' Histogram of the '//
     &                     Label(1:Nlbl))
       CALL writTag(Mt1,'<pre>')
       IF(Ldiff)THEN
        IF(Tblwid.gt.5)THEN
         WRITE(Mt1,9040)
        ELSE
         WRITE(Mt1,9050)
        END IF
       ELSE
        WRITE(Mt1,9060)
       END IF
c     ------------------------------------------------------------------
       IF(bin(1).gt.0)THEN
        IF(Ldiff.and.Tblwid.gt.5)THEN
         WRITE(Mt1,9070)'             Outlier [',('#',i=1,xbin(1))
        ELSE
         WRITE(Mt1,9070)'   Outlier [',('#',i=1,xbin(1))
        END IF
        WRITE(Mt1,9000)
       END IF
c     ------------------------------------------------------------------
       DO irow=2,nbin-2,2
        IF(Muladd.eq.1)THEN
         IF(Ldiff.and.Tblwid.gt.5)THEN
          WRITE(Mt1,9080)(cutpnt(irow)+cutpnt(irow-1))/2,
     &                   ('#',i=1,xbin(irow))
          WRITE(Mt1,9090)'           |',('#',i=1,xbin(irow+1))
         ELSE
          WRITE(Mt1,9100)(cutpnt(irow)+cutpnt(irow-1))/2,
     &                   ('#',i=1,xbin(irow))
          WRITE(Mt1,9090)' |',('#',i=1,xbin(irow+1))
         END IF
        ELSE
         WRITE(Mt1,9110)(cutpnt(irow)+cutpnt(irow-1))/2,
     &                  ('#',i=1,xbin(irow))
         WRITE(Mt1,9090)' |',('#',i=1,xbin(irow+1))
        END IF
       END DO
       IF(Muladd.eq.1)THEN
        IF(Ldiff.and.Tblwid.gt.5)THEN
         WRITE(Mt1,9080)(cutpnt(irow)+cutpnt(irow-1))/2,
     &                  ('#',i=1,xbin(irow))
        ELSE
         WRITE(Mt1,9100)(cutpnt(nbin-1)+cutpnt(nbin-2))/2,
     &                  ('#',i=1,xbin(nbin-1))
        END IF
       ELSE
        WRITE(Mt1,9110)(cutpnt(nbin-1)+cutpnt(nbin-2))/2,
     &                 ('#',i=1,xbin(nbin-1))
       END IF
c     ------------------------------------------------------------------
       IF(bin(nbin).gt.0)THEN
        IF(Ldiff.and.Tblwid.gt.5)THEN
         WRITE(Mt1,9070)'             Outlier [',('#',i=1,xbin(1))
        ELSE
         WRITE(Mt1,9070)'   Outlier [',('#',i=1,xbin(1))
        END IF
       END IF
c     ------------------------------------------------------------------
       WRITE(Mt1,9120)int(xscale)
       CALL writTag(Mt1,'</pre>')
c     ------------------------------------------------------------------
       CALL writTagOneLine(Mt1,'h3','@',
     &                    ' Tabular Histogram of the '//Label(1:Nlbl))
       CALL mkTableTag(Mt1,'w50','@')
       CALL mkCaption(Mt1,'Tabular Histogram of the '//Label(1:Nlbl))
       CALL writTag(Mt1,'<tr>')
       CALL mkHeaderCellScope(Mt1,0,0,'col','@',
     &                        'Standard'//Cbr//'Deviations')
       CALL mkHeaderCellScope(Mt1,0,0,'col','@','Frequency')
       CALL writTag(Mt1,'</tr>')
       IF(bin(1).gt.0)THEN
        CALL writTag(Mt1,'<tr>')
        WRITE(Mt1,9130)'Outlier',xbin(1)
        CALL writTag(Mt1,'</tr>')
       END IF
       DO irow=2,nbin-1
        CALL writTag(Mt1,'<tr>')
        IF(Muladd.eq.1)THEN
         IF(Tblwid.gt.5)THEN
          WRITE(Mt1,9140)(cutpnt(irow)+cutpnt(irow-1))/2,
     &                   xbin(irow)
         ELSE
          WRITE(Mt1,9150)(cutpnt(irow)+cutpnt(irow-1))/2,
     &                   xbin(irow)
         END IF
        ELSE
         WRITE(Mt1,9160)(cutpnt(irow)+cutpnt(irow-1))/2,
     &                   xbin(irow) 
        END IF
        CALL writTag(Mt1,'</tr>')
       END DO
c     ------------------------------------------------------------------
       IF(bin(nbin).gt.0)THEN
        CALL writTag(Mt1,'<tr>')
        WRITE(Mt1,9130)'Outlier',xbin(nbin)
        CALL writTag(Mt1,'</tr>')
       END IF
       CALL writTag(Mt1,'</table>')
       CALL mkPOneLine(Mt1,'@','&nbsp;')
c     ------------------------------------------------------------------
       IF(notlr.gt.0)THEN
        CALL mkTableTag(Mt1,'w50','@')
        CALL mkCaption(Mt1,Label(1:Nlbl)//' considered to be outliers')
        CALL writTag(Mt1,'<tr>')
        CALL mkHeaderCellScope(Mt1,0,0,'col','@','Time')
        CALL mkHeaderCellScope(Mt1,0,0,'col','@',Label(1:Nlbl))
        CALL writTag(Mt1,'</tr>')
        DO i=1,MIN(notlr,50)
         CALL writTag(Mt1,'<tr>')
         iobs=otlrt0(i)+Begsrs-1
         otlobs(MO)=mod(iobs,Ny)
         IF(otlobs(MO).eq.0)otlobs(MO)=Ny
         otlobs(YR)=Lyr+(iobs-1)/Ny
         IF(Tblwid.gt.5)THEN
          WRITE(Mt1,9170)otlobs(MO),otlobs(YR),otlr(i)
         ELSE
          WRITE(Mt1,9180)otlobs(MO),otlobs(YR),otlr(i)
         END IF
         CALL writTag(Mt1,'</tr>')
        END DO
        CALL writTag(Mt1,'</table>')
        CALL mkPOneLine(Mt1,'@','&nbsp;')
        IF(notlr.gt.50)THEN
         CALL mkPClass(Mt1,'center')
         WRITE(Mt1,9190)notlr,Cbr
        END IF
       END IF
      END IF
c     ------------------------------------------------------------------
c     Save hinge values
c     ------------------------------------------------------------------
      IF(Lsav.and.Nform.gt.0)THEN
       i2=2-mod(Itbl,2)
       IF(Ldiff)THEN
        IF(ic.eq.4)THEN
         WRITE(Nform,9200)ex(Itbl)(1:i2),(ts(i),i=1,3),tsxtra,(ts(i),
     &                    i=4,5),stddev
        ELSE
         WRITE(Nform,9200)ex(Itbl)(1:i2),(ts(i),i=1,4),tsxtra,ts(5),
     &                    stddev
        END IF
       ELSE
        IF(ic.eq.4)THEN
         WRITE(Nform,9210)ex(Itbl)(1:i2),(ts(i),i=1,3),tsxtra,(ts(i),
     &                    i=4,5),stddev
        ELSE
         WRITE(Nform,9210)ex(Itbl)(1:i2),(ts(i),i=1,4),tsxtra,ts(5),
     &                    stddev
        END IF
       END IF
      END IF
c     ------------------------------------------------------------------
 9000 FORMAT(1x)
 9010 FORMAT('<td>',G17.10,'</td>')
 9020 FORMAT('<td>',f10.1,'</td>')
 9030 FORMAT('<td>',f10.2,'</td>')
 9040 FORMAT('  Absolute Differences      Frequency')
 9050 FORMAT(' Absolute',/,' Differences      Frequency')
 9060 FORMAT(' Percent',/,' Differences      Frequency')
 9070 FORMAT(/,A,69A1)
 9080 FORMAT(3x,G17.10,' +',69A1)
 9090 FORMAT(10x,a,69A1)
 9100 FORMAT(3x,f7.1,' +',69A1)
 9110 FORMAT(4x,f6.2,' +',69A1)
 9120 FORMAT(/,'  One ''#''=',i2,' observation[s]')
 9130 FORMAT('<td class="center">',a,'</td><td class="center">',i4,
     &       '</td>')
 9140 FORMAT('<td class="center">',G17.10,'</td><td class="center">',i4,
     &       '</td>')
 9150 FORMAT('<td class="center">',f7.1,'</td><td class="center">',i4,
     &       '</td>')
 9160 FORMAT('<td class="center">',f6.2,'</td><td class="center">',i4,
     &       '</td>')
 9170 FORMAT('<td>',i2,':',i4,'</td><td>',G17.10,'</td>')
 9180 FORMAT('<td>',i2,':',i4,'</td><td>',f8.2,'</td>')
 9190 FORMAT('  Number of observations considered to be outliers = ',i3,
     &       a,/,'  (only the first 50 were listed above)</p>')
 9200 FORMAT('s3.',a,'.hinge:',8(2x,G17.10))
 9210 FORMAT('s3.',a,'.hinge:',8(2x,f8.3))
c     ------------------------------------------------------------------
      RETURN
      END
