C     Last change:  BCM  25 Nov 97   11:58 am
      SUBROUTINE hist(Y,Begspn,Sp,Nobs,D,Muladd)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     hist.f, Release 1, Subroutine Version 1.5, Modified 03 Nov 1994.
c-----------------------------------------------------------------------
c     Calculates the histogram of the ny long data vector, y.
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
c median    d  Local median of the y's
c nbin    i  Local number of bins or columns in the histogram
c notlr   i  Local number of outliers (in otlr)
c nobs    i  Input number of observations
c nrow    i  Local number of rows in the histogram
c otlr    i  Local notlr long list of the values of residuals
c             greater than 3.25
c otlrt0  i  Local notlr long list of residuals greater than 3.25
c             standard deviations from the median
c scale   d  Local scale factor to make the number of rows be 40
c stddev  d  Local standard deviation of the y's
c sum     d  Local scalar sum of y
c sumsq   d  Local scalar sum of squares of y
c tmp     d  Local temporary scalar
c width   d  Local width between cut points
c y       d  Input nobs long vector of observations
c ymax    d  Local scalar maximum of the y's
c ymin    d  Local scalar minimum of the y's
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'htmlout.cmn'
c     ------------------------------------------------------------------
      INTEGER PA,POTLEN
      DOUBLE PRECISION ONE
      PARAMETER(PA=PLEN+2*PORDER,ONE=1D0,POTLEN=PLEN/4)
c     ------------------------------------------------------------------
      CHARACTER str*(10)
      INTEGER Begspn,bin,D,i,ibin,idate,irow,midpt,nbin,nchr,Nobs,notlr,
     &        nrow,otlrt0,Sp,Muladd
      DOUBLE PRECISION cutpnt,lowbnd,median,otlr,srtdy,stddev,tmp,width,
     &                 Y,ymax,ymin,ceilng,scale
      DIMENSION Begspn(2),bin(15),cutpnt(15),idate(2),otlr(POTLEN),
     &          otlrt0(POTLEN),srtdy(PA),Y(Nobs)
      EXTERNAL ceilng
c-----------------------------------------------------------------------
c     Find the minimum, maximum, median, and standard deviation of the
c observations.
c-----------------------------------------------------------------------
      CALL copy(Y,Nobs,1,srtdy)
      CALL shlsrt(Nobs,srtdy)
c     ------------------------------------------------------------------
      ymin=srtdy(1)
      ymax=srtdy(Nobs)
c     ------------------------------------------------------------------
      midpt=Nobs/2
      IF(mod(Nobs,2).eq.0)THEN
       median=(srtdy(midpt)+srtdy(midpt+1))/2D0
      ELSE
       median=srtdy(midpt+1)
      END IF
c     ------------------------------------------------------------------
      CALL medabs(Y,Nobs,stddev)
      IF(Lfatal)RETURN
      stddev=1.49D0*stddev
c-----------------------------------------------------------------------
c     Find the range and lower bound of the histogram.
c-----------------------------------------------------------------------
      width=.5D0
      lowbnd=-3.25D0
c-----------------------------------------------------------------------
c     Calculate the cut points.
c-----------------------------------------------------------------------
      tmp=lowbnd
      nbin=15
c     ------------------------------------------------------------------
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
       tmp=(Y(i)-median)/stddev
c     ------------------------------------------------------------------
       IF(tmp.lt.lowbnd)THEN
        notlr=notlr+1
        IF(notlr.le.POTLEN)THEN
         otlrt0(notlr)=i+D
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
        IF(notlr.le.POTLEN)THEN
         otlrt0(notlr)=i+D
         otlr(notlr)=tmp
        END IF
        bin(nbin)=bin(nbin)+1
        nrow=max(nrow,bin(nbin))
       END IF
   10  CONTINUE
      END DO
c     ------------------------------------------------------------------
      scale=ceilng(dble(nrow)/69D0)
      IF(scale.gt.ONE)THEN
       DO ibin=1,nbin
        bin(ibin)=int(bin(ibin)/scale)
       END DO
      END IF
c-----------------------------------------------------------------------
c     Print the histogram sideways with negative values on top.
c-----------------------------------------------------------------------
      CALL writTag(Mt1,'<pre>')
      WRITE(Mt1,1010)
 1010 FORMAT('  Standard',/,'  Deviations     Frequency')
c     ------------------------------------------------------------------
      IF(bin(1).gt.0)THEN
       WRITE(Mt1,1020)('#',i=1,bin(1))
 1020  FORMAT(/,'  Outlier [',69A1)
       WRITE(Mt1,'(1x)')
      END IF
c     ------------------------------------------------------------------
      DO irow=2,nbin-2,2
       WRITE(Mt1,1030)(irow-8)/2,('#',i=1,bin(irow))
 1030  FORMAT(i9,' +',69A1)
       WRITE(Mt1,1040)('#',i=1,bin(irow+1))
 1040  FORMAT(9x,' |',69A1)
      END DO
      WRITE(Mt1,1030)(nbin-9)/2,('#',i=1,bin(nbin-1))
c     ------------------------------------------------------------------
      IF(bin(nbin).gt.0)WRITE(Mt1,1020)('#',i=1,bin(nbin))
c     ------------------------------------------------------------------
      WRITE(Mt1,1050)int(scale)
 1050 FORMAT(/,'  One ''#''=',i2,' observation[s]')
      CALL writTag(Mt1,'</pre>')
c     ------------------------------------------------------------------
      CALL genSkip(1034)
      CALL writTagOneLine(Mt1,'h3','@',
     &                    'Tabular Histogram of the Standardized and '//
     &                    'Mean-Centered Residuals')
      CALL mkTableTag(Mt1,'w50','@')
      CALL mkCaption(Mt1,'Tabular Histogram of the Standardized and '//
     &               'Mean-Centered Residuals')
      CALL writTag(Mt1,'<tr>')
      CALL mkHeaderCellScope(Mt1,0,0,'col','@',
     &                       'Standard'//Cbr//'Deviations')
      CALL mkHeaderCellScope(Mt1,0,0,'col','@','Frequency')
      CALL writTag(Mt1,'</tr>')
      IF(bin(1).gt.0)THEN
       CALL writTag(Mt1,'<tr>')
       WRITE(Mt1,1041)'Outlier',bin(1)
 1041  FORMAT('<td class="center">',a,'</td><td class="center">',i4,
     &        '</td>')
       CALL writTag(Mt1,'</tr>')
      END IF
      DO irow=2,nbin-1
       CALL writTag(Mt1,'<tr>')
       IF(Muladd.eq.1)THEN
        WRITE(Mt1,1051)(cutpnt(irow)+cutpnt(irow-1))/2,
     &                  bin(irow)
 1051   FORMAT('<td class="center">',f7.1,'</td><td class="center">',
     &         i4,'</td>')
       ELSE
        WRITE(Mt1,1061)(cutpnt(irow)+cutpnt(irow-1))/2,
     &                  bin(irow) 
 1061   FORMAT('<td class="center">',f6.2,'</td><td class="center">',
     &         i4,'</td>')
       END IF
       CALL writTag(Mt1,'</tr>')
      END DO
c     ------------------------------------------------------------------
      IF(bin(nbin).gt.0)THEN
       CALL writTag(Mt1,'<tr>')
       WRITE(Mt1,1041)'Outlier',bin(nbin)
       CALL writTag(Mt1,'</tr>')
      END IF
      CALL writTag(Mt1,'</table>')
      CALL mkPOneLine(Mt1,'@','&nbsp;')
c     ------------------------------------------------------------------
      IF(notlr.gt.0)THEN
*       CALL mkPOneLine(Mt1,'@','Residuals with |t|>3.25')
       IF(notlr.gt.POTLEN)THEN
        WRITE(Mt1,1090)POTLEN,notlr
 1090   FORMAT(/,'<p class="center">Only the first ',i3,' of the ',i3,
     &           ' extreme residuals are shown.</p>',/)
       END IF
       CALL mkTableTag(Mt1,'w50','@')
       CALL mkCaption(Mt1,'Residuals with |t|>3.25')
       CALL writTag(Mt1,'<tr>')
       CALL mkTableCell(Mt1,'head','&nbsp;')
       CALL mkHeaderCellScope(Mt1,0,0,'col','@','t-value')
       CALL writTag(Mt1,'</tr>')
       DO i=1,notlr
        IF(i.le.POTLEN)THEN
         CALL addate(Begspn,Sp,otlrt0(i)-(D+1),idate)
         CALL wrtdat(idate,Sp,str,nchr)
         IF(Lfatal)RETURN
         CALL writTag(Mt1,'<tr>')
         WRITE(Mt1,1070)str(1:nchr),otlr(i)
         CALL writTag(Mt1,'</tr>')
 1070    FORMAT('<th scope="row">',a,'</th><td>',f8.2,'</td>')
        END IF
       END DO
       CALL writTag(Mt1,'</table>')
       CALL mkPOneLine(Mt1,'@','&nbsp;')
      END IF
c     ------------------------------------------------------------------
      CALL mkTableTag(Mt1,'w50','@')
      CALL mkCaption(Mt1,
     &            'Summary Statistics for the Unstandardized Residuals')
      WRITE(Mt1,1080)ymin,ymax,median,stddev
 1080 FORMAT('<tr><td class="head">Minimum</td><td>',f15.3,'</td></tr>',
     &     /,'<tr><td class="head">Maximum</td><td>',f15.3,'</td></tr>',
     &     /,'<tr><td class="head">Median </td><td>',f15.3,'</td></tr>',
     &     /,'<tr><td class="head">Robust Standard Deviation</td><td>',
     &       f15.3,'</td></tr>')
      CALL writTag(Mt1,'</table>')
      CALL mkPOneLine(Mt1,'@','&nbsp;')
c     ------------------------------------------------------------------
      RETURN
      END
