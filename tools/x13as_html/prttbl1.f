C     Last change:  BCM  19 Apr 2007   10:39 am
      SUBROUTINE prttbl1(Begdat,Sp,Y,Nobs,Srsttl,Nttl,Outdec,Nfor,
     &                   thisId)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c    Subroutine, prttbl, prints a table of monthly data.  What month
c the series begins in is adjusted for.
c-----------------------------------------------------------------------
c Parameters and include files
c Name  Type Description
c-----------------------------------------------------------------------
c one     d  Double precision 1
c pt5     d  Double precision .5
c zero    d  Double precision 0
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'hiddn.cmn'
c-----------------------------------------------------------------------
c Input Type Description
c-----------------------------------------------------------------------
c begdat  i  2 long array containing the year and period of the begining
c             date
c nobs    i  Number of observations to be printed
c sp      i  Seasonal period or sampling period
c srsttl  c  81 long character string for the input title of the series
c y       i  Nobs long vector of observations
c-----------------------------------------------------------------------
      CHARACTER Srsttl*(50),thisId*(*)
      INTEGER Begdat,Nobs,Sp,Nfor,Nttl
      DOUBLE PRECISION Y,yy
      DIMENSION Begdat(2),Y(*),yy(POBS)
c-----------------------------------------------------------------------
c Local Type Description
c-----------------------------------------------------------------------
c cmonth  c  Array of month abbreviations
c fmt1    c  String containing the format for the first year of data
c i       i  Index value
c ibeg    i  Index of begining observation on the current line
c iend    i  Index of last observation on the current line
c iyr     i  lndex for the year to be printed
c ncol    i  Number of columns in the printout
c-----------------------------------------------------------------------
      CHARACTER blnk*80,cmonth*3,amonth*9,cqtr*3,fmt1*120,fmt2*120,
     &          thisOb*30,valuhd*5,cperiod*2
      LOGICAL thisNeg
      INTEGER BTWNCL,blkwd,clwdth,i,ibeg,idate,idxwd,iend,irow,INCOL,
     &        istrt,itmp,j,mindec,MNSGFG,nblk,nblkln,nclprt,nclskp,ncol,
     &        ndec,nidxhd,nrows,nvalhd,Outdec,strtyr,ivec,mxtbl,nmonth,
     &        iyr,i2,istrtf,nend
      PARAMETER(BTWNCL=3,INCOL=2,MNSGFG=3)
      DIMENSION cmonth(12),cqtr(4),idate(2),ivec(1),nmonth(12),
     &          amonth(12),cperiod(13)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      DOUBLE PRECISION ceilng
      EXTERNAL dpeq,ceilng
c-----------------------------------------------------------------------
      DATA blnk/
     &'                                                                 
     &               '/
      DATA cmonth/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep',
     &     'Oct','Nov','Dec'/
      DATA amonth/'January  ','February ','March    ','April    ',
     &            '@        ','June     ','July     ','August   ',
     &            'September','October  ','November ','December '/
      DATA nmonth/7,8,5,5,1,4,4,6,9,7,8,8/
      DATA cqtr/'1st','2nd','3rd','4th'/
      DATA cperiod/' 1',' 2',' 3',' 4',' 5',' 6',' 7',' 8',' 9','10',
     &             '11','12','13'/
c-----------------------------------------------------------------------
c     Return if this is a transparent seasonal adjustment for sliding
c     spans, revisions, or X-11 Holiday adjustment.
c-----------------------------------------------------------------------
      IF(Lhiddn)RETURN
c-----------------------------------------------------------------------
c     Print the series title
c-----------------------------------------------------------------------
      IF(Nttl.gt.1)CALL writTagOneLine(Mt1,'h3','@',Srsttl(1:Nttl))
c-----------------------------------------------------------------------
c     Figure the column width and decimals
c-----------------------------------------------------------------------
      nend=Nobs+Nfor
      CALL numfmt(Y,nend,Outdec,clwdth,mindec)
      IF(mindec.gt.Outdec)THEN
       ndec=min(mindec+MNSGFG-1,11)
       clwdth=clwdth-Outdec+ndec+1
      ELSE
       ndec=Outdec
      END IF
      IF(ndec.eq.0)clwdth=clwdth+1
*      clwdth=min(max(clwdth,3),15)
c-----------------------------------------------------------------------
c     copy y into yy vector (BCM April 2007)
c-----------------------------------------------------------------------
      thisNeg=.false.
      DO i=1,nend
       yy(i) = Y(i)
c-----------------------------------------------------------------------
c     For cases where ndec = 0 and the decimal fraction is exactly .5,
c     make an adjustment to ensure the number will round properly
c     when printed (BCM April 2007)
c-----------------------------------------------------------------------
       IF(dpeq(yy(i)-ceilng(yy(i)-0.5D0),0.5D0).and.ndec.eq.0)
     &     yy(i)=yy(i)+0.01D0
       IF(yy(i).lt.0.and.(.not.thisNeg))thisNeg=.true.
      END DO
      IF(thisNeg)clwdth=clwdth+1
      IF(ndec.lt.10)THEN
       WRITE(fmt1,1000)clwdth,ndec
      ELSE
       WRITE(fmt1,1001)clwdth,ndec
      END IF
 1000 FORMAT('(f',i2.2,'.',i1,') ')
 1001 FORMAT('(f',i2.2,'.',i2,')')
c-----------------------------------------------------------------------
      strtyr=Begdat(YR)
*      IF(Sp.eq.1)strtyr=strtyr-1
*      ivec(1)=strtyr+Nobs
*      CALL intfmt(ivec,1,idxwd)
*      idxwd=max(2,idxwd)
*      IF(idxwd.gt.3)THEN
*       nidxhd=4
*       idxhd(1:nidxhd)='Year'
*      ELSE
*       nidxhd=2
*       idxhd(1:nidxhd)='Yr'
*      END IF
      if(Sp.eq.1)THEN
       istrt=1
       CALL mkTableTag(Mt1,'w40',Srsttl(1:Nttl))
       CALL mkCaption(Mt1,Srsttl(1:Nttl))
      else
       istrt=Begdat(MO)
       CALL mkTableTag(Mt1,'x11',Srsttl(1:Nttl))
       CALL mkCaption(Mt1,Srsttl(1:Nttl))
      end if
      CALL writTag(Mt1,'<thead>')
      CALL writTag(Mt1,'<tr>')
      CALL mkTableCell(Mt1,'head','&nbsp;')
c-----------------------------------------------------------------------
      IF(Sp.eq.12)THEN
       do i = 1,Sp
        CALL mkHeaderCellScope(Mt1,0,0,'col',amonth(i)(1:nmonth(i)),
     &                         cmonth(i))
       end do
      ELSE IF (Sp.eq.4)THEN
       do i = 1,Sp
        CALL mkHeaderCellScope(Mt1,0,0,'col','@',cqtr(i)//' Quarter')
       end do
      ELSE
       do i = 1,Sp
        CALL mkHeaderCellScope(Mt1,0,0,'col','@','Period '//cperiod(i))
       end do
      END IF
      CALL writTag(Mt1,'</tr>')
      CALL writTag(Mt1,'</thead>')
      CALL writTag(Mt1,'<tbody>')
c-----------------------------------------------------------------------
c    print out first year
c-----------------------------------------------------------------------
      CALL writTag(Mt1,'<tr>')
      write(Mt1,1010)strtyr
 1010 FORMAT('<th scope="row">',i4,'</th>')
      if (istrt.gt.1)THEN
       DO i=1,istrt-1
        CALL mkTableCell(Mt1,'@','&nbsp;')
       END DO
      END IF
      iend=min(Sp-Begdat(2)+1,12,Nobs)
      DO i=1,iend
       write(thisOb,fmt1)yy(i)
       IF(yy(i).lt.0D0)THEN
        CALL mkTableCell(Mt1,'nowrap',thisOb)
       ELSE
        CALL mkTableCell(Mt1,'@',thisOb)
       END IF
      END DO
      IF ((iend+istrt).lt.Sp)THEN
       DO i=iend+istrt+1,Sp
        CALL mkTableCell(Mt1,'@','&nbsp;')
       END DO
      END IF
      CALL writTag(Mt1,'</tr>')
      IF((istrt+Nobs-1).le.Sp)THEN
       CALL writTag(Mt1,'</table>')
       CALL mkPOneLine(Mt2,'@','&nbsp;')
       RETURN
      END IF
c-----------------------------------------------------------------------
c    Now print out the rest of the table
c-----------------------------------------------------------------------
      iyr=Begdat(YR)
      DO ibeg=iend+1,Nobs,Sp
       CALL writTag(Mt1,'<tr>')
       i2=ibeg+Sp-1
       iend=min(i2,Nobs)
       iyr=iyr+1
       write(Mt1,1010)iyr
       DO i=ibeg,iend
        write(thisOb,fmt1)yy(i)
        IF(yy(i).lt.0D0)THEN
         CALL mkTableCell(Mt1,'nowrap',thisOb)
        ELSE
         CALL mkTableCell(Mt1,'@',thisOb)
        END IF
       END DO
       IF(i2.le.iend)CALL writTag(Mt1,'</tr>')
      END DO
      IF (i2.gt.iend) THEN
       DO i=iend+1,i2
        CALL mkTableCell(Mt1,'@','&nbsp;')
       END DO
       CALL writTag(Mt1,'</tr>')
      END IF
      CALL writTag(Mt1,'</tbody>')
      CALL writTag(Mt1,'</table>')
      CALL mkPOneLine(Mt1,'@','&nbsp;')
c     ------------------------------------------------------------------
c     Now print out the forecasts
c     ------------------------------------------------------------------
      istrt=iend+1
      strtyr=iyr
      if (istrt.gt.i2) THEN
       strtyr=strtyr+1
       istrtf=1
       i2=i2+Sp
      ELSE
       istrtf=istrt-(i2-Sp)
      END IF
c     ------------------------------------------------------------------
      if(Sp.eq.1)THEN
       CALL mkTableTag(Mt1,'w40','Forecasts of '//Srsttl(1:Nttl))
       CALL mkCaption(Mt1,'Forecasts of '//Srsttl(1:Nttl))
      else
       CALL mkTableTag(Mt1,'x11',Srsttl(1:Nttl))
       CALL mkCaption(Mt1,'Forecasts of '//Srsttl(1:Nttl))
      end if
      CALL writTag(Mt1,'<thead>')
      CALL writTag(Mt1,'<tr>')
      CALL mkTableCell(Mt1,'head','&nbsp;')
c-----------------------------------------------------------------------
      IF(Sp.eq.12)THEN
       do i = 1,Sp
        CALL mkHeaderCellScope(Mt1,0,0,'col',amonth(i)(1:nmonth(i)),
     &                         cmonth(i))
       end do
      ELSE IF (Sp.eq.4)THEN
       do i = 1,Sp
        CALL mkHeaderCellScope(Mt1,0,0,'col','@',cqtr(i)//' Quarter')
       end do
      ELSE
       do i = 1,Sp
        CALL mkHeaderCellScope(Mt1,0,0,'col','@','Period '//cperiod(i))
       end do
      END IF
      CALL writTag(Mt1,'</tr>')
      CALL writTag(Mt1,'</thead>')
      CALL writTag(Mt1,'<tbody>')
c-----------------------------------------------------------------------
c    print out first year of forecasts
c-----------------------------------------------------------------------
      CALL writTag(Mt1,'<tr>')
      write(Mt1,1010)strtyr
      if (istrtf.gt.1)THEN
       DO i=1,istrtf-1
        CALL mkTableCell(Mt1,'@','&nbsp;')
       END DO
      END IF
      iend=min(iend+nfor,i2)
      DO i=istrt,iend
       write(thisOb,fmt1)yy(i)
       IF(yy(i).lt.0D0)THEN
        CALL mkTableCell(Mt1,'nowrap',thisOb)
       ELSE
        CALL mkTableCell(Mt1,'@',thisOb)
       END IF
      END DO
      IF (iend.lt.i2)THEN
       DO i=iend+1,i2
        CALL mkTableCell(Mt1,'@','&nbsp;')
       END DO
      END IF
      CALL writTag(Mt1,'</tr>')
c-----------------------------------------------------------------------
c    Now print out the rest of the table
c-----------------------------------------------------------------------
      iyr=strtyr
      DO ibeg=iend+1,nend,Sp
       CALL writTag(Mt1,'<tr>')
       i2=ibeg+Sp-1
       iend=min(i2,nend)
       iyr=iyr+1
       write(Mt1,1010)iyr
       DO i=ibeg,iend
        write(thisOb,fmt1)yy(i)
        IF(yy(i).lt.0D0)THEN
         CALL mkTableCell(Mt1,'nowrap',thisOb)
        ELSE
         CALL mkTableCell(Mt1,'@',thisOb)
        END IF
       END DO
       IF(i2.le.iend)CALL writTag(Mt1,'</tr>')
      END DO
      IF (i2.gt.iend) THEN
       DO i=iend+1,i2
        CALL mkTableCell(Mt1,'@','&nbsp;')
       END DO
       CALL writTag(Mt1,'</tr>')
      END IF
      CALL writTag(Mt1,'</tbody>')
      CALL writTag(Mt1,'</table>')
      CALL mkPOneLine(Mt1,'@','&nbsp;')
c     ------------------------------------------------------------------
      RETURN
      END
