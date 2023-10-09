C     Last change:  BCM   1 Oct 1998   10:45 am
**==prtmsr.f    processed by SPAG 4.03F  at 10:40 on 20 Oct 1994
      SUBROUTINE prtmsr(Msr,Revspn,Ny,Nptr)
c-----------------------------------------------------------------------
c     Print and/or save a table of the percent revision, concurrent
c     and final value of seasonally adjusted series, seasonal factors,
c     or month to month changes.
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'rev.prm'
      INCLUDE 'rev.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'tfmts.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'cchars.i'
c-----------------------------------------------------------------------
*      INCLUDE 'tfmts.prm'
      INCLUDE 'tbltitle.prm'
      INCLUDE 'desadj.prm'
c-----------------------------------------------------------------------
      LOGICAL F,T
      INTEGER MO,YR
      PARAMETER(MO=2,YR=1,F=.false.,T=.true.)
c-----------------------------------------------------------------------
      CHARACTER cblank*22,dash*132,filter*3,tfmt*110,fbase*110,fsum*5,
     &          outstr*50,tblttl*(PTTLEN),fobs*5,amonth*9,cyear*(4)
      INTEGER i,j,k,Revspn,Ny,fh,Msr,Nptr,numrev,ik,ipos,idate,rdbdat,
     &        ndash,ntbttl,ifmt,npos,nmonth
      LOGICAL locok
      DIMENSION Msr(PREV),Revspn(2),filter(3),idate(2),amonth(12),
     &          nmonth(12)
c----------------------------------------------------------------------- 
      DATA amonth/'January  ','February ','March    ','April    ',
     &            '@        ','June     ','July     ','August   ',
     &            'September','October  ','November ','December '/
      DATA nmonth/7,8,5,5,1,4,4,6,9,7,8,8/
      DATA filter/'3x3','3x5','3x9'/
c-----------------------------------------------------------------------
*      INCLUDE 'tfmts.var'
      INCLUDE 'desadj.var'
c-----------------------------------------------------------------------
      IF(.not.(Prttab(Nptr).or.Savtab(Nptr)))RETURN
c-----------------------------------------------------------------------
c     Initialize variables
c-----------------------------------------------------------------------
      CALL setchr(' ',22,cblank)
      CALL setchr('-',132,dash)
      dash(1:1)=' '
c-----------------------------------------------------------------------
c     If seasonal filters are to be printed, print out title.
c-----------------------------------------------------------------------
      IF(Prttab(Nptr))THEN
       numrev=Endrev-Begrev
c-----------------------------------------------------------------------
c     Print out title, table header
c-----------------------------------------------------------------------
       CALL genSkip(Nptr)
       CALL makttl(DSADIC,dsaptr,PDSA,Nptr,PDSUM4,tblttl,ntbttl,T,F)
       IF(.not.Lfatal)CALL prtshd(tblttl(1:ntbttl),Revspn,Ny,numrev)
       IF(Lfatal)RETURN
       CALL mkTableTag(Mt1,'@',tblttl(6:ntbttl))
       CALL mkCaption(Mt1,'Table '//tblttl(1:5))
c-----------------------------------------------------------------------
       CALL writTag(Mt1,'<tr>')
       CALL mkTableCell(Mt1,'head','&nbsp;')
       IF(Ny.eq.12)THEN
        do i = 2,Ny+1
         CALL mkHeaderCellScope(Mt1,0,0,'col',amonth(i)(1:nmonth(i)),
     &                          Colhdr(i))
        end do
       ELSE IF (Ny.eq.4)THEN
        do i = 2,Ny+1
         CALL mkHeaderCellScope(Mt1,0,0,'col','@',Colhdr(i)//' Quarter')
        end do
       END IF
       CALL writTag(Mt1,'</tr>')
c-----------------------------------------------------------------------
c     If you are only printing out one period (ie, calendar month) in a
c     year, print only those observation now by looping all observations
c     for that period.
c-----------------------------------------------------------------------
       j=Revspn(YR)-1
c-----------------------------------------------------------------------
c     If you are printing out an entire year's observations, set the
c     pointer for the first observation and loop until you get to the
c     end.
c-----------------------------------------------------------------------
       Revptr=1
       DO WHILE (Revptr.le.numrev)
        j=j+1
c-----------------------------------------------------------------------
c     Print out choices for seasonal filter.
c     If this is then first year, check what the date of the first 
c     observation tested is and adjust pointer for December value to 
c     allow for spaces if first observation not January/1st Quarter.
c-----------------------------------------------------------------------
        IF((Revptr.eq.1).and.(Revspn(MO).gt.1))THEN
         k=Ny-Revspn(MO)+1
         CALL writTag(Mt1,'<tr>')
         ipos=1
         CALL itoc(j,cyear,ipos)
         CALL mkHeaderCellScope(Mt1,0,0,'row','@',cyear(1:(ipos-1)))
         DO ik=1,Ny,k
          CALL mkTableCell(Mt1,'@','&nbsp;') 
         END DO
         DO i=Revptr,k
          CALL mkTableCell(Mt1,'@',filter(Msr(i))) 
         END DO
         CALL writTag(Mt1,'</tr>')
        ELSE
         k=Revptr+Ny-1
         CALL writTag(Mt1,'<tr>')
         ipos=1
         CALL itoc(j,cyear,ipos)
         CALL mkHeaderCellScope(Mt1,0,0,'row','@',cyear(1:(ipos-1)))
         DO i=Revptr,k
          IF(i.le.numrev)THEN
           CALL mkTableCell(Mt1,'@',filter(Msr(i)))
          ELSE
           CALL mkTableCell(Mt1,'@','&nbsp;') 
          END IF
         END DO
         CALL writTag(Mt1,'</tr>')
        END IF
c-----------------------------------------------------------------------
        Revptr=k+1
       END DO
       CALL writTag(Mt1,'</table>')
       CALL mkPOneLine(Mt1,'@','&nbsp;')
      END IF
c-----------------------------------------------------------------------
c     If percent revisions are to be saved, open file for saved
c     revisions.
c-----------------------------------------------------------------------
      IF(Savtab(Nptr))THEN
       CALL opnfil(T,F,Nptr,fh,locok)
       IF(.not.locok)THEN
        CALL abend
        RETURN
       END IF
c-----------------------------------------------------------------------
c     Print header for revisions
c-----------------------------------------------------------------------
       WRITE(fh,1020)'date',TABCHR,'sf'
       WRITE(fh,1020)'----',TABCHR,'---'
c-----------------------------------------------------------------------
c     begin looping though observations
c-----------------------------------------------------------------------
       DO i=Begrev,Endrev-1
        Revptr=i-Begrev+1
c-----------------------------------------------------------------------
c     Set date of revision for observation Revptr
c-----------------------------------------------------------------------
        CALL addate(Revspn,Ny,Revptr-1,idate)
        rdbdat=100*idate(YR)+idate(MO)
c-----------------------------------------------------------------------
c     Save revision measure with date
c-----------------------------------------------------------------------
        ipos=1
        CALL itoc(rdbdat,outstr,ipos)
        IF(Lfatal)RETURN
        outstr(ipos:ipos)=TABCHR
        ipos=ipos+1
        outstr(ipos:ipos+2)=filter(Msr(Revptr))
        WRITE(fh,1020)outstr(1:ipos+2)
       END DO
       CALL fclose(fh)
      END IF
c-----------------------------------------------------------------------
      RETURN
c-----------------------------------------------------------------------
 1020 FORMAT(a:,a,a:,a,a)
      END
