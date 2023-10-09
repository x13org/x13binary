C     Last change:  BCM  16 Feb 1999    3:50 pm
      SUBROUTINE prtrev(Fin,Cnc,Revspn,Tbltyp,Nptr,Ntargt,Vtargt,Sp,
     &                  Lsumm,Lgraf,Lr1y2y)
c-----------------------------------------------------------------------
c     Print and/or save a table of the percent revision, concurrent
c     and final value of seasonally adjusted series, seasonal factors,
c     trend component or month to month changes.
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'rev.prm'
      INCLUDE 'rev.cmn'
      INCLUDE 'tfmts.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'x11msc.cmn'
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'dgnsvl.i'
      INCLUDE 'units.cmn'
      INCLUDE 'cchars.i'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      INCLUDE 'tbltitle.prm'
      INCLUDE 'desdgn.prm'
c-----------------------------------------------------------------------
      LOGICAL F,T
      INTEGER MO,YR,PCOL,PCOLRV,PC1
      DOUBLE PRECISION PCT,ONE,ZERO
      PARAMETER(MO=2,YR=1,F=.false.,T=.true.,PCOLRV=23,PCT=100D0,
     &          ONE=1D0,ZERO=0D0,PCOL=PTARGT+1,PC1=PCOL+1)
c-----------------------------------------------------------------------
      CHARACTER cobs*(13),cpobs*(4),tblttl*(PTTLEN),revfmt*(40),
     &          hdrttl*(PCOLRV*PC1),hd2ttl*(PCOLRV*PC1),
     &          outstr*(6+(23*PC1)),revlbl*(30),laglbl*(5),brklbl*(7),
     &          tblcls*(3),cm2*(8),hvec*(15),revstr*(30),str*(10)
      DOUBLE PRECISION Cnc,Fin,rev,tmp,trev,aarpd,aaryr,aartot,narpd,
     &                 naryr,nartot,fin1yr,ts,revhng,drv,xtmp
      INTEGER i,j,Revspn,fh,fh2,Tbltyp,Nptr,k,k2,i3,ielt,ipos,lstrev,
     &        idate,rdbdat,ntbttl,ncol,i1,i2,i0,iper,hdrptr,hd2ptr,
     &        nhdr,nhd2,iyr,nstr,rnum,tbw,end2,k3,nlbl,Vtargt,Ntargt,
     &        nhdrtl,nhd2tl,Lsumm,j1,j2,nrvstr,ndtchr,Sp,rwid
      LOGICAL Lgraf,locok,Lr1y2y
      DIMENSION Cnc(PREV),Fin(0:PTARGT,PREV),rev(0:PCOL,PREV),Revspn(2),
     &          trev(PREV+12),idate(2),tmp(0:2,PREV),ts(5),cpobs(16),
     &          Vtargt(PTARGT),aarpd(0:PCOL,12),aaryr(0:PCOL,PREVY),
     &          aartot(0:PCOL,1),narpd(0:PCOL,12),naryr(0:PCOL,PREVY),
     &          nartot(0:PCOL,1),hdrptr(0:PC1),hd2ptr(0:PC1),
     &          revhng(0:PCOL,5),tblcls(0:PTARGT),cm2(16),hvec(5)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      INCLUDE 'desdgn.var'
c-----------------------------------------------------------------------
      DATA cpobs /'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec','1st','2nd','3rd','4th'/
      DATA cm2   /'uary    ','ruary   ','ch      ','il      ',
     &            '        ','e       ','y       ','ust     ',
     &            'tember  ','ober    ','ember   ','ember   ',
     &            ' Quarter',' Quarter',' Quarter',' Quarter'/
      DATA hvec  /'Minimum        ','25th Percentile','Median         ',
     &            '75th Percentile','Maximum        ' /
      DATA tblcls/'w30','w40','w50','w60','w70','w80'/
c-----------------------------------------------------------------------
c     Set logical variable to determine if percent revisions are to be 
c     printed out
c-----------------------------------------------------------------------
      locok=T
      Rvper=T
      IF(Tbltyp.eq.2.or.Tbltyp.eq.5)THEN
       Rvper=F
      ELSE IF(Muladd.eq.1)THEN
       IF(Tbltyp.eq.6.or.Rvdiff.eq.1)THEN
        Rvper=F
       ELSE
        i=Begrev
        DO WHILE(i.le.Endtbl.and.Rvper)
         IF(Cnc(i-Begrev+1).le.0)Rvper=F
         i=i+1
        END DO
       END IF
      END IF
c-----------------------------------------------------------------------
      IF((.not.(Tbltyp.eq.2.or.Tbltyp.eq.5)).and.Lsumm.gt.0)THEN
       IF(Rvper)THEN
        WRITE(Nform,1010)Tbltyp,'percent'
       ELSE
        WRITE(Nform,1010)Tbltyp,'difference'
       END IF
 1010  FORMAT('r',i2.2,'.aarmode: ',a)
      END IF
c-----------------------------------------------------------------------
c     Initialize values for printing tables
c-----------------------------------------------------------------------
      ncol=Ntargt
      IF(Lr1y2y)ncol=ncol+1
c-----------------------------------------------------------------------
c     Generate table format
c-----------------------------------------------------------------------
      IF(Prttab(Nptr).or.Prttab(Nptr+1).or.Svltab(LSLASA+Tbltyp-1))THEN
       rwid=10
       IF(Tblwid.gt.rwid)rwid=Tblwid
       WRITE(revfmt,1020)rwid,2
 1020  FORMAT('(''<td class="center">'',f',i2,'.',i1,',''</td>'')')
      END IF
c-----------------------------------------------------------------------
c     create column headers
c-----------------------------------------------------------------------
      IF(Prttab(Nptr).or.Prttab(Nptr+1))THEN
       CALL intlst(PCOL+1,hdrptr,nhdrtl)
       nhdr=nhdrtl+1
       DO i2=ncol,0,-1
        IF(i2.eq.ncol.and.Lr1y2y)THEN
         ipos=1
         CALL itoc(Ny,cobs,ipos) 
         CALL insstr(cobs(1:(ipos-1))//' later-',nhdr,PC1,hdrttl,
     &               hdrptr,nhdrtl)
        ELSE
         IF(Cnctar.or.i2.eq.0)THEN
          CALL insstr('Concurrent -',nhdr,PC1,hdrttl,hdrptr,nhdrtl)
         ELSE
          ipos=1
          CALL itoc(Vtargt(i2),cobs,ipos) 
          CALL insstr(cobs(1:(ipos-1))//' later-',nhdr,PC1,hdrttl,
     &                hdrptr,nhdrtl)
         END IF
        END IF
        IF(Lfatal)RETURN
       END DO
       CALL intlst(PC1,hd2ptr,nhd2tl)
       nhd2=nhd2tl+1
       DO i2=ncol,0,-1
        IF(i2.eq.ncol.and.Lr1y2y)THEN
         ipos=1
         CALL itoc(2*Ny,cobs,ipos) 
         CALL insstr(cobs(1:(ipos-1))//' later ',nhd2,PC1,hd2ttl,
     &               hd2ptr,nhd2tl)
        ELSE
         IF((.not.Cnctar).or.i2.eq.0)THEN
          CALL insstr('Final ',nhd2,PC1,hd2ttl,hd2ptr,nhd2tl)
         ELSE
          ipos=1
          CALL itoc(Vtargt(i2),cobs,ipos) 
          CALL insstr(cobs(1:(ipos-1))//' later ',nhd2,PC1,hd2ttl,
     &                hd2ptr,nhd2tl)
         END IF
        END IF
        IF(Lfatal)RETURN
       END DO
      END IF
c-----------------------------------------------------------------------
c     Intitalize variables for summary tables
c-----------------------------------------------------------------------
      iper=Revspn(MO)-1
      iyr=1
      CALL setdp(0D0,PC1*12,aarpd)
      CALL setdp(0D0,PC1*PREVY,aaryr)
      CALL setdp(0D0,PC1,aartot)
      CALL setdp(0D0,PC1*12,narpd)
      CALL setdp(0D0,PC1*PREVY,naryr)
      CALL setdp(0D0,PC1,nartot)
c-----------------------------------------------------------------------
c     Compute revision for given estimate
c-----------------------------------------------------------------------
      DO i=Begrev,Endtbl-1
       Revptr=i-Begrev+1
c-----------------------------------------------------------------------
c     Set indexes for summary tables
c-----------------------------------------------------------------------
       iper=iper+1
       IF(iper.gt.Ny)THEN
        iper=1
        iyr=iyr+1
       END IF
c-----------------------------------------------------------------------
c     Calculate the (percent) revision between the concurrent and final
c     adjustments.
c-----------------------------------------------------------------------
       DO i2=0,Ntargt
        lstrev=Endsa
        IF(i2.gt.0)lstrev=lstrev-Vtargt(i2)
        IF(Cnctar)lstrev=lstrev+1
        IF(i2.gt.0.and.i.ge.lstrev)THEN
         rev(i2,Revptr)=DNOTST
        ELSE
         IF(Cnctar.or.i2.eq.0)THEN
          rev(i2,Revptr)=Fin(i2,Revptr)-Cnc(Revptr)
          IF(Rvper)rev(i2,Revptr)=(rev(i2,Revptr)/Cnc(Revptr))*PCT
         ELSE
          rev(i2,Revptr)=Fin(0,Revptr)-Fin(i2,Revptr)
          IF(Rvper)rev(i2,Revptr)=(rev(i2,Revptr)/Fin(i2,Revptr))*PCT
         END IF
c-----------------------------------------------------------------------
c     Keep track of summary statistics
c-----------------------------------------------------------------------
         IF(Prttab(Nptr+1).or.Lsumm.gt.0)THEN
          drv=dabs(rev(i2,Revptr))
          aarpd(i2,iper)=aarpd(i2,iper)+drv
          narpd(i2,iper)=narpd(i2,iper)+ONE
          aaryr(i2,iyr)=aaryr(i2,iyr)+drv
          naryr(i2,iyr)=naryr(i2,iyr)+ONE
          aartot(i2,1)=aartot(i2,1)+drv
          nartot(i2,1)=nartot(i2,1)+ONE
         END IF
        END IF
c-----------------------------------------------------------------------
        IF(Lr1y2y.and.i2.gt.0)THEN
         IF(Vtargt(i2).eq.Ny)fin1yr=Fin(i2,Revptr)
         IF(Vtargt(i2).eq.2*Ny)THEN
c          IF(i.ge.Endsa-Vtargt(i2)+1)THEN
          IF(i.ge.Endsa-Vtargt(i2))THEN
           rev(ncol,Revptr)=DNOTST
          ELSE
           rev(ncol,Revptr)=Fin(i2,Revptr)-fin1yr
           IF(Rvper)rev(ncol,Revptr)=(rev(ncol,Revptr)/fin1yr)*PCT
c-----------------------------------------------------------------------
           IF(Prttab(Nptr+1).or.Svltab(LSLASA+Tbltyp-1).or.
     &        Lsumm.gt.0)THEN
            drv=dabs(rev(ncol,Revptr))
            aarpd(ncol,iper)=aarpd(ncol,iper)+drv
            narpd(ncol,iper)=narpd(ncol,iper)+ONE
            aaryr(ncol,iyr)=aaryr(ncol,iyr)+drv
            naryr(ncol,iyr)=naryr(ncol,iyr)+ONE
            aartot(ncol,1)=aartot(ncol,1)+drv
            nartot(ncol,1)=nartot(ncol,1)+ONE
           END IF
c-----------------------------------------------------------------------
          END IF
         END IF
        END IF
       END DO
      END DO
      IF(Prttab(Nptr))THEN
c-----------------------------------------------------------------------
c     First, print out title
c-----------------------------------------------------------------------
       CALL genSkip(Nptr)
       CALL makttl(DSDDIC,dsdptr,PDSD,Nptr,PDSUM6,tblttl,ntbttl,T,F)
       IF(.not.Lfatal)CALL prtshd(tblttl(1:ntbttl),Revspn,Ny,Revnum)
       IF(Lfatal)RETURN
       CALL mkTableTag(Mt1,tblcls(Ncol),tblttl(6:ntbttl))
       CALL mkCaption(Mt1,'Table '//tblttl(1:5))
c-----------------------------------------------------------------------
c     Print out header
c-----------------------------------------------------------------------
       CALL writTag(Mt1,'<tr>')
       CALL mkTableCell(Mt1,'head','&nbsp;')
       DO i=1,nhdrtl
        i1=hdrptr(i-1)
        i2=hdrptr(i)-1
        j1=0
        j2=0
        IF(nhd2tl.gt.0)THEN
         j1=hd2ptr(i-1)
         j2=hd2ptr(i)-1
        END IF
        IF(j2.gt.j1)THEN
         CALL mkHeaderCellScope(Mt1,0,0,'col','@',
     &                          hdrttl(i1:i2)//Cbr//hd2ttl(j1:j2))
        ELSE
         CALL mkHeaderCellScope(Mt1,0,0,'col','@',hdrttl(i1:i2))
        END IF
       END DO
       CALL writTag(Mt1,'</tr>')
c-----------------------------------------------------------------------
c     Print out table
c-----------------------------------------------------------------------
       i=Begrev
       DO WHILE (i.lt.Endtbl)
*        CALL prrvob(rev,Ncol,i-Begrev+1,i2-Begrev+1,1,1,
*     &              cobs,ipos-1,hdrttl,hdrptr,nhdrtl,hd2ttl,hd2ptr,
*     &              nhd2tl,i0,tfmt1(1:npos),i3,PCOL,PC1,9,
*     &              tblttl(1:ntbttl),F)
        i2=i-Begrev+1
        CALL addate(Revspn,Sp,i2-1,idate)
        CALL wrtdat(idate,Sp,str,ndtchr)
        CALL writTag(Mt1,'<tr>')
        CALL mkHeaderCellScope(Mt1,0,0,'row','@',str(1:ndtchr))
        DO j=0,Ncol
         IF(dpeq(rev(j,i2),DNOTST))THEN
          CALL mkTableCell(Mt1,'@','&nbsp;')
         ELSE
          WRITE(Mt1,revfmt)rev(j,i2)
         END IF
        END DO
        CALL writTag(Mt1,'</tr>')
        i=i+1
       END DO
       CALL writTag(Mt1,'</table>')
       CALL mkPOneLine(Mt1,'@','&nbsp;')
      END IF
c-----------------------------------------------------------------------
c     print out summary tables, if necessary.
c-----------------------------------------------------------------------
      IF(Prttab(Nptr+1))THEN
c-----------------------------------------------------------------------
c     First, print out header information
c-----------------------------------------------------------------------
       CALL genSkip(Nptr+1)
       CALL makttl(DSDDIC,dsdptr,PDSD,Nptr+1,PDSUM6,tblttl,ntbttl,T,F)
       IF(.not.Lfatal)CALL prtshd(tblttl(1:ntbttl),Revspn,Ny,0)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c     Compute summary statistics
c-----------------------------------------------------------------------
      IF(Prttab(Nptr+1).or.Lsumm.gt.0.or.Svltab(LSLASA+Tbltyp-1))THEN
       DO j=0,ncol
        IF(dpeq(nartot(j,1),ZERO))THEN
         aartot(j,1)=DNOTST
        ELSE
         aartot(j,1)=aartot(j,1)/nartot(j,1)
        END IF
        DO i=1,max(Ny,iyr)
         IF(i.le.Ny)THEN
          IF(dpeq(narpd(j,i),ZERO))THEN
           aarpd(j,i)=DNOTST
          ELSE
           aarpd(j,i)=aarpd(j,i)/narpd(j,i)
          END IF
         END IF
         IF(i.le.iyr)THEN
          IF(dpeq(naryr(j,i),ZERO))THEN
           aaryr(j,i)=DNOTST
          ELSE
           aaryr(j,i)=aaryr(j,i)/naryr(j,i)
          END IF
         END IF
        END DO
       END DO
      END IF
c-----------------------------------------------------------------------
c     First, print out absolute average for each period.
c-----------------------------------------------------------------------
      IF(Prttab(Nptr+1))THEN
       i0=0
       IF(Ny.eq.12)THEN
        cobs='Months       '
        nstr=7
       ELSE
        cobs='Quarters     '
        nstr=9
       END IF
       CALL mkTableTag(Mt1,'w30',tblttl(6:ntbttl))
       CALL mkCaption(Mt1,'Table '//tblttl(1:5)//
     &                ' : Summary of '//cobs(1:nstr))
*       CALL prrvob(aarpd,Ncol,1,Ny,1,1,cobs,nstr,hdrttl,hdrptr,nhdrtl,
*     &             hd2ttl,hd2ptr,nhd2tl,i0,tfmt1(1:npos),1,PCOL,PC1,9,
*     &             tblttl(1:ntbttl),T)
*       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Print out header
c-----------------------------------------------------------------------
       CALL writTag(Mt1,'<tr>')
       CALL mkTableCell(Mt1,'head','&nbsp;')
       DO i=1,nhdrtl
        i1=hdrptr(i-1)
        i2=hdrptr(i)-1
        j1=0
        j2=0
        IF(nhd2tl.gt.0)THEN
         j1=hd2ptr(i-1)
         j2=hd2ptr(i)-1
        END IF
        IF(j2.gt.j1)THEN
         CALL mkHeaderCellScope(Mt1,0,0,'col','@',
     &                          hdrttl(i1:i2)//Cbr//hd2ttl(j1:j2))
        ELSE
         CALL mkHeaderCellScope(Mt1,0,0,'col','@',hdrttl(i1:i2))
        END IF
       END DO
       CALL writTag(Mt1,'</tr>')
c-----------------------------------------------------------------------
       DO i=1,Ny
        CALL writTag(Mt1,'<tr>')
        i1=i
        IF(Ny.eq.4)i1=i1+12
        CALL mkHeaderCellScope(Mt1,0,0,'row',cpobs(i1)//cm2(i1),
     &                         cpobs(i1))
        DO j=0,Ncol
         IF(dpeq(aarpd(j,i),DNOTST))THEN
          CALL mkTableCell(Mt1,'@','&nbsp;')
         ELSE
          WRITE(Mt1,revfmt)aarpd(j,i)
         END IF
        END DO
        CALL writTag(Mt1,'</tr>')
       END DO
       CALL writTag(Mt1,'</table>')
       CALL mkPOneLine(Mt1,'@','&nbsp;')
c-----------------------------------------------------------------------
c     Then, print out absolute average for each year.
c-----------------------------------------------------------------------
       cobs='Years        '
       nstr=6
*       CALL prrvob(aaryr,Ncol,1,iyr,1,2,cobs,nstr,hdrttl,hdrptr,nhdrtl,
*     &             hd2ttl,hd2ptr,nhd2tl,i0,tfmt2(1:npos),Revspn(YR)-1,
*     &             PCOL,PC1,9,tblttl(1:ntbttl),T)
*       IF(Lfatal)RETURN
       CALL mkTableTag(Mt1,'w30',tblttl(6:ntbttl))
       CALL mkCaption(Mt1,'Table '//tblttl(1:5)//' : Summary of '//
     &                cobs(1:nstr))
c-----------------------------------------------------------------------
c     Print out header
c-----------------------------------------------------------------------
       CALL writTag(Mt1,'<tr>')
       CALL mkTableCell(Mt1,'head','&nbsp;')
       DO i=1,nhdrtl
        i1=hdrptr(i-1)
        i2=hdrptr(i)-1
        j1=0
        j2=0
        IF(nhd2tl.gt.0)THEN
         j1=hd2ptr(i-1)
         j2=hd2ptr(i)-1
        END IF
        IF(j2.gt.j1)THEN
         CALL mkHeaderCellScope(Mt1,0,0,'col','@',
     &                          hdrttl(i1:i2)//Cbr//hd2ttl(j1:j2))
        ELSE
         CALL mkHeaderCellScope(Mt1,0,0,'col','@',hdrttl(i1:i2))
        END IF
       END DO
       CALL writTag(Mt1,'</tr>')
c-----------------------------------------------------------------------
       DO i=1,iyr
        CALL writTag(Mt1,'<tr>')
        i1=Revspn(YR)+i-1
        ipos=1
        CALL itoc(i1,cobs,ipos) 
        CALL mkHeaderCellScope(Mt1,0,0,'row','@',cobs(1:(ipos-1)))
        DO j=0,Ncol
         IF(dpeq(aaryr(j,i),DNOTST))THEN
          CALL mkTableCell(Mt1,'@','&nbsp;')
         ELSE
          WRITE(Mt1,revfmt)aaryr(j,i)
         END IF
        END DO
        CALL writTag(Mt1,'</tr>')
       END DO
       CALL writTag(Mt1,'</table>')
       CALL mkPOneLine(Mt1,'@','&nbsp;')
c-----------------------------------------------------------------------
c     Finally, print out the total absolute average.
c-----------------------------------------------------------------------
       cobs='Total        '
       nstr=7
*       CALL prrvob(aartot,Ncol,1,1,1,0,cobs,nstr,hdrttl,hdrptr,nhdrtl,
*     &             hd2ttl,hd2ptr,nhd2tl,i0,tfmt1(1:npos),Revspn(YR)-1,
*     &             PCOL,PC1,9,tblttl(1:ntbttl),T)
*       IF(Lfatal)RETURN
       CALL mkTableTag(Mt1,'w30',tblttl(6:ntbttl))
       CALL mkCaption(Mt1,'Table '//tblttl(1:5)//' : Summary of '//
     &                cobs(1:nstr))
c-----------------------------------------------------------------------
c     Print out header
c-----------------------------------------------------------------------
       CALL writTag(Mt1,'<tr>')
       CALL mkTableCell(Mt1,'head','&nbsp;')
       DO i=1,nhdrtl
        i1=hdrptr(i-1)
        i2=hdrptr(i)-1
        j1=0
        j2=0
        IF(nhd2tl.gt.0)THEN
         j1=hd2ptr(i-1)
         j2=hd2ptr(i)-1
        END IF
        IF(j2.gt.j1)THEN
         CALL mkHeaderCellScope(Mt1,0,0,'col','@',
     &                          hdrttl(i1:i2)//Cbr//hd2ttl(j1:j2))
        ELSE
         CALL mkHeaderCellScope(Mt1,0,0,'col','@',hdrttl(i1:i2))
        END IF
       END DO
       CALL writTag(Mt1,'</tr>')
c-----------------------------------------------------------------------
*       i1=Revspn(YR)+i-1
       CALL writTag(Mt1,'<tr>')
       CALL mkHeaderCellScope(Mt1,0,0,'row','@','Total')
       DO j=0,Ncol
        WRITE(Mt1,revfmt)aartot(j,1)
       END DO
       CALL writTag(Mt1,'</tr>')
       CALL writTag(Mt1,'</table>')
       CALL mkPOneLine(Mt1,'@','&nbsp;')
      END IF
c-----------------------------------------------------------------------
c     Compute hinge statistics for the absolute revisions
c-----------------------------------------------------------------------
      IF(Prttab(Nptr+1).or.Lsumm.gt.0)THEN
       DO i=0,ncol
        i2=0
        DO k=1,INT(nartot(i,1))
         i2=i2+1
         trev(i2)=dabs(rev(i,k))
        END DO
        CALL hinge(trev,i2,ts,xtmp,0)
        DO k=1,5
         revhng(i,k)=ts(k)
        END DO
       END DO
      END IF
      IF(Prttab(Nptr+1))THEN
       cobs='Hinge Values '
       nstr=13
*       CALL prrvob(revhng,Ncol,1,5,1,3,cobs,13,hdrttl,hdrptr,nhdrtl,
*     &             hd2ttl,hd2ptr,nhd2tl,i0,tfmt1(1:npos),1,PCOL,PC1,9,
*     &             tblttl(1:ntbttl),T)
*       IF(Lfatal)RETURN
       CALL mkTableTag(Mt1,'w30',tblttl(6:ntbttl))
       CALL mkCaption(Mt1,'Table '//tblttl(1:5)//' : '//cobs(1:nstr))
c-----------------------------------------------------------------------
c     Print out header
c-----------------------------------------------------------------------
       CALL writTag(Mt1,'<tr>')
       CALL mkTableCell(Mt1,'head','&nbsp;')
       DO i=1,nhdrtl
        i1=hdrptr(i-1)
        i2=hdrptr(i)-1
        j1=0
        j2=0
        IF(nhd2tl.gt.0)THEN
         j1=hd2ptr(i-1)
         j2=hd2ptr(i)-1
        END IF
        IF(j2.gt.j1)THEN
         CALL mkHeaderCellScope(Mt1,0,0,'col','@',
     &                          hdrttl(i1:i2)//Cbr//hd2ttl(j1:j2))
        ELSE
         CALL mkHeaderCellScope(Mt1,0,0,'col','@',hdrttl(i1:i2))
        END IF
       END DO
       CALL writTag(Mt1,'</tr>')
c-----------------------------------------------------------------------
       DO i=1,5
        CALL writTag(Mt1,'<tr>')
        CALL mkHeaderCellScope(Mt1,0,0,'row','@',hvec(i))
        DO j=0,Ncol
         WRITE(Mt1,revfmt)revhng(j,i)
        END DO
        CALL writTag(Mt1,'</tr>')
       END DO
       CALL writTag(Mt1,'</table>')
       CALL mkPOneLine(Mt1,'@','&nbsp;')
c-----------------------------------------------------------------------
      END IF
c-----------------------------------------------------------------------
c     Save absolute average revision between concurrent and final, if
c     requested.
c-----------------------------------------------------------------------
      revlbl=' '
      IF(Svltab(LSLASA+Tbltyp-1))THEN
       IF(Tbltyp.eq.1)THEN
        nlbl=19
        revlbl(1:nlbl)='Seasonal Adjustment'
       ELSE IF(Tbltyp.eq.2)THEN
        nlbl=21
        revlbl(1:nlbl)='Changes in Adjustment'
       ELSE IF(Tbltyp.eq.3)THEN
        nlbl=28
        revlbl(1:nlbl)='Indirect Seasonal Adjustment'
       ELSE IF(Tbltyp.eq.4)THEN
        nlbl=5
        revlbl(1:nlbl)='Trend'
       ELSE IF(Tbltyp.eq.5)THEN
        nlbl=16
        revlbl(1:nlbl)='Changes in Trend'
       END IF
       CALL writTag(Ng,'<tr>')
       WRITE(Ng,1050)revlbl(1:nlbl)
       WRITE(Ng,revfmt)aartot(0,1)
       CALL writTag(Ng,'</tr>')
c-----------------------------------------------------------------------
c       IF(ncol.gt.0)THEN
c        DO i=1,ncol
c         IF(i.eq.ncol.and.Lr1y2y)THEN
c          WRITE(Ng,1051)revlbl(1:nlbl),aartot(i,1)
c         ELSE
c          WRITE(Ng,1052)revlbl(1:nlbl),aartot(i,1),Vtargt(i)
c         END IF
c        END DO
c       END IF
c-----------------------------------------------------------------------
      END IF
 1050 FORMAT('<th scope="col">',a,'</th>')
c 1051 FORMAT(' AveAbsRev of ',a,t40,f10.3,', 2yr-1yr After')
c 1052 FORMAT(' AveAbsRev of ',a,t40,f10.3,', ',i2,' Lag(s) After')
c-----------------------------------------------------------------------
      IF(Lsumm.gt.0)THEN
       DO j=0,ncol
c-----------------------------------------------------------------------
        IF(j.eq.ncol.and.Lr1y2y)THEN
         laglbl='d2y1y'
        ELSE IF(j.eq.0)THEN
         laglbl='lag00'
        ELSE
         WRITE(laglbl,1048)Vtargt(j)
        END IF
c-----------------------------------------------------------------------
        WRITE(Nform,1049)Tbltyp,laglbl,'aar.all',aartot(j,1)
c-----------------------------------------------------------------------
        i2=0
        IF(Ny.eq.4)i2=12
        DO i=1,Ny
         IF(.not.dpeq(aarpd(j,i),DNOTST))THEN
          WRITE(brklbl,1047)'p',i
          WRITE(Nform,2049)Tbltyp,laglbl,brklbl,cpobs(i+i2),aarpd(j,i)
         END IF
        END DO
c-----------------------------------------------------------------------
        i2=Revspn(YR)-1
        DO i=1,iyr
         IF(.not.dpeq(aaryr(j,i),DNOTST))THEN
          WRITE(brklbl,1047)'y',i
          WRITE(Nform,3049)Tbltyp,laglbl,brklbl,i+i2,aaryr(j,i)
         END IF
        END DO
c-----------------------------------------------------------------------
        WRITE(Nform,1053)Tbltyp,laglbl,'hinge.min',revhng(j,1)
        WRITE(Nform,1053)Tbltyp,laglbl,'hinge.25p',revhng(j,2)
        WRITE(Nform,1053)Tbltyp,laglbl,'hinge.med',revhng(j,3)
        WRITE(Nform,1053)Tbltyp,laglbl,'hinge.75p',revhng(j,4)
        WRITE(Nform,1053)Tbltyp,laglbl,'hinge.max',revhng(j,5)
       END DO
c-----------------------------------------------------------------------
 1047  FORMAT('aar.',a,i2.2)
 1048  FORMAT('lag',i2.2)       
 1049  FORMAT('r0',i1,'.',a,'.',a,': ',E17.10)
 2049  FORMAT('r0',i1,'.',a,'.',a,': ',a,' ',E17.10)
 3049  FORMAT('r0',i1,'.',a,'.',a,': ',i4,' ',E17.10)
 1053  FORMAT('r0',i1,'.',a,'.',a,': ',E17.10)
      END IF
c-----------------------------------------------------------------------
c     If concurrent and final adjustments are to be printed, print out
c     title.
c-----------------------------------------------------------------------
      IF(Prttab(Nptr+2))THEN
c-----------------------------------------------------------------------
c     Print out header information
c-----------------------------------------------------------------------
       CALL genSkip(Nptr+2)
       CALL makttl(DSDDIC,dsdptr,PDSD,Nptr+2,PDSUM6,tblttl,ntbttl,T,F)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
       DO i=0,Ntargt
        i0=0
c-----------------------------------------------------------------------
c     Create data dictionary for column headers
c-----------------------------------------------------------------------
        CALL intlst(PC1,hdrptr,nhdrtl)
        nhdr=nhdrtl+1
        CALL insstr('Revision',nhdr,PC1,hdrttl,hdrptr,nhdrtl)
        IF(Lfatal)RETURN
        IF(Cnctar.or.i.eq.0)THEN
         revstr='Concurrent, Final, and '
         nrvstr=23
         CALL insstr('Final',nhdr,PC1,hdrttl,hdrptr,nhdrtl)
         IF(Lfatal)RETURN
         rnum=Revnum
         CALL insstr('Concurrent',nhdr,PC1,hdrttl,hdrptr,nhdrtl)
        ELSE
         ipos=1
         CALL itoc(Vtargt(i),cobs,ipos) 
         revstr=cobs(1:(ipos-1))//'-period later, Final, and '
         nrvstr=26+ipos-1
         CALL insstr(cobs(1:(ipos-1))//' later',nhdr,PC1,hdrttl,hdrptr,
     &               nhdrtl)
         IF(Lfatal)RETURN
         rnum=Revnum-Vtargt(i)
         CALL insstr('Final',nhdr,PC1,hdrttl,hdrptr,nhdrtl)
        END IF
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
        CALL prtshd(tblttl(1:ntbttl),Revspn,Ny,rnum)
        CALL mkTableTag(Mt1,'w30',tblttl(6:ntbttl)//' : '//
     &                  revstr(1:nrvstr)//'Revisions')
        CALL mkCaption(Mt1,'Table '//tblttl(1:5)//' : '//
     &                 revstr(1:nrvstr)//'Revisions')
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Print out header
c-----------------------------------------------------------------------
        CALL writTag(Mt1,'<tr>')
        CALL mkTableCell(Mt1,'head','&nbsp;')
        DO j=1,nhdrtl
         j1=hdrptr(j-1)
         j2=hdrptr(j)-1
         CALL mkHeaderCellScope(Mt1,0,0,'col','@',hdrttl(j1:j2))
        END DO
        CALL writTag(Mt1,'</tr>')
c-----------------------------------------------------------------------
        DO j=1,rnum
         IF(Cnctar.or.i.eq.0)THEN
          tmp(0,j)=Cnc(j)
         ELSE
          tmp(0,j)=Fin(0,j)
         END IF
         tmp(1,j)=Fin(i,j)
         tmp(2,j)=rev(i,j)
        END DO
c-----------------------------------------------------------------------
        IF(i.gt.0)THEN
         end2=Endsa-(Vtargt(i)+1)
         if(end2.gt.Endtbl-1)end2=Endtbl-1
        ELSE
         end2=Endtbl-1
        END IF
        j=Revspn(YR)-1
        k=Begrev
        k2=Begrev+Ny-Revspn(MO)
        k3=Revspn(MO)
        DO WHILE (k.le.end2)
         CALL addate(Revspn,Sp,k-Begrev,idate)
         CALL wrtdat(idate,Sp,str,ndtchr)
         CALL writTag(Mt1,'<tr>')
         CALL mkHeaderCellScope(Mt1,0,0,'row','@',str(1:ndtchr))
         DO j=0,2
          IF(dpeq(tmp(j,k-Begrev+1),DNOTST))THEN
           CALL mkTableCell(Mt1,'@','&nbsp;')
          ELSE
           WRITE(Mt1,revfmt)tmp(j,k-Begrev+1)
          END IF
         END DO
         CALL writTag(Mt1,'</tr>')
         k=k+1
        END DO
        CALL writTag(Mt1,'</table>')
        CALL mkPOneLine(Mt1,'@','&nbsp;')
       END DO
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
       IF(Tbltyp.eq.1)THEN
        nlbl=11
        revlbl(1:nlbl)='SA_revision'
       ELSE IF(Tbltyp.eq.2)THEN
        nlbl=13
        revlbl(1:nlbl)='CHNG_revision'
       ELSE IF(Tbltyp.eq.3)THEN
        nlbl=15
        revlbl(1:nlbl)='Ind_SA_revision'
       ELSE IF(Tbltyp.eq.4)THEN
        nlbl=13
        revlbl(1:nlbl)='TRND_revision'
       ELSE IF(Tbltyp.eq.5)THEN
        nlbl=18
        revlbl(1:nlbl)='CHNG_TRND_revision'
       ELSE IF(Tbltyp.eq.6)THEN
        nlbl=11
        revlbl(1:nlbl)='SF_revision'
       END IF
c-----------------------------------------------------------------------
       CALL intlst(PC1,hdrptr,nhdrtl)
       nhdr=nhdrtl+1
       IF(Ntargt.gt.0)THEN
        IF(Lr1y2y)THEN
         CALL insstr(revlbl(1:(nlbl-5))//'(1yr-2yr)',nhdr,PC1,hdrttl,
     &               hdrptr,nhdrtl)
         IF(Lfatal)RETURN
        END IF
        DO i=Ntargt,1,-1
         ipos=1
         CALL itoc(Vtargt(i),cobs,ipos) 
         CALL insstr(revlbl(1:(nlbl-5))//'('//cobs(1:(ipos-1))//')',
     &               nhdr,PC1,hdrttl,hdrptr,nhdrtl)
         IF(Lfatal)RETURN
        END DO
       END IF
       CALL insstr(revlbl(1:nlbl),nhdr,PC1,hdrttl,hdrptr,nhdrtl)
       IF(Lfatal)RETURN
       WRITE(fh,1040)'date',(TABCHR,
     &               hdrttl(hdrptr(ielt):hdrptr(ielt+1)-1),ielt=0,ncol)
       WRITE(fh,1040)'----',(TABCHR,'-----------------------',
     &               ielt=0,ncol)
c-----------------------------------------------------------------------
c     begin looping though observations
c-----------------------------------------------------------------------
       DO i=Begrev,Endtbl-1
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
        DO k=0,ncol
         outstr(ipos:ipos)=TABCHR
         ipos=ipos+1
         CALL dtoc(rev(k,Revptr),outstr,ipos)
         IF(Lfatal)RETURN
        END DO
        WRITE(fh,1040)outstr(1:ipos-1)
       END DO
       CALL fclose(fh)
      END IF
c-----------------------------------------------------------------------
c     If concurrent and final adjustments are to be saved, open file for
c     saved adjustments.
c-----------------------------------------------------------------------
      IF(Savtab(Nptr+2).or.Lgraf)THEN
       ncol=0
       IF(Savtab(Nptr+2))CALL opnfil(T,F,Nptr+2,fh,locok)
       IF(locok.and.Lgraf)CALL opnfil(T,Lgraf,Nptr+2,fh2,locok)
       IF(.not.locok)THEN
        CALL abend()
        RETURN
       END IF
c-----------------------------------------------------------------------
c     Print header for revisions
c-----------------------------------------------------------------------
       IF(Tbltyp.eq.1)THEN
        nlbl=3
        revlbl(1:nlbl)='_SA'
       ELSE IF(Tbltyp.eq.2)THEN
        nlbl=5
        revlbl(1:nlbl)='_CHNG'
       ELSE IF(Tbltyp.eq.3)THEN
        nlbl=7
        revlbl(1:nlbl)='_Ind_SA'
       ELSE IF(Tbltyp.eq.4)THEN
        nlbl=5
        revlbl(1:nlbl)='_TRND'
       ELSE IF(Tbltyp.eq.5)THEN
        nlbl=10
        revlbl(1:nlbl)='_CHNG_TRND'
       END IF
       CALL intlst(PC1,hdrptr,nhdrtl)
       nhdr=nhdrtl+1
       IF(Ntargt.gt.0)THEN
        DO i=Ntargt,1,-1
         ipos=1
         CALL itoc(Vtargt(i),cobs,ipos)
         IF(Vtargt(i).le.9)THEN
          CALL insstr('Conc(0'//cobs(1:(ipos-1))//')'//revlbl(1:nlbl),
     &                nhdr,PC1,hdrttl,hdrptr,nhdrtl)
         ELSE
          CALL insstr('Conc('//cobs(1:(ipos-1))//')'//revlbl(1:nlbl),
     &                nhdr,PC1,hdrttl,hdrptr,nhdrtl)
         END IF
         IF(Lfatal)RETURN
        END DO
       END IF
       CALL insstr('Final'//revlbl(1:nlbl),nhdr,PC1,hdrttl,hdrptr,
     &             nhdrtl)
       IF(.not.Lfatal)CALL insstr('Conc'//revlbl(1:nlbl),nhdr,PC1,
     &                            hdrttl,hdrptr,nhdrtl)
       IF(Lfatal)RETURN
       IF(Savtab(Nptr+2))THEN
        WRITE(fh,1040)'date',(TABCHR,
     &                hdrttl(hdrptr(ielt-1):(hdrptr(ielt)-1)),ielt=1,
     &                nhdrtl)
        WRITE(fh,1040)'----',(TABCHR,'-----------------------',
     &                ielt=1,nhdrtl)
       END IF
       IF(Lgraf)THEN
        WRITE(fh2,1040)'date',(TABCHR,
     &                 hdrttl(hdrptr(ielt-1):(hdrptr(ielt)-1)),ielt=1,
     &                 nhdrtl)
        WRITE(fh2,1040)'----',(TABCHR,'-----------------------',
     &                 ielt=1,nhdrtl)
       END IF
c-----------------------------------------------------------------------
c     begin looping though observations
c-----------------------------------------------------------------------
       DO i=Begrev,Endtbl-1
        Revptr=i-Begrev+1
c-----------------------------------------------------------------------
c     Set date for ith revision
c-----------------------------------------------------------------------
        CALL addate(Revspn,Ny,Revptr-1,idate)
        rdbdat=100*idate(YR)+idate(MO)
c-----------------------------------------------------------------------
c     Save concurrent and final adjustments with date
c-----------------------------------------------------------------------
        ipos=1
        CALL itoc(rdbdat,outstr,ipos)
        IF(Lfatal)RETURN
        outstr(ipos:ipos)=TABCHR
        ipos=ipos+1
        CALL dtoc(Cnc(Revptr),outstr,ipos)
        IF(Lfatal)RETURN
        outstr(ipos:ipos)=TABCHR
        ipos=ipos+1
        CALL dtoc(Fin(0,Revptr),outstr,ipos)
        IF(Lfatal)RETURN
        DO k=1,Ntargt
         outstr(ipos:ipos)=TABCHR
         ipos=ipos+1
         CALL dtoc(Fin(k,Revptr),outstr,ipos)
         IF(Lfatal)RETURN
        END DO
        IF(Savtab(Nptr+2))WRITE(fh,1040)outstr(1:ipos-1)
        IF(Lgraf)WRITE(fh2,1040)outstr(1:ipos-1)
       END DO
       IF(Savtab(Nptr+2))CALL fclose(fh)
       IF(Lgraf)CALL fclose(fh2)
      END IF
c-----------------------------------------------------------------------
      RETURN
c-----------------------------------------------------------------------
 1040 FORMAT(1000a)
      END
