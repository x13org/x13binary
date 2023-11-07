C     Last change: Mar.2021 - change from E3->E2 in e4lab
C     previous change:  BCM  16 Feb 1999    3:59 pm
      SUBROUTINE table(Z,Ib,Ie,Ktabl,Itype,Nop,Y,Tblptr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- THIS SUBROUTINE WRITES TABLE OUTPUT.
C --- Z IS THE ARRAY TO BE PRINTED FROM IB TO IE.
C --- Y IS AN ADDITIONAL ARRAY TO BE PRINTED ON TABLE.
C --- KTABL IS THE TABLE NUMBER.
C --- NOP = 1,AVERAGE.
C ---     = 2,TOTAL.
C ---     = 3,STANDARD DEVIATION.
C ---     = 4,MOVING 5-YEAR STD. DEV.
C ---     = 5,NONE.
C ---     = 0,AVERAGE OF ABSOLUTE VALUE.
C --- ITYPE SPECIFIES IF THIS IS ADDITIONAL KTABL.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'tbltitle.prm'
      INCLUDE 'desfct.prm'
      INCLUDE 'desfc2.prm'
      INCLUDE 'tbllog.prm'
      INCLUDE 'filext.prm'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'tfmts.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'missng.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'xtrm.cmn'
      INCLUDE 'goodob.cmn'
      INCLUDE 'force.cmn'
      INCLUDE 'agr.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      INTEGER YR,MO,PSP1
      LOGICAL F,T
      DOUBLE PRECISION ZERO,BIG
      PARAMETER(YR=1,MO=2,F=.false.,T=.true.,PSP1=PSP+1,ZERO=0D0,
     &          BIG=10D16)
c-----------------------------------------------------------------------
      CHARACTER tblttl*(PTTLEN),tyrly*(5),fobs*(7),fsum*(5),
     &          tfmt*(110),tfmt2*(110),e4lab*(21),dash*(60),tbsmlb*(20),
     &          ayrly*(32),cspace*(18),thisOb*(30),thisCode*(3)
      LOGICAL ltbl
      DOUBLE PRECISION tmp,x,Z,Y,xmin,xmax,mtmp,numtmp
      INTEGER i,ip,ifct,jyr,nb,ie2,l,begtbl,nopp,Nop,Ib,Ie,iin,ipow,
     &        ldec,nb2,Ktabl,iopt,ipos,wid,ndash,npos,npos2,Itype,kyr,
     &        nobs,ib1,ie1,im,im1,im2,nop1,ke,Tblptr,nfct,idate,begfct,
     &        ntbttl,nmod,sp1,nftbl,ntbx
      DIMENSION ip(22),tmp(PSP1),x(PLEN),idate(2),begfct(2),tyrly(0:5),
     &          Z(*),Y(*),begtbl(2),e4lab(4),tbsmlb(5),ayrly(0:5)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      DOUBLE PRECISION totals,sdev
      INTEGER nblank
      EXTERNAL totals,sdev,nblank,dpeq
c-----------------------------------------------------------------------
      DATA tyrly/'AVABS',' AVGE','TOTAL',' S.D.',' S.D.','     '/
      DATA ip/0,0,3*1,0,0,3*1,0,0,1,1,0,1,1,1,0,1,1,1/
      DATA e4lab/'           Unmodified','             Modified',
     &           '                (D11)','                 (E2)'/
      DATA tbsmlb/'Table Total-        ','Mean-               ',
     &            'Standard Deviation- ','Minimum-            ',
     &            'Maximum-            '/
      DATA ayrly/'Average of absolute value       ',
     &           'Average                         ',
     &           '@                               ',
     &           'Standard deviation              ',
     &           'Moving 5-year standard deviation',
     &           '@                               '/
      DATA cspace/'&nbsp;&nbsp;&nbsp;'/
c-----------------------------------------------------------------------
c     include files containing DATA statements 
c-----------------------------------------------------------------------
      INCLUDE 'desfct.var'
      INCLUDE 'desfc2.var'
      INCLUDE 'filext.var'
c-----------------------------------------------------------------------
c     Return if this is a transparent seasonal adjustment for sliding
c     spans, revisions, or X-11 Holiday adjustment.
c-----------------------------------------------------------------------
      IF(Lhiddn)RETURN
      IF(.not.Prt1ps.AND.(.NOT.(Kpart.eq.1.and.Ktabl.eq.1)))THEN
       IF(Ixreg.eq.2.OR.Khol.eq.1)THEN
        IF(.NOT.((Kpart.eq.1.and.Ktabl.eq.4).OR.
     &    (Kpart.eq.0.and.Ktabl.eq.1).or.(Ktabl.ge.14.and.Ktabl.le.16)
     &     .or.Ktabl.eq.18.or.Ktabl.eq.21.or.Ktabl.eq.22))RETURN
       END IF
      END IF
c-----------------------------------------------------------------------
C --- INITIALIZATION
c-----------------------------------------------------------------------
      nftbl=Posffc-Posfob
      IF(Kpart.eq.1)ip(3)=0
      IF(Kpart.ne.1)ip(3)=1
      nopp=Nop
      IF(Missng)THEN
       IF(nopp.lt.5.and.Ib.gt.0)THEN
        i=Ib
        DO WHILE (i.le.Ie.and.nopp.ne.5)
         IF(dpeq(Z(i),Mvval))nopp=5
         i=i+1
        END DO
       END IF
       mtmp=10D0**(Tblwid+1)
       IF(Mvval.gt.mtmp)mtmp=Mvval
      END IF
      ke=Ie
      iin=1
      ipow=0
      ldec=Kdec
      sp1=Ny+1
      nmod=5
      IF(Ny.lt.5)nmod=10
      IF(Ib.ne.0)THEN
c-----------------------------------------------------------------------
c     Determine if forecasts are to be printed out and, if so, how many
c-----------------------------------------------------------------------
       IF(Tblptr.le.PDSF)THEN
        ifct=dsfptr(Tblptr)-dsfptr(Tblptr-1)
       ELSE
        ifct=df2ptr(Tblptr-PDSF)-df2ptr(Tblptr-PDSF-1)
       END IF
       nfct=ifct
       IF(ifct.gt.0)THEN
        IF(((Kpart.eq.2.and.Ktabl.eq.1).or.(Kpart.eq.4.and.
     &    (((Ktabl.eq.10.or.Ktabl.eq.16).and.Itype.eq.2).or.
     &    ((Ktabl.eq.16.or.Ktabl.eq.18).and.Itype.eq.3))).or.
     &    (Kpart.eq.5.and.Ktabl.eq.16).and.(Kpart.eq.4.and.Ktabl.eq.11
     &    .and.Itype.eq.6)).and.nftbl.eq.0)THEN
         nfct=0
         ifct=0
        ELSE IF(Kpart.eq.4.and.Ktabl.eq.11.and.Itype.eq.6.and.
     &          (.not.Lfctfr))THEN
         nfct=0
         ifct=0
        ELSE
         nfct=nftbl
         IF(nftbl.eq.0)THEN
          IF((Kpart.eq.0.and.Ktabl.eq.1).or.(Kpart.eq.1.AND.
     &       (Ktabl.eq.4.OR.Ktabl.eq.6.or.Ktabl.eq.7.or.Ktabl.eq.8.or.
     &        Ktabl.eq.16).and.Nfcst.eq.0).or.(Kpart.eq.3.and.
     &       (Ktabl.eq.16.or.Ktabl.eq.18.or.Ktabl.gt.20)).or.
     &       (Iagr.lt.4.and.(Kpart.eq.4.and.(Ktabl.eq.8.or.Ktabl.eq.10
     &        .or.Ktabl.eq.16.or.Ktabl.eq.18))))THEN
           nfct=Ny
          ELSE
           ifct=0
          END IF
         END IF
        END IF
       END IF
c-----------------------------------------------------------------------
c     Copy data into temporary vector x
c-----------------------------------------------------------------------
       ie2=Ie+nfct
       xmin=DNOTST
       xmax=DNOTST
       ltbl=Ktabl.eq.17.or.((Kpart.eq.2.or.Kpart.eq.3).and.
     &     (Ktabl.eq.16.or.Ktabl.eq.21.or.Ktabl.eq.22))
       DO i=Ib,ie2
        IF(Muladd.eq.2.and.(.not.ltbl))THEN
         IF(dpeq(Z(i),DNOTST).or.dpeq(Z(i),BIG))THEN
          x(i)=Z(i)
         ELSE
          x(i)=exp(Z(i))
         END IF
        ELSE
         x(i)=Z(i)
         IF(Missng)THEN
          IF(dpeq(x(i),Mvval))x(i)=mtmp
         END IF
        END IF
        IF(nopp.lt.5.and.i.le.Ie)THEN
         IF(Gudval(i))THEN
          IF(dpeq(xmin,DNOTST))THEN
           xmin=x(i)
           xmax=x(i)
          ELSE
           IF(xmin.gt.x(i))xmin=x(i)
           IF(xmax.lt.x(i))xmax=x(i)
          END IF
         END IF
        END IF
       END DO
       IF(Muladd.eq.1)THEN
        IF((Kpart.eq.5.and.Ktabl.eq.18.and.Itype.eq.1).or.
     &     (Ktabl.eq.17))THEN
         ipow=1
         IF((Kpart.eq.5.and.Ktabl.eq.18.and.Itype.eq.1).and.
     &      (ldec.eq.0))ldec=3
        END IF
       ELSE
        ipow=ip(Ktabl)
        IF((Kpart.eq.1.and.(Ktabl.eq.2.or.Ktabl.eq.6.or.Ktabl.eq.7)).or.
     &     (Kpart.eq.4.and.Ktabl.eq.12.and.Itype.eq.3).or.
     &     (Kpart.eq.5.AND.(Ktabl.eq.6.or.Ktabl.eq.7)).or.
     &     (Kpart.eq.0.and.Ktabl.eq.1).or.
     &     (Kpart.eq.4.and.Ktabl.eq.11.and.Itype.eq.6))
     &      ipow=1
        IF((Kpart.eq.4.and.Ktabl.eq.10.and.Itype.eq.2).or.   
     &    (Kpart.eq.4.and.Ktabl.eq.16.and.Itype.eq.2).or.   
     &    (Kpart.eq.-1.and.Ktabl.eq.10.and.Itype.eq.2).or.
     &    (Kpart.eq.1.and.Ktabl.eq.18))
     &     ipow=0
        IF(ldec.eq.0.and.((Kpart.eq.4.and.Itype.eq.1.and.(Ktabl.eq.10
     &     .or.Ktabl.eq.16)).or.(Kpart.eq.3.and.(Ktabl.eq.16.or.
     &     Ktabl.eq.18)).or.(Kpart.eq.1.and.Ktabl.eq.16)))ldec=2
        IF(ldec.eq.0.and.ipow.eq.1)ldec=1
       END IF
       iopt=ip(Ktabl)+ipow
       IF(Kpart.eq.5.AND.(Ktabl.eq.5.or.Ktabl.eq.7.or.Ktabl.eq.8))iopt=1
       IF(Muladd.ne.1)THEN
        IF((Kpart.eq.1.and.(Ktabl.eq.2.or.Ktabl.eq.6.or.Ktabl.eq.7)).or.
     &     (Kpart.eq.0.and.Ktabl.eq.1).or.
     &     (Kpart.eq.4.and.Ktabl.eq.12.and.Itype.eq.3).or.
     &     (Kpart.eq.4.and.Ktabl.eq.11.and.Itype.eq.6))iopt=2
        IF((Kpart.eq.1.and.Ktabl.eq.10))iopt=0
       END IF
      END IF
c-----------------------------------------------------------------------
c     Compute starting date for table
c-----------------------------------------------------------------------
      begtbl(YR)=Lyr
      begtbl(MO)=mod(Ib,Ny)
      IF(begtbl(MO).eq.0)begtbl(MO)=Ny
      IF(Ib.gt.Pos1bk)THEN
       begtbl(YR)=begtbl(YR)+((Ib-1)/Ny)
*       begtbl(YR)=begtbl(YR)+((Ib-Pos1bk)/Ny)
      ELSE IF((Ixreg.eq.2.or.Ib.gt.Ny).and.Nbcst.eq.0) THEN
       begtbl(YR)=begtbl(YR)+((Ib-1)/Ny)
      END IF
c-----------------------------------------------------------------------
c     compute number of observations in table
c-----------------------------------------------------------------------
      nobs=Ie-Ib+1
c-----------------------------------------------------------------------
c     Get the table description from one of the data dictionaries
C     ------------------------------------------------------------------
      CALL getdes(Tblptr,tblttl,ntbttl,T)
      IF(Lfatal)RETURN
      thisCode='   '
      IF(tbxdic(Tblptr).ne.'xxx')THEN
       thisCode=tbxdic(Tblptr)
       ntbx=nblank(thisCode)
      ELSE
       ntbx=3
      END IF
c     ------------------------------------------------------------------
c     Generate entries for index, skip links
c     ------------------------------------------------------------------
      CALL genSkip(Tblptr)
c-----------------------------------------------------------------------
c --- Write header for table
c-----------------------------------------------------------------------
      If(Ib.eq.0.and.Ie.eq.0)THEN
       IF(ntbttl.gt.0)THEN
        CALL writTagOneLine(Mt1,'h3','@',tblttl(1:ntbttl))
        IF(Kpart.eq.4.and.Ktabl.eq.9)THEN
         CALL mkTableTag(Mt1,'x11',tblttl(1:ntbttl))
         CALL mkCaption(Mt1,tblttl(1:ntbttl))
        END IF
       END IF
       RETURN
      END IF
c-----------------------------------------------------------------------
c     If this is the E 4 table, print out results and print ratios.
c-----------------------------------------------------------------------
      IF(Kpart.eq.5.and.Ktabl.eq.4)THEN
*       IF(Pos1bk.ne.1)begtbl(YR)=begtbl(YR)+1
c-----------------------------------------------------------------------
c     print out header for table
c-----------------------------------------------------------------------
       CALL tblhdr(Ktabl,Itype,Ixreg,nobs,begtbl,1,Y,tblttl,ntbttl)
       IF(Lfatal)RETURN
*       wid=Tblwid
*       IF(wid.lt.12)wid=12
*       write(fobs,1030)wid
* 1030  FORMAT('a',i2)
*       fbase=' '
*       CALL getstr(TFMDIC,tfmptr,PTFM,Iptr+1,fbase,ipos)
*       IF(Lfatal)RETURN
*       CALL cnvfmt(fbase,tfmt,fobs(1:3),fobs(1:3),ipos,npos)
*       CALL setchr('-',60,dash)
*       dash(1:1)=' '
*       ndash=10+2*(Disp2+wid)
*       WRITE(Mt1,1010)dash(1:ndash)
       IF(tbxdic(Tblptr).ne.'xxx')
     &    CALL makDivId(Mt1,thisCode(1:ntbx),'@')
       CALL mkTableTag(Mt1,'w50',tblttl(1:ntbttl))
       CALL mkCaption(Mt1,tblttl(1:ntbttl))
       CALL writTag(Mt1,'<tr>')
       CALL mkTableCell(Mt1,'head','&nbsp;')
       CALL mkHeaderCellScope(Mt1,0,0,'col','@',e4lab(1)//Cbr//e4lab(3))
       CALL mkHeaderCellScope(Mt1,0,0,'col','@',e4lab(2)//Cbr//e4lab(4))
       CALL writTag(Mt1,'</tr>')
*       WRITE(Mt1,1010)dash(1:ndash)
* 1010  FORMAT(a)
       jyr=begtbl(YR)
c-----------------------------------------------------------------------
c     Generate format for table, and print out ratios/differences
c-----------------------------------------------------------------------
       wid=Tblwid
       IF(wid.lt.12)wid=12
       ldec=Kdec
       IF(mod(Muladd,2).eq.0.and.ldec.eq.0)ldec=2
       write(fobs,1040)wid,ldec
 1040  FORMAT('(f',i2,'.',i1,')')
*       fbase=' '
*       CALL getstr(TFMDIC,tfmptr,PTFM,Iptr,fbase,ipos)
*       IF(Lfatal)RETURN
*       CALL cnvfmt(fbase,tfmt,fobs,fobs,ipos,npos)
       DO i=1,Ie
        CALL writTag(Mt1,'<tr>')
        write(Mt1,1010)jyr
 1010   FORMAT('<th scope="row">',i4,'</th>')
        write(thisOb,fobs)X(i)
        CALL mkTableCell(Mt1,'center',thisOb)
        write(thisOb,fobs)Y(i)
        CALL mkTableCell(Mt1,'center',thisOb)
        jyr=jyr+1
        CALL writTag(Mt1,'</tr>')
       END DO
       IF(tbxdic(Tblptr).ne.'xxx')THEN
        CALL writTag(Mt1,'</table></div>')
       ELSE
        CALL writTag(Mt1,'</table>')
       END IF
       CALL mkPOneLine(Mt1,'@','&nbsp;')
       RETURN
      END IF
c-----------------------------------------------------------------------
c     print out header for table
c-----------------------------------------------------------------------
      CALL tblhdr(Ktabl,Itype,Ixreg,nobs,begtbl,Ny,Y,tblttl,ntbttl)
      IF(Lfatal)RETURN
      IF(tbxdic(Tblptr).ne.'xxx')CALL makDivId(Mt1,thisCode(1:ntbx),'@')
      CALL mkTableTag(Mt1,'x11',tblttl(1:ntbttl))
      CALL mkCaption(Mt1,tblttl(1:ntbttl))
c-----------------------------------------------------------------------
C --- WRITE COLUMN HEADINGS.
c-----------------------------------------------------------------------
      l=Ny+1
      CALL prtcol(Ny,Mt1,nopp,tyrly(nopp),ayrly(nopp),Colhdr)
      IF(nopp.eq.5)l=l-1
c-----------------------------------------------------------------------
C --- WRITE TABLE.
c-----------------------------------------------------------------------
      jyr=Lyr+(Ib-1)/Ny
      kyr=(Ie+Ny-1)/Ny+Lyr-1
      iin=iin+(jyr-Lyr)
      DO i=1,PSP1
       tmp(i)=DNOTST
      END DO
      ib1=Ib
      ie1=(jyr-Lyr+1)*Ny
      IF(ie1.gt.Ie)ie1=Ie
      im=Ib-(Ib-1)/Ny*Ny
      DO WHILE (T)
       im1=im
       DO i=ib1,ie1
        tmp(im)=x(i)
        im=im+1
       END DO
       im2=im-1
c-----------------------------------------------------------------------
c     Compute totals or std deviation for this year's observation.
c-----------------------------------------------------------------------
       IF(nopp.eq.2)THEN
        tmp(l)=totals(tmp,im1,im2,1,0)
       ELSE IF(nopp.eq.3)THEN
        tmp(l)=sdev(tmp,im1,im2,1,iopt)
       ELSE IF(nopp.eq.4)THEN
        tmp(l)=Y(iin)
        iin=iin+1
       ELSE IF(nopp.ne.5)THEN
        tmp(l)=totals(tmp,im1,im2,1,2-nopp)
       END IF
c-----------------------------------------------------------------------
c     Compute number of blanks for the beginning or end of the series
c     for observations not in the series.
c-----------------------------------------------------------------------
       nb=0
       IF(jyr.eq.begtbl(YR).and.begtbl(MO).gt.1)nb=begtbl(MO)
       nb2=0
       IF(ie1.eq.Ie)THEN
        CALL addate(begtbl,Ny,nobs-1,idate)
        nb2=idate(MO)
        IF(nb2.eq.Ny)nb2=0
       END IF
c-----------------------------------------------------------------------
c     If number of decimals in printout has changed, redo format
c-----------------------------------------------------------------------
       IF(ldec.eq.Kdec)then
        tfmt=Ifmt1
        tfmt2=Ifmt2
        npos=Nfmt1
        npos2=Nfmt2
       ELSE
        IF(Tblwid.gt.9)then
         write(tfmt,1001)Tblwid,ldec
         write(tfmt2,1001)Tblwid+2,ldec
         npos=7
         npos2=7
        ELSE
         write(tfmt,1002)Tblwid,ldec
         npos=6
         IF(Tblwid.gt.7)THEN
          write(tfmt2,1001)Tblwid+2,ldec
          npos2=7
         ELSE
          write(tfmt2,1002)Tblwid+2,ldec
          npos2=6
         END IF
        END IF
       END IF
 1001  format('(f',i2,'.',i1,')')
 1002  format('(f',i1,'.',i1,')')
c-----------------------------------------------------------------------
c     Write out this year's data.
c-----------------------------------------------------------------------
       CALL wrttbl(tmp,jyr,'XXXXX',l,ldec,Mt1,tfmt(1:npos),ipow,
     &             l.eq.sp1)
       IF(Lfatal)RETURN
*        IF((.not.Lcmpaq) .or. (((mod(jyr,nmod)+1).eq.nmod) .or.  
*     &     (kyr.lt.jyr+1)))WRITE(Mt1,1050)
c-----------------------------------------------------------------------
c     Update year, starting and ending position of year
c-----------------------------------------------------------------------
       jyr=jyr+1
       im=1
       ib1=ie1+1
       ie1=ie1+Ny
       IF(kyr.lt.jyr)THEN
        IF((Kpart.eq.2.and.(Ktabl.eq.4.or.Ktabl.eq.9)).or.
     &    ((Kpart.eq.2.or.Kpart.eq.3).and.Ktabl.eq.17))THEN
         IF(Ksdev.gt.0)THEN
          DO i=1,Ny
           tmp(i)=Stdper(i)
          END DO
          CALL wrttbl(tmp,0,tyrly(3),Ny,ldec,Mt1,tfmt2(1:npos2),ipow,F)
          IF(Lfatal)RETURN
         END IF
         IF(tbxdic(Tblptr).eq.'xxx')then
          CALL writTag(Mt1,'</table>')
         else
          CALL writTag(Mt1,'</table></div>')
         END IF
         CALL mkPOneLine(Mt1,'@','&nbsp;')
c-----------------------------------------------------------------------
C --- CALCULATE AND WRITE COLUMN SUMMARIES.
c-----------------------------------------------------------------------
        ELSE IF(nopp.lt.5)THEN
         ie1=Ib+Ny-1
         im=Ib-(Ib-1)/Ny*Ny
         nb=0
         nb2=0
         DO i=Ib,ie1
          IF(i.gt.Ie)THEN
           tmp(im)=DNOTST
           IF(i.eq.im)THEN
            IF(nb2.eq.0)nb2=im-1
           ELSE
            IF(nb.eq.0)nb=1
            nb=nb+1
           END IF
          ELSE
           IF(nopp.gt.2)THEN
            tmp(im)=sdev(x,i,Ie,Ny,iopt)
           ELSE IF(nopp.gt.0)THEN
            tmp(im)=totals(x,i,Ie,Ny,1)
           ELSE
            tmp(im)=totals(x,i,Ie,Ny,2)
           END IF
          END IF
          IF(im.eq.Ny)im=0
          im=im+1
         END DO
C --- GENERATE COLUMN SUMMARY FORMATS.
         nop1=(nopp-1)/2*2+1
         IF(nopp.eq.0)nop1=0
c02      WRITE(MT1,IF2) TYRLY(NOP1),(TMP(I),I = 1,NY)
         CALL wrttbl(tmp,0,tyrly(nop1),Ny,ldec,Mt1,tfmt2(1:npos2),
     &               ipow,F)
         IF(Lfatal)RETURN
         IF(tbxdic(Tblptr).eq.'xxx')then
          CALL writTag(Mt1,'</table>')
         else
          CALL writTag(Mt1,'</table></div>')
         END IF
         numtmp=totals(x,Ib,Ie,1,3)
         IF(numtmp.gt.ZERO)THEN
          IF(nopp.gt.0)THEN
           tmp(1)=totals(x,Ib,Ie,1,0)
           tmp(2)=tmp(1)/numtmp
          ELSE
           tmp(2)=totals(x,Ib,Ie,1,2)
           tmp(1)=tmp(2)*numtmp
          END IF
          tmp(3)=sdev(x,Ib,Ie,1,iopt)
          IF(ipow.ne.0)THEN
           DO i=1,3
            tmp(i)=tmp(i)*100D0
           END DO
           xmax=xmax*100D0
           xmin=xmin*100D0
          END IF     
C --- WRITE TABLE SUMMARY.
          CALL mkPClass(Mt1,'center')
          WRITE(Mt1,Ifmt3)(tbsmlb(i),tmp(i),cspace,i=1,2),
     &                     tbsmlb(3),tmp(3),Cbr,tbsmlb(4),xmin,
     &                     cspace,tbsmlb(5),xmax,'</p>'
          CALL mkPOneLine(Mt1,'@','&nbsp;')
         END IF
        ELSE
         IF(tbxdic(Tblptr).eq.'xxx')then
          CALL writTag(Mt1,'</table>')
         else
          CALL writTag(Mt1,'</table></div>')
         END IF
         CALL mkPOneLine(Mt1,'@','&nbsp;')
        END IF
        GO TO 10
       ELSE IF(kyr.eq.jyr)THEN
        DO i=1,Ny
         tmp(i)=DNOTST
        END DO
        ie1=ke
       END IF
      END DO
c-----------------------------------------------------------------------
c     Check to see if there are forecasts to be printed out.
c-----------------------------------------------------------------------
   10 IF(ifct.gt.0)THEN
c-----------------------------------------------------------------------
c     If forecasts are to be printed out, get base title for forecasts
c-----------------------------------------------------------------------
       IF(Tblptr.le.PDSF)THEN
        CALL makttl(DSFDIC,dsfptr,PDSF,Tblptr,0,tblttl,ntbttl,T,T)
        IF(Lfatal)RETURN
       ELSE
        CALL makttl(DF2DIC,df2ptr,PDF2,Tblptr-PDSF,0,tblttl,ntbttl,T,T)
        IF(Lfatal)RETURN
       END IF
c-----------------------------------------------------------------------
c     compute starting date of forecasts in table, ending observation
c-----------------------------------------------------------------------
       CALL addate(begtbl,Ny,nobs,begfct)
       ke=Ie+nfct
*       lp=F
c-----------------------------------------------------------------------
c     print header for forecasts
c-----------------------------------------------------------------------
       CALL prtshd(tblttl(1:ntbttl),begfct,Ny,nfct)
       IF(Lfatal)RETURN
       IF((.not.(Kpart.eq.1.and.Ktabl.eq.1)).AND.(Ixreg.eq.2.OR.
     &  (Khol.eq.1.OR.(Kpart.eq.0.and.Ktabl.eq.1))))THEN
        IF(Ixreg.eq.2.AND.(Khol.eq.1.OR.(Kpart.eq.0.and.Ktabl.eq.1)))
     &     THEN
         WRITE(Mt1,1000) 'irregular regression and X-11 Easter effects'
        ELSE IF(Ixreg.eq.2)THEN
         WRITE(Mt1,1000) 'irregular regression effects'
        ELSE
         WRITE(Mt1,1000) 'X-11 Easter effects'
        END IF
 1000   FORMAT('<p><strong>First pass -</strong> Estimating ',a,'</p>')
       END IF
c-----------------------------------------------------------------------
c     Print column headers
c-----------------------------------------------------------------------
       CALL makDivId(Mt1,thisCode(1:ntbx)//'.fct','@')
       CALL mkTableTag(Mt1,'x11',tblttl(1:ntbttl))
       CALL mkCaption(Mt1,tblttl(1:ntbttl))
       CALL prtcol(Ny,Mt1,nopp,tyrly(nopp),ayrly(nopp),Colhdr)
c-----------------------------------------------------------------------
c     Print forecasts
c-----------------------------------------------------------------------
       jyr=begfct(YR)
       CALL addate(begfct,Ny,nfct-1,idate)
       DO i=1,PSP1
        tmp(i)=DNOTST
       END DO
       ib1=Ie+1
       ie1=(((ib1-1)/Ny)+1)*Ny
       IF(ie1.gt.ie2)ie1=ie2
       im=ib1-(ib1-1)/Ny*Ny
       DO WHILE (jyr.le.idate(YR))
        im1=im
        DO i=ib1,ie1
         tmp(im)=x(i)
         im=im+1
        END DO
        im2=im-1
c-----------------------------------------------------------------------
c     Compute totals or std deviation for this year's observation.
c-----------------------------------------------------------------------
        IF(nopp.eq.2)THEN
         tmp(l)=totals(tmp,im1,im2,1,0)
        ELSE IF(nopp.eq.0)THEN
         tmp(l)=totals(tmp,im1,im2,1,2)
        ELSE IF(nopp.ne.5)THEN
         tmp(l)=totals(tmp,im1,im2,1,1)
        END IF
c-----------------------------------------------------------------------
c     Compute number of blanks for the beginning or end of the series
c     for observations not in the series.
c-----------------------------------------------------------------------
        nb=0
        IF(jyr.eq.begfct(YR).and.begfct(MO).gt.1)nb=begfct(MO)
        nb2=0
        IF(jyr.eq.idate(YR).and.idate(MO).lt.Ny)nb2=idate(MO)
c-----------------------------------------------------------------------
c     Write out this year's data.
c-----------------------------------------------------------------------
        CALL wrttbl(tmp,jyr,'XXXXX',l,ldec,Mt1,tfmt(1:npos),ipow,
     &              l.eq.sp1)
        IF(Lfatal)RETURN
*        IF((.not.Lcmpaq) .or. (((mod(jyr,nmod)+1).eq.nmod) .or.  
*     &     (idate(YR).lt.jyr+1)))WRITE(Mt1,1050)
        im=1
        ib1=ie1+1
        ie1=ie1+Ny
        IF(ie1.gt.ke)ie1=ke
        DO i=1,PSP1
         tmp(i)=DNOTST
        END DO
        jyr=jyr+1
       END DO
c-----------------------------------------------------------------------
       CALL writTag(Mt1,'</table></div>')
       CALL mkPOneLine(Mt1,'@','&nbsp;')
c-----------------------------------------------------------------------
      END IF
      RETURN
      END
