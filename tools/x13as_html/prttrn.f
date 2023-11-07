      SUBROUTINE prttrn(Stc,Trnchr,Ib,Ie,Ktabl,Tblptr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- THIS SUBROUTINE GENERATES a table of the trend component with 
c     labels for observations that were replaced because they were
c     less than zero (for a multipicative seasonal adjustment).
c-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'tbltitle.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'error.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'tfmts.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      CHARACTER fsum*(5),Trnchr*(1),fobs*(7),tblttl*(PTTLEN),
     &          ifmt1a*(110),ifmt2a*(110),fbase*(110),fmtcl2*(110),
     &          tbsmlb*(20),ctmp*(2),cspace*(18)
      DOUBLE PRECISION Stc,tmp,xmin,xmax,dvec
      INTEGER Ib,Ie,Tblptr,iopt,Ktabl,tw2,idate,begtbl,ipos,ntbttl,l,im,
     &        im1,im2,jyr,kyr,nbk,nbk2,i,ldec,ipow,ifmt,nfmt1a,nfmt2a,
     &        npos,nobs,ib1,ie1
      DIMENSION Stc(PLEN),Trnchr(PLEN),tmp(PSP+1),ctmp(PSP+1),dvec(1),
     &          tbsmlb(5),idate(2),begtbl(2)
c-----------------------------------------------------------------------
      DOUBLE PRECISION totals,sdev
      EXTERNAL totals,sdev
c-----------------------------------------------------------------------
c     include files containing DATA statements 
c-----------------------------------------------------------------------
      DATA tbsmlb/'Table Total-        ','Mean-               ',
     &            'Standard Deviation- ','Minimum-            ',
     &            'Maximum-            '/
      DATA cspace/'&nbsp;&nbsp;&nbsp;'/
c-----------------------------------------------------------------------
c        INITIALIZE variables
c-----------------------------------------------------------------------
      ldec=Kdec
      ipow=0
      iopt=0
      DO i=Ib,Ie
       IF(i.eq.Ib)THEN
        xmin=Stc(i)
        xmax=Stc(i)
       ELSE
        IF(xmin.gt.Stc(i))xmin=Stc(i)
        IF(xmax.lt.Stc(i))xmax=Stc(i)
       END IF
      END DO
c-----------------------------------------------------------------------
c     Construct revised format for column headings.
c-----------------------------------------------------------------------
c      tw2=Tblwid+1
      tw2=Tblwid
*      write(*,*) Tblwid
      if(tw2.gt.9)then
       write(fobs,1030)tw2,ldec
 1030  format('(f',i2,'.',i1,')')
       ifmt=7
      else
       write(fobs,1040)tw2,ldec
 1040  format('(f',i1,'.',i1,')')
       ifmt=6
      endif
c     ------------------------------------------------------------------
c     Generate entries for index, skip links
c     ------------------------------------------------------------------
      CALL genSkip(Tblptr)
c----------------------------------------------------------------------
c     Generate headers and subheaders for the table
c-----------------------------------------------------------------------
      CALL getdes(Tblptr,tblttl,ntbttl,T)
      IF(Lfatal)RETURN
      nobs=Ie-Ib+1
      begtbl(YR)=Lyr
      begtbl(MO)=mod(Ib,Ny)
      IF(begtbl(MO).eq.0)begtbl(MO)=Ny
      IF(Ib.gt.Pos1bk)begtbl(YR)=begtbl(YR)+((Ib-1)/Ny)-((Pos1bk-1)/Ny)
      CALL tblhdr(Ktabl,0,Ixreg,nobs,begtbl,Ny,dvec,tblttl,ntbttl)
      IF(Lfatal)RETURN
      CALL mkTableTag(Mt1,'x11',tblttl(1:ntbttl))
      CALL mkCaption(Mt1,tblttl(1:ntbttl))
      IF(Ny.eq.4)THEN
       l=5
      ELSE
       l=13
      END IF
      CALL prtcol(Ny,Mt1,2,'TOTAL','@',Colhdr)
c-----------------------------------------------------------------------
c     print out table
c-----------------------------------------------------------------------
      jyr=Lyr+(Ib-1)/Ny
      kyr=(Ie+Ny-1)/Ny+Lyr-1
*      iin=iin+(jyr-Lyr)
      DO i=1,PSP+1
       tmp(i)=DNOTST
       ctmp(i)='  '
      END DO
      ib1=Ib
      ie1=(jyr-Lyr+1)*Ny
      IF(ie1.gt.Ie)ie1=Ie
      im=Ib-(Ib-1)/Ny*Ny
      DO WHILE (T)
       im1=im
       DO i=ib1,ie1
        tmp(im)=Stc(i)
        ctmp(im)=Trnchr(i)//' '
        im=im+1
       END DO
       im2=im-1
       tmp(l)=totals(tmp,im1,im2,1,0)
c-----------------------------------------------------------------------
c     Compute number of blanks for the beginning or end of the series
c     for observations not in the series.
c-----------------------------------------------------------------------
       nbk=0
       IF(jyr.eq.begtbl(YR).and.begtbl(MO).gt.1)nbk=begtbl(MO)
       nbk2=0
       IF(ie1.eq.Ie)THEN
        CALL addate(begtbl,Ny,nobs-1,idate)
        nbk2=idate(MO)
        IF(nbk2.eq.Ny)nbk2=0
       END IF
c-----------------------------------------------------------------------
c     Write out this year's data.
c-----------------------------------------------------------------------
       CALL wrttb2(tmp,ctmp,jyr,'XXXXX',l,ldec,Mt1,fobs(1:ifmt),Kpart,
     &             Ktabl,ipow)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Update year, starting and ending position of year
c-----------------------------------------------------------------------
       jyr=jyr+1
       im=1
       ib1=ie1+1
       ie1=ie1+Ny
       IF(kyr.eq.jyr)THEN
        DO i=1,Ny
         tmp(i)=DNOTST
        END DO
        ie1=Ie
       ELSE IF(kyr.lt.jyr)THEN
        ie1=Ib+Ny-1
        im=Ib-(Ib-1)/Ny*Ny
        nbk=0
        nbk2=0
        DO i=Ib,ie1
         ctmp(im)='  '
         IF(i.gt.Ie)THEN
          tmp(im)=DNOTST
          IF(i.eq.im)THEN
           IF(nbk2.eq.0)nbk2=im-1
          ELSE
           IF(nbk.eq.0)nbk=1
           nbk=nbk+1
          END IF
         ELSE
          tmp(im)=totals(Stc,i,Ie,Ny,1)
         END IF
         IF(im.eq.Ny)im=0
         im=im+1
        END DO
C --- GENERATE COLUMN SUMMARY FORMATS.
c02      WRITE(MT1,IF2) TYRLY(NOP1),(TMP(I),I = 1,NY)
        CALL wrttb2(tmp,ctmp,jyr,'XXXXX',l,ldec,Mt1,fobs(1:ifmt),Kpart,
     &              Ktabl,ipow)
        IF(Lfatal)RETURN
        CALL writTag(Mt1,'</table>')
        CALL mkPOneLine(Mt1,'@','&nbsp;')
        tmp(1)=totals(Stc,Ib,Ie,1,0)
        tmp(2)=tmp(1)/dble(Ie-Ib+1)
        tmp(3)=sdev(Stc,Ib,Ie,1,iopt)
C --- WRITE TABLE SUMMARY.
        CALL mkPClass(Mt1,'center')
        WRITE(Mt1,Ifmt3)(tbsmlb(i),tmp(i),cspace,i=1,2),
     &                   tbsmlb(3),tmp(3),Cbr,tbsmlb(4),xmin,
     &                   cspace,tbsmlb(5),xmax,'</p>'
        GO TO 10
       END IF
      END DO
c-----------------------------------------------------------------------
*   10 IF(Lwdprt)THEN
*       WRITE(Mt1,1060)PRGNAM
*      ELSE
*       WRITE(Mt1,1070)PRGNAM
*      END IF
   10 WRITE(Mt1,1060)PRGNAM
 1060 FORMAT(/,'<p>- : Trend cycle estimate that had a negative',
     &          ' value replaced by ',a,'.</p>')
c-----------------------------------------------------------------------
      RETURN
      END
