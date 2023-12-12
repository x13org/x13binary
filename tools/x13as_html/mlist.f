C     Last change:  BCM  26 Feb 1999    3:40 pm
**==mlist.f    processed by SPAG 4.03F  at 12:23 on 21 Jun 1994
      SUBROUTINE mlist(X,Nopt,Nop2,Dmax,N48,Iagr,Ext,Eststr,Nstr,Ncol,Y,
     &                 Period,Ssdiff)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C  *****  PRINTS OUT EACH OBSERVATION IN SLIDING SPANS ANALYSIS, WITH
C  *****  DATE, ESTIMATES (EXAMPLE, SEASONAL FACTORS) FOR EACH SPAN,
C  *****  MAXIMUM PERCENTAGE DIFFERENCE (DMAX), AND AN INDICATION OF
C  *****  WHETHER THE OBSERVATION WAS FLAGGED AS AN EXTREME (PER)
C  *****  OR CHANGED DIRECTION (CSIGN).
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'ssap.prm'
      INCLUDE 'ssap.cmn'
      INCLUDE 'sspvec.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'tfmts.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION Dmax,X
      LOGICAL Ssdiff,l2Big
      CHARACTER cagr*(31),dash*(1),Eststr*(45),Ext*(2),f*(10),
     &          blank8*(8),cfirst*(11),thisOb*(30),fmt1*(7)
      INTEGER i,Iagr,iy,l,l0,l1,l2,Nstr,m,N48,Nop2,Nopt,Y,Period,nagr,
     &        Ncol,nfirst,nc,nt,fnotky,nssky,iout
      DIMENSION dash(2),X(MXLEN,MXCOL),Dmax(MXLEN,NEST),f(3),Y(2*MXCOL),
     &          Period(2*MXCOL),nfirst(2),cfirst(2),fnotky(7)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      INTEGER nblank
      EXTERNAL dpeq,nblank
c-----------------------------------------------------------------------
      DATA dash/'-','/'/
      DATA f/'MAXIMUM   ','PERCENT   ','DIFFERENCE'/
      DATA cfirst/'seasonal','trading day'/
      DATA nfirst/8,11/
c-----------------------------------------------------------------------
      iy=Iyr
      m=Im-1
      IF(Iagr.eq.5)THEN
       cagr=': Direct seasonal adjustment.'
       nagr=29
      ELSE IF(Iagr.eq.6)THEN
       cagr=': Indirect seasonal adjustment.'
       nagr=31
      ELSE
       cagr='.'
       nagr=1
      END IF
      blank8='        '
      iout=Kdec
      IF(iout.lt.2)iout=2
      write(fmt1,1010)Tblwid,iout
 1010 format('(f',i2,'.',i1,')')
c-----------------------------------------------------------------------
c     Initialize Fnotky to NOTSET
c-----------------------------------------------------------------------
      CALL setint(NOTSET,7,fnotky)
      nssky=0
c-----------------------------------------------------------------------
c  Check to see if series is too large to be printed - if so,
c  switch to scientific format.
c  added by BCM Dec 2005
c-----------------------------------------------------------------------
*      l2Big=.false.
*      IF(Nopt.ge.3.or.Ssdiff)THEN
*       DO l1=1,Ncol
*        DO l2=Im,Sslen+Im-1
*         IF(.not.dpeq(X(l2,l1),DNOTST))THEN
*          IF(X(l2,l1).gt.999999.99 .or. X(l2,l1).lt.-99999.99)
*     &       l2Big=.true.
*          IF(l2Big)GO TO 1000
*         END IF
*        END DO
*       END DO
*      END IF
* 1000 CONTINUE
c-----------------------------------------------------------------------
c     Print out complete sliding spans information, with up to 48
c     observations on a page.
c-----------------------------------------------------------------------
      CALL writTagOneLine(Mt1,'h3','@',
     &                    'S 7.'//Ext//' Sliding spans analysis of '//
     &                    Eststr(1:Nstr)//' for '//Serno(1:Nser)//
     &                    cagr(1:nagr))
      CALL mkTableTag(Mt1,'w90','Sliding spans analysis of '//
     &                    Eststr(1:Nstr)//' for '//Serno(1:Nser)//
     &                    cagr(1:nagr))
      CALL mkCaption(Mt1,'Table S 7.'//Ext)

      CALL writTag(Mt1,'<tr>')
      CALL mkTableCell(Mt1,'head','&nbsp;')
      DO i=1,Ncol
       WRITE(Mt1,1020)Period(i),dash(2),Y(i),dash(1),Cbr,
     &                Period(Ncol+i),dash(2),Y(Ncol+i)
 1020  FORMAT('<th scope="col">',i4,a,i4,1x,a,a,i4,a,i4,'</th>')
      END DO
      IF(Nop2.eq.0.AND.(.not.Ssdiff))THEN
       CALL mkHeaderCellScope(Mt1,0,0,'col','@',
     &                        f(1)//Cbr//f(2)//Cbr//f(3))
      ELSE
       CALL mkHeaderCellScope(Mt1,0,0,'col','@',f(1)//Cbr//f(3))
      END IF
      CALL mkHeaderCellScope(Mt1,0,0,'col','@','Footnote')
      CALL writTag(Mt1,'</tr>')
      
      DO l0=Im,Im+Sslen-1
*       l1=(l0-1)*48+Im
*       l2=l0*48+Im-1
*       IF(l0.eq.N48)l2=Sslen+Im-1
*       IF(Lpage)THEN
*        WRITE(Mt1,Ttlfmt)Newpg,Title(1:Ntitle),Kpage,Serno(1:Nser)
*        Kpage=Kpage+1
*       END IF
       m=m+1
       IF(m.gt.Nsea)THEN
        m=1
        iy=iy+1
       END IF
       CALL writTag(Mt1,'<tr>')
       WRITE(Mt1,1030)m,'-',iy
 1030  FORMAT('<th scope="row">',i4,a,i4,'</th>')
       DO l=1,Ncol
        IF(.not.dpeq(X(l0,l),DNOTST))THEN
         thisOb=' '
         write(thisOb,fmt1)X(l0,l)
         IF(X(l0,l).lt.0D0)THEN
          CALL mkTableCell(Mt1,'nowrap',thisOb(1:nblank(thisOb)))
         ELSE
          CALL mkTableCell(Mt1,'@',thisOb(1:nblank(thisOb)))
         END IF
        ELSE
         CALL mkTableCell(Mt1,'@','&nbsp;')
        END IF
       END DO
       thisOb=' '
       IF(.not.dpeq(Dmax(l0,Nopt),DNOTST))THEN
        write(thisOb,fmt1)Dmax(l0,Nopt)
        IF(Dmax(l0,Nopt).lt.0D0)THEN
         CALL mkTableCell(Mt1,'nowrap',thisOb(1:nblank(thisOb)))
        ELSE
         CALL mkTableCell(Mt1,'@',thisOb(1:nblank(thisOb)))
        END IF
       ELSE
        CALL mkTableCell(Mt1,'@','&nbsp;')
       END IF
c-----------------------------------------------------------------------
c     Generate footnotes for table (BCM, December 2006, rev July 2011)
c-----------------------------------------------------------------------
       CALL ssfnot(Mt1,l0,Nopt,Nop2,fnotky,nssky)
       CALL writTag(Mt1,'</tr>')
      END DO
      CALL writTag(Mt1,'</table>')
      CALL mkPOneLine(Mt1,'@','&nbsp;')
c-----------------------------------------------------------------------
      IF(nssky.gt.0)THEN
c-----------------------------------------------------------------------
c  Print header for footnotes on separate page
c-----------------------------------------------------------------------
*       IF(Lpage)THEN
*        WRITE(Mt1,Ttlfmt)Newpg,Title(1:Ntitle),Kpage,Serno(1:Nser)
*        Kpage=Kpage+1
*       END IF
       CALL writTagOneLine(Mt1,'h3','@',
     &                     'Footnotes for Table S7.'//Ext//':'//
     &                     Cbr//' Sliding spans analysis of '//
     &                     Eststr(1:Nstr)//' for '//Serno(1:Nser)//
     &                     cagr(1:nagr))
       CALL mkTableTag(Mt1,'w70','@')
       CALL mkCaption(Mt1,'Footnotes for Table S 7.'//Ext)
       CALL mkssky(fnotky,nssky,Nopt,Nop2)
      END IF
c-----------------------------------------------------------------------
      RETURN
      END

