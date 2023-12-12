C     Last change:  BCM  17 Apr 2003   11:24 pm
      SUBROUTINE svf2f3(Nw,Ng,Lf2,Lf3,Arglab)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'x11svl.i'
      INCLUDE 'cmpsvl.i'
      INCLUDE 'inpt2.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'work2.cmn'
      INCLUDE 'tests.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
c     Arglab is i if indirect adjustment, blank otherwise
c-----------------------------------------------------------------------
      CHARACTER Arglab*(*),yn*3,thisM*3
      INTEGER i,n,Nw,Ng,im1
      LOGICAL Lf2,Lf3
c-----------------------------------------------------------------------
      DIMENSION yn(2)
c-----------------------------------------------------------------------
      LOGICAL istrue
      EXTERNAL istrue
c-----------------------------------------------------------------------
      DATA yn/'yes','no '/
c-----------------------------------------------------------------------
c     Print out X-11 diagnostics for F2
c-----------------------------------------------------------------------
      IF(Lf2)THEN
       DO i=1,Ny
        WRITE(Nw,1010)Arglab,i,Obar(i),Cibar(i),Ibar(i),Cbar(i),Sbar(i),
     &                Pbar(i),Tdbar(i),Smbar(i),Ombar(i),Cimbar(i),
     &                Imbar(i)
       END DO
       DO i=1,Ny
        WRITE(Nw,1020)Arglab,i,Isq(i),Csq(i),Ssq(i),Psq(i),Tdsq(i),
     &                Osq2(i)
       END DO
       DO i=1,Ny
        WRITE(Nw,1030)Arglab,i,Obar2(i),Osd(i),Ibar2(i),Isd(i),Cbar2(i),
     &                Csd(i),Sbar2(i),Ssd(i),Cibar2(i),Cisd(i),Smbar2(i)
     &                ,Smsd(i)
       END DO
       WRITE(Nw,1040)Arglab,Adrci,Adri,Adrc,Adrmcd
       WRITE(Nw,1050)Arglab,(Smic(i),i=1,Ny)
       WRITE(Nw,1060)Arglab,Mcd
       WRITE(Nw,1070)Arglab,Vi,Vc,Vs,Vp,Vtd,Rv
       n=Ny+2
       WRITE(Nw,1080)Arglab,(Autoc(i),i=1,n)
       WRITE(Nw,1090)Arglab,Ratic,Arglab,Ratis
       WRITE(Nw,1100)Arglab,Fpres,P3
       WRITE(Nw,1120)Arglab,Fstabl,P1,Arglab,Chikw,P5,Arglab,Fmove,P2
       WRITE(Nw,1130)Arglab,yn(Iqfail)
      END IF
c-----------------------------------------------------------------------
      IF(Arglab(1:1).eq.'i')THEN
       IF(istrue(Svltab,LSLIIR,LSLIID).or.
     &   (Svltab(LSLISR).and.Kfulsm.lt.2))THEN
        Inlgfl=Inlgfl+1
        WRITE(Ng,1000)Inlgfl
        CALL mkTableTag(Ng,'w60',
     &                'X-11 Seasonal Adjustment Diagnostics (indirect)')
        CALL mkCaption(Ng,
     &                'X-11 Seasonal Adjustment Diagnostics (indirect)')
       END IF
       IF(Svltab(LSLISR).and.Kfulsm.lt.2)THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &                 'Moving seasonality ratio (indirect adjustment)')
        WRITE(Ng,2010)Ratis
        CALL writTag(Ng,'</tr>')
       END IF
       IF(Svltab(LSLIIR))THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &                         '<abbr title="I over C">I/C</abbr> '//
     &                         'Ratio (indirect adjustment)')
        WRITE(Ng,2010)Ratic
        CALL writTag(Ng,'</tr>')
       END IF
       IF(Svltab(LSLID8))THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &              'Stable Seasonal F, D8 table (indirect adjustment)')
        WRITE(Ng,2010)Fstabl
        CALL writTag(Ng,'</tr>')
       END IF
       IF(Svltab(LSLISF))THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &              'Moving Seasonal F, D8 table (indirect adjustment)')
        WRITE(Ng,2010)Fmove
        CALL writTag(Ng,'</tr>')
       END IF
       IF(Svltab(LSLIID))THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &                 'Identifiable seasonality (indirect adjustment)')
        CALL mkTableCell(Ng,'@',yn(Iqfail))
        CALL writTag(Ng,'</tr>')
       END IF
       IF(istrue(Svltab,LSLIIR,LSLIID).or.
     &   (Svltab(LSLISR).and.Kfulsm.lt.2))THEN
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
       im1=LSLIM1
      ELSE
       IF(istrue(Svltab,LSLICR,LSLIDS).or.
     &   (Svltab(LSLMSR).and.Kfulsm.lt.2))THEN
        Inlgfl=Inlgfl+1
        WRITE(Ng,1000)Inlgfl
        CALL mkTableTag(Ng,'w60','X-11 Seasonal Adjustment Diagnostics')
        CALL mkCaption(Ng,'X-11 Seasonal Adjustment Diagnostics')
       END IF
       IF(Svltab(LSLMSR).and.Kfulsm.lt.2)THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &                         'Moving seasonality ratio')
        WRITE(Ng,2010)Ratis
        CALL writTag(Ng,'</tr>')
       END IF
       IF(Svltab(LSLICR))THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &                        '<abbr title="I over C">I/C</abbr> Ratio')
        WRITE(Ng,2010)Ratic
        CALL writTag(Ng,'</tr>')
       END IF
       IF(Svltab(LSLFB1))THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &                         'Stable Seasonal F, B1 table')
        WRITE(Ng,2010)Fpres
        CALL writTag(Ng,'</tr>')
       END IF
       IF(Svltab(LSLFD8))THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &                         'Stable Seasonal F, D8 table')
        WRITE(Ng,2010)Fstabl
        CALL writTag(Ng,'</tr>')
       END IF
       IF(Svltab(LSLMSF))THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &                         'Moving Seasonal F, D8 table')
        WRITE(Ng,2010)Fmove
        CALL writTag(Ng,'</tr>')
       END IF
       IF(Svltab(LSLIDS))THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &                         'Identifiable seasonality')
        CALL mkTableCell(Ng,'@',yn(Iqfail))
        CALL writTag(Ng,'</tr>')
       END IF
       IF(istrue(Svltab,LSLICR,LSLIDS).or.
     &   (Svltab(LSLMSR).and.Kfulsm.lt.2))THEN
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
       im1=LSLM1
      END IF
 2010 FORMAT('<td>',f11.3,'</td>')
c-----------------------------------------------------------------------
c     Print out Quality control diagnostics for F3
c-----------------------------------------------------------------------
      IF(istrue(Svltab,im1,im1+12))THEN
       Inlgfl=Inlgfl+1
       WRITE(Ng,1000)Inlgfl
       IF(Arglab(1:1).eq.'i')THEN
        CALL mkTableTag(Ng,'w60',
     &                 'M and Q Quality Control Diagnostics (indirect)')
        CALL mkCaption(Ng,
     &                 'M and Q Quality Control Diagnostics (indirect)')
       ELSE
        CALL mkTableTag(Ng,'w60','M and Q Quality Control Diagnostics')
        CALL mkCaption(Ng,'M and Q Quality Control Diagnostics')
       END IF
      END IF
      DO i=1,Nn
       IF(i.ne.6.or.Kfulsm.lt.2)THEN
        IF(Lf3)WRITE(Nw,1140)Arglab,i,Qu(i)
        IF(Svltab(im1+i-1))THEN
         WRITE(thisM,1160)i
         CALL mkF3Row(Ng,thisM,Qu(i))
        END IF
       END IF
      END DO
      IF(Lf3)THEN
       WRITE(Nw,1150)Arglab,Qual,Arglab,Q2m2,Arglab,Kfail
      END IF
      IF(Svltab(im1+11))CALL mkF3Row(Ng,' Q ',Qual)
      IF(Svltab(im1+12))CALL mkF3Row(Ng,' Q2 ',Q2m2)
      IF(istrue(Svltab,im1,im1+12))THEN
       CALL writTag(Ng,'</table></div>')
       CALL mkPOneLine(Ng,'@','&nbsp;')
      END IF
      RETURN
c     ------------------------------------------------------------------
 1000 FORMAT('<div id="lgf3',i6.6,'">')
 1010 FORMAT(a,'2.a',i2.2,':',1x,E15.8,10(1X,E15.8))
 1020 FORMAT(a,'2.b',i2.2,':',1x,5(2PF8.2),'  100.00',2PF8.2)
 1030 FORMAT(a,'2.c',i2.2,':',12(1x,E15.8))
 1040 FORMAT(a,'2.d:',4F8.2)
 1050 FORMAT(a,'2.e:',12F8.2)
 1060 FORMAT(a,'2.mcd:',i8)
 1070 FORMAT(a,'2.f:',6F8.2)
 1080 FORMAT(a,'2.g:',14F8.2)
 1090 FORMAT(a,'2.ic:',F12.2,/,a,'2.is:',F12.2)
 1100 FORMAT(a,'2.fsb1:',F11.3,F8.2)
 1120 FORMAT(a,'2.fsd8:',F11.3,F8.2,/,a,'2.kw:',F11.3,F8.2,/,
     &       a,'2.msf:',F11.3,F8.2)
 1130 FORMAT(a,'2.idseasonal: ',a)
 1140 FORMAT(a,'3.m',i2.2,':',1x,f6.3)
 1150 FORMAT(a,'3.q:',1x,F5.2,/,a,'3.qm2:',1x,F5.2,/,a,'3.fail:',
     &       1x,i2)
 1160 FORMAT('M',i2.2)
c     ------------------------------------------------------------------
      END
c     ------------------------------------------------------------------
      SUBROUTINE mkF3Row(Fh,thisHeader,thisReal)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      DOUBLE PRECISION thisReal
      CHARACTER thisHeader*(*)
      INTEGER Fh
c     ------------------------------------------------------------------
      CALL writTag(Fh,'<tr>')
      CALL mkHeaderCellScope(Fh,0,0,'row','@',thisHeader)
      WRITE(Fh,1010)thisReal
      CALL writTag(Fh,'</tr>')
c     ------------------------------------------------------------------
 1010 FORMAT('<td class="center">',f10.4,'</td>')
c     ------------------------------------------------------------------
      END
c     ------------------------------------------------------------------
