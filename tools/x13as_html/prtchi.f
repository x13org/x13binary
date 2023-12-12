      SUBROUTINE prtchi(Fh,Lprhdr,Tbwdth,Baselt,Grpstr,Nchr,Info,Df,
     &                  Chi2vl,Pv,Hdrstr,Nhdr,Lsvlch)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'title.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      CHARACTER Grpstr*(PGRPCR),Hdrstr*(31)
      LOGICAL Lprhdr,Lsvlch
      INTEGER Fh,Tbwdth,Baselt,Nchr,Info,Df,i,Nhdr
      DOUBLE PRECISION Chi2vl,Pv
c-----------------------------------------------------------------------
      IF(Lprhdr)THEN
       IF(.not.Lcmpaq)CALL mkPOneLine(Fh,'@','&nbsp;')
       IF(.not.Lsvlch)CALL genSkip(1501)
*       WRITE(Fh,1010)Hdrstr
* 1010  FORMAT(/,' Chi-squared Tests for Groups of ',a)
*       WRITE(Fh,1020)('-',i=1,tbwdth)
*       WRITE(Fh,1030)
* 1030  FORMAT(' Regression Effect',t37,'df',t45,'Chi-Square',t61,
*     &        'P-Value')
*       WRITE(Fh,1020)('-',i=1,tbwdth)
       IF(Lsvlch)THEN
        WRITE(Fh,1000)'mdl',Inpmdl,'.chi'
       ELSE
        Inlgfl=Inlgfl+1
        WRITE(Fh,1000)'lgmdl',Inlgfl,'.chi'
       END IF
       CALL mkTableTag(Fh,'w70',
     &               'Chi-squared Tests for Groups of '//Hdrstr(1:Nhdr))
       CALL mkCaption(Fh,
     &               'Chi-squared Tests for Groups of '//Hdrstr(1:Nhdr))
       CALL writTag(Fh,'<tr>')
       CALL mkTableCell(Fh,'head','&nbsp;')
       CALL mkHeaderCellScope(Fh,0,0,'col','Degrees of Freedom','df')
       CALL mkHeaderCellScope(Fh,0,0,'col','@','Chi-square')
       CALL mkHeaderCellScope(Fh,0,0,'col','@','p-value')
       CALL writTag(Fh,'</tr>')
      END IF
c-----------------------------------------------------------------------
      CALL writTag(Fh,'<tr>')
      CALL mkHeaderCellScope(Fh,0,0,'col','@',Grpstr(1:Nchr))
      IF(Info.eq.0)THEN
       IF(Baselt.eq.NOTSET)THEN
        CALL mkTableCellSpan(Fh,'col',3,'center',
     &                       'All coefficients fixed')
       ELSE
        WRITE(Fh,1060)Df,Chi2vl,Pv
       END IF
c-----------------------------------------------------------------------
      ELSE
       CALL mkTableCellSpan(Fh,'col',3,'center','Not tested')
      END IF
      CALL writTag(Fh,'</tr>')
c-----------------------------------------------------------------------
      RETURN
c-----------------------------------------------------------------------
 1000 FORMAT('<div id="',a,i3.3,a,'">')
 1060 FORMAT('<td class="center">',i4,'</td><td class="center">',f16.2,
     &       '</td><td class="center">',f13.2,'</td>')
c-----------------------------------------------------------------------
      END
