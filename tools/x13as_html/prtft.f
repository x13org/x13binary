      SUBROUTINE prtft(Lprsft,Lprhdr,Tbwdth,Lsvsft,Lsvlog,Baselt,
     &                 Grpstr,Nchr,Tsttyp,Ntyp,Info,Df1,Df2,Sftvl,Pv,
     &                 Tbcode)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      LOGICAL F
      PARAMETER(F=.false.)
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'title.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      CHARACTER Tsttyp*(11),Grpstr*(PGRPCR)
      LOGICAL Lprsft,Lprhdr,Lsvsft,Lsvlog
      INTEGER Tbwdth,Baselt,Nchr,Info,Df1,Df2,i,Ntyp,Tbcode
      DOUBLE PRECISION Sftvl,Pv
c-----------------------------------------------------------------------
      IF(Lprhdr.and.(.not.Lnoprt))THEN
       IF(.not.Lcmpaq)CALL mkPOneLine(Mt1,'@','&nbsp;')
       CALL genSkip(Tbcode)
       IF(Tbcode.eq.1070)THEN
        WRITE(Mt1,1000)Inpmdl,'.sft'
       ELSE
        WRITE(Mt1,1000)Inpmdl,'.tdft'
       END IF
       CALL mkTableTag(Mt1,'w70','@')
       CALL mkCaption(Mt1,'F Tests for '//Tsttyp(1:Ntyp)//' Regressors')
       CALL writTag(Mt1,'<tr>')
       CALL mkTableCell(Mt1,'head','&nbsp;')
       CALL mkHeaderCellScope(Mt1,0,0,'col','Degrees of Freedom','df')
       CALL mkHeaderCellScope(Mt1,0,0,'col','@','F-statistic')
       CALL mkHeaderCellScope(Mt1,0,0,'col','@','P-Value')
       CALL writTag(Mt1,'</tr>')
       IF(Lsvlog)THEN
        Inlgfl=Inlgfl+1
        IF(Tbcode.eq.1070)THEN
         WRITE(Ng,1001)Inlgfl,'.sft'
        ELSE
         WRITE(Ng,1001)Inlgfl,'.tdft'
        END IF
        CALL mkTableTag(Ng,'w60','@')
        CALL mkCaption(Ng,'F Tests for '//Tsttyp(1:Ntyp)//' Regressors')
        CALL writTag(Ng,'<tr>')
        CALL mkTableCell(Ng,'head','&nbsp;')
        CALL mkHeaderCellScope(Ng,0,0,'col','Degrees of Freedom','df')
        CALL mkHeaderCellScope(Ng,0,0,'col','@','F-statistic')
        CALL mkHeaderCellScope(Ng,0,0,'col','@','P-Value')
        CALL writTag(Ng,'</tr>')
       END IF
       Lprhdr=F
      END IF
c-----------------------------------------------------------------------
      IF(Lsvsft.and.baselt.ne.NOTSET)
     &   WRITE(Nform,1040)Grpstr(1:Nchr),Df1,Df2,Sftvl,Pv
c-----------------------------------------------------------------------
      IF(Lprsft)THEN
       CALL writTag(Mt1,'<tr>')
       CALL mkHeaderCellScope(Mt1,0,0,'row','@',Grpstr(1:Nchr))
       IF(Lsvlog)THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@',Grpstr(1:Nchr))
       END IF
       IF(Info.eq.0)THEN
        IF(Baselt.eq.NOTSET)THEN
         CALL mkTableCellSpan(Mt1,'col',3,'center',
     &                        'All coefficients fixed')
         IF(Lsvlog)
     &      CALL mkTableCellSpan(Ng,'col',3,'center',
     &                           'All coefficients fixed')
        ELSE
         WRITE(Mt1,1060)Df1,Df2,Sftvl,Pv
         IF(Lsvlog)WRITE(Ng,1060)Df1,Df2,Sftvl,Pv
        END IF
c-----------------------------------------------------------------------
       ELSE
        CALL mkTableCellSpan(Mt1,'col',3,'center','Not tested')
        IF(Lsvlog)
     &     CALL mkTableCellSpan(Ng,'col',3,'center','Not tested')
       END IF
       CALL writTag(Mt1,'</tr>')
       IF(Lsvlog)CALL writTag(Ng,'</tr>')
      END IF
c-----------------------------------------------------------------------
      RETURN
c-----------------------------------------------------------------------
 1000 FORMAT('<div id="mdl',i3.3,a,'">')
 1001 FORMAT('<div id="lgmdl',i6.6,a,'">')
 1040 FORMAT('ftest$',a,': ',2(1x,i4),2(1x,e22.15))
 1060 FORMAT('<td class="center">',i4,',',i4,'</td><td class="center">',
     &       f16.2,'</td><td class="center">',f13.2,'</td>')
c-----------------------------------------------------------------------
        END
