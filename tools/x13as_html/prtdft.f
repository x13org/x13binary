      SUBROUTINE prtdft(Lprsft,Lprhdr,Tbwdth,Lsvsft,Lsvlog,Baselt,
     &                  Grpstr,Nchr,Info,Df1,Df2,Sftvl,Pv)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      LOGICAL F
      PARAMETER(F=.false.)
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'title.cmn'
      INCLUDE 'htmlout.cmn'
      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
      CHARACTER Grpstr*(PGRPCR)
      LOGICAL Lprsft,Lprhdr,Lsvsft,Lsvlog
      INTEGER Tbwdth,Baselt,Nchr,Info,Df1,Df2,i
      DOUBLE PRECISION Sftvl,Pv
c-----------------------------------------------------------------------
      IF(Lprhdr)THEN
       IF(.not.Lcmpaq)CALL mkPOneLine(Mt1,'@','&nbsp;')
       Inmd=Inmd+1
       WRITE(Mt1,1010)Inmd
       CALL writTagOneLine(Mt1,'h3','@',
     &                     'F Tests for Trading Day Regressors')
       CALL mkTableTag(Mt1,'w70','@')
       CALL mkCaption(Mt1,'w70','F Tests for Trading Day Regressors')
       CALL writTag(Mt1,'<tr>')
       CALL mkTableCell(Mt1,'@','&nbsp;')
       CALL mkHeaderCellScope(Mt1,0,0,'col','Degrees of Freedom','df')
       CALL mkHeaderCellScope(Mt1,0,0,'col','@','F-statistic')
       CALL mkHeaderCellScope(Mt1,0,0,'col','@','P-Value')
       CALL writTag(Mt1,'</tr>')
       IF(Lsvlog)THEN
        Inlgfl=Inlgfl+1
        WRITE(Ng,1020)Inlgfl
        CALL writTagOneLine(Ng,'h3','@',
     &                     'F Tests for Trading Day Regressors:')
        CALL mkTableTag(Ng,'w70','@')
        CALL mkCaption(Ng,'w70','F Tests for Trading Day Regressors')
        CALL writTag(Ng,'<tr>')
        CALL writTagOneLine(Ng,'td','@','&nbsp;')
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
 1010 FORMAT('<div id="mdft',i3.3,'">')
 1020 FORMAT('<div id="lgft',i6.6,'">')
 1040 FORMAT('tdtest$',a,': ',2(1x,i4),2(1x,e22.15))
 1060 FORMAT('<td>',i4,',',i4,'</td><td>',f16.2,'</td><td>',f13.2,
     &       '</td>')
c-----------------------------------------------------------------------
        END
