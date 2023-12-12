C     Last change:  BCM  29 Aug 2005 10:02 am
**==savacf.f    processed by SPAG 4.03F  at 10:31 on 29 Jul 1994
      SUBROUTINE acfdgn(Nefobs,A,Na,Mxlag,Nlagbl,Ldiag)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     acfdgn() saves diagnostics related to the residual autocorrelation
c     function to the log file and to the unified diagnostics file.
c     Written by BCM July 2007
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'mdlsvl.i'
      INCLUDE 'error.cmn'
      INCLUDE 'htmlout.cmn'
c     ------------------------------------------------------------------
      LOGICAL F,T
      INTEGER PR
      PARAMETER(F=.FALSE.,T=.true.,PR=PLEN/4)
c-----------------------------------------------------------------------
      CHARACTER outstr*(PR*3)
      DOUBLE PRECISION A(PLEN),smpac,seacf,tacflg
      INTEGER Nlagbl,i1,i2,i3,ilag,i,np,endlag,Nefobs,Na,Mxlag,ipos
      LOGICAL Ldiag
      DIMENSION smpac(PR),seacf(PR)
c-----------------------------------------------------------------------
      INCLUDE 'autoq.cmn'
c-----------------------------------------------------------------------
c     Compute residual ACF
c-----------------------------------------------------------------------
      IF(Mxlag.eq.0)THEN
       IF(Sp.eq.1)THEN
        Mxlag=10
       ELSE
        Mxlag=2*Sp
       END IF
       Mxlag=min(Mxlag,Nefobs/4)
      ELSE
c     ------------------------------------------------------------------
       Mxlag=min(Mxlag,Nefobs-1)
      END IF
c     ------------------------------------------------------------------
      np=0
      endlag=Opr(Nopr)-1
      DO ilag=1,endlag
       IF(.not.Arimaf(ilag))np=np+1
      END DO
c     ------------------------------------------------------------------
      CALL acf(A(Na-Nefobs+1),Nefobs,Nefobs,smpac,seacf,Mxlag,np,Sp,
     &         0,T,F,F)
c     ------------------------------------------------------------------
      i1=0
      i2=0
      IF(Ldiag.or.Svltab(LSLLBQ))THEN
       DO i=1,Nlagbl
        IF(Dgf(i).gt.0.and.Qpv(i).lt.Qcheck)i1=i1+1
        IF(dabs(smpac(i)/seacf(i)).gt.Acflim)i2=i2+1
       END DO
      END IF
c     ------------------------------------------------------------------
c     Print ACF seasonal lag info to log file
c     ------------------------------------------------------------------
      IF(Svltab(LSLSAC).and.(Sp.eq.4.or.Sp.eq.12))THEN
       Inlgfl=Inlgfl+1
       WRITE(Ng,1000)Inlgfl
       CALL mkTableTag(Ng,'w60','Summary of Seasonal ACF Lags')
       CALL mkCaption(Ng,'Summary of Seasonal ACF Lags')
       CALL writTag(Ng,'<tr>')
       CALL mkTableCell(Ng,'head','&nbsp;')
       CALL mkHeaderCellScope(Ng,0,0,'col','Autocorrelation Function',
     &                        'ACF')
       CALL mkHeaderCellScope(Ng,0,0,'col','Standard Error','se')
       CALL mkHeaderCellScope(Ng,0,0,'col','t-statistic','t-stat')
       CALL mkHeaderCellScope(Ng,0,0,'col','Ljung-Box Q','Q')
       CALL mkHeaderCellScope(Ng,0,0,'col','Degrees of Freedom','df')
       CALL mkHeaderCellScope(Ng,0,0,'col','@','p-value')
       CALL writTag(Ng,'</tr>')
       i=1
       ilag=Sp*1
       DO WHILE (ilag.le.Mxlag)
        tacflg=smpac(ilag)/seacf(ilag)
        CALL writTag(Ng,'<tr>')
        WRITE(Ng,1020)ilag
        WRITE(Ng,1030)smpac(ilag),seacf(ilag),tacflg
        WRITE(Ng,1130)Qs(ilag),Dgf(ilag),Qpv(ilag)
        CALL writTag(Ng,'</tr>')
        i=i+1
        ilag=Sp*i
       END DO
       CALL writTag(Ng,'</table></div>')
       CALL mkPOneLine(Ng,'@','&nbsp;')
      END IF
c     ------------------------------------------------------------------
c     Print ACF seasonal lag info to udg file
c     ------------------------------------------------------------------
      IF(Ldiag.and.(Sp.eq.4.or.Sp.eq.12))THEN
       i=i+1
       ilag=Sp*i
       DO WHILE (ilag.le.Mxlag)
        tacflg=smpac(ilag)/seacf(ilag)
        WRITE(Nform,1200)'acf',ilag,smpac(ilag),seacf(ilag),tacflg,
     &                    Qs(ilag),Dgf(ilag),Qpv(ilag)
        i=i+1
        ilag=Sp*i
       END DO
      END IF
c     ------------------------------------------------------------------
c     Print LB-Q information in log file
c     ------------------------------------------------------------------
      IF(Svltab(LSLLBQ))THEN
       IF(i1.eq.0)THEN
        CALL mkPOneLine(Ng,'center',
     &                  '<strong>No significant Ljung-Box Qs</strong>')
       ELSE
        Inlgfl=Inlgfl+1
        WRITE(Ng,1000)Inlgfl
        CALL mkTableTag(Ng,'w60','Summary of Significant Ljung-Box Q')
        CALL mkCaption(Ng,'Summary of Significant Ljung-Box Q')
        CALL writTag(Ng,'<tr>')
        CALL mkTableCell(Ng,'head','&nbsp;')
        CALL mkHeaderCellScope(Ng,0,0,'col','Ljung-Box Q','Q')
        CALL mkHeaderCellScope(Ng,0,0,'col','Degrees of Freedom','df')
        CALL mkHeaderCellScope(Ng,0,0,'col','@','p-value')
        CALL writTag(Ng,'</tr>')
        DO i=1,Nlagbl
         IF(Dgf(i).gt.0.and.Qpv(i).lt.Qcheck)THEN
          CALL writTag(Ng,'<tr>')
          WRITE(Ng,1020)i
          WRITE(Ng,1130)Qs(i),Dgf(i),Qpv(i)
          CALL writTag(Ng,'</tr>')
         END IF
        END DO
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
      END IF
c     ------------------------------------------------------------------
c     Print LB-Q information in udg file
c     ------------------------------------------------------------------
      IF(Ldiag)THEN
       WRITE(Nform,1110)'qlimit',Qcheck
       WRITE(Nform,1160)'lb',i1
       outstr=' '
       ipos=1
       IF(i1.gt.0)THEN
        DO i=1,Nlagbl
         IF(Dgf(i).gt.0.and.Qpv(i).lt.Qcheck)THEN
          WRITE(Nform,1150)'lb',i,Qs(i),Dgf(i),Qpv(i)
          call itoc(i,outstr,ipos)
          IF(Lfatal)RETURN
          outstr(ipos:ipos)=' '
          ipos=ipos+1
         END IF
        END DO
        write(Nform,1190)'lb',outstr(1:(ipos-1))
       ELSE
        write(Nform,1190)'lb','0'
       END IF
      END IF
c     ------------------------------------------------------------------
c     Now produce BP-Q information
c     ------------------------------------------------------------------
      i3=0
      IF(Ldiag.or.Svltab(LSLLBQ+1))THEN
       CALL acf(A(Na-Nefobs+1),Nefobs,Nefobs,smpac,seacf,Mxlag,np,Sp,
     +         1,T,F,F)
c     ------------------------------------------------------------------
       DO i=1,Nlagbl
        IF(Dgf(i).gt.0.and.Qpv(i).lt.Qcheck)i3=i3+1
       END DO
      END IF
c     ------------------------------------------------------------------
c     Print BP-Q information in log file
c     ------------------------------------------------------------------
      IF(Svltab(LSLLBQ+1))THEN
*       IF(Laccss)CALL insmrk(Ng,LSLLBQ,T,T)    
       IF(i3.eq.0)THEN
        CALL mkPOneLine(Ng,'center',
     &                  '<strong>No significant Box-Pierce Qs</strong>')
       ELSE
        Inlgfl=Inlgfl+1
        WRITE(Ng,1000)Inlgfl
        CALL mkTableTag(Ng,'w60','Summary of Significant Box-Pierce Q')
        CALL mkCaption(Ng,'Summary of Significant Box-Pierce Q')
        CALL writTag(Ng,'<tr>')
        CALL mkTableCell(Ng,'head','&nbsp;')
        CALL mkHeaderCellScope(Ng,0,0,'col','Box-Pierce Q','Q')
        CALL mkHeaderCellScope(Ng,0,0,'col','Degrees of Freedom','df')
        CALL mkHeaderCellScope(Ng,0,0,'col','@','p-value')
        CALL writTag(Ng,'</tr>')
        DO i=1,Nlagbl
         IF(Dgf(i).gt.0.and.Qpv(i).lt.Qcheck)THEN
          CALL writTag(Ng,'<tr>')
          WRITE(Ng,1020)i
          WRITE(Ng,1130)Qs(i),Dgf(i),Qpv(i)
          CALL writTag(Ng,'</tr>')
         END IF
        END DO
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
*       IF(Laccss)CALL insmrk(Ng,LSLLBQ,F,T)    
      END IF
c     ------------------------------------------------------------------
c     Print BP-Q information in udg file
c     ------------------------------------------------------------------
      IF(Ldiag)THEN
       WRITE(Nform,1160)'bp',i3
       outstr=' '
       ipos=1
       IF(i3.gt.0)THEN
        DO i=1,Nlagbl
         IF(Dgf(i).gt.0.and.Qpv(i).lt.Qcheck)THEN
          WRITE(Nform,1150)'bp',i,Qs(i),Dgf(i),Qpv(i)
          call itoc(i,outstr,ipos)
          IF(Lfatal)RETURN
          outstr(ipos:ipos)=' '
          ipos=ipos+1
         END IF
        END DO
        write(Nform,1190)'bp',outstr(1:(ipos-1))
       ELSE
        write(Nform,1190)'bp','0'
       END IF
c     ------------------------------------------------------------------
c     Print significant ACF lags in udg file
c     ------------------------------------------------------------------
       WRITE(Nform,1110)'acflimit',Acflim
       WRITE(Nform,1170)'acf',i2
       outstr=' '
       ipos=1
       IF(i2.gt.0)THEN
        DO i=1,Nlagbl
         tacflg=smpac(i)/seacf(i)
         IF(dabs(tacflg).gt.Acflim)THEN
          WRITE(Nform,1180)'sigacf',i,smpac(i),seacf(i),tacflg
          call itoc(i,outstr,ipos)
          IF(Lfatal)RETURN
          outstr(ipos:ipos)=' '
          ipos=ipos+1
         END IF
        END DO
        write(Nform,1190)'sigacf',outstr(1:(ipos-1))
       ELSE
        write(Nform,1190)'sigacf','0'
       END IF
      END IF
c     ------------------------------------------------------------------
c     Print significant PACF lags in udg file
c     ------------------------------------------------------------------
      IF(Ldiag)THEN
       CALL pacf(Nefobs,Sp,smpac,seacf,Mxlag,F)
       i2=0
       DO i=1,Nlagbl
        IF(dabs(smpac(i)/seacf(i)).gt.Acflim)i2=i2+1
       END DO
       WRITE(Nform,1170)'pacf',i2
       outstr=' '
       ipos=1
       IF(i2.gt.0)THEN
        DO i=1,Nlagbl
         tacflg=smpac(i)/seacf(i)
         IF(dabs(tacflg).gt.Acflim)THEN
          WRITE(Nform,1180)'sigpacf',i,smpac(i),seacf(i),tacflg
          call itoc(i,outstr,ipos)
          IF(Lfatal)RETURN
          outstr(ipos:ipos)=' '
          ipos=ipos+1
         END IF
        END DO
        write(Nform,1190)'sigpacf',outstr(1:(ipos-1))
       ELSE
        write(Nform,1190)'sigpacf','0'
       END IF
      END IF
c     ------------------------------------------------------------------
 1000 FORMAT('<div id="lgacf',i6.6,'">')
 1110 FORMAT(a,': ',f7.4)
 1020 FORMAT('<th scope="row"> Lag ',I6,'</th>')
 1030 FORMAT('<td class="center">',f7.3,'</td><td class="center">',
     &       f7.3,'</td><td class="center">',f6.3,'</td>')
 1130 FORMAT('<td class="center">',f7.3,'</td><td class="center">',i3,
     &       '</td><td class="center">',f6.3,'</td>')
 1150 FORMAT(a,'q$',i2.2,': ',f7.3,5x,i3,5x,f6.3)
 1160 FORMAT('n',a,'q: ',i3)
 1170 FORMAT('nsig',a,': ',i3)
 1180 FORMAT(a,'$',i2.2,': ',f7.4,5x,f7.4,3x,f7.4)
 1190 FORMAT(a,'lags: ',a)
 1200 FORMAT(a,'$',i2.2,': ',f7.4,5x,f7.4,3x,f7.4,3x,f7.3,5x,i3,5x,f6.3)
c-----------------------------------------------------------------------
      RETURN
      END
