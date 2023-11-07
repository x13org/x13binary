C     Last change:  BCM  25 Nov 1998   12:42 pm
**==prtfct.f    processed by SPAG 4.03F  at 10:36 on 16 Nov 1994
      SUBROUTINE prtfct(Nobspf,Nrxy,Fcntyp,Lam,Lognrm,Fctdrp,Nfcst,
     &                  Ciprob,Fcstx,Untfct,Outdec,Pos2,Lgraf,Svdiag,
     &                  Lseats,Khol,Kswv)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Calculate and print the forecast
c-----------------------------------------------------------------------
      LOGICAL T,F
      DOUBLE PRECISION ZERO,ONE
      INTEGER ADD,BTWNCL,DIV,INCOL,MNSGFG,MULT,SUB
      PARAMETER(ADD=1,BTWNCL=3,DIV=4,INCOL=2,MNSGFG=3,ZERO=0D0,ONE=1D0,
     &          MULT=3,SUB=2,T=.true.,F=.false.)
c     ------------------------------------------------------------------
      INCLUDE 'cchars.i'
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'mdltbl.i'
      INCLUDE 'units.cmn'
      INCLUDE 'rev.prm'
      INCLUDE 'rev.cmn'
      INCLUDE 'adj.cmn'
      INCLUDE 'revsrs.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'x11log.cmn'
      INCLUDE 'x11fac.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'priusr.cmn'
      INCLUDE 'savcmn.cmn'
      INCLUDE 'htmlout.cmn'
C   LINES OF CODE ADDED FOR X-13A-S : 2
      LOGICAL Lseats
      INCLUDE 'seatad.cmn'
C   END OF CODE BLOCK       
      INCLUDE 'filext.prm'
      INCLUDE 'filext.var'
c     ------------------------------------------------------------------
      CHARACTER blnk*80,fmt*80,str*10,ttlfct*80,ttlfc2*100,outstr*75,
     &          dash*22
      LOGICAL locok,lpria,ltrns,lprix,Lgraf,Svdiag,Lognrm
      INTEGER bgfcst,clwdth,endspn,Fcntyp,Fctdrp,fctori,fh,fh2,i,idate,
     &        mindec,mxdtcr,ndec,ndtchr,Nfcst,nobsf,Nobspf,Nrxy,npos,
     &        nttlcr,ntl2cr,Outdec,rdbdat,tmp1,tmp2,k,Pos2,Kswv,Khol,ff,
     &        fh0
      DOUBLE PRECISION Ciprob,fcst,fcstse,fcterr,Lam,lwrci,nstder,
     &                 pctrgv,pval,rgvar,stctvr,totvar,trnsrs,tval,
     &                 untfct,uprci,Fcstx,tempse,regvar,dlr
      DIMENSION bgfcst(2),endspn(2),idate(2),fcst(PFCST),fcstse(PFCST),
     &          fcterr(PFCST),tval(PFCST),lwrci(PFCST),rgvar(PFCST),
     &          trnsrs(PFCST),untfct(PFCST),uprci(PFCST),Fcstx(PFCST),
     &          tempse(PFCST)
c-----------------------------------------------------------------------
      LOGICAL chkcvr,dpeq,istrue
      DOUBLE PRECISION dinvnr
      EXTERNAL chkcvr,dinvnr,istrue,dpeq
c-----------------------------------------------------------------------
      DATA dash /'----------------------'/
      DATA blnk/
     &'                                                                 
     &               '/
c-----------------------------------------------------------------------
      fh0=0
      IF(.not.Lquiet)fh0=STDERR
c-----------------------------------------------------------------------
c     Forecast options
c-----------------------------------------------------------------------
      ff=ADD
      IF(Adjmod.lt.2)ff=MULT
      locok=T
      CALL addate(Begspn,Sp,Nspobs-1,endspn)
      ltrns=(.not.dpeq(Lam,ONE)).or.Fcntyp.ne.4
      CALL addate(endspn,Sp,-Fctdrp+1,bgfcst)
      lpria=(Nustad.gt.0.and.chkcvr(Bgutad,Nustad,bgfcst,Nfcst,Sp)).or.
     &      (Nuspad.gt.0.and.chkcvr(Bgupad,Nuspad,bgfcst,Nfcst,Sp)).or.
     &      (Priadj.gt.0.and.chkcvr(Begadj,Nadj,bgfcst,Nfcst,Sp))
      lprix=(Axrghl.or.Axrgtd.and.Ixreg.eq.3).or.Khol.eq.2.or.Kswv.eq.1
c     ------------------------------------------------------------------
      CALL addate(endspn,Sp,-Fctdrp,idate)
      CALL wrtdat(idate,Sp,str,ndtchr)
      IF(Lfatal)RETURN
      IF(.not.Lhiddn.and.istrue(Prttab,LFORTS,LFOROS))THEN
       CALL mkPOneLine(Mt1,'@','&nbsp;')
       CALL genSkip(1069)
       CALL writTagOneLine(Mt1,'h2','@','FORECASTING')
       CALL mkPClass(Mt1,'indent')
       WRITE(Mt1,1010)str(1:ndtchr)//Cbr,Nfcst
       CALL writTag(Mt1,'</p>')
      END IF
 1010 FORMAT('  Origin : ',a,/,'  Number : ',i10)
c-----------------------------------------------------------------------
c     Calculate the forecasts
c-----------------------------------------------------------------------
      fctori=Nspobs-Fctdrp
      CALL fcstxy(fctori,Nfcst,fcst,fcstse,rgvar)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     apply lognormal correction to forecasts to be appended to series
c     prior to seasonal adjustment (BCM May 2008)
c-----------------------------------------------------------------------
      IF(Lognrm.and.dpeq(Lam,ZERO))THEN
       CALL lgnrmc(Nfcst,fcst,fcstse,Fcstx,F)
      ELSE
c-----------------------------------------------------------------------
c     Create forecasts to be appended to series prior to seasonal 
c     adjustment (BCM May 2000)
c-----------------------------------------------------------------------
       CALL copy(fcst,Nfcst,1,Fcstx)
      END IF
c-----------------------------------------------------------------------
c     Format the transformed forecasts and their standard errors
c-----------------------------------------------------------------------
      CALL numfmt(fcst,Nfcst,Outdec,clwdth,mindec)
      IF(Lmvaft.or.Ln0aft)THEN
       nobsf=0
      ELSE
       nobsf=min(Nfcst,Nobspf-fctori)
      END IF
      IF(nobsf.gt.0)THEN
       CALL subset(Xy,Nrxy,Ncxy,fctori+1,fctori+nobsf,Ncxy,Ncxy,trnsrs)
       CALL numfmt(trnsrs,nobsf,Outdec,tmp1,tmp2)
       clwdth=max(tmp1,clwdth)
       mindec=max(tmp2,mindec)+MNSGFG-1
      ELSE
       mindec=mindec+MNSGFG-1
      END IF
c-----------------------------------------------------------------------
      IF(mindec.gt.Outdec)THEN
       ndec=min(mindec,11)
       clwdth=clwdth-Outdec+ndec
      ELSE
       ndec=Outdec
      END IF
      IF(ndec.eq.0)clwdth=clwdth+1
      clwdth=min(max(clwdth,8),21)
c     ------------------------------------------------------------------
      CALL addate(endspn,Sp,Nfcst-Fctdrp,idate)
      CALL wrtdat(idate,Sp,str,mxdtcr)
      IF(Lfatal)RETURN
      mxdtcr=max(4,mxdtcr)
      IF(Prttab(LFORTS).or.Savtab(LFORTS).OR.(Lgraf.and.ltrns).or.
     &   Svdiag)THEN
       IF(Prttab(LFORTS))THEN
        CALL makDivId(Mt1,tbxdic(LFORTS),'@')
        CALL mkTableTag(Mt1,'w70','@')
        IF((lpria.or.lprix).and.ltrns)THEN
         CALL mkCaption(Mt1,'Forecasts and Standard Errors of '//
     &                      'the Prior Adjusted and Transformed Data')
        ELSE IF(ltrns)THEN
         CALL mkCaption(Mt1,'Forecasts and Standard Errors of '//
     &                      'the Transformed Data')
        ELSE IF(lpria)THEN
         CALL mkCaption(Mt1,
     &       'Forecasts and Standard Errors of the Prior Adjusted Data')
        ELSE
         CALL mkCaption(Mt1,'Forecasts and Standard Errors')
        END IF
       END IF
c     ------------------------------------------------------------------
       IF(Savtab(LFORTS).OR.(Lgraf.and.ltrns))THEN
        IF(Savtab(LFORTS))CALL opnfil(T,F,LFORTS,fh,locok)
        IF(Lgraf.and.ltrns.and.locok)
     &     CALL opnfil(T,Lgraf,LFORTS,fh2,locok)
        IF(.not.locok)THEN
         CALL abend()
         RETURN
        END IF
        IF(Savtab(LFORTS))THEN
         WRITE(fh,1030)'date',TABCHR,'forecast',TABCHR,'standarderror'
         WRITE(fh,1030)'------',TABCHR,dash(1:Svsize),TABCHR,
     &                 dash(1:Svsize)
        END IF
        IF(Lgraf.and.ltrns)THEN
         WRITE(fh2,1030)'date',TABCHR,'forecast',TABCHR,
     &                          'standarderror'
         WRITE(fh2,1030)'------',TABCHR,dash(1:Svsize),TABCHR,
     &                  dash(1:Svsize)
        END IF
 1030   FORMAT(a:,a,a,a,a:,a,a)
       END IF
c-----------------------------------------------------------------------
c     Print or save the transformed forecasts and their standard errors
c if there are no forecast errors.
c-----------------------------------------------------------------------
       IF(Svdiag)write(Nform,1160)nobsf
 1160  FORMAT('nforctval: ',i3)
       IF(nobsf.eq.0)THEN
c     ------------------------------------------------------------------
        IF(Prttab(LFORTS))THEN
*         WRITE(Mt1,1040)('-',i=1,mxdtcr+BTWNCL+INCOL+2*clwdth+1)
* 1040    FORMAT('   ',77(a))
*         WRITE(Mt1,1050)blnk(1:mxdtcr+BTWNCL+INCOL+2*clwdth-6),
*     &                  blnk(1:mxdtcr-3),blnk(1:BTWNCL+clwdth-8),
*     &                  blnk(1:INCOL+clwdth+1-5)
* 1050    FORMAT('  ',a,'Standard',/,'  ',a,'Date',a,'Forecast',a,
*     &          'Error')
*         WRITE(Mt1,1040)('-',i=1,mxdtcr+BTWNCL+INCOL+2*clwdth+1)
c     ------------------------------------------------------------------
*         WRITE(fmt,1060)mxdtcr+3,BTWNCL+clwdth,ndec,INCOL+clwdth+1,
*     &                  ndec+1
* 1060    FORMAT('(a',i2.2,',f',i2.2,'.',i2.2,',f',i2.2,'.',i2.2,')')
c     ------------------------------------------------------------------
         CALL writTag(Mt1,'<tr>')
         CALL mkHeaderCellScope(Mt1,0,0,'col','@','Date')
         CALL mkHeaderCellScope(Mt1,0,0,'col','@','Forecast')
         CALL mkHeaderCellScope(Mt1,0,0,'col','@','Standard'//Cbr//
     &                                            'Error')
         CALL writTag(Mt1,'</tr>')
        END IF
c     ------------------------------------------------------------------
        DO i=1,Nfcst
         CALL addate(endspn,Sp,i-Fctdrp,idate)
         CALL wrtdat(idate,Sp,str,ndtchr)
         IF(Lfatal)RETURN
c     ------------------------------------------------------------------
         IF(Prttab(LFORTS))THEN
          CALL writTag(Mt1,'<tr>')
          CALL mkHeaderCellScope(Mt1,0,0,'row','@',str(1:ndtchr))
          WRITE(Mt1,602)fcst(i),fcstse(i)
  602     FORMAT(1x,2('<td>',G13.6,'</td>'))
          CALL writTag(Mt1,'</tr>')
         END IF
c     ------------------------------------------------------------------
         IF(Savtab(LFORTS).OR.(Lgraf.and.ltrns))THEN
          npos=1
          rdbdat=100*idate(YR)+idate(MO)
c     ------------------------------------------------------------------
          CALL itoc(rdbdat,outstr,npos)
          IF(Lfatal)RETURN
          outstr(npos:npos)=TABCHR
          npos=npos+1
          CALL dtoc(fcst(i),outstr,npos)
          IF(Lfatal)RETURN
          outstr(npos:npos)=TABCHR
          npos=npos+1
          CALL dtoc(fcstse(i),outstr,npos)
          IF(Lfatal)RETURN
          IF(Savtab(LFORTS))WRITE(fh,1030)outstr(1:npos-1)
          IF(Lgraf.and.ltrns)WRITE(fh2,1030)outstr(1:npos-1)
         END IF
        END DO
c     ------------------------------------------------------------------
        IF(Prttab(LFORTS))THEN
         CALL writTag(Mt1,'</table></div>')
         CALL mkPOneLine(Mt1,'@','&nbsp;')
        END IF
c-----------------------------------------------------------------------
c     Print the forecast errors and t-values for forecasts that occur
c within the span of the data
c-----------------------------------------------------------------------
       ELSE
        CALL subset(Xy,Nrxy,Ncxy,fctori+1,fctori+nobsf,Ncxy,Ncxy,trnsrs)
        CALL eltfcn(SUB,trnsrs,fcst,nobsf,PFCST,fcterr)
        CALL eltfcn(DIV,fcterr,fcstse,nobsf,PFCST,tval)
c     ------------------------------------------------------------------
        IF(Prttab(LFORTS))THEN
c     ------------------------------------------------------------------
*         WRITE(Mt1,1040)('-',i=1,mxdtcr+BTWNCL+4*INCOL+4*clwdth+8)
c     ------------------------------------------------------------------
*         WRITE(Mt1,1070)blnk(1:mxdtcr+BTWNCL+2*INCOL+3*clwdth-7),
*     &                  blnk(1:INCOL+clwdth+1-8),blnk(1:mxdtcr-3),
*     &                  blnk(1:BTWNCL+clwdth-4),blnk(1:INCOL+clwdth-8),
*     &                  blnk(1:INCOL+clwdth-6),blnk(1:INCOL+clwdth+1-5),
*     &                  blnk(1:INCOL)
* 1070    FORMAT('  ',a,'Forecast',a,'Standard',/,'  ',a,'Date',a,'Data',
*     &          a,'Forecast',a,'Error',a,'Error',a,'t-value')
c     ------------------------------------------------------------------
*         WRITE(Mt1,1040)('-',i=1,mxdtcr+BTWNCL+4*INCOL+4*clwdth+8)
c     ------------------------------------------------------------------
*         WRITE(fmt,1080)mxdtcr+3,BTWNCL+clwdth,ndec,INCOL+clwdth,ndec,
*     &                  INCOL+clwdth+1,ndec+1,INCOL+7,2
* 1080    FORMAT('(a',i2.2,',f',i2.2,'.',i2.2,',2f',i2.2,'.',i2.2,',f',
*     &          i2.2,'.',i2.2,',f',i2.2,'.',i2.2,')')
         CALL writTag(Mt1,'<tr>')
         CALL mkHeaderCellScope(Mt1,0,0,'col','@','Date')
         CALL mkHeaderCellScope(Mt1,0,0,'col','@','Data')
         CALL mkHeaderCellScope(Mt1,0,0,'col','@','Forecast')
         CALL mkHeaderCellScope(Mt1,0,0,'col','@','Forecast'//Cbr//
     &                                            'Error')
         CALL mkHeaderCellScope(Mt1,0,0,'col','@','Standard'//Cbr//
     &                                            'Error')
         CALL mkHeaderCellScope(Mt1,0,0,'col','@','t-value')
         CALL writTag(Mt1,'</tr>')
        END IF
c     ------------------------------------------------------------------
        DO i=1,nobsf
         CALL addate(endspn,Sp,i-Fctdrp,idate)
         CALL wrtdat(idate,Sp,str,ndtchr)
         IF(Lfatal)RETURN
c     ------------------------------------------------------------------
         IF(Prttab(LFORTS))THEN
          CALL writTag(Mt1,'<tr>')
          CALL mkHeaderCellScope(Mt1,0,0,'row','@',str(1:ndtchr))
          WRITE(Mt1,601)trnsrs(i)
          WRITE(Mt1,601)fcst(i)
          WRITE(Mt1,601)fcterr(i)
          WRITE(Mt1,601)fcstse(i)
          WRITE(Mt1,601)tval(i)
  601     FORMAT(1x,'<td>',G13.6,'</td>')
          CALL writTag(Mt1,'</tr>')
         END IF
         IF(Svdiag)WRITE(Nform,1170) i,str(1:ndtchr),tval(i)
 1170    FORMAT('forctval',i2.2,': ',a,2x,f12.6)
c     ------------------------------------------------------------------
         IF(Savtab(LFORTS).OR.(Lgraf.and.ltrns))THEN
          npos=1
          rdbdat=100*idate(YR)+idate(MO)
          CALL itoc(rdbdat,outstr,npos)
          IF(Lfatal)RETURN
          outstr(npos:npos)=TABCHR
          npos=npos+1
          CALL dtoc(fcst(i),outstr,npos)
          IF(Lfatal)RETURN
          outstr(npos:npos)=TABCHR
          npos=npos+1
          CALL dtoc(fcstse(i),outstr,npos)
          IF(Lfatal)RETURN
          IF(Savtab(LFORTS))WRITE(fh,1030)outstr(1:npos-1)
          IF(Lgraf.and.ltrns)WRITE(fh2,1030)outstr(1:npos-1)
         END IF
c     ------------------------------------------------------------------
        END DO
c     ------------------------------------------------------------------
        IF(Nfcst.gt.nobsf)THEN
*         IF(Prttab(LFORTS))WRITE(fmt,1060)mxdtcr+3,
*     &                           BTWNCL+INCOL+2*clwdth,ndec,
*     &                           2*INCOL+2*clwdth+1,ndec+1
c     ------------------------------------------------------------------
         DO i=nobsf+1,Nfcst
          CALL addate(endspn,Sp,i-Fctdrp,idate)
          CALL wrtdat(idate,Sp,str,ndtchr)
          IF(Lfatal)RETURN
          IF(Prttab(LFORTS))THEN
*           WRITE(Mt1,fmt)str(1:ndtchr),fcst(i),
*     &                                    fcstse(i)
           CALL writTag(Mt1,'<tr>')
           CALL mkHeaderCellScope(Mt1,0,0,'row','@',str(1:ndtchr))
           CALL mkTableCell(Mt1,'@','&nbsp;')
           WRITE(Mt1,601)fcst(i)
           CALL mkTableCell(Mt1,'@','&nbsp;')
           WRITE(Mt1,601)fcstse(i)
           CALL mkTableCell(Mt1,'@','&nbsp;')
           CALL writTag(Mt1,'</tr>')
          END IF
c     ------------------------------------------------------------------
          IF(Savtab(LFORTS).OR.(Lgraf.and.ltrns))THEN
           npos=1
           rdbdat=100*idate(YR)+idate(MO)
           CALL itoc(rdbdat,outstr,npos)
           IF(Lfatal)RETURN
           outstr(npos:npos)=TABCHR
           npos=npos+1
           CALL dtoc(fcst(i),outstr,npos)
           IF(Lfatal)RETURN
           outstr(npos:npos)=TABCHR
           npos=npos+1
           CALL dtoc(fcstse(i),outstr,npos)
           IF(Lfatal)RETURN
           IF(Savtab(LFORTS))WRITE(fh,1030)outstr(1:npos-1)
           IF(Lgraf.and.ltrns)WRITE(fh2,1030)outstr(1:npos-1)
          END IF
c     ------------------------------------------------------------------
         END DO
        END IF
c     ------------------------------------------------------------------
        IF(Prttab(LFORTS))THEN
*     &     WRITE(Mt1,1040)('-',i=1,mxdtcr+BTWNCL+4*INCOL+4*clwdth+8)
         CALL writTag(Mt1,'</table></div>')
         CALL mkPOneLine(Mt1,'@','&nbsp;')
        END IF
       END IF
       IF(Savtab(LFORTS).and.locok)CALL fclose(fh)
       IF(Lgraf.and.ltrns.and.locok)CALL fclose(fh2)
      END IF
c-----------------------------------------------------------------------
c     Print out or save the contribution of the regression variance to
c the forecast variance.
c-----------------------------------------------------------------------
      IF((Prttab(LFORVR).or.Savtab(LFORVR)).and.Ncxy.gt.1)THEN
       clwdth=max(clwdth,12)
c     ------------------------------------------------------------------
       IF(Prttab(LFORVR))THEN
        CALL makDivId(Mt1,tbxdic(LFORVR),'@')
        CALL mkTableTag(Mt1,'w70','Stochastic and regression '//
     &                  'contributions to the forecast error variances')
        CALL mkCaption(Mt1,'Stochastic and regression '//
     &                 'contributions to the forecast error variances')
*        WRITE(Mt1,1020)'Stochastic and regression contributions '//
*     &                 'to the forecast error variances'
c     ------------------------------------------------------------------
*        WRITE(Mt1,1040)('-',i=1,mxdtcr+BTWNCL+3*INCOL+4*clwdth)
c     ------------------------------------------------------------------
*        WRITE(Mt1,1090)blnk(1:mxdtcr+BTWNCL+clwdth-9),
*     &                 blnk(1:INCOL+clwdth-10),blnk(1:INCOL+clwdth-5),
*     &                 blnk(1:INCOL+clwdth-10),blnk(1:mxdtcr-3),
*     &                 blnk(1:BTWNCL+clwdth-8),blnk(1:INCOL+clwdth-8),
*     &                 blnk(1:INCOL+clwdth-8),blnk(1:INCOL+clwdth-10)
* 1090   FORMAT('  ',a,'Regression',a,'Stochastic',a,'Total',a,
*     &         'Regression',/,'  ',a,'Date',a,'Variance',a,'Variance',a,
*     &         'Variance',a,'Percentage')
c     ------------------------------------------------------------------
*        WRITE(Mt1,1040)('-',i=1,mxdtcr+BTWNCL+3*INCOL+4*clwdth)
c     ------------------------------------------------------------------
*        WRITE(fmt,1100)mxdtcr+3,BTWNCL+clwdth,ndec,INCOL+clwdth,ndec,
*     &                 INCOL+clwdth,2
* 1100   FORMAT('(a',i2.2,',e',i2.2,'.',i2.2,',2e',i2.2,'.',i2.2,',f',
*     &         i2.2,'.',i2.2,')')
        CALL writTag(Mt1,'<tr>')
        CALL mkHeaderCellScope(Mt1,0,0,'col','@','Date')
        CALL mkHeaderCellScope(Mt1,0,0,'col','@','Regression'//Cbr//
     &                                           'Variance')
        CALL mkHeaderCellScope(Mt1,0,0,'col','@','Stochastic'//Cbr//
     &                                           'Variance')
        CALL mkHeaderCellScope(Mt1,0,0,'col','@','Total'//Cbr//
     &                                           'Variance')
        CALL mkHeaderCellScope(Mt1,0,0,'col','@','Regression'//Cbr//
     &                                           'Percentage')
        CALL writTag(Mt1,'</tr>')
       END IF
c     ------------------------------------------------------------------
       IF(Savtab(LFORVR))THEN
        CALL opnfil(T,F,LFORVR,fh,locok)
        IF(.not.locok)THEN
         CALL abend
         IF(Lfatal)RETURN
        END IF
        WRITE(fh,1030)'date',TABCHR,'regressionvariance',TABCHR,
     &                'stochasticvariance'
        WRITE(fh,1030)'------',TABCHR,dash(1:Svsize),TABCHR,
     &                dash(1:Svsize)
       END IF
c     ------------------------------------------------------------------
       DO i=1,Nfcst
        CALL addate(endspn,Sp,i-Fctdrp,idate)
        CALL wrtdat(idate,Sp,str,ndtchr)
        IF(Lfatal)RETURN
        dlr=log10(rgvar(i))
        if(dlr.gt.-100d0)then
         regvar=rgvar(i)
        else
         regvar=0d0
        end if
        totvar=fcstse(i)**2
        stctvr=totvar-regvar
        pctrgv=100D0*regvar/totvar
        IF(Prttab(LFORVR))THEN
*         WRITE(Mt1,fmt)str(1:ndtchr),rgvar(i),stctvr,
*     &                                  totvar,pctrgv
         CALL writTag(Mt1,'<tr>')
         CALL mkHeaderCellScope(Mt1,0,0,'row','@',str(1:ndtchr))
         WRITE(Mt1,601)regvar
         WRITE(Mt1,601)stctvr
         WRITE(Mt1,601)totvar
         WRITE(Mt1,601)pctrgv
         CALL writTag(Mt1,'</tr>')
        END IF
c     ------------------------------------------------------------------
        IF(Savtab(LFORVR))THEN
         npos=1
         rdbdat=100*idate(YR)+idate(MO)
c     ------------------------------------------------------------------
         CALL itoc(rdbdat,outstr,npos)
         IF(Lfatal)RETURN
         outstr(npos:npos)=TABCHR
         npos=npos+1
         CALL dtoc(regvar,outstr,npos)
         IF(Lfatal)RETURN
         outstr(npos:npos)=TABCHR
         npos=npos+1
         CALL dtoc(stctvr,outstr,npos)
         IF(Lfatal)RETURN
         WRITE(fh,1030)outstr(1:npos-1)
        END IF
c     ------------------------------------------------------------------
       END DO
c     ------------------------------------------------------------------
       IF(Prttab(LFORVR))THEN
*     &    WRITE(Mt1,1040)('-',i=1,mxdtcr+BTWNCL+3*INCOL+4*clwdth)
        CALL writTag(Mt1,'</table></div>')
        CALL mkPOneLine(Mt1,'@','&nbsp;')
       END IF
c     ------------------------------------------------------------------
       IF(locok.and.Savtab(LFORVR))CALL fclose(fh)
      END IF
c-----------------------------------------------------------------------
c     Print or save a table of upper and lower confidence intervals
c with F^{-1}(Ciprob/2+.5)*standard error on the original scale with the
c prior adjustments removed.  The Ciprob/2+.5 is because it is a two-
c tailed probability.
c-----------------------------------------------------------------------
      IF(Lognrm.and.dpeq(Lam,ZERO))THEN
       CALL lgnrmc(Nfcst,fcst,fcstse,untfct,T)
      ELSE
       CALL invfcn(fcst,Nfcst,Fcntyp,Lam,untfct)
      END IF
      IF(Nustad.gt.0.or.Nuspad.gt.0.or.Priadj.gt.1)THEN
       IF(lpria)THEN
        CALL eltfcn(ff,untfct,Adj(Adj1st+fctori),Nfcst,PFCST,untfct)
        IF(Khol.eq.2)
     &     CALL eltfcn(ff,untfct,X11hol(Pos2+1),Nfcst,PFCST,untfct)
        IF(Axrghl.and.Ixreg.eq.3)
     &     CALL eltfcn(ff,untfct,Facxhl(Pos2+1),Nfcst,PFCST,untfct)
        IF(Kswv.eq.1.or.(Axrgtd.and.Ixreg.eq.3))
     &     CALL eltfcn(ff,untfct,Stptd(Pos2+1),Nfcst,PFCST,untfct)
       END IF
      END IF
c     ------------------------------------------------------------------
      IF(Prttab(LFOROS).or.Savtab(LFOROS).or.Lgraf)THEN
       pval=(Ciprob+ONE)/2D0
       nstder=dinvnr(pval,ONE-pval)
       CALL scrmlt(nstder,Nfcst,fcstse)
       CALL eltfcn(SUB,fcst,fcstse,Nfcst,PFCST,lwrci)
       CALL eltfcn(ADD,fcst,fcstse,Nfcst,PFCST,uprci)
       ttlfct='Confidence intervals with coverage probability ('
       nttlcr=48
       ntl2cr=0
       IF(Prttab(LFOROS))WRITE(ttlfct(nttlcr+1:),1110)Ciprob
 1110  FORMAT(f8.5,')')
       IF(Savtab(LFOROS))CALL opnfil(T,F,LFOROS,fh,locok)
       IF(Lgraf.and.locok)CALL opnfil(T,Lgraf,LFOROS,fh2,locok)
       IF(.not.locok)THEN
        CALL abend()
        RETURN
       END IF
       nttlcr=nttlcr+9
c     ------------------------------------------------------------------
       IF(ltrns)THEN
        ttlfc2='On the Original Scale'
        ntl2cr=21
        CALL invfcn(lwrci,Nfcst,Fcntyp,Lam,lwrci)
        CALL invfcn(uprci,Nfcst,Fcntyp,Lam,uprci)
       END IF
c     ------------------------------------------------------------------
       IF(Nustad.gt.0.or.Nuspad.gt.0.or.Priadj.gt.1)THEN
        IF(lpria)THEN
         IF(ltrns)THEN
          ttlfc2=ttlfc2(1:ntl2cr)//' Before Prior Adjustments'
          ntl2cr=ntl2cr+25
         ELSE
          ttlfc2='Before Prior Adjustments'
          ntl2cr=24
         END IF
c     ------------------------------------------------------------------
         CALL eltfcn(ff,lwrci,Adj(Adj1st+fctori),Nfcst,PFCST,lwrci)
         CALL eltfcn(ff,uprci,Adj(Adj1st+fctori),Nfcst,PFCST,uprci)
c     ------------------------------------------------------------------
         IF(Khol.eq.2)THEN
          CALL eltfcn(ff,lwrci,X11hol(Pos2+1),Nfcst,PFCST,lwrci)
          CALL eltfcn(ff,uprci,X11hol(Pos2+1),Nfcst,PFCST,uprci)
         END IF
         IF(Axrghl.and.Ixreg.eq.3)THEN
          CALL eltfcn(ff,lwrci,Facxhl(Pos2+1),Nfcst,PFCST,lwrci)
          CALL eltfcn(ff,uprci,Facxhl(Pos2+1),Nfcst,PFCST,uprci)
         END IF
         IF(Kswv.eq.1.or.(Axrgtd.and.Ixreg.eq.3))THEN
          CALL eltfcn(ff,lwrci,Stptd(Pos2+1),Nfcst,PFCST,lwrci)
          CALL eltfcn(ff,uprci,Stptd(Pos2+1),Nfcst,PFCST,uprci)
         END IF
c     ------------------------------------------------------------------
        ELSE
         IF(ltrns)THEN
          ttlfc2=ttlfc2(1:ntl2cr)//' After Prior Adjustments'
          ntl2cr=ntl2cr+25
         ELSE
          ttlfc2='After Prior Adjustments'
          ntl2cr=23
         END IF
c     ------------------------------------------------------------------
         IF(Prttab(LFOROS).or.Savtab(LFOROS).or.Lgraf)THEN
          CALL wWritln('User-defined prior adjustment factor not '//
     &                 'provided',fh0,Mt2,T,F)
          CALL writln('           for the forecast period.',
     &                fh0,Mt2,F,T)
         END IF
        END IF
       ELSE IF(lprix)THEN
        IF(ltrns)THEN
         ttlfc2=ttlfc2(1:ntl2cr)//' Before Prior Adjustments'
         ntl2cr=ntl2cr+25
        ELSE
         ttlfc2='Before Prior Adjustments'
         ntl2cr=24
        END IF
c     ------------------------------------------------------------------
        IF(Axrghl.and.Ixreg.eq.3)THEN
         CALL eltfcn(ff,untfct,Facxhl(Pos2+1),Nfcst,PFCST,untfct)
         CALL eltfcn(ff,lwrci,Facxhl(Pos2+1),Nfcst,PFCST,lwrci)
         CALL eltfcn(ff,uprci,Facxhl(Pos2+1),Nfcst,PFCST,uprci)
        END IF
        IF(Khol.eq.2)THEN
         CALL eltfcn(ff,untfct,X11hol(Pos2+1),Nfcst,PFCST,untfct)
         CALL eltfcn(ff,lwrci,X11hol(Pos2+1),Nfcst,PFCST,lwrci)
         CALL eltfcn(ff,uprci,X11hol(Pos2+1),Nfcst,PFCST,uprci)
        END IF
        IF(Kswv.eq.1.or.(Axrgtd.and.Ixreg.eq.3))THEN
         CALL eltfcn(ff,untfct,Stptd(Pos2+1),Nfcst,PFCST,untfct)
         CALL eltfcn(ff,lwrci,Stptd(Pos2+1),Nfcst,PFCST,lwrci)
         CALL eltfcn(ff,uprci,Stptd(Pos2+1),Nfcst,PFCST,uprci)
        END IF
       END IF
c     ------------------------------------------------------------------
       CALL numfmt(lwrci,Nfcst,Outdec,tmp1,tmp2)
       CALL numfmt(uprci,Nfcst,Outdec,clwdth,mindec)
       clwdth=max(tmp1,clwdth)
       mindec=max(tmp2,mindec)+MNSGFG-1
       IF(mindec.gt.Outdec)THEN
        ndec=min(mindec,11)
        clwdth=clwdth-Outdec+ndec
       ELSE
        ndec=Outdec
       END IF
       IF(ndec.eq.0)clwdth=clwdth+1
       clwdth=min(max(clwdth,8),21)
c     ------------------------------------------------------------------
       IF(Prttab(LFOROS))THEN
        CALL makDivId(Mt1,tbxdic(LFOROS),'@')
        CALL mkTableTag(Mt1,'w70',ttlfct(1:nttlcr))
        IF(ntl2cr.gt.0)THEN
         IF(Lognrm.and.dpeq(Lam,ZERO))THEN
          CALL mkCaption(Mt1,ttlfct(1:nttlcr)//Cbr//ttlfc2(1:ntl2cr)//
     &                   ' with LogNormal correction')
         ELSE
          CALL mkCaption(Mt1,ttlfct(1:nttlcr)//Cbr//ttlfc2(1:ntl2cr))
         END IF
        ELSE
         IF(Lognrm.and.dpeq(Lam,ZERO))THEN
          CALL mkCaption(Mt1,ttlfct(1:nttlcr)//Cbr//
     &                   ' with LogNormal correction')
         ELSE
          CALL mkCaption(Mt1,ttlfct(1:nttlcr))
         END IF
        END IF
c     ------------------------------------------------------------------
*        WRITE(Mt1,1040)('-',i=1,mxdtcr+BTWNCL-INCOL+3*(INCOL+clwdth))
c     ------------------------------------------------------------------
*        WRITE(Mt1,1140)blnk(1:mxdtcr-3),blnk(1:BTWNCL+clwdth-5),
*     &                 blnk(1:INCOL+clwdth-8),blnk(1:INCOL+clwdth-5)
* 1140   FORMAT('  ',a,'Date',a,'Lower',a,'Forecast',a,'Upper')
c     ------------------------------------------------------------------
*        WRITE(Mt1,1040)('-',i=1,mxdtcr+BTWNCL-INCOL+3*(INCOL+clwdth))
c     ------------------------------------------------------------------
*        WRITE(fmt,1150)mxdtcr+3,BTWNCL+clwdth,ndec,INCOL+clwdth,ndec
* 1150   FORMAT('(a',i2.2,',f',i2.2,'.',i2.2,',2f',i2.2,'.',i2.2,')')
        CALL writTag(Mt1,'<tr>')
        CALL mkHeaderCellScope(Mt1,0,0,'col','@','Date')
        CALL mkHeaderCellScope(Mt1,0,0,'col','@','Lower')
        CALL mkHeaderCellScope(Mt1,0,0,'col','@','Forecast')
        CALL mkHeaderCellScope(Mt1,0,0,'col','@','Upper')
        CALL writTag(Mt1,'</tr>')
       END IF
c     ------------------------------------------------------------------
       IF(Savtab(LFOROS))THEN
        WRITE(fh,1030)'date',TABCHR,'forecast',TABCHR,'lowerci',TABCHR,
     &                'upperci'
        WRITE(fh,1030)'------',TABCHR,dash(1:Svsize),TABCHR,
     &                dash(1:Svsize),TABCHR,dash(1:Svsize)
       END IF
       IF(Lgraf)THEN
        WRITE(fh2,1030)'date',TABCHR,'forecast',TABCHR,'lowerci',TABCHR,
     &                 'upperci'
        WRITE(fh2,1030)'------',TABCHR,dash(1:Svsize),TABCHR,
     &                 dash(1:Svsize),TABCHR,dash(1:Svsize)
       END IF
c     ------------------------------------------------------------------
       DO i=1,Nfcst
        CALL addate(endspn,Sp,i-Fctdrp,idate)
        CALL wrtdat(idate,Sp,str,ndtchr)
        IF(Lfatal)RETURN
        IF(Prttab(LFOROS))THEN
*         WRITE(Mt1,fmt)str(1:ndtchr),lwrci(i),untfct(i),uprci(i)
         CALL writTag(Mt1,'<tr>')
         CALL mkHeaderCellScope(Mt1,0,0,'row','@',str(1:ndtchr))
         WRITE(Mt1,601)lwrci(i)
         WRITE(Mt1,601)untfct(i)
         WRITE(Mt1,601)uprci(i)
         CALL writTag(Mt1,'</tr>')
        END IF
        IF(Savtab(LFOROS).or.Lgraf)THEN
         npos=1
         rdbdat=100*idate(YR)+idate(MO)
c     ------------------------------------------------------------------
         CALL itoc(rdbdat,outstr,npos)
         IF(Lfatal)RETURN
         outstr(npos:npos)=TABCHR
         npos=npos+1
         CALL dtoc(untfct(i),outstr,npos)
         IF(Lfatal)RETURN
         outstr(npos:npos)=TABCHR
         npos=npos+1
         CALL dtoc(lwrci(i),outstr,npos)
         IF(Lfatal)RETURN
         outstr(npos:npos)=TABCHR
         npos=npos+1
         CALL dtoc(uprci(i),outstr,npos)
         IF(Lfatal)RETURN
         IF(Savtab(LFOROS))WRITE(fh,1030)outstr(1:npos-1)
         IF(Lgraf)WRITE(fh2,1030)outstr(1:npos-1)
        END IF
c     ------------------------------------------------------------------
       END DO
c     ------------------------------------------------------------------
       IF(Prttab(LFOROS))THEN
*     &    WRITE(Mt1,1040)('-',i=1,mxdtcr+BTWNCL-INCOL+3*(INCOL+clwdth))
        CALL writTag(Mt1,'</table></div>')
        CALL mkPOneLine(Mt1,'@','&nbsp;')
       END IF
c     ------------------------------------------------------------------
       IF(Savtab(LFOROS).and.locok)CALL fclose(fh)
       IF(Lgraf.and.locok)CALL fclose(fh2)
      END IF
C   LINES OF CODE ADDED FOR X-13A-S : 2
      IF(Lseats)CALL copy(fcstse,Nfcst,1,Fctses)
C   END OF CODE BLOCK
c     ------------------------------------------------------------------
c     If forecast error revisions collected, store forecast.
c-----------------------------------------------------------------------
      IF(Irev.eq.4.and.Lrvfct.and.Revptr.gt.0)THEN
       IF(.NOT.(Prttab(LFOROS).or.Savtab(LFOROS).or.Lgraf))THEN
        IF(Lognrm.and.dpeq(Lam,ZERO))THEN
         CALL lgnrmc(Nfcst,fcst,fcstse,untfct,T)
        ELSE
         CALL invfcn(fcst,Nfcst,Fcntyp,Lam,untfct)
        END IF
        CALL eltfcn(ff,untfct,Adj(Adj1st+fctori),Nfcst,PFCST,untfct)
        IF(Axrghl.and.Ixreg.eq.3)
     &     CALL eltfcn(ff,untfct,Facxhl(Pos2+1),Nfcst,PFCST,untfct)
        IF(Khol.eq.2)
     &     CALL eltfcn(ff,untfct,X11hol(Pos2+1),Nfcst,PFCST,untfct)
        IF(Kswv.eq.1.or.(Axrgtd.and.Ixreg.eq.3))
     &     CALL eltfcn(ff,untfct,Stptd(Pos2+1),Nfcst,PFCST,untfct)
       END IF
       DO k=1,Nfctlg
        i=Revptr+Rfctlg(k)
        IF(i.le.(Endrev-Begrev+1))THEN
         Cncfct(k,i)=untfct(Rfctlg(k))
c        ELSE IF(Begrev.le.Endrev-Rfctlg(k))THEN
c         DO i=Begrev,Endrev-Rfctlg(k)
c          rptr=i-Begrev+1
c          Fctdrp=Endrev-i
c          fctori=Nspobs-Fctdrp
c          CALL fcstxy(fctori,Nfcst,fcst,fcstse,rgvar)
c          IF(Lfatal)RETURN
c          CALL invfcn(fcst,Nfcst,Fcntyp,Lam,untfct)
c          Finfct(k,rptr+Rfctlg(k))=untfct(Rfctlg(k))
c         END DO
c         Fctdrp=0
        END IF
       END DO
      END IF
      RETURN
      END
