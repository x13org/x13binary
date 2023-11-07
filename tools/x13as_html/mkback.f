C     Last change:  BCM  29 Sep 97   10:13 am
      SUBROUTINE mkback(Trnsrs,Priadj,Bcst,Ubcst,Pos2,Outdec,Lgraf)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     Generate Backcasts
c     Added November 2006 - print and save tables of backcasts
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'x11log.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'adj.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11fac.cmn'
      INCLUDE 'priusr.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'savcmn.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'mdltbl.i'
      INCLUDE 'stdio.i'
      INCLUDE 'cchars.i'
      INCLUDE 'htmlout.cmn'
      INCLUDE 'units.cmn'
c     ------------------------------------------------------------------
      LOGICAL F,T
      INTEGER BTWNCL,DIV,INCOL,MNSGFG,ADD,MULT,SUB
      DOUBLE PRECISION ZERO,ONE,TWO
      PARAMETER(F=.false.,T=.true.,ADD=1,SUB=2,MULT=3,DIV=4,BTWNCL=3,
     &          INCOL=2,MNSGFG=3,ZERO=0D0,ONE=1D0,TWO=2D0)
c     ------------------------------------------------------------------
      CHARACTER blnk*80,fmt*80,ttlfct*80,ttlfc2*100,str*10,outstr*75,
     &          dash*(22)
      LOGICAL Lgraf,ltrns,lpria,lprix,locok
      INTEGER Priadj,fctori,frstry,i,Pos2,ff,endbak,ndtchr,
     &        Outdec,ndec,rdbdat,npos,fh,fh2,nttlcr,ntl2cr,mindec,idate,
     &        tmp1,tmp2,clwdth,mxdtcr,fh0
      DOUBLE PRECISION rgvar,fcstse,Trnsrs,Bcst,Ubcst,bkxy,pval,nstder,
     &                 lwrci,uprci,revfse
      DIMENSION Bcst(PFCST),Ubcst(PFCST),rgvar(PFCST),fcstse(PFCST),
     &          Trnsrs(PLEN),endbak(2),lwrci(PFCST),uprci(PFCST),
     &          bkxy(PLEN*(PB+1)),idate(2),revfse(PFCST)
c-----------------------------------------------------------------------
      DOUBLE PRECISION dinvnr
      LOGICAL chkcvr,dpeq
      EXTERNAL chkcvr,dinvnr,dpeq
c-----------------------------------------------------------------------
      DATA dash /'----------------------'/
      DATA blnk/
     &'                                                                 
     &               '/
c-----------------------------------------------------------------------
      fh0=0
      IF(.not.Lquiet)fh0=STDERR
c-----------------------------------------------------------------------
c     Generate backcasts
c-----------------------------------------------------------------------
      locok=T
      CALL regvar(Trnsrs,Nspobs,Fctdrp,Nfcst,Nbcst,Userx,Bgusrx,Nrusrx,
     &            Priadj,Reglom,Nrxy,Begxy,frstry,T,Elong)
      IF(Lfatal)RETURN
      fctori=Nspobs
      CALL copy(Xy,Nrxy*Ncxy,1,bkxy)
      CALL revrse(bkxy,Nrxy-Nfcst,Ncxy,Xy)
      CALL fcstxy(fctori,Nbcst,Bcst,fcstse,rgvar)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
      CALL copy(bkxy,Nrxy*Ncxy,1,Xy)
c      CALL revrse(Xy,Nrxy-Nfcst,Ncxy,Xy)
c-----------------------------------------------------------------------
c     Create versions of the backcasts that are on the original scale
c-----------------------------------------------------------------------
      ff=ADD
      IF(Adjmod.lt.2)ff=MULT
      DO i=1,Nbcst
       Ubcst(Nbcst-i+1)=Bcst(i)
      END DO
c-----------------------------------------------------------------------
      IF(Lognrm.and.dpeq(Lam,ZERO))CALL lgnrmc(Nbcst,Bcst,fcstse,Bcst,F)
c-----------------------------------------------------------------------
      CALL wrtdat(Begbak,Sp,str,ndtchr)
      IF(Lfatal)RETURN
      mxdtcr=max(4,ndtchr)
      IF(.not.Lhiddn.and.(Prttab(LFORBC).or.Prttab(LFORTB)))THEN
*     &   WRITE(Mt1,1010)str(1:ndtchr),Nbcst
       CALL mkPOneLine(Mt1,'@','&nbsp;')
       CALL genSkip(1066)
       CALL writTagOneLine(Mt1,'h2','@','BACKCASTING')
       CALL mkPClass(Mt1,'indent')
       WRITE(Mt1,1010)str(1:ndtchr)//Cbr,Nfcst
       CALL writTag(Mt1,'</p>')
      END IF
 1010 FORMAT('  Origin : ',a,/,'  Number : ',i10)
c-----------------------------------------------------------------------
c     Format the transformed backcasts and their standard errors
c-----------------------------------------------------------------------
      CALL numfmt(Ubcst,Nbcst,Outdec,clwdth,mindec)
      mindec=mindec+MNSGFG-1
c-----------------------------------------------------------------------
      IF(mindec.gt.Outdec)THEN
       ndec=min(mindec,11)
       clwdth=clwdth-Outdec+ndec
      ELSE
       ndec=Outdec
      END IF
      IF(ndec.eq.0)clwdth=clwdth+1
      clwdth=min(max(clwdth,8),21)
c-----------------------------------------------------------------------
      ltrns=(.not.dpeq(Lam,ONE)).or.Fcntyp.ne.4
      lpria=(Nustad.gt.0.and.chkcvr(Bgutad,Nustad,Begbak,Nbcst,Sp)).or.
     &      (Nuspad.gt.0.and.chkcvr(Bgupad,Nuspad,Begbak,Nbcst,Sp)).or.
     &      (Priadj.gt.0.and.chkcvr(Begadj,Nadj,Begbak,Nbcst,Sp))
      lprix=(Axrghl.or.Axrgtd.and.Ixreg.eq.3).or.Khol.eq.2.or.Kswv.eq.1
c-----------------------------------------------------------------------
      IF(Prttab(LFORBC).or.Savtab(LFORBC).or.Lgraf)THEN
       IF(Prttab(LFORBC))THEN
        CALL mkTableTag(Mt1,'w70','@')
        IF((lpria.or.lprix).and.ltrns)THEN
         CALL mkCaption(Mt1,'Backcasts and Standard Errors of '//
     &                      'the Prior Adjusted and Transformed Data')
        ELSE IF(ltrns)THEN
         CALL mkCaption(Mt1,'Backcasts and Standard Errors of '//
     &                      'the Transformed Data')
        ELSE IF(lpria)THEN
         CALL mkCaption(Mt1,
     &       'Backcasts and Standard Errors of the Prior Adjusted Data')
        ELSE
         CALL mkCaption(Mt1,'Backcasts and Standard Errors')
        END IF
       END IF
 1020  FORMAT(/,'  ',a)
c-----------------------------------------------------------------------
       IF(Savtab(LFORTB).OR.(Lgraf.and.ltrns))THEN
        IF(Savtab(LFORTB))CALL opnfil(T,F,LFORTB,fh,locok)
        IF((Lgraf.and.ltrns).and.locok)
     &     CALL opnfil(T,Lgraf,LFORTB,fh2,locok)
        IF(.not.locok)THEN
         CALL abend()
         RETURN
        END IF
        IF(Savtab(LFORTB))THEN
         WRITE(fh,1030)'date',TABCHR,'backcast',TABCHR,'standarderror'
         WRITE(fh,1030)'------',TABCHR,dash(1:Svsize),TABCHR,
     &                 dash(1:Svsize)
        END IF
        IF(Lgraf.and.ltrns)THEN
         WRITE(fh2,1030)'date',TABCHR,'backcast',TABCHR,
     &                          'standarderror'
         WRITE(fh2,1030)'------',TABCHR,dash(1:Svsize),TABCHR,
     &                  dash(1:Svsize)
        END IF
 1030   FORMAT(a:,a,a,a,a:,a,a)
       END IF
c-----------------------------------------------------------------------
       IF(Prttab(LFORTB))THEN
*        WRITE(Mt1,1040)('-',i=1,mxdtcr+BTWNCL+INCOL+2*clwdth+1)
* 1040   FORMAT('   ',77(a))
*        WRITE(Mt1,1050)blnk(1:mxdtcr+BTWNCL+INCOL+2*clwdth-6),
*     &                 blnk(1:mxdtcr-3),blnk(1:BTWNCL+clwdth-8),
*     &                 blnk(1:INCOL+clwdth+1-5)
* 1050   FORMAT('  ',a,'Standard',/,'  ',a,'Date',a,'Backcast',a,
*     &         'Error')
*        WRITE(Mt1,1040)('-',i=1,mxdtcr+BTWNCL+INCOL+2*clwdth+1)
c     ------------------------------------------------------------------
*        WRITE(fmt,1060)mxdtcr+3,BTWNCL+clwdth,ndec,INCOL+clwdth+1,
*     &                 ndec+1
* 1060   FORMAT('(a',i2.2,',f',i2.2,'.',i2.2,',f',i2.2,'.',i2.2,')')
         CALL writTag(Mt1,'<tr>')
         CALL mkHeaderCellScope(Mt1,0,0,'col','@','Date')
         CALL mkHeaderCellScope(Mt1,0,0,'col','@','Backcast')
         CALL mkHeaderCellScope(Mt1,0,0,'col','@','Standard'//Cbr//
     &                                            'Error')
         CALL writTag(Mt1,'</tr>')
       END IF
c     ------------------------------------------------------------------
       DO i=1,Nbcst
        CALL addate(Begbak,Sp,i-1,idate)
        CALL wrtdat(idate,Sp,str,ndtchr)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
        IF(Prttab(LFORTB))THEN
*     &     WRITE(Mt1,fmt)str(1:ndtchr),Ubcst(i),fcstse(Nbcst-i+1)
         CALL writTag(Mt1,'<tr>')
         CALL mkHeaderCellScope(Mt1,0,0,'row','@',str(1:ndtchr))
         WRITE(Mt1,602)Ubcst(i),fcstse(Nbcst-i+1)
  602    FORMAT(1x,2('<td>',G13.6,'</td>'))
         CALL writTag(Mt1,'</tr>')
        END IF
c     ------------------------------------------------------------------
        IF(Savtab(LFORTB).OR.(Lgraf.and.ltrns))THEN
         npos=1
         rdbdat=100*idate(YR)+idate(MO)
c     ------------------------------------------------------------------
         CALL itoc(rdbdat,outstr,npos)
         IF(Lfatal)RETURN
         outstr(npos:npos)=TABCHR
         npos=npos+1
         CALL dtoc(Ubcst(i),outstr,npos)
         IF(Lfatal)RETURN
         outstr(npos:npos)=TABCHR
         npos=npos+1
         CALL dtoc(fcstse(Nbcst-i+1),outstr,npos)
         IF(Lfatal)RETURN
         IF(Savtab(LFORTB))WRITE(fh,1030)outstr(1:npos-1)
         IF(Lgraf.and.ltrns)WRITE(fh2,1030)outstr(1:npos-1)
        END IF
       END DO
c     ------------------------------------------------------------------
       IF(Prttab(LFORTB))THEN
*     &    WRITE(Mt1,1040)('-',i=1,mxdtcr+BTWNCL+INCOL+2*clwdth+1)
        CALL writTag(Mt1,'</table>')
        CALL mkPOneLine(Mt1,'@','&nbsp;')
       END IF
       IF(Savtab(LFORTS).and.locok)CALL fclose(fh)
       IF(Lgraf.and.ltrns.and.locok)CALL fclose(fh2)
      END IF
c-----------------------------------------------------------------------
      IF(Prttab(LFORBC).or.Savtab(LFORBC).or.Lgraf)THEN
       pval=(Ciprob+ONE)/TWO
       nstder=dinvnr(pval,ONE-pval)
       DO i=1,Nbcst
        revfse(i)=fcstse(Nbcst-i+1)
       END DO
       CALL scrmlt(nstder,Nbcst,revfse)
       CALL eltfcn(SUB,Ubcst,revfse,Nbcst,PFCST,lwrci)
       CALL eltfcn(ADD,Ubcst,revfse,Nbcst,PFCST,uprci)
      END IF
c-----------------------------------------------------------------------
      IF(ltrns)THEN
       IF(Lognrm.and.dpeq(Lam,ZERO))THEN
        CALL lgnrmc(Nbcst,Ubcst,revfse,Ubcst,T)
       ELSE
        CALL invfcn(Ubcst,Nbcst,Fcntyp,Lam,Ubcst)
       END IF
       IF(Prttab(LFORBC).or.Savtab(LFORBC).or.Lgraf)THEN
        CALL invfcn(lwrci,Nbcst,Fcntyp,Lam,lwrci)
        CALL invfcn(uprci,Nbcst,Fcntyp,Lam,uprci)
       END IF
      END IF
      IF(lpria)THEN
       CALL eltfcn(ff,Ubcst,Adj(Adj1st-Nbcst),Nbcst,PFCST,Ubcst)
       IF(Prttab(LFORBC).or.Savtab(LFORBC).or.Lgraf)THEN
        CALL eltfcn(ff,lwrci,Adj(Adj1st-Nbcst),Nbcst,PFCST,lwrci)
        CALL eltfcn(ff,uprci,Adj(Adj1st-Nbcst),Nbcst,PFCST,uprci)
       END IF
      END IF
      IF(Khol.eq.2)THEN
       CALL eltfcn(ff,Ubcst,X11hol(Pos2),Nbcst,PFCST,Ubcst)
       IF(Prttab(LFORBC).or.Savtab(LFORBC).or.Lgraf)THEN
        CALL eltfcn(ff,lwrci,X11hol(Pos2),Nbcst,PFCST,lwrci)
        CALL eltfcn(ff,uprci,X11hol(Pos2),Nbcst,PFCST,uprci)
       END IF
      END IF
      IF(Axrghl.and.Ixreg.eq.3)THEN
       CALL eltfcn(ff,Ubcst,Facxhl(Pos2),Nbcst,PFCST,Ubcst)
       IF(Prttab(LFORBC).or.Savtab(LFORBC).or.Lgraf)THEN
        CALL eltfcn(ff,lwrci,Facxhl(Pos2),Nbcst,PFCST,lwrci)
        CALL eltfcn(ff,uprci,Facxhl(Pos2),Nbcst,PFCST,uprci)
       END IF
      END IF
      IF(Kswv.eq.1.or.(Axrgtd.and.Ixreg.eq.3))THEN
       CALL eltfcn(ff,Ubcst,Stptd(Pos2),Nbcst,PFCST,Ubcst)
       IF(Prttab(LFORBC).or.Savtab(LFORBC).or.Lgraf)THEN
        CALL eltfcn(ff,lwrci,Stptd(Pos2),Nbcst,PFCST,lwrci)
        CALL eltfcn(ff,uprci,Stptd(Pos2),Nbcst,PFCST,uprci)
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Prttab(LFORBC).or.Savtab(LFORBC).or.Lgraf)THEN
       ttlfct='Confidence intervals with coverage probability ('
       nttlcr=48
       ntl2cr=0
       IF(Prttab(LFORBC))WRITE(ttlfct(nttlcr+1:),1110)Ciprob
 1110  FORMAT(f8.5,')')
       IF(Savtab(LFORBC))CALL opnfil(T,F,LFORBC,fh,locok)
       IF(Lgraf.and.locok)CALL opnfil(T,Lgraf,LFORBC,fh2,locok)
       IF(.not.locok)THEN
        CALL abend()
        RETURN
       END IF
       nttlcr=nttlcr+9
c     ------------------------------------------------------------------
       IF(ltrns)THEN
        ttlfc2='On the Original Scale'
        ntl2cr=21
       END IF
c-----------------------------------------------------------------------
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
        ELSE
         IF(ltrns)THEN
          ttlfc2=ttlfc2(1:ntl2cr)//' After Prior Adjustments'
          ntl2cr=ntl2cr+25
         ELSE
          ttlfc2='After Prior Adjustments'
          ntl2cr=23
         END IF
         IF(Prttab(LFORBC).or.Savtab(LFORBC).or.Lgraf)THEN
          CALL wWritln('User-defined prior adjustment factor not '//
     &                 'provided',fh0,Mt2,T,F)
          CALL writln('           for the backcast period.',
     &                fh0,Mt2,T,F)
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
       END IF
c-----------------------------------------------------------------------
       CALL numfmt(lwrci,Nbcst,Outdec,tmp1,tmp2)
       CALL numfmt(uprci,Nbcst,Outdec,clwdth,mindec)
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
       IF(Prttab(LFORBC))THEN
        CALL mkTableTag(Mt1,'w70',ttlfct(1:nttlcr))
        IF(ntl2cr.gt.0)THEN
         IF(Lognrm.and.dpeq(Lam,ZERO))THEN
          CALL mkCaption(Mt1,
     &                   ttlfct(1:nttlcr)//Cbr//ttlfc2(1:ntl2cr)//
     &                   ' with LogNormal correction')
         ELSE
          CALL mkCaption(Mt1,
     &                   ttlfct(1:nttlcr)//Cbr//ttlfc2(1:ntl2cr))
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
* 1140   FORMAT('  ',a,'Date',a,'Lower',a,'Backcast',a,'Upper')
c     ------------------------------------------------------------------
*        WRITE(Mt1,1040)('-',i=1,mxdtcr+BTWNCL-INCOL+3*(INCOL+clwdth))
c     ------------------------------------------------------------------
*        WRITE(fmt,1150)mxdtcr+3,BTWNCL+clwdth,ndec,INCOL+clwdth,ndec
* 1150   FORMAT('(a',i2.2,',f',i2.2,'.',i2.2,',2f',i2.2,'.',i2.2,')')
        CALL writTag(Mt1,'<tr>')
        CALL mkHeaderCellScope(Mt1,0,0,'col','@','Date')
        CALL mkHeaderCellScope(Mt1,0,0,'col','@','Lower')
        CALL mkHeaderCellScope(Mt1,0,0,'col','@','Backcast')
        CALL mkHeaderCellScope(Mt1,0,0,'col','@','Upper')
        CALL writTag(Mt1,'</tr>')
       END IF
c     ------------------------------------------------------------------
       IF(Savtab(LFORBC))THEN
        WRITE(fh,1030)'date',TABCHR,'backcast',TABCHR,'lowerci',TABCHR,
     &                'upperci'
        WRITE(fh,1030)'------',TABCHR,dash(1:Svsize),TABCHR,
     &                dash(1:Svsize),TABCHR,dash(1:Svsize)
       END IF
       IF(Lgraf)THEN
        WRITE(fh2,1030)'date',TABCHR,'backcast',TABCHR,'lowerci',TABCHR,
     &                 'upperci'
        WRITE(fh2,1030)'------',TABCHR,dash(1:Svsize),TABCHR,
     &                 dash(1:Svsize),TABCHR,dash(1:Svsize)
       END IF
c     ------------------------------------------------------------------
       DO i=1,Nbcst
        CALL addate(Begbak,Sp,i-1,idate)
        CALL wrtdat(idate,Sp,str,ndtchr)
        IF(Lfatal)RETURN
        IF(Prttab(LFORBC))THEN
*         WRITE(Mt1,fmt)str(1:ndtchr),lwrci(i),Ubcst(i),uprci(i)
         CALL writTag(Mt1,'<tr>')
         CALL mkHeaderCellScope(Mt1,0,0,'row','@',str(1:ndtchr))
         WRITE(Mt1,601)lwrci(i)
         WRITE(Mt1,601)Ubcst(i)
         WRITE(Mt1,601)uprci(i)
  601    FORMAT(1x,'<td>',G13.6,'</td>')
         CALL writTag(Mt1,'</tr>')
        END IF
        IF(Savtab(LFORBC).or.Lgraf)THEN
         npos=1
         rdbdat=100*idate(YR)+idate(MO)
c     ------------------------------------------------------------------
         CALL itoc(rdbdat,outstr,npos)
         IF(Lfatal)RETURN
         outstr(npos:npos)=TABCHR
         npos=npos+1
         CALL dtoc(Ubcst(i),outstr,npos)
         IF(Lfatal)RETURN
         outstr(npos:npos)=TABCHR
         npos=npos+1
         CALL dtoc(lwrci(i),outstr,npos)
         IF(Lfatal)RETURN
         outstr(npos:npos)=TABCHR
         npos=npos+1
         CALL dtoc(uprci(i),outstr,npos)
         IF(Lfatal)RETURN
         IF(Savtab(LFORBC))WRITE(fh,1030)outstr(1:npos-1)
         IF(Lgraf)WRITE(fh2,1030)outstr(1:npos-1)
        END IF
c     ------------------------------------------------------------------
       END DO
c     ------------------------------------------------------------------
       IF(Prttab(LFORBC))THEN
*     &    WRITE(Mt1,1040)('-',i=1,mxdtcr+BTWNCL-INCOL+3*(INCOL+clwdth))
        CALL writTag(Mt1,'</table>')
        CALL mkPOneLine(Mt1,'@','&nbsp;')
       END IF
c     ------------------------------------------------------------------
       IF(Savtab(LFORBC).and.locok)CALL fclose(fh)
       IF(Lgraf.and.locok)CALL fclose(fh2)
c     ------------------------------------------------------------------
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
