C     Last change:  BCM   3 Mar 1999    8:34 am
      SUBROUTINE amdfct(Trnsrs,Mape,Nobspf,Nfcst,Bckcst,Fctok,Lauto)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Calculate forecasts, average MAPE for three years
c-----------------------------------------------------------------------
      LOGICAL T,F
      INTEGER ADD,SUB,DIV
      DOUBLE PRECISION ONE,ZERO
      PARAMETER(ADD=1,SUB=2,DIV=4,ONE=1D0,ZERO=0D0,T=.true.,F=.false.)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'fxreg.cmn'
c-----------------------------------------------------------------------
      CHARACTER str*(PGRPCR)
      LOGICAL Bckcst,locok,tst1,tst2,Fctok,Lauto,outf,latemp
      DOUBLE PRECISION a,Mape,tmpsrs,fdiff,ad1,ave,dn,fcst,Trnsrs,fotl,
     &                 tsrs,tmpse,tmpxpx,tmpgpg,tmpvwp,tmptcv,tmpkhd,
     &                 tmpmd,tmpvar,tmpacm,xybak,fxfc
      INTEGER i,j,Nobspf,nfc,Nfcst,nobsf,ivalue,fctori,disp,na,emdl2,
     &        nefobs,frstry,icol,iusr,otltyp,begotl,endotl,nchr,bmdl2,
     &        nobsot,rtype,fdbak
      DIMENSION a(PLEN+2*PORDER),Mape(4),tmpsrs(PFCST),fdiff(PFCST),
     &          fcst(PFCST),Trnsrs(*),emdl2(2),fotl(PLEN),tsrs(PLEN),
     &          bmdl2(2),tmpse(PFCST),tmpxpx(PXPX),tmpgpg(PGPG),
     &          tmpmd((PLEN+PORDER)*PORDER),tmpvwp(PGPG),
     &          tmpacm(PLEN+2*PORDER,PARIMA),xybak(PLEN*(PB+1)),
     &          fxfc(PFCST)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
c     Initialize number of forecasts
c-----------------------------------------------------------------------
      latemp=Lauto
      nfc=Sp
      nobsf=3*Sp
      nobsot=Nspobs-nobsf
      IF(Lauto)THEN
       outf=Outfer
      ELSE
       outf=Outfct
      END IF
c-----------------------------------------------------------------------
c     Check to see if there is enough data to perform the analysis
c-----------------------------------------------------------------------
      IF(.not.Bckcst.and.(((nobsot-(Mxdflg+Mxarlg))*Ncxy)+1).le.0)THEN
       IF(.not.Lauto)
     &    CALL nWritln('Insufficient data to compute average '//
     &                 'forecast error diagnostic.',Mt1,Mt2,T,T)
       Fctok=F
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Initialize percent error of forecasts, temporary series
c-----------------------------------------------------------------------
      CALL setdp(ZERO,4,Mape)
      CALL setdp(ZERO,PFCST,fxfc)
      CALL copy(Trnsrs,Nspobs,-1,tsrs)
      fdbak=Fctdrp
      Fctdrp=0
c-----------------------------------------------------------------------
c     If out-of-sample forecasts used, save model values, ending date
c     for model span.
c-----------------------------------------------------------------------
      CALL setdp(ZERO,PLEN,fotl)
      IF(outf)THEN
       CALL copy(Chlxpx,PXPX,-1,tmpxpx)
       CALL copy(Chlgpg,PGPG,-1,tmpgpg)
       CALL copy(Chlvwp,PGPG,-1,tmpvwp)
       CALL copy(Matd,(PLEN+PORDER)*PORDER,-1,tmpmd)
       CALL copy(Armacm,(PLEN+2*PORDER)*PARIMA,-1,tmpacm)
       tmptcv=Lndtcv
       tmpkhd=Lnlkhd
       tmpvar=Var
       CALL ssprep(T,F,F)
       IF(Bckcst)THEN
        CALL cpyint(Begmdl,2,1,bmdl2)
       ELSE
        CALL cpyint(Endmdl,2,1,emdl2)
       END IF
c-----------------------------------------------------------------------
c     Remove outliers which occur in the last three years from series,
c     regression variables
c-----------------------------------------------------------------------
       iusr=0
       IF(Nb.gt.0)THEN
        icol=Nb
        DO WHILE (icol.ge.1)
         rtype=Rgvrtp(icol)
c-----------------------------------------------------------------------
c     Check for regression variables defined to be outliers in the
c     user-defined regression variables.
c-----------------------------------------------------------------------
c         IF((rtype.ge.PRGTUH.and.rtype.le.PRGUH5).or.rtype.eq.PRGTUS.or.
c     &       rtype.eq.PRGTUD)THEN
c          iusr=iusr+1
c          rtype=Usrtyp(iusr)
c         END IF
         IF(rtype.eq.PRGTAO.or.rtype.eq.PRGTAA.or.rtype.eq.PRGTMV.or.
c     &      rtype.eq.PRGUAO.or.rtype.eq.PRGULS.or.rtype.eq.PRGUSO.or.
     &      rtype.eq.PRGTLS.or.rtype.eq.PRGTAL.or.
     &      rtype.eq.PRGTRP.or.rtype.eq.PRGTTC.or.rtype.eq.PRGTAT.or.
     &      rtype.eq.PRGTSO.or.
     &      rtype.eq.PRGTTL.or.rtype.eq.PRGTQI.or.rtype.eq.PRGTQD.or.
     &      rtype.eq.PRSQAO.or.rtype.eq.PRSQLS)THEN
          CALL getstr(Colttl,Colptr,Ncoltl,icol,str,nchr)
          IF(Lfatal)RETURN
          CALL rdotlr(str(1:nchr),Begxy,Sp,otltyp,begotl,endotl,locok)
          IF(.not.locok)THEN
           CALL abend
           RETURN
          END IF
c-----------------------------------------------------------------------
c     IF Outlier is within the last three years (or the first three
c     years, if backcasts are computed), generate a factor to remove it
c     from the series
c-----------------------------------------------------------------------
          tst1=(otltyp.eq.RP.and.endotl.gt.nobsot).or.
     &         (otltyp.ne.RP.and.begotl.gt.nobsot)
          tst2=(begotl.le.nobsf)
          IF((.not.Bckcst.and.tst1).or.(Bckcst.and.tst2))THEN
           CALL daxpy(Nrxy,B(icol),Xy(icol),Ncxy,fotl,1)
c-----------------------------------------------------------------------
c     remove outlier regressor from the regression
c-----------------------------------------------------------------------
           CALL dlrgef(icol,Nrxy,1)
           IF(Lfatal)RETURN
c-----------------------------------------------------------------------
          END IF
c-----------------------------------------------------------------------
         END IF
         icol=icol-1
        END DO
       END IF
c-----------------------------------------------------------------------
c      remove regressor from series
c-----------------------------------------------------------------------
       CALL eltfcn(SUB,Trnsrs,fotl,Nspobs,PLEN,tsrs)
c-----------------------------------------------------------------------
c     Reverse XY matrix for backcasts, if necessary
c-----------------------------------------------------------------------
      ELSE IF(Bckcst)THEN
       CALL copy(Xy,PLEN*(PB+1),1,xybak)
       CALL revrse(xybak,Nrxy-Nfcst,Ncxy,Xy)
      END IF
c-----------------------------------------------------------------------
c     Check to see if any values are less than zero
c     If fixed regressors are in model, put back into original series
c     (BCM June 2007)
c-----------------------------------------------------------------------
      ivalue=0
      IF(Bckcst)THEN
       CALL copy(tsrs,nobsf,1,tmpsrs)
       IF(Nfxttl.gt.0)CALL copy(Fixfac,nobsf,1,fxfc)
      ELSE
       CALL copy(tsrs(nobsot+1),nobsf,1,tmpsrs)
       IF(Nfxttl.gt.0)CALL copy(Fixfac(nobsot+1),nobsf,1,fxfc)
      END IF
      IF(Nfxttl.gt.0)CALL eltfcn(ADD,tmpsrs,fxfc,nobsf,PFCST,tmpsrs)
      CALL invfcn(tmpsrs,nobsf,Fcntyp,Lam,tmpsrs)
      ad1=ONE
      DO i=1,nobsf
       ad1=dmin1(ad1,tmpsrs(i))
      END DO
      ave=ONE
      IF(ad1.le.ZERO)THEN
       ivalue=1
       dn=ZERO
       DO i=1,nobsf
        ave=ave+abs(tmpsrs(i))
        dn=dn+ONE
       END DO
       ave=ave/dn
       IF(dpeq(ave,ZERO))ave=ONE
      END IF
c-----------------------------------------------------------------------
      DO i=1,3
       disp=-i*Sp
       IF(outf)THEN
        IF(Bckcst)THEN
c-----------------------------------------------------------------------
c     Change the starting date, if backcasts are to be tested
c-----------------------------------------------------------------------
         CALL addate(Begmdl,Sp,Sp,Begmdl)
         CALL cpyint(Begmdl,2,1,Begspn)
c-----------------------------------------------------------------------
c     Change the ending date, number of observations for the series
c-----------------------------------------------------------------------
        ELSE
         CALL addate(Endmdl,Sp,-Sp,Endmdl)
         CALL cpyint(Endmdl,2,1,Endspn)
        END IF
c-----------------------------------------------------------------------
c     Change the number of observations for the series, forecast origin
c-----------------------------------------------------------------------
        CALL dfdate(Endspn,Begspn,Sp,Nspobs)
        Nspobs=Nspobs+1
        Nobspf=min(Nspobs+max(nfc-Fctdrp,0),Nomnfy)
        fctori=Nspobs
c-----------------------------------------------------------------------
c     IF backcasts are being generated, copy the first year of data into
c     tmpsrs in inverse order, then drop the first year of observations
c     from the series
c-----------------------------------------------------------------------
        IF(Bckcst)THEN
         DO j=1,Sp
          tmpsrs(Sp-j+1)=tsrs(j)
         END DO
         CALL copy(tsrs(Sp+1),Nspobs,1,tsrs(1))
        END IF
c-----------------------------------------------------------------------
c     Set up the regression matrix
c-----------------------------------------------------------------------
        CALL regvar(tsrs,Nobspf,Fctdrp,nfc,0,Userx,Bgusrx,Nrusrx,Priadj,
     &              Reglom,Nrxy,Begxy,frstry,T,Elong)
c-----------------------------------------------------------------------
c     Estimate the regression and ARMA parameters
c-----------------------------------------------------------------------
        CALL rgarma(T,Mxiter,Mxnlit,F,a,na,nefobs,Lauto)
        IF(Lfatal.or.(latemp.and.(.not.Lauto)))RETURN
        nobsf=nfc
        IF(Bckcst)THEN
         CALL copy(Xy,PLEN*(PB+1),1,xybak)
         CALL revrse(xybak,Nrxy-nfc,Ncxy,Xy)
        END IF
       ELSE
        fctori=Nspobs+disp
        nobsf=min(nfc,Nobspf-fctori)
       END IF
       CALL fcstxy(fctori,nfc,fcst,tmpse,fdiff)
       IF(Lfatal)RETURN
       IF(.not.(outf.and.Bckcst))
     &    CALL subset(Xy,Nrxy,Ncxy,fctori+1,fctori+nobsf,Ncxy,Ncxy,
     &                tmpsrs)
c-----------------------------------------------------------------------
c     If fixed regressors were in the model, put fixed factor back
c     into the forecasts and original series (BCM June 2007)
c-----------------------------------------------------------------------
       IF(Nfxttl.gt.0)THEN
        CALL copy(Fixfac(fctori+1),nobsf,1,fxfc)
        CALL eltfcn(ADD,tmpsrs,fxfc,nobsf,PFCST,tmpsrs)
        CALL eltfcn(ADD,fcst,fxfc,nobsf,PFCST,fcst)
       END IF
c-----------------------------------------------------------------------
       CALL invfcn(fcst,nobsf,Fcntyp,Lam,fcst)
       CALL invfcn(tmpsrs,nobsf,Fcntyp,Lam,tmpsrs)
       CALL eltfcn(SUB,tmpsrs,fcst,nobsf,PFCST,fdiff)
       IF(ivalue.eq.0)CALL eltfcn(DIV,fdiff,tmpsrs,nobsf,PFCST,fdiff)
       dn=ZERO
       DO j=1,nobsf
        Mape(i)=Mape(i)+abs(fdiff(j))
        dn=dn+1D0
       END DO
       Mape(i)=(Mape(i)*100D0)/(dn*ave)
      END DO
c-----------------------------------------------------------------------
c     Compute average MAPE for all three years
c-----------------------------------------------------------------------
      Mape(4)=(Mape(1)+Mape(2)+Mape(3))/3.0D0
c-----------------------------------------------------------------------
c     Reset model parameters, model span
c-----------------------------------------------------------------------
      IF(outf)THEN
       IF(Bckcst)THEN
        CALL cpyint(bmdl2,2,1,Begmdl)
        CALL cpyint(Begmdl,2,1,Begspn)
       ELSE
        CALL cpyint(emdl2,2,1,Endmdl)
        CALL cpyint(Endmdl,2,1,Endspn)
       END IF
       CALL dfdate(Endspn,Begspn,Sp,Nspobs)
       Nspobs=Nspobs+1
       Nobspf=min(Nspobs+max(nfc-Fctdrp,0),Nomnfy)
c-----------------------------------------------------------------------
c     Restore regression matrix, redo estimation on entire series.
c-----------------------------------------------------------------------
       CALL restor(T,F,F)
       CALL copy(tmpxpx,PXPX,-1,Chlxpx)
       CALL copy(tmpgpg,PGPG,-1,Chlgpg)
       CALL copy(tmpvwp,PGPG,-1,Chlvwp)
       CALL copy(tmpmd,(PLEN+PORDER)*PORDER,-1,Matd)
       CALL copy(tmpacm,(PLEN+2*PORDER)*PARIMA,-1,Armacm)
       Lndtcv=tmptcv
       Lnlkhd=tmpkhd
       Var=tmpvar
       CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &             Priadj,Reglom,Nrxy,Begxy,frstry,T,Elong)
c       IF(.not.Lfatal)CALL rgarma(T,Mxiter,Mxnlit,F,a,na,nefobs,Lauto)
       IF(Lfatal)RETURN
      ELSE IF(Bckcst)THEN
       CALL copy(Xy,PLEN*(PB+1),1,xybak)
       CALL revrse(xybak,Nrxy-Nfcst,Ncxy,Xy)
      END IF
      Fctdrp=fdbak
c-----------------------------------------------------------------------
      RETURN
      END
