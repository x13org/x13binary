C     Last change:  BCM   1 Dec 2004   10:00 am
      SUBROUTINE iddiff(Idr,Ids,Trnsrs,Nefobs,Frstry,A,Na,Imu,Lmu,
     &                  Svldif,Lsumm)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     Unit root identification procedure outlined in the paper by Gomez
c     and Maravall (1998) and implemented in TRAMO modeling package
c     ------------------------------------------------------------------
      DOUBLE PRECISION ONE,ZERO,PONE,PTWO,PT9
      INTEGER TWOHND
      LOGICAL T,F
      PARAMETER(T=.true.,F=.FALSE.,ONE=1D0,ZERO=0D0,PONE=1.D-1,
     &          PTWO=2.D-1,PT9=9.D-1,TWOHND=200)
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'prior.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'prior.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'htmlout.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'mdltbl.i'
c     ------------------------------------------------------------------
      DOUBLE PRECISION Trnsrs,A,rmaxr,rmaxs,tval,vct,din,rmse,
     &                 xpxinv,tmp,seb,xmu,txy
      LOGICAL inptok,Svldif,linv,lxar,Lmu,lchks,lchks2
      INTEGER i,i2,ardsp,Frstry,Idr,Ids,idrf,idsf,Na,ar1r,nparma,
     &        ar1s,ma1r,ma1s,icon,irm1,Nefobs,iround,Id,i3,nelt,info,
     &        nelta,Imu,limrd,limsd,mxitbk,Lsumm
      DIMENSION Trnsrs(PLEN),xpxinv(PB*(PB+1)/2),tmp(2),txy(PLEN),a(*)
      DOUBLE PRECISION tolbak,PTOL,nltbak
      PARAMETER(PTOL=1.0D-3)
c     ------------------------------------------------------------------
      DOUBLE PRECISION dpmpar,totals,sdev,wd,wm
      LOGICAL dpeq
      EXTERNAL strinx,dpmpar,totals,sdev,dpeq
c     ------------------------------------------------------------------
      irm1=0
      inptok=T
      mxitbk=Mxiter
c     ------------------------------------------------------------------
      iround=1
      Id=0
      idrf=0
      idsf=0
      limrd=Idr
      limsd=Ids
      rmaxr=ZERO
      rmaxs=ZERO
c     ------------------------------------------------------------------
c     Set up first model for unit root identification
c     ------------------------------------------------------------------
      DO WHILE(T)
       IF(Prttab(LAUURM))WRITE(Mt1,1010)'(H-R)','Unit Root',iround
       CALL mdlint()
       IF(Id.gt.0.or.iround.GT.1)THEN
        IF(Lseff.or.Sp.eq.1)THEN
         CALL mdlset(1,idrf,1,0,0,0,inptok)
        ELSE
         CALL mdlset(1,idrf,1,1,idsf,1,inptok)
        END IF
        IF(Lfatal)RETURN
        ardsp=Nnsedf+Nseadf
        Nefobs=Nspobs-Nintvl
        i3=3
        IF (Sp.eq.1) i3=2
        ar1r=ardsp+1
        ma1r=ardsp+i3
        ar1s=0
        ma1s=0
        IF (Sp.gt.1) THEN
         ar1s=ar1r+1
         ma1s=ma1r+1
        END IF
       ELSE
        IF(Lseff.or.Sp.eq.1)THEN
         ar1r=Frstar
         ar1s=0
        ELSE IF (Sp.eq.2) THEN
         ar1r=1
         ar1s=1
        ELSE IF (Sp.eq.3.and.Frstar.ge.3) THEN
         ar1r=2
         ar1s=1
        ELSE IF (Sp.eq.4.and.Frstar.eq.4) THEN
         ar1r=3
         ar1s=1
        ELSE
         ar1r=Frstar
         ar1s=1
        END IF
        CALL mdlset(ar1r,0,0,ar1s,0,0,inptok)
        IF(Lfatal)RETURN
        ardsp=0
        Nefobs=Nspobs-Nintvl
       END IF
c     ------------------------------------------------------------------
       IF((.not.inptok).or.Lfatal)THEN
        CALL eWritln('Unable to set up ARIMA model for unit root '//
     &               'testing procedure',STDERR,Mt2,T,F)
        CALL writln('        for the reason(s) given above.',
     &              STDERR,Mt2,F,T)
        IF (.not.Lfatal) CALL abend()
        RETURN
       END IF
c     ------------------------------------------------------------------
c       Difference data, if necessary
c     ------------------------------------------------------------------
       nelta=Nspobs
       CALL copy(Trnsrs,nelta,1,txy)
       IF(id.gt.0)CALL arflt(nelta,Arimap,Arimal,Opr,Mdl(DIFF-1),
     &                       Mdl(DIFF)-1,txy,nelta)
       CALL smeadl(txy,1,nelta,nelta,xmu)
c-----------------------------------------------------------------------
c     Estimate the regression and ARMA parameters
c-----------------------------------------------------------------------
       CALL amdest(txy,nelta,Nefobs,ardsp,T,Prttab(LAUURT),info)
       IF(Lfatal)RETURN
       IF(Prttab(LAUURM).and.info.eq.0)CALL amdprt(ardsp,T,F)
c-----------------------------------------------------------------------
c     If an estimation error is found, discontinue the routine.
c-----------------------------------------------------------------------
       IF(Armaer.eq.PSNGER)THEN
        CALL writln('Estimation error found during unit root testing ',
     &              STDERR,Mt2,T,F)
        CALL writln('procedure while fitting inital regARIMA model '//
     &              'to the series.',STDERR,Mt2,F,T)
        CALL abend()
        RETURN
c-----------------------------------------------------------------------
c     If only a warning message would be printed out, reset the error
c     indicator variable to zero.
c-----------------------------------------------------------------------
       ELSE IF(Armaer.ne.0)THEN
        Armaer=0
       END IF
c-----------------------------------------------------------------------
c     If first round, check to see if roots are inside the unit circle
c-----------------------------------------------------------------------
       IF(iround.eq.1.and.info.eq.0)THEN
        linv=T
        CALL chkrt1(Idr,Ids,rmaxr,rmaxs,linv,Ub1lim)
        IF(Lfatal)RETURN
        IF(.not.linv)THEN
         IF(ar1r.EQ.1.AND.DABS(Arimap(1)).GT.1.02D0)THEN
          info=1
         ELSE IF(ar1r.GT.1)THEN
          info=1
         END IF
         IF (ar1s.EQ.1.AND.DABS(Arimap(ar1r+1)).GT.1.02D0) info=1
        END IF
        IF(info.gt.0)THEN
         DO i=1,ar1r+ar1s
          Arimap(i)=PONE
         END DO
        END IF
       ELSE IF (info.ne.0) THEN
        IF(iround.eq.1)ma1r=ar1r+ar1s+1
        DO i=ar1r,ma1r-1
         Arimap(i)=PONE
        END DO
        IF(iround.gt.1)THEN
         DO i=ma1r,ma1s
          Arimap(i)=PTWO
         END DO
        END IF
       END IF
       IF (irm1.EQ.1) THEN
        info=1
        DO i=ar1r,ma1s
         Arimap(i)=PONE
        END DO
       END IF
       IF (idrf.EQ.0.AND.idsf.EQ.0.AND.iround.GT.2) info=1
       IF (info.ne.0) THEN
        lxar=Lextar
        Lextar=Exdiff.gt.0
        Lar=Lextar.and.Mxarlg.gt.0
c     ------------------------------------------------------------------
        IF(Lextar)THEN
         Nintvl=Mxdflg
         Nextvl=Mxarlg+Mxmalg
         IF(Exdiff.eq.2)Mxiter=TWOHND
c     ------------------------------------------------------------------
        ELSE
         Nintvl=Mxdflg+Mxarlg
         Nextvl=0
        END IF
c     ------------------------------------------------------------------
        IF(Prttab(LAUURM))THEN
         IF(Exdiff.gt.0)THEN
          WRITE(Mt1,1010)'(exact mle)','Unit Root',iround
         ELSE
          WRITE(Mt1,1010)'(conditional)','Unit Root',iround
         END IF
        END IF
c     ------------------------------------------------------------------
c     Loosen tolerance for Exact likelihood model estimation
c     ------------------------------------------------------------------
*        IF(Exdiff)THEN
         IF(Tol.lt.PTOL)THEN
          nltbak=Nltol
          tolbak=Tol
          Tol=PTOL
          Nltol=PTOL
          Nltol0=100D0*Tol
         END IF
*        END IF
c     ------------------------------------------------------------------
        CALL regvar(Trnsrs,Nspobs,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &              Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
*        CALL rgarma(Lestim,Mxiter,Mxnlit,T,a,na,nefobs,inptok)
        CALL rgarma(Lestim,Mxiter,Mxnlit,F,a,na,nefobs,inptok)
        IF(.not.Lfatal)THEN
         IF((.not.Convrg).and.Exdiff.eq.2)THEN
          Nintvl=Mxdflg+Mxarlg
          Nextvl=0
          Lextar=F
          Lar=F
          Mxiter=mxitbk
          IF(Prttab(LAUURM))
     &       WRITE(Mt1,1010)'(conditional)','Unit Root',iround
          CALL regvar(Trnsrs,Nspobs,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &                Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
*          CALL rgarma(Lestim,Mxiter,Mxnlit,T,a,na,nefobs,inptok)
          CALL rgarma(Lestim,Mxiter,Mxnlit,F,a,na,nefobs,inptok)
          IF(Lfatal)RETURN
         END IF
         CALL prterr(nefobs,T)
         IF(.not.Convrg)THEN
          WRITE(STDERR,1020)
          CALL eWritln('Estimation failed to converge during the '//
     &                 'automatic model',Mt1,Mt2,T,F)
          CALL writln(' identification procedure.',Mt1,Mt2,F,T)
          CALL abend()
         ELSE IF(.not.inptok)THEN
          CALL abend()
         END IF
*         IF(Exdiff)THEN
          IF(dpeq(Tol,PTOL))THEN
           Tol=tolbak
           Nltol=nltbak
           Nltol0=Tol*100D0
          END IF
*         END IF
        END IF
        IF(Lfatal)RETURN
        IF(Prttab(LAUURM))CALL amdprt(ardsp,inptok,F)
c     ------------------------------------------------------------------
c     Reset estimation variables to what they were before
c     ------------------------------------------------------------------
        Lextar=lxar
        Lar=Lextar.and.Mxarlg.gt.0
        IF(Lextar)THEN
         Nintvl=Mxdflg
         Nextvl=Mxarlg+Mxmalg
        ELSE
         Nintvl=Mxdflg+Mxarlg
         Nextvl=0
        END IF
c-----------------------------------------------------------------------
        if(iround.eq.1)THEN
         linv=T
         CALL chkrt1(Idr,Ids,rmaxr,rmaxs,linv,Ub1lim)
         IF(Lfatal)RETURN
        END IF
       END IF
c-----------------------------------------------------------------------
c     Check roots and update difference counters
c-----------------------------------------------------------------------
       icon=0
       irm1=0
       IF(iround.eq.1)THEN
        idrf=idrf+Idr
        idsf=idsf+Ids
c-----------------------------------------------------------------------
       ELSE
        din=1.005D0-Ub2lim
        Cancel=Cancel-0.002D0
        IF (Armaer.EQ.0.AND.Ub2lim.GE.0.869D0) din=din+Ub2lim-0.869D0
        IF (DABS(ONE-Arimap(ar1r)).LE.DIN) THEN
         IF (Arimap(ar1r).GT.1.02D0) THEN
          irm1=1
          icon=1
         ELSE IF (DABS(Arimap(ar1r)-Arimap(ma1r)).GT.Cancel) THEN
          icon=icon+1
          idrf=idrf+1
          Id=idrf+idsf*Sp
         END IF
        ELSE IF (DABS(Arimap(ar1r)).GT.1.12D0) THEN
         irm1=1
         icon=1
        END IF
        IF (Sp.GT.1) THEN
         IF (DABS(ONE-Arimap(ar1s)).LE.0.19D0) THEN
          IF (Arimap(ar1s).GT.1.02D0) THEN
           irm1=1
           icon=1
          ELSE IF ((DABS(Arimap(ar1s)-Arimap(ma1s)).GT.Cancel).AND.
     &             (idsf.EQ.0)) THEN
           icon=icon+1
           idsf=idsf+1
           Id=idrf+idsf*Sp
           IF (irm1.EQ.1) THEN
            irm1=0
            icon=icon-1
           END IF
          END IF
         ELSE IF (DABS(Arimap(ar1s)).GT.1.12D0) THEN
          irm1=1
          icon=1
         END IF
        END IF
        lchks=ar1s.gt.0
        IF(lchks)lchks=DABS(Arimap(ar1s)-Arimap(ma1s)).LE.Cancel
        IF (((DABS(Arimap(ar1r)-Arimap(ma1r)).LE.Cancel).OR.lchks)
     &      .AND.iround.EQ.2) THEN
         IF (idrf.EQ.0.AND.idsf.EQ.0.AND.
     &       (rmaxr.GE.PT9.OR.rmaxs.GE.PT9))THEN
          IF (irm1.EQ.1.AND.icon.EQ.1) THEN
           irm1=0
           icon=0
          END IF
          icon=icon+1
          IF (rmaxr.GT.rmaxs) THEN
           idrf=idrf+1
          ELSE
           idsf=idsf+1
          END IF
         END IF
        END IF
        lchks=ar1s.gt.0
        IF(lchks)lchks=DABS(ONE-Arimap(ar1s)).LE.0.16D0
        lchks2=ar1s.gt.0
        IF(lchks)lchks2=DABS(ONE-Arimap(ar1s)).LE.0.17D0
        IF ((iround.EQ.2.AND.idrf.EQ.0.AND.idsf.EQ.0.AND.
     &      (DABS(ONE-Arimap(ar1r)).LE.0.15D0.OR.lchks).AND.
     &      (rmaxr.GE.PT9.OR.rmaxs.GE.0.88D0)).OR.
     &      (iround.EQ.2.AND.idrf.EQ.0.AND.idsf.EQ.0.AND.
     &      (DABS(ONE-Arimap(ar1r)).LE.0.16D0.OR.lchks2).AND.
     &      (rmaxr.GE.0.91D0.OR.rmaxs.GE.0.89D0))) THEN
         IF (irm1.EQ.1.AND.icon.EQ.1) THEN
          irm1=0
          icon=0
         END IF
         icon=icon+1
         IF (rmaxr.GT.rmaxs) THEN
          idrf=idrf+1
         ELSE
          idsf=idsf+1
         END IF
        END IF
        lchks=ar1s.gt.0
        IF(lchks)lchks=DABS(ONE-Arimap(ar1s)).LE.0.25D0
        IF (iround.GE.2.AND.idrf.EQ.0.AND.idsf.EQ.0.AND.
     &      (DABS(ONE-Arimap(ar1r)).LE.0.15D0.OR.lchks)) THEN
         IF (Sp.EQ.1) THEN
          idrf=idrf+1
         ELSE
          rmaxr=Arimap(ar1r)
          rmaxs=Arimap(ar1s)
          IF (rmaxr.GT.rmaxs) THEN
           idrf=idrf+1
          ELSE
           idsf=idsf+1
          END IF
         END IF
        END IF
c-----------------------------------------------------------------------
C INCREASE ONE BY ONE ONLY. POSSIBLE OVERDIFFERENCING
c-----------------------------------------------------------------------
        IF (icon.EQ.2) THEN
         idrf=idrf-1
         idsf=idsf-1
         IF (iround.GE.2.AND.Armaer.gt.0) THEN
          IF (DABS(Arimap(ar1r)).GT.DABS(Arimap(ar1s))) THEN
           idrf=idrf+1
          ELSE
           idsf=idsf+1
          END IF
         ELSE IF (rmaxr.GT.rmaxs) THEN
          IF (rmaxr.GT.-9999.D0) idrf=idrf+1
         ELSE IF (rmaxs.GT.-9999.D0) THEN
          idsf=idsf+1
         END IF
        END IF
       END IF
       IF (iround.GE.1) THEN
        iround=iround+1
c-----------------------------------------------------------------------
c     Check to see if too much differencing is allowed
c-----------------------------------------------------------------------
        IF (idrf.EQ.3) THEN
         idrf=2
         icon=0
        END IF
        IF (idsf.EQ.2) THEN
         idsf=1
         icon=0
        END IF
c-----------------------------------------------------------------------
c     Update Id and see if we can leave
c-----------------------------------------------------------------------
        Id=idrf+idsf*Sp
        IF(idrf.gt.limrd)THEN
         idrf=limrd
         WRITE(Mt1,1030)'Regular',idrf
         GO TO 30
        END IF
        IF(idsf.gt.limsd)THEN
         idsf=limsd
         WRITE(Mt1,1030)'Seasonal',idsf
         GO TO 30
        END IF
        IF ((icon.GE.1.AND.iround.LE.7).OR.iround.EQ.2) GO TO 10
       END IF
   30  IF(Prttab(LAUURT))THEN
        CALL mkPOneLine(Mt1,'@','Results of Unit Root Test for '//
     &                  'identifying orders of differencing:')
        CALL mkPClass(Mt1,'indent')
        WRITE(Mt1,1040)'Regular',idrf
        IF(Sp.gt.1)WRITE(Mt1,1040)'Seasonal',idsf
        CALL writTag(Mt1,'</p>')
       END IF
       IF(Svldif)THEN
        Inlgfl=Inlgfl+1
        WRITE(Ng,1000)Inlgfl
        CALL mkTableTag(Ng,'w60','Results of Unit Root Test for '//
     &                  'identifying orders of differencing')
        CALL mkCaption(Ng,'Orders of Differencing Identified')
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &                        'Regular difference order')
        WRITE(Ng,1050)idrf
        IF(Sp.gt.1)THEN
         CALL mkHeaderCellScope(Ng,0,0,'row','@',
     &                          'Seasonal difference order')
         WRITE(Ng,1050)idsf
        END IF
        CALL writTag(Ng,'</tr>')
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
       IF(Lsumm.gt.0)THEN
        WRITE(Nform,1060)'idnonseasonaldiff.first',idrf
        IF(Sp.gt.1)WRITE(Nform,1060)'idseasonaldiff.first',idsf
       END IF
c-----------------------------------------------------------------------
c     Check to see if mean regressor is significant by creating
c     t-value of residuals if no differencing found
c-----------------------------------------------------------------------
       IF(Imu.eq.0.and.Lchkmu)THEN
        Lmu=T
        IF(id.eq.0)THEN
         wm=totals(Trnsrs,1,Nspobs,1,1)
         wd=sdev(Trnsrs,1,Nspobs,1,1)
         tval=dsqrt(dble(Nspobs))*wm/wd
         IF (Nspobs.LE.80) THEN
          vct=1.96D0
         ELSE IF (Nspobs.GT.80.AND.Nspobs.LE.200) THEN
          vct=2.0D0
         ELSE
          vct=2.55D0
         END IF
         IF(DABS(tval).lt.vct)Lmu=F
        ELSE
c-----------------------------------------------------------------------
c     Else, check to see if mean regressor is significant by adding a
c     constant regressor and generating a t statistic
c-----------------------------------------------------------------------
         CALL adrgef(DNOTST,'Constant','Constant',PRGTCN,F,F)
         IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     Generate regression matrix
c     ------------------------------------------------------------------
*         CALL mdlint()
*         CALL mdlset(0,idrf,0,0,idsf,0,inptok)
*         IF(Lfatal)RETURN
         nelta=Nspobs
         CALL copy(Trnsrs,nelta,1,txy)
         CALL regvar(txy,nelta,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &               Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
         IF(Lfatal)RETURN
c     ------------------------------------------------------------------
**         CALL setdp(ZERO,PARIMA,Arimap)
c     ------------------------------------------------------------------
c     Initialize ARIMA parameters to 0.1 rather than 0 -
c     BCM January 2007, Revised July 2008
c     ------------------------------------------------------------------
         i=1
         nparma=Mdl(MA)
         DO WHILE (i.le.nparma)
          IF(.not.Arimaf(i))Arimap(i)=0.1D0
          i=i+1
         END DO
         IF(Tol.lt.PTOL)THEN
          nltbak=Nltol
          tolbak=Tol
          Tol=PTOL
          Nltol=PTOL
          Nltol0=100D0*Tol
         END IF
*         CALL rgarma(Lestim,Mxiter,Mxnlit,T,a,na,nefobs,inptok)
         CALL rgarma(Lestim,Mxiter,Mxnlit,F,a,na,nefobs,inptok)
         IF(.not.Lfatal)THEN
          CALL prterr(nefobs,T)
          IF(.not.Convrg)THEN
           WRITE(STDERR,1020)
           CALL eWritln('Estimation failed to converge during the '//
     &                  'automatic model identification procedure.',
     &                  Mt1,Mt2,T,T)
           CALL abend()
          ELSE IF(.not.inptok)THEN
           CALL abend()
          END IF
         END IF
         IF(Lfatal)RETURN
         IF(dpeq(Tol,PTOL))THEN
          Tol=tolbak
          Nltol=nltbak
          Nltol0=Tol*100D0
         END IF
         nelt=Ncxy*(Ncxy+1)/2
         IF(Var.gt.2D0*dpmpar(1))THEN
          rmse=sqrt(Var)
          CALL copy(Chlxpx,nelt,1,xpxinv)
          CALL dppdi(xpxinv,Nb,tmp,1)
         ELSE
          rmse=ZERO
         END IF
c-----------------------------------------------------------------------
c     compute standard error and t-value
c-----------------------------------------------------------------------
         seb=sqrt(xpxinv(1))*rmse
         tval=ZERO
         IF(seb.gt.ZERO)tval=B(1)/seb
         IF (Nspobs.LE.80) THEN
          vct=1.96D0
         ELSE IF (Nspobs.GT.80.AND.Nspobs.LE.155) THEN
          vct=1.98D0
         ELSE IF (Nspobs.GT.155.AND.Nspobs.LE.230) THEN
          vct=2.1D0
         ELSE IF (Nspobs.GT.230.AND.Nspobs.LE.350) THEN
          vct=2.3D0
         ELSE
          vct=2.5D0
         END IF
         IF (DABS(tval).LT.vct) THEN
          IF(Prttab(LAUURT))
     &       CALL mkPOneLine(Mt1,'@','Mean is not significant.')
          Lmu=F
         ELSE IF(Prttab(LAUURT)) THEN
          CALL mkPOneLine(Mt1,'@','Mean is significant.')
         END IF
         i2=1
         CALL dlrgef(i2,Nrxy,1)
         IF(Lfatal)RETURN
        END IF
       END IF
       GO TO 20
   10  CONTINUE
      END DO
   20 Idr=idrf
      Ids=idsf
      Mxiter=mxitbk
c-----------------------------------------------------------------------
 1000 FORMAT('<div id="lgidd',i6.6,'">')
 1010 FORMAT(/,'<p><strong>ARIMA Estimates ',a,' for ',a,
     &         ' Identification :</strong> Model No. ',i2,'</p>')
c     Change format of warning message (BCM 10-14-2008)
 1020 FORMAT(/,' ERROR: Estimation failed to converge during the ',
     &         'automatic model',/
     &         '        identification procedure.')
 1030 FORMAT(/,'<p>',a,' difference order reset to ',i1,', the limit ',
     &         'specified in the maxdiff argument.</p>')
 1040 FORMAT(a,' difference order : ',i3)
 1050 FORMAT('<td class="center">',i3,'</td>')
 1060 FORMAT(a,': ',i3)
c-----------------------------------------------------------------------
      END
