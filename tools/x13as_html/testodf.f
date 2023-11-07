      SUBROUTINE tstodf(Trnsrs,Frstry,Nefobs,A,Na,Lsumm,lpr,ldr,lqr,
     &                  lps,lds,lqs,Kstep,Lidotl,Lnoprt,FctOK,redoMD,
     &                  argok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'mdldg.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'mdltbl.i'
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'mdlsvl.i'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'extend.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION MALIM,ONE,TWO,ZERO,PT5
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.,MALIM=0.001D0,ONE=1D0,TWO=2D0,
     &          ZERO=0D0,PT5=0.05D0)
c-----------------------------------------------------------------------
      CHARACTER effttl*(PCOLCR),cmonth*3,ordend*2
      DOUBLE PRECISION Trnsrs,sumMA,A,xpxinv,tmp
      INTEGER disp,lpr,ldr,lps,lds,lqr,lqs,Frstry,Nefobs,Na,Lsumm,spm1,
     &        cnote,i,ipos,fh0,icol,icol1,igrp,begcol,endcol,regidx,
     &        nb2,nfix,nchr,nelt,j,Kstep
      LOGICAL Lidotl,Lnoprt,FctOK,redoMd,reReDoMd,argok
      DIMENSION Trnsrs(PLEN),A(*),cmonth(12),ordend(10),xpxinv(PXPX),
     &          regidx(PB),tmp(2)
c     ------------------------------------------------------------------
      DATA cmonth/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep',
     &     'Oct','Nov','Dec'/
      DATA ordend/'th','st','nd','rd','th','th','th','th','th','th'/
c     ------------------------------------------------------------------
      DOUBLE PRECISION dpmpar
      INTEGER strinx
      EXTERNAL dpmpar,strinx
c-----------------------------------------------------------------------
c   If model has nonseasonal diffencing and MA, check for
c   overdifferencing by seeing if sum of MA parameters is close to 1.
c-----------------------------------------------------------------------
      redoMd=F
      reReDoMd=F
      IF (ldr.gt.0 .and. lqr.gt.0) THEN
       IF(Prttab(LAUFNT))THEN
        CALL mkPOneLine(Mt1,'@','&nbsp;')
        CALL mkPOneLine(Mt1,'@',
     &                  'Checking for nonseasonal overdifferencing.')
       END IF
       disp=lpr+ldr+lps+lds
       sumMa=ZERO
       do i = disp+1,disp+lqr
        sumMa = sumMa + Arimap(i)
       end do
c-----------------------------------------------------------------------
c   Reduce by one the number of nonseasonal differences, nonseasonal
c   MA terms in the model, and add a constant term.
c-----------------------------------------------------------------------
       IF (ABS(sumMA-ONE).lt.MALIM) THEN
        redoMd=T
        IF(Prttab(LAUFNT))CALL mkPOneLine(Mt1,'@',
     &     'Reduce order of nonseasonal <abbr title="moving average">'//
     &     'MA</abbr>, nonseasonal differencing.')
        ldr=ldr-1
        lqr=lqr-1
        CALL mkPClass(Mt1,'indent')
        IF(Prttab(LAUFNT))WRITE(Mt1,1010) lpr,ldr,lqr,lps,lds,lqs
        CALL writTag(Mt1,'</p>')
        icol=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Constant')
        IF(icol.eq.0)THEN
         IF(Prttab(LAUFNT))
     &      CALL mkPOneLine(Mt1,'@','Add constant term.')
         CALL adrgef(DNOTST,'Constant','Constant',PRGTCN,F,F)
         IF(Lfatal)RETURN
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
c   If model has seasonal diffencing and MA, check for
c   overdifferencing by seeing if sum of seasonal MA parameters is 
c   close to 1.
c-----------------------------------------------------------------------
      IF (lds.gt.0 .and. lqs.gt.0 .and. Lsovdf) THEN
       IF(Prttab(LAUFNT))THEN
        CALL mkPOneLine(Mt1,'@','&nbsp;')
        CALL mkPOneLine(Mt1,'@',
     &                  'Checking for seasonal overdifferencing.')
       END IF
       disp=lpr+ldr+lps+lds+lqr
       sumMa=ZERO
       do i = disp+1,disp+lqs
        sumMa = sumMa + Arimap(i)
       end do
c-----------------------------------------------------------------------
c   Reduce by one the number of nonseasonal differences, nonseasonal
c   MA terms in the model, and add a constant term.
c-----------------------------------------------------------------------
       IF (ABS(sumMA-ONE).lt.MALIM) THEN
        IF(.not.redoMd)redoMd=T
        IF(Prttab(LAUFNT))CALL mkPOneLine(Mt1,'@',
     &     'Reduce order of seasonal <abbr title="moving average">'//
     &     'MA</abbr>, seasonal differencing.')
        lds=lds-1
        lqs=lqs-1
        CALL mkPClass(Mt1,'indent')
        IF(Prttab(LAUFNT))WRITE(Mt1,1010) lpr,ldr,lqr,lps,lds,lqs
        CALL writTag(Mt1,'</p>')
         IF(Prttab(LAUFNT))
     &      CALL mkPOneLine(Mt1,'@','Add seasonal regressors.')
         spm1=Sp-1
c     ------------------------------------------------------------------
         IF(Sp.eq.12)THEN
          DO i=1,spm1
           effttl=cmonth(i)
           nchr=3
           CALL adrgef(DNOTST,effttl(1:nchr),'Seasonal',PRGTSE,F,T)
           IF(Lfatal)RETURN
          END DO
c     ------------------------------------------------------------------
         ELSE
          DO i=1,spm1
           ipos=1
           CALL itoc(i,effttl,ipos)
           IF(Lfatal)RETURN
           IF(mod(i,100).ge.11.and.mod(i,100).le.13)THEN
            effttl(ipos:ipos+1)='th'
           ELSE
            effttl(ipos:ipos+1)=ordend(mod(i,10))
           END IF
           nchr=ipos+1
           CALL adrgef(DNOTST,effttl(1:nchr),'Seasonal',PRGTSE,F,T)
           IF(Lfatal)RETURN
          END DO
         END IF
         IF(Lfatal)RETURN
       END IF
      END IF
c-----------------------------------------------------------------------
c   re-estimate model.
c-----------------------------------------------------------------------
      IF(redoMD)THEN
       CALL mdlint()
       CALL mdlset(lpr,ldr,lqr,lps,lds,lqs,argok)
       IF(.not.Lfatal)
     &    CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &                Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c   If automatic outliers are identified for the model, eliminate the
c   outliers from the model (BCM April 2007)
c-----------------------------------------------------------------------
       IF(Natotl.gt.0)THEN
        CALL clrotl(Nrxy)
        IF(.not.Lfatal)
     &     CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,
     &                 Nrusrx,Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
        IF(Lfatal)RETURN
       END IF
c-----------------------------------------------------------------------
       CALL rgarma(Lestim,Mxiter,Mxnlit,F,a,na,nefobs,argok)
       IF(.not.Lfatal)THEN
        CALL prterr(nefobs,T)
        IF(.not.Convrg)THEN
         WRITE(STDERR,1020)
         fh0=0
         IF(Prttab(LAUFNT))fh0=Mt1
         CALL eWritln('Estimation failed to converge during the '//
     &                'automatic model identification procedure.',
     &                fh0,Mt2,T,T)
         CALL abend()
        ELSE IF(.not.argok)THEN
         CALL abend()
        END IF
       END IF
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c   Redo automatic outlier identification (BCM April 2007)
c-----------------------------------------------------------------------
       IF(Lidotl.and.(.not.Lotmod))THEN
        CALL amidot(A,Trnsrs,Frstry,Nefobs,Priadj,Convrg,Fctok,Argok)
        IF(Lfatal)RETURN
       END IF
c-----------------------------------------------------------------------
c   Check if mean term added is significant.
c   If not, remove and re-estimate model.
c   Added by B C Monsell Aug 2018
c-----------------------------------------------------------------------
       IF(Lchkmu)THEN
        CALL chkmu(Trnsrs,A,Nefobs,Na,Frstry,kstep,Prttab(LAUFNT))
        IF(Lfatal)RETURN
        icol1=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Constant')
        IF(icol1.eq.0)reReDoMd=T
       END IF
c-----------------------------------------------------------------------
c   Check if seasonal regressors added are significant.
c   If not, remove and re-estimate model.
c   Added by B C Monsell Aug 2018
c-----------------------------------------------------------------------
       IF(Lsovdf)THEN
        icol=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Seasonal')
        IF(icol.gt.0)THEN
c     ------------------------------------------------------------------
c     Generate number of unfixed regressors
c     ------------------------------------------------------------------
         nb2=Nb
         IF(Iregfx.ge.2)THEN
          DO j=1,Nb
           IF(Regfx(j))nb2=nb2-1
          END DO
         END IF
c-----------------------------------------------------------------------
c     Get the X'X inverse.
c-----------------------------------------------------------------------
         IF(nb2.gt.0)THEN
          nelt=(nb2+1)*(nb2+2)/2
          IF(Var.gt.TWO*dpmpar(1))THEN
           CALL copy(Chlxpx,nelt,1,xpxinv)
           CALL dppdi(xpxinv,nb2,tmp,1)
          END IF
         END IF
c-----------------------------------------------------------------------
c     set up regidx variable
c-----------------------------------------------------------------------
         nfix=0
         DO igrp=1,Ngrp
          begcol=Grp(igrp-1)
          endcol=Grp(igrp)-1
          DO icol=begcol,endcol
           IF(Regfx(icol))THEN
            nfix=nfix+1
            regidx(icol)=NOTSET
           ELSE
            regidx(icol)=icol-nfix
           END IF
          END DO
         END DO
c-----------------------------------------------------------------------
c     Generate F-test for seasonal F-test
c-----------------------------------------------------------------------
         CALL sftest(Xpxinv,Regidx,Prttab(LAUFNT),F,F,F)
c-----------------------------------------------------------------------
c     generate p-value limit, 
c     remove seasonal regressors if not significant
c-----------------------------------------------------------------------
         IF(Sfpv.gt.PT5)THEN
          igrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Seasonal')
          begcol=Grp(igrp-1)
          endcol=Grp(igrp)-1
          CALL dlrgef(begcol,Nrxy,endcol-begcol+1)
          IF(Prttab(LAUFNT))CALL mkPOneLine(Mt1,'@',
     &                        'Seasonal regressors removed from model')
         END IF
         icol1=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Seasonal')
         IF(icol1.eq.0)reReDoMd=T
        END IF
       END IF
c-----------------------------------------------------------------------
c      Re-estimate model if model has changed
c-----------------------------------------------------------------------
       IF(reReDoMd)THEN
        CALL rgarma(Lestim,Mxiter,Mxnlit,F,a,na,nefobs,argok)
        IF(.not.Lfatal)THEN
         CALL prterr(nefobs,T)
         IF(.not.Convrg)THEN
          WRITE(STDERR,1020)
          fh0=0
          IF(Prttab(LAUFNT))fh0=Mt1
          CALL eWritln('Estimation failed to converge during the '//
     &                 'automatic model identification procedure.',
     &                 fh0,Mt2,T,T)
          CALL abend()
         ELSE IF(.not.argok)THEN
          CALL abend()
         END IF
        END IF
        IF(Lfatal)RETURN
       END IF
c-----------------------------------------------------------------------
      ELSE
       IF(Prttab(LAUFNT))WRITE(Mt1,1030)MALIM
      END IF
c-----------------------------------------------------------------------
 1010 FORMAT('  ',2(' (',i2,',',i2,',',i2,')'))
 1020 FORMAT(/,' ERROR: Estimation failed to converge during the ',
     &         'automatic model',
     &       /,'        identification procedure.')
 1030 FORMAT('<p>Nonseasonal <abbr title="moving average">MA</abbr>',
     &       ' not within ',f6.3,' of 1.0 - model passes test.</p>')
c-----------------------------------------------------------------------
      RETURN
      END