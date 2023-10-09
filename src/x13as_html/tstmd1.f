C     Last change:  SRD  31 Jan 100    7:10 am
      SUBROUTINE tstmd1(Trnsrs,Frstry,A,Na,Nefobs,Pdfm,Rsddfm,Rtval,
     &                  Lpr,Lps,Lqr,Lqs,Ldr,Lds,Lmu,Lprt,Aici0,Pcktd0,
     &                  Adj0,Trns0,Tair)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER (T=.true.,F=.false.)
c     ------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'arima.cmn'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'priusr.cmn'
      INCLUDE 'adj.cmn'
      INCLUDE 'inpt.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'htmlout.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION A,tval,cval,Pdfm,Rsddfm,Rtval,rtv,fct0,pami,blq,
     &                 rsdami,fct2,Trnsrs,Tair,Adj0,Trns0
      LOGICAL pcktd0,inptok,Lmu,Lprt
      INTEGER ipr,ips,idr,ids,iqr,iqs,id,ip,iq,iprs,iqrs,n,iopr,
     &        Aici0,ilag,dipr,dips,diqr,diqs,Ldr,Lds,bldf,fh0,
     &        Frstry,Na,Nefobs,iround,Lpr,Lps,Lqr,Lqs,ichk,i1dfm,i2dfm
      DIMENSION tval(PARIMA),A(PLEN+2*PORDER),Trnsrs(PLEN),Tair(2),
     &          Adj0(*),Trns0(*)
c     ------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c     ------------------------------------------------------------------
c       Convert X-13ARIMA-SEATS model variables into variables 
c       compatable with TRAMO/SEATS model data structure
c     ------------------------------------------------------------------
      CALL cnvmdl(ipr,ips,idr,ids,iqr,iqs,id,ip,iq,iprs,iqrs,n)
      IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     Check to see if default model had at least one significant
c     coefficient.  If not, exit
c     ------------------------------------------------------------------
      i1dfm=1
      IF (Sp.GT.1) THEN
       i2dfm=1
      ELSE IF (Sp.EQ.1) THEN
       i2dfm=0
      END IF
      IF (DABS(Tair(1)).LT.1.96D0) i1dfm=0
      IF (Sp.GT.1.AND.DABS(Tair(2)).LT.1.96D0) i2dfm=0
      IF ((idr.EQ.1.AND.ids.EQ.1.AND.Sp.GT.1.AND.iqr.EQ.1.AND.
     &    iqs.EQ.1.AND.ipr.EQ.0.AND.ips.EQ.0).OR.
     &    (idr.EQ.1.AND.Sp.EQ.1.AND.iqr.EQ.1.AND.ipr.EQ.0).OR.
     &    (i1dfm+i2dfm.EQ.0)) RETURN
c     ------------------------------------------------------------------
c     Estimate model
c     ------------------------------------------------------------------
      inptok=T
      iround=1
   10 CALL rgarma(T,Mxiter,Mxnlit,F,A,Na,Nefobs,inptok)
      IF(Lfatal)RETURN
      IF(iround.gt.1)CALL ssprep(T,F,F)
c-----------------------------------------------------------------------
c     generate t-statistics for ARMA parameters
c-----------------------------------------------------------------------
      CALL armats(tval)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Check to see if there are insig lags
c     if so, decrease model order.
c     continue testing until model has no insig lags
c-----------------------------------------------------------------------
      iopr=0
      cval=1.8D0
      dipr=0
      dips=0
      diqr=0
      diqs=0
      DO WHILE (T)
       ilag=0
       IF(ipr.gt.dipr)THEN
        IF(dabs(tval(ipr-dipr)).lt.cval)THEN
         dipr=dipr+1
         ilag=ilag+1
        END IF
       END IF
       IF(ips.gt.dips)THEN
        IF(dabs(tval(iprs-dips)).lt.cval)THEN
         dips=dips+1
         ilag=ilag+1
        END IF
       END IF
       IF(iqr.gt.diqr)THEN
        IF(dabs(tval(iprs+iqr-diqr)).lt.cval)THEN
         diqr=diqr+1
         ilag=ilag+1
        END IF
       END IF
       IF(ips.gt.dips)THEN
        IF(dabs(tval(iprs+iqrs-diqs)).lt.cval)THEN
         diqs=diqs+1
         ilag=ilag+1
        END IF
       END IF
       IF(ilag.lt.1)GO TO 9999
      END DO
 9999 iopr=iopr+dipr+dips+diqr+diqs
      IF(iopr.eq.1.and.iprs+iqrs.gt.0.and.iround.eq.1)THEN
       iround=iround+1
       IF (dipr.EQ.1) THEN
        ipr=ipr-1
       ELSE IF (dips.EQ.1) THEN
        ips=ips-1
       ELSE IF (diqr.EQ.1) THEN
        iqr=iqr-1
       ELSE
        iqs=iqs-1
       END IF
c-----------------------------------------------------------------------
c     initialize X-13A-S model stats with new model order so the new 
c     model can be estimated.
c-----------------------------------------------------------------------
       inptok=T
       CALL mdlint()
       CALL mdlset(ipr,idr,iqr,ips,ids,iqs,inptok)
       IF(.not.Lfatal.and.inptok)GO TO 10
       RETURN
      END IF
c-----------------------------------------------------------------------
      IF(iround.gt.1)THEN
       IF(Lprt)THEN
        WRITE(Mt1,1010)
     &        '<p>Due to insignificant <abbr title="autoregressive '//
     &          'moving average">ARMA</abbr> coefficients, model '//
     &           'changed to'//Cbr
        WRITE(Mt1,1020) ipr,idr,iqr,ips,ids,iqs
        CALL writTag(Mt1,'</p>')
       END IF
       CALL mkmdsn(ipr,idr,iqr,ips,ids,iqs,Bstdsn,Nbstds)
       IF(Lfatal)RETURN
       IF(Lpr.ne.ipr)Lpr=ipr
       IF(Lps.ne.ips)Lps=ips
       IF(Lqr.ne.iqr)Lqr=iqr
       IF(Lqs.ne.iqs)Lqs=iqs
       IF(Ldr.ne.idr)Ldr=idr
       IF(Lds.ne.ids)Lds=ids
      END IF
c-----------------------------------------------------------------------
c     generate residual statisics
c-----------------------------------------------------------------------
      CALL mdlchk(A,Na,Nefobs,pami,blq,bldf,rsdami,rtv)
      fct2=1.0D0
      fct0=1.025D0
      ichk=0
      IF(pami.LT..95D0.AND.Pdfm.LT..75D0.AND.rsddfm.LT.rsdami)THEN
*         write(Mt1,991)pami,rsdami,pdfm,rsddfm
*  991    format('  pami = ',f10.3,'  rsdami = ',e15.10,/,
*     &          '  pdfm = ',f10.3,'  rsddfm = ',e15.10)
         ichk=1
      ELSE IF(pami.LT..95D0.AND.Pdfm.LT..75D0.AND.Pdfm.LT.pami.AND.
     &     rsddfm.LT.fct0*rsdami)THEN
*         write(Mt1,992)pami,rsdami,fct0,pdfm,rsddfm
*  992    format('  pami = ',f10.3,'  rsdami = ',e15.10,'  fct0 = ',f5.3,
*     &        /,'  pdfm = ',f10.3,'  rsddfm = ',e15.10)
         ichk=2
      ELSE IF(pami.GE..95D0.AND.Pdfm.LT..95D0.AND.
     &          rsddfm.LT.fct2*rsdami)THEN
*         write(Mt1,993)pami,rsdami,fct0,pdfm,rsddfm
*  993    format('  pami = ',f10.3,'  rsdami = ',e15.10,'  fct2 = ',f5.3,
*     &        /,'  pdfm = ',f10.3,'  rsddfm = ',e15.10)
         ichk=3
      ELSE IF(Idr.EQ.0.AND.Ids.EQ.1.AND.Ipr.EQ.1.AND.Arimap(2).GE.
     &     0.82D0.AND.Ips.EQ.0.AND.Iqr.LE.1.AND.Iqs.EQ.1)THEN
*         write(Mt1,994)Arimap(2)
*  994    format('  arimap(2) = ',f10.5)
         ichk=4
      ELSE IF(Idr.EQ.1.AND.Ids.EQ.0.AND.Ipr.EQ.0.AND.Arimap(2).GE.
     &     0.65D0.AND.Ips.EQ.1.AND.Iqr.EQ.1.AND.Iqs.LE.1)THEN
*         write(Mt1,994)Arimap(2)
         ichk=5
      END IF
c     &     iround.eq.1.and.iopr.gt.1)THEN
      IF(ichk.eq.0)THEN
*         write(Mt1,991)pami,rsdami,pdfm,rsddfm
      ELSE IF(ichk.gt.0)THEN
*         WRITE(Mt1,1000)ichk
* 1000    FORMAT(' ichk (auto versus default model) = ',i3)
       idr=1
       ids=1
       ipr=0
       ips=0
       iqr=1
       iqs=1
       IF(Sp.EQ.1)THEN
        ids=0
        iqs=0
       END IF
       IF(Lpr.ne.ipr)Lpr=ipr
       IF(Lps.ne.ips)Lps=ips
       IF(Lqr.ne.iqr)Lqr=iqr
       IF(Lqs.ne.iqs)Lqs=iqs
       IF(Ldr.ne.idr)Ldr=idr
       IF(Lds.ne.ids)Lds=ids
       inptok=T
       CALL mdlint()
       CALL mdlset(ipr,idr,iqr,ips,ids,iqs,inptok)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
       IF((Pcktd0.and.(.not.Picktd)).or.
     &     ((.not.Pcktd0).and.Picktd))THEN
        CALL copy(adj0,PLEN,1,Adj)
        CALL copy(trns0,PLEN,1,Trnsrs)
        CALL copy(Adj,Nadj,-1,Sprior(Setpri))
        IF(.not.(Fcntyp.eq.4.OR.dpeq(Lam,1D0)))THEN
         IF(Pcktd0)THEN
          IF(Kfmt.eq.0)Kfmt=1
          IF(.not.Lpradj)Lpradj=T
         ELSE
          IF(Nustad.eq.0.and.Nuspad.eq.0)THEN
           Kfmt=0
           IF(Lpradj)Lpradj=F
          END IF
         END IF
        END IF
       END IF
       Aicind=Aici0
c-----------------------------------------------------------------------
       CALL bkdfmd(F)
       CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &             Priadj,Reglom,Nrxy,Begxy,Frstry,T,F)
       IF(Lprt)THEN
        WRITE(Mt1,1010)'<p>Model changed to default model'//Cbr
        WRITE(Mt1,1020) Ipr,Idr,Iqr,Ips,Ids,Iqs
        CALL writTag(Mt1,'</p>')
       END IF
       CALL mkmdsn(ipr,idr,iqr,ips,ids,iqs,Bstdsn,Nbstds)
       IF(Lfatal)RETURN
       IF((.not.Lmu).and.Rtval.gt.1.96D0)THEN
        IF(Lprt)CALL mkPOneLine(Mt1,'@','Mean is signficant.')
        IF(Lchkmu)THEN
         Lmu=T
         CALL adrgef(DNOTST,'Constant','Constant',PRGTCN,F,F)
         IF(.not.Lfatal)
     &      CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,
     &                  Nrusrx,Priadj,Reglom,Nrxy,Begxy,Frstry,T,F)
         IF(.not.Lfatal)
     &      CALL rgarma(T,Mxiter,Mxnlit,F,A,Na,Nefobs,inptok)
         IF(.not.Lfatal)CALL prterr(Nefobs,T)
         IF(Lfatal)RETURN
        END IF
        CALL ssprep(T,F,F)
       ELSE
        fh0=0
        IF(Lprt)fh0=Mt1
        CALL wWritln('A significant mean term will not be added '//
     &               'to the model since',fh0,Mt2,T,F)
        CALL writln('the automdl argument checkmu was set to no '//
     &              'in the input specification file.',fh0,Mt2,F,T)
       END IF
      END IF
c-----------------------------------------------------------------------
      RETURN
c-----------------------------------------------------------------------
 1010 FORMAT(a)
 1020 FORMAT('  ',2(' (',i2,',',i2,',',i2,')'))
c-----------------------------------------------------------------------
      END
c-----------------------------------------------------------------------
