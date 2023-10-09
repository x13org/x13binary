C     Last change:  BCM  15 Apr 2005   11:46 am
      SUBROUTINE x11pt1(Lmodel,Lgraf,Lgrfxr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This subroutine performs pre-adjustments for holiday, prior
c     adjustment factors and prior trading day.
c-----------------------------------------------------------------------
c     add backcast saving bcm october 2006
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'agr.cmn'
      INCLUDE 'adj.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11fac.cmn'
      INCLUDE 'xrgtbl.i'
      INCLUDE 'cmptbl.i'
      INCLUDE 'mdltbl.i'
      INCLUDE 'tbllog.prm'
      INCLUDE 'filext.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'inpt.cmn'
      INCLUDE 'x11msc.cmn'
      INCLUDE 'x11reg.cmn'
      INCLUDE 'x11log.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'priusr.cmn'
      INCLUDE 'missng.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'orisrs.cmn'
      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c-----------------------------------------------------------------------
      DOUBLE PRECISION std,dvec
      INTEGER i,n2,fext,fplt,lastpr,phol,lasttd,frsttd
      LOGICAL Lmodel,aorb1,Lgraf,mvind,Lgrfxr
      DIMENSION dvec(1),std(PLEN),mvind(PLEN)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      INTEGER nblank
      EXTERNAL dpeq,nblank
c-----------------------------------------------------------------------
      INCLUDE 'filext.var'
c-----------------------------------------------------------------------
      IF(Muladd.eq.2)Muladd=0
      dvec(1)=0D0
c-----------------------------------------------------------------------
C --- PART A.
c-----------------------------------------------------------------------
      Kpart=1
c-----------------------------------------------------------------------
c     Set indicators for missing values.
c-----------------------------------------------------------------------
      CALL setlg(F,PLEN,mvind)
      IF(Missng)THEN
       DO i=Pos1ob,Posfob
        IF(dpeq(Series(i),Mvval))mvind(i)=T
       END DO
      END IF
c-----------------------------------------------------------------------
C --- SET STO and Stcsi EQUAL TO INPUT SERIES.
c-----------------------------------------------------------------------
      Nspobs=Nofpob-Nfdrp
      CALL copy(Series(Pos1ob),Nspobs,-1,Stcsi(Pos1ob))
      CALL copy(Series,Posfob,-1,Stoap)
      CALL copy(Series,Posfob,-1,Stopp)
      CALL copy(Series,Posfob,-1,Stocal)
c      CALL dfdate(Begspn,Begsrs,Sp,Frstsy)
c      Frstsy=Frstsy+1
c      Nomnfy=Nobs-Frstsy+1
c      Nobspf=min(Nspobs+max(Nfcst-Fctdrp,0),Nobs-Frstsy+1)
*      write(Mtprof,*) ' Orig(Pos1ob) = ',Orig(Pos1ob)
      CALL copy(Orig(Pos1ob),Nomnfy,-1,Sto(Pos1ob))
*      write(Mtprof,*) ' Sto(Pos1ob) = ',Sto(Pos1ob)
*      IF(Lmvaft.or.Ln0aft)THEN
*       CALL copy(Orig(Pos1ob),Nspobs,-1,Sto(Pos1ob))
*      ELSE
*       CALL copy(Orig(Pos1ob),Nobspf,-1,Sto(Pos1ob))
*      END IF
      lastpr=Nofpob
      IF(Pos1ob.gt.1)lastpr=lastpr+Pos1ob-1
c-----------------------------------------------------------------------
C --- WRITE UNADJUSTED ORIGINAL SERIES A1.
c-----------------------------------------------------------------------
c     Set logical variables to determine if original series should be
c     printed, saved, or graphed
c-----------------------------------------------------------------------
      fext=LSRSSP
      fplt=LSRA1P
      IF(Iagr.eq.3)THEN
       fext=LCMPA1
       fplt=LCPA1P
      END IF
c-----------------------------------------------------------------------
c     Print out original series.
c-----------------------------------------------------------------------
      IF(.not.dpeq(Cnstnt,DNOTST))THEN
       DO i=Pos1ob,Posffc
        IF(.not.mvind(i))Series(i)=Series(i)-Cnstnt
       END DO
      END IF
      aorb1=.not.(Khol.eq.2.or.Ixreg.eq.3)
      IF(Prttab(fext).and.aorb1)THEN
       IF(Ny.eq.4.or.Ny.eq.12)THEN
        CALL table(Series,Pos1ob,Posfob,1,1,2,dvec,fext)
       ELSE
        CALL genSkip(fext)
        CALL prtshd('Data for regARIMA modeling',Begspn,Sp,
     &              Posfob-Pos1ob+1)
        CALL prttbl(Begspn,Sp,Series(Pos1ob),Posfob-Pos1ob+1,'Data',
     &              Kdec,tbxdic(fext)(1:nblank(tbxdic(fext))))
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Lgraf)CALL punch(Series,Pos1ob,Posfob,fext,Lgraf,F)
      IF((.not.Lfatal).and.aorb1.and.Savtab(fext))
     &   CALL punch(Series,Pos1ob,Posfob,fext,F,F)
      IF((.not.Lfatal).and.Prttab(fplt).and.(Ny.eq.4.or.Ny.eq.12).and.
     &   aorb1)
     &   CALL x11plt(Series,Series,Pos1ob,Posfob,fplt,0,0,6,1)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Print series with constant added (added by BCM - July 2005)
c-----------------------------------------------------------------------
      IF(.not.dpeq(Cnstnt,DNOTST))THEN
       DO i=Pos1ob,Posffc
        IF(.not.mvind(i))Series(i)=Series(i)+Cnstnt
       END DO
       IF(Prttab(LTRSCN))THEN
        IF(Ny.eq.4.or.Ny.eq.12)THEN
         CALL table(Series,Pos1ob,Posfob,1,1,2,dvec,LTRSCN)
        ELSE
         CALL genSkip(LTRSCN)
         CALL prtshd('Original Series with Constant Added',
     &               Begspn,Sp,Posfob-Pos1ob+1)
         CALL prttbl(Begspn,Sp,Series(Pos1ob),Posfob-Pos1ob+1,'Data',
     &               Kdec,'ori.constant.data')
        END IF
       END IF
c-----------------------------------------------------------------------
       IF((.not.Lfatal).and.aorb1.and.Savtab(LTRSCN))
     &    CALL punch(Series,Pos1ob,Posfob,LTRSCN,F,F)
       IF(Lgraf)CALL punch(Series,Pos1ob,Posfob,LTRSCN,Lgraf,F)
       IF((.not.Lfatal).and.Prttab(LTRACP).and.(Ny.eq.4.or.Ny.eq.12)
     &     .and.aorb1)
     &   CALL x11plt(Series,Series,Pos1ob,Posfob,LTRACP,0,0,6,1)
      END IF
c-----------------------------------------------------------------------
c     If no seasonal adjustment or modelling is done in this run,
c     return.
c-----------------------------------------------------------------------
c      IF(.not.(Lx11.or.Lmodel).or.Lfatal)RETURN
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
C --- TEST FOR PRIOR ADJUSTMENT.
c-----------------------------------------------------------------------
      IF(Kfmt.ge.1)THEN
c-----------------------------------------------------------------------
c     Print out prior factors
c-----------------------------------------------------------------------
       CALL prtadj(Sprior,Pos1ob,Posfob,Nspobs,Lgraf)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
C --- DIVIDE (SUBTRACT) BY THE PRIOR ADJUSTMENT SERIES.
c-----------------------------------------------------------------------
       CALL divsub(Sto,Sto,Sprior,Pos1ob,lastpr)
       IF(Missng)THEN
        CALL setmv(Sto,mvind,Mvval,Pos1ob,Posfob)
        CALL setmv(Stoap,mvind,Mvval,Pos1ob,Posfob)
        CALL setmv(Stopp,mvind,Mvval,Pos1ob,Posfob)
       END IF
      END IF
c-----------------------------------------------------------------------
C --- TEST FOR PRIOR Calendar ADJUSTMENT via X-11 Regression or 
c     X-11 Easter.
c     (Changed by Brian Monsell, Feb. 1996, Feb. 1998, May 1999)
c-----------------------------------------------------------------------
      phol=Posffc
      IF(Posfob.eq.Posffc)phol=Posfob+Ny
      IF(((Axrghl.or.Axrgtd).and.Ixreg.eq.3).or.Khol.gt.1)THEN
       IF(Khol.gt.1)CALL addmul(Faccal,Faccal,X11hol,Pos1bk,phol)
       CALL divsub(Sto,Sto,Faccal,Pos1ob,Posfob)
       IF(Missng)CALL setmv(Sto,mvind,Mvval,Pos1ob,Posfob)
      END IF
c-----------------------------------------------------------------------
C --- TEST FOR PRIOR TRADING DAY ADJUSTMENT, both specified by the user
c     and via X-11 Regression.(Changed by Brian Monsell, Dec. 1996)
c-----------------------------------------------------------------------
      IF(Kswv.ne.0.or.(Ixreg.ge.2.and.Axrgtd))THEN
c-----------------------------------------------------------------------
C --- WRITE PRIOR ADJUSTED SERIES BEFORE T.D. ADJUSTMENT A3.
c-----------------------------------------------------------------------
       IF(Prttab(LTRNA3).and.(Kfmt.gt.0.or.Khol.eq.2))THEN
        IF(Ny.eq.4.or.Ny.eq.12)THEN
       	 CALL table(Sto,Pos1ob,Posfob,3,1,2,dvec,LTRNA3)
        ELSE
         CALL genSkip(LTRNA3)
         CALL prtshd(
     &      'Prior Adjusted Series (Before Prior Calendar Adjustments)',
     &               Begspn,Sp,Posfob-Pos1ob+1)
         CALL prttbl(Begspn,Sp,Sto(Pos1ob),Posfob-Pos1ob+1,'Data',Kdec,
     &               'a3')
        END IF
       END IF
       IF(.not.Lfatal.and.Savtab(LTRNA3).and.Kfmt.gt.0)
     &    CALL punch(Sto,Pos1ob,Posfob,LTRNA3,F,F)
       IF(.not.Lfatal.and.Lgraf.and.Kfmt.gt.0)
     &    CALL punch(Sto,Pos1ob,Posfob,LTRNA3,Lgraf,F)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
C --- WRITE Permanent PRIOR ADJUSTED SERIES A3P.
c-----------------------------------------------------------------------
       IF(Prttab(LTRA3P).and.(Kfmt.gt.0.and.Nuspad.gt.0))THEN
        IF(Ny.eq.4.or.Ny.eq.12)THEN
             CALL table(Stopp,Pos1ob,Posfob,3,2,2,dvec,LTRA3P)
        ELSE
         CALL genSkip(LTRA3P)
         CALL prtshd('Prior Adjusted Series (Permanent Prior Factors)',
     &               Begspn,Sp,Posfob-Pos1ob+1)
         CALL prttbl(Begspn,Sp,Stopp(Pos1ob),Posfob-Pos1ob+1,'Data',
     &               Kdec,'a3p')
        END IF
       END IF
       IF((.not.Lfatal).and.Savtab(LTRA3P).and.Kfmt.gt.0.and.
     &    Nuspad.gt.0)CALL punch(Stopp,Pos1ob,Posfob,LTRA3P,F,F)
       IF((.not.Lfatal).and.Lgraf.and.Kfmt.gt.0.and.Nuspad.gt.0)
     &    CALL punch(Stopp,Pos1ob,Posfob,LTRA3P,Lgraf,F)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Generate prior trading day factors
c-----------------------------------------------------------------------
       IF(Kswv.eq.1.and.(((Axrghl.or.Axrgtd).and.Ixreg.eq.3).or.
     &    Khol.lt.2))THEN
        n2=Nbfpob
        IF(Nfcst.eq.0)n2=n2+Sp
        CALL pritd(Dwt,Stptd,n2,Sp,Begbk2,Muladd,Psuadd,Kswv,Pos1bk)
        IF(Lfatal)RETURN
        IF(Axrgtd)Kswv=Kswv+2
c-----------------------------------------------------------------------
C --- WRITE PRIOR TRADING FACTORS A4.
c-----------------------------------------------------------------------
        IF(Prttab(LXRGA4))
     &     CALL table(Stptd,Pos1ob,Posfob,4,1,1,Dwt,LXRGA4)
        IF(.not.Lfatal.and.(Savtab(LXRGA4).or.Lgrfxr))THEN
         IF(.not.Savfct)THEN
          lasttd=Posfob
         ELSE IF(Nfcst.gt.0)THEN
          lasttd=Posffc
         ELSE
          lasttd=Posfob+Sp
         END IF
         IF(Savbct)THEN
          frsttd=Pos1bk
         ELSE
          frsttd=Pos1ob
         END IF
         IF(Savtab(LXRGA4))CALL punch(Stptd,frsttd,lasttd,LXRGA4,F,F)
         IF(Lgrfxr)CALL punch(Stptd,frsttd,lasttd,LXRGA4,Lgrfxr,F)
        END IF
c-----------------------------------------------------------------------
c     Save X-11 Regression TD into sliding spans variable
c-----------------------------------------------------------------------
        IF(Issap.eq.2.and.Ixreg.ne.2)
     &     CALL ssrit(Stptd,Pos1ob,Posfob,1,Series)
c-----------------------------------------------------------------------
C --- DIVIDE (SUBTRACT) PRIOR ADJUSTED OR ORIGINAL BY PRIOR T.D. FACTORS
c-----------------------------------------------------------------------
        CALL copy(Stptd(Pos1ob),Nbfpob,-1,std(Pos1ob))
        CALL divsub(Sto,Sto,std,Pos1ob,lastpr)
        IF(Kswv.eq.1)CALL addmul(Faccal,Faccal,Stptd,Pos1bk,phol)
c-----------------------------------------------------------------------
c     Check to see if prior adjustments have brought down missing
c     value code.
c-----------------------------------------------------------------------
        IF(Missng)CALL setmv(Sto,mvind,Mvval,Pos1ob,lastpr)
       END IF
      END IF
      IF(Lmodel.AND.(Ixreg.eq.3.or.Ixreg.eq.0).and.Khol.ne.1)THEN
c-----------------------------------------------------------------------
C --- WRITE PRIOR ADJUSTED SERIES BEFORE ARIMA MODELLING
c --- update table pointer fext if prior trading day factors are used
c     (BCM March 2004)
c-----------------------------------------------------------------------
       fext=LTRNA3
       IF(Kswv.gt.0)fext=LTRA4D
       IF(Prttab(fext).and.Kfmt.gt.0)THEN
        IF(Ny.eq.4.or.Ny.eq.12)THEN
         CALL table(Sto,Pos1ob,Posfob,3,1,2,dvec,fext)
        ELSE
         CALL genSkip(fext)
         CALL prtshd('Prior Adjusted Series',Begspn,Sp,Posfob-Pos1ob+1)
         CALL prttbl(Begspn,Sp,Sto(Pos1ob),Posfob-Pos1ob+1,'Data',Kdec,
     &               tbxdic(fext)(1:nblank(tbxdic(fext))))
        END IF
       END IF
       IF(Lfatal)RETURN
       IF((.not.Lfatal).and.Savtab(fext))
     &    CALL punch(Sto,Pos1ob,Posfob,fext,F,F)
       IF((.not.Lfatal).and.Lgraf)
     &    CALL punch(Sto,Pos1ob,Posfob,fext,Lgraf,F)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
C --- WRITE Permanent PRIOR ADJUSTED SERIES A3P.
c --- update table pointer fext if prior trading day factors are used
c     (BCM March 2004)
c-----------------------------------------------------------------------
       fext=LTRA3P
       IF(Kswv.gt.0)fext=LTRA4P
       IF(Prttab(fext).and.(Kfmt.gt.0.and.Nuspad.gt.0))THEN
        IF(Ny.eq.4.or.Ny.eq.12)THEN
         CALL table(Stopp,Pos1ob,Posfob,3,2,2,dvec,fext)
        ELSE
         CALL genSkip(fext)
         CALL prtshd('Prior Adjusted Series (Permanent Prior Factors)',
     &               Begspn,Sp,Posfob-Pos1ob+1)
         CALL prttbl(Begspn,Sp,Stopp(Pos1ob),Posfob-Pos1ob+1,'Data',
     &               Kdec,tbxdic(fext))
        END IF
       END IF
       IF((.not.Lfatal).and.Savtab(fext).and.Kfmt.gt.0.and.Nuspad.gt.0)
     &    CALL punch(Stopp,Pos1ob,Posfob,fext,F,F)
       IF((.not.Lfatal).and.Lgraf.and.Kfmt.gt.0.and.Nuspad.gt.0)
     &    CALL punch(Stopp,Pos1ob,Posfob,fext,Lgraf,F)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
C --- SET STSCI EQUAL TO PRIOR ADJUSTED SERIES.
c-----------------------------------------------------------------------
      CALL copy(Sto(Pos1ob),Posfob-Pos1ob+1,-1,Stcsi(Pos1ob))
c-----------------------------------------------------------------------
      RETURN
      END
