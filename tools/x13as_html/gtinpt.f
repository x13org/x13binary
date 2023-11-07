C     Last change: Nov,2021, if there is a composite spec, set l1stcomp
C                  True
C     previous change:Oct, 2021
C     previous change:  March. 2021 change the format of AOS and LSS such as
C     AOSdate-0.0/date-0.0 in regression variables to set
C     sequence outliers with a convention for the end of the series
C     previous change:  Jan. 2021, error message and error check
C     previous change:  SRD  25 Jan 100    2:09 pm
      SUBROUTINE gtinpt(Sscut,Srsttl,Nsrscr,Ttlvec,Notc,Lx11,X11agr,
     &                  Lseats,Lmodel,Ldata,Dtafil,l1stcomp,Hvmfil,
     &                  Mdlfil,Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     gtinpt.f, Release 1, Subroutine Version 1.13, Modified 14 Feb 1995.
c-----------------------------------------------------------------------
c     Gets input for all specs from input files.
c-----------------------------------------------------------------------
      LOGICAL F,T
      DOUBLE PRECISION ONE,ZERO,PT5
      PARAMETER(F=.false.,T=.true.,ONE=1D0,ZERO=0D0,PT5=0.05D0)
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'lex.i'
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'rev.prm'
      INCLUDE 'arima.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'prittl.cmn'
      INCLUDE 'priadj.cmn'
      INCLUDE 'priusr.cmn'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'seatlg.cmn'
      INCLUDE 'seatop.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11log.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'usrxrg.cmn'
      INCLUDE 'xrgmdl.cmn'
      INCLUDE 'xrgfct.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'metadata.prm'
      INCLUDE 'metadata.cmn'
      INCLUDE 'fxreg.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'rho.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'rev.cmn'
      INCLUDE 'agr.cmn'
      INCLUDE 'adj.cmn'
      INCLUDE 'force.cmn'
      INCLUDE 'sspinp.cmn'
      INCLUDE 'x11msc.cmn'
      INCLUDE 'xclude.cmn'
      INCLUDE 'x11reg.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'missng.cmn'
      INCLUDE 'tukey.cmn'
      INCLUDE 'savcmn.cmn'
      INCLUDE 'xrgum.cmn'
      INCLUDE 'xtrm.cmn'
      INCLUDE 'revtrg.cmn'
      INCLUDE 'deftab.prm'
      INCLUDE 'sumtab.prm'
      INCLUDE 'spctbl.i'
      INCLUDE 'spcsvl.i'
c-----------------------------------------------------------------------
      CHARACTER Ttlvec*(*),Srsttl*(PSRSCR),Mdlfil*(PFILCR),perstr*(7),
     &          Dtafil*(PFILCR)
      DOUBLE PRECISION Sscut
      LOGICAL havotl,havesp,havsrs,Inptok,Lx11,X11agr,lagr,Lmodel,
     &        Hvmfil,havreg,Ldata,larma,x11reg,hvx12f,havtca,havhol,
     &        Lseats,hvfcst,hvspec,l1stcomp
      INTEGER Notc,icol,i,Nsrscr,iautom,outamd,outest,fhnote,igrp,
     &        numcol,iper,nspec
      DIMENSION Ttlvec(*),Sscut(*),hvx12f(PNAD)
c-----------------------------------------------------------------------
c      INTEGER Iwt,Kexopt
c      COMMON /oldopt/ Iwt,Kexopt 
      LOGICAL dpeq,getfcn
      INTEGER strinx
      EXTERNAL dpeq,getfcn,strinx
c----------------------------------------------------------------------- 
      CHARACTER SPCDIC*150
      INTEGER spcidx,spcptr,PSPC,spclog
      PARAMETER(PSPC=20)
      DIMENSION spcptr(0:PSPC),spclog(2,PSPC)
      PARAMETER(SPCDIC='seriestransformidentifyregressionarimaautomdlest
     &imateoutliercheckforecastx11historyslidingspanscompositex11regress
     &ionseatspickmdlforcemetadataspectrum')
c-----------------------------------------------------------------------
      DATA spcptr /1,7,16,24,34,39,46,54,61,66,74,77,84,96,105,118,123,
     &             130,135,143,151/
c-----------------------------------------------------------------------
      INCLUDE 'deftab.var'
      INCLUDE 'sumtab.var'
c-----------------------------------------------------------------------
c     Initialize parser input
c-----------------------------------------------------------------------
      CALL intinp(Mt)
      IF(Lfatal)RETURN
      CALL setint(NOTSET,2*PSPC,spclog)
c-----------------------------------------------------------------------
c     Set default values for print table, print plot, and save variables
c-----------------------------------------------------------------------
      IF(Lnoprt)THEN
       CALL setlg(F,NTBL,Prttab)
      ELSE
       CALL copylg(deftab,NTBL,1,Prttab)
      END IF
      IF(Lsumm.gt.0)THEN
       CALL copylg(sumtab,NTBL,1,Savtab)
      ELSE
       CALL setlg(F,NTBL,Savtab)
      END IF
      CALL setlg(F,NSVLOG,Svltab)
c-----------------------------------------------------------------------
c     Set the default values
c-----------------------------------------------------------------------
      havsrs=F
      havesp=F
      havotl=F
      Inptok=T
      Picktd=F
      havreg=F
      x11reg=F
      larma=F
      havhol=F
      hvfcst=F
      hvspec=F
c-----------------------------------------------------------------------
      Begsrs(YR)=1
      Begsrs(MO)=1
      Nobs=0
      Sp=12
      Hvmfil=F
      Svprec=15
      Svsize=Svprec+7
      Yr2000=T
      Divpwr=NOTSET
      Isrflw=NOTSET
      CALL setdp(ZERO,PLEN,Y)
c-----------------------------------------------------------------------
      CALL setdp(DNOTST,PB,B)
      Priadj=0
      Reglom=0
      Fcntyp=NOTSET
      Lam=ONE
      Nustad=0
      Nuspad=0
      CALL setchr(' ',PSRSCR,Adjttl)
      Nadjcr=12
      Adjttl(1:Nadjcr)='User-defined'
      CALL setint(NOTSET,PNAD,Percnt)
      CALL setint(NOTSET,PNAD,Prtype)
      Nprtyp=0
      Lprntr=F
      Cnstnt=DNOTST
c-----------------------------------------------------------------------
      Niddf=0
      Nidsdf=0
      Mxidlg=NOTSET
      Lidsdf=F
      Lprtdf=F
c-----------------------------------------------------------------------
c     Initialize the parmeters and lag vectors.  Rewind the input file
c because we are only going go through the input once.
c-----------------------------------------------------------------------
      Lseff=F
      Lrgmse=F
      Lrgmtd=F
      Lrgmln=F
      Fulltd=F
      Fullln=F
      Fulllp=F
      CALL intlst(PB,Colptr,Ncoltl)
      Nb=Ncoltl
      Ncxy=1
      CALL intlst(PGRP,Grpptr,Ngrptl)
      CALL intlst(PGRP,Grp,Ngrp)
      CALL intlst(POPR,Opr,Nopr)
      CALL intlst(PMDL,Mdl,Nmdl)
      CALL intlst(PGRP,Grpfix,Ngrpfx)
      CALL intlst(PB,Gfxptr,Nfxttl)
      CALL setchr(' ',PCOLCR*PB,Colttl)
      CALL setchr(' ',PGRPCR*PGRP,Grpttl)
      CALL setlg(F,PB,Regfx)
      CALL intlst(PUREG,Usrptr,Ncusrx)
      CALL setchr(' ',PCOLCR*PUREG,Usrttl)
c-----------------------------------------------------------------------
      Lseadf=F
      CALL setchr(' ',PFILCR,Mdlfil)
      Fixmdl=0
      Mdl(AR)=1
      Mdl(MA)=1
      CALL setlg(F,PARIMA,Arimaf)
      CALL setint(0,PARIMA,Arimal)
      CALL setdp(ZERO,PARIMA,Arimap)
      CALL setchr(' ',POPRCR,Mdlttl)
      Nmdlcr=11
      Mdlttl(1:Nmdlcr)='ARIMA Model'
      CALL setchr(' ',132,Mdldsn)
      Nmddcr=7
      Mdldsn(1:Nmddcr)='(0,0,0)'
      Mxarlg=0
      Mxdflg=0
      Mxmalg=0
c-----------------------------------------------------------------------
      Lautom=F
      Lautod=F
      Exdiff=2
      Hrinit=F
      CALL setchr(' ',132,Bstdsn)
      Bstdsn(1:1)=CNOTST
      Nbstds=0
      Ub1lim=ONE/0.96D0
      Ub2lim=0.88D0
      Ubfin=1.05D0
      Tsig=ONE
      Fct=ONE/(ONE-.0125D0)
      Predcv=.14286D0
      Cancel=0.1D0
      Pcr=.95D0
      Lbalmd=F
      Laccdf=F
      Lotmod=T
      CALL setint(0,2,Maxord)
      CALL setint(NOTSET,2,Diffam)
      Frstar=2
      Lchkmu=T
      Lmixmd=T
      Lrejfc=F
      Fctlm2=15D0
      Lsovdf=F
c-----------------------------------------------------------------------
      Lautox=F
      Pck1st=T
      Id1st=T
      outamd=NOTSET
      iautom=0
      CALL setchr(' ',PFILMD,Autofl)
      Fctlim=15D0
      Bcklim=18D0
      Qlim=5D0
      Ovrdif=0.9D0
c-----------------------------------------------------------------------
      Nintvl=0
      Nextvl=0
      Var=ZERO
      Lndtcv=ZERO
      CALL setint(0,PUREG,Usrtyp)
c-----------------------------------------------------------------------
c     Set the default values for the options
c-----------------------------------------------------------------------
      Armaer=0
      Convrg=T
      Iregfx=1
      Imdlfx=1
      Mxiter=1500
      Mxnlit=40
      Stepln=ZERO
      Tol=DFTOL
      Nltol0=DFNLT0
      Nltol=DFNLTL
      Lextar=T
      Lextma=T
      Lestim=T
      Ldestm=F
      Lcalcm=F
      Itdtst=0
      Leastr=F
      Lceaic=F
      Eastst=0
      Luser=F
      Lttc=F
      Lomtst=0
      Elong=T
      Rmcnst=F
      Aicstk=31
      Easidx=0
      Traicd=DNOTST
      CALL setdp(ZERO,PAICT,Rgaicd)
      Acflim=1.6D0
      Qcheck=PT5
      Eick=DNOTST
      Ch2tst=F
      Chi2cv=0.01D0
      Tlimit=DNOTST
      Pvaic=DNOTST
      Iqtype=0
c-----------------------------------------------------------------------
      Ltstao=F
      Ltstls=F
      Ltsttc=F
*      Ltstso=F
      Ladd1=T
      CALL setdp(DNOTST,POTLR,Critvl)
      Lsrun=0
      Lindot=T
c-----------------------------------------------------------------------
      Fctdrp=0
      Ciprob=.95D0
      Nfcst=NOTSET
      Nbcst=NOTSET
      Lognrm=F
c-----------------------------------------------------------------------
      Kdec=0
      Mxcklg=0
      outest=NOTSET
c-----------------------------------------------------------------------
      Muladd=NOTSET
      Kfulsm=0
      Sigml=1.5D0
      Sigmu=2.5D0
      Lterm=NOTSET
      Ktcopt=0
      Ksdev=1
      CALL setlg(F,PSP,Csigvc)
      Tic=0.0D0
*      Iwt=0
      CALL setint(0,12,Lter)
      Notc=0
      Imad=0
      Thtapr=0.0D0
      Nspeak=0
      Ntpeak=0
      CALL setchr(' ',35,Cspeak)
      CALL setchr(' ',35,Ctpeak)
      DO i=1,8
       CALL setchr(' ',80,Ttlvec(i))
      END DO
      Iag=NOTSET
      lagr=F
      W=ONE
      CALL setchr(' ',64,Serno)
      CALL setchr(' ',64,Tmpser)
      CALL setchr(' ',64,Prmser)
      Nser=0
      Ntser=0
      Npser=0
      Shrtsf=F
      Spcdff=T
      Spdfor=NOTSET
      Lstdff=F
      Lfqalt=F
      Lqchk=F
      Ltk120=T
      Llogqs=F
      Lrbstsa=T
      Svallf=F
      Ldecbl=T
      Spctyp=0
      Spcsrs=2
      Mxarsp=NOTSET
      Spclim=6D0
      Peakwd=NOTSET
      Plocal=0.002D0
      CALL setint(NOTSET,2,Bgspec)
      Mvcode=-99999D0
      Mvval=1000000000D0
      Missng=F
      Psuadd=F
      Savfct=F
      Savbct=F
      Prt1ps=F
      Axsame=F
      Noxfct=F
      Tru7hn=F
      Lcentr=F
      Ishrnk=0
c-----------------------------------------------------------------------
      CALL setdp(DNOTST,6,Ptsr)
      CALL setdp(DNOTST,6,Ptso)
      CALL setdp(DNOTST,6,Ptsa)
      CALL setdp(DNOTST,6,Ptsi)
      Pttdr=NOTSET
      Pttdo=NOTSET
      Pttda=NOTSET
      Pttdi=NOTSET
      Ntukey=0
      CALL setint(NOTSET,4,Itukey)
c-----------------------------------------------------------------------
c     Initialize model adjustment parameters
c-----------------------------------------------------------------------
      Adjtd=1
      Adjhol=1
      Adjao=1
      Adjls=1
      Adjtc=1
      Adjso=1
      Adjsea=1
      Adjcyc=1
      Adjusr=1
      Finhol=T
      Finao=F
      Finls=F
      Fintc=F
      Finusr=F
      Nusrrg=0
      Tdzero=0
      Lnzero=0
      CALL setint(NOTSET,2,Tddate)
      CALL setint(NOTSET,2,Lndate)
      Tcalfa=DNOTST
      havtca=F
      Cvalfa=PT5
      Cvtype=F
      Cvrduc=0.5D0
c-----------------------------------------------------------------------
      Iyrt=NOTSET
      Begyrt=NOTSET
      Lrndsa=F
      Lindfr=T
      Lfctfr=T
      Iftrgt=NOTSET
      Mid=NOTSET
      Lamda=DNOTST
      Rol=DNOTST
c-----------------------------------------------------------------------
c      Kexopt=0
c      Kdwopt=0
c      Lcyr=0
c      Layr=0
c      Sigm=2.5D0
c      Lopt=0
      Keastr=0
c-----------------------------------------------------------------------
      Ixreg=0
      Nbx=0
      Begum(YR)=0
      Begum(MO)=0
      Haveum=F
      Noxfac=F
      Ixrgtd=1
      Ixrghl=1
      Havxtd=F
      Havxhl=F
      Axrgtd=F
      Axrghl=F
      Axruhl=F
      CALL setint(0,PUREG,Usxtyp)
      CALL intlst(PUREG,Usrxpt,Ncxusx)
      Ncxusx=0
      Sigxrg=DNOTST
      Critxr=DNOTST
      Otlxrg=F
      Ladd1x=T
      Xtdtst=0
      Xeastr=F
      Xuser=F
      Xhlnln=F
      Lxrneg=F
      Xelong=T
      Calfrc=F
      CALL setint(NOTSET,2,Xaicrg)
      Xaicst=31
      CALL setdp(DNOTST,7,Dwt)
      Fxprxr=0
      Xdsp=0
      Nfcstx=0
      Nbcstx=0
      CALL setlg(F,PLEN,Rgxcld)
      Nxcld=0
      Xraicd=ZERO
      Cvxalf=PT5
      Cvxrdc=0.5D0
      Cvxtyp=F
c-----------------------------------------------------------------------
      Issap=0
      Nlen=0
      Ncol=0
      Irev=0
      Irevsa=0
      Fixper=0
      Cnctar=F
      CALL setint(NOTSET,PTARGT,Targsa)
      Ntarsa=0
      CALL setint(NOTSET,PTARGT,Targtr)
      Ntartr=0
      CALL setint(NOTSET,PFCLAG,Rfctlg)
      Nfctlg=0
      Rvstrt(YR)=0
      Rvstrt(MO)=0
      Rvend(YR)=0
      Rvend(MO)=0
      Lrvsa=F
      Lrvsf=F
      Lrvch=F
      Lrvtrn=F
      Lrvtch=F
      Lrvaic=F
      Lrvfct=F
      Lrvarma=F
      Lrvtdrg=F
      Revfix=F
      Lrfrsh=F
      Otlrev=0
      Otlwin=NOTSET
      Rvtran=T
      Revfxx=F
      Rvtrfc=F
      Rvxotl=T
      CALL setint(NOTSET,4,Rvfxrg)
      Nrvfxr=0
      Rvdiff=2
c-----------------------------------------------------------------------
      Ssotl=1
      Ssinit=1
      Sstran=T
      CALL setdp(3D0,5,Sscut)
      Sscut(2)=2D0
      CALL setint(NOTSET,2,Strtss)
      CALL setint(NOTSET,4,Ssfxrg)
      Nssfxr=0
      Ssdiff=T
      Ssidif=T
      Ssxotl=T
      Ssxint=T
c-----------------------------------------------------------------------
      Lnoadm=F
      Kmean=NOTSET
      Lstsea=F
      Lhp=T
      Lfinit=F
      Lhprmls=F
      Qmax2=NOTSET
      Out2=NOTSET
      Maxit2=NOTSET
      Epsph2=DNOTST
      Xl2=DNOTST
      Rmod2=DNOTST
      Epsiv2=DNOTST
      Hplan2=DNOTST
      Bias2=NOTSET
      Iphtrf=NOTSET
      Hptrgt=NOTSET
      CALL setchr(' ',100,Tabtbl)
      Tabtbl(1:1)=CNOTST
c-----------------------------------------------------------------------
      CALL intlst(PMTDAT,Keyptr,Nkey)
      CALL intlst(PMTDAT,Valptr,Nval)
      CALL setchr(' ',PLMETA,Keystr)
      CALL setchr(' ',PLMETA,Valstr)
      Hvmtdt=F
c-----------------------------------------------------------------------
      Lx11=F
      Lseats=F
      Lmodel=F
c-----------------------------------------------------------------------
c     Get the series, prior adjustments, regression and ARIMA model,
c and options.
c-----------------------------------------------------------------------
      DO WHILE (T)
       IF(getfcn(SPCDIC,spcptr,PSPC,spcidx,spclog,Inptok))THEN
        GO TO(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,
     &        170,180,190,200),spcidx
c-----------------------------------------------------------------------
   10   Endspn(YR)=0
        Endspn(MO)=0
        CALL getsrs(Sp,Y,Nobs,Begsrs,Nspobs,Begspn,Srsttl,Nsrscr,Serno,
     &              Nser,havsrs,havesp,Kdec,Begmdl,Endmdl,Ldata,Dtafil,
     &              Iag,Iagr,lagr,W,Mvcode,Mvval,Fixper,Svprec,Yr2000,
     &              Divpwr,Isrflw,Inptok)
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Initialize outlier span variable
c-----------------------------------------------------------------------
        IF(havsrs)THEN
         IF(.not.havotl)THEN
          CALL cpyint(Begmdl,2,1,Begtst)
          CALL cpyint(Endmdl,2,1,Endtst)
c          CALL addate(Begspn,Sp,Nobs-1,Endtst)
         END IF
         CALL addate(Begspn,Sp,Nspobs-1,Endspn)
        END IF
c-----------------------------------------------------------------------
c     Set up composite adjusment.
c-----------------------------------------------------------------------
        IF(lagr)THEN
         CALL agr1(Y,Nobs)
         X11agr=T
        END IF
        GO TO 210
c-----------------------------------------------------------------------
   20   CALL getadj(Begsrs,havsrs,havesp,Sp,Begspn,Nspobs,Endspn,Usrtad,
     &              Nustad,Bgutad,Tmpser,Ntser,Usrpad,Nuspad,Bgupad,
     &              Prmser,Npser,Adjttl,Nadjcr,Priadj,Reglom,Fcntyp,Lam,
     &              Prtype,Nprtyp,Percnt,Traicd,Lprntr,hvx12f,Cnstnt,
     &              Inptok)
        IF(Lfatal)RETURN
        GO TO 210
c-----------------------------------------------------------------------
   30   CALL getid(Dflist,Niddf,Nidsdf,Mxidlg,Inptok)
        IF(Lfatal)RETURN
        IF(.not.Lmodel)Lmodel=T
        GO TO 210
c-----------------------------------------------------------------------
c     Specify Regression portion of model if requested
c-----------------------------------------------------------------------
   40   IF(Hvmfil)THEN
         CALL inpter(PERROR,Pos,
     &               'Cannot specify regression variables when a '//
     &               'model file is given.',T)
         Inptok=F
        END IF
        CALL getreg(Begsrs,Endmdl,Nobs,havsrs,havesp,Userx,Nrusrx,
     &              Bgusrx,Itdtst,Leastr,Eastst,Luser,Lttc,Elong,Adjtd,
     &              Adjao,Adjls,Adjtc,Adjso,Adjhol,Adjsea,Adjcyc,Adjusr,
     &              Nusrrg,havtca,Rgaicd,Lam,Fcntyp,havhol,Lomtst,
     &              Ch2tst,Chi2cv,Tlimit,Pvaic,Lceaic,Inptok)
        IF(Lfatal)RETURN
        IF(.not.Lmodel)Lmodel=T
        IF(.not.havreg)havreg=T
c        call profiler(2,'gtinpt, Regfx=')
c        write(Mtprof,*) (Regfx(i), i=1,Nb)
        GO TO 210
c-----------------------------------------------------------------------
c     Specify ARIMA portion of model if requested
c-----------------------------------------------------------------------
   50   IF(Lautom)THEN
         CALL inpter(PERROR,Pos,
     &               'Cannot specify arima and automdl spec in the '//
     &               'same input file.',T)
         Inptok=F
        ELSE IF(Lautox)THEN
         CALL inpter(PERROR,Pos,
     &               'Cannot specify arima and pickmdl spec in the '//
     &               'same input file.',T)
         Inptok=F
        END IF
        IF(Hvmfil)THEN
         CALL inpter(PERROR,Pos,
     &               'Cannot specify arima spec if model is read in '//
     &               'from the file argument',F)
         CALL writln('        of the estimate spec.',Mt2,STDERR,F,T)
         Inptok=F
        END IF
        Imdlfx=1
        CALL gtarma(Inptok)
        IF(Lfatal)RETURN
        IF(.not.Lmodel)Lmodel=T
        larma=T
        GO TO 210
c-----------------------------------------------------------------------
c     Specify automatic ARIMA modeling options
c-----------------------------------------------------------------------
   60   IF(larma)THEN
         CALL inpter(PERROR,Pos,
     &               'Cannot specify arima and automdl spec in the '//
     &               'same input file.',T)
         Inptok=F
        ELSE IF(Lautox)THEN
         CALL inpter(PERROR,Pos,
     &               'Cannot specify automdl and pickmdl spec in the '//
     &               'same input file.',T)
         Inptok=F
        END IF
        IF(Hvmfil)THEN
         CALL inpter(PERROR,Pos,
     &               'Cannot specify automdl spec if model is read '//
     &               'in from the file argument',F)
         CALL writln('        of the estimate spec.',Mt2,STDERR,F,T)
         Inptok=F
        END IF
        Imdlfx=1
        CALL gtauto(Lautom,Lautod,Ub1lim,Ub2lim,Cancel,Maxord,Diffam,
     &              Exdiff,Lbalmd,Hrinit,Tsig,Pcr,Fct,Predcv,Laccdf,
     &              Lotmod,Ubfin,Frstar,Lchkmu,Lmixmd,Lrejfc,Fctlm2,
     &              Lsovdf,Inptok)
        IF(Lfatal)RETURN
        IF(.not.Lmodel)Lmodel=T
        IF(Lautom)Ldestm=T
        GO TO 210
c-----------------------------------------------------------------------
c     estimate model if requested
c-----------------------------------------------------------------------
   70   CALL gtestm(havreg,larma,Nspobs,Mxiter,Mxnlit,Lestim,outest,
     &              Mdlfil,Hvmfil,Eick,Rmcnst,Inptok)
        IF(Lfatal)RETURN
        Ldestm=T
        IF(.not.Lmodel)Lmodel=T
        GO TO 210
c-----------------------------------------------------------------------
c     identify outliers and level changes if requested
c-----------------------------------------------------------------------
   80   IF(.not.havsrs)THEN
         CALL inpter(PERROR,Pos,
     &               'Need to specify a series to identify outliers',T)
         Inptok=F
        END IF
        CALL gtotlr(Begsrs,Nobs,Begmdl,Endmdl,Sp,Ltstao,Ltstls,Ltsttc,
*     &              Ltstso,Ladd1,Critvl,Begtst,Endtst,Lsrun,Tcalfa,
     &              Ladd1,Critvl,Begtst,Endtst,Lsrun,Tcalfa,
     &              havtca,Cvalfa,Cvtype,Cvrduc,havotl,Inptok)
        IF(Lfatal)RETURN
        Ldestm=T
        GO TO 210
c-----------------------------------------------------------------------
c     Produce model diagnostics if requested
c-----------------------------------------------------------------------
   90   CALL getchk(Mxcklg,Acflim,Qcheck,Iqtype,Sp,Inptok)
        IF(Lfatal)RETURN
        Ldestm=T
        IF(.not.Lmodel)Lmodel=T
        GO TO 210
c-----------------------------------------------------------------------
c     Calculate the forecasts if requested.
c-----------------------------------------------------------------------
  100   CALL gtfcst(Fctdrp,Nfcst,Nbcst,Ciprob,Lognrm,Inptok)
        IF(Lfatal)RETURN
        IF(Nfcst.gt.0.or.Nbcst.gt.0.or.hvfcst)Ldestm=T
        IF(.not.Lmodel)Lmodel=T
        hvfcst=T
        GO TO 210
c-----------------------------------------------------------------------
c     Perform X-11 seasonal adjustment if requested.
c-----------------------------------------------------------------------
  110   IF(Lseats)THEN
         CALL inpter(PERROR,Pos,
     &               'Cannot specify x11 and seats spec in the '//
     &               'same input file.',T)
         Inptok=F
        END IF
        CALL getx11(havesp,Sp,Muladd,Kfulsm,Sigml,Sigmu,Lterm,Ktcopt,
     &              Lter,Notc,Imad,Ttlvec,Tic,Ksdev,Csigvc,Keastr,
     &              Thtapr,Finhol,Finao,Finls,Fintc,Finusr,Shrtsf,
     &              Psuadd,Prt1ps,Noxfct,Tru7hn,Lcentr,Ishrnk,Inptok)
c     &              Kexopt,Iwt,Inptok)
        IF(Lfatal)RETURN
        IF(.not.Lx11)Lx11=T
        GO TO 210
c-----------------------------------------------------------------------
c     Generate revisions diagnostics if specified
c-----------------------------------------------------------------------
  120   CALL gtrvst(havesp,Sp,Irev,Irevsa,Rfctlg,Nfctlg,Rvstrt,Rvend,
     &              Otlrev,Otlwin,Lrvsa,Lrvch,Lrvtrn,Lrvaic,Lrvfct,
     &              Lrvtch,Lrvsf,Lrvarma,Lrvtdrg,Revfix,Cnctar,Targsa,
     &              Ntarsa,Targtr,Ntartr,Lrfrsh,Rvtran,Rvfxrg,Nrvfxr,
     &              Rvxotl,Rvdiff,Revfxx,Rvtrfc,Indrev,Indrvs,Iagr,
     &              Inptok)
        IF(Lfatal)RETURN
        IF(Lrvfct.or.Lrvaic)Ldestm=T
        GO TO 210
c-----------------------------------------------------------------------
c     Generate sliding spans seasonal adjustment diagnostics
c     if specified
c-----------------------------------------------------------------------
  130   CALL getssp(havesp,Sp,Issap,Ssotl,Ssinit,Strtss,Sscut,Nlen,Ncol,
     &              Sstran,Ssfxrg,Nssfxr,Ssdiff,Ssxotl,Ssxint,Inptok)
        IF(Lfatal)RETURN
        IF(.not.Ssdiff)Ssidif=Ssdiff
        GO TO 210
c-----------------------------------------------------------------------
c     Set up direct and indirect composite adjustment, if specified
c-----------------------------------------------------------------------
  140   IF (Iagr.le.0)THEN
c-----------------------------------------------------------------------
c     Test to see if component series have been specified.
c-----------------------------------------------------------------------
         IF(Iagr.eq.0)THEN
          CALL inpter(PERROR,Pos,
     &                'No component series were specified for '//
     &                'composite adjustment.',T)
         ELSE
          CALL inpter(PERROR,Errpos,
     &                'Error(s) were found while executing the spec '//
     &                'file(s) of component',F)
          CALL writln('         series used for this composite '//
     &                'adjustment.  The direct and indirect',
     &                STDERR,Mt2,F,F)
          CALL writln('         seasonal adjustment of the total '//
     &                'series will not be performed.',STDERR,Mt2,F,T)
          Iagr=NOTSET
         END IF
         Inptok=F
        END IF
        CALL getcmp(PLEN,havesp,Sp,Y,Nobs,Begsrs,Nspobs,Begspn,Srsttl,
     &              Nsrscr,Serno,Nser,Itest,Kdec,Begmdl,Endmdl,Svprec,
     &              lagr,Yr2000,Lindot,Isrflw,Inptok)
        IF(Lfatal)RETURN
        l1stcomp=T
        IF(lagr)THEN
         havsrs=T
         IF(.not.havotl)THEN
          CALL cpyint(Begmdl,2,1,Begtst)
          CALL cpyint(Endmdl,2,1,Endtst)
         END IF
        END IF
        GO TO 210
c-----------------------------------------------------------------------
c     Specify X-11 Regression model if requested
c-----------------------------------------------------------------------
c     Store model parameters into temporary storage and delete 
c     regressors (if they exist)
c-----------------------------------------------------------------------
  150   CALL ssprep(T,F,F)
        IF(Nb.gt.0)THEN
         DO igrp=Ngrp,1,-1
          icol=Grp(igrp-1)
          numcol=Grp(igrp)-Grp(igrp-1)
          CALL dlrgef(icol,Nrxy,numcol)
         END DO
         CALL intlst(PGRP,Grpptr,Ngrptl)
         CALL intlst(PGRP,Grp,Ngrp)
         CALL setchr(' ',PCOLCR*PB,Colttl)
         Ncoltl=0
         IF(Fulltd)Fulltd=F
        END IF
c-----------------------------------------------------------------------
        CALL gtxreg(Begsrs,Nobs,Endmdl,havsrs,havesp,Priadj,
     &              Xuserx,Bgusrx,Ixreg,Nusxrg,Sigxrg,Critxr,Otlxrg,
     &              Umean,Begum,Haveum,Noxfac,Ladd1x,Xtdtst,Xeastr,
     &              Xuser,Dwt,Ixrgtd,Ixrghl,Xhlnln,Xelong,Calfrc,Begxrg,
     &              Endxrg,Fxprxr,Begxot,Endxot,Havxhl,Havxtd,Axrghl,
     &              Axrgtd,Lxrneg,Cvxalf,Cvxtyp,Cvxrdc,Xraicd,Inptok)
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     store regression options into X-11 regression variables and 
c     restore previous values.
c-----------------------------------------------------------------------
        CALL loadxr(T)
        CALL restor(T,F,F)
        IF(.not.x11reg.and.Ixreg.gt.0)x11reg=T
        IF(.not.Lx11)Lx11=T
        GO TO 210
c-----------------------------------------------------------------------
c    Seats spec
c-----------------------------------------------------------------------
  160   IF(Lx11)THEN
         CALL inpter(PERROR,Pos,
     &               'Cannot specify x11 and seats spec in the '//
     &               'same input file.',T)
         Inptok=F
        END IF
        CALL gtseat(Qmax2,Out2,Maxit2,Epsph2,Xl2,Rmod2,Epsiv2,Hplan2,
     &              Lseats,Lnoadm,Kmean,Lhp,Lstsea,Bias2,
     &              Lfinit,Iphtrf,Tabtbl,Hptrgt,Lhprmls,Inptok)
        IF(Lfatal)RETURN
        Ldestm=T
        IF(.not.Lmodel)Lmodel=T
        GO TO 210
c-----------------------------------------------------------------------
c     Specify automatic ARIMA modeling options
c-----------------------------------------------------------------------
  170   IF(larma)THEN
         CALL inpter(PERROR,Pos,
     &               'Cannot specify arima and pickmdl spec in the '//
     &               'same input file.',T)
         Inptok=F
        ELSE IF(Lautom)THEN
         CALL inpter(PERROR,Pos,
     &               'Cannot specify automdl and pickmdl spec in the '//
     &               'same input file.',T)
         Inptok=F
        END IF
        IF(Hvmfil)THEN
         CALL inpter(PERROR,Pos,
     &               'Cannot specify pickmdl spec if model is read '//
     &               'in from the file argument',F)
         CALL writln('        of the estimate spec.',Mt2,STDERR,F,T)
         Inptok=F
        END IF
        Imdlfx=1
        CALL gtautx(iautom,Autofl,Fctlim,Bcklim,Qlim,Ovrdif,Pck1st,
     &              Id1st,outamd,Inptok)
        IF(Lfatal)RETURN
        IF(.not.Lmodel)Lmodel=T
        IF(iautom.gt.0)THEN
         Lautox=T
         Ldestm=T
        END IF
        GO TO 210
c-----------------------------------------------------------------------
c     Specify forcing options
c-----------------------------------------------------------------------
  180   CALL getfrc(havesp,Iyrt,Lrndsa,Iftrgt,Begyrt,Mid,Lamda,Rol,Sp,
     &              Lindfr,Lfctfr,Inptok)
        IF(Lfatal)RETURN
        GO TO 210
c-----------------------------------------------------------------------
c     Specify metadata options
c-----------------------------------------------------------------------
  190   CALL gtmtdt(Inptok)
        IF(Lfatal)RETURN
        GO TO 210
c-----------------------------------------------------------------------
c     Specify spectrum options
c-----------------------------------------------------------------------
  200   CALL gtspec(Sp,Begspn,Endspn,Havesp,Bgspec,Spcdff,Spctyp,Spcsrs,
     &              Mxarsp,Spclim,Peakwd,Lfqalt,Axsame,Svallf,Ldecbl,
     &              Plocal,Spdfor,Lstdff,Lprsfq,Ltk120,Llogqs,Lqchk,
     &              Lrbstsa,Inptok)
        IF(Lfatal)RETURN
        IF(Inptok)hvspec=T
        GO TO 210
       END IF
       IF(.not.Inptok)RETURN
c-----------------------------------------------------------------------
c     check to see if series or composite spec has been specified
c-----------------------------------------------------------------------
       IF(.not.havsrs)THEN
        CALL eWritln('Series for analysis not specifed; a valid '//
     &               'series or composite',Mt2,STDERR,T,F)
        CALL writln('       spec is required.',Mt2,STDERR,F,T)
        Inptok=F
        RETURN
       END IF
c-----------------------------------------------------------------------
c     set up seasonal adjustment indicator variables
c-----------------------------------------------------------------------
       Havesa=Lx11.OR.Lseats
       IF(.not.Havesa)THEN
        IF(Lrvsa.or.Lrvch.or.Lrvtrn.or.Lrvtch.or.Lrvsf)Lx11=T
        IF(Issap.eq.1)Lx11=T
        IF(Lx11)Havesa=T
       END IF
c-----------------------------------------------------------------------
c     Set Muladd and Fcntyp to appropriate values.
c-----------------------------------------------------------------------
       IF(Muladd.eq.NOTSET)THEN
        IF(Fcntyp.eq.0.or.Fcntyp.eq.4.or.
     &    (Fcntyp.eq.5.and.dpeq(Lam,ONE)))THEN
         Muladd=1
        ELSE
         Muladd=0
         IF(Fcntyp.eq.NOTSET)Fcntyp=4
        END IF
       ELSE 
        IF(Fcntyp.eq.0)THEN
         CALL eWritln('Cannot set seasonal adjustment mode when '//
     &                'automatic transformation',Mt2,STDERR,T,F)
         CALL writln('       selection is done.',Mt2,STDERR,F,T)
         Inptok=F
         RETURN
        ELSE IF(Fcntyp.eq.NOTSET)THEN
         Fcntyp=4
        END IF
       END IF
       Tmpma=Muladd
c-----------------------------------------------------------------------
c     Read in a previously stored model file, if requested
c-----------------------------------------------------------------------
       IF(Hvmfil)THEN
        IF(Inptok)THEN
         CALL gtmdfl(Mdlfil,Mtm,Begsrs,Endmdl,Nobs,havsrs,havesp,Userx,
     &               Nrusrx,Bgusrx,Itdtst,Lmodel,Lestim,havreg,Leastr,
     &               Eastst,Luser,Elong,havtca,havhol,Rgaicd,Lam,Fcntyp,
     &               Lomtst,Ch2tst,Chi2cv,Tlimit,Pvaic,Lceaic,Inptok)
         IF(Lfatal)RETURN
        ELSE
c-----------------------------------------------------------------------
c     If there are errors in the spec file, do not read model file.
c-----------------------------------------------------------------------
         CALL inpter(PERROR,Pos,
     &               'Program will not read model file until input '//
     &               'errors are corrected.',T)
        END IF
       END IF
c-----------------------------------------------------------------------
c     Setup the TD, lom adjust the series and the regression variables
c (if there is no boxcox transformation).
c The additive case, td6+lom, is already setup in the regresssion spec.
c For other transformations the series must be prior adjusted for lom
c if it is not already adjusted by the leap year adjustment
c (priadj=2 or 3) and the seventh trading day variable needs to be
c removed.
c-----------------------------------------------------------------------
       IF(Picktd)THEN
        IF(dpeq(Lam,ZERO))THEN
         IF(Priadj.gt.1)THEN
          IF(Kfulsm.lt.2)THEN
           IF(Priadj.eq.2)THEN
            CALL eWritln('Length-of-month prior adustment '//
     &                   'requested in transform spec',
     &                   STDERR,Mt2,T,F)
            WRITE(STDERR,1000)'lom'
            WRITE(Mt2,1000)'lom'
           ELSE IF(Priadj.eq.3)THEN
            CALL eWritln('Length-of-quarter prior adustment '//
     &                   'requested in transform spec',
     &                   STDERR,Mt2,T,F)
            WRITE(STDERR,1000)'loq'
            WRITE(Mt2,1000)'loq'
           ELSE IF(Priadj.eq.4)THEN
            CALL eWritln('Leap year prior adustment '//
     &                   'requested in transform spec',
     &                   STDERR,Mt2,T,F)
            WRITE(STDERR,1000)'lpyear'
            WRITE(Mt2,1000)'lpyear'
           END IF
           CALL writln('        variables argument of the '//
     &                 'regression spec.',
     &                 STDERR,Mt2,F,T)
          ELSE
           perstr='month  '
           iper=5
           IF(Sp.eq.4)then
            perstr='quarter'
            iper=7
           END IF
           IF(Priadj.eq.2)THEN
            CALL eWritln('Length-of-month prior adustment '//
     &                   'requested in transform spec',
     &                   STDERR,Mt2,T,F)
            WRITE(STDERR,1001)perstr(1:iper),'lom'
            WRITE(Mt2,1001)perstr(1:iper),'lom'
           ELSE IF(Priadj.eq.3)THEN
            CALL eWritln('Length-of-quarter prior adustment '//
     &                   'requested in transform spec',
     &                   STDERR,Mt2,T,F)
            WRITE(STDERR,1001)perstr(1:iper),'loq'
            WRITE(Mt2,1001)perstr(1:iper),'loq'
           ELSE IF(Priadj.eq.4)THEN
            CALL eWritln('Leap year prior adustment '//
     &                   'requested in transform spec',
     &                   STDERR,Mt2,T,F)
            WRITE(STDERR,1001)perstr(1:iper),'lpyear'
            WRITE(Mt2,1001)perstr(1:iper),'lpyear'
           END IF
           CALL writln('        spec or change td in the variables '//
     &                 'argument of the regression spec.',
     &                 STDERR,Mt2,F,T)
          END IF
          Inptok=F
         ELSE
          CALL rmlnvr(Priadj,Kfulsm,Nspobs)
          IF(Lfatal)RETURN
         END IF
        ELSE 
         IF(Priadj.gt.1)THEN
          IF(Kfulsm.lt.2)THEN
           IF(Priadj.eq.2)THEN
            CALL eWritln('Length-of-month prior adustment '//
     &                   'requested in transform spec',
     &                   STDERR,Mt2,T,F)
            WRITE(STDERR,1010)'lom'
            WRITE(Mt2,1010)'lom'
           ELSE IF(Priadj.eq.3)THEN
            CALL eWritln('Length-of-quarter prior adustment '//
     &                   'requested in transform spec',
     &                   STDERR,Mt2,T,F)
            WRITE(STDERR,1010)'loq'
            WRITE(Mt2,1010)'loq'
           ELSE IF(Priadj.eq.4)THEN
            CALL eWritln('Leap year prior adustment '//
     &                   'requested in transform spec',
     &                   STDERR,Mt2,T,F)
            WRITE(STDERR,1010)'lpyear'
            WRITE(Mt2,1010)'lpyear'
           END IF
           CALL writln(
     &        '        variables argument of the regression spec.',
     &                 STDERR,Mt2,F,T)
          ELSE
           perstr='lom    '
           IF(Sp.eq.4)perstr='loq    '
           iper=3
           IF(Priadj.eq.2)THEN
            CALL eWritln('Length-of-month prior adustment '//
     &                   'requested in transform spec',
     &                   STDERR,Mt2,T,F)
            WRITE(STDERR,1011)perstr(1:iper),'lom'
            WRITE(Mt2,1011)perstr(1:iper),'lom'
           ELSE IF(Priadj.eq.3)THEN
            CALL eWritln('Length-of-quarter prior adustment '//
     &                   'requested in transform spec',
     &                   STDERR,Mt2,T,F)
            WRITE(STDERR,1011)perstr(1:iper),'loq'
            WRITE(Mt2,1011)perstr(1:iper),'loq'
           ELSE IF(Priadj.eq.4)THEN
            CALL eWritln('Leap year prior adustment '//
     &                   'requested in transform spec',
     &                   STDERR,Mt2,T,F)
            WRITE(STDERR,1011)perstr(1:iper),'lpyear'
            WRITE(Mt2,1011)perstr(1:iper),'lpyear'
           END IF
           CALL writln('        or change td in the variables '//
     &                 'argument of the regression spec.',
     &                 STDERR,Mt2,F,T)
          END IF
          Inptok=F
         ELSE IF(Kfulsm.eq.2)THEN
          CALL replyf()
         END IF
        END IF
       END IF
c-----------------------------------------------------------------------
c     Check for lom in the regression and in the prior adjustment.
c-----------------------------------------------------------------------
       IF(Priadj.gt.1.and.Nb.gt.0)THEN
        DO icol=1,Nb
         IF(Rgvrtp(icol).eq.PRGTLM.or.Rgvrtp(icol).eq.PRGTLQ.or.
     &      Rgvrtp(icol).eq.PRGTLY.or.Rgvrtp(icol).eq.PRGTSL.or.
     &      Rgvrtp(icol).eq.PRGULM.or.Rgvrtp(icol).eq.PRGULQ.or.
     &      Rgvrtp(icol).eq.PRGULY)THEN
          CALL eWritln('Cannot include a length-of-month type '//
     &                 'variable as both a',STDERR,Mt2,T,F)
          CALL writln('        regression variable and a prior '//
     &                'adjustment.',STDERR,Mt2,F,F)
          CALL writln('        Drop at least one from the model.',
     &                STDERR,Mt2,F,T)
          Inptok=F
          GO TO 220
         END IF
        END DO
       END IF
c-----------------------------------------------------------------------
c     Compute the number of effective observations and initialize |G'G|
c-----------------------------------------------------------------------
  220  Lar=Lextar.and.Mxarlg.gt.0
       Lma=Lextma.and.Mxmalg.gt.0
c-----------------------------------------------------------------------
       IF(Lextar)THEN
        Nintvl=Mxdflg
        Nextvl=Mxarlg+Mxmalg
c-----------------------------------------------------------------------
       ELSE
        Nintvl=Mxdflg+Mxarlg
c-----------------------------------------------------------------------
        Nextvl=0
        IF(Lextma)Nextvl=Mxmalg
       END IF
c-----------------------------------------------------------------------
       IF((.not.Hvmfil).and.Fixmdl.eq.0.AND.Lmodel)THEN
        IF(Iregfx.eq.3)Fixmdl=2
        IF(Imdlfx.eq.3)Fixmdl=Fixmdl+1
       END IF
c-----------------------------------------------------------------------
c     If X-11 seasonal adjustment to be done, print a warning message
c     set number of forecasts dropped to zero
c-----------------------------------------------------------------------
       IF(Lx11.and.Fctdrp.gt.0)THEN
        fhnote=STDERR
        IF(Lquiet)fhnote=0
        CALL wWritln('No observations should be excluded from '//
     &               'forecasting when a',Mt2,fhnote,T,F)
        CALL writln('         seasonal adjustment is done.',
     &              Mt2,fhnote,F,T)
        Fctdrp=0
       END IF
c-----------------------------------------------------------------------
c     If X-11 seasonal adjustment to be done, check to see if seasonal
c     period is either 4 or 12.  If not, print error message.
c-----------------------------------------------------------------------
*       IF((.NOT.(Sp.eq.4.or.Sp.eq.12.or.Sp.eq.24.or.Sp.eq.36)).and.
*     &     Lx11)THEN
*        CALL writln('ERROR: Seasonal period must be 4, 12, 24 or 36 if
*     &a seasonal adjustment is done.',Mt2,STDERR,T)
       IF((.NOT.(Sp.eq.4.or.Sp.eq.12)).and.Lx11)THEN
        CALL eWritln('Seasonal period must be 4 or 12 if a seasonal '//
     &               'adjustment is done.',Mt2,STDERR,T,T)
        Inptok=F
        RETURN
       END IF
c-----------------------------------------------------------------------
c     Set ssotl=0 if outlier identification not performed
c-----------------------------------------------------------------------
*       IF((.not.Ltstao).AND.(.not.Ltstls).AND.(.not.Ltsttc).AND.
*     &    (.not.Ltstso))Ssotl=0
       IF((.not.Ltstao).AND.(.not.Ltstls).AND.(.not.Ltsttc))Ssotl=0
c-----------------------------------------------------------------------
c     If backcasts not set, set number of backcasts equal to 0
c-----------------------------------------------------------------------
       IF(Nbcst.eq.NOTSET)Nbcst=0
c-----------------------------------------------------------------------
c     If seasonal adjustment and model estimation are to be done, reset
c     the number of forecasts to one year if a number of forecasts 
c     hasn't been specified by the user.
c-----------------------------------------------------------------------
       IF(Nfcst.eq.NOTSET)THEN
        IF(Lmodel)THEN
         IF(Lx11.or.Lseats)THEN
          IF(.not.Ldestm)Ldestm=T
          IF(Lseats)THEN
           Nfcst=MAX(12,3*Sp)
          ELSE
           Nfcst=Sp
          END IF 
         ELSE IF(hvfcst)THEN
          Nfcst=Sp
         ELSE
          Nfcst=0
         END IF
        ELSE
c-----------------------------------------------------------------------
c     Else, set the number of forecasts equal to zero
c-----------------------------------------------------------------------
         Nfcst=0
        END IF
       END IF
       IF(Lseats.and.Mxcklg.eq.0)Mxcklg=3*Sp
       IF(Iagr.gt.0.and.Iagr.lt.3)X11agr=X11agr.and.Lx11
c-----------------------------------------------------------------------
c     If X-11 regression done, set # of forecasts for X-11 regressions
c-----------------------------------------------------------------------
       IF(Ixreg.gt.0)THEN
        Nfcstx=Nfcst
        Nbcstx=Nbcst
c-----------------------------------------------------------------------
c     Set number of X-11 forecasts to be at least one year 
c-----------------------------------------------------------------------
        IF(Nfcst.lt.Sp)Nfcstx=Sp
       END IF
c-----------------------------------------------------------------------
c     Set up format for table saves
c-----------------------------------------------------------------------
       IF(Svprec.lt.15)Svsize=Svprec+7
       WRITE(Svfmt,1040)Svsize,Svprec
 1040  FORMAT('(sp,e',i2.2,'.',i2.2,')')
c-----------------------------------------------------------------------
c     Reset default prior adjustment factor mode to ratio if
c     multiplicative adjustment and format of prior factors = fsave
c-----------------------------------------------------------------------
       IF(Muladd.ne.1.or.Fcntyp.eq.1)THEN
        DO i=1,Nprtyp
         IF(hvx12f(i).and.Percnt(i).eq.NOTSET)Percnt(i)=1
        END DO
       END IF
c-----------------------------------------------------------------------
c     If model estimated and irregular regression performed, set Ixreg
c     to indicate a prior adjustment.
c-----------------------------------------------------------------------
       IF(Lmodel.and.Ixreg.eq.1)Ixreg=2
c-----------------------------------------------------------------------
       IF(Lmodel)THEN
        IF(outest.eq.NOTSET.and.outamd.eq.NOTSET)THEN
         Outfct=F
         Outfer=F
        ELSE IF(outest.eq.NOTSET)THEN
         Outfer=outamd.eq.1
         Outfct=Outfer
        ELSE IF(outamd.eq.NOTSET)THEN
         Outfct=outest.eq.1
         Outfer=Outfct
        ELSE
         Outfer=outamd.eq.1
         Outfct=outest.eq.1
        END IF
        IF(dpeq(Tcalfa,DNOTST))THEN
         IF(Sp.ge.4)THEN
          Tcalfa=0.7D0**(12D0/DBLE(Sp))
         ELSE
          ntc=0
          IF(Nb.gt.0)THEN
           DO i=1,Nb
            IF(Rgvrtp(i).eq.PRGTTC)ntc=ntc+1
           END DO
          END IF
          IF(ntc.gt.0.or.Ltsttc)THEN
           CALL eWritln('If the seasonal period is less than 4, the '//
     &                  'user must specify the ',Mt2,STDERR,T,F)
           CALL writln('       decay rate for TC outliers.',Mt2,STDERR,
     &                 F,T)
           Inptok=F
           RETURN
          END IF
         END IF
        END IF
       END IF
c-----------------------------------------------------------------------
       Khol=Keastr
       IF((.NOT.(havhol.or.Axrghl.or.Axruhl.or.Khol.eq.1)).and.Finhol)
     &    Finhol=F
       IF((.not.havreg))THEN
        Adjtd=0
        Adjhol=0
        Adjao=0
        Adjls=0
        Adjtc=0
        Adjso=0
        Adjsea=0
        Adjcyc=0
        Adjusr=0
       ELSE IF((.not.Ldestm).and.Lx11)THEN
        IF(Adjtd.gt.0.or.Adjhol.gt.0.or.Adjao.gt.0.or.Adjls.gt.0.or.
     &     Adjtc.gt.0.or.Adjso.gt.0.or.Adjsea.gt.0.or.Adjcyc.gt.0.or.
     &     Adjusr.gt.0.OR.Finusr.or.Finao.or.Finls.or.Fintc.or.
     &     ((.NOT.(Axrghl.or.Axruhl)).and.Finhol).or.Khol.eq.1)Ldestm=T
       END IF
c-----------------------------------------------------------------------
       IF(Issap.eq.1.and.(.not.(Lx11.or.Lseats)))Lx11=T
       IF(.not.(Lx11.or.Lseats))THEN
        IF((Lrvsa.or.Lrvch.or.Lrvtrn.or.Lrvtch.or.Lrvsf).or.
     &     (Issap.eq.1))Lx11=T
        IF(Iyrt.gt.0)THEN
         CALL wWritln('Must specify either the x11 or seats spec '//
     &                'when the force spec is specified.',
     &                Mt2,STDERR,T,F)
         CALL writln('         Options from force spec ignored.',
     &               Mt2,STDERR,F,T)
         Iyrt=0
        END IF
       END IF
c-----------------------------------------------------------------------
       IF(hvspec)THEN
        IF(.not.(Sp.eq.12))THEN
         CALL wWritln(
     &               'Spectrums are only generated for monthly series.',
     &                Mt2,STDERR,T,T)
*         CALL writln('         Options from spectrum spec ignored.',
*     &               Mt2,STDERR,F,T)
        END IF
       ELSE
        IF(Bgspec(YR).eq.NOTSET)THEN
         CALL addate(Endspn,Sp,-95,Bgspec)
         CALL dfdate(Bgspec,Begspn,Sp,nspec)
         IF(nspec.lt.0)CALL cpyint(Begspn,2,1,Bgspec)
        END IF
        IF(Peakwd.eq.NOTSET)THEN
         Peakwd=1
         IF(Sp.eq.4)Peakwd=3
        END IF
       END IF
c-----------------------------------------------------------------------
       RETURN
  210  CONTINUE
      END DO
c-----------------------------------------------------------------------
 1000 FORMAT('        which conflicts with inclusion of leap year ',
     &       'prior adjustment implied ',/,
     &       '        from variable=td in regression spec (with log ',
     &       'transformation).  ',/,
     &       '        Take out adjust=',a,' in the transform spec ',
     &       'or change td in the')
 1001 FORMAT('        which conflicts with inclusion of length-of-',a,
     &       ' prior adjustment',/,
     &       '        implied from variable=td in regression spec ',
     &       '(with log transformation',/,
     &       '        and type=trend in x11 spec).  Take out adjust=',
     &       a,' in the transform')
 1010 FORMAT('        which conflicts with inclusion of lpyear ',
     &       'regression variable from ',/,
     &       '        variable=td in regression spec (with no ',
     &       'transformation).  ',/,
     &       '        Take out adjust=',a,' in the transform spec ',
     &       'or change td in the')
 1011 FORMAT('        which conflicts with inclusion of ',a,
     &       'regression variable from',/,
     &       '        variable=td in regression spec (with no ',
     &       'transformation and',/,
     &       '        type=trend in x11 spec).  Take out adjust=',a,
     &       ' in the transform spec or')
      END
