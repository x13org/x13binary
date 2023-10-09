C     Last change:  BCM  22 Sep 1998   10:58 am
      SUBROUTINE regvar(Y,Nobpf,Fctdrp,Nfcst,Nbcst,Userx,Bgusrx,Nrusrx,
     &                  Priadj,Reglom,Nrxy,Begxy,Frstry,Xmeans,Elong)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Adds the data and special regression variables, constant,
c seasonal effects, trading day, AO's and LS's, and ramps.  Checks
c the user defined regression variable input.
c-----------------------------------------------------------------------
c Name   Type  Description
c-----------------------------------------------------------------------
c bgusrX  i  Input begining date for the user defined regression
c             variables, this is notset if there are no user variables
c begXy  i  Output beginning date for the extended Xy matrix (yr,mo)
c             This is set to begspn-nbcst if there are no user
c             regression variables
c begspn  i  Input beginning date for the input series (yr,mo)
c fctdrp  i  Input number of observations dropped off the span before
c             starting the forecasts
c frstry  i  Output the first element of Xy to use in the estimation
c ielt    i  Local index for the current element
c igrp    i  Local index for the current regression group
c itmp    i  Local temporary scalar
c iXymU   i  Local difference between the begining of the user defined
c             regression variables and the begining of the Xy matrix
c             which is defined by the begining of the data - number of
c             backcasts
c nbcst   i  Input number of backcasts requested
c nfcst   i  Input number of forecasts requested
c nrusrX  i  Input number of rows of user defined regression variables
c nrxy  i  I/O On input it's the length of the user defined regression
c             matrix, on output, it's the rows in [X:y].
c one     d  Local PARAMETER for a double precision 1
c puserX  i  PARAMETER of the maximum number of elements in the
c             user specified regression matrix
c pXy     i  Input PARAMETER for the maximum number of elements in the
c             extended [X:y] matrix Xy
c tsrs    d  Work pobs long temporary series vector
c userX   d  Input puserX, nrowsx by nuserg columns used, matrix of
c             user specified regression variables
c y       d  Input pobs (nspobs used) Vector of possibly transformed data
c             undifferenced series
c-----------------------------------------------------------------------
c  Variable Typing and Initialization
c-----------------------------------------------------------------------
      DOUBLE PRECISION ONE,ZERO
      LOGICAL F,T
      INTEGER PLOM,PLOQ
      PARAMETER(F=.false.,T=.true.,ONE=1D0,ZERO=0D0,PLOM=2,PLOQ=3)
c     ------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
c     ------------------------------------------------------------------
      INCLUDE 'error.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'model.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'units.cmn'
c     ------------------------------------------------------------------
      INTEGER PXY
      PARAMETER(PXY=PLEN*(PB+1))
c     ------------------------------------------------------------------
      CHARACTER igrptl*(PGRPCR)
      CHARACTER cstr1*(6),cstr2*(6),cstr3*(6)
      LOGICAL begrgm,chkcvr,lom,locok,Xmeans,ltd1,Elong,lckurg,lurspc
      INTEGER begcol,Bgusrx,begelt,Begxy,ctoi,endcol,endelt,Fctdrp,
     &        Frstry,i,ielt,ipos,itogrp,igrp,ixymu,irow,lstngp,ndays,
     &        Nbcst,nchr,Nfcst,nigrpc,Nobpf,Nrusrx,Nrxy,Priadj,typidx,
     &        Reglom,smpday,strinx,idtpos,regmdt,itype,thisgp,nstr1,
     &        nstr2,nstr3,rtype2
      DOUBLE PRECISION lomadj,Userx,Y,tsrs,emean
      DIMENSION Bgusrx(2),Begxy(2),Userx(PUSERX),Y(*),regmdt(2),
     &          tsrs(PLEN),begrgm(PLEN),emean(PSP)
      EXTERNAL chkcvr,ctoi,strinx
c----------------------------------------------------------------------
      CHARACTER UTYDIC*320
      INTEGER utyptr,PUTY
      PARAMETER(PUTY=15)
      DIMENSION utyptr(0:PUTY)
      PARAMETER(UTYDIC='User-defined SeasonalUser-defined HolidayUser-de
     &fined Holiday Group 2User-defined Holiday Group 3User-defined Holi
     &day Group 4User-defined Holiday Group 5User-defined ConstantUser-d
     &efined Trading DayUser-defined LOMUser-defined LOQUser-defined Lea
     &p YearUser-defined AOUser-defined LSUser-defined SOUser-defined Tr
     &ansitory')
c----------------------------------------------------------------------
      DATA utyptr / 1,22,42,70,98,126,154,175,199,215,231,253,268,283,
     &              298,321 /
c----------------------------------------------------------------------
c     Define the dimensions of the Xy matrix
c-----------------------------------------------------------------------
      lckurg=T
      CALL addate(Begspn,Sp,-Nbcst,Begxy)
      Nrxy=Nspobs+Nbcst+max(0,Nfcst-Fctdrp)
c-----------------------------------------------------------------------
c     Check the dimensions of the data and regressor variables
c-----------------------------------------------------------------------
      IF(Nrxy*Ncxy.gt.PXY)THEN
       CALL itoc(Nrxy,cstr1,nstr1)
       IF(.not.Lfatal)CALL itoc(Ncxy,cstr2,nstr2)
       IF(.not.Lfatal)CALL itoc(PXY,cstr3,nstr3)
       IF(Lfatal)RETURN
       CALL errhdr
       CALL writln('Too many elements in [X:y] '//cstr1(1:(nstr1-1))//
     &             ' * '//cstr2(1:(nstr2-1))//' > '//cstr3(1:(nstr3-1)),
     &             STDERR,Mt2,T,T)
       CALL abend
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Add the possibly transformed data to the last column of Xy.
c Note that there might be rows of regression variables before the data
c starts because of backcasts.
c-----------------------------------------------------------------------
      Frstry=Nbcst*Ncxy+1
      CALL copycl(Y,Nobpf,1,1,Ncxy,Ncxy,Xy(Frstry))
c-----------------------------------------------------------------------
c     Set Observations of Xy that stand in for the backcasts to zero
c     (BCM July 2008)
c-----------------------------------------------------------------------
      IF(Nbcst.gt.0)THEN
       DO irow=1,Nbcst
        ielt=irow*Ncxy
        Xy(ielt)=ZERO
       END DO
      END IF
c-----------------------------------------------------------------------
c     Add each regression variable or group
c-----------------------------------------------------------------------
      igrp=0
      lstngp=Ngrp
      DO WHILE (igrp.lt.Ngrp)
       igrp=igrp+1
       CALL setlg(T,PLEN,begrgm)
c     ------------------------------------------------------------------
       CALL getstr(Grpttl,Grpptr,Ngrp,igrp,igrptl,nchr)
       IF(Lfatal)RETURN
       nigrpc=index(igrptl(1:nchr),'[')-1
       IF(nigrpc.eq.-1)nigrpc=nchr
       IF(igrptl(1:3).eq.'AOS'.or.igrptl(1:3).eq.'LSS')THEN
        nigrpc=3
       ELSE IF(igrptl(1:2).eq.'AO'.or.igrptl(1:2).eq.'LS'.or.
     &    igrptl(1:2).eq.'Rp'.or.igrptl(1:2).eq.'Mi'.or.
     &    igrptl(1:2).eq.'TC'.or.igrptl(1:2).eq.'SO'.or.
     &    igrptl(1:2).eq.'TL'.or.igrptl(1:2).eq.'QI'.or.
     &    igrptl(1:2).eq.'QD')THEN
        nigrpc=2
       END IF
c-----------------------------------------------------------------------
c     Determine the beginning and ending columns in the group
c-----------------------------------------------------------------------
       begcol=Grp(igrp-1)
       endcol=Grp(igrp)-1
       rtype2=Rgvrtp(begcol)
       IF(rtype2.gt.100)rtype2=rtype2-100
c-----------------------------------------------------------------------
c     Determine the type of regression variable
c-----------------------------------------------------------------------
       GO TO(10,20,30,40,50,50,60,70,80,90,
     &       100,110,120,120,120,130,130,140,150,150,
     &       150,150,150,150,150,150,90,120,90,155,
     &       155,155,155,155,155,155,155,140,120,120,
     &       40,150,155,120,130,70,150,155,140,140,
     &       140,140,140,120,120,140,140,140,140,140,
     &       140,140,140,140,140),rtype2
c-----------------------------------------------------------------------
c     Constant is a column of ones filtered by 1/Diff(B).
c-----------------------------------------------------------------------
   10  CALL setdp(ONE,Nrxy,tsrs)
       CALL ratpos(Nrxy,Arimap,Arimal,Opr,Mdl(DIFF-1),Mdl(DIFF)-1,Nrxy,
     &             tsrs)
       CALL copycl(tsrs,Nrxy,1,1,Ncxy,begcol,Xy)
       GO TO 160
c-----------------------------------------------------------------------
c     Seasonal effects
c-----------------------------------------------------------------------
   20  CALL addsef(Begxy,Nrxy,Ncxy,begcol,endcol,Xy,begrgm)
       IF(Lfatal)RETURN
       GO TO 160
c-----------------------------------------------------------------------
c     Trigonometric Seasonal effects
c-----------------------------------------------------------------------
   30  CALL adsncs(Begxy,Sp,Nrxy,Ncxy,Colttl,Colptr,begcol,endcol,Xy,
     &             begrgm)
       IF(Lfatal)RETURN
       GO TO 160
c-----------------------------------------------------------------------
c     Trading Day effects                                            
c-----------------------------------------------------------------------
   40  ltd1=rtype2.eq.PRG1TD.or.rtype2.eq.PRR1TD.or.rtype2.eq.PRA1TD
       CALL td6var(Begxy,Sp,Nrxy,Ncxy,begcol,endcol,0,Xy,begrgm,ltd1)
       IF(Lfatal)RETURN
       GO TO 160
c-----------------------------------------------------------------------
c     Length-of-Month and Length-of-Quarter effects
c-----------------------------------------------------------------------
   50  CALL td7var(Begxy,Sp,Nrxy,Ncxy,begcol,T,F,F,Xy,begrgm)
       GO TO 160
c-----------------------------------------------------------------------
c     Leap Year effect
c-----------------------------------------------------------------------
   60  CALL td7var(Begxy,Sp,Nrxy,Ncxy,begcol,F,F,F,Xy,begrgm)
       GO TO 160
c-----------------------------------------------------------------------
c     Stock Trading Day effects
c-----------------------------------------------------------------------
   70  ltd1=rtype2.eq.PRG1ST.or.rtype2.eq.PRR1ST.or.rtype2.eq.PRA1ST
       ipos=nigrpc+2
       smpday=ctoi(igrptl(1:nchr),ipos)
       CALL td6var(Begxy,Sp,Nrxy,Ncxy,begcol,endcol,smpday,Xy,begrgm,
     &             ltd1)
       IF(Lfatal)RETURN
       GO TO 160
c-----------------------------------------------------------------------
c     Stock Length-of-Month effect
c-----------------------------------------------------------------------
   80  CALL td7var(Begxy,Sp,Nrxy,Ncxy,begcol,T,T,F,Xy,begrgm)
       GO TO 160
c-----------------------------------------------------------------------
c     Easter holiday effect
c-----------------------------------------------------------------------
   90  DO ielt=begcol,endcol
        CALL getstr(Colttl,Colptr,Nb,ielt,igrptl,nchr)
        IF(Lfatal)RETURN
        nigrpc=index(igrptl(1:nchr),'[')-1
        ipos=nigrpc+2
        ndays=ctoi(igrptl(1:nchr),ipos)
        CALL estrmu(Begxy,Nrxy,Sp,ndays,Elong,emean,rtype2.eq.PRGTES)
        CALL adestr(Begxy,Nrxy,Ncxy,Sp,ielt,ndays,Easidx,Xy,Xmeans,
     &              emean,rtype2.eq.PRGTES)
       END DO
       GO TO 160
c-----------------------------------------------------------------------
c     Labor day holiday effect
c-----------------------------------------------------------------------
  100  ipos=nigrpc+2
       ndays=ctoi(igrptl(1:nchr),ipos)
       CALL adlabr(Begxy,Nrxy,Ncxy,begcol,ndays,Xy,Xmeans)
       GO TO 160
c-----------------------------------------------------------------------
c     Thanksgiving-Christmas holiday effect
c-----------------------------------------------------------------------
  110  ipos=nigrpc+2
       ndays=ctoi(igrptl(1:nchr),ipos)
       CALL adthnk(Begxy,Nrxy,Ncxy,begcol,ndays,Xy,Xmeans)
       GO TO 160
c-----------------------------------------------------------------------
c     AOs, LSs, MVs, TCs, SOs, TLs, and Ramps
c-----------------------------------------------------------------------
  120  CALL addotl(Begxy,Nrxy,Nbcst,begcol,endcol)
       IF(Lfatal)RETURN
       GO TO 160
c-----------------------------------------------------------------------
c     Automatically Identified Outliers
c-----------------------------------------------------------------------
  130  CALL addotl(Begxy,Nrxy,Nbcst,begcol,endcol)
       IF(Lfatal)RETURN
       GO TO 160
c-----------------------------------------------------------------------
c     User-defined regression variables.  First check the dates or the
c variables.
c-----------------------------------------------------------------------
  140  IF(lckurg)THEN
        IF(.not.chkcvr(Bgusrx,Nrusrx,Begspn,Nspobs,Sp))THEN
         CALL cvrerr('user-defined regression variables',Bgusrx,Nrusrx,
     &               'span of data',Begspn,Nspobs,Sp)
         IF(.not.Lfatal)CALL abend()
         RETURN
c     ------------------------------------------------------------------
        ELSE IF(.not.chkcvr(Bgusrx,Nrusrx,Begxy,Nrxy,Sp))THEN
         CALL cvrerr('user-defined regression variables',Bgusrx,Nrusrx,
     &               'forecasts',Begxy,Nrxy,Sp)
         IF(.not.Lfatal)CALL abend()
         RETURN
c     ------------------------------------------------------------------
        END IF
        lckurg=F
       END IF
       CALL dfdate(Begxy,Bgusrx,Sp,ixymu)
c     ------------------------------------------------------------------
c      Determine what type of user defined regressor is being added
c     ------------------------------------------------------------------
       typidx=strinx(F,UTYDIC,utyptr,1,PUTY,igrptl(1:nchr))
       itype=typidx
       IF(typidx.eq.1)THEN
        itype=PRGTUS
       ELSE IF(typidx.ge.2.and.typidx.le.6)THEN
        itype=PRGTUH+typidx-2
       ELSE IF(typidx.eq.7)THEN
        itype=PRGUCN
       ELSE IF(typidx.eq.8)THEN
        itype=PRGUTD
       ELSE IF(typidx.eq.9)THEN
        itype=PRGULM
       ELSE IF(typidx.eq.10)THEN
        itype=PRGULQ
       ELSE IF(typidx.eq.11)THEN
        itype=PRGULY
       ELSE IF(typidx.eq.12)THEN
        itype=PRGUAO
       ELSE IF(typidx.eq.13)THEN
        itype=PRGULS
       ELSE IF(typidx.eq.14)THEN
        itype=PRGUSO
       ELSE IF(typidx.eq.15)THEN
        itype=PRGUCY
       END IF
c     ------------------------------------------------------------------
c      Only add the type of user defined regressor specified
c     ------------------------------------------------------------------
       thisgp=1
       DO i=1,Ncusrx
        lurspc=(Usrtyp(i).ge.PRGTUH.and.Usrtyp(i).le.PRGUH5).or.
     &          Usrtyp(i).eq.PRGTUS.or.
     &         (Usrtyp(i).ge.PRGUTD.and.Usrtyp(i).le.PRGUCY)
        IF(lurspc)THEN
         IF(itype.eq.Usrtyp(i))THEN
          itogrp=begcol+thisgp-1
          CALL copycl(Userx(ixymu*ncusrx+1),Nrxy,ncusrx,i,Ncxy,
     &                  itogrp,Xy)
          thisgp=thisgp+1
         END IF
        ELSE IF(itype.eq.0)THEN
         itogrp=begcol+thisgp-1
         CALL copycl(Userx(ixymu*ncusrx+1),Nrxy,ncusrx,i,Ncxy,
     &               itogrp,Xy)
         thisgp=thisgp+1
        END IF
       END DO
       GO TO 160
c----------------------------------------------------------------------
c     Change of regime regression variables.  First, get the date of
c     the change-of-regime from the group title.
c----------------------------------------------------------------------
  150  idtpos=index(igrptl(1:nchr),'(before ')+8
       IF(idtpos.eq.8)
     &    idtpos=index(igrptl(1:nchr),'(change for before ')+19
       CALL ctodat(igrptl(1:nchr-1),Sp,idtpos,regmdt,locok)
c----------------------------------------------------------------------
c     Set the pointer for the start of the regime change, then 
c     generate the proper regression variables.
c----------------------------------------------------------------------
       CALL gtrgpt(Begxy,regmdt,1,begrgm,Nrxy)
       IF(rtype2.eq.PRR1TD)GO TO 40
       IF(rtype2.eq.PRR1ST)GO TO 70
       GO TO(20,30,40,50,50,60,70,80),rtype2-PRRTSE+1
c----------------------------------------------------------------------
c     Change of regime regression variables, regressor defined after
c     the change of regime data.  First, get the date of the 
c     change-of-regime from the group title.
c----------------------------------------------------------------------
  155  idtpos=index(igrptl(1:nchr),'(starting ')+10
       IF(idtpos.eq.10)
     &    idtpos=index(igrptl(1:nchr),'(change for after ')+18
       CALL ctodat(igrptl(1:nchr-1),Sp,idtpos,regmdt,locok)
c----------------------------------------------------------------------
c     Set the pointer for the start of the regime change, then 
c     check to see if the start of the regime date is before the end
c     of the data.
c----------------------------------------------------------------------
       CALL gtrgpt(Begxy,regmdt,-1,begrgm,Nrxy)
c----------------------------------------------------------------------
c     generate the proper regression variables.
c----------------------------------------------------------------------
       IF(rtype2.eq.PRA1TD)GO TO 40
       IF(rtype2.eq.PRA1ST)GO TO 70
       GO TO(20,30,40,50,50,60,70,80),rtype2-PRATSE+1
c----------------------------------------------------------------------
c     In case a group (of outliers outside the span) has been deleted
c then do not index igrp because the next group has move into the
c previous location.
c----------------------------------------------------------------------
  160  IF(lstngp.gt.Ngrp)igrp=igrp-1
       lstngp=Ngrp
      END DO
c----------------------------------------------------------------------
c     Generate length of month factors and adjust the regression
c variables.  The series is already adjusted in adjsrs because
c those prior adjustment factors are used in the calculation of the
c jacobian in prlkhd but the regression variables are not.
c For the regression adjustment none=1, td=2, or all=3
c-----------------------------------------------------------------------
      IF(Reglom.gt.1.and.Priadj.gt.1)THEN
c-----------------------------------------------------------------------
c     Generate length of period factors.  Note lom and loq are the
c same the factor is determined by the seasonal period.
c-----------------------------------------------------------------------
       IF(Priadj.eq.PLOM.or.Priadj.eq.PLOQ)THEN
        lom=T
       ELSE
        lom=F
       END IF
c-----------------------------------------------------------------------
c     The 7th trading day factors
c-----------------------------------------------------------------------
       CALL setlg(T,PLEN,begrgm)
       CALL td7var(Begxy,Sp,Nrxy,1,1,lom,F,T,tsrs,begrgm)
c-----------------------------------------------------------------------
       IF(Reglom.eq.3)THEN
        begcol=1
        endcol=Ncxy-1
c     ------------------------------------------------------------------
       ELSE
        igrp=strinx(F,Grpttl,Grpptr,1,Ngrptl,'Trading Day')
        IF(igrp.le.0)GO TO 170
        begcol=Grp(igrp-1)
        endcol=Grp(igrp)-1
       END IF
c     ------------------------------------------------------------------
       DO irow=1,Nrxy
        begelt=Ncxy*(irow-1)
        endelt=begelt+endcol
        begelt=begelt+begcol
        lomadj=tsrs(irow)
        DO ielt=begelt,endelt
         Xy(ielt)=Xy(ielt)*lomadj
        END DO
       END DO
      END IF
c     ------------------------------------------------------------------
  170 RETURN
      END
