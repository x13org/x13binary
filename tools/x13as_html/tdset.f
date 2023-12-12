C     Last change:  BCM  20 May 1999    8:53 am
      SUBROUTINE tdset(Sp,Tdgrp,Begdat,Lfda,Llda,Indxtd,Ixreg,Adjtd,
     &                 Adjusr,Kswv,Noxfac)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Initialize variables used for type-of-month trading day table.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'tdtyp.cmn'
      INCLUDE 'xtdtyp.cmn'
c-----------------------------------------------------------------------
      LOGICAL Noxfac
      INTEGER Adjtd,Adjusr,Indxtd,Lfda,Llda,Begdat,predat,Sp,
     &        irow,idate,year,period,fouryr,sdoyr,cendsp,lpyr,sd,nd,
     &        fdomo,lnomo,lnoqtr,fdoqtr,Ixreg,Tdgrp,Kswv
      DIMENSION Begdat(2),predat(2),idate(2),fouryr(0:3),fdomo(12,2),
     &          lnomo(12,2),lnoqtr(4,2),fdoqtr(4,2)
c-----------------------------------------------------------------------
      INTEGER YR,MO
      PARAMETER(YR=1,MO=2)
c-----------------------------------------------------------------------
      DATA fouryr/0,2,3,4/
      DATA lnomo/31,28,31,30,31,30,31,31,30,31,30,31,
     &           31,29,31,30,31,30,31,31,30,31,30,31/
      DATA fdomo/0,3,3,6,1,4,6,2,5,0,3,5,
     &           0,3,4,0,2,5,0,3,6,1,4,6/
      DATA lnoqtr/90,91,92,92,
     &            91,91,92,92/
      DATA fdoqtr/0,6,6,0,
     &            0,0,0,1/
c-----------------------------------------------------------------------
c     Set indicator variable for the type of trading day table.
c     0 - none, 1 - X-11 TD only, 2 - Model TD only, 3 - Both TD
c-----------------------------------------------------------------------
      Tdtbl=0
      IF((Adjtd.eq.1).or.(Indxtd.lt.0.and.Adjusr.eq.1))THEN
       Tdtbl=2
       IF(Tdgrp.gt.0.and.(.not.Noxfac))Tdtbl=3
      ELSE IF(Tdgrp.gt.0.and.(.not.Noxfac))THEN
       Tdtbl=1
       IF(Indxtd.ne.0)Tdtbl=3
      END IF
c-----------------------------------------------------------------------
c     Initialize the variables for X-11 and model based trading day
c-----------------------------------------------------------------------
      CALL setdp(DNOTST,PTD,Tdx11)
      CALL setdp(DNOTST,PTD,Tdx11b)
      CALL setdp(DNOTST,PTD,Tdmdl)
      CALL setdp(DNOTST,PTD,Tdmdl1)
      CALL setdp(DNOTST,2,Lpmdl)
      CALL setdp(DNOTST,2,Lpmdl1)
      CALL setdp(0D0,PLEN,Xnstar)
      CALL setdp(0D0,PLEN,Xlpyr)
      CALL setint(0,PTD,Tday)
c-----------------------------------------------------------------------
c     Set the indicator variable for the type of a given month t.
c     Code goes from 1-28, where 1-7 means 30 (91) day month (quarter) 
c     starting in Monday, Tuesday, ..., Sunday; 8-14 means 31 (92) day 
c     month (quarter) starting in Monday, Tuesday, ..., Sunday; 15-21 
c     is a non-leap year February starting in Monday, Tuesday, ..., 
c     Sunday; 22-28 means a leap year February starting in Monday, 
c     Tuesday, ..., Sunday.  Do only if a trading day table will be 
c     produced.
c-----------------------------------------------------------------------
      IF(Tdtbl.gt.0.or.Ixreg.gt.0.or.Kswv.gt.0)THEN
       CALL addate(Begdat,Sp,-Lfda,predat)
       DO irow=Lfda,Llda
        CALL addate(predat,Sp,irow,idate)
        year=idate(YR)
        period=idate(MO)
c-----------------------------------------------------------------------
c     The calendar as we know it begins in October 1752.  If we define
c Sun=0, Mon=1, ..., Sat=6, then we would like to start our pattern
c on the first leap year that start on a Sunday after 1753.  This is
c 1764.
c-----------------------------------------------------------------------
        sdoyr=5*(year/4-441)+fouryr(mod(year,4))
        cendsp=(year-1601)/100
        cendsp=cendsp-cendsp/4-1
        sdoyr=sdoyr-cendsp
        sdoyr=mod(sdoyr,7)
c-----------------------------------------------------------------------
        IF((mod(year,100).ne.0.and.mod(year,4).eq.0).or.mod(year,400)
     &     .eq.0)THEN
         lpyr=2
        ELSE
         lpyr=1
        END IF
c-----------------------------------------------------------------------
        IF(Sp.eq.12)THEN
         sd=mod(sdoyr+fdomo(period,lpyr),7)
         nd=lnomo(period,lpyr)
        ELSE
         sd=mod(sdoyr+fdoqtr(period,lpyr),7)
         nd=lnoqtr(period,lpyr)
        END IF
        IF(sd.eq.0)sd=7
        Tday(irow)=sd
        Xn(irow)=dble(float(nd))
        Xnstar(irow)=dble(float(nd))
        IF(Sp.eq.12)THEN
         IF(nd.eq.31)Tday(irow)=Tday(irow)+7
         IF(nd.eq.28)Tday(irow)=Tday(irow)+14
         IF(nd.eq.29)Tday(irow)=Tday(irow)+21
         IF(period.eq.2)Xnstar(irow)=28.25D0
        ELSE
         IF(nd.eq.92)Tday(irow)=Tday(irow)+7
         IF(nd.eq.90)Tday(irow)=Tday(irow)+14
c  changes suggested by NBB May 2004
         IF(lpyr.eq.2.and.period.eq.1)Tday(irow)=Tday(irow)+21
         IF(period.eq.1)Xnstar(irow)=90.25D0
c  end of changes
        END IF
        Xlpyr(irow)=Xn(irow)-Xnstar(irow)
       END DO
c-----------------------------------------------------------------------
       IF(Sp.eq.12)THEN
        Daybar=30.4375D0
       ELSE
        Daybar=91.25D0
       END IF
c-----------------------------------------------------------------------
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
