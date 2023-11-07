c     Last change:9/29/2021, only print F test when Lprtdt is true
      SUBROUTINE tdftest(Xpxinv,Regidx,Lprtdt,Lsvtdt,Lsvlog,Lxreg)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     generate model-based f-tests for seasonality from chi square
c     statistics of seasonal regressors; also generate model-based
c     f-tests for combinations of seasonal regression groups,
c     such as change of regime regressors and user defined seasonal
c     regressors
c     (BCM July 2007)
c-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'mdldg.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'usrxrg.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      CHARACTER grpstr*(PGRPCR),rg0str*(PGRPCR),rg1str*(PGRPCR),
     &          rg2str*(PGRPCR)
      DOUBLE PRECISION Xpxinv,chi2vl,pv
      LOGICAL havlp,havlm,Lxreg,Lprtdt,Lsvtdt,Lsvlog,lprthd,lprund
      INTEGER baselt,begcol,endcol,igrp,gtdall,gtdrg,gtdrg1,gtdrg2,gutd,
     &        Regidx,rgi2,rtype,df,df1,df2,iusr,utype,info,tbwdth,nchr,
     &        icol,ud1st,udlast,i,nchr0,nchr1,nchr2,ipos,ntdgp,k
      DIMENSION gtdall(0:2),gtdrg(0:2),gtdrg1(0:2),gtdrg2(0:2),
     &          gutd(0:2),Regidx(PB),rgi2(PB),Xpxinv(PXPX)
c-----------------------------------------------------------------------
      DOUBLE PRECISION fvalue
      EXTERNAL fvalue
c-----------------------------------------------------------------------
      lprthd=Lprtdt
      lprund=F
      tbwdth=71
      ntdgp=0
c-----------------------------------------------------------------------
c     Compute number of regressors estimated
c-----------------------------------------------------------------------
      k=Nb
      IF(Iregfx.eq.2)THEN
       DO igrp=1,Ngrp
        begcol=Grp(igrp-1)
        endcol=Grp(igrp)-1
        DO icol=begcol,endcol
         IF(regidx(icol).eq.NOTSET)k=k-1
        END DO
       END DO
      END IF
      df2=Nspobs-Mxdflg-k
c-----------------------------------------------------------------------
c   Print out f-tests for individual groups of trading day regressors
c-----------------------------------------------------------------------
      DO igrp=1,Ngrp
       begcol=Grp(igrp-1)
       rtype=Rgvrtp(begcol)
       IF(rtype.eq.PRGTTD.or.rtype.eq.PRRTTD.or.rtype.eq.PRATTD.or.
     &    rtype.eq.PRG1TD.or.rtype.eq.PRR1TD.or.rtype.eq.PRA1TD.or.
     &    rtype.eq.PRGTST.or.rtype.eq.PRRTST.or.rtype.eq.PRATST.or.
     &    rtype.eq.PRG1ST.or.rtype.eq.PRR1ST.or.rtype.eq.PRR1ST)THEN
        IF(Lprtdt)lprund=T
        endcol=Grp(igrp)-1
        CALL getstr(Grpttl,Grpptr,Ngrp,igrp,grpstr,nchr)
        IF(Lfatal)RETURN
        info=0
        baselt=regidx(begcol)
        df=endcol-begcol+1
        IF(Iregfx.eq.2)THEN
         IF(baselt.eq.NOTSET)df=df-1
         DO icol=begcol+1,endcol
          IF(regidx(icol).eq.NOTSET)THEN
           df=df-1
          ELSE
           baselt=regidx(icol)
          END IF
         END DO
        END IF
        IF(baselt.ne.NOTSET)THEN
         CALL chitst(Xpxinv,begcol,endcol,chi2vl,pv,regidx,T,info)
c         call profiler(2,'tdftest, chitst 1, info =')
c         write(Mtprof,*)info
        end if
        df1=df
c        write(*,*)'  df1 = ',df1
c        df2=Nspobs-Mxdflg-df
c        write(*,*)'  df2,Nspobs,Mxdflg,df = ',df2,Nspobs,Mxdflg,df
        Tdfval=(chi2vl/dble(df1))*(dble(df2)/dble(Nspobs-Mxdflg))
        Tdfpv=fvalue(Tdfval,df1,df2)
        CALL prtft(Lprtdt,lprthd,tbwdth,Lsvtdt,Lsvlog,baselt,grpstr,
     &             nchr,'Trading Day',11,info,df1,df2,Tdfval,Tdfpv,1071)
       END IF
      END DO
c-----------------------------------------------------------------------
c     Create pointer dictionaries for different tests we wish to
c     perform
c-----------------------------------------------------------------------
      DO icol=0,2
       gtdall(icol)=0
       gtdrg(icol)=0
       gtdrg1(icol)=0
       gtdrg2(icol)=0
       gutd(icol)=0
      END DO
      ud1st=NOTSET
      udlast=NOTSET
      iusr=1
c-----------------------------------------------------------------------
      DO igrp=1,Ngrp
       begcol=Grp(igrp-1)
       endcol=Grp(igrp)-1
       rtype=Rgvrtp(begcol)
c-----------------------------------------------------------------------
       IF((rtype.eq.PRGTTD.or.rtype.eq.PRGTST.or.rtype.eq.PRRTTD.or.
     &    rtype.eq.PRRTST.or.rtype.eq.PRATTD.or.rtype.eq.PRATST.or.
     &    rtype.eq.PRG1TD.or.rtype.eq.PRR1TD.or.rtype.eq.PRA1TD.or.
     &    rtype.eq.PRG1ST.or.rtype.eq.PRR1ST.or.rtype.eq.PRA1ST).or.
     &    (rtype.eq.PRGTLM.or.rtype.eq.PRGTSL.or.rtype.eq.PRGTLQ.or.
     &    rtype.eq.PRGTLY.or.rtype.eq.PRRTLM.or.rtype.eq.PRRTSL.or.
     &    rtype.eq.PRRTLQ.or.rtype.eq.PRRTLY.or.rtype.eq.PRATLM.or.
     &    rtype.eq.PRATSL.or.rtype.eq.PRATLQ.or.rtype.eq.PRATLY))THEN
        gtdall(0)=gtdall(0)+1
        IF(gtdall(0).eq.1)gtdall(1)=begcol
        gtdall(2)=endcol
       END IF
       IF((rtype.eq.PRGTTD.or.rtype.eq.PRGTST.or.rtype.eq.PRG1TD.or.
     &     rtype.eq.PRG1ST).or.
     &    (rtype.eq.PRGTLM.or.rtype.eq.PRGTSL.or.rtype.eq.PRGTLQ.or.
     &     rtype.eq.PRGTLY))THEN
        gtdrg(0)=gtdrg(0)+1
        IF(gtdrg(0).eq.1)gtdrg(1)=begcol
        gtdrg(2)=endcol
        CALL getstr(Grpttl,Grpptr,Ngrp,igrp,rg0str,nchr0)
        IF(Lfatal)RETURN
       END IF
       IF((rtype.eq.PRRTTD.or.rtype.eq.PRRTST.or.rtype.eq.PRR1TD.or.
     &    (rtype.eq.PRR1ST).or.rtype.eq.PRRTLM.or.rtype.eq.PRRTSL.or.
     &     rtype.eq.PRRTLQ.or.rtype.eq.PRRTLY))THEN
        gtdrg1(0)=gtdrg1(0)+1
        IF(gtdrg1(0).eq.1)gtdrg1(1)=begcol
        gtdrg1(2)=endcol
        CALL getstr(Grpttl,Grpptr,Ngrp,igrp,rg1str,nchr1)
        IF(Lfatal)RETURN
       END IF
       IF((rtype.eq.PRATTD.or.rtype.eq.PRATST.or.rtype.eq.PRA1TD.or.
     &    (rtype.eq.PRA1ST).or.rtype.eq.PRATLM.or.rtype.eq.PRATSL.or.
     &     rtype.eq.PRATLQ.or.rtype.eq.PRATLY))THEN
        gtdrg2(0)=gtdrg2(0)+1
        IF(gtdrg2(0).eq.1)gtdrg2(1)=begcol
        gtdrg2(2)=endcol
        CALL getstr(Grpttl,Grpptr,Ngrp,igrp,rg2str,nchr2)
        IF(Lfatal)RETURN
       END IF
c-----------------------------------------------------------------------
       IF(rtype.eq.PRGTUD.or.rtype.eq.PRGTUS.or.rtype.eq.PRGTUH.or.
     &    rtype.eq.PRGUH2.or.rtype.eq.PRGUH3.or.rtype.eq.PRGUH4.or.
     &    rtype.eq.PRGUH5.or.rtype.eq.PRGUAO.or.rtype.eq.PRGULS.or.
     &    rtype.eq.PRGUSO.or.rtype.eq.PRGUCN.or.rtype.eq.PRGUCY.or.
     &    rtype.eq.PRGUTD.or.rtype.eq.PRGULM.or.rtype.eq.PRGULQ.or.
     &    rtype.eq.PRGULY)THEN
        IF(ud1st.eq.NOTSET)ud1st=begcol
       END IF
       IF(rtype.eq.PRGUTD.or.rtype.eq.PRGULM.or.rtype.eq.PRGULQ.or.
     &    rtype.eq.PRGULY)THEN
        IF(gtdall(0).eq.0)gtdall(1)=begcol
        gtdall(0)=gtdall(0)+1
        gtdall(2)=endcol
c-----------------------------------------------------------------------
        IF(rtype.eq.PRGUTD)THEN
         IF(gutd(0).eq.0)gutd(1)=begcol
         gutd(0)=gutd(0)+1
         gutd(2)=endcol
        END IF
       END IF
       udlast=utype
      END DO
c-----------------------------------------------------------------------
c     Generate combined Chi-Square test for trading day regressors
c-----------------------------------------------------------------------
      IF(gtdrg(0).ge.2.and.gtdrg(2).lt.gtdall(2))THEN
       CALL setint(NOTSET,Nb,rgi2)
       havlp=F
       havlm=F
       df=gtdrg(2)-gtdrg(1)+1
       baselt=regidx(gtdrg(1))
       DO icol=gtdrg(1),gtdrg(2)
        rtype=Rgvrtp(icol)
        IF((rtype.eq.PRGTTD.or.rtype.eq.PRGTST.or.rtype.eq.PRG1TD.or.
     &      rtype.eq.PRG1ST).or.
     &     (rtype.eq.PRGTLM.or.rtype.eq.PRGTSL.or.rtype.eq.PRGTLQ.or.
     &      rtype.eq.PRGTLY))THEN
         rgi2(icol)=Regidx(icol)
         IF(regidx(icol).eq.NOTSET)THEN
          df=df-1
         ELSE IF(baselt.eq.NOTSET)THEN
          baselt=rgi2(icol)
         END IF
        ELSE
         df=df-1
        END IF
        IF(rtype.eq.PRGTLY)havlp=T
        IF(rtype.eq.PRGTLM.or.rtype.eq.PRGTSL.or.rtype.eq.PRGTLQ)havlm=T
       END DO
       CALL chitst(Xpxinv,gtdrg(1),gtdrg(2),chi2vl,pv,rgi2,F,info)
c-----------------------------------------------------------------------
c     Print out and/or save chi square statistic
c-----------------------------------------------------------------------
       nchr=13
       grpstr(1:nchr)='Trading Day '
       IF(havlp)THEN
        grpstr(nchr+1:nchr+12)='+ Leap Year '
        nchr=nchr+12
       ELSE IF(havlm)THEN
        IF(Sp.eq.4)THEN
         grpstr(nchr+1:nchr+20)='+ Length of Quarter '
         nchr=nchr+20
        ELSE
         grpstr(nchr+1:nchr+18)='+ Length of Month '
         nchr=nchr+18
        END IF
       END IF
       ipos=index(rg0str(1:nchr0),'(')
       grpstr(nchr+1:(nchr+nchr0-ipos+1))=rg0str(ipos:nchr0)
       nchr=nchr+nchr0-ipos+1
       df1=df
c       df2=Nspobs-Mxdflg-df1
       Tdfval=(chi2vl/dble(df1))*(dble(df2)/dble(Nspobs-Mxdflg))
       Tdfpv=fvalue(Tdfval,df1,df2)
       CALL prtft(Lprtdt,lprthd,tbwdth,Lsvtdt,Lsvlog,baselt,grpstr,
     &            nchr,'Trading Day',11,info,df1,df2,Tdfval,Tdfpv,1071)
      ELSE
       ntdgp=0
       IF(gtdrg(0).gt.0)ntdgp=ntdgp+1
       IF(gtdrg1(0).gt.0)ntdgp=ntdgp+1
       IF(gtdrg2(0).gt.0)ntdgp=ntdgp+1
       IF(gutd(0).gt.0)ntdgp=ntdgp+1
       IF(ntdgp.gt.1.and.gutd(0).gt.0)THEN
        CALL setint(NOTSET,Nb,rgi2)
        df=gtdall(2)-gtdall(1)+1
        baselt=regidx(gtdall(1))
        DO icol=gtdall(1),gtdall(2)
         rtype=Rgvrtp(icol)
         IF((rtype.eq.PRGUTD.or.rtype.eq.PRGULM.or.rtype.eq.PRGULQ.or.
     &       rtype.eq.PRGULY).and.gutd(0).gt.0)THEN
          rgi2(icol)=Regidx(icol)
          IF(regidx(icol).eq.NOTSET)THEN
           df=df-1
          ELSE IF(baselt.eq.NOTSET)THEN
           baselt=rgi2(icol)
          END IF
         ELSE
          df=df-1
         END IF
        END DO
        CALL chitst(Xpxinv,gtdall(1),gtdall(2),chi2vl,pv,rgi2,F,info)
c-----------------------------------------------------------------------
c     Print out and/or save chi square statistic
c-----------------------------------------------------------------------
        nchr=31
        grpstr(1:nchr)='Combined Trading Day Regressors'
        df1=df
c        df2=Nspobs-Mxdflg-df1
        Tdfval=(chi2vl/dble(df1))*(dble(df2)/dble(Nspobs-Mxdflg))
        Tdfpv=fvalue(Tdfval,df1,df2)
        CALL prtft(Lprtdt,lprthd,tbwdth,Lsvtdt,Lsvlog,baselt,grpstr,
     &             nchr,'Trading Day',11,info,df1,df2,Tdfval,Tdfpv,1071)
       END IF
      END IF 
c-----------------------------------------------------------------------
c     Generate combined Chi-Square test for trading day and lom
c     change of regime regressors
c-----------------------------------------------------------------------
      IF(gtdrg1(0).ge.2.and.(.not.(gtdrg1(1).eq.gtdall(1).and.
     &   gtdrg1(2).eq.gtdall(2))))THEN
       CALL setint(NOTSET,Nb,rgi2)
       havlp=F
       havlm=F
       df=gtdrg1(2)-gtdrg1(1)+1
       baselt=regidx(gtdrg1(1))
       DO icol=gtdrg1(1),gtdrg1(2)
        rtype=Rgvrtp(icol)
        IF((rtype.eq.PRRTTD.or.rtype.eq.PRRTST.or.rtype.eq.PRR1TD.or.
     &      rtype.eq.PRR1ST).or.
     &     (rtype.eq.PRRTLM.or.rtype.eq.PRRTSL.or.rtype.eq.PRRTLQ.or.
     &      rtype.eq.PRRTLY).or.(Fulltd.and.
     &     (rtype.eq.PRGTTD.or.rtype.eq.PRGTST.or.rtype.eq.PRG1TD.or.
     &      rtype.eq.PRG1ST).or.
     &     (rtype.eq.PRGTLM.or.rtype.eq.PRGTSL.or.rtype.eq.PRGTLQ.or.
     &      rtype.eq.PRGTLY)))THEN
         rgi2(icol)=Regidx(icol)
         IF(regidx(icol).eq.NOTSET)THEN
          df=df-1
         ELSE IF(baselt.eq.NOTSET)THEN
          baselt=rgi2(icol)
         END IF
        ELSE
         df=df-1
        END IF
        IF(rtype.eq.PRRTLY)THEN
         havlp=T
        ELSE IF(rtype.eq.PRRTLM.or.rtype.eq.PRRTSL.or.rtype.eq.PRRTLQ
     &          .or.(Fulltd.and.(rtype.eq.PRGTLM.or.rtype.eq.PRGTSL.or.
     &               rtype.eq.PRGTLQ.or.rtype.eq.PRGTLY)))THEN
         havlm=T
        END IF
       END DO
       CALL chitst(Xpxinv,gtdrg1(1),gtdrg1(2),chi2vl,pv,rgi2,F,info)
c-----------------------------------------------------------------------
c     Print out and/or save chi square statistic
c-----------------------------------------------------------------------
       nchr=13
       grpstr(1:nchr)='Trading Day '
       IF(havlp)THEN
        grpstr(nchr+1:nchr+12)='+ Leap Year '
        nchr=nchr+12
       ELSE IF(havlm)THEN
        IF(Sp.eq.4)THEN
         grpstr(nchr+1:nchr+20)='+ Length of Quarter '
         nchr=nchr+20
        ELSE
         grpstr(nchr+1:nchr+18)='+ Length of Month '
         nchr=nchr+18
        END IF
       END IF
       ipos=index(rg1str(1:nchr1),'(')
       grpstr(nchr+1:(nchr+nchr1-ipos+1))=rg1str(ipos:nchr1)
       nchr=nchr+nchr1-ipos+1
       df1=df
c       df2=Nspobs-Mxdflg-df1
       Tdfval=(chi2vl/dble(df1))*(dble(df2)/dble(Nspobs-Mxdflg))
       Tdfpv=fvalue(Tdfval,df1,df2)
       CALL prtft(Lprtdt,lprthd,tbwdth,Lsvtdt,Lsvlog,baselt,grpstr,
     &            nchr,'Trading Day',11,info,df1,df2,Tdfval,Tdfpv,1071)
      END IF 
c-----------------------------------------------------------------------
      IF(gtdrg2(0).ge.2.and.(.not.(gtdrg2(1).eq.gtdall(1).and.
     &   gtdrg2(2).eq.gtdall(2))))THEN
       CALL setint(NOTSET,Nb,rgi2)
       havlp=F
       havlm=F
       df=gtdrg2(2)-gtdrg2(1)+1
       baselt=regidx(gtdrg2(1))
       DO icol=gtdrg2(1),gtdrg2(2)
        rtype=Rgvrtp(icol)
        IF((rtype.eq.PRATTD.or.rtype.eq.PRATST.or.rtype.eq.PRA1TD).or.
     &     (rtype.eq.PRATLM.or.rtype.eq.PRATSL.or.rtype.eq.PRATLQ.or.
     &      rtype.eq.PRATLY))THEN
         rgi2(icol)=Regidx(icol)
         IF(regidx(icol).eq.NOTSET)THEN
          df=df-1
         ELSE IF(baselt.eq.NOTSET)THEN
          baselt=rgi2(icol)
         END IF
        ELSE
         df=df-1
        END IF
        IF(rtype.eq.PRATLY)THEN
         havlp=T
        ELSE IF(rtype.eq.PRATLM.or.rtype.eq.PRATSL.or.rtype.eq.PRATLQ)
     &          THEN
         havlm=T
        END IF
       END DO
       CALL chitst(Xpxinv,gtdrg2(1),gtdrg2(2),chi2vl,pv,rgi2,F,info)
c-----------------------------------------------------------------------
c     Print out and/or save chi square statistic
c-----------------------------------------------------------------------
       nchr=13
       grpstr(1:nchr)='Trading Day '
       IF(havlp)THEN
        grpstr(nchr+1:nchr+12)='+ Leap Year '
        nchr=nchr+12
       ELSE IF(havlm)THEN
        IF(Sp.eq.4)THEN
         grpstr(nchr+1:nchr+20)='+ Length of Quarter '
         nchr=nchr+20
        ELSE
         grpstr(nchr+1:nchr+18)='+ Length of Month '
         nchr=nchr+18
        END IF
       END IF
       ipos=index(rg2str(1:nchr2),'(')
       grpstr(nchr+1:(nchr+nchr2-ipos+1))=rg2str(ipos:nchr2)
       nchr=nchr+nchr2-ipos+1
       df1=df
c       df2=Nspobs-Mxdflg-df1
       Tdfval=(chi2vl/dble(df1))*(dble(df2)/dble(Nspobs-Mxdflg))
       Tdfpv=fvalue(Tdfval,df1,df2)
       CALL prtft(Lprtdt,lprthd,tbwdth,Lsvtdt,Lsvlog,baselt,grpstr,
     &            nchr,'Trading Day',11,info,df1,df2,Tdfval,Tdfpv,1071)
      END IF
c-----------------------------------------------------------------------
c     Generate combined Chi-Square test for trading day and lom
c     regressors
c-----------------------------------------------------------------------
      IF(gtdall(0).ge.2.and.(gutd(0).gt.0.and.gtdall(0).gt.gutd(0)))THEN
       CALL setint(NOTSET,Nb,rgi2)
       havlp=F
       havlm=F
       df=gtdall(2)-gtdall(1)+1
       baselt=regidx(gtdall(1))
       DO icol=gtdall(1),gtdall(2)
        rtype=Rgvrtp(icol)
        IF((rtype.eq.PRGTTD.or.rtype.eq.PRGTST.or.rtype.eq.PRRTTD.or.
     &      rtype.eq.PRRTST.or.rtype.eq.PRATTD.or.rtype.eq.PRATST.or.
     &      rtype.eq.PRG1TD.or.rtype.eq.PRR1TD.or.rtype.eq.PRA1TD.or.
     &      rtype.eq.PRG1ST.or.rtype.eq.PRR1ST.or.rtype.eq.PRA1ST).or.
     &     (rtype.eq.PRGTLM.or.rtype.eq.PRGTSL.or.rtype.eq.PRGTLQ.or.
     &      rtype.eq.PRGTLY.or.rtype.eq.PRRTLM.or.rtype.eq.PRRTSL.or.
     &      rtype.eq.PRRTLQ.or.rtype.eq.PRRTLY.or.rtype.eq.PRATLM.or.
     &      rtype.eq.PRATSL.or.rtype.eq.PRATLQ.or.rtype.eq.PRATLY).or.
     &     (rtype.eq.PRGUTD.or.rtype.eq.PRGULM.or.rtype.eq.PRGULQ.or.
     &      rtype.eq.PRGULY))THEN
         rgi2(icol)=Regidx(icol)
         IF(regidx(icol).eq.NOTSET)THEN
          df=df-1
         ELSE IF(baselt.eq.NOTSET)THEN
          baselt=rgi2(icol)
         END IF
        ELSE
         df=df-1
        END IF
        IF(rtype.eq.PRGTLY.or.rtype.eq.PRRTLY.or.rtype.eq.PRATLY.or.
     &     rtype.eq.PRGULY)THEN
         havlp=T
        ELSE IF(rtype.eq.PRGTLM.or.rtype.eq.PRGTSL.or.rtype.eq.PRGTLQ
     &      .or.rtype.eq.PRRTLM.or.rtype.eq.PRRTSL.or.rtype.eq.PRRTLQ
     &      .or.rtype.eq.PRATLM.or.rtype.eq.PRATSL.or.rtype.eq.PRATLQ
     &      .or.rtype.eq.PRGULM.or.rtype.eq.PRGULQ)THEN
         havlm=T
        END IF
       END DO
       CALL chitst(Xpxinv,gtdall(1),gtdall(2),chi2vl,pv,rgi2,F,info)
c-----------------------------------------------------------------------
c     Print out and/or save chi square statistic
c-----------------------------------------------------------------------
       nchr=21
       grpstr(1:nchr)='Combined Trading Day '
       IF(havlp)THEN
        grpstr(nchr+1:nchr+14)='and Leap Year '
        nchr=nchr+14
       ELSE IF(havlm)THEN
        IF(Sp.eq.4)THEN
         grpstr(nchr+1:nchr+22)='and Length of Quarter '
         nchr=nchr+22
        ELSE
         grpstr(nchr+1:nchr+20)='and Length of Month '
         nchr=nchr+20
        END IF
       END IF
       grpstr(nchr+1:nchr+10)='Regressors'
       nchr=nchr+10
       df1=df
c       df2=Nspobs-Mxdflg-df1
       Tdfval=(chi2vl/dble(df1))*(dble(df2)/dble(Nspobs-Mxdflg))
       Tdfpv=fvalue(Tdfval,df1,df2)
       CALL prtft(Lprtdt,lprthd,tbwdth,Lsvtdt,Lsvlog,baselt,grpstr,
     &            nchr,'Trading Day',11,info,df1,df2,Tdfval,Tdfpv,1071)
      END IF
c-----------------------------------------------------------------------
c     Generate combined Chi-Square test for user defined trading day
c     regressors if there are more than one type of user defined
c     regressor defined.
c-----------------------------------------------------------------------
      IF(ud1st.eq.NOTSET)THEN
       ntdgp=0
       IF(gtdrg(0).gt.0)ntdgp=ntdgp+1
       IF(gtdrg1(0).gt.0)ntdgp=ntdgp+1
       IF(gtdrg2(0).gt.0)ntdgp=ntdgp+1
       IF(gutd(0).gt.0)ntdgp=ntdgp+1
       IF(ntdgp.gt.1)THEN
        CALL setint(NOTSET,Nb,rgi2)
        havlp=F
        havlm=F
        df=gtdall(2)-gtdall(1)+1
        baselt=regidx(gtdall(1))
        DO icol=gtdall(1),gtdall(2)
         rtype=Rgvrtp(icol)
         IF((rtype.eq.PRGTTD.or.rtype.eq.PRGTST.or.rtype.eq.PRRTTD.or.
     &       rtype.eq.PRRTST.or.rtype.eq.PRATTD.or.rtype.eq.PRATST.or.
     &       rtype.eq.PRG1TD.or.rtype.eq.PRR1TD.or.rtype.eq.PRA1TD.or.
     &       rtype.eq.PRG1ST.or.rtype.eq.PRR1ST.or.rtype.eq.PRA1ST).or.
     &      (rtype.eq.PRGTLM.or.rtype.eq.PRGTSL.or.rtype.eq.PRGTLQ.or.
     &       rtype.eq.PRGTLY.or.rtype.eq.PRRTLM.or.rtype.eq.PRRTSL.or.
     &       rtype.eq.PRRTLQ.or.rtype.eq.PRRTLY.or.rtype.eq.PRATLM.or.
     &       rtype.eq.PRATSL.or.rtype.eq.PRATLQ.or.rtype.eq.PRATLY).or.
     &      (rtype.eq.PRGUTD.or.rtype.eq.PRGULM.or.rtype.eq.PRGULQ.or.
     &       rtype.eq.PRGULY))THEN
          rgi2(icol)=Regidx(icol)
          IF(regidx(icol).eq.NOTSET)THEN
           df=df-1
          ELSE IF(baselt.eq.NOTSET)THEN
           baselt=rgi2(icol)
          END IF
         ELSE
          df=df-1
         END IF
         IF(rtype.eq.PRGTLY.or.rtype.eq.PRRTLY.or.rtype.eq.PRATLY.or.
     &      rtype.eq.PRGULY)THEN
          havlp=T
         ELSE IF(rtype.eq.PRGTLM.or.rtype.eq.PRGTSL.or.rtype.eq.PRGTLQ
     &       .or.rtype.eq.PRRTLM.or.rtype.eq.PRRTSL.or.rtype.eq.PRRTLQ
     &       .or.rtype.eq.PRATLM.or.rtype.eq.PRATSL.or.rtype.eq.PRATLQ
     &       .or.rtype.eq.PRGULM.or.rtype.eq.PRGULQ)
     &           THEN
          havlm=T
         END IF
        END DO
c        IF(.not.(havlp.or.havlm))THEN
c         IF(.not.lprthd)THEN
c          CALL writTag(Mt1,'</table></div>')
c          IF(Lsvlog)CALL writTag(Ng,'</table>')
c         END IF
c         RETURN
c        END IF
        CALL chitst(Xpxinv,gtdall(1),gtdall(2),chi2vl,pv,rgi2,F,info)
c-----------------------------------------------------------------------
c     Print out and/or save chi square statistic
c-----------------------------------------------------------------------
        nchr=21
        grpstr(1:nchr)='Combined Trading Day '
        IF(havlp)THEN
         grpstr(nchr+1:nchr+14)='and Leap Year '
         nchr=nchr+14
        ELSE IF(havlm)THEN
         IF(Sp.eq.4)THEN
          grpstr(nchr+1:nchr+22)='and Length of Quarter '
          nchr=nchr+22
         ELSE
          grpstr(nchr+1:nchr+20)='and Length of Month '
          nchr=nchr+20
         END IF
        END IF
        grpstr(nchr+1:nchr+10)='Regressors'
        nchr=nchr+10
        df1=df
c        df2=Nspobs-Mxdflg-df1
        Tdfval=(chi2vl/dble(df1))*(dble(df2)/dble(Nspobs-Mxdflg))
        Tdfpv=fvalue(Tdfval,df1,df2)
        CALL prtft(Lprtdt,lprthd,tbwdth,Lsvtdt,Lsvlog,baselt,grpstr,
     &             nchr,'Trading Day',11,info,df1,df2,Tdfval,Tdfpv,1071)
       END IF
       IF(.not.lprthd)THEN
        CALL writTag(Mt1,'</table></div>')
        IF(Lsvlog)THEN
         CALL writTag(Ng,'</table></div>')
         CALL mkPOneLine(Ng,'@','&nbsp;')
        END IF
       END IF
       RETURN
      END IF
      IF(gutd(0).gt.0.and.(gutd(2)-gutd(1)).gt.0)THEN
        CALL setint(NOTSET,Nb,rgi2)
        havlp=F
        havlm=F
        df=gutd(2)-gutd(1)+1
        baselt=regidx(gutd(1))
        DO icol=gutd(1),gutd(2)
         iusr=icol-ud1st+1
         IF(Lxreg)THEN
          utype=Usxtyp(iusr)
         ELSE
          utype=Usrtyp(iusr)
         END IF
         IF(utype.eq.PRGUTD.or.utype.eq.PRGULM.or.
     &      utype.eq.PRGULQ.or.utype.eq.PRGULY)THEN
          rgi2(icol)=Regidx(icol)
          IF(regidx(icol).eq.NOTSET)THEN
           df=df-1
          ELSE IF(baselt.eq.NOTSET)THEN
           baselt=rgi2(icol)
          END IF
         ELSE
          df=df-1
         END IF
         IF(utype.eq.PRGULY)THEN
          havlp=T
         ELSE IF(utype.eq.PRGULM.or.utype.eq.PRGULQ)
     &           THEN
          havlm=T
         END IF
        END DO
        CALL chitst(Xpxinv,gutd(1),gutd(2),chi2vl,pv,rgi2,gutd(0).lt.2,
     &              info)
c-----------------------------------------------------------------------
c     Print out and/or save chi square statistic
c-----------------------------------------------------------------------
        nchr=25
        grpstr(1:nchr)='User-defined Trading Day '
        IF(havlp)THEN
         grpstr(nchr+1:nchr+14)='and Leap Year '
         nchr=nchr+14
        ELSE IF(havlm)THEN
         IF(Sp.eq.4)THEN
          grpstr(nchr+1:nchr+22)='and Length of Quarter '
          nchr=nchr+22
         ELSE
          grpstr(nchr+1:nchr+20)='and Length of Month '
          nchr=nchr+20
         END IF
        END IF
        grpstr(nchr+1:nchr+10)='Regressors'
        nchr=nchr+10
        df1=df
c        df2=Nspobs-Mxdflg-df1
        Utdfvl=(chi2vl/dble(df1))*(dble(df2)/dble(Nspobs-Mxdflg))
        Utdfpv=fvalue(Utdfvl,df1,df2)
        CALL prtft(Lprtdt,lprthd,tbwdth,Lsvtdt,Lsvlog,baselt,grpstr,
     &             nchr,'Trading Day',11,info,df1,df2,Utdfvl,Utdfpv,
     &             1071)
      END IF 
c-----------------------------------------------------------------------
      IF(.not.lprthd)THEN
       CALL writTag(Mt1,'</table></div>')
       IF(Lsvlog)THEN
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
      
