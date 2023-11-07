      SUBROUTINE cmpchi(Xpxinv,Regidx,Lsvchi,Lsvlch,Lprchi,Lprhdr,
     &                  Tbwdth,Lxreg)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     perform chi square tests for combinations of regression groups,
c     such as change of regime regressors and length of month with
c     trading day
c-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'usrxrg.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
c     replace dimension length for Xpxinv (BCM May 2007)
      CHARACTER grpstr*(PGRPCR),rg0str*(PGRPCR),rg1str*(PGRPCR),
     &          rg2str*(PGRPCR),ctype*(31)
      DOUBLE PRECISION chi2vl,pv,Xpxinv
      LOGICAL allusr,havlp,havlm,Lsvchi,Lprchi,Lprhdr,Lxreg,Lsvlch
      INTEGER begcol,endcol,gtdall,gtdrg,gtdrg1,gtdrg2,gsearg,Regidx,
     &        info,igrp,ipos,nchr,nchr0,nchr1,nchr2,df,baselt,rtype,
     &        rgi2,icol,iusr,gutd,guhol,gusea,utype,udrest,Tbwdth,ud1st,
     &        udlast,guall,nuh1,nuh2,nuh3,nuh4,nuh5
      DIMENSION gtdall(0:2),gtdrg(0:2),gtdrg1(0:2),gtdrg2(0:2),
     &          gsearg(0:2),rgi2(PB),Regidx(PB),Xpxinv(PXPX),gutd(0:2),
     &          guhol(0:2),gusea(0:2),guall(0:2)
c-----------------------------------------------------------------------
c     Initialize counts for the pointer dictionaries
c-----------------------------------------------------------------------
      DO icol=0,2
       gtdall(icol)=0
       gtdrg(icol)=0
       gtdrg1(icol)=0
       gtdrg2(icol)=0
       gsearg(icol)=0
       gutd(icol)=0
       guhol(icol)=0
       gusea(icol)=0
       guall(icol)=0
      END DO
      udrest=0
      ud1st=NOTSET
      udlast=NOTSET
      iusr=1
c-----------------------------------------------------------------------
c     Create pointer dictionaries for different tests we wish to
c     perform
c-----------------------------------------------------------------------
      DO igrp=1,Ngrp
       begcol=Grp(igrp-1)
       endcol=Grp(igrp)-1
       rtype=Rgvrtp(begcol)
c-----------------------------------------------------------------------
       IF(rtype.eq.PRGTUD.or.rtype.eq.PRGTUS.or.rtype.eq.PRGTUH.or.
     &    rtype.eq.PRGUH2.or.rtype.eq.PRGUH3.or.rtype.eq.PRGUH4.or.
     &    rtype.eq.PRGUH5.or.rtype.eq.PRGUAO.or.rtype.eq.PRGULS.or.
     &    rtype.eq.PRGUSO.or.rtype.eq.PRGUCN.or.rtype.eq.PRGUCY.or.
     &    rtype.eq.PRGUTD.or.rtype.eq.PRGULM.or.rtype.eq.PRGULQ.or.
     &    rtype.eq.PRGULY)THEN
        IF(ud1st.eq.NOTSET)ud1st=begcol
        IF(guall(0).eq.0)guall(1)=begcol
        guall(0)=guall(0)+1
        guall(2)=endcol
        IF(Lxreg)THEN
         utype=Usxtyp(begcol)
        ELSE
         utype=Usrtyp(begcol)
        END IF
        IF(utype.eq.PRGUTD.or.utype.eq.PRGULM.or.utype.eq.PRGULQ.or.
     &     utype.eq.PRGULY)THEN
          IF(gtdall(0).eq.0)gtdall(1)=begcol
          gtdall(0)=gtdall(0)+1
          gtdall(2)=endcol
          IF(utype.eq.PRGUTD)THEN
           IF(gutd(0).eq.0)gutd(1)=begcol
           gutd(0)=gutd(0)+1
           gutd(2)=endcol
          END IF
         ELSE IF(utype.eq.PRGTUS)THEN
          IF(gusea(0).eq.0)gusea(1)=begcol
          gusea(0)=gusea(0)+1
          gusea(2)=endcol
         ELSE IF((utype.ge.PRGTUH.and.utype.le.PRGUH5))THEN
          IF(guhol(0).eq.0)THEN
           guhol(1)=begcol
           guhol(0)=guhol(0)+1
          ELSE
           IF(.not.(utype.eq.udlast))guhol(0)=guhol(0)+1
          END IF
          guhol(2)=endcol
         ELSE
          udrest=udrest+1
         END IF
         iusr=iusr+1
         IF(icol.lt.endcol)udlast=utype
       END IF
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
       IF((rtype.eq.PRGTTD.or.rtype.eq.PRGTST.or.rtype.eq.PRG1TD).or.
     &    (rtype.eq.PRGTLM.or.rtype.eq.PRGTSL.or.rtype.eq.PRGTLQ.or.
     &     rtype.eq.PRGTLY))THEN
        gtdrg(0)=gtdrg(0)+1
        IF(gtdrg(0).eq.1)gtdrg(1)=begcol
        gtdrg(2)=endcol
        CALL getstr(Grpttl,Grpptr,Ngrp,igrp,rg0str,nchr0)
        IF(Lfatal)RETURN
       END IF
       IF((rtype.eq.PRRTTD.or.rtype.eq.PRRTST.or.rtype.eq.PRR1TD).or.
     &    (rtype.eq.PRRTLM.or.rtype.eq.PRRTSL.or.rtype.eq.PRRTLQ.or.
     &     rtype.eq.PRRTLY))THEN
        gtdrg1(0)=gtdrg1(0)+1
        IF(gtdrg1(0).eq.1)gtdrg1(1)=begcol
        gtdrg1(2)=endcol
        CALL getstr(Grpttl,Grpptr,Ngrp,igrp,rg1str,nchr1)
        IF(Lfatal)RETURN
       END IF
       IF((rtype.eq.PRATTD.or.rtype.eq.PRATST.or.rtype.eq.PRA1TD).or.
     &    (rtype.eq.PRATLM.or.rtype.eq.PRATSL.or.rtype.eq.PRATLQ.or.
     &     rtype.eq.PRATLY))THEN
        gtdrg2(0)=gtdrg2(0)+1
        IF(gtdrg2(0).eq.1)gtdrg2(1)=begcol
        gtdrg2(2)=endcol
        CALL getstr(Grpttl,Grpptr,Ngrp,igrp,rg2str,nchr2)
        IF(Lfatal)RETURN
       END IF
       IF(rtype.eq.PRRTTS.or.rtype.eq.PRRTSE.or.rtype.eq.PRATTS.or.
     &    rtype.eq.PRATSE.or.rtype.eq.PRGTTS.or.rtype.eq.PRGTSE)THEN
        gsearg(0)=gsearg(0)+1
        IF(gsearg(0).eq.1)gsearg(1)=begcol
        gsearg(2)=endcol
       END IF
      END DO
c-----------------------------------------------------------------------
c     Generate combined Chi-Square test for user defined holiday
c     regressors.
c-----------------------------------------------------------------------
      IF((guhol(2)-guhol(1)).gt.0.and.(guhol(0).lt.guall(0)))THEN
       CALL setint(NOTSET,Nb,rgi2)
       df=guhol(2)-guhol(1)+1
       baselt=regidx(guhol(1))
       nuh1=0
       nuh2=0
       nuh3=0
       nuh4=0
       nuh5=0
       DO icol=guhol(1),guhol(2)
        iusr=icol-ud1st+1
        IF(Lxreg)THEN
         utype=Usxtyp(iusr)
        ELSE
         utype=Usrtyp(iusr)
        END IF
        IF((utype.ge.PRGTUH.and.utype.le.PRGUH5))THEN
         rgi2(icol)=Regidx(icol)
         IF(regidx(icol).eq.NOTSET)THEN
          df=df-1
         ELSE IF(baselt.eq.NOTSET)THEN
          baselt=rgi2(icol)
         END IF
         IF(utype.eq.PRGTUH.and.nuh1.eq.0)nuh1=nuh1+1
         IF(utype.eq.PRGUH2.and.nuh2.eq.0)nuh2=nuh2+1
         IF(utype.eq.PRGUH3.and.nuh3.eq.0)nuh3=nuh3+1
         IF(utype.eq.PRGUH4.and.nuh4.eq.0)nuh4=nuh4+1
         IF(utype.eq.PRGUH5.and.nuh5.eq.0)nuh5=nuh5+1
        ELSE
         df=df-1
        END IF
       END DO
       IF((nuh1+nuh2+nuh3+nuh4+nuh5).gt.1)THEN
        CALL chitst(Xpxinv,guhol(1),guhol(2),chi2vl,pv,rgi2,
     &              guhol(0).lt.2,info)
c-----------------------------------------------------------------------
c     Print out and/or save chi square statistic
c-----------------------------------------------------------------------
        nchr=35
        grpstr(1:nchr)='All User-defined Holiday Regressors'
        CALL setchr(' ',31,ctype)
        ctype(1:1)=CNOTST
        CALL savchi(Lsvchi,F,Lprhdr,Tbwdth,baselt,grpstr,nchr,info,df,
     &              chi2vl,pv,ctype,0,'chi$')
        IF(Lprchi)THEN
         ctype(1:10)='Regressors'
         CALL prtchi(Mt1,Lprhdr,Tbwdth,baselt,grpstr,nchr,info,df,
     &               chi2vl,pv,ctype,10,F)
         IF(Lprhdr)Lprhdr=F
        END IF
       END IF
      END IF
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
       if (ipos.gt.0) then
        grpstr(nchr+1:(nchr+nchr0-ipos+1))=rg0str(ipos:nchr0)
        nchr=nchr+nchr0-ipos+1
       endif
       CALL setchr(' ',31,ctype)
       ctype(1:1)=CNOTST
       CALL savchi(Lsvchi,F,Lprhdr,Tbwdth,baselt,grpstr,nchr,info,df,
     &             chi2vl,pv,ctype,0,'chi$')
       IF(Lprchi)THEN
        ctype(1:10)='Regressors'
        CALL prtchi(Mt1,Lprhdr,Tbwdth,baselt,grpstr,nchr,info,df,chi2vl,
     &              pv,ctype,10,F)
        IF(Lprhdr)Lprhdr=F
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
       if (ipos.gt.0) then
        grpstr(nchr+1:(nchr+nchr1-ipos+1))=rg1str(ipos:nchr1)
        nchr=nchr+nchr1-ipos+1
       end if
       CALL setchr(' ',31,ctype)
       ctype(1:1)=CNOTST
       CALL savchi(Lsvchi,F,Lprhdr,Tbwdth,baselt,grpstr,nchr,info,df,
     &             chi2vl,pv,ctype,0,'chi$')
       IF(Lprchi)THEN
        ctype(1:10)='Regressors'
        CALL prtchi(Mt1,Lprhdr,Tbwdth,baselt,grpstr,nchr,info,df,chi2vl,
     &              pv,ctype,10,F)
        IF(Lprhdr)Lprhdr=F
       END IF
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
       if (ipos.gt.0) then
        grpstr(nchr+1:(nchr+nchr2-ipos+1))=rg2str(ipos:nchr2)
        nchr=nchr+nchr2-ipos+1
       end if
       CALL setchr(' ',31,ctype)
       ctype(1:1)=CNOTST
       CALL savchi(Lsvchi,F,Lprhdr,Tbwdth,baselt,grpstr,nchr,info,df,
     &             chi2vl,pv,ctype,0,'chi$')
       IF(Lprchi)THEN
        ctype(1:10)='Regressors'
        CALL prtchi(Mt1,Lprhdr,Tbwdth,baselt,grpstr,nchr,info,df,chi2vl,
     &              pv,ctype,10,F)
        IF(Lprhdr)Lprhdr=F
       END IF
      END IF
c-----------------------------------------------------------------------
c     Generate combined Chi-Square test for user defined trading day
c     regressors.
c-----------------------------------------------------------------------
      IF(ud1st.eq.NOTSET)THEN
       IF((gutd(2)-gutd(1)).gt.0)THEN
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
         IF(utype.eq.PRGUTD)THEN
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
        CALL chitst(Xpxinv,gutd(1),gutd(2),chi2vl,pv,rgi2,gutd(0).lt.2,
     &              info)
c-----------------------------------------------------------------------
c     Print out and/or save chi square statistic
c-----------------------------------------------------------------------
        nchr=25
        grpstr(1:nchr)='User-defined Trading Day '
        grpstr(nchr+1:nchr+10)='Regressors'
        nchr=nchr+10
        CALL setchr(' ',31,ctype)
        ctype(1:1)=CNOTST
        CALL savchi(Lsvchi,F,Lprhdr,Tbwdth,baselt,grpstr,nchr,info,df,
     &              chi2vl,pv,ctype,0,'chi$')
        IF(Lprchi)THEN
         ctype(1:10)='Regressors'
         CALL prtchi(Mt1,Lprhdr,Tbwdth,baselt,grpstr,nchr,info,df,
     &               chi2vl,pv,ctype,10,F)
         IF(Lprhdr)Lprhdr=F
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
c     Generate combined Chi-Square test for trading day and lom
c     regressors
c-----------------------------------------------------------------------
      IF(gtdall(0).ge.2)THEN
       CALL setint(NOTSET,Nb,rgi2)
       havlp=F
       havlm=F
       allusr=T
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
     &      rtype.eq.PRATSL.or.rtype.eq.PRATLQ.or.rtype.eq.PRATLY.or.
     &      rtype.eq.PRGUTD.or.rtype.eq.PRGULM.or.rtype.eq.PRGULQ.or.
     &      rtype.eq.PRGULY))THEN
         rgi2(icol)=Regidx(icol)
         IF(regidx(icol).eq.NOTSET)THEN
          df=df-1
         ELSE IF(baselt.eq.NOTSET)THEN
          baselt=rgi2(icol)
         END IF
         IF(.not.(rtype.eq.PRGUTD.or.rtype.eq.PRGULM.or.rtype.eq.PRGULQ
     &        .or.rtype.eq.PRGULY))allusr=F
        ELSE
         df=df-1
        END IF
        IF(rtype.eq.PRGTLY.or.rtype.eq.PRRTLY.or.rtype.eq.PRATLY.or.
     &     rtype.eq.PRGULY)THEN
         havlp=T
        ELSE IF(rtype.eq.PRGTLM.or.rtype.eq.PRGTSL.or.rtype.eq.PRGTLQ
     &      .or.rtype.eq.PRRTLM.or.rtype.eq.PRRTSL.or.rtype.eq.PRRTLQ
     &      .or.rtype.eq.PRATLM.or.rtype.eq.PRATSL.or.rtype.eq.PRATLQ
     &      .or.rtype.eq.PRGULM.or.rtype.eq.PRGULQ)
     &          THEN
         havlm=T
        END IF
       END DO
       CALL chitst(Xpxinv,gtdall(1),gtdall(2),chi2vl,pv,rgi2,F,info)
c-----------------------------------------------------------------------
c     Print out and/or save chi square statistic
c-----------------------------------------------------------------------
       nchr=9
       grpstr(1:nchr)='Combined '
       IF(allusr)THEN
        grpstr(nchr+1:nchr+14)='User-Defined '
        nchr=nchr+14
       END IF
       grpstr(nchr+1:nchr+12)='Trading Day '
       nchr=nchr+12
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
       CALL setchr(' ',31,ctype)
       ctype(1:1)=CNOTST
       CALL savchi(Lsvchi,F,Lprhdr,Tbwdth,baselt,grpstr,nchr,info,df,
     &             chi2vl,pv,ctype,0,'chi$')
       IF(Lprchi)THEN
        ctype(1:10)='Regressors'
        CALL prtchi(Mt1,Lprhdr,Tbwdth,baselt,grpstr,nchr,info,df,chi2vl,
     &              pv,ctype,10,F)
        IF(Lprhdr)Lprhdr=F
       END IF
      END IF
c-----------------------------------------------------------------------
c     Generate combined Chi-Square test for seasonal regressors
c-----------------------------------------------------------------------
      IF(gsearg(0).ge.2)THEN
       CALL setint(NOTSET,Nb,rgi2)
       havlp=F
       havlm=F
       df=gsearg(2)-gsearg(1)+1
       baselt=regidx(gsearg(1))
       DO icol=gsearg(1),gsearg(2)
        rtype=Rgvrtp(icol)
        IF(rtype.eq.PRRTTS.or.rtype.eq.PRRTSE.or.rtype.eq.PRATTS.or.
     &     rtype.eq.PRATSE.or.rtype.eq.PRGTTS.or.rtype.eq.PRGTSE)THEN
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
       CALL chitst(Xpxinv,gsearg(1),gsearg(2),chi2vl,pv,rgi2,F,info)
c-----------------------------------------------------------------------
c     Print out and/or save chi square statistic
c-----------------------------------------------------------------------
       nchr=9
       grpstr(1:nchr)='Combined '
       rtype=Rgvrtp(gsearg(1))
       IF(rtype.eq.PRRTTS.or.rtype.eq.PRATTS)THEN
        grpstr(nchr+1:nchr+14)='Trigonometric '
        nchr=nchr+14
       END IF
       grpstr(nchr+1:nchr+19)='Seasonal Regressors'
       nchr=nchr+19
       CALL setchr(' ',31,ctype)
       ctype(1:1)=CNOTST
       CALL savchi(Lsvchi,F,Lprhdr,Tbwdth,baselt,grpstr,nchr,info,df,
     &             chi2vl,pv,ctype,0,'chi$')
       IF(Lprchi)THEN
        ctype(1:10)='Regressors'
        CALL prtchi(Mt1,Lprhdr,Tbwdth,baselt,grpstr,nchr,info,df,chi2vl,
     &              pv,ctype,10,F)
        IF(Lprhdr)Lprhdr=F
       END IF
      END IF
c-----------------------------------------------------------------------
c     Generate combined Chi-Square test for user defined seasonal
c     regressors.
c-----------------------------------------------------------------------
c      IF((gusea(2)-gusea(1)).gt.0)THEN
c       CALL setint(NOTSET,Nb,rgi2)
c       df=gusea(2)-gusea(1)+1
c       baselt=regidx(gusea(1))
c       DO icol=gusea(1),gusea(2)
c        iusr=icol-ud1st+1
c        IF(Lxreg)THEN
c         utype=Usxtyp(iusr)
c        ELSE
c         utype=Usrtyp(iusr)
c        END IF
c        IF(utype.eq.PRGTUS)THEN
c         rgi2(icol)=Regidx(icol)
c         IF(regidx(icol).eq.NOTSET)THEN
c          df=df-1
c         ELSE IF(baselt.eq.NOTSET)THEN
c          baselt=rgi2(icol)
c         END IF
c        ELSE
c         df=df-1
c        END IF
c       END DO
c       CALL chitst(Xpxinv,gusea(1),gusea(2),chi2vl,pv,rgi2,
c     &             gusea(0).lt.2,info)
c-----------------------------------------------------------------------
c     Print out and/or save chi square statistic
c-----------------------------------------------------------------------
c       nchr=32
c       grpstr(1:nchr)='User-defined Seasonal Regressors'
c       CALL setchr(' ',31,ctype)
c       ctype(1:1)=CNOTST
c       CALL savchi(Lsvchi,F,Lprhdr,Tbwdth,baselt,grpstr,nchr,info,df,
c     &             chi2vl,pv,ctype,0,'chi$')
c       IF(Lprchi)THEN
c        ctype(1:10)='Regressors'
c        CALL prtchi(Mt1,Lprhdr,Tbwdth,baselt,grpstr,nchr,info,df,chi2vl,
c     &              pv,ctype,10)
c        IF(Lprhdr)Lprhdr=F
c       END IF
c      END IF
c-----------------------------------------------------------------------
c     Generate combined Chi-Square test for user defined regressors
c     if there are more than one type of user defined regressor defined
c     or if there are no special types of user defined regressors
c     defined.
c-----------------------------------------------------------------------
      IF((guall(0).eq.1.and.((guall(2)-guall(1)+1).gt.udrest)).or.
     &    guall(0).gt.1)THEN
       CALL setint(NOTSET,Nb,rgi2)
       df=guall(2)-guall(1)+1
       baselt=regidx(guall(1))
       DO icol=guall(1),guall(2)
        rgi2(icol)=Regidx(icol)
        IF(regidx(icol).eq.NOTSET)THEN
         df=df-1
        ELSE IF(baselt.eq.NOTSET)THEN
         baselt=rgi2(icol)
        END IF
       END DO
       CALL chitst(Xpxinv,guall(1),guall(2),chi2vl,pv,rgi2,
     &             guall(0).lt.2,info)
c-----------------------------------------------------------------------
c     Print out and/or save chi square statistic
c-----------------------------------------------------------------------
       IF(guall(0).eq.1)THEN
        nchr=23
        grpstr(1:nchr)='User-defined Regressors'
       ELSE
        nchr=27
        grpstr(1:nchr)='All User-defined Regressors'
       END IF
       CALL setchr(' ',31,ctype)
       ctype(1:1)=CNOTST
       CALL savchi(Lsvchi,F,Lprhdr,Tbwdth,baselt,grpstr,nchr,info,df,
     &             chi2vl,pv,ctype,0,'chi$')
       IF(Lprchi)THEN
        ctype(1:10)='Regressors'
        CALL prtchi(Mt1,Lprhdr,Tbwdth,baselt,grpstr,nchr,info,df,chi2vl,
     &              pv,ctype,10,F)
        IF(Lprhdr)Lprhdr=F
       END IF
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
