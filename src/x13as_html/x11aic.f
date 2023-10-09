C     Last change:  BCM  20 May 1999    9:20 am
      SUBROUTINE x11aic(Irridx,Irrend,Muladd,Psuadd,Trnsrs,A,Nbeg,Sti,
     &                  Kswv,Priadj,Trumlt,Lprt,Lsavlg,Ldiag)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This routine performs two tests for the irregular regression.
c-----------------------------------------------------------------------
c     First, perform the test for trading day:
c     Estimate two regARIMA models - one with TD, one without.  The
c     routine will choose the model with the lowest value of AICC and
c     print out the resulting model.
c-----------------------------------------------------------------------
c     Then, perform the test for easter:
c     Estimate a number of regARIMA model, each with either no easter
c     effect or an easter effect with length 1, 8, or 15.  This routine
c     chooses the model with the lowest value of AICC and prints out the
c     resulting model.
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'xrgmdl.cmn'
      INCLUDE 'x11reg.cmn'
      INCLUDE 'x11log.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'usrxrg.cmn'
      INCLUDE 'xrgum.cmn'
      INCLUDE 'xclude.cmn'
      INCLUDE 'xtdtyp.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      LOGICAL T,F
      INTEGER PA
      PARAMETER(T=.true.,F=.false.,PA=PLEN+2*PORDER)
c-----------------------------------------------------------------------
      CHARACTER effttl*(PCOLCR),tdstr*(30),datstr*(10),fmtsvl*(21),
     &          tdabb*(80)
      LOGICAL Lprt,Psuadd,xm,Trumlt,tdhol,lhum2,estend,ladj,Lsavlg,fx2,
     &        Ldiag
      DOUBLE PRECISION A,aicbst,aichol,aicntd,aicnus,aictd,aicusr,bu2,
     &                 jadj,jadj2,Sti,Trnsrs,zero
      INTEGER i,icol,Muladd,Nbeg,nchr,frstry,Irridx,Kswv,ncx2,Priadj,
     &        ndifum,begcol,iuser,tdindx,rtype,ntdchr,nchdat,Irrend,
     &        ncol,igrp,typ2,ntdabb
      DIMENSION A(PA),Sti(PLEN),Trnsrs(PLEN),bu2(PUREG),zero(PLEN),
     &          fx2(PUREG),typ2(PUREG)
c-----------------------------------------------------------------------
      INTEGER strinx
      EXTERNAL strinx
c-----------------------------------------------------------------------
      aicind=-1
      estend=T
      IF(Xtdtst.gt.0)THEN
       IF(Tdgrp.gt.0)Tdgrp=0
       IF(Stdgrp.gt.0)Stdgrp=0
      END IF
      IF(Xeastr)THEN
       IF(Holgrp.gt.0)Holgrp=0
       IF(Easgrp.gt.0)Easgrp=0
      END IF
      IF(Xuser)THEN
       IF(Ncusrx.gt.0)THEN
        ncx2=Ncusrx
        Ncusrx=0
        lhum2=Haveum
        Haveum=F
       ELSE
        Xuser=F
       END IF
      END IF
      CALL setdp(0D0,PLEN,zero)
      iuser=0
c-----------------------------------------------------------------------
      tdindx=Xtdtst
      IF(tdindx.eq.4)tdindx=6
      IF(tdindx.eq.3)tdindx=4
      IF(tdindx.eq.2)tdindx=3
c-----------------------------------------------------------------------
c     If multiplicative or log-additive seasonal adjustment done,
c     get jacobean adjustment for AICC based on N(t)*
c-----------------------------------------------------------------------
      IF(Trumlt.or.Muladd.eq.2)THEN
       jadj=0D0
       DO i=Irridx,Irrend
        ladj=T
        IF(Nxcld.gt.0)ladj=.not.Rgxcld(i-Irridx+1)
        IF(ladj)jadj=jadj+log(Xnstar(i))
       END DO
c-----------------------------------------------------------------------
c     If log-additive seasonal adjustment done, get jacobean adjustment
c     for AICC based on Irr(t)
c-----------------------------------------------------------------------
       IF(Muladd.eq.2)THEN
        jadj2=0D0
        DO i=Irridx,Irrend
         ladj=T
         IF(Nxcld.gt.0)ladj=Rgxcld(i-Irridx+1)
         IF(ladj)jadj2=jadj2+Sti(i)
        END DO
       END IF
      END IF
c-----------------------------------------------------------------------
c     see if trading day and/or easter and/or user defined regressors
c     are in the regression matrix.  If so, remove them before
c     performing an aictest.
c-----------------------------------------------------------------------
      icol=Nb
      DO WHILE (icol.ge.1)
       rtype=Rgvrtp(icol)
       IF((Xtdtst.gt.0.AND.(rtype.eq.PRGTTD.or.rtype.eq.PRGTST.or.
     &     rtype.eq.PRRTTD.or.rtype.eq.PRRTST.or.rtype.eq.PRATTD.or.
     &     rtype.eq.PRATST.or.rtype.eq.PRG1TD.or.rtype.eq.PRR1TD.or.
     &     rtype.eq.PRA1TD.or.rtype.eq.PRG1ST.or.rtype.eq.PRR1ST.or.
     &     rtype.eq.PRA1ST.or.rtype.eq.PRGTLM.or.
     &     rtype.eq.PRGTSL.or.rtype.eq.PRGTLQ.or.rtype.eq.PRGTLY.or.
     &     rtype.eq.PRRTLM.or.rtype.eq.PRRTSL.or.rtype.eq.PRRTLQ.or.
     &     rtype.eq.PRRTLY.or.rtype.eq.PRATLM.or.rtype.eq.PRATSL.or.
     &     rtype.eq.PRATLQ.or.rtype.eq.PRATLY)).or.
     &   ((rtype.eq.PRGTEA.or.rtype.eq.PRGTEC).and.Xeastr).or.
     &   ((rtype.eq.PRGTUD.or.rtype.eq.PRGTUH.or.rtype.eq.PRGUAO.or.
     &    (rtype.eq.PRGUTD.or.rtype.eq.PRGULM.or.rtype.eq.PRGULQ.or.
     &     rtype.eq.PRGULY)).and.Xuser))THEN
        CALL dlrgef(icol,Nrxy,1)
        IF(Lfatal)RETURN
        IF(Xuser.and.
     &    (rtype.eq.PRGTUD.or.rtype.eq.PRGTUH.or.rtype.eq.PRGUAO.or.
     &    (rtype.eq.PRGUTD.or.rtype.eq.PRGULM.or.rtype.eq.PRGULQ.or.
     &     rtype.eq.PRGULY)))THEN
         iuser=iuser+1
         bu2(iuser)=B(icol)
         fx2(iuser)=Regfx(icol)
         typ2(iuser)=rtype
        END IF
       ELSE IF(Xeastr.and.(rtype.eq.PRGTLD.or.rtype.eq.PRGTTH))THEN
        Holgrp=icol
       END IF
       icol=icol-1
      END DO
c-----------------------------------------------------------------------
c     Start trading day AICC test by fitting the model without trading
c     day regressors
c-----------------------------------------------------------------------
      IF(Xtdtst.gt.0)THEN
c-----------------------------------------------------------------------
c     Generate string for label of trading day effect
c-----------------------------------------------------------------------
       CALL setchr(' ',30,tdstr)
       CALL setchr(' ',80,tdabb)
       CALL mktdlb(tdstr,ntdchr,tdabb,ntdabb,tdindx,Xaicst,Xaicrg,
     &             Xtdzro,Sp)
c-----------------------------------------------------------------------
c     transform the Irregular
c-----------------------------------------------------------------------
       IF((Muladd.eq.0.or.Muladd.eq.2).or.Haveum)THEN
        CALL copy(Sti(Irridx),Nobspf,-1,Trnsrs)
        ndifum=0
        IF(Haveum)CALL dfdate(Begspn,Begum,Sp,ndifum)
        IF((Muladd.eq.0.or.Muladd.eq.2).or.Haveum)
     &     CALL xrgtrn(Trnsrs,Irridx,Irrend,Psuadd,Muladd,Tdgrp,Haveum,
     &                 Umean,ndifum,Kswv)
       END IF
c-----------------------------------------------------------------------
c     Generate the new regression matrix and estimate the new model
c-----------------------------------------------------------------------
       CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,Nbcst,Userx,Bgusrx,Nrusrx,
     &             Priadj,Reglom,Nrxy,Begxy,frstry,T,Xelong)
       IF(.not.Lfatal)CALL regx11(A)
       IF(.not.Lfatal.and.Armaer.eq.PSNGER)CALL prterx()
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Get AICC(NO TD), print it out
c-----------------------------------------------------------------------
       CALL xrlkhd(aicntd,Nxcld)
       IF(Lfatal)RETURN
       IF(Holgrp.gt.0.and.Muladd.eq.2)aicntd=aicntd+2*jadj2
c-----------------------------------------------------------------------
c     Add trading day regressors to the model and reestimate the model
c-----------------------------------------------------------------------
       CALL addtd(Xaicst,Xaicrg,Xtdzro,Sp,tdindx)
       Tdgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Trading Day')
       IF(Tdgrp.eq.0.and.Xtdtst.eq.2)
     &    Stdgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Stock Trading Day')
       IF(Muladd.eq.1.and.Xtdtst.eq.1)
     &    CALL adrgef(DNOTST,'Leap Year','Leap Year',PRGTLY,F,F)
c-----------------------------------------------------------------------
c     transform the Irregular
c-----------------------------------------------------------------------
       IF((Muladd.eq.0.or.Muladd.eq.2).or.Haveum)THEN
        CALL copy(Sti(Irridx),Nobspf,-1,Trnsrs)
        ndifum=0
        IF(Haveum)CALL dfdate(Begspn,Begum,Sp,ndifum)
        IF((Muladd.eq.0.or.Muladd.eq.2).or.Haveum)
     &     CALL xrgtrn(Trnsrs,Irridx,Irrend,Psuadd,Muladd,Tdgrp,Haveum,
     &                 Umean,ndifum,Kswv)
       END IF
c-----------------------------------------------------------------------
c     Generate the new regression matrix and estimate the new model
c-----------------------------------------------------------------------
       CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,Nbcst,Userx,Bgusrx,Nrusrx,
     &             Priadj,Reglom,Nrxy,Begxy,frstry,T,Xelong)
c       IF((.not.Axruhl).and.Holgrp.gt.0)
c     &    CALL xrghol(Irridx,Psuadd,Xlpyr,Daybar)
       IF(.not.Lfatal)CALL regx11(A)
       IF(.not.Lfatal.and.Armaer.eq.PSNGER)CALL prterx()
       IF(.not.Lfatal)CALL rgtdhl(A,Nbeg)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Get AICC(TD), print it out
c-----------------------------------------------------------------------
       CALL xrlkhd(aictd,Nxcld)
       IF(Lfatal)RETURN
       IF(Trumlt.or.Muladd.eq.2)THEN
        aictd=aictd-2*jadj
        IF(Muladd.eq.2)aictd=aictd+2*jadj2
       END IF
c-----------------------------------------------------------------------
c     make choice, print it out
c-----------------------------------------------------------------------
       IF(Lprt)THEN
        CALL genSkip(1097)
        CALL mkPClass(Mt1,'indent')
        CALL writAbb(Mt1,'corrected Akaike information criterion',
     &                   'AICC')
        WRITE(Mt1,*)' for model without '
        CALL writAbb(Mt1,tdabb(1:ntdabb),tdstr(1:ntdchr))
        WRITE(Mt1,1010)aicntd,Cbr
        CALL writAbb(Mt1,'corrected Akaike information criterion',
     &                   'AICC')
        WRITE(Mt1,*)' for model with '
        CALL writAbb(Mt1,tdabb(1:ntdabb),tdstr(1:ntdchr))
        WRITE(Mt1,1010)aictd,'</p>'
       END IF
       IF(Lsavlg)THEN
        Inlgfl=Inlgfl+1
        WRITE(Ng,1000)Inlgfl
        CALL mkTableTag(Ng,'w60','@')
        CALL mkCaption(Ng,'<abbr title="A I C test">AICtest</abbr> '//
     &                 'for Irregular Regression (Trading Day)')
        CALL mkAicRowReal(Ng,'AICC','corrected Akaike information '//
     &                    'criterion',' (no ','td','trading day',
     &                    aicntd)
        CALL mkAicRowReal(Ng,'AICC','corrected Akaike information '//
     &                    'criterion',' (',tdstr(1:ntdchr),
     &                    tdabb(1:ntdabb),aictd)
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
       IF(Ldiag)THEN
        WRITE(Nform,1040)'notd',aicntd
        WRITE(Nform,1040)'td',aictd
        WRITE(Nform,1030)tdstr(1:ntdchr)
       END IF
       IF((aictd+Xraicd).lt.aicntd)THEN
        IF(Lprt)WRITE(Mt1,1050)
     &     Xraicd,'with <abbr title="'//tdabb(1:ntdabb)//'">'//
     &     tdstr(1:ntdchr)//'</abbr>'
        IF(.not.Axrgtd.and.Ixrgtd.gt.0)Axrgtd=T
        estend=F
        IF(Xeastr)THEN
         aichol=aictd
        ELSE IF(Xeastr)THEN
         aicnus=aictd
        END IF
       ELSE
        IF(Lprt)WRITE(Mt1,1050)Xraicd,'without <abbr title="'//
     &     tdabb(1:ntdabb)//'">'//tdstr(1:ntdchr)//'</abbr>'
        IF(Axrgtd)Axrgtd=F
        IF(Havxtd)Havxtd=F
        Tdgrp=0
        Stdgrp=0
c-----------------------------------------------------------------------
c     if AICC(NO TD) < AICC(TD), remove trading day regressors
c-----------------------------------------------------------------------
        icol=Nb
        DO WHILE (icol.ge.1)
         rtype=Rgvrtp(icol)
         IF(rtype.eq.PRGTTD.or.rtype.eq.PRGTST.or.
     &      rtype.eq.PRRTTD.or.rtype.eq.PRRTST.or.rtype.eq.PRATTD.or.
     &      rtype.eq.PRATST.or.rtype.eq.PRG1TD.or.rtype.eq.PRR1TD.or.
     &      rtype.eq.PRA1TD.or.rtype.eq.PRG1ST.or.rtype.eq.PRR1ST.or.
     &      rtype.eq.PRA1ST.or.rtype.eq.PRGTLM.or.
     &      rtype.eq.PRGTSL.or.rtype.eq.PRGTLQ.or.rtype.eq.PRGTLY.or.
     &      rtype.eq.PRRTLM.or.rtype.eq.PRRTSL.or.rtype.eq.PRRTLQ.or.
     &      rtype.eq.PRRTLY.or.rtype.eq.PRATLM.or.rtype.eq.PRATSL.or.
     &      rtype.eq.PRATLQ.or.rtype.eq.PRATLY)THEN
          CALL getstr(Colttl,Colptr,Ncoltl,icol,effttl,nchr)
          IF(.not.Lfatal)CALL dlrgef(icol,Nrxy,1)
          IF(Lfatal)RETURN
         END IF
         icol=icol-1
        END DO
c-----------------------------------------------------------------------
c     transform the Irregular
c-----------------------------------------------------------------------
        IF((Xeastr.or.Holgrp.gt.0).or.(Xuser.or.Ncusrx.gt.0))THEN
         IF((Muladd.eq.0.or.Muladd.eq.2).or.Haveum)THEN
          CALL copy(Sti(Irridx),Nobspf,-1,Trnsrs)
          ndifum=0
          IF(Haveum)CALL dfdate(Begspn,Begum,Sp,ndifum)
          IF((Muladd.eq.0.or.Muladd.eq.2).or.Haveum)
     &       CALL xrgtrn(Trnsrs,Irridx,Irrend,Psuadd,Muladd,Tdgrp,
     &                   Haveum,Umean,ndifum,Kswv)
         END IF
c-----------------------------------------------------------------------
c     Generate the new regression matrix
c-----------------------------------------------------------------------
         CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,Nbcst,Userx,Bgusrx,
     &               Nrusrx,Priadj,Reglom,Nrxy,Begxy,frstry,T,Xelong)
         IF(Lfatal)RETURN
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
c     Start loop through model choices for Easter
c-----------------------------------------------------------------------
      IF(Xeastr)THEN
       DO i=1,Neasvx
c-----------------------------------------------------------------------
c     If i > 2, locate and delete easter regressor from model
c-----------------------------------------------------------------------
        IF(i.gt.2)THEN
         Easgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Easter')
         IF(Easgrp.eq.0)Easgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &                                'StatCanEaster')
         begcol=Grp(easgrp-1)
         ncol=Grp(easgrp)-begcol
         CALL dlrgef(begcol,Nrxy,ncol)
         IF(Lfatal)RETURN
        END IF
c-----------------------------------------------------------------------
c     Add new easter regressor to model, if i > 1
c-----------------------------------------------------------------------
        IF(i.gt.1)THEN
         CALL addeas(Xeasvc(i)+Easidx,Easidx,1)
         IF(Lfatal)RETURN
         Easgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Easter')
         IF(Easgrp.eq.0)Easgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &                                'StatCanEaster')
         IF(Holgrp.eq.0)Holgrp=Easgrp
        END IF
c-----------------------------------------------------------------------
c     Generate regression matrix
c-----------------------------------------------------------------------
        IF(i.gt.1.or.estend)THEN
         tdhol=Holgrp.gt.0.and.Tdgrp.gt.0
         xm=(.not.(Trumlt.and.tdhol.and.Xhlnln)).and.Easidx.eq.0
         CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,Nbcst,Userx,Bgusrx,
     &               Nrusrx,Priadj,Reglom,Nrxy,Begxy,frstry,xm,Xelong)
         IF(Lfatal)RETURN
c         IF(Tdgrp.gt.0)CALL xrghol(Irridx,Psuadd,Xlpyr,Daybar)
c-----------------------------------------------------------------------
c     Estimate model
c-----------------------------------------------------------------------
         IF(.not.Lfatal)CALL regx11(A)
         IF(.not.Lfatal.and.Armaer.eq.PSNGER)CALL prterx()
         IF(.not.Lfatal)CALL rgtdhl(A,Nbeg)
         IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Compute the likelihood statistics and AICC for the model
c-----------------------------------------------------------------------
         CALL xrlkhd(aichol,Nxcld)
         IF(Tdgrp.gt.0.and.(Trumlt.or.Muladd.eq.2))THEN
          aichol=aichol-2*jadj
          IF(Muladd.eq.2)aichol=aichol+2*jadj2
         ELSE IF(Holgrp.gt.0.and.Muladd.eq.2)THEN
          aichol=aichol+2*jadj2
         END IF
        END IF
        IF(i.eq.1)THEN
         IF(Lprt)THEN
          CALL genSkip(1098)
          CALL mkPClass(Mt1,'indent')
          WRITE(Mt1,1060)aichol
         END IF
         IF(Lsavlg)THEN
          Inlgfl=Inlgfl+1
          WRITE(Ng,1000)Inlgfl
          CALL mkTableTag(Ng,'w60','@')
          CALL mkCaption(Ng,'<abbr title="A I C test">AICtest</abbr> '//
     &                   'for Irregular Regression (Easter)')
          WRITE(Ng,1020)'<abbr title="corrected Akaike information '//
     &                  'criterion">AICC</abbr>(no easter)',aichol
	     END IF
         IF(Ldiag)WRITE(Nform,1070)'noeaster',aichol
        ELSE
         IF(Lprt)THEN
          IF(Easidx.eq.0)THEN
           WRITE(Mt1,1080)Cbr,'Easter',Xeasvc(i),aichol
          ELSE
           WRITE(Mt1,1080)Cbr,'StatCanEaster',Xeasvc(i),aichol
          END IF
         END IF
         IF(Lsavlg)THEN
          IF(Easidx.eq.0)THEN
           WRITE(Ng,1090)'easter',Xeasvc(i),aichol
          ELSE
           WRITE(Ng,1090)'sceaster',Xeasvc(i),aichol
          END IF
         END IF
         IF(Ldiag)THEN
          IF(Easidx.eq.0)THEN
           WRITE(Nform,1100)'easter',Xeasvc(i),aichol
          ELSE
           WRITE(Nform,1100)'sceaster',Xeasvc(i),aichol
          END IF
         END IF
        END IF
c-----------------------------------------------------------------------
c     See if this AICC is the smallest.  If so, update value and index
c     of best AICC.
c-----------------------------------------------------------------------
        IF(i.eq.1)THEN
         aicbst=aichol
         aicind=Xeasvc(1)
        ELSE IF((aicind.eq.0.AND.(aichol+Xraicd).lt.aicbst).or.
     &         (aicind.gt.0.and.aichol.lt.aicbst))THEN
         aicbst=aichol
         aicind=Xeasvc(i)
        END IF
       END DO
c-----------------------------------------------------------------------
       IF(Lsavlg)THEN
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
c-----------------------------------------------------------------------
c     Show Easter effect that aic prefers
c-----------------------------------------------------------------------
       IF(Lprt)THEN
        CALL writTag(Mt1,'</p>')
        IF(aicind.eq.0)THEN
         WRITE(Mt1,1110)Xraicd
         IF(Axrghl)Axrghl=F
         IF(Havxhl)Havxhl=F
         IF(Finhol)Finhol=T
        ELSE
         IF(Easidx.eq.0)THEN
          WRITE(Mt1,1120)Xraicd,'Easter',aicind
         ELSE
          WRITE(Mt1,1120)Xraicd,'Statistics Canada Easter',aicind
         END IF
         IF(.not.Axrghl.and.Ixrghl.gt.0)Axrghl=T
        END IF
       END IF
c-----------------------------------------------------------------------
c     If model with best AICC wasn't the last one estimated, generate
c     regression matrix for the best model.
c-----------------------------------------------------------------------
       estend=F
       IF(aicind.lt.Xeasvc(Neasvx))THEN
        estend=T
        easgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Easter')
        IF(easgrp.eq.0)easgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &                               'StatCanEaster')
        begcol=Grp(easgrp-1)
        ncol=Grp(easgrp)-begcol
        CALL dlrgef(begcol,Nrxy,ncol)
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     IF no Easter effect, reset indicator variables and exit routine
c     if there are no other holiday or td regressors in model.
c-----------------------------------------------------------------------
        IF(aicind.eq.0)THEN
         Easgrp=0
         Holgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Thanksgiving')
         IF(Holgrp.eq.0)Holgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Labor')
c-----------------------------------------------------------------------
c     Add new Easter variable, if necessary
c-----------------------------------------------------------------------
        ELSE IF(aicind.gt.0)THEN
         CALL addeas(aicind+Easidx,Easidx,1)
         IF(Lfatal)RETURN
        END IF
        tdhol=Holgrp.gt.0.and.Tdgrp.gt.0
        xm=Easidx.eq.0.and.(.not.(Trumlt.and.tdhol.and.Xhlnln))
        CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,Nbcst,Userx,Bgusrx,
     &              Nrusrx,Priadj,Reglom,Nrxy,Begxy,frstry,xm,Xelong)
        IF(Lfatal)RETURN
c        IF((.not.Axruhl).and.Holgrp.gt.0.and.Tdgrp.gt.0)
c     &     CALL xrghol(Irridx,Psuadd,Xlpyr,Daybar)
       END IF
       IF(.not.estend.and.Xuser)aicnus=aichol
      END IF
c-----------------------------------------------------------------------
c     Finally, test user defined AICC
c-----------------------------------------------------------------------
      IF(Xuser)THEN
       IF(estend)THEN
        IF(.not.Lfatal)CALL regx11(A)
        IF(.not.Lfatal.and.Armaer.eq.PSNGER)CALL prterx()
        IF(.not.Lfatal)CALL rgtdhl(A,Nbeg)
c-----------------------------------------------------------------------
c     Compute the likelihood statistics and AICC for the model
c-----------------------------------------------------------------------
        CALL xrlkhd(aicnus,Nxcld)
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
        IF(Tdgrp.gt.0.and.(Trumlt.or.Muladd.eq.2))THEN
         aicnus=aicnus-2*jadj
         IF(Muladd.eq.2)aicnus=aicnus+2*jadj2
        ELSE IF(Holgrp.gt.0.and.Muladd.eq.2)THEN
         aicnus=aicnus+2*jadj2
        END IF
       END IF
c-----------------------------------------------------------------------
       IF(Lprt)THEN
        CALL genSkip(1099)
        CALL mkPClass(Mt1,'indent')
        WRITE(Mt1,1130)aicnus,Cbr
       END IF
       IF(Lsavlg)THEN
        Inlgfl=Inlgfl+1
        WRITE(Ng,1000)Inlgfl
        CALL mkTableTag(Ng,'w60','@')
        CALL mkCaption(Ng,'<abbr title="A I C test">AICtest</abbr> '//
     &                   'for Irregular Regression (User Defined)')
        WRITE(Ng,1020)'<abbr title="corrected Akaike information '//
     &                  'criterion">AICC</abbr>(no user defined)',aicnus
       END IF
       IF(Ldiag)WRITE(Nform,1140)'nouser',aicnus
c-----------------------------------------------------------------------
c     Add user defined regressors back into model
c-----------------------------------------------------------------------
       Ncusrx=ncx2
       Haveum=lhum2
c-----------------------------------------------------------------------
c     Restore user-defined regressors to the regression matrix
c-----------------------------------------------------------------------
       DO i=1,Ncusrx
        CALL getstr(Usrttl,Usrptr,Ncusrx,i,effttl,nchr)
        IF(Lfatal)RETURN
        IF(typ2(i).eq.PRGTUD)THEN
         CALL adrgef(bu2(i),effttl(1:nchr),'User-defined',PRGTUD,
     &               fx2(i),F)
        ELSE IF(typ2(i).eq.PRGUTD)THEN
         CALL adrgef(bu2(i),effttl(1:nchr),'User-defined Trading Day',
     &               PRGUTD,fx2(i),F)
        ELSE IF(typ2(i).eq.PRGULY)THEN
         CALL adrgef(bu2(i),effttl(1:nchr),'User-defined Leap Year',
     &               PRGULY,fx2(i),F)
        ELSE IF(typ2(i).eq.PRGULM)THEN
         CALL adrgef(bu2(i),effttl(1:nchr),'User-defined LOM',
     &               PRGULM,fx2(i),F)
        ELSE IF(typ2(i).eq.PRGULQ)THEN
         CALL adrgef(bu2(i),effttl(1:nchr),'User-defined LOQ',
     &               PRGULQ,fx2(i),F)
        ELSE IF(typ2(i).eq.PRGUAO)THEN
         CALL adrgef(bu2(i),effttl(1:nchr),'User-defined AO',
     &               PRGUAO,fx2(i),F)
        ELSE IF(typ2(i).eq.PRGTUH)THEN
         CALL adrgef(bu2(i),effttl(1:nchr),'User-defined Holiday',
     &               PRGTUH,fx2(i),F)
        END IF
       END DO
c-----------------------------------------------------------------------
c     Restore transformed data, if necessary
c-----------------------------------------------------------------------
       IF(Haveum)THEN
        CALL copy(Sti(Irridx),Nobspf,-1,Trnsrs)
        ndifum=0
        CALL dfdate(Begspn,Begum,Sp,ndifum)
        CALL xrgtrn(trnsrs,Irridx,Irrend,Psuadd,Muladd,Tdgrp,Haveum,
     &              Umean,ndifum,Kswv)
       END IF
c-----------------------------------------------------------------------
c     Re-generate regression matrix and estaimate model
c-----------------------------------------------------------------------
       CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,Nbcst,Userx,Bgusrx,Nrusrx,
     &             Priadj,Reglom,Nrxy,Begxy,frstry,T,Xelong)
       IF(.not.Lfatal)CALL regx11(A)
       IF(.not.Lfatal.and.Armaer.eq.PSNGER)CALL prterx()
       IF(.not.Lfatal)CALL rgtdhl(A,Nbeg)
c-----------------------------------------------------------------------
c     Compute the likelihood statistics and AICC for the model
c-----------------------------------------------------------------------
       IF(.not.Lfatal)CALL xrlkhd(aicusr,Nxcld)
       IF(Lfatal)RETURN
       IF(Tdgrp.gt.0.and.(Trumlt.or.Muladd.eq.2))THEN
        aicusr=aicusr-2*jadj
        IF(Muladd.eq.2)aicusr=aicusr+2*jadj2
       ELSE IF(Holgrp.gt.0.and.Muladd.eq.2)THEN
        aicusr=aicusr+2*jadj2
       END IF
       IF(Lprt)WRITE(Mt1,1150)aicusr
       IF(Lsavlg)THEN
        WRITE(Ng,1020)'<abbr title="corrected Akaike information '//
     &                  'criterion">AICC</abbr> (user defined)',aicusr
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
       IF(Ldiag)WRITE(Nform,1140)'user',aicusr
c-----------------------------------------------------------------------
c     Print out result of test
c-----------------------------------------------------------------------
       IF((aicusr+Xraicd).lt.aicnus)THEN
        estend=F
        IF(Lprt)WRITE(Mt1,1050)Xraicd,'with user-defined regressor(s)'
       ELSE
        estend=T
        WRITE(Mt1,1050)Xraicd,'without user-defined regressor(s)'
c-----------------------------------------------------------------------
c     Remove user defined regressors from regression matrix and
c     retransform series, if necessary.
c-----------------------------------------------------------------------
        IF(Haveum)THEN
         Haveum=F
         CALL copy(Sti(Irridx),Nobspf,-1,Trnsrs)
         IF(Muladd.eq.0.or.Muladd.eq.2)
     &     CALL xrgtrn(trnsrs,Irridx,Irrend,Psuadd,Muladd,Tdgrp,Haveum,
     &                 Umean,ndifum,Kswv)
        END IF
        igrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'User-defined')
        begcol=Grp(igrp-1)
        ncol=Grp(igrp)-begcol
        CALL dlrgef(begcol,Nrxy,ncol)
        IF(Lfatal)RETURN
        Ncusrx=0
        Ncxusx=0
        Nrxusx=0
c-----------------------------------------------------------------------
c     Regenerate regression matrix
c-----------------------------------------------------------------------
        CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,Nbcst,Userx,Bgusrx,
     &              Nrusrx,Priadj,Reglom,Nrxy,Begxy,frstry,T,Xelong)
        IF(Lfatal)RETURN
       END IF
      END IF
c-----------------------------------------------------------------------
c     Estimate model
c-----------------------------------------------------------------------
      IF(estend)THEN
       CALL regx11(A)
       IF(.not.Lfatal.and.Armaer.eq.PSNGER)CALL prterx()
       IF(.not.Lfatal)CALL rgtdhl(A,Nbeg)
      END IF
c-----------------------------------------------------------------------
      RETURN
c-----------------------------------------------------------------------
 1000 FORMAT('<div id="lgxaic',i6.6,'">')
 1010 FORMAT(' : ',f20.4,a)
 1020 FORMAT('<tr><th>',a,'</th><td>',f20.4,'</td></tr>')
 1030 FORMAT('aictest.xtd.reg: ',a)
 1040 FORMAT('aictest.xtd.aicc.',a,': ',e29.15)
 1050 FORMAT('<p class="center"> ***** <abbr title="corrected ',
     &       'Akaike information criterion">AICC</abbr>',/,
     &       ' (with aicdiff=',F5.2,') prefers model <strong>',a,
     &       '</strong> ***** </p>')
 1060 FORMAT(' <abbr title="corrected Akaike information ',
     &       'criterion">AICC</abbr> for model without Easter : ',f20.4)
 1070 FORMAT('aictest.xe.aicc.',a,': ',e29.15)
 1080 FORMAT(a,' <abbr title="corrected Akaike information ',
     &       'criterion">AICC</abbr> for model with ',a,'[',i2,'] : ',
     &       f20.4)
 1090 FORMAT('<tr><th><abbr title="corrected Akaike information ',
     &       'criterion">AICC</abbr>(',a,'[',i2,'])</th><td>',f20.4,
     &       '</td></tr>')
 1100 FORMAT('aictest.xe.aicc.',a,i2.2,': ',e29.15)
 1110 FORMAT(/,'<p class="center">***** <abbr title="corrected ',
     &         'Akaike information criterion">AICC</abbr>',
     &       /,' (with aicdiff=',F5.2,') prefers model without Easter',
     &         ' *****</p>')
 1120 FORMAT(/,'<p class="center">***** <abbr title="corrected ',
     &         'Akaike information criterion">AICC</abbr>',
     &       /,' (with aicdiff=',F5.2,') prefers model with ',a,
     &         '[',i2,'] *****</p>')
 1130  FORMAT('<abbr title="corrected Akaike information criterion">',
     &       'AICC</abbr> for model without user-defined regressors : ',
     &        f20.4,a)
 1140  FORMAT('aictest.xu.aicc.',a,': ',e29.15)
 1150  FORMAT(' <abbr title="corrected Akaike information criterion">',
     &        'AICC</abbr> for model with user-defined regressor(s) : ',
     &        f20.4,'</p>')
c-----------------------------------------------------------------------
      END
