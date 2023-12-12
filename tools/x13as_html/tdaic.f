C     Last change:  BCM   7 Sep 2005    3:30 pm
      SUBROUTINE tdaic(Trnsrs,A,Nefobs,Na,Frstry,Lester,Tdmdl1,Ltdlom,
     &                 Lprtit,Lprt,Lprtfm,Lsavlg,Lsumm,Lhiddn)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Estimate two or three regARIMA models: one with TD, one without, 
c     and one coefficient TD (in default mode).  
c     The routine will choose the model with the lowest value of AICC
c     and print out the resulting model.
c-----------------------------------------------------------------------
      LOGICAL F,T
      INTEGER DIV,MULT,PLOM,PLOQ
      DOUBLE PRECISION ZERO
      PARAMETER(DIV=4,MULT=3,PLOM=2,PLOQ=3,F=.false.,T=.true.,ZERO=0D0)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'inpt.cmn'
      INCLUDE 'lkhd.cmn'
      INCLUDE 'adj.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'priadj.cmn'
      INCLUDE 'priusr.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'notset.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      INTEGER PA
      DOUBLE PRECISION ONE
      PARAMETER(PA=PLEN+2*PORDER,ONE=1D0)
c-----------------------------------------------------------------------
      CHARACTER tdstr*(30),rgstr*(30),tdabb*(80),rgabb*(80)
      LOGICAL pktd,Lprt,lhide,Lprtit,Lester,lom,argok,Lprtfm,Lsavlg,
     &        begrgm,Lhiddn,Ltdlom,lpv
      DOUBLE PRECISION A,a2,aic1,aic2,aicntd,lomeff,Trnsrs,tsrs,
     &                 aicbst,aicno,aictd,thiscv
c      DOUBLE PRECISION tap(PARIMA),tb(PB),tap0(PARIMA),tb0(PB)
      INTEGER Frstry,Tdmdl1,kf2,Na,Nefobs,begcol,ncol,ilom,lencol,
     &        irgfx,igrp,endlag,ilag,ntdchr,nrgchr,Lsumm,thisTD,i,tdgrp,
     &        aicdf,nbtd,nbno,ntdabb,nrgabb
      DIMENSION A(PA),lomeff(PLEN),Trnsrs(PLEN),tsrs(PLEN),a2(PLEN),
     &          begrgm(PLEN)
c-----------------------------------------------------------------------
      INTEGER strinx
      LOGICAL dpeq
      EXTERNAL dpeq,strinx
c-----------------------------------------------------------------------
c     Store initial model values
c-----------------------------------------------------------------------
      CALL setchr(' ',30,tdstr)
      CALL setchr(' ',80,tdabb)
      IF(.not.Lprt)THEN
       lhide=Lhiddn
       Lhiddn=T
      END IF
      CALL setdp(ONE,PLEN,lomeff)
      CALL copy(Adj,PLEN,1,a2)
      irgfx=Iregfx
      pktd=Picktd
      kf2=Kfmt
      ilom=Priadj
      aictd=DNOTST
      IF(Lprt)CALL genSkip(1017)
c-----------------------------------------------------------------------
c     Make copy of transformed data
c-----------------------------------------------------------------------
      CALL copy(Trnsrs,PLEN,1,tsrs)
c-----------------------------------------------------------------------
c     Set indicator variable to determine if td regression is in the
c     regression variables.
c-----------------------------------------------------------------------
      Tdmdl1=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Trading Day')
      IF(Tdmdl1.eq.0)
     &   Tdmdl1=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Stock Trading Day')
      IF(Tdmdl1.eq.0)
     &   Tdmdl1=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &                 '1-Coefficient Trading Day')
      IF(Tdmdl1.eq.0)
     &   Tdmdl1=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &                 '1-Coefficient Stock Trading Day')
c-----------------------------------------------------------------------
c     Generate string for label of trading day effect
c-----------------------------------------------------------------------
      CALL mktdlb(rgstr,nrgchr,rgabb,nrgabb,Tdayvc(Ntdvec),Aicstk,
     &            Tddate,Tdzero,Sp)
      IF(.not.Lfatal)
     &   CALL mktdlb(tdstr,ntdchr,tdabb,ntdabb,Itdtst,Aicstk,Tddate,
     &               Tdzero,Sp)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
      IF(Lsumm.gt.0)THEN
       WRITE(Nform,1020)'aictest.td.num:',Ntdvec-1
       WRITE(Nform,1030)'aictest.td.reg:',tdstr(1:ntdchr)
       IF(Ntdvec.eq.3)
     &    WRITE(Nform,1030)'aictest.td.reg2:',rgstr(1:nrgchr)
      END IF
c-----------------------------------------------------------------------
c     Check for length of month or leapyear regressors.
c-----------------------------------------------------------------------
      lencol=strinx(T,Colttl,Colptr,1,Ncoltl,'Length-of-')
      IF(lencol.eq.0)lencol=strinx(T,Colttl,Colptr,1,Ncoltl,
     &                             'Stock Length-of-')
      IF(lencol.eq.0)lencol=strinx(F,Colttl,Colptr,1,Ncoltl,'Leap Year')
c-----------------------------------------------------------------------
c     Generate length of month effect for possible later use.
c-----------------------------------------------------------------------
      IF(Priadj.eq.PLOM.or.Priadj.eq.PLOQ)THEN
       lom=T
      ELSE
       lom=F
      END IF
      IF(Lrgmtd.and.MOD(Tdzero,2).ne.0)THEN
       CALL gtrgpt(Begadj,Tddate,Tdzero,begrgm,Nadj)
      ELSE
       CALL setlg(T,PLEN,begrgm)
      END IF
      CALL td7var(Begadj,Sp,Nadj,1,1,lom,F,T,lomeff,begrgm)
c-----------------------------------------------------------------------
c     Start loop through model choices
c-----------------------------------------------------------------------
      DO i=1,Ntdvec
       thisTD=Tdayvc(i)
c-----------------------------------------------------------------------
c     See if there is a trading day effect in the model
c-----------------------------------------------------------------------
       tdgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Trading Day')
       IF(tdgrp.eq.0)
     &    tdgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Stock Trading Day')
       IF(tdgrp.eq.0)
     &    tdgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &                 '1-Coefficient Trading Day')
       IF(tdgrp.eq.0)
     &    tdgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &                 '1-Coefficient Stock Trading Day')
c-----------------------------------------------------------------------
c     If trading day regressor in model, delete regressor from model
c-----------------------------------------------------------------------
       IF(tdgrp.gt.0)THEN
        DO igrp=Ngrp,1,-1
         begcol=Grp(igrp-1)
         ncol=Grp(igrp)-begcol
         IF(Rgvrtp(begcol).eq.PRGTST.or.Rgvrtp(begcol).eq.PRGTTD.or.
     &      Rgvrtp(begcol).eq.PRRTST.or.Rgvrtp(begcol).eq.PRRTTD.or.
     &      Rgvrtp(begcol).eq.PRATST.or.Rgvrtp(begcol).eq.PRATTD.or.
     &     (Lomtst.eq.0.and.(Rgvrtp(begcol).eq.PRGTLM.or.
     &      Rgvrtp(begcol).eq.PRGTLQ.or.Rgvrtp(begcol).eq.PRGTLY.or.
     &      Rgvrtp(begcol).eq.PRGTSL.or.Rgvrtp(begcol).eq.PRRTLM.or.
     &      Rgvrtp(begcol).eq.PRRTLQ.or.Rgvrtp(begcol).eq.PRRTLY.or.
     &      Rgvrtp(begcol).eq.PRRTSL.or.Rgvrtp(begcol).eq.PRATLM.or.
     &      Rgvrtp(begcol).eq.PRATLQ.or.Rgvrtp(begcol).eq.PRATLY)).or.
     &      Rgvrtp(begcol).eq.PRATSL.or.
     &      Rgvrtp(begcol).eq.PRG1TD.or.Rgvrtp(begcol).eq.PRR1TD.or.
     &      Rgvrtp(begcol).eq.PRA1TD.or.Rgvrtp(begcol).eq.PRG1ST.or.
     &      Rgvrtp(begcol).eq.PRR1ST.or.Rgvrtp(begcol).eq.PRA1ST)THEN
          CALL dlrgef(begcol,Nrxy,ncol)
          IF(Lfatal)RETURN
         END IF
        END DO
       END IF       
c-----------------------------------------------------------------------
       IF(i.eq.1)THEN
c-----------------------------------------------------------------------
c     if Picktd, put length of month back in series.
c-----------------------------------------------------------------------
        IF(pktd)THEN
         Picktd=F
         Priadj=1
c ***  Change Nspobs to Nadj and Adj1st to 1 until END OF CHANGE
c ***  Add if block so this is only done if log transform done
c ***  JAN 2000 BCM
         IF(dpeq(Lam,0D0))THEN
          IF(Nustad.gt.0)THEN
           CALL eltfcn(DIV,Y(Frstsy),Usrtad(Frstat),Nobspf,PLEN,Trnsrs)
           CALL copy(Usrtad(Frstat),Nadj,1,Adj(1))
          ELSE
           CALL copy(Y(Frstsy),Nobspf,-1,Trnsrs)
          END IF
          IF(Nuspad.gt.0)THEN
           CALL eltfcn(DIV,Y(Frstsy),Usrpad(Frstap),Nobspf,PLEN,Trnsrs)
           IF(Nustad.gt.0)THEN
            CALL eltfcn(MULT,Adj(1),Usrpad(Frstap),Nadj,PLEN,Adj(1))
           ELSE
            CALL copy(Usrpad(Frstap),Nadj,1,Adj(1))
           END IF
          ELSE
           CALL setdp(1D0,PLEN,Adj)
          END IF
c ***  END OF CHANGE (BCM, JAN 1997)
          IF(Lmvaft.or.Ln0aft)THEN
           CALL trnfcn(trnsrs,Nspobs,Fcntyp,Lam,trnsrs)
          ELSE
           CALL trnfcn(trnsrs,Nobspf,Fcntyp,Lam,trnsrs)
          END IF
          IF(Lfatal)RETURN
         END IF
        END IF
c-----------------------------------------------------------------------
c    Else, preadjust series if necessary
c-----------------------------------------------------------------------
       ELSE IF(i.eq.2)THEN
        IF(Tdmdl1.eq.0)THEN
c-----------------------------------------------------------------------
c     If the td or td1coef options are selected, set Picktd to true and
c     add either a leap year regressor or leap year preadjustment
c     factors, depending on the data transformation.
c-----------------------------------------------------------------------
         IF(thisTD.eq.1.or.thisTD.eq.4)THEN
          Picktd=T
          IF(Fcntyp.eq.4.OR.dpeq(Lam,1D0))THEN
           IF(Ltdlom)THEN
            IF(Sp.eq.12)THEN
             CALL adrgef(DNOTST,'Length-of-Month','Length-of-Month',
     &                   PRGTLM,F,F)
            ELSE IF(Sp.eq.4)THEN
             CALL adrgef(DNOTST,'Length-of-Quarter','Length-of-Quarter',
     &                   PRGTLQ,F,F)
            END IF
           ELSE
            CALL adrgef(DNOTST,'Leap Year','Leap Year',PRGTLY,F,F)
           END IF
           IF(Lfatal)RETURN
          ELSE
           IF(Ltdlom)THEN
            IF(Sp.eq.12)THEN
             Priadj=PLOM
            ELSE IF(Sp.eq.4)THEN
             Priadj=PLOQ
            END IF
           ELSE
            Priadj=4
           END IF
          END IF
         END IF
         Iregfx=0
*         CALL addtd(Aicstk,Tddate,Tdzero,Sp,Itdtst)
c-----------------------------------------------------------------------
c     Length of Month adjust original series, if necessary.
c-----------------------------------------------------------------------
         IF((ilom.le.1.and.Picktd).AND.(.not.(Fcntyp.eq.4.OR.
     &       dpeq(Lam,1D0))))THEN
c-----------------------------------------------------------------------
c     Generate length of month (or length of quarter) variables.
c-----------------------------------------------------------------------
          IF(kf2.eq.0)THEN
           CALL eltfcn(DIV,Y(Frstsy),lomeff(Adj1st),Nobspf,PLEN,Trnsrs)
           IF(Lmvaft.or.Ln0aft)THEN
            CALL trnfcn(trnsrs,Nspobs,Fcntyp,Lam,trnsrs)
           ELSE
            CALL trnfcn(trnsrs,Nobspf,Fcntyp,Lam,trnsrs)
           END IF
           IF(Lfatal)RETURN
           CALL copy(lomeff,PLEN,1,Adj)
           Kfmt=1
          ELSE
           CALL eltfcn(DIV,Y(Frstsy),lomeff(Adj1st),Nobspf,PLEN,Trnsrs)
           CALL eltfcn(DIV,Trnsrs,Adj(Adj1st),Nobspf,PLEN,Trnsrs)
           IF(Lmvaft.or.Ln0aft)THEN
            CALL trnfcn(trnsrs,Nspobs,Fcntyp,Lam,trnsrs)
           ELSE
            CALL trnfcn(trnsrs,Nobspf,Fcntyp,Lam,trnsrs)
           END IF
           IF(Lfatal)RETURN
           CALL eltfcn(MULT,lomeff(Adj1st),Adj(Adj1st),Nobspf,PLEN,
     &                 Adj(Adj1st))
          END IF
         END IF
c-----------------------------------------------------------------------
c     If not, restore variables from original model
c-----------------------------------------------------------------------
        ELSE
         CALL copy(a2,PLEN,1,Adj)
         Picktd=pktd
         Kfmt=kf2
         Priadj=ilom
c-----------------------------------------------------------------------
c     Copy over transformed data
c-----------------------------------------------------------------------
         CALL copy(tsrs,PLEN,1,Trnsrs)
c-----------------------------------------------------------------------
c     Add LOM or LPY regressor, if necessary.
c-----------------------------------------------------------------------
         IF((thisTD.eq.1.or.thisTD.eq.4).and.Picktd)THEN
          IF(Fcntyp.eq.4.OR.dpeq(Lam,1D0))THEN
           IF(Ltdlom)THEN
            IF(Sp.eq.12)THEN
             CALL adrgef(DNOTST,'Length-of-Month','Length-of-Month',
     &                   PRGTLM,F,F)
            ELSE IF(Sp.eq.4)THEN
             CALL adrgef(DNOTST,'Length-of-Quarter','Length-of-Quarter',
     &                   PRGTLQ,F,F)
            END IF
           ELSE
            CALL adrgef(DNOTST,'Leap Year','Leap Year',PRGTLY,F,F)
           END IF
           IF(Lfatal)RETURN
          END IF
         END IF
        END IF
       ELSE IF(i.eq.3)THEN
        IF(thisTD.eq.4)THEN
         IF(Fcntyp.eq.4.OR.dpeq(Lam,1D0))THEN
          IF(Ltdlom)THEN
           IF(Sp.eq.12)THEN
            CALL adrgef(DNOTST,'Length-of-Month','Length-of-Month',
     &                  PRGTLM,F,F)
           ELSE IF(Sp.eq.4)THEN
            CALL adrgef(DNOTST,'Length-of-Quarter','Length-of-Quarter',
     &                  PRGTLQ,F,F)
           END IF
          ELSE
           CALL adrgef(DNOTST,'Leap Year','Leap Year',PRGTLY,F,F)
          END IF
          IF(Lfatal)RETURN
         END IF
        END IF
       END IF
c-----------------------------------------------------------------------
c     If i > 1, add new trading day regressor to model
c-----------------------------------------------------------------------
       IF(i.gt.1.or.tdgrp.gt.0)THEN
        IF(i.gt.1)THEN
         CALL mktdlb(tdstr,ntdchr,tdabb,ntdabb,thisTD,Aicstk,Tddate,
     &               Tdzero,Sp)
         IF(.not.Lfatal)CALL addtd(Aicstk,Tddate,Tdzero,Sp,thisTD)
        END IF
        IF(.not.Lfatal)
     &     CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,
     &                 Nrusrx,Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
        IF(Lfatal)RETURN
       END IF
c-----------------------------------------------------------------------
c    If there are ARMA parameters that were set as initial values by
c    the user, reset Arimap to those values (BCM, 9-2010)
c-----------------------------------------------------------------------
       IF(Nopr.gt.0)THEN
        endlag=Opr(Nopr)-1
        DO ilag=1,endlag
         IF(.not.Arimaf(ilag))Arimap(ilag)=Ap1(ilag)
        END DO
       END IF
c-----------------------------------------------------------------------
c     Estimate the model
c-----------------------------------------------------------------------
       argok=Lautom.or.Lautox
       CALL rgarma(T,Mxiter,Mxnlit,F,A,Na,Nefobs,argok)
c      CALL rgarma(T,Mxiter,Mxnlit,T,A,Na,Nefobs,argok)
       IF((.not.Lfatal).and.(Lautom.or.Lautox).and.(.not.argok))Lester=T
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     If an estimation error is found, discontinue the routine.
c-----------------------------------------------------------------------
       IF(Armaer.eq.PMXIER.or.Armaer.eq.PSNGER.or.Armaer.eq.PISNER.or.
     &    Armaer.eq.PNIFER.or.Armaer.eq.PNIMER.or.Armaer.eq.PCNTER.or.
     &    Armaer.eq.POBFN0.or.Armaer.lt.0.or.
     &    ((Lautom.or.Lautox).and..not.argok))THEN
        Lester=T
        RETURN
c-----------------------------------------------------------------------
c     If only a warning message would be printed out, reset the error
c     indicator variable to zero.
c-----------------------------------------------------------------------
       ELSE IF(Armaer.ne.0)THEN
        Armaer=0
       END IF
c-----------------------------------------------------------------------
c     Compute the likelihood statistics and AICC for the model
c-----------------------------------------------------------------------
       IF(Lprt)THEN
        CALL writTag(Mt1,'<h3>')
        IF(i.eq.1)THEN
         WRITE(Mt1,*)'  Likelihood statistics for model without '
        ELSE
         WRITE(Mt1,*)'  Likelihood statistics for model with '
        END IF
        CALL writAbb(Mt1,tdabb(1:ntdabb),tdstr(1:ntdchr))
        CALL writTag(Mt1,'</h3>')
       END IF
       IF(i.eq.Ntdvec)THEN
        CALL prlkhd(Y(Frstsy),Adj(Adj1st),Adjmod,Fcntyp,Lam,F,Lprt,
     &                Lprtfm)
       ELSE
        CALL prlkhd(Y(Frstsy),Adj(Adj1st),Adjmod,Fcntyp,Lam,F,Lprt,F)
       END IF
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Start testing AICC.  First save the AICC for no TD.
c-----------------------------------------------------------------------
       IF(i.eq.1)THEN
        aicno=Aicc
        nbno=Nb
        IF(Lsavlg)THEN
         Inlgfl=Inlgfl+1
         WRITE(Ng,1000)Inlgfl
         CALL mkTableTag(Ng,'w60','@')
         CALL mkCaption(Ng,
     &        '<abbr title="A I C test">AICtest</abbr> for Trading Day')
         CALL mkAicRowReal(Ng,'AICC','corrected Akaike '//
     &                     'information criterion',' (no ',
     &                     tdstr(1:ntdchr),tdabb(1:ntdabb),Aicc)
        END IF
        IF(Lsumm.gt.0)WRITE(Nform,1050)'notd',Aicc
c-----------------------------------------------------------------------
c     Next find best AICC for models with TD.
c-----------------------------------------------------------------------
       ELSE
        IF(Lsavlg)THEN
         CALL mkAicRowReal(Ng,'AICC','corrected Akaike '//
     &                     'information criterion',' (',
     &                     tdstr(1:ntdchr),tdabb(1:ntdabb),Aicc)
        END IF
        IF(Lsumm.gt.0)WRITE(Nform,1050)tdstr(1:ntdchr),Aicc
c-----------------------------------------------------------------------
c     If second pass, save td indicator, AICC, Nb
c-----------------------------------------------------------------------
        IF(i.eq.2)THEN
         Aicint=thisTD
         aictd=Aicc
         IF(.not.dpeq(Pvaic,DNOTST))nbtd=Nb
c-----------------------------------------------------------------------
c     If third pass, determine "critical value" by chi square,
c     if specified
c-----------------------------------------------------------------------
        ELSE
         IF(.not.dpeq(Pvaic,DNOTST))THEN
          aicdf=nbtd-Nb
          CALL chsppf(Pvaic,aicdf,thiscv,Mt1)
          Rgaicd(PTDAIC)=thiscv-2D0*DBLE(aicdf)
         END IF
c-----------------------------------------------------------------------
c     Determine which TD model is best
c-----------------------------------------------------------------------
         Dfaict=Aicc-aictd
         IF(.not.(Dfaict.gt.Rgaicd(PTDAIC)))THEN
          Aicint=thisTD
          aictd=Aicc
          IF(.not.dpeq(Pvaic,DNOTST))nbtd=Nb
         END IF
        END IF
       END IF
      END DO
c-----------------------------------------------------------------------
      IF(Lsavlg)THEN
       CALL writTag(Ng,'</table></div>')
       CALL mkPOneLine(Ng,'@','&nbsp;')
      END IF      
c-----------------------------------------------------------------------
c     Determine best model between TD, no TD
c-----------------------------------------------------------------------
      Dfaict=aicno-aictd
      IF(.not.dpeq(Pvaic,DNOTST))THEN
       aicdf=nbtd-nbno
       CALL chsppf(Pvaic,aicdf,thiscv,Mt1)
       Rgaicd(PTDAIC)=thiscv-2D0*dble(aicdf)
      END IF
      IF(Dfaict.gt.Rgaicd(PTDAIC))THEN
       aicbst=aictd
      ELSE
       aicbst=Aicno
       Aicint=0
      END IF
c-----------------------------------------------------------------------
      IF(.not.Lprt)Lhiddn=lhide
c-----------------------------------------------------------------------
c     Show Trading Day effect that aic prefers
c-----------------------------------------------------------------------
      IF(Lprt)THEN
       IF(Aicint.eq.0)THEN
        CALL mktdlb(tdstr,ntdchr,tdabb,ntdabb,Tdayvc(2),Aicstk,Tddate,
     &              Tdzero,Sp)
        IF(Lfatal)RETURN
        IF(dpeq(Pvaic,DNOTST))THEN
         WRITE(Mt1,1060)Rgaicd(PTDAIC),'without'
        ELSE
         WRITE(Mt1,1070)ONE-Pvaic,Rgaicd(PTDAIC),'without'
        END IF         
       ELSE
        IF(Aicint.ne.Tdayvc(Ntdvec))THEN
         CALL mktdlb(tdstr,ntdchr,tdabb,ntdabb,Aicint,Aicstk,Tddate,
     &              Tdzero,Sp)
         IF(Lfatal)RETURN
        END IF
        IF(dpeq(Pvaic,DNOTST))THEN
         WRITE(Mt1,1060)Rgaicd(PTDAIC),'with'
        ELSE
         WRITE(Mt1,1070)ONE-Pvaic,Rgaicd(PTDAIC),'with'
        END IF
       END IF
       CALL writAbb(Mt1,tdabb(1:ntdabb),tdstr(1:ntdchr))
       CALL writTag(Mt1,'</strong> *****</p>')
      END IF
c-----------------------------------------------------------------------
c    If no trading day selected, set up new model and data
c-----------------------------------------------------------------------
      IF(Aicint.eq.0)THEN
c-----------------------------------------------------------------------
c     if Picktd, put length of month back in series.
c-----------------------------------------------------------------------
       IF(pktd)THEN
        Picktd=F
        Priadj=1
c ***  Change Nspobs to Nadj and Adj1st to 1 until END OF CHANGE
c ***  Add if block so this is only done if log transform done
c ***  JAN 2000 BCM
        IF(dpeq(Lam,0D0))THEN
         IF(Nustad.gt.0)THEN
          CALL eltfcn(DIV,Y(Frstsy),Usrtad(Frstat),Nobspf,PLEN,Trnsrs)
          CALL copy(Usrtad(Frstat),Nadj,1,Adj(1))
         ELSE
          CALL copy(Y(Frstsy),Nobspf,-1,Trnsrs)
         END IF
         IF(Nuspad.gt.0)THEN
          CALL eltfcn(DIV,Y(Frstsy),Usrpad(Frstap),Nobspf,PLEN,Trnsrs)
          IF(Nustad.gt.0)THEN
           CALL eltfcn(MULT,Adj(1),Usrpad(Frstap),Nadj,PLEN,Adj(1))
          ELSE
           CALL copy(Usrpad(Frstap),Nadj,1,Adj(1))
          END IF
         ELSE
          CALL setdp(1D0,PLEN,Adj)
         END IF
c ***  END OF CHANGE (BCM, JAN 1997)
         IF(Lmvaft.or.Ln0aft)THEN
          CALL trnfcn(trnsrs,Nspobs,Fcntyp,Lam,trnsrs)
         ELSE
          CALL trnfcn(trnsrs,Nobspf,Fcntyp,Lam,trnsrs)
         END IF
         IF(Lfatal)RETURN
        END IF
       ELSE
        CALL copy(a2,PLEN,1,Adj)
        Kfmt=kf2
        Picktd=pktd
        Iregfx=irgfx
        IF(ilom.le.1)THEN
         Priadj=ilom
         CALL copy(tsrs,PLEN,1,Trnsrs)
        END IF
       END IF
c-----------------------------------------------------------------------
c     removed trading day variables
c-----------------------------------------------------------------------
       DO igrp=Ngrp,1,-1
        begcol=Grp(igrp-1)
        ncol=Grp(igrp)-begcol
        IF(Rgvrtp(begcol).eq.PRGTST.or.Rgvrtp(begcol).eq.PRGTTD.or.
     &     Rgvrtp(begcol).eq.PRRTST.or.Rgvrtp(begcol).eq.PRRTTD.or.
     &     Rgvrtp(begcol).eq.PRATST.or.Rgvrtp(begcol).eq.PRATTD.or.
     &    (Lomtst.eq.0.and.(Rgvrtp(begcol).eq.PRGTLM.or.
     &     Rgvrtp(begcol).eq.PRGTLQ.or.Rgvrtp(begcol).eq.PRGTLY.or.
     &     Rgvrtp(begcol).eq.PRGTSL.or.Rgvrtp(begcol).eq.PRRTLM.or.
     &     Rgvrtp(begcol).eq.PRRTLQ.or.Rgvrtp(begcol).eq.PRRTLY.or.
     &     Rgvrtp(begcol).eq.PRRTSL.or.Rgvrtp(begcol).eq.PRATLM.or.
     &     Rgvrtp(begcol).eq.PRATLQ.or.Rgvrtp(begcol).eq.PRATLY)).or.
     &     Rgvrtp(begcol).eq.PRATSL.or.
     &     Rgvrtp(begcol).eq.PRG1TD.or.Rgvrtp(begcol).eq.PRR1TD.or.
     &     Rgvrtp(begcol).eq.PRA1TD.or.Rgvrtp(begcol).eq.PRG1ST.or.
     &     Rgvrtp(begcol).eq.PRR1ST.or.Rgvrtp(begcol).eq.PRA1ST)THEN
         CALL dlrgef(begcol,Nrxy,ncol)
         IF(Lfatal)RETURN
        END IF
       END DO
c-----------------------------------------------------------------------
c    If there are ARMA parameters that were set as initial values by
c    the user, reset Arimap to those values (BCM, 9-2010)
c-----------------------------------------------------------------------
       IF(Nopr.gt.0)THEN
        endlag=Opr(Nopr)-1
        DO ilag=1,endlag
         IF(.not.Arimaf(ilag))Arimap(ilag)=Ap1(ilag)
        END DO
       END IF
c-----------------------------------------------------------------------
c     Estimate the model
c-----------------------------------------------------------------------
       CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &             Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
       CALL rgarma(T,Mxiter,Mxnlit,Lprtit,A,Na,Nefobs,argok)
       IF((.not.Lfatal).and.(Lautom.or.Lautox).and.(.not.argok))
     &     CALL abend()
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Remove trading day
c-----------------------------------------------------------------------
      ELSE IF(Aicint.ne.Tdayvc(Ntdvec))THEN
       DO igrp=Ngrp,1,-1
        begcol=Grp(igrp-1)
        ncol=Grp(igrp)-begcol
        IF(Rgvrtp(begcol).eq.PRGTST.or.Rgvrtp(begcol).eq.PRGTTD.or.
     &     Rgvrtp(begcol).eq.PRRTST.or.Rgvrtp(begcol).eq.PRRTTD.or.
     &     Rgvrtp(begcol).eq.PRATST.or.Rgvrtp(begcol).eq.PRATTD.or.
     &     Rgvrtp(begcol).eq.PRATSL.or.
     &     Rgvrtp(begcol).eq.PRG1TD.or.Rgvrtp(begcol).eq.PRR1TD.or.
     &     Rgvrtp(begcol).eq.PRA1TD.or.Rgvrtp(begcol).eq.PRG1ST.or.
     &     Rgvrtp(begcol).eq.PRR1ST.or.Rgvrtp(begcol).eq.PRA1ST)THEN
         CALL dlrgef(begcol,Nrxy,ncol)
         IF(Lfatal)RETURN
        END IF
       END DO
       CALL addtd(Aicstk,Tddate,Tdzero,Sp,Aicint)
c-----------------------------------------------------------------------
c    If there are ARMA parameters that were set as initial values by
c    the user, reset Arimap to those values (BCM, 9-2010)
c-----------------------------------------------------------------------
       IF(Nopr.gt.0)THEN
        endlag=Opr(Nopr)-1
        DO ilag=1,endlag
         IF(.not.Arimaf(ilag))Arimap(ilag)=Ap1(ilag)
        END DO
       END IF
c-----------------------------------------------------------------------
       CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &             Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
       CALL rgarma(T,Mxiter,Mxnlit,Lprtit,A,Na,Nefobs,argok)
       IF((.not.Lfatal).and.(Lautom.or.Lautox).and.(.not.argok))
     &     CALL abend()
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
      IF(.not.Lpradj.and.Kfmt.eq.1)Lpradj=.true.
      IF(Aicint.eq.0)THEN
       IF(pktd.and.(.not.Picktd))THEN
        CALL copy(Adj,Nadj,-1,Sprior(Setpri))
        IF((Nustad.eq.0.or.Nuspad.eq.0).and.Kfmt.gt.0)Kfmt=0
       END IF
       IF(Tdmdl1.gt.0)Tdmdl1=1
      ELSE
       IF((.not.pktd).and.Picktd)THEN
        CALL copy(Adj,Nadj,-1,Sprior(Setpri))
        IF(Kfmt.eq.0)Kfmt=1
        IF(Nuspad.eq.0.or.Npser.eq.0)THEN
         Prmser(1:3)='LPY'
         Npser=3
        END IF
       END IF
       IF(Ntdvec.eq.2)THEN
        Tdmdl1=0
       ELSE IF(Aicint.eq.Tdayvc(Ntdvec))THEN
        Tdmdl1=2
       ELSE
        Tdmdl1=0
       END IF
      END IF
      RETURN
c-----------------------------------------------------------------------
 1000 FORMAT('<div id="lgaic',i6.6,'">')
 1010 FORMAT('<td>',f15.4,'</td>')
 1020 FORMAT(a,1x,i6)
 1030 FORMAT(a,1x,a)
 1050 FORMAT('aictest.td.aicc.',a,': ',e29.15)
 1060 FORMAT(/,'<p class="center">***** <abbr title="corrected ',
     &         'Akaike information criterion">AICC</abbr>',
     &       /,' (with <abbr title="A I C diff">aicdiff</abbr> = ',
     &         F7.4,') prefers model <strong>',a)
 1070 FORMAT(/,'<p class="center">***** <abbr title="corrected ',
     &         'Akaike information criterion">AICC</abbr>',
     &       /,' (with p-value = ',F7.5,
     &         ' and <abbr title="A I C diff">aicdiff</abbr> = ',
     &         F7.4,') prefers model <strong>',a)
c-----------------------------------------------------------------------
      END

      