      SUBROUTINE pass2(Trnsrs,Frstry,Ipr,Idr,Iqr,Ips,Ids,Iqs,Lpr,
     &                 Ldr,Lqr,Lps,Lds,Lqs,Naut,Naut0,Plbox,Plbox0,
     &                 Bldf,Bldf0,Rvr,Rvr0,Lmu,Lmu0,A,A0,Na,Na0,Aici0,
     &                 Pcktd0,Aicit0,Adj0,Trns0,Fct2,Lprt,Lprtlb,Ismd0,
     &                 Cvl0,Nefobs,Nloop,Nround,Igo)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c   Compares two models to determine which is the final model
c   selected by the automatic modeling routine.
c-----------------------------------------------------------------------
      LOGICAL T,F
      DOUBLE PRECISION ONE,PI,TWO,TWOPT8
      PARAMETER(T=.true.,F=.false.,ONE=1D0,PI=3.14159265358979d0,
     &          TWO=2D0,TWOPT8=2.8D0)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'series.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'priusr.cmn'
      INCLUDE 'adj.cmn'
      INCLUDE 'inpt.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      INTEGER ardsp,ar1p,Frstry,Igo,Ipr,Idr,Iqr,Ips,Ids,Iqs,Lpr,Ldr,Lqr,
     &        Lps,Lds,Lqs,Naut0,Naut,Nloop,Nround,ichk,Bldf,Bldf0,Na,
     &        Na0,Nefobs,icol,Aicit0,Aici0
      DOUBLE PRECISION A,A0,Cvl0,Fct2,Plbox,Plbox0,Rvr,Rvr0,Trnsrs,Adj0,
     &                 Trns0
      LOGICAL Lmu,Lmu0,Lprt,Ismd0,inptok,Pcktd0,lcv,Lprtlb
      DIMENSION A(*),A0(*),Cvl0(POTLR),Trnsrs(*),adj0(*),trns0(*)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      ardsp=Nnsedf+Nseadf
      ar1p=ardsp+1
      Igo=0
      ichk=0
      lcv=.false.
      IF(Naut0.le.Naut.AND.
     &  (Idr.NE.Ldr.OR.Ids.NE.Lds.OR.Ipr.NE.Lpr.OR.Ips.NE.Lps.OR.
     &   Iqr.NE.Lqr.OR.Iqs.NE.Lqs.OR.(Lmu.NEQV.Lmu0)))THEN
       IF(Plbox.LT.0.95D0.AND.Plbox0.LT.0.75D0.AND.Rvr0.LT.Rvr)THEN
c        write(Mt1,991)Plbox,Rvr,Plbox0,Rvr0
c  991   format('  Plbox  = ',f10.3,'  Rvr  = ',e15.10,/,
*     &         '  Plbox0 = ',f10.3,'  Rvr0 = ',e15.10)
        ichk=1
       ELSE IF(Nloop.eq.1.and.Plbox.GE.0.95D0.AND.Plbox0.LT.0.95D0)THEN
c        write(Mt1,992)Plbox,Plbox0
c  992   format('  Plbox  = ',f10.3,'  Plbox0 = ',f10.3)
        ichk=2
       ELSE IF(Plbox.LT.0.95D0.AND.Plbox0.LT.0.75D0.AND.Plbox0.LT.Plbox
     &         .and.Rvr0.LT.Fct*Rvr)THEN
c        write(Mt1,993)Plbox,Rvr,Fct,Plbox0,Rvr0
c  993   format('  Plbox  = ',f10.3,'  Rvr  = ',e15.10,'  Fct = ',f5.3,/,
*     &         '  Plbox0 = ',f10.3,'  Rvr0 = ',e15.10)
        ichk=3
       ELSE IF(Plbox.GE.0.95D0.AND.Plbox0.LT.0.95D0.AND.
     &         Rvr0.LT.Fct2*Rvr)THEN
c        write(Mt1,994)Plbox,Rvr,Fct2,Plbox0,Rvr0
c  994   format('  Plbox  = ',f10.3,'  Rvr  = ',e15.10,'  Fct2 = ',f5.3,
c     &       /,'  Plbox0 = ',f10.3,'  Rvr0 = ',e15.10)
        ichk=4
       ELSE IF(Idr.EQ.0.AND.Ids.EQ.1.AND.Ipr.EQ.1.AND.
     &         Arimap(ar1p).GE.0.82D0.AND.Ips.EQ.0.AND.Iqr.LE.1.AND.
     &         Iqs.EQ.1)THEN
c        write(Mt1,995)Arimap(ar1p)
c  995   format('  arimap(ar1p) = ',f10.5)
        ichk=5
       ELSE IF(Idr.EQ.1.AND.Ids.EQ.0.AND.Ipr.EQ.0.AND.
     &         Arimap(ar1p).GE.0.65D0.AND.Ips.EQ.1.AND.Iqr.EQ.1.AND.
     &         Iqs.LE.1)THEN
c        write(Mt1,995)Arimap(ar1p)
        ichk=6
       END IF
       IF(ichk.gt.0)THEN
c        WRITE(Mt1,1000)ichk
c 1000   FORMAT(' ichk (auto versus default model) = ',i3)
        CALL mdlint()
        CALL mdlset(Lpr,Ldr,Lqr,Lps,Lds,Lqs,inptok)
        IF(Lfatal)RETURN
        Nefobs=Nspobs-Nintvl
        Dnefob=dble(Nefobs)
        Lnlkhd=-(Lndtcv+Dnefob*(log(TWO*PI*Var)+ONE))/TWO
c-----------------------------------------------------------------------
c  If choice of trading day changed from default to chosen model,
c  reset adjustment factors, transformed series to what they were
c  for the default model  (BCM May 2004)
c-----------------------------------------------------------------------
        IF((Pcktd0.and.(.not.Picktd)).or.((.not.Pcktd0).and.Picktd))THEN
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
c-----------------------------------------------------------------------
        CALL bkdfmd(F)
        CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &              Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
        IF(.not.Lfatal)CALL rgarma(T,Mxiter,Mxnlit,F,A,Na,Nefobs,Lautom)
        IF(.not.Lfatal)CALL prterr(nefobs,T)
        IF(Lfatal)RETURN
        IF(Lprt)THEN
         WRITE(Mt1,1010)'<p>Model changed to '
         IF(Lmu.EQV.Lmu0)THEN
          WRITE(Mt1,1020) Lpr,Ldr,Lqr,Lps,Lds,Lqs
         ELSE IF(Lmu0)THEN
          WRITE(Mt1,1021) Lpr,Ldr,Lqr,Lps,Lds,Lqs,' with constant term'
         ELSE
          WRITE(Mt1,1021) Lpr,Ldr,Lqr,Lps,Lds,Lqs,
     &                    ' without constant term'
         END IF
         CALL writTag(Mt1,'</p>')
        END IF
        Ismd0=T
        Plbox=Plbox0
        Rvr=Rvr0
        Ipr=Lpr
        Idr=Ldr
        Iqr=Lqr
        Ips=Lps
        Ids=Lds
        Iqs=Lqs
        Lmu=Lmu0
        Bldf=Bldf0
        Aicind=Aici0
        Aicint=Aicit0
        CALL mkmdsn(Ipr,Idr,Iqr,Ips,Ids,Iqs,Bstdsn,Nbstds)
        IF(Lfatal)RETURN
        Na=Na0
        CALL copy(A0,Na,1,A) 
       END IF
      END IF
*      IF(ichk.eq.0)THEN
*       write(Mt1,996)Plbox,Rvr,Naut,Plbox0,Rvr0,Naut0
* 996   format('  Plbox  = ',f10.3,'  Rvr  = ',e15.10,'  Naut  = ',i3,/,
*     &        '  Plbox0 = ',f10.3,'  Rvr0 = ',e15.10,'  Naut0 = ',i3)
*      END IF
      Plbox0=Plbox
      Rvr0=Rvr
      Naut0=Naut
c-----------------------------------------------------------------------
c    Check to see if automatic modeling needs to be redone.
c    add to nloop if so
c-----------------------------------------------------------------------
      IF(Nloop.eq.1)THEN
       Pcr=Pcr+.025D0
      ELSE
       Pcr=Pcr+.015D0
      END IF
      IF(Plbox.GT.Pcr)THEN
       IF(Lprtlb)WRITE(Mt1,1040)Bldf,Plbox,Pcr
       IF((Nloop.eq.1).and.(.not.Lotmod))THEN
        lcv=(Ltstao.and.Critvl(AO).gt.TWOPT8).or.
     &      (Ltstls.and.Critvl(LS).gt.TWOPT8).or.
*     &      (Ltsttc.and.Critvl(TC).gt.TWOPT8).or.
*     &      (Ltstso.and.Critvl(SO).gt.TWOPT8)
     &      (Ltsttc.and.Critvl(TC).gt.TWOPT8)
        IF(lcv)THEN
         IF(Lprtlb)CALL mkPOneLine(Mt1,'@',
     &      'Automatic outlier identification will be redone.')
         IF(Ltstao)THEN
          Cvl0(AO)=Critvl(AO)
          Critvl(AO)=DMAX1(TWOPT8,Critvl(AO)-Critvl(AO)*Predcv)
          IF((.not.dpeq(Cvl0(AO),Critvl(AO))).and.Lprtlb)
     &       WRITE(Mt1,1030)'AO',Critvl(AO)
         END IF
         IF(Ltstls)THEN
          Cvl0(LS)=Critvl(LS)
          Critvl(LS)=DMAX1(TWOPT8,Critvl(LS)-Critvl(LS)*Predcv)
          IF((.not.dpeq(Cvl0(LS),Critvl(LS))).and.Lprtlb)
     &       WRITE(Mt1,1030)'LS',Critvl(LS)
         END IF
         IF(Ltsttc)THEN
          Cvl0(TC)=Critvl(TC)
          Critvl(TC)=DMAX1(TWOPT8,Critvl(TC)-Critvl(TC)*Predcv)
          IF((.not.dpeq(Cvl0(TC),Critvl(TC))).and.Lprtlb)
     &       WRITE(Mt1,1030)'TC',Critvl(TC)
         END IF
*         IF(Ltstso)THEN
*          Cvl0(SO)=Critvl(SO)
*          Critvl(SO)=DMAX1(TWOPT8,Critvl(SO)-Critvl(SO)*Predcv)
*          IF((.not.dpeq(Cvl0(SO),Critvl(SO))).and.Lprtlb)
*     &       WRITE(Mt1,1030)'SO',Critvl(SO)
*         END IF
        END IF
       END IF
       Ldr=Idr
       Lds=Ids
       Lpr=Ipr
       Lps=Ips
       Lqr=Iqr
       Lqs=Iqs
       Lmu0=Lmu
       Nloop=Nloop+1
       Nround=Nround+1
c-----------------------------------------------------------------------
c   Change automatically identified outliers into regular outliers
c   of the same type
c-----------------------------------------------------------------------
c       iao=0
c       DO icol=Nb,1,-1
c        IF(Rgvrtp(icol).eq.PRGTAA.or.Rgvrtp(icol).eq.PRGTAL.or.
c     &     Rgvrtp(icol).eq.PRGTAT)THEN
c         bcol=B(icol)
c         IF(Rgvrtp(icol).eq.PRGTAA.or.Rgvrtp(icol).eq.PRGTAL)THEN
c          rtype=Rgvrtp(icol)-3
c         ELSE IF(Rgvrtp(icol).eq.PRGTAT)THEN
c          rtype=PRGTTC
c         END IF
c         CALL getstr(Colttl,Colptr,Ncoltl,icol,str,nchr)
c         CALL dlrgef(icol,Nrxy,1)
c         CALL adrgef(bcol,str(1:nchr),str(1:nchr),rtype,F)
c         iao=iao+1
c        END IF
c       END DO 
c       CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
c     &             Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
c       IF(Lfatal)RETURN
       IF(Nloop.le.2.and.(.not.Lotmod).or.(Nloop.eq.2.and.lcv))THEN
        IF(Ismd0)THEN
         Ismd0=F
         Naut=Naut0
         Igo=2
        ELSE IF(Lautod.and.Lautom)THEN
         Igo=1
        ELSE
         Igo=3
        END IF
       ELSE
        Ipr=3
        IF (Ids.GT.0) Ips=0
c-----------------------------------------------------------------------
c   Change model of last resort to a non mixed model if Lmixmd not true
c   BCM May 2004
c-----------------------------------------------------------------------
        IF(Lmixmd)THEN
         Iqr=1
        ELSE
         Iqr=0
        END IF
c-----------------------------------------------------------------------
        IF (Sp.GT.1) THEN
         IF(Lmixmd)THEN
          IF(Maxord(2).gt.0)Iqs=1
         ELSE
          IF(Ips.eq.0.and.Maxord(2).gt.0)Iqs=1
         END IF
        END IF
        CALL mkmdsn(Ipr,Idr,Iqr,Ips,Ids,Iqs,Bstdsn,Nbstds)
        IF(Lprtlb)CALL mkPOneLine(Mt1,'@','Model changed to '//
     &           Bstdsn(1:Nbstds)//' due to unacceptable Ljung-Box Q '//
     &           ' statistics for previously identified models.')
        IF(Lfatal)RETURN
        CALL mdlint()
        CALL mdlset(Ipr,Idr,Iqr,Ips,Ids,Iqs,inptok)
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
        CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &              Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
        IF(.not.Lfatal)CALL rgarma(T,Mxiter,Mxnlit,F,A,Na,Nefobs,Lautom)
*        IF(.not.Lfatal)CALL rgarma(T,Mxiter,Mxnlit,T,SA,Na,Nefobs,Lautom)
        IF((.not.Lfatal).and.Convrg)CALL prterr(nefobs,T)
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     If the estimation did not converge, reduce the AR order until
c     either the estimation converges or there are no AR orders left
c     BCM (Nov 2008, Aug 2018)
c-----------------------------------------------------------------------
        IF(.not.Convrg)THEN
         DO WHILE ((.not.Convrg).and.Ipr.gt.0)
          Ipr=Ipr-1
          CALL mdlint()
          CALL mdlset(Ipr,Idr,Iqr,Ips,Ids,Iqs,inptok)
          IF(.not.Lfatal)
     &       CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,
     &                   Nrusrx,Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
          IF(.not.Lfatal)
     &       CALL rgarma(T,Mxiter,Mxnlit,F,A,Na,Nefobs,Lautom)
         END DO
c-----------------------------------------------------------------------
c    print out error message if Convrg false and Ipr < 0 and set
c    lfatal = true         
c-----------------------------------------------------------------------
         IF(Ipr.le.0.and.(.not.Convrg))THEN
          CALL abend()
          RETURN
         END IF
c-----------------------------------------------------------------------
c    If model is changed, change model string to new model
c-----------------------------------------------------------------------
         IF(Ipr.lt.3)THEN
          CALL mkmdsn(Ipr,Idr,Iqr,Ips,Ids,Iqs,Bstdsn,Nbstds)
          IF(Lprtlb)CALL mkPOneLine(Mt1,'@','Model changed to '//
     &                   Bstdsn(1:Nbstds)//' due to estimation errors.')
          IF(Lfatal)RETURN
         END IF
        END IF
        IF(Lotmod.or.(Nloop.eq.2.and.(.not.lcv)))THEN
         Nloop=3
        ELSE
         IF(Ltstao)Critvl(AO)=cvl0(AO)
         IF(Ltstls)Critvl(LS)=cvl0(LS)
         IF(Ltsttc)Critvl(TC)=cvl0(TC)
*         IF(Ltstso)Critvl(SO)=cvl0(SO)
c-----------------------------------------------------------------------
c    remove automatically identified outliers 
c-----------------------------------------------------------------------
         IF(Natotl.gt.0)THEN
          CALL clrotl(Nrxy)
          IF(.not.Lfatal)
     &       CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,
     &                   Nrusrx,Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
          IF(Lfatal)RETURN
         END IF
        END IF
        Igo=2
       END IF
      END IF
      RETURN
c-----------------------------------------------------------------------
 1010 FORMAT(' ',a)
 1020 FORMAT('  ',2(' (',i2,',',i2,',',i2,')'))
 1021 FORMAT('  ',2(' (',i2,',',i2,',',i2,')'),a)
 1030 FORMAT('<p>Critical Value for ',a,' outlier id changed to:',1x,
     &       F12.3,'.</p>')
 1040 FORMAT(/,'<p>Confidence coefficient for Ljung-Box Q at lag ',i3,
     &         ' = ',f10.4,','
     &       /,'  which is greater than the acceptance limit, ',f10.4,
     &         '.</p>')
      END
