C     Last change:  BCM  23 Mar 2005    9:23 am
      SUBROUTINE usraic(Trnsrs,A,Nefobs,Na,Frstry,Lester,Lprtit,Lprt,
     &                  Lprtfm,Lsavlg,Lsumm,Lhiddn)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Estimate two regARIMA models, one with user-defined regressors and
c     one without.  This routine chooses the model with the lowest value
c     of AICC and prints out the resulting model.
c-----------------------------------------------------------------------
      LOGICAL F,T
      DOUBLE PRECISION ZERO,ONE
      PARAMETER(F=.false.,T=.true.,ZERO=0D0,ONE=1D0)
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'lkhd.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'adj.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'htmlout.cmn'
      INCLUDE 'usrreg.cmn'
c-----------------------------------------------------------------------
      INTEGER PA
      PARAMETER(PA=PLEN+2*PORDER)
c-----------------------------------------------------------------------
      CHARACTER effttl*(PCOLCR),ubkttl*(PCOLCR*PUREG)
      LOGICAL Lprt,Lprtit,Lester,argok,lhide,Lprtfm,Lsavlg,ubkfix,Lhiddn
      DOUBLE PRECISION A,aicnou,aicusr,Trnsrs,ubkx,ubkb,thiscv
      INTEGER Frstry,i,Na,Nefobs,nchr,iuser,nubk,ubktyp,ubkptr,begcol,
     &        endcol,igrp,Lsumm,endlag,ilag,nbu,nbno,aicdf,rtype
      DIMENSION A(PA),Trnsrs(PLEN),ubktyp(PUREG),ubkptr(0:PUREG),
     &          ubkx(PUSERX),ubkfix(PUREG),ubkb(PUREG)
c-----------------------------------------------------------------------
      INTEGER strinx
      LOGICAL dpeq
      EXTERNAL strinx,dpeq
c-----------------------------------------------------------------------
c     Initialize variables
c-----------------------------------------------------------------------
      IF(.not.Lprt)THEN
       lhide=Lhiddn
       Lhiddn=T
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
      IF(Lprt)CALL genSkip(1019)
c-----------------------------------------------------------------------
c     Estimate model with user-defined regressors
c-----------------------------------------------------------------------
      CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &            Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
      IF(Lfatal)RETURN
      argok=Lautom.or.Lautox
      CALL rgarma(T,Mxiter,Mxnlit,F,A,Na,Nefobs,argok)
      IF((.not.Lfatal).and.(Lautom.or.Lautox).and.(.not.argok))
     &   CALL abend()
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     If an estimation error is found, discontinue the routine.
c-----------------------------------------------------------------------
      IF(Armaer.eq.PMXIER.or.Armaer.eq.PSNGER.or.Armaer.eq.PISNER.or.
     &   Armaer.eq.PNIFER.or.Armaer.eq.PNIMER.or.Armaer.eq.PCNTER.or.
     &   Armaer.eq.POBFN0.or.Armaer.lt.0.or.
     &   ((Lautom.or.Lautox).and..not.argok))THEN
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
      IF(Lprt)CALL writTagOneLine(Mt1,'h3','@',
     &   'Likelihood statistics for model with user-defined regressors')
      CALL prlkhd(Y(Frstsy),Adj(Adj1st),Adjmod,Fcntyp,Lam,F,Lprt,F)
      IF(Lfatal)RETURN
      aicusr=Aicc
      IF(.not.dpeq(Pvaic,DNOTST))nbu=Nb
      IF(Lsavlg)THEN
       Inlgfl=Inlgfl+1
       WRITE(Ng,1000)Inlgfl
       CALL mkTableTag(Ng,'w60','@')
       CALL mkCaption(Ng,'<abbr title="A I C test">AICtest</abbr> '//
     &                'for User Defined Regressors')
       CALL writTag(Ng,'<tr>')
       CALL mkHeaderCellScope(Ng,0,0,'row','corrected Akaike '//
     &                        'information criterion','AICC')
       CALL mkTableCell(Ng,'center','(user regression)')
       WRITE(Ng,1010)Aicc
       CALL writTag(Ng,'</tr>')
      END IF
      IF(Lsumm.gt.0)WRITE(Nform,1020)'user',Aicc
c-----------------------------------------------------------------------
c     Make local backup copy of user defined regressors.
c-----------------------------------------------------------------------
      CALL copy(Userx,PUSERX,1,ubkx)
      CALL cpyint(Usrtyp,PUREG,1,ubktyp)
      CALL cpyint(Usrptr(0),PUREG+1,1,ubkptr(0))
      nubk=Ncusrx
      ubkttl=Usrttl
c-----------------------------------------------------------------------
c     save the regression coefficients, fixed regression indicators for
c     the user defined regressors
c-----------------------------------------------------------------------
      iuser=0
      DO igrp=1,Ngrp
       begcol=Grp(igrp-1)
       rtype=Rgvrtp(begcol)
       IF(rtype.eq.PRGUTD.or.rtype.eq.PRGULM.or.rtype.eq.PRGULQ.or.
     &    rtype.eq.PRGULY.or.rtype.eq.PRGTUD.or.rtype.eq.PRGTUH.or.
     &    rtype.eq.PRGUH2.or.rtype.eq.PRGUH3.or.rtype.eq.PRGUH4.or.
     &    rtype.eq.PRGUH5.or.rtype.eq.PRGUAO.or.rtype.eq.PRGULS.or.
     &    rtype.eq.PRGUSO.or.rtype.eq.PRGUCN.or.rtype.eq.PRGUCY)THEN
        endcol=Grp(igrp)-1
        DO i=begcol,endcol
         iuser=iuser+1
         ubkb(iuser)=B(i)
         ubkfix(iuser)=Regfx(i)
        END DO
       END IF
      END DO
c-----------------------------------------------------------------------
c     remove the user defined regressors from the regression matrix.
c-----------------------------------------------------------------------
      DO igrp=Ngrp,1,-1
       begcol=Grp(igrp-1)
       rtype=Rgvrtp(begcol)
       IF(rtype.eq.PRGUTD.or.rtype.eq.PRGULM.or.rtype.eq.PRGULQ.or.
     &    rtype.eq.PRGULY.or.rtype.eq.PRGTUD.or.rtype.eq.PRGTUH.or.
     &    rtype.eq.PRGUH2.or.rtype.eq.PRGUH3.or.rtype.eq.PRGUH4.or.
     &    rtype.eq.PRGUH5.or.rtype.eq.PRGUAO.or.rtype.eq.PRGULS.or.
     &    rtype.eq.PRGUSO.or.rtype.eq.PRGUCN.or.rtype.eq.PRGUCY)THEN
        endcol=Grp(igrp)-1
        iuser=endcol-begcol+1
        CALL dlrgef(begcol,Nrxy,iuser)
        IF(Lfatal)RETURN
       END IF
      END DO
      Ncusrx=0
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
c     Re-estimate model without user-defined regressors
c-----------------------------------------------------------------------
      CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &            Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
      IF(.not.Lfatal)CALL rgarma(T,Mxiter,Mxnlit,F,A,Na,Nefobs,argok)
      IF((.not.Lfatal).and.(Lautom.or.Lautox).and.(.not.argok))
     &   CALL abend()
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     If an estimation error is found, discontinue the routine.
c-----------------------------------------------------------------------
      IF(Armaer.eq.PMXIER.or.Armaer.eq.PSNGER.or.Armaer.eq.PISNER.or.
     &   Armaer.eq.PNIFER.or.Armaer.eq.PNIMER.or.Armaer.eq.PCNTER.or.
     &   Armaer.eq.POBFN0.or.Armaer.lt.0.or.
     &   ((Lautom.or.Lautox).and..not.argok))THEN
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
      IF(Lprt)CALL writTagOneLine(Mt1,'h3','@',
     &'Likelihood statistics for model without user-defined regressors')
      CALL prlkhd(Y(Frstsy),Adj(Adj1st),Adjmod,Fcntyp,Lam,F,Lprt,Lprtfm)
      aicnou=Aicc
      IF((.not.dpeq(Pvaic,DNOTST)).and.dpeq(Rgaicd(PUAIC),ZERO))nbno=Nb
      IF(Lsavlg)THEN
       CALL writTag(Ng,'<tr>')
       CALL mkHeaderCellScope(Ng,0,0,'row','corrected Akaike '//
     &                        'information criterion','AICC')
       CALL mkTableCell(Ng,'center','(no user regression)')
       WRITE(Ng,1010)Aicc
       CALL writTag(Ng,'</tr>')
      END IF
      IF(Lsumm.gt.0)WRITE(Nform,1020)'nouser',Aicc
      IF(.not.Lprt)Lhiddn=lhide
      IF(Lsavlg)THEN
       CALL writTag(Ng,'</table></div>')
       CALL mkPOneLine(Ng,'@','&nbsp;')
      END IF      
c-----------------------------------------------------------------------
c     Show the regression model AICC prefers
c-----------------------------------------------------------------------
      Dfaicu=aicnou-aicusr
      IF(.not.dpeq(Pvaic,DNOTST))THEN
       aicdf=nbu-nbno
       CALL chsppf(Pvaic,aicdf,thiscv,Mt1)
       Rgaicd(PUAIC)=thiscv-2D0*dble(aicdf)
      END IF
      IF(Dfaicu.gt.Rgaicd(PUAIC))THEN
       IF(Lprt)THEN
        IF(dpeq(Pvaic,DNOTST))THEN
         WRITE(Mt1,1030)Rgaicd(PUAIC),'with'
        ELSE
         WRITE(Mt1,1040)ONE-Pvaic,Rgaicd(PUAIC),'with'
        END IF       
       END IF
c-----------------------------------------------------------------------
c     Add user-defined regressors back to model
c-----------------------------------------------------------------------
       CALL copy(ubkx,PUSERX,1,Userx)
       CALL cpyint(ubktyp,PUREG,1,Usrtyp)
       CALL cpyint(ubkptr(0),PUREG+1,1,Usrptr(0))
       Ncusrx=nubk
       Usrttl=ubkttl
c-----------------------------------------------------------------------
c     Restore user-defined regressors to the regression matrix
c-----------------------------------------------------------------------
       DO i=1,Ncusrx
        CALL getstr(Usrttl,Usrptr,Ncusrx,i,effttl,nchr)
        IF(.not.Lfatal)THEN
         IF(Usrtyp(i).eq.PRGTUH)THEN
          CALL adrgef(ubkb(i),effttl(1:nchr),'User-defined Holiday',
     &                PRGTUH,ubkfix(i),F)
         ELSE IF(Usrtyp(i).eq.PRGUH2)THEN
          CALL adrgef(ubkb(i),effttl(1:nchr),
     &                'User-defined Holiday Group 2',PRGUH2,ubkfix(i),F)
         ELSE IF(Usrtyp(i).eq.PRGUH3)THEN
          CALL adrgef(ubkb(i),effttl(1:nchr),
     &                'User-defined Holiday Group 3',PRGUH3,ubkfix(i),F)
         ELSE IF(Usrtyp(i).eq.PRGUH4)THEN
          CALL adrgef(ubkb(i),effttl(1:nchr),
     &                'User-defined Holiday Group 4',PRGUH4,ubkfix(i),F)
         ELSE IF(Usrtyp(i).eq.PRGUH5)THEN
          CALL adrgef(ubkb(i),effttl(1:nchr),
     &                'User-defined Holiday Group 5',PRGUH5,ubkfix(i),F)
         ELSE IF(Usrtyp(i).eq.PRGTUS)THEN
          CALL adrgef(ubkb(i),effttl(1:nchr),
     &                'User-defined Seasonal',PRGTUS,ubkfix(i),F)
         ELSE IF(Usrtyp(i).eq.PRGUCN)THEN
          CALL adrgef(ubkb(i),effttl(1:nchr),
     &               'User-defined Constant',PRGUCN,ubkfix(i),F)
         ELSE IF(Usrtyp(i).eq.PRGUTD)THEN
          CALL adrgef(ubkb(i),effttl(1:nchr),
     &                'User-defined Trading Day',PRGUTD,
     &                ubkfix(i),F)
         ELSE IF(Usrtyp(i).eq.PRGULM)THEN
          CALL adrgef(ubkb(i),effttl(1:nchr),
     &                'User-defined LOM',PRGULM,ubkfix(i),F)
         ELSE IF(Usrtyp(i).eq.PRGULQ)THEN
          CALL adrgef(ubkb(i),effttl(1:nchr),
     &                'User-defined LOQ',PRGULQ,ubkfix(i),F)
         ELSE IF(Usrtyp(i).eq.PRGULY)THEN
          CALL adrgef(ubkb(i),effttl(1:nchr),
     &                'User-defined Leap Year',PRGULY,ubkfix(i),F)
         ELSE IF(Usrtyp(i).eq.PRGUAO)THEN
          CALL adrgef(ubkb(i),effttl(1:nchr),'User-defined AO',
     &                PRGUAO,ubkfix(i),F)
         ELSE IF(Usrtyp(i).eq.PRGULS)THEN
          CALL adrgef(ubkb(i),effttl(1:nchr),'User-defined LS',
     &                PRGULS,ubkfix(i),F)
         ELSE IF(Usrtyp(i).eq.PRGUSO)THEN
          CALL adrgef(ubkb(i),effttl(1:nchr),'User-defined SO',
     &                PRGUSO,ubkfix(i),F)
         ELSE IF(Usrtyp(i).eq.PRGUCY)THEN
          CALL adrgef(ubkb(i),effttl(1:nchr),
     &                'User-defined Transitory',PRGUCY,
     &                ubkfix(i),F)
         ELSE
          CALL adrgef(ubkb(i),effttl(1:nchr),'User-defined',PRGTUD,
     &                ubkfix(i),F)
         END IF
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
c     Estimate model
c-----------------------------------------------------------------------
       CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &             Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
       IF(.not.Lfatal)CALL rgarma(T,Mxiter,Mxnlit,Lprtit,A,Na,Nefobs,
     &                            argok)
       IF((.not.Lfatal).and.(Lautom.or.Lautox).and.(.not.argok))Lester=T
      ELSE
       IF(Lprt)THEN
        IF(dpeq(Pvaic,DNOTST))THEN
         WRITE(Mt1,1030)Rgaicd(PUAIC),'without'
        ELSE
         WRITE(Mt1,1040)ONE-Pvaic,Rgaicd(PUAIC),'without'
        END IF       
       END IF       
      END IF
c-----------------------------------------------------------------------
 1000 FORMAT('<div id="lgaic',i6.6,'">')
 1010 FORMAT('<td>',f15.4,'</td>')
 1020 FORMAT('aictest.u.aicc.',a,': ',e29.15)
 1030 FORMAT(/,'<p class="center">***** <abbr title="corrected ',
     &         'Akaike information criterion">AICC</abbr>',
     &       /,' (with <abbr title="A I C diff">aicdiff</abbr> = ',
     &         F7.4,') prefers model <strong>',a,
     &         ' user-defined regressor</strong> *****</p>')
 1040 FORMAT(/,'<p class="center">***** <abbr title="corrected ',
     &         'Akaike information criterion">AICC</abbr>',
     &       /,'p-value = ',F7.5,' and <abbr title="A I C diff">'
     &       /,'aicdiff</abbr> = ',F7.4,') prefers model <strong>',a,
     &         ' user-defined regressor</strong> *****</p>')
      RETURN
      END
