C     Last change:  BCM   8 Dec 1998    4:03 pm
      SUBROUTINE ssmdl(Iyr,Im,Itd,Ihol,Tdfix,Holfix,Otlfix,Usrfix,Locok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'ssprep.cmn'
      INCLUDE 'sspinp.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'otlrev.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'usrreg.cmn'
c-----------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c-----------------------------------------------------------------------
      CHARACTER grchr1*(PGRPCR*PGRP),grchr2*(PGRPCR*PGRP),
     &          igrptl*(PGRPCR)
      INTEGER igrp,begcol,nchr,idtpos,regmdt,begrgm,nbeg,nend,begss,
     &        starta,enda,sspos,endcol,icol,rtype,Iyr,Im,Itd,Ihol,
     &        gr1ptr,gr2ptr,ngrp1,ngrp2,ngr1tl,ngr2tl,fhnote
      LOGICAL erregm,Locok,regchg,erreg2,Tdfix,Holfix,Usrfix,Otlfix
      DIMENSION gr1ptr(0:PGRP),gr2ptr(0:PGRP),regmdt(2),starta(2),
     &          enda(2),begss(2)
c-----------------------------------------------------------------------
c     Initialize change of regime dictionaries and pointer variables.
c-----------------------------------------------------------------------
      CALL intlst(PB,gr1ptr,ngr1tl)
      CALL intlst(PB,gr2ptr,ngr2tl)
      ngrp1=ngr1tl+1
      ngrp2=ngr2tl+1
c-----------------------------------------------------------------------
c     If automatic modelling done, setup so the model selected for
c     the entire series will be used in each of the spans
c-----------------------------------------------------------------------
      Lautox=F
      Lautom=F
      Lautod=F
      regchg=F
      fhnote=STDERR
      IF(Lquiet)fhnote=0
c-----------------------------------------------------------------------
c     Fix selected regressors based on values of Ssfxrg
c-----------------------------------------------------------------------
      IF(Nb.gt.0)THEN
       IF(Nssfxr.gt.0)THEN
        CALL rvfixd(Tdfix,Holfix,Otlfix,Usrfix,Iregfx,Regfx,Nb,Rgvrtp,
     &              Nusrrg,Usrtyp,Ncusrx,Userfx)
        IF(Itd.eq.1.and.Tdfix)Itd=-1
        IF(Ihol.eq.1.and.Holfix)Ihol=-1
       ELSE IF(Iregfx.eq.3)THEN
        IF(Itd.eq.1)THEN
         Itd=-1
         IF(Ssinit.ne.1)THEN
          Nssfxr=1
          Ssfxrg(1)=1
         END IF
        END IF
        IF(Ihol.eq.1)THEN
         Ihol=-1
         IF(Ssinit.ne.1)THEN
          Nssfxr=Nssfxr+1
          Ssfxrg(Nssfxr)=2
         END IF
        END IF
       ELSE IF(Iregfx.eq.2)THEN
        Tdfix=T
        Holfix=T
        DO igrp=1,Ngrp
         endcol=Grp(igrp)-1
         begcol=Grp(igrp-1)
         rtype=Rgvrtp(begcol)
         IF((rtype.eq.PRGTTD.or.rtype.eq.PRGTST.or.rtype.eq.PRRTTD.or.
     &       rtype.eq.PRA1TD.or.rtype.eq.PRRTST.or.rtype.eq.PRATTD.or.
     &       rtype.eq.PRATST.or.rtype.eq.PRG1TD.or.rtype.eq.PRR1TD.or.
     &       rtype.eq.PRG1ST.or.rtype.eq.PRR1ST.or.rtype.eq.PRA1ST.or.
     &      (rtype.eq.PRGTLM.or.rtype.eq.PRGTSL.or.
     &       rtype.eq.PRGTLQ.or.rtype.eq.PRGTLY.or.rtype.eq.PRATLQ.or.
     &       rtype.eq.PRRTLM.or.rtype.eq.PRRTSL.or.rtype.eq.PRRTLQ.or.
     &       rtype.eq.PRRTLY.or.rtype.eq.PRATLM.or.rtype.eq.PRATSL.or.
     &       rtype.eq.PRATLY).or.
     &      (rtype.eq.PRGUTD.or.rtype.eq.PRGULM.or.rtype.eq.PRGULQ.or.
     &       rtype.eq.PRGULY).or.
     &      (rtype.eq.PRGTEA.or.rtype.eq.PRGTEC.or.rtype.eq.PRGTES.or.
     &       rtype.eq.PRGTLD.or.rtype.eq.PRGTTH).or.
     &      (rtype.ge.PRGTUH.and.rtype.le.PRGUH5).or.
     &       rtype.eq.PRGTUS))THEN
          DO icol=begcol,endcol
           IF(rtype.eq.PRGTEA.or.rtype.eq.PRGTEC.or.rtype.eq.PRGTES
     &             .or.rtype.eq.PRGTLD.or.rtype.eq.PRGTTH.or.
     &             (rtype.ge.PRGTUH.and.rtype.le.PRGUH5))THEN
            Holfix=Holfix.and.Regfx(icol)
           ELSE
            Tdfix=Tdfix.and.Regfx(icol)
           END IF
          END DO
         END IF
        END DO
        IF(Tdfix.and.Itd.gt.0)THEN
         Itd=-1
         IF(Ssinit.ne.1)THEN
          Nssfxr=1
          Ssfxrg(1)=1
         END IF
        END IF
        IF(Holfix.and.Ihol.eq.1)THEN
         Ihol=-1
         IF(Ssinit.ne.1)THEN
          Nssfxr=Nssfxr+1
          Ssfxrg(Nssfxr)=2
         END IF
        END IF
       END IF
      ELSE
       Nssfxr=0
      END IF
c-----------------------------------------------------------------------
      CALL intlst(PB,Otrptr,Notrtl)
      IF(Ssotl.le.1)THEN
       Ltstao=F
       Ltstls=F
       Ltsttc=F
*       Ltstso=F
      END IF
c-----------------------------------------------------------------------
c     Set up dates for testing change of regime and outlier variables.
c-----------------------------------------------------------------------
      begss(YR)=Iyr
      begss(MO)=Im
      CALL addate(begss,Sp,(Ncol-1)*Sp,starta)
      CALL addate(Endspn,Sp,(1-Ncol)*Sp,enda)
      CALL dfdate(enda,starta,Sp,sspos)
      Locok=T
      erregm=F
      erreg2=F
      DO igrp=Ngrp,1,-1
       begcol=Grp(igrp-1)
       endcol=Grp(igrp)-1
c-----------------------------------------------------------------------
c     Check to see if there are any change of regime regression 
c     variables in the model.  If there is, check to see if the 
c     change of regime will be defined over all the spans.
c-----------------------------------------------------------------------
       IF(Rgvrtp(begcol).eq.PRRTST.or.Rgvrtp(begcol).eq.PRRTTD.or.
     &    Rgvrtp(begcol).eq.PRRTSE.or.Rgvrtp(begcol).eq.PRRTTS.or.
     &    Rgvrtp(begcol).eq.PRRTLM.or.Rgvrtp(begcol).eq.PRRTLQ.or.
     &    Rgvrtp(begcol).eq.PRRTLY.or.Rgvrtp(begcol).eq.PRRTSL.or.
     &    Rgvrtp(begcol).eq.PRR1TD.or.Rgvrtp(begcol).eq.PRR1ST)THEN
        CALL getstr(Grpttl,Grpptr,Ngrp,igrp,igrptl,nchr)
        IF(Lfatal)RETURN
        idtpos=index(igrptl(1:nchr),'(before ')+8
        IF(idtpos.eq.8)
     &     idtpos=index(igrptl(1:nchr),'(change from before ')+20
        CALL ctodat(igrptl(1:nchr-1),Sp,idtpos,regmdt,Locok)
        CALL dfdate(regmdt,starta,Sp,begrgm)
        IF(begrgm.le.Sp)THEN
         CALL insstr(igrptl(1:nchr),ngrp1,PB,grchr1,gr1ptr,ngr1tl)
         IF(Lfatal)RETURN
         IF(Ssinit.ne.1)THEN
          DO icol=begcol,endcol
           IF(.not.Regfx(icol))THEN
            Regfx(icol)=T
            IF(.not.erregm)erregm=T
           END IF
          END DO
         END IF
         ngrp1=ngrp1+1
         IF(.not.regchg)regchg=T
        ELSE IF(begrgm.ge.sspos.AND.
     &         ((Fulltd.AND.(Rgvrtp(begcol).eq.PRRTTD.or.
     &          Rgvrtp(begcol).eq.PRRTST.or.Rgvrtp(begcol).eq.PRR1TD.or.
     &          Rgvrtp(begcol).eq.PRR1ST)).or.
     &         (Lseff.AND.(Rgvrtp(begcol).eq.PRRTSE.or.
     &                     Rgvrtp(begcol).eq.PRRTTS)).or.
     &         (Fullln.AND.(Rgvrtp(begcol).eq.PRRTLM.or.
     &        Rgvrtp(begcol).eq.PRRTLQ.or.Rgvrtp(begcol).eq.PRRTSL)).or.
     &         (Fulllp.and.Rgvrtp(begcol).eq.PRRTLY)))THEN
         CALL insstr(igrptl(1:nchr),ngrp2,PB,grchr2,gr2ptr,ngr2tl)
         IF(Lfatal)RETURN
         IF(Ssinit.ne.1)THEN
          DO icol=begcol,endcol
           IF(.not.Regfx(icol))THEN
            Regfx(icol)=T
            IF(.not.erregm)erreg2=T
           END IF
          END DO
         END IF
         ngrp2=ngrp2+1
         IF(.not.regchg)regchg=T
        END IF
       ELSE IF(Rgvrtp(begcol).eq.PRATST.or.Rgvrtp(begcol).eq.PRATTD.or.
     &    Rgvrtp(begcol).eq.PRATSE.or.Rgvrtp(begcol).eq.PRATTS.or.
     &    Rgvrtp(begcol).eq.PRATLM.or.Rgvrtp(begcol).eq.PRATLQ.or.
     &    Rgvrtp(begcol).eq.PRATLY.or.Rgvrtp(begcol).eq.PRATSL.or.
     &    Rgvrtp(begcol).eq.PRA1ST.or.Rgvrtp(begcol).eq.PRA1TD)THEN
        CALL getstr(Grpttl,Grpptr,Ngrp,igrp,igrptl,nchr)
        IF(Lfatal)RETURN
        idtpos=index(igrptl(1:nchr),'(starting ')+10
        CALL ctodat(igrptl(1:nchr-1),Sp,idtpos,regmdt,Locok)
        CALL dfdate(enda,regmdt,Sp,begrgm)
        IF(begrgm.le.Sp)THEN
         CALL insstr(igrptl(1:nchr),ngrp1,PB,grchr1,gr1ptr,ngr1tl)
         IF(Lfatal)RETURN
         IF(Ssinit.ne.1)THEN
          DO icol=begcol,endcol
           IF(.not.Regfx(icol))THEN
            Regfx(icol)=T
            IF(.not.erregm)erregm=T
           END IF
          END DO
         END IF
         ngrp1=ngrp1+1
         IF(.not.regchg)regchg=T
        ELSE IF(begrgm.ge.(sspos-Sp).AND.
     &        ((Fulltd.AND.(Rgvrtp(begcol).eq.PRATST.or.
     &          Rgvrtp(begcol).eq.PRATTD.or.Rgvrtp(begcol).eq.PRA1TD.or.
     &          Rgvrtp(begcol).eq.PRA1ST)).or.
     &         (Lseff.AND.(Rgvrtp(begcol).eq.PRATSE.or.
     &                     Rgvrtp(begcol).eq.PRATTS)).or.
     &         (Fullln.AND.(Rgvrtp(begcol).eq.PRATLM.or.
     &        Rgvrtp(begcol).eq.PRATLQ.or.Rgvrtp(begcol).eq.PRATSL)).or.
     &         (Fulllp.and.Rgvrtp(begcol).eq.PRATLY)))THEN
         CALL insstr(igrptl(1:nchr),ngrp2,PB,grchr2,gr2ptr,ngr2tl)
         IF(Lfatal)RETURN
         IF(Ssinit.ne.1)THEN
          DO icol=begcol,endcol
           IF(.not.Regfx(icol))THEN
            Regfx(icol)=T
            IF(.not.erregm)erreg2=T
           END IF
          END DO
         END IF
         ngrp2=ngrp2+1
         IF(.not.regchg)regchg=T
        END IF
c-----------------------------------------------------------------------
c     Check regular outliers to see if they are defined within the
c     sliding spans.
c-----------------------------------------------------------------------
       ELSE IF(Rgvrtp(begcol).eq.PRGTAO.or.Rgvrtp(begcol).eq.PRGTLS.or.
     &         Rgvrtp(begcol).eq.PRGTRP.or.Rgvrtp(begcol).eq.PRGTTC.or.
     &         Rgvrtp(begcol).eq.PRGTSO.or.Rgvrtp(begcol).eq.PRGTTL.or.
     &         Rgvrtp(begcol).eq.PRGTQD.or.Rgvrtp(begcol).eq.PRGTQI)THEN
        DO icol=endcol,begcol,-1
         CALL rmotss(icol,Begxy,Nrxy,begss,starta,enda,Botr,Otrptr,
     &               Notrtl,Fixotr,Otrttl,Otlfix.or.Ssinit.eq.1,regchg)
        END DO
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Check automatic outliers to see if they are defined within the
c     sliding spans.
c-----------------------------------------------------------------------
*       ELSE IF(Rgvrtp(begcol).eq.PRGTAA.or.Rgvrtp(begcol).eq.PRGTAL.or.
*     &         Rgvrtp(begcol).eq.PRGTAT.or.Rgvrtp(begcol).eq.PRGTAS)THEN
       ELSE IF(Rgvrtp(begcol).eq.PRGTAA.or.Rgvrtp(begcol).eq.PRGTAL.or.
     &         Rgvrtp(begcol).eq.PRGTAT)THEN
        icol=endcol
        DO WHILE (icol.ge.begcol)
         IF(Ssotl.eq.1)THEN
          IF(Rgvrtp(icol).eq.PRGTAA)Rgvrtp(icol)=PRGTAO
          IF(Rgvrtp(icol).eq.PRGTAL)Rgvrtp(icol)=PRGTLS
          IF(Rgvrtp(icol).eq.PRGTAT)Rgvrtp(icol)=PRGTTC
*          IF(Rgvrtp(icol).eq.PRGTAS)Rgvrtp(icol)=PRGTSO
          CALL rmotss(icol,Begxy,Nrxy,begss,starta,enda,Botr,Otrptr,
     &                Notrtl,Fixotr,Otrttl,Otlfix.or.Ssinit.eq.1,regchg)
          IF(Lfatal)RETURN
         ELSE
          CALL dlrgef(icol,Nrxy,1)
          IF(Lfatal)RETURN
         END IF
         icol=icol-1
         IF(.not.regchg)regchg=T
        END DO
       END IF         
      END DO
c-----------------------------------------------------------------------
c     Print out warning message(s) for change-of-regime variables.
c-----------------------------------------------------------------------
      IF(ngr1tl.gt.0)THEN
       CALL nWritln('The following change of regime regression '//
     &              'variables are not',Mt1,Mt2,T,F)
       CALL writln('      defined for at least one year of one of '//
     &             'the spans of the',Mt1,Mt2,F,F)
       CALL writln('      sliding spans analysis:',Mt1,Mt2,F,T)
       DO igrp=1,ngr1tl
        CALL getstr(grchr1,gr1ptr,ngrp1,igrp,igrptl,nchr)
        IF(Lfatal)RETURN
        IF(igrp.eq.1)THEN
         IF(igrp.eq.ngr1tl)THEN
          CALL writln(igrptl(1:nchr),Mt1,Mt2,T,T)
         ELSE
          CALL writln(igrptl(1:nchr),Mt1,Mt2,T,F)
         END IF
        ELSE
         IF(igrp.eq.ngr1tl)THEN
          CALL writln(igrptl(1:nchr),Mt1,Mt2,F,T)
         ELSE
          CALL writln(igrptl(1:nchr),Mt1,Mt2,F,F)
         END IF
        END IF
       END DO
       IF(erregm)
     &    CALL writln('     Change of regime regressor will be fixed.',
     &                Mt1,Mt2,T,T)
      END IF
      IF(ngr2tl.gt.0)THEN
       CALL nWritln('The following change of regime regression '//
     &              'variables could cause',Mt1,Mt2,T,F)
       CALL writln('      singularity problems in the regression '//
     &             'matrix for at least one',Mt1,Mt2,F,F)
       CALL writln('      of the spans of the sliding spans analysis:',
     &             Mt1,Mt2,F,T)
       DO igrp=1,ngr2tl
        CALL getstr(grchr2,gr2ptr,ngrp2,igrp,igrptl,nchr)
        IF(Lfatal)RETURN
        IF(igrp.eq.1)THEN
         IF(igrp.eq.ngr1tl)THEN
          CALL writln(igrptl(1:nchr),Mt1,Mt2,T,T)
         ELSE
          CALL writln(igrptl(1:nchr),Mt1,Mt2,T,F)
         END IF
        ELSE
         IF(igrp.eq.ngr1tl)THEN
          CALL writln(igrptl(1:nchr),Mt1,Mt2,F,T)
         ELSE
          CALL writln(igrptl(1:nchr),Mt1,Mt2,F,F)
         END IF
        END IF
       END DO
       IF(erreg2)
     &    CALL writln('     Change of regime regressor will be fixed.',
     &                Mt1,Mt2,T,T)
      END IF
c-----------------------------------------------------------------------
c     If change-of-regime regression variables check out, set be sure
c     model parameters are fixed.
c-----------------------------------------------------------------------
      IF(Ssinit.ne.1)THEN
c       IF(Lrgmse.or.Lrgmtd.or.Lrgmln)THEN
c        Ssinit=1
c        CALL writln('NOTE: Since change of regime regression variables a
c     &re used, model',Mt1,Mt2,T)
c        CALL writln('      parameters will be held fixed.',STDERR,Mt2,F)
c       END IF
c-----------------------------------------------------------------------
c     If a model span is used, be sure model parameters are fixed.
c-----------------------------------------------------------------------
       CALL dfdate(Begmdl,Begspn,Sp,nbeg)
       CALL dfdate(Endspn,Endmdl,Sp,nend)
       IF(nbeg.gt.0.or.nend.gt.0)THEN
        Ssinit=1
        IF(Itd.eq.1.and.Adjtd.gt.0)Itd=-1
        IF(Ihol.eq.1.and.Adjhol.gt.0)Ihol=-1
        CALL nWritln('Since a model span is used, model parameters '//
     &               'will be held fixed.',fhnote,Mt2,T,T)
       END IF
      END IF
c-----------------------------------------------------------------------
c     Fix all model parameters based on value of Ssinit
c-----------------------------------------------------------------------
      IF(Ssinit.eq.1)THEN
       CALL copy(Arimap,PARIMA,1,Ap2)
       CALL setlg(T,PARIMA,Fxa)
       IF(.not.regchg)CALL copy(B,PB,1,Bb)
       CALL setlg(T,PB,Regfx2)
       IF(Iregfx.lt.3)Iregfx=3
       CALL setlg(T,Nb,Regfx)
       Irfx2=3
       IF(.not.Userfx)Userfx=Ncusrx.gt.0
       IF(Userfx)CALL bakusr(Userx,Usrtyp,Usrptr,Ncusrx,Usrttl,Regfx,B,
     &                       Rgvrtp,Ngrp,Grpttl,Grp,Grpptr,Ngrptl,0,T)
      END IF
c-----------------------------------------------------------------------
c     If outlier regressors have been changed, store new regression
c     variables 
c-----------------------------------------------------------------------
      IF(regchg)THEN
       Ngr2=Ngrp
       Ngrt2=Ngrptl
       Ncxy2=Ncxy
       Nbb=Nb
       Nct2=Ncoltl
       Cttl=Colttl
       Gttl=Grpttl
       CALL cpyint(Colptr(0),PB+1,1,Clptr(0))
       CALL cpyint(Grp(0),PGRP+1,1,G2(0))
       CALL cpyint(Grpptr(0),PGRP+1,1,Gptr(0))
       CALL cpyint(Rgvrtp,PB,1,Rgv2)
       CALL copy(B,PB,1,Bb)
       Irfx2=Iregfx
       CALL copylg(Regfx,PB,1,Regfx2)
      END IF
c----------------------------------------------------------------------- 
      RETURN
      END
