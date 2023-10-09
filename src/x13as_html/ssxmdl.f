      SUBROUTINE ssxmdl(Begspn,Begss,Itd,Ihol,Tdfix,Holfix,Otlfix,
     &                  Usrfix)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'x11reg.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'xrgmdl.cmn'
      INCLUDE 'usrxrg.cmn'
      INCLUDE 'otxrev.cmn'
      INCLUDE 'x11log.cmn'
      INCLUDE 'sspinp.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c-----------------------------------------------------------------------
      LOGICAL Tdfix,Holfix,tdfx,holfx,Usrfix,regchg,Otlfix
      INTEGER Itd,Ihol,Begspn,Begss,starta,enda,igrp,begcol,endcol,icol,
     &        rtype,nbeg,nend,fhnote
      DIMENSION Begspn(2),Begss(2),starta(2),enda(2)
c-----------------------------------------------------------------------
      CALL dfdate(Begxrg,Begspn,Sp,nbeg)
      CALL dfdate(Endspn,Endxrg,Sp,nend)
      fhnote=STDERR
      IF(Lquiet)fhnote=0
      IF(nbeg.gt.0.or.nend.gt.0)THEN
       Ssxint=T
       CALL nWritln('Since a span is used in the x11regression spec, '//
     &              'the irregular ',fhnote,Mt2,T,F)
       CALL writln('      regression coefficient estimates will be '//
     &             'held fixed during the ',fhnote,Mt2,F,F)
       CALL writln('      sliding spans analysis.',fhnote,Mt2,F,F)
       IF(Itd.gt.0)Itd=-1
       IF(Ihol.gt.0)Ihol=-1
      END IF
c-----------------------------------------------------------------------
      CALL intlst(PB,Otxptr,Notxtl)
      regchg=F
      CALL loadxr(F)
c-----------------------------------------------------------------------
      IF((.not.Ssxotl).and.Otlxrg)THEN
       CALL addate(Begss,Sp,(Ncol-1)*Sp,starta)
       CALL addate(Endspn,Sp,(1-Ncol)*Sp,enda)
       DO igrp=Ngrp,1,-1
        begcol=Grp(igrp-1)
        endcol=Grp(igrp)-1
c-----------------------------------------------------------------------
c     Check regular outliers to see if they are defined within the
c     sliding spans.
c-----------------------------------------------------------------------
        IF(Rgvrtp(begcol).eq.PRGTAO)THEN
         DO icol=endcol,begcol,-1
          CALL rmotss(icol,Begxy,Nrxy,Begss,starta,enda,Botx,Otxptr,
     &                Notxtl,Fixotx,Otxttl,Otlfix.or.Ssxint,regchg)
          IF(Lfatal)RETURN
         END DO
c-----------------------------------------------------------------------
c     Check automatic outliers to see if they are defined within the
c     sliding spans.
c-----------------------------------------------------------------------
        ELSE IF(Rgvrtp(begcol).eq.PRGTAA)THEN
         icol=endcol
         DO WHILE (icol.ge.begcol)
          IF(Rgvrtp(icol).eq.PRGTAA)Rgvrtp(icol)=PRGTAO
          CALL rmotss(icol,Begxy,Nrxy,Begss,starta,enda,Botx,Otxptr,
     &                Notxtl,Fixotx,Otxttl,Otlfix.or.Ssxint,regchg)
          IF(Lfatal)RETURN
          icol=icol-1
         END DO
        END IF 
       END DO
      END IF
c-----------------------------------------------------------------------
      tdfx=F
      holfx=F
      IF(Nbx.gt.0)
     &   CALL rvfixd(Tdfix,Holfix,Otlfix,Usrfix,Irgxfx,Regfxx,Nbx,
     &               Rgxvtp,Nusxrg,Usxtyp,Nusxrg,Usrxfx)
      IF(Tdfix.and.Itd.gt.0)Itd=-1
      IF(Holfix.and.Ihol.gt.0)Ihol=-1
      IF(((Itd.eq.1.and.Axrgtd.and.(.not.Tdfix)).OR.
     &   (Ihol.eq.1.and.Axrghl.and.(.not.Holfix))).AND.Irgxfx.ge.2)THEN
       tdfx=T
       holfx=T
       IF(Irgxfx.eq.2)THEN
        DO igrp=1,Nxgrp
         endcol=Grpx(igrp)-1
         begcol=Grpx(igrp-1)
         rtype=Rgxvtp(begcol)
         IF((rtype.eq.PRGTTD.or.rtype.eq.PRGTST.or.rtype.eq.PRRTTD.or.
     &       rtype.eq.PRA1TD.or.rtype.eq.PRRTST.or.rtype.eq.PRATTD.or.
     &       rtype.eq.PRATST.or.rtype.eq.PRG1TD.or.rtype.eq.PRR1TD.or.
     &       rtype.eq.PRA1ST.or.rtype.eq.PRG1ST.or.rtype.eq.PRR1ST.or.
     &      (rtype.eq.PRGTLM.or.rtype.eq.PRGTSL.or.rtype.eq.PRGTLQ.or.
     &       rtype.eq.PRGTLY.or.rtype.eq.PRATLQ.or.rtype.eq.PRRTLM.or.
     &       rtype.eq.PRRTSL.or.rtype.eq.PRRTLQ.or.rtype.eq.PRRTLY.or.
     &       rtype.eq.PRATLM.or.rtype.eq.PRATSL.or.rtype.eq.PRATLY).or.
     &      (rtype.eq.PRGUTD.or.rtype.eq.PRGULM.or.rtype.eq.PRGULQ.or.
     &       rtype.eq.PRGULY))
     &  .or.(rtype.eq.PRGTEA.or.rtype.eq.PRGTEC.or.rtype.eq.PRGTES.or.
     &       rtype.eq.PRGTLD.or.rtype.eq.PRGTTH))THEN
          DO icol=begcol,endcol
           IF(rtype.eq.PRGTEA.or.rtype.eq.PRGTEC.or.rtype.eq.PRGTES
     &             .or.rtype.eq.PRGTLD.or.rtype.eq.PRGTTH.or.
     &             rtype.eq.PRGTUH)THEN
            holfx=holfx.and.Regfxx(icol)
           ELSE
            tdfx=tdfx.and.Regfxx(icol)
           END IF
          END DO
         END IF
        END DO
       END IF
c-----------------------------------------------------------------------
       IF(tdfx.and.Itd.gt.0)THEN
        Itd=-1
        IF(.not.Tdfix)THEN
         Nssfxx=Nssfxx+1
         Ssfxxr(Nssfxx)=1
        END IF
       END IF
       IF(holfx.and.Ihol.gt.0)THEN
        Ihol=-1
        IF(.not.Holfix)THEN
         Nssfxx=Nssfxx+1
         Ssfxxr(Nssfxx)=2
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
      CALL loadxr(T)
c-----------------------------------------------------------------------
c     Fix all model parameters based on value of Ssxint
c-----------------------------------------------------------------------
      IF(Ssxint)THEN
       CALL setlg(T,PB,Regfxx)
       IF(Irgxfx.lt.3)Irgxfx=3
       IF(.not.Usrxfx)THEN
        Usrxfx=Nusxrg.gt.0
        IF(Usrxfx)
     &     CALL bakusr(Xuserx,Usxtyp,Usrxpt,Nusxrg,Usrxtt,Regfxx,Bx,
     &                 Rgxvtp,Nxgrp,Grpttx,Grpx,Gpxptr,Ngrptx,1,T)
       END IF
      END IF
c-----------------------------------------------------------------------
c    Reset Lxrneg if td regressors results are fixed
c-----------------------------------------------------------------------
      IF(Lxrneg.and.(Irgxfx.eq.3.or.tdfx))Lxrneg=F
c-----------------------------------------------------------------------
      RETURN
      END
