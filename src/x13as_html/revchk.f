C     Last change:  BCM  15 Apr 2005   12:41 pm
      SUBROUTINE revchk(Irev,Irevsa,Ixreg,Ny,Lfda,Llda,Ltmax,Nspobs,
     &                  Begspn,Endspn,Begmdl,Lx11,Lseats,Lmodel,Lnoprt,
     &                  Iagr,Ncomp,Fctdrp,Lr1y2y,Revsa,Revmdl,Fhnote,
     &                  Khol,Kfulsm)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Check settings for revisons history analysis
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'rev.prm'
      INCLUDE 'rev.cmn'
      INCLUDE 'revtrg.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'x11log.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11reg.cmn'
*      INCLUDE 'seatcm.cmn'
      INCLUDE 'seatlg.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'revtbl.i'
      INCLUDE 'error.cmn'
      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
      LOGICAL F,T
      INTEGER LRVR1S,LRVR1A,LRVR2S,LRVR2A,LRVR3S,LRVR3A,LRVR4S,
     &        LRVR4A,LRVR5S,LRVR5A,LRVR6S,LRVR6A,LRVR8A,MINSPN,MINYR
      PARAMETER(F=.false.,T=.true.,LRVR1S=LREVR1+1,MINSPN=60,MINYR=5,
     &          LRVR1A=LREVR1+2,LRVR2S=LREVR2+1,LRVR2A=LREVR2+2,
     &          LRVR3S=LREVR3+1,LRVR3A=LREVR3+2,LRVR4S=LREVR4+1,
     &          LRVR4A=LREVR4+2,LRVR5S=LREVR5+1,LRVR5A=LREVR5+2,
     &          LRVR6S=LREVR6+1,LRVR6A=LREVR6+2,LRVR8A=LREVR8+1)
c-----------------------------------------------------------------------
      CHARACTER str*(10),datstr*(10)
      LOGICAL Lx11,Lseats,Lmodel,Lnoprt,Revsa,Revmdl,Lr1y2y,nosarv,
     &        usstrt,isFixed,tdfix
      INTEGER Begspn,Endspn,Begmdl,Fctdrp,Irev,Ixreg,Lfda,Llda,Ny,
     &        Nspobs,nyrev,Ltmax,i,i2,strtyr,Iagr,imdl,idate,ndmdl,
     &        nchr,nchdat,ndx11,n1,Ncomp,Fhnote,Irevsa,Khol,Kfulsm,
     &        icol,ntd,begopr,endopr,rtype,iusr
      DIMENSION Begspn(2),Endspn(2),Begmdl(2),idate(2),strtyr(-1:5)
c-----------------------------------------------------------------------
c     strtyr - number of years in starting period for each type of
c              seasonal filter.
c-----------------------------------------------------------------------
      DATA strtyr/6,5,6,8,12,18,6/
c-----------------------------------------------------------------------
      Lr1y2y=F
      usstrt=Rvstrt(1).gt.0
c-----------------------------------------------------------------------
      IF(Lseats.and.(((Lrvsa.or.Lrvch).and.(.not.Havesa)).or.
     &   ((Lrvtrn.or.Lrvtch).and.(.not.Havetr)).or.
     &   (Lrvsf.and.(.not.Havesf))))THEN
       CALL wWritln('History analysis for estimates derived from '//
     &              'SEATS adjustments',STDERR,Mt2,T,F)
       CALL writln('         cannot be done when SEATS cannot perform'// 
     &             ' a signal extraction.',STDERR,Mt2,F,T)
       Irev=0
       Irevsa=-1
       IF(Indrev.gt.0)Indrev=0
       RETURN
      END IF
c-----------------------------------------------------------------------
c     If seasonal adjustment not done, set logical indicators for s.a.
c     revisions estimates (s.a. series, sf, changes) to false and print
c     warning message.
c-----------------------------------------------------------------------
      IF(.not.(Lx11.or.Lseats))THEN
       IF(Lrvsa)THEN
        CALL eWritln('Cannot calculate revision statistics for seas'//
     &               'onally adjusted data',STDERR,Mt2,T,F)
        CALL writln('       if seasonal adjustment is not specified'//
     &              ' via x11 or seats spec.',STDERR,Mt2,F,T)
        Lrvsa=F
        IF(Indrev.gt.0)Indrev=0
       END IF
c-----------------------------------------------------------------------
       IF(Lrvsf)THEN
        CALL eWritln('Cannot calculate revision statistics for seas'//
     &               'onal factors',STDERR,Mt2,T,F)
        CALL writln('       if seasonal adjustment is not specified'//
     &              ' via x11 or seats spec.',STDERR,Mt2,F,T)
        Lrvsf=F
       END IF
c-----------------------------------------------------------------------
       IF(Lrvch)THEN
        CALL eWritln('Cannot calculate revision statistics for '//
     &               'changes in the adjusted data',STDERR,Mt2,T,F)
        CALL writln('       if seasonal adjustment is not specified'//
     &               ' via x11 or seats spec.',STDERR,Mt2,F,T)
        Lrvch=F
       END IF
c-----------------------------------------------------------------------
       IF(Lrvtrn)THEN
        CALL eWritln('Cannot calculate revision statistics for trend '//
     &               'component',STDERR,Mt2,T,F)
        CALL writln('       if seasonal adjustment is not specified'//
     &              ' via x11 or seats spec.',STDERR,Mt2,F,T)
        Lrvtrn=F
       END IF
c-----------------------------------------------------------------------
       IF(Lrvtch)THEN
        CALL eWritln('Cannot calculate revision statistics for '//
     &               'changes in the trend',STDERR,Mt2,T,F)
        CALL writln('       if seasonal adjustment is not specified'//
     &              ' via x11 or seats spec.',STDERR,Mt2,F,T)
        Lrvtch=F
       END IF
      ELSE IF(Lseats)THEN
       IF(Lrvsa.and.(.not.Havesa))THEN
        CALL eWritln('Cannot calculate revision statistics for '//
     &               'seasonally adjusted data',STDERR,Mt2,T,F)
        CALL writln('       if seasonal adjustment is not performed '// 
     &              'during SEATS analysis.',STDERR,Mt2,F,T)
        Lrvsa=F
        IF(Indrev.gt.0)Indrev=0
       END IF
c-----------------------------------------------------------------------
       IF(Lrvsf.and.(.not.Havesf))THEN
        CALL eWritln('Cannot calculate revision statistics for '//
     &              'seasonal factors',STDERR,Mt2,T,F)
        CALL writln('       if seasonal adjustment is not performed '// 
     &              'during SEATS analysis.',STDERR,Mt2,F,T)
        Lrvsf=F
       END IF
c-----------------------------------------------------------------------
       IF(Lrvch.and.(.not.Havesa))THEN
        CALL eWritln('Cannot calculate revision statistics for '//
     &               'changes in the adjusted data',STDERR,Mt2,T,F)
        CALL writln('       if seasonal adjustment is not performed '//
     &              'during SEATS analysis.',STDERR,Mt2,F,T)
        Lrvch=F
       END IF
c-----------------------------------------------------------------------
       IF(Lrvtrn.and.(.not.Havetr))THEN
        CALL eWritln('Cannot calculate revision statistics for trend'//
     &               ' component',STDERR,Mt2,T,F)
        CALL writln('       if trend component not estimated during '// 
     &              'SEATS analysis.',STDERR,Mt2,F,T)
        Lrvtrn=F
       END IF
c-----------------------------------------------------------------------
       IF(Lrvtch.and.(.not.Havetr))THEN
        CALL eWritln('Cannot calculate revision statistics for '//
     &               'changes in the trend',STDERR,Mt2,T,F)
        CALL writln('       if trend component not estimated during '// 
     &              'SEATS analysis.',STDERR,Mt2,F,T)
        Lrvtch=F
       END IF
      END IF
c-----------------------------------------------------------------------
c     If regARIMA modelling not done, set logical indicators for s.a.
c     revisions estimates (s.a. series, sf, changes) to false and print
c     warning message.
c-----------------------------------------------------------------------
      IF(.not.Lmodel)THEN
       IF(Prttab(LREVOT))Prttab(LREVOT)=F
       IF(Lrvaic)THEN
        CALL eWritln('Cannot calculate revision statistics for '//
     &               'likelihood statistics',STDERR,Mt2,T,F)
        CALL writln('       if regARIMA modelling is not specified.',
     &              STDERR,Mt2,F,T)
        Lrvaic=F
       END IF
c-----------------------------------------------------------------------
       IF(Lrvfct)THEN
        CALL eWritln('Cannot calculate revision statistics for '//
     &               'forecasts if no forecasts',STDERR,Mt2,T,F)
        CALL writln('       are specified.',STDERR,Mt2,F,T)
        Lrvfct=F
       END IF
c-----------------------------------------------------------------------
       IF(Lrvarma)THEN
        CALL eWritln('Cannot calculate revision statistics for '//
     &               'ARMA parameters if no',STDERR,Mt2,T,F)
        CALL writln('       ARIMA model is specified.',STDERR,Mt2,F,T)
        Lrvarma=F
       END IF
c-----------------------------------------------------------------------
       IF(Lrvtdrg)THEN
        CALL eWritln('Cannot calculate revision statistics for'//
     &               ' trading day ',STDERR,Mt2,T,F)
        CALL writln(
     &     '       coefficients if no ARIMA model is specified.',
     &     STDERR,Mt2,F,T)
        Lrvtdrg=F
       END IF
c-----------------------------------------------------------------------
c     If projected seasonal factors are analysed and the number of
c     forecasts are > 0 and < Ny, do not perform projected seasonal 
c     factor revisions history analysis
c-----------------------------------------------------------------------
      ELSE IF(Nfcst.gt.0.and.Nfcst.lt.Ny.and.Lrvsf)THEN
       CALL wWritln('Cannot calculate revision statistics for '//
     &              'projected seasonal',Fhnote,Mt2,T,F)
       CALL writln('         factors unless either zero forecasts or '//
     &             'at least one year of',Fhnote,Mt2,F,F)
       CALL writln('         forecasts are specified.',Fhnote,Mt2,F,T)
       Lrvsf=F
c-----------------------------------------------------------------------
      ELSE IF(Lrvarma)THEN
       isFixed=T
       IF(.not.Revfix)THEN
        DO i=AR,MA
         begopr=Mdl(i-1)
         endopr=Mdl(i)-1
         DO i2=begopr,endopr
          isFixed=isFixed.and.Arimaf(i)
         END DO
        END DO
       END IF
       IF(isFixed)THEN
        CALL eWritln('Cannot calculate revision statistics for '//
     &               'ARMA parameters if all',STDERR,Mt2,T,F)
        IF(Revfix)THEN
         CALL writln('       coefficients in regARIMA model is '//
     &               'fixed (fixmdl=yes).',STDERR,Mt2,F,T)
        ELSE
         CALL writln('       the ARIMA model coefficients are fixed.',
     &               STDERR,Mt2,F,T)
        END IF
        Lrvarma=F
       END IF
c-----------------------------------------------------------------------
      ELSE IF(Lrvtdrg)THEN
       isFixed=T
       DO icol=1,Nb
         rtype=Rgvrtp(icol)
         IF(Nusrrg.gt.0)THEN
          IF(rtype.eq.PRGTUD)THEN
           rtype=Usrtyp(iusr)
           iusr=iusr+1
          ELSE IF((rtype.ge.PRGTUH.and.rtype.le.PRGUH5).or.
     &             rtype.eq.PRGTUS)THEN
           iusr=iusr+1
          END IF
         END IF
c-----------------------------------------------------------------------
c     regARIMA trading day regressors
c-----------------------------------------------------------------------
         IF((rtype.eq.PRGTTD.or.rtype.eq.PRGTST.or.rtype.eq.PRRTTD.or.
     &      rtype.eq.PRRTST.or.rtype.eq.PRATTD.or.rtype.eq.PRATST.or.
     &      rtype.eq.PRG1TD.or.rtype.eq.PRR1TD.or.rtype.eq.PRA1TD.or.
     &      rtype.eq.PRG1ST.or.rtype.eq.PRR1ST.or.rtype.eq.PRA1ST).or.
     &      (rtype.eq.PRGTLM.or.rtype.eq.PRGTSL.or.rtype.eq.PRGTLQ.or.
     &      rtype.eq.PRGTLY.or.rtype.eq.PRRTLM.or.rtype.eq.PRRTSL.or.
     &      rtype.eq.PRRTLQ.or.rtype.eq.PRRTLY.or.rtype.eq.PRATLM.or.
     &      rtype.eq.PRATSL.or.rtype.eq.PRATLQ.or.rtype.eq.PRATLY).or.
     &     (rtype.eq.PRGUTD.or.rtype.eq.PRGULM.or.rtype.eq.PRGULQ.or.
     &      rtype.eq.PRGULY))THEN
          Ntd=Ntd+1
          IF(.not.Revfix)isFixed=isFixed.and.Regfx(i)
         END IF
       END DO
       IF(Ntd.gt.0.and.Nrvfxr.gt.0)THEN
        tdfix=F
        DO i=1,Nrvfxr
         IF(Rvfxrg(i).eq.1)tdfix=T
        END DO
       END IF
       IF(ntd.eq.0)THEN
        CALL eWritln('Cannot calculate revision statistics for '//
     &               'trading day',STDERR,Mt2,T,F)
        CALL writln('       coefficients if no trading day regressors'//
     &              ' are specified.',STDERR,Mt2,F,T)
        Lrvtdrg=F
       ELSE IF(tdfix)THEN
        CALL eWritln('Cannot calculate revision statistics for '//
     &               'trading day',STDERR,Mt2,T,F)
        CALL writln('       coefficients if fixreg=td.',STDERR,Mt2,F,T)
        Lrvtdrg=F
       ELSE IF(isFixed)THEN
        CALL eWritln('Cannot calculate revision statistics for '//
     &               'trading day ',STDERR,Mt2,T,F)
        IF(Revfix)THEN
         CALL writln('       coefficients if regARIMA model is fixed'//
     &               ' (fixmdl=yes).',STDERR,Mt2,F,T)
        ELSE
         CALL writln('       coefficients if all trading day '// 
     &               'regressors are fixed.',STDERR,Mt2,F,T)
        END IF
        Lrvtdrg=F
       END IF
      END IF
c-----------------------------------------------------------------------
c     If revisions analysis is not selected for a particular estimate,
c     turn off the print and save indicators for that estimate.
c-----------------------------------------------------------------------
      IF(.not.Lrvsa.and.
     &   (Prttab(LREVR1).or.Prttab(LRVR1S).or.Prttab(LRVR1A).or.
     &    Savtab(LREVR1).or.Savtab(LRVR1S).or.Savtab(LRVR1A).or.
     &    Prttab(LREVR3).or.Prttab(LRVR3S).or.Prttab(LRVR3A).or.
     &    Savtab(LREVR3).or.Savtab(LRVR3S).or.Savtab(LRVR3A)))THEN
       IF(Prttab(LREVR1))Prttab(LREVR1)=F
       IF(Savtab(LREVR1))Savtab(LREVR1)=F
       IF(Prttab(LRVR1S))Prttab(LRVR1S)=F
       IF(Savtab(LRVR1S))Savtab(LRVR1S)=F
       IF(Prttab(LRVR1A))Prttab(LRVR1A)=F
       IF(Savtab(LRVR1A))Savtab(LRVR1A)=F
       IF(Prttab(LREVR3))Prttab(LREVR3)=F
       IF(Savtab(LREVR3))Savtab(LREVR3)=F
       IF(Prttab(LRVR3S))Prttab(LRVR3S)=F
       IF(Savtab(LRVR3S))Savtab(LRVR3S)=F
       IF(Prttab(LRVR3A))Prttab(LRVR3A)=F
       IF(Savtab(LRVR3A))Savtab(LRVR3A)=F
      END IF
c-----------------------------------------------------------------------
      IF(.not.Lrvch.and.
     &   (Prttab(LREVR2).or.Prttab(LRVR2S).or.Prttab(LRVR2A).or.
     &    Savtab(LREVR2).or.Savtab(LRVR2S).or.Savtab(LRVR2A)))THEN
       IF(Prttab(LREVR2))Prttab(LREVR2)=F
       IF(Savtab(LREVR2))Savtab(LREVR2)=F
       IF(Prttab(LRVR2S))Prttab(LRVR2S)=F
       IF(Savtab(LRVR2S))Savtab(LRVR2S)=F
       IF(Prttab(LRVR2A))Prttab(LRVR2A)=F
       IF(Savtab(LRVR2A))Savtab(LRVR2A)=F
      END IF
c-----------------------------------------------------------------------
      IF(.not.Lrvtrn.and.
     &   (Prttab(LREVR4).or.Prttab(LRVR4S).or.Prttab(LRVR4A).or.
     &    Savtab(LREVR4).or.Savtab(LRVR4S).or.Savtab(LRVR4A)))THEN
       IF(Prttab(LREVR4))Prttab(LREVR4)=F
       IF(Savtab(LREVR4))Savtab(LREVR4)=F
       IF(Prttab(LRVR4S))Prttab(LRVR4S)=F
       IF(Savtab(LRVR4S))Savtab(LRVR4S)=F
       IF(Prttab(LRVR4A))Prttab(LRVR4A)=F
       IF(Savtab(LRVR4A))Savtab(LRVR4A)=F
      END IF
c-----------------------------------------------------------------------
      IF(.not.Lrvtch.and.
     &   (Prttab(LREVR5).or.Prttab(LRVR5S).or.Prttab(LRVR5A).or.
     &    Savtab(LREVR5).or.Savtab(LRVR5S).or.Savtab(LRVR5A)))THEN
       IF(Prttab(LREVR5))Prttab(LREVR5)=F
       IF(Savtab(LREVR5))Savtab(LREVR5)=F
       IF(Prttab(LRVR5S))Prttab(LRVR5S)=F
       IF(Savtab(LRVR5S))Savtab(LRVR5S)=F
       IF(Prttab(LRVR5A))Prttab(LRVR5A)=F
       IF(Savtab(LRVR5A))Savtab(LRVR5A)=F
      END IF
c-----------------------------------------------------------------------
      IF(.not.Lrvsf.and.
     &   (Prttab(LREVR6).or.Prttab(LRVR6S).or.Prttab(LRVR6A).or.
     &    Savtab(LREVR6).or.Savtab(LRVR6S).or.Savtab(LRVR6A)))THEN
       IF(Prttab(LREVR6))Prttab(LREVR6)=F
       IF(Savtab(LREVR6))Savtab(LREVR6)=F
       IF(Prttab(LRVR6S))Prttab(LRVR6S)=F
       IF(Savtab(LRVR6S))Savtab(LRVR6S)=F
       IF(Prttab(LRVR6A))Prttab(LRVR6A)=F
       IF(Savtab(LRVR6A))Savtab(LRVR6A)=F
      END IF
c-----------------------------------------------------------------------
      IF(.not.Lrvaic.and.(Prttab(LREVR7).or.Savtab(LREVR7)))THEN
       IF(Prttab(LREVR7))Prttab(LREVR7)=F
       IF(Savtab(LREVR7))Savtab(LREVR7)=F
      END IF
c-----------------------------------------------------------------------
      IF(.not.Lrvfct.and.(Prttab(LREVR8).or.Savtab(LREVR8).or.
     &   Prttab(LRVR8A)))THEN
       IF(Prttab(LREVR8))Prttab(LREVR8)=F
       IF(Savtab(LREVR8))Savtab(LREVR8)=F
       IF(Prttab(LRVR8A))Prttab(LRVR8A)=F
      END IF
c-----------------------------------------------------------------------
      IF(.not.Lrvarma.and.(Prttab(LRVR9A).or.Savtab(LRVR9A)))THEN
       IF(Prttab(LRVR9A))Prttab(LRVR9A)=F
       IF(Savtab(LRVR9A))Savtab(LRVR9A)=F
      END IF
c-----------------------------------------------------------------------
      IF(.not.Lrvtdrg.and.(Prttab(LRVR9B).or.Savtab(LRVR9B)))THEN
       IF(Prttab(LRVR9B))Prttab(LRVR9B)=F
       IF(Savtab(LRVR9B))Savtab(LRVR9B)=F
      END IF
c-----------------------------------------------------------------------
c     If estimate selected for revisions analysis but no tables are
c     printed or stored, ensure that the revisions table will be printed
c     out.
c     comment out for Build 59 - 9/24/2021 if print=none, no tables are
c     printed
c-----------------------------------------------------------------------
c      IF(.not.Lnoprt)THEN
c       IF(Lrvsa)THEN
c        IF(.not.(Prttab(LREVR1).or.Prttab(LRVR1S).or.Prttab(LRVR1A).or.
c     &           Savtab(LREVR1).or.Savtab(LRVR1S).or.Savtab(LRVR1A)))
c     &     Prttab(LREVR1)=T
c       IF(Iagr.ge.5.and..not.
c     &    (Prttab(LREVR3).or.Prttab(LRVR3S).or.Prttab(LRVR3A).or.
c     &     Savtab(LREVR3).or.Savtab(LRVR3S).or.Savtab(LRVR3A)))
c     &     Prttab(LREVR3)=T
c       END IF
c       IF(Lrvch.and..not.
c     &   (Prttab(LREVR2).or.Prttab(LRVR2S).or.Prttab(LRVR2A).or.
c     &    Savtab(LREVR2).or.Savtab(LRVR2S).or.Savtab(LRVR2A)))
c     &    Prttab(LREVR2)=T
c       IF(Lrvtrn.and..not.
c     &   (Prttab(LREVR4).or.Prttab(LRVR4S).or.Prttab(LRVR4A).or.
c     &    Savtab(LREVR4).or.Savtab(LRVR4S).or.Savtab(LRVR4A)))
c     &    Prttab(LREVR4)=T
c       IF(Lrvtch.and..not.
c     &   (Prttab(LREVR5).or.Prttab(LRVR5S).or.Prttab(LRVR5A).or.
c     &    Savtab(LREVR5).or.Savtab(LRVR5S).or.Savtab(LRVR5A)))
c     &    Prttab(LREVR5)=T
c      IF(Lrvsf.and..not.
c     &   (Prttab(LREVR6).or.Prttab(LRVR6S).or.Prttab(LRVR6A).or.
c     &    Savtab(LREVR6).or.Savtab(LRVR6S).or.Savtab(LRVR6A)))
c     &    Prttab(LREVR6)=T
c       IF(Lrvaic.and..not.(Prttab(LREVR7).or.Savtab(LREVR7)))
c     &    Prttab(LREVR7)=T
c       IF(Lrvfct.and..not.(Prttab(LREVR8).or.Savtab(LREVR8).or.
c     &    Prttab(LRVR8A)))Prttab(LREVR8)=T
c       IF(Lrvarma.and..not.(Prttab(LRVR9A).or.Savtab(LRVR9A)))
c     &    Prttab(LRVR9A)=T
c       IF(Lrvtdrg.and..not.(Prttab(LRVR9B).or.Savtab(LRVR9B)))
c     &    Prttab(LRVR9B)=T
c      END IF
c-----------------------------------------------------------------------
c     Check to see if value of the forecast lag to be collected is
c     appropriate and if revisions of the forecasts should be collected.
c-----------------------------------------------------------------------
      IF(Lrvfct)THEN
       IF(Nfcst.eq.0)THEN
        CALL eWritln('Cannot calculate revision statistics for '//
     &               'forecasts if no forecasts',STDERR,Mt2,T,F)
        CALL writln('       are specified via the forecast spec.',
     &              STDERR,Mt2,F,T)
        Lrvfct=F
c-----------------------------------------------------------------------
c     If revisions are not collected of the forecasts, set Nfctlg=0
c-----------------------------------------------------------------------
        DO i=1,Nfctlg
         Rfctlg(i)=0
        END DO
        Nfctlg=0
c-----------------------------------------------------------------------
c     If the forecast lag has not been specified, set equal to 1.
c-----------------------------------------------------------------------
       ELSE IF(Nfctlg.eq.0)THEN
        Nfctlg=2
        Rfctlg(1)=1
        IF(Nfcst.ge.Ny)THEN
         Rfctlg(2)=Ny
        ELSE IF(Nfcst.gt.1)THEN
         Rfctlg(2)=Nfcst
        ELSE
         Nfctlg=1
        END IF
c-----------------------------------------------------------------------
c     If the forecast lag is greater than the number of forecasts,
c     print an error message.
c-----------------------------------------------------------------------
       ELSE
        DO i=1,Nfctlg
         IF(Rfctlg(i).gt.Nfcst)THEN
          CALL wWritln('Cannot utilize forecast lead for history that'//
     &                 ' is greater than',Fhnote,Mt2,T,F)
          CALL writln('         the number of forecasts (maxlead).',
     &                Fhnote,Mt2,F,T)
          CALL writln('         History analysis for forecasts will '//
     &                'not be done for this run.',Fhnote,Mt2,T,T)
          Lrvfct=F
         END IF
        END DO
        IF(Lrvfct)CALL intsrt(Nfctlg,Rfctlg)
       END IF
       IF(Fctdrp.gt.0)Fctdrp=0
      END IF
c-----------------------------------------------------------------------
c     If summary measures run being done, only allow seasonal 
c     adjustments to be tallied for composite run.
c-----------------------------------------------------------------------
      IF(Kfulsm.ge.1)THEN
       nosarv=Kfulsm.eq.1.or.(.not.(Axrgtd.or.Axrghl.or.Adjtd.eq.1.or.
     &        Adjhol.eq.1.or.Khol.gt.0))
       IF(Lrvsa)THEN
        IF(Iagr.eq.0.or.Iagr.ge.5)THEN
         IF(nosarv)THEN
          CALL eWritln('Cannot calculate revision statistics for '//
     &                 'seasonally adjusted data',STDERR,Mt2,T,F)
          IF(Kfulsm.eq.1)THEN
           CALL writln('       if a summary measures run is specified'//
     &                 ' in the x11 spec.',STDERR,Mt2,F,T)
          ELSE
           CALL writln('       if a trend estimation run is specified'//
     &                 ' in the x11 spec.',STDERR,Mt2,F,T)
          END IF
          Lrvsa=F
         END IF
        ELSE 
         IF(Prttab(LREVR1))Prttab(LREVR1)=F
         IF(Prttab(LRVR1S))Prttab(LRVR1S)=F
         IF(Prttab(LRVR1A))Prttab(LRVR1A)=F
         IF(Savtab(LREVR1))Savtab(LREVR1)=F
        END IF
       END IF
c-----------------------------------------------------------------------
       IF(Lrvsf)THEN
        CALL eWritln('Cannot calculate revision statistics for '//
     &               'seasonal factors',STDERR,Mt2,T,F)
        IF(Kfulsm.eq.1)THEN
         CALL writln('       if a summary measures run is specified '//
     &               'in the x11 spec.',STDERR,Mt2,F,T)
        ELSE
         CALL writln('       if a trend estimation run is specified '//
     &               'in the x11 spec.',STDERR,Mt2,F,T)
        END IF
        Lrvsf=F
       END IF
c-----------------------------------------------------------------------
       IF(Lrvch.and.nosarv)THEN
        CALL eWritln('Cannot calculate revision statistics for '//
     &               'changes in the adjusted data',STDERR,Mt2,T,F)
        IF(Kfulsm.eq.1)THEN
         CALL writln('       if a summary measures run is specified '//
     &               'in the x11 spec.',STDERR,Mt2,F,T)
        ELSE
         CALL writln('       if a trend estimation run is specified '//
     &               'in the x11 spec.',STDERR,Mt2,F,T)
        END IF
        Lrvch=F
       END IF
c-----------------------------------------------------------------------
       IF(Lrvtrn.and.Kfulsm.eq.1)THEN
        CALL eWritln('Cannot calculate revision statistics for trend'//
     &               ' component',STDERR,Mt2,T,F)
        CALL writln('       if a summary measures run is specified '//
     &              'in the x11 spec.',STDERR,Mt2,F,T)
        Lrvtrn=F
       END IF
c-----------------------------------------------------------------------
       IF(Lrvtch.and.Kfulsm.eq.1)THEN
        CALL eWritln('Cannot calculate revision statistics for '//
     &               'changes in the trend',STDERR,Mt2,T,F)
        CALL writln('       if a summary measures run is specified '//
     &              'in the x11 spec.',STDERR,Mt2,F,T)
        Lrvtch=F
       END IF
c-----------------------------------------------------------------------
c     If this is a composite analysis, make sure all components were
c     accounted for in the indirect adjustment.  If not, print out
c     warning message and do not compute revisions for indirect seasonal
c     adjustments.
c-----------------------------------------------------------------------
      ELSE IF(Iagr.ge.5.and.Ncomp.ne.Nrcomp)THEN
       nchr=1
       CALL itoc(Ncomp,str,nchr)
       IF(Lfatal)RETURN
       CALL nWritln('Composite seasonal adjustment performed with '//
     &              str(1:(nchr-1))//' components, ',
     &              Fhnote,Mt2,T,F)
       CALL writln('       but the indirect concurrent seasonal '//
     &             '       adjustments collected for',
     &              Fhnote,Mt2,F,F)
       nchr=1
       CALL itoc(Nrcomp,str,nchr)
       IF(Lfatal)RETURN
       CALL writln('       the revisions history analysis were '//
     &             'updated for only '//str(1:(nchr-1))//
     &             ' component(s).',Fhnote,Mt2,F,T)
       CALL writln('       Revisions histories of the indirect '//
     &             'seasonal adjustments will not',Fhnote,Mt2,T,F)
       CALL writln('       be produced.  Check for errors in the '//
     &             'revisions histories of the',Fhnote,Mt2,F,F)
       CALL writln('       components, and ensure that a history '//
     &             'spec is present in the',Fhnote,Mt2,F,F)
       CALL writln('       spec files of all the components.',
     &             Fhnote,Mt2,F,T)
       IF(Indrev.gt.0)Indrev=0
      END IF
c-----------------------------------------------------------------------
c     Check to see if there will be a revisions analysis on at least one
c     of the estimates.  If not, print out warning message.
c-----------------------------------------------------------------------
      IF(.not.(Lrvsa.or.Lrvsf.or.Lrvch.or.Lrvaic.or.Lrvfct.or.Lrvtrn.or.
     &   Lrvtch.or.Lrvarma.or.Lrvtdrg))THEN
       CALL wWritln('History analysis will not be performed for this'//
     &              ' run because',Fhnote,Mt2,T,F)
       CALL writln('         of error(s) indicated above.',
     &             Fhnote,Mt2,F,T)
       Irev=0
       IF(Irevsa.gt.0)Irevsa=-1
       IF(Indrev.gt.0)Indrev=0
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Set up starting period
c-----------------------------------------------------------------------
      Revsa=(Lrvsa.or.Lrvsf.or.Lrvch.or.Lrvtrn.or.Lrvtch)
      Revmdl=(Lrvfct.or.Lrvaic.or.Lrvarma.or.Lrvtdrg)
      IF(Rvstrt(MO).eq.0.and.Rvstrt(YR).eq.0)THEN
       IF(Revmdl)THEN
        imdl=8*Ny
        IF(Ny.eq.4)imdl=10*Ny
        IF(Revsa)THEN
         IF(imdl.ge.strtyr(Ltmax)*Ny)THEN
          CALL addate(Begspn,Ny,imdl,Rvstrt)
          IF(imdl.gt.strtyr(Ltmax))THEN
           IF (Lrvfct) THEN
            WRITE(Mt2,1010)'forecast'
           ELSE IF (Lrvaic) THEN
            WRITE(Mt2,1010)'<abbr title="Akaike information '//
     &                     'criterion">AIC</abbr>'
           ELSE IF (Lrvarma) THEN
            WRITE(Mt2,1010)'<abbr title="autoregressive moving '//
     &                     'average">ARMA</abbr> coefficients'
           ELSE IF (Lrvtdrg) THEN
            WRITE(Mt2,1010)'<abbr title="trading day">TD</abbr> '//
     &                     'coefficients'
           END IF
          END IF
         ELSE
          CALL addate(Begspn,Ny,strtyr(Ltmax)*Ny,Rvstrt)
          IF (Lrvfct) THEN
           WRITE(Mt2,1020)'forecast'
          ELSE IF (Lrvaic) THEN
           WRITE(Mt2,1020)'<abbr title="Akaike information '//
     &                    'criterion">AIC</abbr>'
          ELSE IF (Lrvarma) THEN
           WRITE(Mt2,1020)'<abbr title="autoregressive moving '//
     &                     'average">ARMA</abbr> coefficients'
          ELSE IF (Lrvtdrg) THEN
           WRITE(Mt2,1020)'<abbr title="trading day">TD</abbr> '//
     &                     'coefficients'
          END IF
         END IF
        ELSE
         CALL addate(Begspn,Ny,imdl,Rvstrt)
        END IF
       ELSE
        CALL addate(Begspn,Ny,strtyr(Ltmax)*Ny,Rvstrt)
       END IF
      ELSE 
       IF(Rvstrt(YR).lt.1900)Rvstrt(YR)=1900+Rvstrt(YR)
      END IF
c-----------------------------------------------------------------------
c     Set up ending period
c-----------------------------------------------------------------------
      IF(Rvend(MO).eq.0.and.Rvend(YR).eq.0)THEN
       CALL addate(Begspn,Ny,Nspobs-1,Rvend)
      ELSE
       IF(.not.Revsa)THEN
        CALL wWritln('Ending date of revisions valid only for '//
     &               'estimates derived from',Fhnote,Mt2,T,F)
        CALL writln('         seasonal adjustment (seasonally '//
     &              'adjusted data, seasonal factors',Fhnote,Mt2,F,F)
        CALL writln('         trends, etc.).',Fhnote,Mt2,F,T)
        CALL writln('         Ending date of history analysis reset '//
     &              'to end of series.',Fhnote,Mt2,T,T)
        CALL addate(Begspn,Ny,Nspobs-1,Rvend)
       ELSE IF(Rvend(YR).lt.1900)THEN
        Rvend(YR)=1900+Rvend(YR)
       END IF 
      END IF
      IF(Irev.eq.2)CALL addate(Begspn,Ny,Nspobs-Ny,Rvend)
c-----------------------------------------------------------------------
c     Check to see if there will be enough data between the start of
c     the model span and the start of the revisons history loop.
c-----------------------------------------------------------------------
      IF(Lmodel)THEN
       CALL dfdate(Rvstrt,Begmdl,Ny,ndmdl)
       ndmdl=ndmdl+1 
       IF(ndmdl.lt.MINSPN)THEN
c-----------------------------------------------------------------------
c   IF the model is fixed, see if the number of effective observations
c   will be enough to evaluate the regARIMA model
c-----------------------------------------------------------------------
        IF(Revfix)THEN
         n1=Nintvl+Nextvl+1
         ndmdl=ndmdl-Nintvl
c-----------------------------------------------------------------------
c    If there are not enough effective observations, attempt to
c    change the starting point of the revisions history to
c    allow enough observations to evaluate the model
c-----------------------------------------------------------------------
         IF(ndmdl.lt.Nextvl)THEN
          nchr=1
          CALL itoc(n1,str,nchr)
          IF(Lfatal)RETURN
          CALL addate(Begmdl,Ny,n1,idate)
          CALL dfdate(Endspn,idate,Ny,ndmdl)
c-----------------------------------------------------------------------
c    check to see if the proposed starting date for the revisions 
c    history is within the span of data.  If so, update the
c    revisions history starting date.
c-----------------------------------------------------------------------
          IF(ndmdl.gt.0)THEN
           CALL cpyint(idate,2,1,Rvstrt)
c-----------------------------------------------------------------------
c    If the user specified the starting date for the revisions 
c    history, print out a message docmenting the change in the
c    revisions history starting date.
c-----------------------------------------------------------------------
           IF(usstrt)THEN
            CALL wrtdat(Rvstrt,Ny,datstr,nchdat)
            IF(Lfatal)RETURN
            CALL nWritln('The start of the history analysis has been '//
     &                   'advanced to '//datstr(1:nchdat),
     &                   Fhnote,Mt2,T,F)
            CALL writln('      to allow '//str(1:nchr-1)//
     &              ' observations between the start of the model span',
     &                  Fhnote,Mt2,F,F)
            CALL writln('      and the start of the history analysis.',
     &                  Fhnote,Mt2,F,T)
            usstrt=F
            IF(Indrev.gt.0)THEN
             CALL writln('      Due to this change, the program will '//
     &                   'not generate a history analysis',
     &                   Fhnote,Mt2,T,F)
             CALL writln('      of the indirect seasonal adjustments.',
     &                   Fhnote,Mt2,F,T)
             CALL writln('      Change the starting date for all '//
     &                   ' analysis to be '//datstr(1:nchdat),
     &                   Fhnote,Mt2,T,F)
             CALL writln('      or later.',Fhnote,Mt2,F,T)
             Indrev=0
            END IF
           END IF
          ELSE
c-----------------------------------------------------------------------
c    If the proposed starting date for the revisions 
c    history is not within the span of data, update the model span
c    starting date to be the beginning of the data span, and check if
c    there are enough observations to evaluate the regARIMA model.
c-----------------------------------------------------------------------
           CALL cpyint(Begspn,2,1,Begmdl)
           CALL dfdate(Rvstrt,Begmdl,Ny,ndmdl)
           ndmdl=ndmdl+1
c-----------------------------------------------------------------------
c    If there are not, print an error message and stop execution
c-----------------------------------------------------------------------
           IF(ndmdl.lt.n1)THEN
            CALL eWritln('There must be at least '//str(1:nchr-1)//
     &                   ' observations between the start of',
     &                   STDERR,Mt2,T,F)
            CALL writln('       the model span and the start of the '//
     &                  'history analysis when a',STDERR,Mt2,F,F)
            CALL writln('       '//Mdldsn(1:Nmddcr)//
     &                  ' ARIMA model is present.',STDERR,Mt2,F,T)
            IF(Indrev.gt.0)Indrev=0
            CALL abend
            RETURN
c-----------------------------------------------------------------------
c    If there are, print a message explaining what happened
c-----------------------------------------------------------------------
           ELSE
            CALL nWritln('regARIMA model span will be reset during '//
     &                   'the history analysis.',Fhnote,Mt2,T,T)
           END IF
          END IF
         END IF
        ELSE
c-----------------------------------------------------------------------
c   IF the model is not fixed, determine if there are more than MINSPN
c   observations between the start of the model span and the end of the
c   data span.
c-----------------------------------------------------------------------
         CALL addate(Begmdl,Ny,MINSPN,idate)
         CALL dfdate(Endspn,idate,Ny,ndmdl)
c-----------------------------------------------------------------------
c   IF there are, use this date as the start of the revisions history
c   analysis.
c-----------------------------------------------------------------------
         nchr=1
         CALL wrtdat(Rvstrt,Ny,datstr,nchdat)
         IF(.not.Lfatal)CALL itoc(MINSPN,str,nchr)
         IF(ndmdl.gt.0)THEN
          CALL cpyint(idate,2,1,Rvstrt)
c-----------------------------------------------------------------------
c    If the user specified the starting date for the revisions 
c    history, print out a message docmenting the change in the
c    revisions history starting date.
c-----------------------------------------------------------------------
          IF(usstrt)THEN
           CALL wrtdat(Rvstrt,Ny,datstr,nchdat)
           IF(Lfatal)RETURN
           CALL nWritln('The start of the history analysis has been '//
     &                  'advanced to '//datstr(1:nchdat),
     &                  Fhnote,Mt2,T,F)
           CALL writln('      to allow '//str(1:nchr-1)//
     &              ' observations between the start of the model span',
     &                 Fhnote,Mt2,F,F)
           CALL writln('      and the start of the history analysis.',
     &                 Fhnote,Mt2,F,T)
           usstrt=F
           IF(Indrev.gt.0)THEN
            CALL writln('      Due to this change, the program will '//
     &                  'not generate a history analysis',
     &                  Fhnote,Mt2,T,F)
            CALL writln('      of the indirect seasonal adjustments.',
     &                  Fhnote,Mt2,F,T)
            CALL writln('      Change the starting date for all '//
     &                  'history analysis to be '//datstr(1:nchdat),
     &                  Fhnote,Mt2,T,F)
            CALL writln('      or later.',Fhnote,Mt2,F,T)
            Indrev=0
           END IF
          END IF
         ELSE 
c-----------------------------------------------------------------------
c    If the proposed starting date for the revisions history is not
c    within the span of data, update the model span starting date to be
c    the beginning of the data span.
c-----------------------------------------------------------------------
          CALL dfdate(Begmdl,Begspn,Ny,ndmdl)
          IF(ndmdl.gt.0)CALL cpyint(Begspn,2,1,Begmdl)
          IF(.not.Revfix)THEN
           Revfix=T
           IF(ndmdl.gt.0)THEN
            CALL nWritln('Since the number of observations modeled '//
     &                   'is less than '//str(1:nchr-1)//',',
     &                   Fhnote,Mt2,T,F)
            CALL writln('      regARIMA model span will be reset and '//
     &                  'regARIMA model parameters',Fhnote,Mt2,F,F)
            CALL writln('      will be held fixed during the history '//
     &                  'analysis.',Fhnote,Mt2,F,T)
           ELSE
            CALL nWritln('Since the number of observations modeled '//
     &                   'is less than '//str(1:nchr-1)//',',
     &                   Fhnote,Mt2,T,F)
            CALL writln('      regARIMA model parameters will be '//
     &                  'held fixed during the ',Fhnote,Mt2,F,F)
            CALL writln('      history analysis.',Fhnote,Mt2,F,T)
           END IF
          END IF
         END IF
        END IF
       END IF
c-----------------------------------------------------------------------
c    If the regARIMA model is to be fixed, update model parameters 
c    once a year as for the 0.per convention, and print out a  
c    warning to this effect.
c-----------------------------------------------------------------------
       IF(Fixper.gt.0.and.Revfix)THEN
        Fixper=0
        CALL nWritln('regARIMA model parameters will not be '//
     &               're-estimated once a year',Fhnote,Mt2,T,F)
        CALL writln('      during the history analysis.',Fhnote,Mt2,F,T)
       END IF
c-----------------------------------------------------------------------
c    If the regARIMA model is to be updated once a year as for the 0.per
c    convention, turn off the model refresh option, and print out a  
c    warning to this effect.
c-----------------------------------------------------------------------
       IF(Fixper.gt.0.and.Lrfrsh)THEN
        Lrfrsh=F
        CALL nWritln('In order to allow parameter estimation to '//
     &               'occur only once a year,',Fhnote,Mt2,T,F)
        CALL writln('      the refresh option is ignored.',
     &              Fhnote,Mt2,F,T)
       END IF
      END IF
c-----------------------------------------------------------------------
c     Ensure there are at least 5 years of data when seasonal
c     adjustment is done.
c-----------------------------------------------------------------------
      IF(Lx11)THEN
       CALL dfdate(Rvstrt,Begspn,Ny,ndmdl)
       ndmdl=ndmdl+1
c-----------------------------------------------------------------------
c   determine if there are more than MINYR years between the start and
c   end of the data span.
c-----------------------------------------------------------------------
       IF(ndmdl.lt.MINYR*Ny)THEN
        CALL addate(Begspn,Ny,MINYR*Ny,idate)
        CALL dfdate(Endspn,idate,Ny,ndx11)
        nchr=1
        CALL itoc(MINYR,str,nchr)
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c   IF there are, use this date as the start of the revisions history
c   analysis.
c-----------------------------------------------------------------------
        IF(ndx11.gt.0)THEN
         CALL cpyint(idate,2,1,Rvstrt)
c-----------------------------------------------------------------------
c    If the user specified the starting date for the revisions 
c    history, print out a message docmenting the change in the
c    revisions history starting date.
c-----------------------------------------------------------------------
         IF(usstrt)THEN
          CALL wrtdat(Rvstrt,Ny,datstr,nchdat)
          IF(Lfatal)RETURN
          CALL eWritln('The start of the history analysis has been '//
     &                 'advanced to '//datstr(1:nchdat),
     &                 Fhnote,Mt2,T,F)
          CALL writln('      to allow '//str(1:nchr-1)//
     &        ' years between the start of the data span and the start',
     &                Fhnote,Mt2,F,F)
          CALL writln('      of the history analysis.',
     &                Fhnote,Mt2,F,T)
          usstrt=F
          IF(Indrev.gt.0)THEN
           CALL writln('      Due to this change, the program will '//
     &                 'not generate a history analysis',Fhnote,Mt2,T,F)
           CALL writln('      of the indirect seasonal adjustments.',
     &                 Fhnote,Mt2,F,T)
           CALL writln('      Change the starting date for all '//
     &                 'history analysis to be '//datstr(1:nchdat),
     &                 Fhnote,Mt2,T,F)
           CALL writln('      or later.',Fhnote,Mt2,F,T)
           Indrev=0
          END IF
         END IF
        ELSE
c-----------------------------------------------------------------------
c    If there are not enough observations, then check if history
c    analysis for forecasts and/or aic is being done.
c-----------------------------------------------------------------------
         IF(Revmdl)THEN
c-----------------------------------------------------------------------
c    If so, turn off history analysis for seasonal adjustment estimates
c    and print a warning message.          
c-----------------------------------------------------------------------
          IF(Lrvsa)THEN
           Lrvsa=F
           IF(Indrev.gt.0)Indrev=0
          END IF
          IF(Lrvch)Lrvch=F
          IF(Lrvtrn)Lrvtrn=F
          IF(Lrvtch)Lrvtch=F
          IF(Lrvsf)Lrvsf=F
          Revsa=F
          CALL nWritln('There must be at least '//str(1:nchr-1)//
     &                'years between the start of',Fhnote,Mt2,T,F)
          CALL writln('      the data span and the start of the '//
     &                'history analysis when seasonal',Fhnote,Mt2,F,F)
          CALL writln('      adjustment is performed.',
     &                Fhnote,Mt2,F,T)
          CALL writln('      History analysis of seasonal '//
     &                'adjustments, trends, and their',
     &                Fhnote,Mt2,T,F)
          CALL writln('      related changes are not performed.',
     &                Fhnote,Mt2,F,T)
         ELSE
c-----------------------------------------------------------------------
c    If not, print an error message and stop program execution
c-----------------------------------------------------------------------
          CALL eWritln('There must be at least '//str(1:nchr-1)//
     &                 ' years between the start of',STDERR,Mt2,T,F)
          CALL writln('       the data span and the start of the '//
     &                'history analysis when seasonal',STDERR,Mt2,F,F)
          CALL writln('       adjustment is performed.',
     &                STDERR,Mt2,F,T)
          If(Indrev.gt.0)Indrev=0
          CALL abend
          RETURN
         END IF
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
c     Check to see if there will be enough data between the start of
c     the span used for the irregular regression and the start of the
c     revisons history loop.
c-----------------------------------------------------------------------
      IF(Ixreg.gt.0)THEN
       CALL dfdate(Rvstrt,Begxrg,Ny,ndmdl)
       ndmdl=ndmdl+1
       IF(ndmdl.lt.MINSPN)THEN
c-----------------------------------------------------------------------
c   determine if there are more than MINSPN observations between the
c   start and end of the data span.
c-----------------------------------------------------------------------
        CALL addate(Begxrg,Ny,MINSPN,idate)
        CALL dfdate(Endspn,idate,Ny,ndmdl)
c-----------------------------------------------------------------------
c   IF there are, use this date as the start of the revisions history
c   analysis.
c-----------------------------------------------------------------------
        IF(ndmdl.gt.0)THEN
         nchr=1
         CALL cpyint(idate,2,1,Rvstrt)
c-----------------------------------------------------------------------
c    If the user specified the starting date for the revisions 
c    history, print out a message docmenting the change in the
c    revisions history starting date.
c-----------------------------------------------------------------------
         IF(usstrt)THEN
          CALL wrtdat(Rvstrt,Ny,datstr,nchdat)
          IF(.not.Lfatal)CALL itoc(MINSPN,str,nchr)
          IF(Lfatal)RETURN
          CALL wWritln('The start of the history analysis has been'//
     &                 ' changed to '//datstr(1:nchdat),Fhnote,Mt2,T,F)
          CALL writln('         to allow '//str(1:nchr-1)//
     &                ' observations between the start of the ',
     &                Fhnote,Mt2,F,F)
          CALL writln('         irregular regression and the start '//
     &               'of the history analysis.',Fhnote,Mt2,F,T)
          usstrt=F
          IF(Indrev.gt.0)THEN
           CALL writln('         Due to this change, the program '//
     &                 'will not generate a history analysis',
     &                 Fhnote,Mt2,T,F)
           CALL writln('         of the indirect seasonal adjustments.',
     &                 Fhnote,Mt2,F,T)
           CALL writln('         Change the starting date for all '//
     &                 'history analysis to be '//datstr(1:nchdat),
     &                 Fhnote,Mt2,T,F)
           CALL writln('         or later.',Fhnote,Mt2,F,T)
           Indrev=0
          END IF
         END IF
        ELSE 
c-----------------------------------------------------------------------
c    If the proposed starting date for the revisions history is not
c    within the span of data, update the starting date of the irregular
c    regression to be the beginning of the data span.
c-----------------------------------------------------------------------
         CALL cpyint(Begspn,2,1,Begxrg)
         IF(Revfxx)THEN
          CALL nWritln('Span for irregular regression will be reset.',
     &                Fhnote,Mt2,T,T)
         ELSE
c-----------------------------------------------------------------------
c    If irregular regression coefficients were to be estimated, have
c    irregular regression coefficients fixed.
c-----------------------------------------------------------------------
          Revfxx=T
          CALL nwritln('Span for irregular regression will be reset '//
     &                 'and the irregular regression',Fhnote,Mt2,T,F)
          CALL writln('      coefficients will be held fixed during '//
     &                'the history analysis.',Fhnote,Mt2,F,T)
         END IF
        END IF
       END IF
c-----------------------------------------------------------------------
       IF(Fxprxr.gt.0.and.Revfxx)THEN
        Fxprxr=0
        CALL nWritln('Irregular component regression parameters '//
     &               'will not be re-estimated',Fhnote,Mt2,T,F)
        CALL writln('      once a year during the revisions history '//
     &              'analysis.',Fhnote,Mt2,F,T)
       END IF
      END IF
c-----------------------------------------------------------------------
c     Check to see if there is enough data for revisions analysis
c-----------------------------------------------------------------------
      CALL dfdate(Rvend,Rvstrt,Ny,nyrev)
      IF(nyrev.lt.0)THEN
       IF(usstrt)THEN
        nchr=1
        CALL wrtdat(Rvstrt,Ny,datstr,nchr)
        IF(Lfatal)RETURN
        CALL nWritln('Not enough data to perform history analysis '//
     &               'starting in '//datstr(1:nchr)//'.',Fhnote,Mt2,T,F)
       ELSE
        CALL nWritln('Not enough data to perform history analysis '//
     &               'using default starting',Fhnote,Mt2,T,F)
        CALL writln('      date; use start argument in history spec '//
     &              'to override default.',Fhnote,Mt2,F,F)
       END IF
       CALL writln('      See '//SPCSEC//' of the '//PRGNAM//' '//
     &             DOCNAM//'.',Fhnote,Mt2,F,T)
       Irev=0
       IF(Irevsa.gt.0)Irevsa=-1
       IF(Indrev.gt.0)Indrev=0
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Check to see if all forecast lags are valid.
c-----------------------------------------------------------------------
      IF(Nfctlg.gt.0.and.Lrvfct)THEN
       DO i=Nfctlg,1,-1
        IF(nyrev.lt.Rfctlg(i))THEN
         nchr=1
         CALL itoc(Rfctlg(i),str,nchr)
         IF(Lfatal)RETURN
         IF(usstrt)THEN
          CALL wrtdat(Rvstrt,Ny,datstr,nchr)
          IF(Lfatal)RETURN
          CALL nWritln('Not enough data to perform forecast history '//
     &                 'analysis for lag '//str(1:(nchr-1)),
     &                 Fhnote,Mt2,F,F)
          CALL writln('       starting in '//datstr(1:nchr)//'.',
     &                Fhnote,Mt2,F,T)
          CALL writln('       See '//SPCSEC//' of the '//PRGNAM//' '//
     &                DOCNAM//'.',Fhnote,Mt2,T,T)
         ELSE
          CALL nWritln('Not enough data to perform forecast history '//
     &                 'analysis for lag '//str(1:(nchr-1)),
     &                 Fhnote,Mt2,F,F)
          CALL writln('       from default starting date.',
     &                Fhnote,Mt2,F,T)
          CALL writln('       See '//SPCSEC//' of the '//PRGNAM//' '//
     &                DOCNAM//'.',Fhnote,Mt2,T,T)
         END IF
         Rfctlg(i)=0
         Nfctlg=Nfctlg-1
        END IF
       END DO
       IF(Nfctlg.eq.0)THEN
        Irev=0
        RETURN
       END IF
      END IF
      CALL setrvp(Begspn,Ny,Lfda,Llda,Lmodel)
c-----------------------------------------------------------------------
c     If revision targets specified for seasonally adjusted series,
c     sort them and see if one and two year revision targets are 
c     specified.
c-----------------------------------------------------------------------
      IF(Ntarsa.gt.0)THEN
       CALL intsrt(Ntarsa,Targsa)
       i2=0
       DO i=Ntarsa,1,-1
        IF(nyrev.le.Targsa(i))THEN
         nchr=1
         CALL itoc(Targsa(i),str,nchr)
         IF(Lfatal)RETURN
         CALL nWritln('Not enough data to perform a history '//
     &                'analysis for seasonal adjustments',
     &                Fhnote,Mt2,T,F)
         IF(usstrt)THEN
          CALL wrtdat(Rvstrt,Ny,datstr,nchr)
          IF(Lfatal)RETURN
          CALL writln('       at lag '//str(1:(nchr-1))//
     &                ' starting in '//datstr(1:nchr)//'.',
     &                 Fhnote,Mt2,F,T)
         ELSE
          CALL writln('       at lag '//str(1:(nchr-1))//
     &                ' from default starting date.',
     &                 Fhnote,Mt2,F,T)
         END IF
         CALL writln('       See '//SPCSEC//' of the '//PRGNAM//' '//
     &               DOCNAM//'.',Fhnote,Mt2,T,T)
         Targsa(i)=0
         Ntarsa=Ntarsa-1
        ELSE
         IF(Targsa(i).eq.Ny.or.Targsa(i).eq.2*Ny)i2=i2+1
        END IF
       END DO
       Lr1y2y=i2.eq.2
      END IF
c-----------------------------------------------------------------------
c     If revision targets specified for trend component, sort them.
c-----------------------------------------------------------------------
      IF(Ntartr.gt.0)THEN
       CALL intsrt(Ntartr,Targtr)
       i2=0
       DO i=Ntartr,1,-1
        IF(nyrev.le.Targtr(i))THEN
         nchr=1
         CALL itoc(Targsa(i),str,nchr)
         IF(Lfatal)RETURN
         CALL nWritln('Not enough data to perform a history '//
     &                'analysis for trends',
     &                Fhnote,Mt2,T,F)
         IF(usstrt)THEN
          CALL wrtdat(Rvstrt,Ny,datstr,nchr)
          IF(Lfatal)RETURN
          CALL writln('       at lag '//str(1:(nchr-1))//
     &                ' starting in '//datstr(1:nchr)//'.',
     &                 Fhnote,Mt2,F,T)
         ELSE
          CALL writln('       at lag '//str(1:(nchr-1))//
     &                ' from default starting date.',
     &                 Fhnote,Mt2,F,T)
         END IF
         CALL writln('       See '//SPCSEC//' of the '//PRGNAM//' '//
     &               DOCNAM//'.',Fhnote,Mt2,T,T)
         Targtr(i)=0
         Ntartr=Ntartr-1
        ELSE
         IF(Targtr(i).eq.Ny.or.Targtr(i).eq.2*Ny)i2=i2+1
        END IF
       END DO
       Lr1y2y=i2.eq.2
      END IF
c-----------------------------------------------------------------------
 1010 FORMAT(' <p><strong>NOTE:</strong> The default starting date of ',
     &       'the ',a,' history analysis has been',/,
     &       ' used since it is later than the default starting date ',
     &       'determined by',/,
     &       ' the length of the maximum seasonal filter from the ',
     &       'seasonal adjustment.</p>')
 1020 FORMAT(' <p><strong>NOTE:</strong> The default starting date ',
     &       'determined by the length of the maximum',/,
     &       '       seasonal filter from the seasonal adjustment was ',
     &       'used since it is',/,
     &       '       later than the default starting date for the ',a,
     &       ' history analysis.</p>')
c-----------------------------------------------------------------------
      RETURN
      END
