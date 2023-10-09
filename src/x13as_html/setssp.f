C     Last change:  BCM  19 May 2003    2:29 pm
      SUBROUTINE setssp(Issap,Begspn,Pos1,Pos2,Ltmax,Lmodel,Lseats,
     &                  Lncset,Lnlset,Otlfix)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Set up sliding spans options
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'ssap.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'agr.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'ssap.cmn'
      INCLUDE 'sspinp.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'lzero.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'x11log.cmn'
      INCLUDE 'xrgmdl.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11opt.cmn'
c      INCLUDE 'model.cmn'
c-----------------------------------------------------------------------
      INTEGER MAXNL
      LOGICAL T,F
      PARAMETER(MAXNL=4,T=.true.,F=.false.)
c-----------------------------------------------------------------------
      CHARACTER numstr*(10)
      LOGICAL tdfix,holfix,Otlfix,usrfix,Lnlset,Lncset,mdlok,Lmodel,
     &        Lseats
      INTEGER i,Issap,nl,l2,Ltmax,ncmax,lm,begss,endss,Pos1,Pos2,frstsp,
     &        nmcomp,ssbak,ipos,ipos2,Begspn,pos0,lyr0,fhnote
      DIMENSION nl(-1:MAXNL),ssbak(2),frstsp(2),begss(2),endss(2),
     &          Begspn(2)
c-----------------------------------------------------------------------
      INTEGER mdssln
      EXTERNAL mdssln
c-----------------------------------------------------------------------
      DATA nl/6,6,7,8,11,17/
c-----------------------------------------------------------------------
      fhnote=STDERR
      IF(Lquiet)fhnote=0
      IF(Itd.ne.1.AND.(Axrgtd.OR.Adjtd.gt.0))Itd=1
      IF(Ihol.ne.1.AND.(Axrghl.or.Adjhol.gt.0))Ihol=1
      IF(Itd.eq.1.and.Adjtd.gt.0.and.Ssinit.eq.1)Itd=-1
      IF(Ihol.eq.1.and.Adjhol.gt.0.and.Ssinit.eq.1)Ihol=-1
      IF(Ihol.eq.1.and.(.not.Finhol))Ihol=0
      IF(Muladd.ne.1)THEN
       IF(Ssdiff)Ssdiff=F
       IF(Ssidif.and.Iagr.ge.5)Ssidif=F
      END IF
c-----------------------------------------------------------------------
c     Check to see if summary measures run is done; if so, only go on if
c     this is a component of a composite seasonal adjustment.
c-----------------------------------------------------------------------
      IF(Kfulsm.eq.1)THEN
       CALL wWritln('No seasonal adjustment has been done because '//
     &              'type=summary',fhnote,Mt2,T,F)
       CALL writln('         occurs in the x11 spec.  Therefore no'//
     &             ' sliding spans analysis',fhnote,Mt2,F,F)
       CALL writln('         has been done.',fhnote,Mt2,F,T)
       IF(Iagr.eq.2)THEN
        CALL writln('         The unadjusted series from this spec '//
     &              'file has been incorporated',fhnote,Mt2,T,F)
        CALL writln('         into the composite seasonal adjustment '//
     &              'for each of the sliding',fhnote,Mt2,F,F)
        CALL writln('         spans.',fhnote,Mt2,F,T)
       ELSE
        IF(Iagr.ge.0)CALL abend
        RETURN
       END IF
      END IF
c-----------------------------------------------------------------------
c     check to see if there is enough data for sliding spans
c-----------------------------------------------------------------------
      IF(Lncset.and.Lnlset)THEN
       IF(Length.LT.(Nlen+(Ncol-1)*Ny))THEN
        ipos=1
        CALL itoc(Ncol,numstr,ipos)
        ipos2=ipos
        CALL itoc(Nlen,numstr,ipos2)
        Issap=0
        CALL nWritln('Not enough data to produce '//
     &               numstr(1:(ipos-1))//' sliding spans of length '//
     &               numstr(ipos:(ipos2-1))//'.',fhnote,Mt2,T,T)
       END IF
c-----------------------------------------------------------------------
c     set length of sliding spans for stable seasonal.
c-----------------------------------------------------------------------
      ELSE IF(Ltmax.eq.5)THEN
       IF(.not.Lncset)THEN
        IF(Lnlset)THEN
         ncmax=((Length-Nlen)/Ny)+1
         IF(ncmax.ge.4)THEN
          Ncol=4
         ELSE IF(ncmax.le.1)THEN
          Issap=0
          CALL nWritln(
     &       'Not enough data to perform sliding spans analysis.',
     &       fhnote,Mt2,T,F)
          ipos=1
          CALL itoc(Nlen,numstr,ipos)
          CALL writln('      '//
     &         'Must be able to form at least two spans of length '//
     &         numstr(1:ipos-1)//'.',fhnote,Mt2,F,T)
         ELSE
          Ncol=ncmax
         END IF
        ELSE
         Ncol=4
        END IF
       END IF
       IF(.not.Lnlset)THEN
        Nlen=Length-(Ncol-1)*Ny
        IF(Nlen.gt.nl(MAXNL)*Ny)THEN
         Nlen=nl(MAXNL)*Ny
         IF(Lstmo.lt.Ny)Nlen=Nlen-Ny
        END IF
        IF(Nlen.lt.3*Ny)THEN
         IF(Lncset)THEN
          Issap=0
          ipos=1
          CALL itoc(Ncol,numstr,ipos)
          CALL nWritln('Not enough data to produce '//
     &                numstr(1:(ipos-1))//
     &                ' spans with at least 3 years of data.',
     &                fhnote,Mt2,T,T)
         ELSE
          Ncol=((Length-3*Ny)/Ny)+1
          IF(Ncol.le.1)THEN
           Issap=0
           CALL nWritln('Not enough data to perform sliding spans '//
     &                  'analysis.',fhnote,Mt2,T,F)
           CALL writln('      Must have at least 3 years of data in '//
     &                 'each span to perform',fhnote,Mt2,F,F)
           CALL writln('      sliding spans analysis for stable '//
     &                 'seasonal filters.',fhnote,Mt2,F,T)
          END IF
         END IF
         IF(Issap.gt.0)Nlen=Length-Ny*(Ncol-1)
        END IF
       END IF
      ELSE
c-----------------------------------------------------------------------
c     set length of sliding spans for other seasonal filter lengths, or
c     by the first order seasonal moving average parameter estimate if
c     SEATS seasonal adjustments are used.
c-----------------------------------------------------------------------
       IF(.not.Lnlset)THEN
        IF(Lseats)THEN
         Nlen=mdssln(Ny)
        ELSE
         Nlen=nl(Ltmax)*Ny
        END IF
       END IF
       ncmax=((Length-Nlen)/Ny)+1
       IF(Lncset)THEN
        IF(ncmax.lt.Ncol)THEN
         ipos=1
         CALL itoc(Ncol,numstr,ipos)
         ipos2=ipos
         CALL itoc(Nlen,numstr,ipos2)
         Issap=0
         CALL nWritln('Not enough data to produce '//
     &                numstr(1:(ipos-1))//' sliding spans of length '//
     &                numstr(ipos:(ipos2-1))//'.',fhnote,Mt2,T,T)
        END IF
       ELSE
        IF(ncmax.gt.4)THEN
         Ncol=4
        ELSE IF(ncmax.lt.2)THEN
         Issap=0
         CALL nWritln(
     &      'Not enough data to perform sliding spans analysis.',
     &      fhnote,Mt2,T,F)
         IF(Lnlset)THEN
          ipos=1
          CALL itoc(Nlen,numstr,ipos)
          CALL writln('      '//
     &         'Must be able to form at least two spans of length '//
     &         numstr(1:ipos-1)//'.',fhnote,Mt2,F,T)
         ELSE
          CALL writln(
     &       '      Must be able to form at least two sliding spans.',
     &       fhnote,Mt2,F,T)
         END IF
        ELSE
         Ncol=ncmax
        END IF
       END IF
      END IF
      IF(Issap.eq.0)THEN
       Ncol=0
       Nlen=0
*       CALL abend
       RETURN
      END IF
c-----------------------------------------------------------------------
c     set number of months (Sslen) to be used in sliding spans
c     analysis.
c-----------------------------------------------------------------------
      Sslen=Nlen+(Ncol-1)*Ny
c-----------------------------------------------------------------------
c     set first month, year of sliding spans analysis.
c-----------------------------------------------------------------------
      IF(Length.eq.Sslen)THEN
       Im=mod(Pos1,Ny)
       Iyr=Lyr+(Pos1/Ny)
       IF(Im.eq.0)THEN
        Im=Ny
        Iyr=Iyr-1
       END IF
      ELSE
       l2=Length-(Nlen+(Ncol-1)*Ny)
       Iyr=Lyr+(Pos1+l2)/Ny
       Im=mod(Pos1+l2,Ny)
       IF(Im.eq.0)THEN
        Im=Ny
        Iyr=Iyr-1
       END IF
c       IF(Ny.eq.4)Ltmax=Ltmax-3
       IF((.not.Lnlset).and.Ltmax.lt.4)THEN
c-----------------------------------------------------------------------
c     set first month of sliding spans analysis to be january or
c     first quarter of series.  adjust nlen, len if necessary.
c-----------------------------------------------------------------------
        lyr0=Lyr+(Pos1/Ny)
        pos0=mod(Pos1,Ny)
        IF(pos0.eq.0)THEN
         pos0=Ny
         lyr0=lyr-1
        END IF
        IF((pos0.eq.1.and.Im.ne.1).or.(pos0.lt.Im.and.Iyr.eq.lyr0))THEN
         Nlen=(Im-Pos0)+Nlen
         Im=Pos0
        ELSE IF(Im.gt.1.and.Iyr.gt.lyr0)THEN
         Nlen=(Im-1)+Nlen
         Im=1
        END IF
       END IF
       Sslen=Nlen+(Ncol-1)*Ny
      END IF
c-----------------------------------------------------------------------
c     set last month of sliding spans analysis.
c-----------------------------------------------------------------------
      lm=mod(SSlen,Ny)
      IF(lm.eq.0)lm=Ny
c-----------------------------------------------------------------------
c     set first month (icm) and year (icyr) of sliding spans
c     comparisons, relative position of first sliding spans
c     comparison (ic).
c-----------------------------------------------------------------------
      IF(Strtss(YR).eq.NOTSET)THEN
       Strtss(YR)=Iyr+1
       Strtss(MO)=Im
       Icm=Im
       Icyr=Iyr+1
       Ic=Im+Ny
      ELSE 
       frstsp(MO)=Im
       frstsp(YR)=Iyr
       CALL dfdate(Strtss,frstsp,Ny,i)
       IF(i.ge.Ny)THEN
        Icm=Strtss(MO)
        Icyr=Strtss(YR)
        Ic=(Icyr-Iyr)*Ny+Icm
       ELSE
        CALL wWritln('Date of the first sliding spans comparison is '//
     &               'set too early.',fhnote,Mt2,T,F)
        CALL writln('         This date will be reset so that it is '//
     &               'one year after the',fhnote,Mt2,F,F)
        CALL writln('         starting date of the first span.',
     &              fhnote,Mt2,F,T)
        Icm=Im
        Icyr=Iyr+1
        Ic=Im+Ny
        Strtss(MO)=Icm
        Strtss(YR)=Icyr
       END IF
      END IF
c-----------------------------------------------------------------------
c     Set number of sliding spans comparisons for each type of variable
c-----------------------------------------------------------------------
      begss(YR)=Iyr
      begss(MO)=Im
      CALL addate(begss,Ny,Sslen-Ny,endss)
      CALL dfdate(endss,Strtss,Ny,nmcomp)
      DO i=1,5
       IF(i.le.3)THEN
        Itot(i)=nmcomp
       ELSE IF(i.eq.4)THEN
        Itot(i)=nmcomp-1
       ELSE
        Itot(i)=nmcomp-Ny
       END IF
      END DO
c-----------------------------------------------------------------------
c     calculate beginning date of backcasts
c-----------------------------------------------------------------------
      CALL addate(begss,Ny,-Nbcst,ssbak)
c-----------------------------------------------------------------------
c     if first month of backcasts not = 1, increase number of backcasts
c     to accomodate.
c-----------------------------------------------------------------------
      IF(ssbak(MO).gt.1)THEN
       Nbcst2=Nbcst+ssbak(MO)-1
      ELSE
       Nbcst2=Nbcst
      END IF
c-----------------------------------------------------------------------
c     Check to see if length of span is long enough to support
c     trading day adjustment.
c-----------------------------------------------------------------------
      IF(Nlen.lt.5*Ny)THEN
       IF(Itd.eq.1)Itd=-2
       IF(Ihol.eq.1)Ihol=-2
      END IF
c-----------------------------------------------------------------------
      Nsea=Ny
      L0=Pos2-(Nlen+Im-2+(Ncol-1)*Ny)
      tdfix=F
      holfix=F
      usrfix=F
      Otlfix=F
      IF(Nssfxr.gt.0)THEN
       DO i=1,Nssfxr
        IF(Ssfxrg(i).eq.1)THEN
         tdfix=T
        ELSE IF(Ssfxrg(i).eq.2)THEN
         holfix=T
        ELSE IF(Ssfxrg(i).eq.3)THEN
         usrfix=T
        ELSE IF(Ssfxrg(i).eq.4)THEN
         Otlfix=T
        END IF
       END DO
      END IF
C-----------------------------------------------------------------------
      IF(Nssfxr.gt.0)THEN
       CALL cpyint(Ssfxrg,4,1,Ssfxxr)
       Nssfxx=Nssfxr
      END IF
C-----------------------------------------------------------------------
c     Set regARIMA modelling options for sliding spans, if regARIMA
c     modelling requested
C-----------------------------------------------------------------------
      IF(Lmodel)THEN
       mdlok=T
       CALL ssmdl(Iyr,Im,Itd,Ihol,tdfix,holfix,Otlfix,usrfix,mdlok)
       IF(.not.mdlok)Lfatal=T
       IF(Lfatal)RETURN
      END IF
C-----------------------------------------------------------------------
      IF(Nbx.gt.0)THEN
       CALL ssxmdl(Begspn,Begss,Itd,Ihol,tdfix,holfix,Otlfix,usrfix)
       IF(Lfatal)RETURN
       IF(Lmodel)CALL restor(Lmodel,F,F)
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
