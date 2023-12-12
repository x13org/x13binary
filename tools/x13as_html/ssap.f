C     Last change: LS Nov, 2022- comment generating histogram skip links
c     in sliding spans.
C     previous change:  BCM  17 Apr 2003   11:22 pm
      SUBROUTINE ssap(S,Sa,Td,Sfrng,Iagr,Ncol,Nlen,Lsumm,Lyy,Ssdiff,
     &                Lgraf)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c  *****  main subroutine for the sliding spans analysis.  This
c  *****  subroutine initializes variables and calls subroutines which
c  *****  generate breakdown tables, a listing of each observation in
c  *****  each span, and a range analysis of the seasonal factors
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'stdio.i'
      INCLUDE 'ssap.prm'
      INCLUDE 'ssap.cmn'
      INCLUDE 'notset.prm'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'dgnsvl.i'
      INCLUDE 'ssptbl.i'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'force.cmn'
      INCLUDE 'mq3.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO
      LOGICAL T,F
      PARAMETER(ZERO=0D0,T=.TRUE.,F=.FALSE.)
c-----------------------------------------------------------------------
      LOGICAL Lsaneg,lrange,Lyy,Ssdiff,Lgraf
      CHARACTER fc*(1),cpobs*(9),upper*(1),eststr*(45),chrarg*(31),
     &          ex*(2)
      DOUBLE PRECISION c,dmax,S,Sa,Td,yy,Saabav,sabs,dn,Sfrng
      INTEGER i1,i2,io1,iobs,tagr,l,l0,i,nstr,j,j2,nyears,Lsumm,nyearz,
     &        y,per,narg,nmq,iext,Iagr,n48,Ncol,nqm,isum,ispan,Nlen,
     &        spanvc,fhname
      DIMENSION c(MXLEN,MXCOL),cpobs(20),dmax(MXLEN,NEST),eststr(NEST),
     &          ex(2*NEST),fc(3),nstr(NEST),per(2*MXCOL),S(MXLEN,MXCOL),
     &          Sa(MXLEN,MXCOL),Sfrng(MXLEN,MXCOL),spanvc(NEST),
     &          Td(MXLEN,MXCOL),y(2*MXCOL),yy(MXLEN,MXCOL)
c-----------------------------------------------------------------------
      INTEGER nblank
      LOGICAL istrue,dpeq
      EXTERNAL nblank,istrue,dpeq
c-----------------------------------------------------------------------
      COMMON /addneg/ Saabav,Lsaneg
c-----------------------------------------------------------------------
      CHARACTER SSEDIC*174
      INTEGER sseptr,PSSE
      PARAMETER(PSSE=6)
      DIMENSION sseptr(0:PSSE)
      PARAMETER(SSEDIC=
     &'Seasonal FactorsTrading Day FactorsFinal Seasonally Adjusted Seri
     &esMonth-to-Month Changes in SA SeriesYear-to-Year Changes in SA Se
     &riesQuarter-to-Quarter Changes in SA Series')
c-----------------------------------------------------------------------
      DATA sseptr/1,17,36,68,103,136,175/
      DATA fc/'2','3','4'/
      DATA ex/'a ','ai','b ','bi','c ','ci','d ','di','e ','ei'/
      DATA cpobs/'January  ','February ','March    ','April    ',
     &     'May      ','June     ','July     ','August   ','September',
     &     'October  ','November ','December ','First    ','Second   ',
     &     'Third    ','Fourth   ','1st      ','2nd      ','3rd      ',
     &     '4th      '/
      DATA spanvc/LSSSFS,LSSTDS,LSSSAS,LSSCHS,LSSYCS/
c-----------------------------------------------------------------------
      l=Im+(Iyr*Nsea)+Nlen-1
      Lobs=mod(l,Nsea)
      IF(Lobs.eq.0)THEN
       Lobs=Nsea
       Lyear=(l/Nsea)-1
      ELSE
       Lyear=l/Nsea
      END IF
      nyears=Lyear-Iyr+1
      Sslen2=Sslen-Nsea+Im
      Ns1=Ncol+1
      nyearz=nyears+Ncol-3
      iobs=Im+Nsea-1
      io1=1
      IF(Nsea.eq.4)io1=2
      fhname=STDERR
      IF(Lquiet)fhname=0
c-----------------------------------------------------------------------
c     Set variables needed to print out sliding spans tables.
c-----------------------------------------------------------------------
      tagr=0
      chrarg='.'
      narg=1
      IF(Iagr.eq.6)THEN
       tagr=1
       chrarg=': Indirect seasonal adjustment.'
       narg=31
      END IF
      nmq=nblank(Moqu)
c-----------------------------------------------------------------------
c     Set printing of spans output if differences are analyzed
c-----------------------------------------------------------------------
      IF(Ssdiff)THEN
       IF(.not.Prttab(LSSTDS).AND.(Itd.eq.1).and.tagr.eq.0)
     &    Prttab(LSSTDS)=T
       IF(.not.Prttab(LSSSFS+tagr))Prttab(LSSSFS+tagr)=T
       IF(.not.Prttab(LSSSAS+tagr))Prttab(LSSSAS+tagr)=T
       IF(.not.Prttab(LSSCHS+tagr))Prttab(LSSCHS+tagr)=T
       IF(.not.Prttab(LSSYCS+tagr).and.Lyy)Prttab(LSSYCS+tagr)=T
c       IF(Prttab(LSSPCT+tagr))Prttab(LSSPCT+tagr)=F
c       IF(Prttab(LSSYPC+tagr))Prttab(LSSYPC+tagr)=F
       IF(Lsumm.gt.0)THEN
        Savtab(LSSPCT+tagr)=F
        Savtab(LSSYPC+tagr)=F
        CALL nWritln('Sliding spans percentages cannot be stored in a'//
     &               'separate diagonstics',Mt2,fhname,T,F)
        CALL writln('      file when absolute differences of additive'//
     &              ' adjustments are analyzed.',Mt2,fhname,F,T)
       END IF
       IF(Svltab(LSLPCT))THEN
        CALL nWritln('Sliding spans percentages cannot be saved to a'//
     &               ' log file when absolute',Mt2,fhname,T,F)
        CALL writln('      differences of additive adjustments are '//
     &              'analyzed.',Mt2,fhname,F,T)
       END IF
      END IF
c-----------------------------------------------------------------------
c     Initialize Ntot (Number of months flagged) to null
c-----------------------------------------------------------------------
      DO i=1,NEST
       Ntot(i)=NOTSET
c-----------------------------------------------------------------------
c     setup eststr (name of estimate) and nstr (length of name).
c-----------------------------------------------------------------------
       eststr(i)=' '
       IF(i.eq.4.and.Nsea.eq.4)THEN
        CALL getstr(SSEDIC,sseptr,PSSE,PSSE,eststr(i),nstr(i))
       ELSE IF(i.lt.5.or.Lyy)THEN
        CALL getstr(SSEDIC,sseptr,PSSE,i,eststr(i),nstr(i))
       END IF
       IF(Lfatal)RETURN
      END DO
c-----------------------------------------------------------------------
      Lsaneg=F
      IF(Muladd.eq.1)THEN
c-----------------------------------------------------------------------
c     If additive adjustment, determine if any values of the
c     seasonally adjusted series are less than or equal to zero.
c-----------------------------------------------------------------------
       sabs=ZERO
       dn=ZERO
       DO i=1,Ncol
        DO j=1,Sslen
         j2=j+Im-1
         IF((.not.dpeq(Sa(j2,i),DNOTST)).and.j2.ge.Ic)THEN
          IF(Sa(j2,i).le.0D0)Lsaneg=T
          sabs=sabs+abs(Sa(j2,i))
          dn=dn+1D0
         END IF
        END DO
       END DO
       IF(Lsaneg)Saabav=sabs/dn
      END IF
c-----------------------------------------------------------------------
c     Calculate and print out range values for each span
c-----------------------------------------------------------------------
      lrange=Kfulsm.eq.2
      IF((.NOT.Lsaneg).AND.(.NOT.Ssdiff).and.Kfulsm.eq.0)THEN
       IF(Prttab(LSSFMN+tagr))THEN
*        IF(Lpage)THEN
*         WRITE(Mt1,Ttlfmt)Newpg,Title(1:Ntitle),Kpage,Serno(1:Nser)
*         Kpage=Kpage+1
*        END IF
        upper=CHAR(ICHAR(Qm(1:1))-32)
        nqm=nblank(Qm)
        IF(tagr.eq.0)THEN
         CALL genSkip(1090)
        ELSE
         CALL genSkip(1091)
        END IF
        IF(Muladd.eq.0)THEN
         CALL writTagOneLine(Mt1,'h2','@','S1.  '//upper//Qm(2:nqm)//
     &                       ' means of Seasonal Factors for '//
     &                       Serno(1:Nser)//chrarg(1:narg))
        ELSE
         CALL writTagOneLine(Mt1,'h2','@','S1.  '//upper//Qm(2:nqm)//
     &                       ' means of Implied Adjustment Factors '//
     &                       'for '//Serno(1:Nser)//chrarg(1:narg))
        END IF
        CALL writTagOneLine(Mt1,'h3','@','(movements within a '//
     &                      Moqu(1:nmq)//' should be small)')
       END IF
       IF(Muladd.eq.1)THEN
        CALL ssrng(Sfrng,cpobs,Iagr,lrange,Ncol,Muladd)
       ELSE
        CALL ssrng(S,cpobs,Iagr,lrange,Ncol,Muladd)
       END IF
       IF(Prttab(LSSYCS+tagr).AND.(.not.Lyy))Prttab(LSSYCS+tagr)=F
      END IF
c-----------------------------------------------------------------------
c     Compute the month-to-month and year-to-year changes in the final
c     seasonally adjusted series for each span.
c-----------------------------------------------------------------------
      CALL xchng(Sa,c,Ncol,Im,Sslen,1,Ssdiff)
      IF(Lyy)CALL xchng(Sa,yy,Ncol,Im,Sslen,Nsea,Ssdiff)
c-----------------------------------------------------------------------
c     Determine how many months were flagged for each of the seasonal
c     adjustment estimates collected for the sliding spans procedure 
c     and compute the percentage for months flagged.
c-----------------------------------------------------------------------
      IF(Muladd.eq.0)THEN
c-----------------------------------------------------------------------
c     If multiplicative adjustment, flag seasonal and trading day
c     factors, if requested.
c-----------------------------------------------------------------------
       IF(Kfulsm.eq.0)CALL mflag(S,1,0,iobs,dmax,Ncol,Ssdiff)
       IF(Iagr.lt.6)THEN
        IF(Itd.eq.1)CALL mflag(Td,2,0,iobs,dmax,Ncol,Ssdiff)
        IF((Kfulsm.eq.0.and.(Lrndsa.or.Iyrt.gt.0.or.Itd.eq.1)).or.
     &     Ihol.eq.1)CALL mflag(Sa,3,0,iobs,dmax,Ncol,Ssdiff)
       END IF
      ELSE
c-----------------------------------------------------------------------
c     Flag seasonally adjusted series.
c-----------------------------------------------------------------------
       IF(Ssdiff)THEN
        IF(Kfulsm.eq.0)CALL mflag(S,1,0,iobs,dmax,Ncol,Ssdiff)
        IF(Itd.eq.1)CALL mflag(Td,2,0,iobs,dmax,Ncol,Ssdiff)
        IF((Kfulsm.eq.0.and.(Lrndsa.or.Iyrt.gt.0.or.Itd.eq.1)).or.
     &      Ihol.eq.1)CALL mflag(Sa,3,0,iobs,dmax,Ncol,Ssdiff)
       ELSE
        CALL mflag(Sa,3,0,iobs,dmax,Ncol,Ssdiff)
       END IF
      END IF
c-----------------------------------------------------------------------
      iobs=iobs+1
      CALL mflag(c,4,io1,iobs,dmax,Ncol,Ssdiff)
      IF(Lyy)THEN
       iobs=iobs+Nsea-1
       CALL mflag(yy,5,3,iobs,dmax,Ncol,Ssdiff)
      END IF
c-----------------------------------------------------------------------
c     Print out percent of observations flagged as extremes 
c-----------------------------------------------------------------------
      IF(.NOT.Ssdiff)THEN
       IF(lrange)THEN
        IF(Prttab(LSSPCT+tagr))THEN
*         IF(Lpage)THEN
*          WRITE(Mt1,Ttlfmt)Newpg,Title(1:Ntitle),Kpage,Serno(1:Nser)
*          Kpage=Kpage+1
*         END IF
         IF(tagr.eq.0)THEN
          CALL genSkip(1092)
         ELSE
          CALL genSkip(1093)
         END IF
         CALL writTagOneLine(Mt1,'h2','@',
     &                       'S  2.  Percentage of '//Moqu(1:nmq)//
     &                       's flagged as unstable'//chrarg(1:narg))
        END IF
c-----------------------------------------------------------------------
        IF(Savtab(LSSPCT+tagr).or.Savtab(LSSYPC+tagr))
     &     WRITE(Nform,1032)'yes'
        CALL pctrit(ex,tagr,Muladd,Nsea,eststr,nstr,Ntot,Itot,Cut,
     &              Moqu,nmq,chrarg,narg,Prttab(LSSPCT+tagr),
     &              Savtab(LSSPCT+tagr),Prttab(LSSYPC+tagr),
     &              Savtab(LSSYPC+tagr))
       ELSE
        IF(Svltab(LSLPCT))THEN
         CALL mkPOneLine(Ng,'@',
     &         'Range of seasonal factors is too low for '//
     &         'summary sliding spans measures to be reliable.')
         CALL mkPOneLine(Ng,'@',
     &         'Summary sliding spans statistics not computed.')
        END IF
        IF(Savtab(LSSPCT+tagr).or.Savtab(LSSYPC+tagr))
     &     WRITE(Nform,1032)'no'
       END IF
      ELSE
       IF(Savtab(LSSPCT+tagr).or.Savtab(LSSYPC+tagr))
     &    WRITE(Nform,1032)'no'
      END IF
 1032 FORMAT('s2.pct: ',a)
c-----------------------------------------------------------------------
c     Generate summary of months flagged for each estimate
c-----------------------------------------------------------------------
c     For each estimate, check to see if the number of months flagged
c     has been reset.
c-----------------------------------------------------------------------
      DO i=1,NEST
       IF(Ntot(i).ne.NOTSET)THEN
        iext=tagr+(2*i)-1
        i2=0
        IF(i.gt.3)THEN
         i2=i-2
         IF(i2.eq.2.and.Nsea.eq.12)i2=i2-1
        END IF
        isum=LSSSUM+tagr
        IF(i.eq.NEST)isum=LSSYSM+tagr
c-----------------------------------------------------------------------
c     Print or save breakdown tables, histogram for sliding spans
c     analysis.
c-----------------------------------------------------------------------
        IF(Prttab(isum))CALL genSkip(1020+iext)
        CALL btrit(nyearz,i,i2,Iagr,ex(iext),eststr(i),nstr(i),cpobs,
     &             lrange,Ssdiff,Prttab(isum),Savtab(isum))
        ispan=spanvc(i)+tagr
c     stop generateing hisogram link and skips, 1045 is duplicate with
c     spectral diagnostics.
c        IF(Prttab(isum))CALL genSkip(1035+iext)
        CALL sshist(dmax,i,Iagr,ex(iext),iext,eststr(i),nstr(i),lrange,
     &              Prttab(isum),Savtab(isum),Prttab(ispan),
     &              Ssdiff)
        IF(Lfatal)RETURN
       END IF
      END DO
c-----------------------------------------------------------------------
c     Check to see if spans are printed or stored
c-----------------------------------------------------------------------
      IF(istrue(Prttab,LSSSFS,LSSTDS).or.istrue(Savtab,LSSSFS,LSSTDS)
     &   .OR.Ssdiff.or.Lgraf)
     &   THEN
c-----------------------------------------------------------------------
c     Set up labels for spans printout
c-----------------------------------------------------------------------
       DO l=1,Ncol
        i1=l
        i2=l+Ncol
        y(i1)=Iyr+l-1
        y(i2)=Lyear+l-1
        per(i1)=Im
        per(i2)=Lobs
       END DO
c-----------------------------------------------------------------------
c     Initialize other variables needed to print out spans
c-----------------------------------------------------------------------
c       length=Sslen+Im-1
       n48=(Sslen/48)+1
       IF(mod(Sslen,48).eq.0)n48=n48-1
       l0=Ncol-1
       F1(17:17)=fc(l0)
       F2(6:6)=fc(l0)
       F3(17:17)=fc(l0)
c-----------------------------------------------------------------------
c     Print out or save spans for each estimate
c-----------------------------------------------------------------------
       DO i=1,NEST
        IF(Ntot(i).ne.NOTSET)THEN
         ispan=spanvc(i)+tagr
         IF(Prttab(ispan))THEN
          CALL genSkip(ispan)
          IF(i.eq.1)THEN
c-----------------------------------------------------------------------
c     Print spans of Seasonal factors
c-----------------------------------------------------------------------
           CALL mlist(S,i,0,dmax,n48,Iagr,ex(tagr+1),eststr(i),nstr(i),
     &                Ncol,y,per,Ssdiff)
          ELSE IF(i.eq.2)THEN
c-----------------------------------------------------------------------
c     Print spans of Trading day factors
c-----------------------------------------------------------------------
           CALL mlist(Td,i,0,dmax,n48,Iagr,ex(3),eststr(i),nstr(i),Ncol,
     &               y,per,Ssdiff)
          ELSE IF(i.eq.3)THEN
c-----------------------------------------------------------------------
c     Print spans of Seasonally adjusted series
c-----------------------------------------------------------------------
           CALL mlist(Sa,i,0,dmax,n48,Iagr,ex(tagr+5),eststr(i),nstr(i),
     &                Ncol,y,per,Ssdiff)
          ELSE IF(i.eq.4)THEN
c-----------------------------------------------------------------------
c     Print spans of Month-to-Month (or quarter to quarter) changes
c-----------------------------------------------------------------------
           CALL mlist(c,i,io1,dmax,n48,Iagr,ex(tagr+7),eststr(i),
     &                nstr(i),Ncol,y,per,Ssdiff)
          ELSE IF(i.eq.5)THEN
c-----------------------------------------------------------------------
c     Print spans of Year-to-Year changes
c-----------------------------------------------------------------------
           CALL mlist(yy,i,3,dmax,n48,Iagr,ex(tagr+9),eststr(i),nstr(i),
     &                Ncol,y,per,Ssdiff)
          END IF
          IF(Lfatal)RETURN
         END IF
         IF(Savtab(ispan).or.Lgraf)THEN
          IF(i.eq.1)THEN
c-----------------------------------------------------------------------
c     Save spans of Seasonal factors
c-----------------------------------------------------------------------
           IF(Savtab(ispan))CALL svspan(S,i,dmax,ispan,Ncol,F)
           IF(Lgraf)CALL svspan(S,i,dmax,ispan,Ncol,Lgraf)
          ELSE IF(i.eq.2)THEN
c-----------------------------------------------------------------------
c     Save spans of Trading day factors
c-----------------------------------------------------------------------
           IF(Savtab(ispan))CALL svspan(Td,i,dmax,ispan,Ncol,F)
           IF(Lgraf)CALL svspan(Td,i,dmax,ispan,Ncol,Lgraf)
          ELSE IF(i.eq.3)THEN
c-----------------------------------------------------------------------
c     Save spans of Seasonally adjusted series
c-----------------------------------------------------------------------
           IF(Savtab(ispan))CALL svspan(Sa,i,dmax,ispan,Ncol,F)
           IF(Lgraf)CALL svspan(Sa,i,dmax,ispan,Ncol,Lgraf)
          ELSE IF(i.eq.4)THEN
c-----------------------------------------------------------------------
c     Save spans of Month-to-Month (or quarter to quarter) changes
c-----------------------------------------------------------------------
           IF(Savtab(ispan))CALL svspan(C,i,dmax,ispan,Ncol,F)
           IF(Lgraf)CALL svspan(C,i,dmax,ispan,Ncol,Lgraf)
          ELSE IF(i.eq.5)THEN
c-----------------------------------------------------------------------
c     Save spans of Year-to-Year changes
c-----------------------------------------------------------------------
           IF(Savtab(ispan))CALL svspan(yy,i,dmax,ispan,Ncol,F)
           IF(Lgraf)CALL svspan(yy,i,dmax,ispan,Ncol,Lgraf)
          END IF
          IF(Lfatal)RETURN
         END IF
        END IF
       END DO
      END IF
c-----------------------------------------------------------------------
      RETURN
c-----------------------------------------------------------------------
      END
