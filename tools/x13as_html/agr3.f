C     Last change: Mar. 2021 - add abs to value tmpe at line 359
C     Last change:  BCM  19 May 2003    8:32 am
      SUBROUTINE agr3(Lgraf,Begspn,Lx11)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- THIS ROUTINE PRODUCES THE INDIRECT SEASONALLY ADJUSTED SERIES.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'stdio.i'
      INCLUDE 'build.prm'
      INCLUDE 'lex.i'
      INCLUDE 'agr.cmn'
      INCLUDE 'agrsrs.cmn'
      INCLUDE 'inpt.cmn'
      INCLUDE 'seatcm.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11fac.cmn'
      INCLUDE 'x11msc.cmn'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'x11srs.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'cmptbl.i'
      INCLUDE 'force.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'adxser.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      LOGICAL F,T
      INTEGER MO,YR
      DOUBLE PRECISION ONE,ZERO
      PARAMETER(ONE=1D0,ZERO=0D0,F=.false.,T=.true.,MO=2,YR=1)
c-----------------------------------------------------------------------
      CHARACTER dattim*(24),Cmpfil*(PFILCR),ctype*(4),trnchr*(1),
     &          xb*(65),str*(3)
      LOGICAL Lgraf,Lx11,rndok,chkfct,oktrn
      DOUBLE PRECISION Ckhs,ebar,stexx,Stime,Stmcd,Stsie,tempo,temp3,
     &                 tmpe,stsb,flsind,faoind,Cmpwt,dvec,stc2in,rinit,
     &                 ststd,temp4,tempk,stbase,frcfac
      INTEGER Begspn,i,ib,ie,ncfil,i1,i2,i3,lnlen,ifac,Cmptyp,lastf,
     &        lstfrc,frstf,ipos
      DIMENSION stexx(PLEN),tempo(PLEN),temp3(PLEN),Stsie(PLEN),
     &          Stmcd(PLEN),Stime(PLEN),stsb(PLEN),Ckhs(PLEN),
     &          Cmpwt(PSRS),Cmptyp(PSRS),Cmpfil(PSRS),trnchr(PLEN),
     &          flsind(PLEN),faoind(PLEN),ctype(4),dvec(1),stc2in(PLEN),
     &          ststd(PLEN),Begspn(2),temp4(PLEN),stbase(PLEN),
     &          frcfac(PLEN)
c     ------------------------------------------------------------------
      CHARACTER*24 cvdttm
      INTEGER nblank
      LOGICAL dpeq
      EXTERNAL nblank,dpeq,cvdttm
c-----------------------------------------------------------------------
      EQUIVALENCE(Orig2(1),tempo(1)),(stexx(1),Omod(1))
c-----------------------------------------------------------------------
      COMMON /work3 / Stsie
      COMMON /mq5a  / Stmcd,Stime
      COMMON /kcser / Ckhs
      COMMON /cmpsum/ Cmpwt,Cmptyp,Cmpfil
c-----------------------------------------------------------------------
      DATA ctype/'add ','sub ','mult','div'/
c     ------------------------------------------------------------------
C --- WRITE THE TITLE PAGE.
c     ------------------------------------------------------------------
c      Kpage=0
      dvec(1)=ZERO
      ifac=1
      IF(Muladd.eq.1)ifac=0 
      IF(Prttab(LCMPAH))THEN
       CALL genSkip(LCMPAH)
       WRITE(Mt1,1010)PRGNAM,Cbr,Cbr,VERNUM,BUILD
       WRITE(Mt1,1020)Title,Cbr,Serno(1:Nser)
       CALL fdate(dattim)
       dattim=cvdttm(dattim)
       CALL mkPOneLine(Mt1,'@',dattim)
       WRITE(Mt1,1040)'<p>',Begspn(MO),Begspn(YR),Lstmo,Lstyr 
       WRITE(Mt1,1050)Ncomp
       CALL mkTableTag(Mt1,'w80','Summary of component series')
       CALL mkCaption(Mt1,'Summary of component series')
       CALL writTag(Mt1,'<tr>')
       CALL mkTableCell(Mt1,'head','&nbsp;')
       CALL mkHeaderCellScope(Mt1,0,0,'col','@',
     &                        'Composite'//Cbr//'Type')
       CALL mkHeaderCellScope(Mt1,0,0,'col','@',
     &                        'Composite'//Cbr//'Weight')
       CALL mkHeaderCellScope(Mt1,0,0,'col','@',
     &                        'Spec file'//Cbr//'for series')
       CALL writTag(Mt1,'</tr>')
       DO i=1,Ncomp
        CALL writTag(Mt1,'<tr>')
        ipos=1
        CALL itoc(i,str,ipos)
        IF(Lfatal)RETURN
        CALL mkHeaderCellScope(Mt1,0,0,'row','@',
     &                         'Component '//str(1:(ipos-1)))
        CALL mkTableCell(Mt1,'center',ctype(Cmptyp(i)+1))
        WRITE(Mt1,1030)Cmpwt(i)
        ncfil=nblank(Cmpfil(i))
        CALL mkTableCell(Mt1,'center',Cmpfil(i)(1:ncfil))
        CALL writTag(Mt1,'</tr>')
       END DO
       CALL writTag(Mt1,'</table>')
       CALL mkPOneLine(Mt1,'@','&nbsp;')
      END IF
      IF(Savtab(LCMPAH))WRITE(Nform,1080)Ncomp
c     ------------------------------------------------------------------
C --- INDIRECT SEASONAL ADJUSTMENT.
c     ------------------------------------------------------------------
      rinit=1D0
      IF(Muladd.eq.1)rinit=0D0
      DO i=Pos1bk,Posffc
       IF(Lx11)THEN
        tempo(i)=Stci(i)
       ELSE
        tempo(i)=Seatsa(i)
       END IF
       Stci(i)=Ckhs(i)
      END DO
      Iagr=4
c     ------------------------------------------------------------------
C --- APPLY A 13- OR 5-TERM HENDERSON TO THE CI
c     ------------------------------------------------------------------
      Ktcopt=13
      IF(Ny.eq.4)Ktcopt=5
      IF(Lx11)THEN
       CALL vtc(Stc,Stci)
       IF(Lsumm.gt.0.and.Issap.lt.2.and.Irev.lt.4)THEN
        WRITE(Nform,1000)Nterm
 1000   FORMAT('indtrendma: ',i3)
       END IF
c-----------------------------------------------------------------------
c     For multiplicative seasonal adjustment, check to see if any
c     of the trend values are negative.
c-----------------------------------------------------------------------
       oktrn=T
       IF(Muladd.eq.0)THEN
        chkfct=F
        CALL chktrn(Stc,Kpart,12,trnchr,chkfct,oktrn)
       END IF
      ELSE
       Tic=3.5D0
       IF(Ny.eq.4)Tic=0.001D0
      END IF
c     ------------------------------------------------------------------
C --- SAVE DIRECT  SEASONALLY ADJUSTED SERIES.
c     ------------------------------------------------------------------
      DO i=Pos1bk,Posffc
       IF(Lx11)THEN
        Tem(i)=Stc(i)
       ELSE
        Tem(i)=Seattr(i)
       END IF
      END DO
c     ------------------------------------------------------------------
c     Update pointers and starting variables with indirect pointers
c     (BCM January 2003)
c     ------------------------------------------------------------------
      Pos1bk=Ind1bk
      Posffc=Indffc
      Nofpob=Nofpob-Nfcst+Indnfc
      Nbfpob=Nofpob-Nfcst+Indnfc-Nbcst+Indnbc
      Dirnfc=Nfcst
      Dirnbc=Nbcst
      Nfcst=Indnfc
      Nbcst=Indnbc
      CALL addate(Begspn,Ny,-Nbcst,Begbak)
      lastf=Posfob
      IF(Savfct.and.(Posffc.gt.Posfob))lastf=Posffc
      frstf=Pos1ob
      IF(Savbct.and.(Pos1bk.lt.Pos1ob))frstf=Pos1bk
c     ------------------------------------------------------------------
      DO i=Pos1bk,Posffc
       Stci(i)=Ci(i)
       Stome(i)=Omod(i)
      END DO
      Kpart=1
      IF(Tmpma.eq.2)Tmpma=0
c     ------------------------------------------------------------------
c     print out prior adjusted composite series (BCM March 2004)
c     ------------------------------------------------------------------
      IF(Prttab(LCMPA3))
     &   CALL table(O1,Pos1ob,Posfob,3,1,2,dvec,LCMPA3)
      IF(.not.Lfatal.and.Savtab(LCMPA3))
     &   CALL punch(O1,Pos1ob,Posfob,LCMPA3,F,F)
      IF(.not.Lfatal.and.Lgraf)
     &   CALL punch(O1,Pos1ob,Posfob,LCMPA3,Lgraf,F)
      IF(Lfatal)RETURN
      Kpart=4
c     ------------------------------------------------------------------
C --- CALCULATE EXTREMES FOR MODIFIED SERIES.
c     ------------------------------------------------------------------
      CALL divsub(stexx,Series,Stome,Pos1ob,Posfob)
c     ------------------------------------------------------------------
C --- CALCULATE THE MODIFIED SEASONALLY ADJUSTED SERIES E2.
c     ------------------------------------------------------------------
      CALL divsub(Stcime,Stci,stexx,Pos1ob,Posfob)
c     ------------------------------------------------------------------
C --- MODIFY EXTREMES IN THE SEASONALLY ADJUSTED SERIES FOR CALCULATING
C --- THE FINAL TREND-CYCLE.
c     ------------------------------------------------------------------
      DO i=Pos1ob,Posfob
       Stci(i)=Stcime(i)
      END DO
c     ------------------------------------------------------------------
c     Remove level changes from seasonally adjusted series before
c     calculating the final trend cycle (BCM - December 2002)
c     ------------------------------------------------------------------
      CALL setdp(rinit,PLEN,flsind)
      CALL setdp(rinit,PLEN,faoind)
      IF(Lindot)THEN
       IF(Lindls)THEN
        CALL divsub(flsind,O,O3,Pos1bk,Posffc)
        CALL divsub(Stci,Stci,flsind,Pos1bk,Posffc)
       END IF
       IF(Lindao)CALL divsub(faoind,O,O4,Pos1bk,Posffc)
      END IF
c     ------------------------------------------------------------------
C --- APPLY THE VARIABLE TREND-CYCLE ROUTINE TO OBTAIN THE FINAL TREND
C --- CYCLE D12, BUT INSIST ON A 13- OR 5-TERM HENDERSON
c     ------------------------------------------------------------------
      Ktcopt=13
      IF(Ny.eq.4)Ktcopt=5
      CALL vtc(Stc,Stci)
      DO i=Pos1ob,Posfob
       Stci(i)=Ci(i)
      END DO
c-----------------------------------------------------------------------
c     If there are level change adjustments to be made to the indirect
c     trend, make them here.
c-----------------------------------------------------------------------
      CALL copy(Stc,PLEN,1,stc2in)
      IF(Lindot.and.Lindls)
     &   CALL addmul(stc2in,stc2in,flsind,Pos1bk,Posffc)
c-----------------------------------------------------------------------
c     Print out indirect level change, AO factors here.
c-----------------------------------------------------------------------
      IF(Lindot.and.Lindao)THEN
       IF(Prttab(LCPIAO))
     &    CALL table(faoind,Pos1ob,Posfob,8,1,1,dvec,LCPIAO)
       IF(.not.Lfatal.and.Savtab(LCPIAO))
     &    CALL punch(faoind,Pos1ob,Posfob,LCPIAO,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(faoind,Pos1ob,Posfob,LCPIAO,Lgraf,F)
       IF(Lfatal)RETURN
      END IF
      IF(Lindot.and.Lindls)THEN
       IF(Prttab(LCPILS))
     &    CALL table(flsind,Pos1ob,Posfob,8,1,1,dvec,LCPILS)
       IF(.not.Lfatal.and.Savtab(LCPILS))
     &    CALL punch(flsind,Pos1ob,Posfob,LCPILS,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(flsind,Pos1ob,Posfob,LCPILS,Lgraf,F)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c     For multiplicative seasonal adjustment, check to see if any
c     of the trend values are negative.
c-----------------------------------------------------------------------
      oktrn=T
      IF(Muladd.eq.0)THEN
       chkfct=F
       CALL chktrn(Stc,Kpart,12,trnchr,chkfct,oktrn)
      END IF
c     ------------------------------------------------------------------
C --- DIVIDE THE FINAL TREND-CYCLE INTO THE FINAL SEASONALLY ADJUSTED
C --- SERIES TO GET THE FINAL IRREGULAR D13.
c     ------------------------------------------------------------------
      CALL divsub(Sti,Stci,stc2in,Pos1ob,Posfob)
c     ------------------------------------------------------------------
C --- OBTAIN THE MODIFIED IRREGULAR BY DIVIDING THE FINAL IRREGULAR BY
C --- THE EXTREMES.
c     ------------------------------------------------------------------
      CALL divsub(Stime,Sti,stexx,Pos1ob,Posfob)
c     ------------------------------------------------------------------
C --- CALCULATE THE INDIRECT SEASONAL FACTORS.
c     ------------------------------------------------------------------
      IF(Psuadd)THEN
       DO i=Pos1bk,Posffc
c        Sts(i)=(Series(i)/Stc(i))-Sti(i)+ONE
        Sts(i)=(O2(i)/Stc(i))-Sti(i)+ONE
        stsb(i)=Stc(i)*(Sts(i)-ONE)
       END DO
      ELSE
c       CALL divsub(Sts,Series,Stci,Pos1ob,kcldaf)
       CALL divsub(Sts,O5,Stci,Pos1bk,Posffc)
      END IF
c     ------------------------------------------------------------------
      CALL divsub(ststd,O2,Stci,Pos1bk,Posffc)
      CALL divsub(Faccal,O2,O5,Pos1bk,Posffc)
c     ------------------------------------------------------------------
C --- FORECAST THE SEASONAL FACTORS ONE YEAR AHEAD.
c     ------------------------------------------------------------------
*      CALL forcst(Sts,0,kcldaf,kclda,Ny,1,PT5,ONE)
c     ------------------------------------------------------------------
C --- OBTAIN THE UNMODIFIED SI BY DIVIDING THE ORIGINAL SERIES BY THE
C --- FINAL TREND-CYCLE.
c     ------------------------------------------------------------------
      CALL divsub(Stsie,Series,stc2in,Pos1ob,Posfob)
c     ------------------------------------------------------------------
C --- OBTAIN THE MODIFIED SI BY DIVIDING THE UNMODIFIED SI BY THE
C --- EXTREMES.
c     ------------------------------------------------------------------
      CALL divsub(Stsi,Stsie,stexx,Pos1ob,Posfob)
c     ------------------------------------------------------------------
c     Remove AO outliers from unmodified SI ratios before calculating
c     the seasonal adjustment diagnostics (BCM - February 2003)
c     ------------------------------------------------------------------
      IF(Lindot.and.Lindao)THEN
       CALL divsub(temp4,Stsie,faoind,Pos1bk,Posffc)
      ELSE
       CALL copy(Stsie,Posffc,1,temp4)
      END IF
c     ------------------------------------------------------------------
C --- WRITE THE UNMODIFIED SI RATIOS D8.
c     ------------------------------------------------------------------
      IF(Prttab(LCMPD8))
     &   CALL table(Stsie,Pos1ob,Posfob,8,1,1,dvec,LCMPD8)
      IF(.not.Lfatal.and.Savtab(LCMPD8))
     &   CALL punch(Stsie,Pos1ob,Posfob,LCMPD8,F,F)
      IF(.not.Lfatal.and.Lgraf)
     &   CALL punch(Stsie,Pos1ob,Posfob,LCMPD8,Lgraf,F)
      IF(Lfatal)RETURN
c     ------------------------------------------------------------------
C --- PERFORM ANALYSIS OF VARIANCE ON THE UNMODIFIED SI RATIOS.
c     ------------------------------------------------------------------
      IF(Prttab(LCPD8F))THEN
       CALL table(temp3,0,0,8,2,0,dvec,LCPD8F)
       IF(Lfatal)RETURN
      END IF
      CALL ftest(temp4,Pos1ob,Posfob,Ny,0,Prttab(LCPD8F),F)
      CALL kwtest(temp4,Pos1ob,Posfob,Ny,Prttab(LCPD8F))
      CALL divsub(temp4,Series,stc2in,Pos1ob,Posfob)
c     ------------------------------------------------------------------
C --- PERFORM F-TEST FOR MOVING SEASONALITY.
c     ------------------------------------------------------------------
      CALL mstest(temp4,Pos1ob,Posfob,Ny,Prttab(LCPD8F))
c     ------------------------------------------------------------------
C --- PERFORM TEST FOR THE PRESENCE OF IDENTIFIABLE SEASONALITY.
c     ------------------------------------------------------------------
      CALL combft(Prttab(LCPD8F))
c     ------------------------------------------------------------------
C --- CALCULATE I/S RATIOS.
c     ------------------------------------------------------------------
      CALL vsfa(Stsi,Pos1ob,Posfob,Ny)
c     ------------------------------------------------------------------
C --- IDENTIFY SI RATIOS THAT ARE MODIFIED.
c     ------------------------------------------------------------------
      ebar=1-Muladd
      DO i=Pos1ob,Posfob
       tmpe=stexx(i)-ebar
       IF(.not.dpeq(tmpe,0D0))THEN
        tmpe=tmpe/Stsie(i)
        IF(abs(tmpe).ge.0.0001D0)THEN
         temp3(i)=Stsi(i)
         GO TO 10
        END IF
       END IF
       temp3(i)=DNOTST
   10  CONTINUE
      END DO
c     ------------------------------------------------------------------
C --- WRITE THE FINAL REPLACEMENT VALUES FOR THE SI D9.
c     ------------------------------------------------------------------
      IF(Prttab(LCMPD9))THEN
       CALL table(temp3,Pos1ob,Posfob,9,1,5,dvec,LCMPD9)
       IF(Lfatal)RETURN
      END IF
      IF((.not.Lfatal).and.Savtab(LCMPD9))
     &   CALL punch(temp3,Pos1ob,Posfob,LCMPD9,F,F)
      IF((.not.Lfatal).and.Lgraf)
     &   CALL punch(temp3,Pos1ob,Posfob,LCMPD9,Lgraf,F)
c     ------------------------------------------------------------------
C --- WRITE THE INDIRECT SEASONAL FACTORS.
c     ------------------------------------------------------------------
      IF(Prttab(LCMPSF).or.Prttab(LCPIPS))
     &   CALL table(Sts,Pos1ob,Posfob,10,3,1,dvec,LCMPSF)
      IF((.not.Lfatal).AND.(Savtab(LCMPSF).or.Savtab(LCPIPS).or.
     &    Lgraf))THEN
       IF(Savfct.or.Savbct)THEN
        IF(Savtab(LCMPSF))CALL punch(Sts,frstf,lastf,LCMPSF,F,F)
c         WRITE(Mtprof,*)'  lastsf = ',lastf
c         WRITE(Mtprof,*)' Sts = ', (Sts(i), i = lastf-11, lastf)
        IF(Savtab(LCPIPS))
     &     CALL punch(Sts,frstf,lastf,LCPIPS,F,Muladd.ne.1)
        IF(.not.Lfatal.and.Lgraf)
     &     CALL punch(Sts,frstf,lastf,LCMPSF,Lgraf,F)
       ELSE
        IF(Savtab(LCMPSF))CALL punch(Sts,Pos1ob,Posfob,LCMPSF,F,F)
        IF(Savtab(LCPIPS))
     &     CALL punch(Sts,Pos1ob,Posfob,LCPIPS,F,Muladd.ne.1)
        IF(.not.Lfatal.and.Lgraf)
     &     CALL punch(Sts,Pos1ob,Posfob,LCMPSF,Lgraf,F)
       END IF
      END IF
      IF(.not.Lfatal.and.Prttab(LCMPSP))
     &   CALL x11plt(Sts,Sts,Pos1ob,Posfob+Ny,LCMPSP,0,ifac,7,1)
c     ------------------------------------------------------------------
c     If psuedo-additive seasonal adjustment is done, generate and print
c     out the final seasonal differences (Table D 10b).
c     ------------------------------------------------------------------
      IF(Psuadd)THEN
       IF(Prttab(LCPFSD))
     &    CALL table(stsb,Pos1ob,Posfob,10,2,1,dvec,LCPFSD)
       IF(.not.Lfatal.AND.Savtab(LCPFSD))THEN
        IF(Savfct.and.Nfcst.gt.0)THEN
         CALL punch(stsb,Pos1ob,Posffc,LCPFSD,F,F)
        ELSE
         CALL punch(stsb,Pos1ob,Posfob,LCPFSD,F,F)
        END IF
       END IF
       IF(Lfatal)RETURN
      END IF
c     ------------------------------------------------------------------
C --- WRITE THE INDIRECT SEASONALLY ADJUSTED SERIES
c     ------------------------------------------------------------------
      IF(.not.Lfatal.and.Prttab(LCMPSA))
     &   CALL table(Stci,Pos1ob,Posfob,11,1,2,dvec,LCMPSA)
      IF(.not.Lfatal.and.Savtab(LCMPSA))
     &   CALL punch(Stci,Pos1ob,Posfob,LCMPSA,F,F)
      IF(.not.Lfatal.and.Lgraf)
     &   CALL punch(Stci,Pos1ob,Posfob,LCMPSA,Lgraf,F)
      IF(.not.Lfatal.and.Prttab(LCMPAP))
     &   CALL x11plt(Stci,Stci,Pos1ob,Pos1ob,LCMPAP,0,0,6,1)
      IF(Lfatal)RETURN
*      IF(Issap.eq.2)CALL ssrit(Stci,Pos1ob,Posfob,3,O2)
c     ------------------------------------------------------------------
C --- DO A TEST FOR RESIDUAL SEASONALITY.
c     ------------------------------------------------------------------
      CALL ftest(Stci,Pos1ob,Posfob,Ny,1,Prttab(LCPRSF),Savtab(LCPRSF))
c     ------------------------------------------------------------------
C --- IF OPTION SELECTED ADJUST YEARLY TOTALS OF D11 TO EQUAL THE YEARLY
C --- TOTALS OF SERIES.
c     ------------------------------------------------------------------
c     Force indirect seasonally adjusted series only if requested by
c     user, else add forced series from the components to form indirect
c     forced seasonal adjustment (BCM, May 2006)
c     ------------------------------------------------------------------
      IF(Iyrt.gt.0)THEN
c     ------------------------------------------------------------------
c     use Lfctfr to set last observation to be forced (BCM, May 2006)
c     ------------------------------------------------------------------
       IF(Lfctfr)THEN
        lstfrc=Posffc
       ELSE
        lstfrc=Posfob
       END IF
c     ------------------------------------------------------------------
       IF(Lindfr)THEN
        IF(Lsumm.gt.0.and.Issap.lt.2.and.Irev.lt.4)
     &     WRITE(Nform,1090)'yes'
c     ------------------------------------------------------------------
        IF(Iftrgt.eq.0)THEN
         CALL copy(O,lstfrc,1,stbase)
        ELSE IF(Iftrgt.eq.1)THEN
         CALL copy(O5,lstfrc,1,stbase)
        ELSE
         CALL copy(O1,lstfrc,1,stbase)
         IF(Iftrgt.eq.3)CALL divsub(stbase,stbase,Faccal,Pos1ob,lstfrc)
        END IF
c     ------------------------------------------------------------------
        IF(Iyrt.eq.1)THEN
         CALL qmap(stbase,Stci,Stci2,Pos1ob,lstfrc,Ny,ib,ie,Begyrt)
c     ------------------------------------------------------------------
c     Change made October 1995 to duplicate X-11-ARIMA/88 partial year
c     adjustment of yearly totals. BCM
c     ------------------------------------------------------------------
         IF(ie.lt.Posfob)THEN
          tempk=Stci2(ie)-Stci(ie)
          DO i=ie+1,lstfrc
           Stci2(i)=Stci(i)+tempk
          END DO
         END IF
c     ------------------------------------------------------------------
c     Change made May 2005 to do the same partial year adjustment
c     to early data BCM
c     ------------------------------------------------------------------
         IF(ib.gt.Pos1ob)THEN
          tempk=Stci2(ib)-Stci(ib)
          DO i=Posfob,ib-1
           Stci2(i)=Stci(i)+tempk
          END DO
         END IF
        ELSE
         CALL qmap2(stbase,Stci,Stci2,Pos1ob,lstfrc,Ny,Iagr)
        END IF
c     ------------------------------------------------------------------
       ELSE
        CALL copy(Ci2,lstfrc,1,Stci2)
        IF(Lsumm.gt.0.and.Issap.lt.2.and.Irev.lt.4)
     &     WRITE(Nform,1090)'no'
       END IF
c     ------------------------------------------------------------------
C --- WRITE THE SEASONALLY ADJUSTED SERIES WITH REVISED YEARLY TOTALS.
c     ------------------------------------------------------------------
       IF(Prttab(LCPSAA))
     &    CALL table(Stci2,Pos1ob,Posfob,11,2,2,dvec,LCPSAA)
       IF(.not.Lfatal.and.Savtab(LCPSAA))
     &    CALL punch(Stci2,Pos1ob,Posfob,LCPSAA,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(Stci2,Pos1ob,Posfob,LCPSAA,Lgraf,F)
       IF(Lfatal)RETURN
c     ------------------------------------------------------------------
C --- DO TEST FOR RESIDUAL SEASONALITY.
c     ------------------------------------------------------------------
       CALL ftest(Stci2,Pos1ob,Posfob,Ny,1,
     &            Prttab(LCPSAA).and.Prttab(LCPRSF),Savtab(LCPRSF))
       IF(.not.Lrndsa)THEN
        IF(Issap.eq.2)THEN
         CALL ssrit(Stci2,Pos1ob,Posfob,3,O2)
         RETURN
        END IF
       END IF
c     ------------------------------------------------------------------
c     compute forcing factor from seasonally adjusted series
c     (BCM May 2006)
c     ------------------------------------------------------------------
       CALL divsub(frcfac,Stci,Stci2,Posfob,lstfrc)
c     ------------------------------------------------------------------
C --- WRITE SEASONALLY ADJUSTED SERIES WITH REVISED YEARLY TOTALS D11A.
c     ------------------------------------------------------------------
       IF(Prttab(LCPFFC))
     &    CALL table(frcfac,Pos1ob,Posfob,11,6,1,dvec,LCPFFC)
       IF((.not.Lfatal).and.Savtab(LCPFFC))
     &    CALL punch(frcfac,Pos1ob,lstfrc,LCPFFC,F,F)
       IF((.not.Lfatal).and.Lgraf)
     &    CALL punch(frcfac,Pos1ob,lstfrc,LCPFFC,Lgraf,F)
       IF(Lfatal)RETURN
      END IF
c     ------------------------------------------------------------------
c     If option selected ensure the rounded seasonally adjusted values
c     equals the rounded seasonally adjusted total.
c     ------------------------------------------------------------------
      IF(Lrndsa)THEN
       CALL rndsa(Stci,Stcirn,Pos1ob,Posfob,rndok)
       IF(rndok)THEN
c     ------------------------------------------------------------------
C --- WRITE rounded SEASONALLY ADJUSTED SERIES
c     ------------------------------------------------------------------
        IF(Prttab(LCPRND))
     &     CALL table(Stcirn,Pos1ob,Posfob,11,2,2,dvec,LCPRND)
        IF(.not.Lfatal.and.Savtab(LCPRND))
     &     CALL punch(Stcirn,Pos1ob,Posfob,LCPRND,F,F)
        IF(.not.Lfatal.and.Lgraf)
     &     CALL punch(Stcirn,Pos1ob,Posfob,LCPRND,Lgraf,F)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
C --- APPLY THE TEST FOR RESIDUAL SEASONALITY
c     ------------------------------------------------------------------
        CALL ftest(Stcirn,ib,ie,Ny,1,Prttab(LCPRND).and.Prttab(LCPRSF),
     &             Savtab(LCPRSF))
        IF(Issap.eq.2)THEN
         CALL ssrit(Stcirn,Pos1ob,Posfob,3,O2)
         RETURN
        END IF
       ELSE
        Lrndsa=F
        IF(Issap.eq.2)RETURN
       END IF
      END IF
c     ------------------------------------------------------------------
C --- WRITE THE FINAL TREND CYCLE D12.
c     ------------------------------------------------------------------
      IF(Prttab(LCPTRN))THEN
       IF(oktrn)THEN
        CALL table(stc2in,Pos1ob,Posfob,12,1,2,dvec,LCPTRN)
       ELSE
        CALL prttrn(stc2in,trnchr,Pos1ob,Posfob,12,LCPTRN)
       END IF
      END IF
      IF(.not.Lfatal.and.Savtab(LCPTRN))
     &   CALL punch(stc2in,Pos1ob,Posfob,LCPTRN,F,F)
      IF(.not.Lfatal.and.Lgraf)
     &   CALL punch(stc2in,Pos1ob,Posfob,LCPTRN,Lgraf,F)
      IF(.not.Lfatal.and.Prttab(LCMPTP))
     &   CALL x11plt(stc2in,stc2in,Pos1ob,Posfob,LCMPTP,0,0,6,1)
c     ------------------------------------------------------------------
C --- WRITE THE FINAL IRREGULAR D13.
c     ------------------------------------------------------------------
      IF(.not.Lfatal.and.(Prttab(LCPIRR).or.Prttab(LCPIPI)))
     &   CALL table(Sti,Pos1ob,Posfob,13,1,3,dvec,LCPIRR)
      IF(.not.Lfatal.and.Savtab(LCPIRR))
     &   CALL punch(Sti,Pos1ob,Posfob,LCPIRR,F,F)
      IF(.not.Lfatal.and.Savtab(LCPIPI))
     &   CALL punch(Sti,Pos1ob,Posfob,LCPIPI,F,Muladd.ne.1)
      IF(.not.Lfatal.and.Lgraf)
     &   CALL punch(Sti,Pos1ob,Posfob,LCPIRR,Lgraf,F)
      IF(.not.Lfatal.and.Prttab(LCMPIP))
     &   CALL x11plt(Sti,Sti,Pos1ob,Posfob,LCMPIP,0,ifac,29,1)
c     ------------------------------------------------------------------
C --- WRITE THE FINAL Adjustment factors D16 (BCM December 2002).
c     ------------------------------------------------------------------
      IF(.not.Lfatal.and.(Prttab(LCPCAF).or.Prttab(LCPIPA)))
     &   CALL table(ststd,Pos1ob,Posfob,16,3,3,dvec,LCPCAF)
      IF(.not.Lfatal.and.Savtab(LCPCAF))
     &   CALL punch(ststd,frstf,lastf,LCPCAF,F,F)
      IF(.not.Lfatal.and.Savtab(LCPIPA))
     &   CALL punch(ststd,frstf,lastf,LCPIPA,F,Muladd.ne.1)
      IF(.not.Lfatal.and.Lgraf)
     &   CALL punch(ststd,frstf,lastf,LCPCAF,Lgraf,F)
c     ------------------------------------------------------------------
C --- WRITE THE FINAL indirect calendar effects D18 (BCM December 2002).
c     ------------------------------------------------------------------
      IF(.not.Lfatal.and.Prttab(LCPFCF))
     &   CALL table(Faccal,Pos1ob,Posfob,18,3,3,dvec,LCPFCF)
      IF(.not.Lfatal.and.Savtab(LCPFCF))
     &   CALL punch(Faccal,frstf,lastf,LCPFCF,F,F)
      IF(.not.Lfatal.and.Lgraf)
     &   CALL punch(Faccal,frstf,lastf,LCPFCF,Lgraf,F)
c     ------------------------------------------------------------------
      IF(.not.Lfatal)Kpart=5
      CALL copy(stc2in,Posffc,-1,Stc2)
      RETURN
c-----------------------------------------------------------------------
 1010 FORMAT('<h2 class="center">',a,a,/,
     &       'Indirect Seasonal Adjustment of Composite Series',a,/,
     &       'U. S. Census Bureau, Release Version ',a,' Build ',a,
     &       '</h2>')
 1020 FORMAT('<p><strong>Series Title-</strong> ',a80,a,/,
     &       '<strong>Series No.-</strong> ',a,'</p>')
 1030 FORMAT('<td class="center">',f10.5,'</td>')
 1040 FORMAT(/,a,'<strong>Period Covered-</strong> ',I2,'/',I4,' to ',
     &         I2,'/',I4,'.</p>')
 1050 FORMAT(/,'<p class="center">There are ',I5,' components in the ',
     &       'composite.</p>')
 1080 FORMAT('indirect:  ',i5)
 1090 FORMAT('indforce: ',a)
c-----------------------------------------------------------------------
      END
