c     Last change:10,2021, pass additional parameter because there is a
c     new argument trendtc in regression
C     previous change:  BCM  15 Apr 2005   12:40 pm
      SUBROUTINE x11pt3(Lgraf,Lttc)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      LOGICAL F,T
      DOUBLE PRECISION ONE,ZERO,PT5
      PARAMETER(F=.false.,T=.true.,ONE=1D0,ZERO=0D0,PT5=0.5D0)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'agr.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'priadj.cmn'
      INCLUDE 'priusr.cmn'
      INCLUDE 'adj.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'inpt.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11fac.cmn'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'x11msc.cmn'
      INCLUDE 'x11log.cmn'
      INCLUDE 'frctbl.i'
      INCLUDE 'x11tbl.i'
      INCLUDE 'revtbl.i'
      INCLUDE 'xrgum.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'x11srs.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'xtrm.cmn'
      INCLUDE 'orisrs.cmn'
      INCLUDE 'adxser.cmn'
      INCLUDE 'force.cmn'
      INCLUDE 'goodob.cmn'
      INCLUDE 'rev.prm'
      INCLUDE 'rev.cmn'
c-----------------------------------------------------------------------
      CHARACTER trnchr*1
      LOGICAL Lgraf,Lttc,rndok,oktrn,oktrf,gudrun,chkfct,negmsg,negfin
      DOUBLE PRECISION Ckhs,ebar,stsx11,stbase,Stex,Stime,Stsie,ststd,
     &                 Temp,biasfc,stsb,tempk,sti2,dvec,Stmcd,sp2,
     &                 stcipc,stc2pc,frcfac,rbak,lbak,temp2
      INTEGER i,i2,ib,ie,ifac,k2,klda,nadj2,lastsf,iadj1,lstfrc,frstsf,
     &        idate
      DIMENSION ststd(PLEN),biasfc(PLEN),Temp(PLEN),stbase(PLEN),
     &          Stsie(PLEN),Stime(PLEN),sp2(PLEN),Stex(PLEN),Ckhs(PLEN),
     &          dvec(1),sti2(PLEN),stsb(PLEN),stsx11(PLEN),
     &          Stmcd(PLEN),trnchr(PLEN),stcipc(PLEN),stc2pc(PLEN),
     &          frcfac(PLEN),temp2(PLEN),idate(2)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      COMMON /work  / Temp
      COMMON /work3 / Stsie
      COMMON /mq5a  / Stmcd,Stime
      COMMON /mq10  / Stex
      COMMON /kcser / Ckhs
c     ------------------------------------------------------------------
      ifac=1
      IF(Muladd.eq.1)ifac=0
      nadj2=0
      IF(Kfmt.gt.0)nadj2=Nadj
      CALL copy(Sprior,PLEN,1,sp2)
      Nfcst=Posffc-Posfob
      oktrn=T
      oktrf=T
      chkfct=Nfcst.gt.0.and.Prttab(LXETRF)
      gudrun=Issap.lt.2.and.Irev.lt.4.and.(Khol.ne.1)
c     ------------------------------------------------------------------
C --- APPLY THE VARIABLE SEASONAL FACTOR ROUTINE FOR AN
C --- ESTIMATE OF THE SEASONAL FACTORS, USING MSR TO SELECT A SEASONAL
C --- FILTER FOR D10.
c-----------------------------------------------------------------------
c     This implements the MSR seasonal filter selection option from
c     X-11-ARIMA/88, developed at Statistics Canada.
c     ------------------------------------------------------------------
c     Moved from x11pt2 by BCM, 11/98, to correctly adjust for extreme
c     with calendarsigma options
c     ------------------------------------------------------------------
      IF(Kfulsm.lt.2.and.Ksdev.le.1)THEN
       IF(Prttab(LXEASF))CALL genSkip(LXEASF)
       CALL sfmsr(Sts,Stsi,Pos1bk,Posfob,Posffc,Prttab(LXEASF),
     &            (Lsumm.gt.0.and.Irev.ne.4).or.Savtab(LXEASF))
      END IF
c     ------------------------------------------------------------------
C --- MULTIPLY (ADD) STSI BY STEX TO GET THE UNMODIFIED SI.
c     ------------------------------------------------------------------
      CALL addmul(Stsie,Stsi,Stex,Pos1bk,Posffc)
c     ------------------------------------------------------------------
C --- WRITE UNMODIFIED SI RATIOS (DIFFERENCES) D8.
c     ------------------------------------------------------------------
      frstsf=Pos1ob
      lastsf=Posfob
      IF(Lgraf.or.Savtab(LX11D8))THEN
       IF(Savfct.and.Nfcst.gt.0)lastsf=Posffc
      END IF
      IF(Prttab(LX11D8))
     &   CALL table(Stsie,Pos1ob,Posfob,8,1,1,dvec,LX11D8)
      IF((.not.Lfatal).and.Savtab(LX11D8))
     &   CALL punch(Stsie,Pos1ob,lastsf,LX11D8,F,F)
      IF((.not.Lfatal).and.Lgraf)
     &   CALL punch(Stsie,Pos1ob,lastsf,LX11D8,Lgraf,F)
      IF(Lfatal)RETURN
C      IF(Lgraf.or.Savtab(LX11D8))THEN
C       IF(Savfct.and.Nfcst.gt.0)lastsf=last0
C      END IF
c     ------------------------------------------------------------------
C --- PERFORM ANALYSIS OF VARIANCE ON THE UNMODIFIED SI RATIOS.
c     ------------------------------------------------------------------
      IF((.not.Lhiddn).and.Khol.ne.1)THEN
       IF(Prttab(LXED8F))THEN
        CALL table(Stsie,0,0,8,2,0,dvec,LXED8F)
        IF(Lfatal)RETURN
       END IF
       CALL ftest(Stsie,Pos1ob,Posfob,Ny,0,Prttab(LXED8F),F)
       CALL kwtest(Stsie,Pos1bk,Posfob,Ny,Prttab(LXED8F))
       CALL addmul(Stsie,Stsi,Stex,Pos1bk,Posffc)
c     ------------------------------------------------------------------
C --- PERFORM F-TEST FOR MOVING SEASONALITY.
c     ------------------------------------------------------------------
       CALL mstest(Stsie,Pos1bk,Posfob,Ny,Prttab(LXED8F))
c     ------------------------------------------------------------------
C --- PERFORM TEST FOR THE PRESENCE OF IDENTIFIABLE SEASONALITY.
c     ------------------------------------------------------------------
       CALL COMBFT(Prttab(LXED8F))
      ELSE IF (Issap.EQ.2) THEN
       CALL FTEST(Stsie,Pos1ob,Posfob,Ny,0,F,F)
       CALL KWTEST(Stsie,Pos1bk,Posfob,Ny,F)
       CALL ADDMUL(Stsie,Stsi,Stex,Pos1bk,Posffc)
       CALL MSTEST(Stsie,Pos1bk,Posfob,Ny,F)
       CALL COMBFT(F)
      END IF
      ebar=ZERO
      IF(Muladd.eq.0)ebar=ONE
c     ------------------------------------------------------------------
      IF(Ksdev.gt.1)THEN
       CALL copy(Stsie,Posfob,1,Stsi)
       CALL replac(Stsi,Temp,Stwt,Pos1bk,Posfob,Ny)
       IF(Kfulsm.lt.2)THEN
        IF(Prttab(LXEASF))CALL genSkip(LXEASF)
        CALL sfmsr(Sts,Stsi,Pos1bk,Posfob,Posffc,Prttab(LXEASF),
     &             (Lsumm.gt.0.and.Irev.ne.4).or.Savtab(LXEASF))
       END IF
      END IF
c     ------------------------------------------------------------------
C --- WRITE UNMODIFIED SI RATIOS (DIFFERENCES) with extreme value labels
c     D8B.  (added by BCM, Jan 2000, revised to add save file May 2000)
c     ------------------------------------------------------------------
      IF(Prttab(LXED8B).or.Savtab(LXED8B).or.(Lsumm.gt.0.and.gudrun))
     &   CALL prtd8b(Stsie,Stwt,Pos1ob,Posfob,LXED8B,Prttab(LXED8B),
     &               Savtab(LXED8B),Lgraf)
c     ------------------------------------------------------------------
C --- IDENTIFY SI RATIOS (DIFFERENCES) WHICH ARE MODIFIED.
c     ------------------------------------------------------------------
      CALL setdp(DNOTST,PLEN,TEMP)
      CALL setdp(ebar,PLEN,Stc2)
      DO i=Pos1bk,Posffc
       IF(.not.dpeq(Stex(i),ebar))Temp(i)=Stsi(i)
      END DO
c     ------------------------------------------------------------------
C --- WRITE FINAL REPLACEMENT VALUES FOR EXTREME SI RATIOS (DIFFERENCES)
C --- D9.
c     ------------------------------------------------------------------
      IF(Lgraf.or.Savtab(LX11D9))THEN
       IF(Savfct.and.Nfcst.gt.0)lastsf=Posffc
      END IF
      IF(Prttab(LX11D9))CALL table(Temp,Pos1ob,Posfob,9,1,5,dvec,LX11D9)
      IF((.not.Lfatal).and.Savtab(LX11D9))
     &   CALL punch(Temp,Pos1ob,lastsf,LX11D9,F,F)
      IF((.not.Lfatal).and.Lgraf)
     &   CALL punch(Temp,Pos1ob,lastsf,LX11D9,Lgraf,F)
      IF(Lfatal)RETURN
      IF(Khol.ne.1.and.Kfulsm.lt.2)THEN
       IF(Prttab(LXED9A))THEN
        CALL table(Temp,0,0,9,2,5,dvec,LXED9A)
        IF(Lfatal)RETURN
       END IF
       IF(Lsumm.gt.0.or.Prttab(LXED9A))THEN
        CALL prtd9a(Prttab(LXED9A))
        IF(Lfatal)RETURN
       END IF
      END IF
c     ------------------------------------------------------------------
C --- COMPUTE YEAR AHEAD SEASONAL FACTORS.
c     ------------------------------------------------------------------
      klda=Posffc+Ny
      IF(Kfulsm.lt.2)CALL forcst(Sts,0,Posffc,klda,Ny,1,PT5,ONE)
c     ------------------------------------------------------------------
C --- DIVIDE (SUBTRACT) D1 BY THE FINAL SEASONAL FACTORS TO OBTAIN A
C --- MODIFIED SEASONALLY ADJUSTED SERIES.
c     ------------------------------------------------------------------
      IF(Savtab(LXED10).or.Savtab(LXEPSF).or.Savtab(LXEARS).or.
     &   Lgraf)THEN
       IF(.not.Savfct)THEN
        lastsf=Posfob
       ELSE IF(Nfcst.gt.0)THEN
        lastsf=Posffc
       ELSE
        lastsf=Posfob+Ny
       END IF
       IF(Savbct)frstsf=Pos1bk
      END IF
      IF(Kfulsm.eq.2)THEN
       CALL copy(Stcsi,Posffc,1,Stci)
       CALL setdp(ebar,klda,Sts)
c     ------------------------------------------------------------------
c     If adjustment for regARIMA seasonal performed, combine with X-11
c     seasonal here (except in psuedo additive case).
c     ------------------------------------------------------------------
       IF((Adjsea.eq.1.or.Adjso.eq.1).or.Savtab(LXEARS))THEN
        CALL copy(Sts,Posffc,1,stsx11)
        IF(Adjsea.eq.1)CALL addmul(Sts,Facsea,Sts,Pos1bk,Posffc)
        IF(Adjso.eq.1)CALL addmul(Sts,Facso,Sts,Pos1bk,Posffc)
c     ------------------------------------------------------------------
c     center the combined seasonal effec, if requested by the user
c     BCM June 2003
c     ------------------------------------------------------------------
        IF(Lcentr)CALL vsfc(Sts,Pos1bk,Posffc,Ny,Lter)
       END IF
c     ------------------------------------------------------------------
       IF(Savtab(LXED10))
     &    CALL punch(Sts,frstsf,lastsf,LXED10,F,F)
       IF(Savtab(LXEPSF))
     &    CALL punch(Sts,frstsf,lastsf,LXEPSF,F,Muladd.ne.1)
       IF((.not.Lfatal).and.Lgraf)
     &    CALL punch(Sts,frstsf,lastsf,LXED10,Lgraf,F)
       IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     Else, print out the pure X-11 seasonal factors if the regARIMA
c     seasonal component is used.
c     ------------------------------------------------------------------
       IF((Adjsea.eq.1.or.Adjso.eq.1).and.Savtab(LXEARS))THEN
        IF(Savtab(LXEARS))
     &     CALL punch(stsx11,frstsf,lastsf,LXEARS,F,F)
        IF((.not.Lfatal).and.Lgraf)
     &     CALL punch(stsx11,frstsf,lastsf,LXEARS,Lgraf,F)
        IF(Lfatal)RETURN
       END IF
c     ------------------------------------------------------------------
      ELSE
       IF(Psuadd)THEN
        DO i=Pos1bk,Posffc
         Stci(i)=Stcsi(i)-Stc(i)*(Sts(i)-ONE)
         IF(Gudval(i).and.Stci(i).le.ZERO)Gudval(i)=F
        END DO
       ELSE
        CALL divsub(Stci,Stcsi,Sts,Pos1bk,Posffc)
        IF(Muladd.eq.2)THEN
         Muladd=0
c     ------------------------------------------------------------------
C --- IF LOGARITHMIC MODEL SELECTED TAKE THE ANTILOG OF THE SEASONAL
C --- FACTORS.
c     ------------------------------------------------------------------
         CALL antilg(Sts,Pos1bk,klda)
         CALL antilg(Stsi,Pos1bk,Posffc)
         CALL antilg(Stsie,Pos1bk,Posffc)
c     ------------------------------------------------------------------
c     set up Stex and Stcsi for the spectral routines - July 2006 BCM
c     ------------------------------------------------------------------
         CALL divsub(Stex,Stsie,Stsi,Pos1bk,Posffc)
         CALL antilg(Stcsi,Pos1bk,Posffc)
        END IF
       END IF
c     ------------------------------------------------------------------
c     If adjustment for regARIMA seasonal performed, combine with X-11
c     seasonal here (except in psuedo additive case).
c     ------------------------------------------------------------------
       IF((Adjsea.eq.1.or.Adjso.eq.1).and.(.not.Psuadd))THEN
        CALL copy(Sts,Posffc,1,stsx11)
        IF(Adjsea.eq.1)CALL addmul(Sts,Facsea,Sts,Pos1bk,Posffc)
        IF(Adjso.eq.1)CALL addmul(Sts,Facso,Sts,Pos1bk,Posffc)
c     ------------------------------------------------------------------
c     center the combined seasonal effec, if requested by the user
c     BCM June 2003
c     ------------------------------------------------------------------
        IF(Lcentr)CALL vsfc(Sts,Pos1bk,Posffc,Ny,Lter)
       END IF
c     ------------------------------------------------------------------
       IF(Ishrnk.gt.0)THEN
        IF(.not.(Adjsea.eq.1.and.(.not.Psuadd)))
     &     CALL copy(Sts,Posffc,1,stsx11)
        CALL shrink(Stsi,Sts,Mtype,Ishrnk,Muladd,Ny)
       END IF
c     ------------------------------------------------------------------
C --- WRITE THE FINAL SEASONAL FACTORS D10.
c     ------------------------------------------------------------------
       IF(Prttab(LXED10).or.Prttab(LXEPSF))
     &    CALL table(Sts,Pos1ob,Posfob,10,1,1,dvec,LXED10)
       IF((.not.Lfatal).and.Savtab(LXED10))
     &    CALL punch(Sts,frstsf,lastsf,LXED10,F,F)
       IF((.not.Lfatal).and.Savtab(LXEPSF))
     &    CALL punch(Sts,frstsf,lastsf,LXEPSF,F,Muladd.ne.1)
       IF((.not.Lfatal).and.Lgraf)
     &    CALL punch(Sts,frstsf,lastsf,LXED10,Lgraf,F)
       IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     Graph the final seasonal factors
c     ------------------------------------------------------------------
       IF(Prttab(LXESFP).and.Khol.ne.1)THEN
        CALL x11plt(Sts,Sts,Pos1ob,Posfob+Ny,LXESFP,0,ifac,7,1)
        IF(Lfatal)RETURN
       END IF
c     ------------------------------------------------------------------
c     If revisions history or sliding spans analysis are done, store
c     the seasonal factors.
c     ------------------------------------------------------------------
       IF(Issap.eq.2)CALL ssrit(Sts,Pos1ob,Posfob,2,Series)
       IF(Irev.eq.4.and.Lrvsf)THEN
        CALL getrev(Sts,Posfob,Muladd,0,Ny,Iag,Iagr)
c     ------------------------------------------------------------------
c     BCM - July 29, 2009
c     Special code to save seasonal factor forecasts for concurrent
c     adjustments.
c     ------------------------------------------------------------------
        IF(Revptr.gt.0.and.Savtab(LRVSSH))THEN
         CALL addate(Rvstrt,Ny,Revptr,idate)
         WRITE(Fhsfh,1120)idate
         DO i=Posfob+1,Posffc
          WRITE(Fhsfh,1130)Sts(i)
         END DO
 1120    FORMAT(2i5)
 1130    FORMAT(1x,e21.14)
        END IF
        IF(.not.(Lrvsa.or.Lrvch.or.Lrvtrn.or.Lrvtch))RETURN
       END IF
       IF((Prttab(LXEARS).or.Savtab(LXEARS).or.Lgraf).and.
     &    (Adjsea.eq.1.or.Adjso.eq.1))THEN
c     ------------------------------------------------------------------
c     Else, print out the pure X-11 seasonal factors if the regARIMA
c     seasonal component is used.
c     ------------------------------------------------------------------
        IF(Prttab(LXEARS))
     &     CALL table(stsx11,Pos1ob,Posfob,10,1,1,dvec,LXEARS)
        IF((.not.Lfatal).and.Savtab(LXEARS))
     &     CALL punch(stsx11,frstsf,lastsf,LXEARS,F,F)
        IF((.not.Lfatal).and.Lgraf)
     &     CALL punch(stsx11,frstsf,lastsf,LXEARS,Lgraf,F)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     If psuedo-additive seasonal adjustment is done, generate and print
c     out the final seasonal differences (Table D 10b).
c     ------------------------------------------------------------------
       ELSE IF(Psuadd)THEN
        DO i=Pos1bk,Posffc
         stsb(i)=Stc(i)*(Sts(i)-ONE)
        END DO
        IF(Prttab(LXEFSD))
     &     CALL table(stsb,Pos1ob,Posfob,10,2,1,dvec,LXEFSD)
        IF((.not.Lfatal).and.Savtab(LXEFSD))THEN
         frstsf=Pos1ob
         IF(Savbct)frstsf=Pos1bk
         lastsf=Posfob
         IF(Savfct)lastsf=Posffc
         CALL punch(stsb,frstsf,lastsf,LXEFSD,F,F)
        END IF
        IF(Lfatal)RETURN
       END IF
c     ------------------------------------------------------------------
       IF(Ishrnk.gt.0.and.(Prttab(LXESNS).or.Savtab(LXESNS)))THEN
        IF(Prttab(LXESNS))THEN
         Ishrnk=2-Ishrnk
         CALL table(stsx11,Pos1ob,Posfob,10,1,1,dvec,LXESNS)
         Ishrnk=2+Ishrnk
         IF(Lfatal)RETURN
        END IF
        IF(Savtab(LXESNS))CALL punch(stsx11,frstsf,lastsf,LXESNS,F,F)
        IF(.not.Lfatal.and.Lgraf)
     &     CALL punch(stsx11,frstsf,lastsf,LXESNS,Lgraf,F)
        IF(Lfatal)RETURN
       END IF
c     ------------------------------------------------------------------
       Muladd=Tmpma
      END IF
c     ------------------------------------------------------------------
      CALL copy(Stci(Pos1bk),Nbfpob,1,Ckhs(Pos1bk))
c      DO i=Pos1bk,Posffc
c       Ckhs(i)=Stci(i)
c      END DO
      klda=Posfob+Ny
      IF(Nfcst.gt.0)klda=Posfob+Nfcst
      k2=klda-Pos1bk+1
c     ------------------------------------------------------------------
c     If length of month/quarter regressor is removed from series, 
c     put mean of effect back into the seasonally adjusted series
c     ------------------------------------------------------------------
c      IF(Nln.gt.0)THEN
c       DO i=Pos1bk,Posffc
c        i2=Frstat+i-Pos1bk
c        IF(Muladd.eq.1)THEN
c         Stci(i)=Stci(i)+LnMean
c        ELSE
c         Stci(i)=Stci(i)*LnMean
c        END IF
c       END DO
c      END IF
      IF(Kfulsm.eq.0.or.Kfulsm.eq.2)THEN
c     ------------------------------------------------------------------
C --- FULL ADJUSTMENT OR TREND ESTIMATION VERSION ONLY. APPLY THE
C --- VARIABLE TREND CYCLE TO THE MODIFIED SEASONALLY ADJUSTED SERIES
C --- TO OBTAIN THE FINAL TREND CYCLE.
c     ------------------------------------------------------------------
       CALL vtc(Stc,Stci)
       IF(Ktcopt.eq.0.and.Lsumm.gt.0.and.gudrun)THEN
        WRITE(Nform,1010)Nterm
 1010   FORMAT('finaltrendma: ',i3)
       END IF
       IF(Muladd.eq.2)THEN
c     ------------------------------------------------------------------
C --- IF THE LOGARITHMIC MODEL WAS SELECTED TAKE THE ANTILOG OF THE
C --- COMPONENTS.
c     ------------------------------------------------------------------
        CALL antilg(Stc,Pos1bk,Posffc)
c     ------------------------------------------------------------------
c --- correct for bias in the trend component
c     ------------------------------------------------------------------
        CALL trbias(Stc,Sts,Sti,Pos1bk,Posffc,biasfc,Ny)
        ebar=ONE
        Muladd=0
       END IF
c-----------------------------------------------------------------------
c     For multiplicative seasonal adjustment, check to see if any
c     of the trend values are negative.
c-----------------------------------------------------------------------
       IF(Muladd.eq.0)THEN
        CALL chktrn(Stc,Kpart,12,trnchr,chkfct,oktrn)
        IF(chkfct)THEN
         i=Posfob
         DO WHILE(oktrf.and.i.le.Posffc)
          IF(oktrf.and.trnchr(i).eq.'*')oktrf=F
          i=i+1
         END DO
        END IF
       END IF
c     ------------------------------------------------------------------
       IF(Adjls.eq.1)CALL addmul(sp2,Facls,sp2,Pos1bk,Posffc)
       IF(Adjao.eq.1)CALL addmul(sp2,Facao,sp2,Pos1bk,Posffc)
       IF(Adjtc.eq.1)CALL addmul(sp2,Factc,sp2,Pos1bk,Posffc)
       IF(Adjusr.eq.1)CALL addmul(sp2,Facusr,sp2,Pos1bk,Posffc)
c     ------------------------------------------------------------------
C --- DIVIDE (SUBTRACT) THE ORIGINAL SERIES BY THE SEASONAL AND TRADING
C --- DAY FACTORS TO OBTAIN THE FINAL SEASONALLY ADJUSTED SERIES.
c     ------------------------------------------------------------------
       IF(Kfulsm.eq.2)THEN
        CALL copy(Series,Posffc,1,Stci)
       ELSE
        IF(Psuadd)THEN
         DO i=Pos1bk,Posffc
          Stci(i)=Series(i)-Stc(i)*(Sts(i)-ONE)
          IF(Gudval(i).and.Stci(i).le.ZERO)Gudval(i)=F
         END DO
        ELSE
         CALL divsub(Stci,Series,Sts,Pos1bk,Posffc)
        END IF
       END IF
c     ------------------------------------------------------------------
C --- SET THE COMBINED FACTORS EQUAL TO THE SEASONAL FACTORS AND IF
C --- TRADING DAY IS REQUESTED MULTIPLY BY THE TRADING-DAY FACTORS.
c     ------------------------------------------------------------------
       IF(Kfulsm.eq.2)THEN
        CALL setdp(ebar,klda,ststd)
       ELSE
        CALL copy(Sts(Pos1bk),k2,1,ststd(Pos1bk))
       END IF
      ELSE
c     ------------------------------------------------------------------
C --- SUMMARY MEASURES VERSION ONLY. REPLACE THE FINAL SEASONALLY
C --- ADJUSTED SERIES WITH D1.
c     ------------------------------------------------------------------
       CALL copy(Stcsi(Pos1bk),Nbfpob,1,Stci(Pos1bk))
c     ------------------------------------------------------------------
C --- APPLY THE VARIABLE TREND CYCLE ROUTINE TO D1.
c     ------------------------------------------------------------------
       CALL vtc(Stc,Stci)
       IF(Muladd.eq.2)THEN
c     ------------------------------------------------------------------
C --- IF THE LOGARITHMIC OPTION WAS SELECTED, TAKE THE ANTILOG
C --- OF THE COMPONENTS.
c     ------------------------------------------------------------------
        CALL antilg(Stc,Pos1bk,Posffc)
c     ------------------------------------------------------------------
c --- correct for bias in the trend component
c     ------------------------------------------------------------------
        CALL trbias(Stc,Sts,Sti,Pos1bk,Posffc,biasfc,Ny)
        ebar=ONE
        Muladd=0
       END IF
c-----------------------------------------------------------------------
c     For multiplicative seasonal adjustment, check to see if any
c     of the trend values are negative.
c-----------------------------------------------------------------------
       IF(Muladd.eq.0)THEN
        CALL chktrn(Stc,Kpart,12,trnchr,chkfct,oktrn)
        IF(chkfct)THEN
         i=Posfob
         DO WHILE(oktrf.and.i.le.Posffc)
          IF(oktrf.and.trnchr(i).eq.'*')oktrf=F
          i=i+1
         END DO
        END IF
       END IF
c     ------------------------------------------------------------------
C --- CHANGE BY BRIAN C. MONSELL 11-1-88, MODIFIED 11-7-89
c     ------------------------------------------------------------------
       IF(Adjls.eq.1)CALL addmul(sp2,Facls,sp2,Pos1bk,Posffc)
       IF(Adjao.eq.1)CALL addmul(sp2,Facao,sp2,Pos1bk,Posffc)
       IF(Adjtc.eq.1)CALL addmul(sp2,Factc,sp2,Pos1bk,Posffc)
       IF(Adjusr.eq.1)CALL addmul(sp2,Facusr,sp2,Pos1bk,Posffc)
c     ------------------------------------------------------------------
C --- END OF CHANGE
C --- REPLACE THE FINAL SEASONALLY ADJUSTED SERIES WITH A1.
c     ------------------------------------------------------------------
       CALL copy(Series(Pos1bk),Nbfpob,1,Ckhs(Pos1bk))
       CALL copy(Series(Pos1bk),Nbfpob,1,Stci(Pos1bk))
c     ------------------------------------------------------------------
C --- SET THE COMBINED FACTORS EQUAL TO THE SEASONAL FACTORS AND IF
C --- TRADING DAY IS REQUESTED MULTIPLY BY THE TRADING-DAY FACTORS.
c     ------------------------------------------------------------------
       CALL copy(Sts(Pos1bk),k2,1,ststd(Pos1bk))
      END IF
c     ------------------------------------------------------------------
      IF(.not.Finhol.and.(Khol.eq.2.or.(Ixreg.gt.0.and.Axrghl).or.
     &   Adjhol.eq.1))THEN
       IF(Haveum)THEN
        IF(Muladd.eq.1)THEN
         CALL setdp(ZERO,PLEN,Faccal)
        ELSE
         CALL setdp(ONE,PLEN,Faccal)
        END IF
        IF(Adjtd.eq.1)THEN
         CALL addmul(Faccal,Faccal,Factd,Pos1bk,klda)
        ELSE IF(Kswv.gt.0)THEN
         CALL addmul(Faccal,Faccal,Stptd,Pos1bk,klda)
        END IF
       ELSE
        CALL divsub(Faccal,Faccal,Fachol,Pos1bk,klda)
       END IF
      END IF
c     ------------------------------------------------------------------
C **** CHANGES TO INCORPORATE NEW ARIMA ESTIMATION ROUTINES
c     ------------------------------------------------------------------
      IF((Adjtd.eq.1.or.(Ixreg.gt.0.and.Axrgtd)).or.(Finhol.and.
     &  (Ixreg.gt.0.and.Axrghl).or.Adjhol.eq.1).or.Kswv.gt.0)THEN
       IF(Kfulsm.eq.0.or.Kfulsm.eq.2)
     &    CALL divsub(Stci,Stci,Faccal,Pos1bk,Posffc)
       CALL addmul(ststd,ststd,Faccal,Pos1bk,klda)
      END IF
c     ------------------------------------------------------------------
C     If prior length of month or leap year adjustments are performed
c     with no trading day, include prior factors into combined
c     adjustment factor.
c     ------------------------------------------------------------------
      IF((Adjtd.eq.0.or.Kswv.eq.0).and.Priadj.gt.1)THEN
       CALL dfdate(Begbak,Begadj,Ny,iadj1)
       iadj1=iadj1+1
       DO i=Pos1bk,klda
        IF(Muladd.eq.1)THEN
         ststd(i)=ststd(i)+Adj(i+iadj1-1)
        ELSE
         ststd(i)=ststd(i)*Adj(i+iadj1-1)
        END IF
       END DO
      END IF
c     ------------------------------------------------------------------
      IF(Nuspad.gt.0.or.Priadj.gt.1)
     &   CALL rmpadj(Stci,Sprior,Pos1bk,Posffc,Muladd)
c     ------------------------------------------------------------------
C --- DIVIDE (SUBTRACT) THE FINAL SEASONALLY ADJUSTED SERIES BY THE
C --- FINAL TREND-CYCLE TO OBTAIN THE FINAL IRREGULAR.
c     ------------------------------------------------------------------
      CALL divsub(Sti,Stci,Stc,Pos1bk,Posffc)
      IF(Finao.and.Nao.gt.0)CALL divsub(Stci,Stci,Facao,Pos1bk,Posffc)
      IF(Finls.and.Nls.gt.0)CALL divsub(Stci,Stci,Facls,Pos1bk,Posffc)
      IF(Fintc.and.Ntc.gt.0)CALL divsub(Stci,Stci,Factc,Pos1bk,Posffc)
      IF(Finusr)CALL divsub(Stci,Stci,Facusr,Pos1bk,Posffc)
c     ------------------------------------------------------------------
      IF(Adjls.eq.1.and.Nls.gt.0)
     &   CALL divsub(Sti,Sti,Facls,Pos1bk,Posffc)
      IF(Adjao.eq.1.and.Nao.gt.0)
     &   CALL divsub(Sti,Sti,Facao,Pos1bk,Posffc)
      IF(Adjtc.eq.1.and.Ntc.gt.0)
     &   CALL divsub(Sti,Sti,Factc,Pos1bk,Posffc)
      IF(Adjusr.eq.1)
     &   CALL divsub(Sti,Sti,Facusr,Pos1bk,Posffc)
c     ------------------------------------------------------------------
C --- IF PRIOR ADJUSTMENT REQUESTED REMOVE IT FROM THE IRREGULAR.
c     ------------------------------------------------------------------
      IF(Nustad.gt.0)THEN
c      IF(Nustad.gt.0)
c     &   CALL rmtadj(Sti,Sprior,Pos1bk,Posffc,Muladd)
c       CALL divsub(Sti,Sti,Sprior,Pos1ob,Posfob)
       DO i=Pos1bk,Posffc
        i2=Frstat+i-Pos1bk
        IF(Muladd.eq.1)THEN
         Sti(i)=Sti(i)-Usrtad(i2)
        ELSE
         Sti(i)=Sti(i)/Usrtad(i2)
        END IF
       END DO
      END IF
c      IF(Khol.eq.1)RETURN
c     ------------------------------------------------------------------
C --- WRITE THE FINAL SEASONALLY ADJUSTED SERIES D11.
c     ------------------------------------------------------------------
      negmsg=F
      IF(dpeq(Cnstnt,DNOTST))THEN
       IF(Prttab(LXED11))THEN
        CALL table(Stci,Pos1ob,Posfob,11,1,2,dvec,LXED11)
c        IF(Nln.gt.0)THEN
c         IF(Ny.eq.12)THEN
c          write(Mt1,1011)'month',LnMean
c         else
c          write(Mt1,1011)'quarter',LnMean
c         end if
c 1011    FORMAT('<p>Average length of ',a,' (',f13.6,
c     &          ') put to seasonally adjusted series.</p>') 
c        END IF
       END IF
       IF((.not.Lfatal).and.Savtab(LXED11))
     &    CALL punch(Stci,Pos1ob,Posfob,LXED11,F,F)
       IF((.not.Lfatal).and.Lgraf)
     &    CALL punch(Stci,Pos1ob,Posfob,LXED11,Lgraf,F)
       IF(Lfatal)RETURN
c     ------------------------------------------------------------------
C --- DO TEST FOR RESIDUAL SEASONALITY.
c     ------------------------------------------------------------------
       CALL ftest(Stci,Pos1ob,Posfob,Ny,1,Prttab(LXERSF),Savtab(LXERSF))
      ELSE
c     ------------------------------------------------------------------
C --- remove constant from seasonally adjusted series and original
c     series.
c     (added by BCM July 2005)
c     ------------------------------------------------------------------
       CALL copy(Stci,Posffc,-1,Stcipc)
       DO i=Pos1ob,Posffc
        Stci(i)=Stci(i)-Cnstnt
        Series(i)=Series(i)-Cnstnt
        IF((.not.(Stci(i).gt.ZERO)).and.Muladd.ne.1)THEN
         IF(.not.negmsg)negmsg=T
         IF(Iyrt.gt.0)Stci(i)=ZERO
        END IF
       END DO
c     ------------------------------------------------------------------
C --- WRITE THE FINAL SEASONALLY ADJUSTED SERIES D11 without the
c     constant.
c     ------------------------------------------------------------------
       IF(Prttab(LXED11))
     &    CALL table(Stci,Pos1ob,Posfob,11,1,2,dvec,LXED11)
       IF((.not.Lfatal).and.Savtab(LXED11))
     &    CALL punch(Stci,Pos1ob,Posfob,LXED11,F,F)
       IF((.not.Lfatal).and.Lgraf)
     &    CALL punch(Stci,Pos1ob,Posfob,LXED11,Lgraf,F)
c     ------------------------------------------------------------------
       IF(negmsg.and.gudrun)THEN
        CALL nWritln('Negative values were created in the seasonally'//
     &               ' adjusted series when ',Mt1,Mt2,T,F)
        CALL writln(' removing the temporary constant.',Mt1,Mt2,F,T)
        IF(Iyrt.gt.0)THEN
         CALL writln('Negative values in the seasonally '//
     &               'adjusted series were replaced by ',Mt1,Mt2,T,F)
         CALL writln(' zero before forcing annual totals.',
     &               Mt1,Mt2,F,T)
        END IF
       END IF
c     ------------------------------------------------------------------
C --- DO TEST FOR RESIDUAL SEASONALITY.
c     ------------------------------------------------------------------
       CALL ftest(Stci,Pos1ob,Posfob,Ny,1,Prttab(LXERSF),Savtab(LXERSF))
c     ------------------------------------------------------------------
C --- WRITE THE FINAL SEASONALLY ADJUSTED SERIES D11 including the
c     constant.
c     ------------------------------------------------------------------
       IF(Prttab(LXESAC))
     &    CALL table(Stcipc,Pos1ob,Posfob,11,3,2,dvec,LXESAC)
       IF((.not.Lfatal).and.Savtab(LXESAC))
     &    CALL punch(Stcipc,Pos1ob,Posfob,LXESAC,F,F)
       IF((.not.Lfatal).and.Lgraf)
     &    CALL punch(Stcipc,Pos1ob,Posfob,LXESAC,Lgraf,F)
      END IF
c     ------------------------------------------------------------------
c     Store value of seasonally adjusted series for sliding spans
c     analysis.
c     ------------------------------------------------------------------
      IF(Issap.eq.2.and.Iyrt.eq.0.and.(.not.Lrndsa))THEN
       CALL ssrit(Stci,Pos1ob,Posfob,3,Series)
       RETURN
      END IF
c     ------------------------------------------------------------------
c     Store value of seasonally adjusted series for revisions analysis.
c     ------------------------------------------------------------------
      IF(Irev.eq.4.and.(Lrvsa.or.Lrvch).and.Iyrt.eq.0)THEN
       CALL getrev(Stci,Posfob,Muladd,1,Ny,Iag,Iagr)
       IF(.not.(Lrvtrn.or.Lrvtch))RETURN
      END IF
c     ------------------------------------------------------------------
      IF(Nfcst.gt.0)THEN
       IF(Prttab(LXESAF))
     &    CALL table(Stci,Posfob+1,Posffc,11,1,2,dvec,LXESAF)
       IF((.not.Lfatal).and.Savtab(LXESAF))
     &    CALL punch(Stci,Posfob+1,Posffc,LXESAF,F,F)
       IF(Lfatal)RETURN
      END IF
      IF(Prttab(LXESAP))
     &   CALL x11plt(Stci,Stci,Pos1ob,Posfob,LXESAP,0,0,6,1)
c     ------------------------------------------------------------------
C --- IF OPTION SELECTED ADJUST YEARLY TOTALS OF D11 TO EQUAL THE YEARLY
C --- TOTALS OF THE ORIGINAL SERIES.
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
c     Based on value of Iftrgt, set up target (stbase) for forcing the
c     seasonally adjusted series (BCM, May 2003)
c     ------------------------------------------------------------------
       IF(Iftrgt.eq.0)THEN
        CALL copy(Series,lstfrc,1,stbase)
       ELSE IF(Iftrgt.eq.1)THEN
        CALL copy(Stocal,lstfrc,1,stbase)
       ELSE
        CALL copy(Stopp,lstfrc,1,stbase)
        IF(Iftrgt.eq.3)CALL divsub(stbase,stbase,Faccal,Pos1ob,lstfrc)
       END IF
c     ------------------------------------------------------------------
       IF(Iyrt.eq.1)THEN
        CALL qmap(stbase,Stci,Stci2,Pos1ob,lstfrc,Ny,ib,ie,Begyrt)
c     ------------------------------------------------------------------
c     Change made October 1995 to duplicate X-11-ARIMA/88 partial year
c     adjustment of yearly totals. BCM
c     ------------------------------------------------------------------
        IF(ie.lt.lstfrc)THEN
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
         DO i=Pos1ob,ib-1
          Stci2(i)=Stci(i)+tempk
         END DO
        END IF
       ELSE IF(Iyrt.eq.2)THEN
        CALL qmap2(stbase,Stci,Stci2,Pos1ob,lstfrc,Ny,Iagr)
       END IF
c     ------------------------------------------------------------------
c     check for values <= 0 in Stci2
c     ------------------------------------------------------------------
       negmsg=F
       negfin=F
       IF(Muladd.ne.1.and.(.not.dpeq(Cnstnt,DNOTST)))THEN
        DO i=Pos1ob,lstfrc
         IF(.not.(Stci2(i).gt.ZERO))THEN
          IF(.not.negmsg)negmsg=T
          Stci2(i)=ZERO
         END IF
        END DO
c     ------------------------------------------------------------------
c     if so, correct the negative values by replacing the values < 0
c     by prorating the modified SAA series with the target series
c     programmed by BCM from suggestion by Susie Fortier, June 2006
c     ------------------------------------------------------------------
        IF(negmsg)THEN
         rbak=Rol
         lbak=Lamda
         Rol=ZERO
         Lamda=0.5D0
         CALL qmap2(stbase,Stci2,temp2,Pos1ob,lstfrc,Ny,Iagr)
         Rol = rbak
         Lamda = lbak
         CALL copy(temp2,PLEN,1,Stci2)
c     ------------------------------------------------------------------
c     check to see if Stci2 has observations that are <= 0
c     ------------------------------------------------------------------
         DO i=Pos1ob,lstfrc
          IF(.not.(Stci2(i).gt.ZERO))THEN
           IF(.not.negfin)negfin=T
          END IF
         END DO
        END IF
       END IF
c     ------------------------------------------------------------------
C --- WRITE SEASONALLY ADJUSTED SERIES WITH REVISED YEARLY TOTALS D11A.
c     ------------------------------------------------------------------
       IF(Prttab(LFCSAA))
     &    CALL table(Stci2,Pos1ob,Posfob,11,2,2,dvec,LFCSAA)
       IF((.not.Lfatal).and.Savtab(LFCSAA))
     &    CALL punch(Stci2,Pos1ob,Posfob,LFCSAA,F,F)
       IF((.not.Lfatal).and.Lgraf)
     &    CALL punch(Stci2,Pos1ob,Posfob,LFCSAA,Lgraf,F)
       IF(Lfatal)RETURN
c     ------------------------------------------------------------------
       IF(negmsg)THEN
        CALL nWritln('Values &le; 0 were found in the forced '//
     &               'seasonally adjusted series.',Mt1,Mt2,T,F)
        IF(Iyrt.gt.0)THEN
         CALL writln('      These values were corrected by replacing '//
     &              'the negative values with zero',Mt1,Mt2,F,F)
         CALL writln('      and prorating the modified forced '//
     &               'seasonally adjusted series with ',Mt1,Mt2,F,F)
         CALL writln('      the target series.',Mt1,Mt2,F,T)
        ELSE
         CALL writln('      These values were corrected by replacing '//
     &              'the negative values with zero.',Mt1,Mt2,F,T)
        END IF
       END IF
c     ------------------------------------------------------------------
C --- APPLY THE TEST FOR RESIDUAL SEASONALITY
c     ------------------------------------------------------------------
       CALL ftest(Stci2,Pos1ob,Posfob,Ny,1,Prttab(LFCSAA).and.
     &            Prttab(LXERSF),Savtab(LXERSF))
       IF(.not.Lrndsa)THEN
c     ------------------------------------------------------------------
c     Store value of seasonally adjusted series for sliding spans
c     analysis.
c     ------------------------------------------------------------------
        IF(Issap.eq.2)THEN
         CALL ssrit(Stci2,Pos1ob,Posfob,3,Series)
         RETURN
        END IF
c     ------------------------------------------------------------------
c     Store value of seasonally adjusted series for revision history
c     analysis.
c     ------------------------------------------------------------------
        IF(Irev.eq.4.and.(Lrvsa.or.Lrvch))THEN
         CALL getrev(Stci2,Posfob,Muladd,1,Ny,Iag,Iagr)
         IF(.not.(Lrvtrn.or.Lrvtch))RETURN
        END IF
       END IF
c     ------------------------------------------------------------------
c     compute forcing factor from seasonally adjusted series
c     (BCM May 2006)
c     ------------------------------------------------------------------
c     If there are observations <= 0, set the forcing factor to DNOTST
c     and print a warning message.
c     (BCM July 2006)
c     ------------------------------------------------------------------
       IF(negfin)THEN
        DO i=Pos1ob,lstfrc
         IF(.not.(Stci2(i).gt.ZERO))THEN
          frcfac(i)=DNOTST
         ELSE
          frcfac(i)=Stci(i)/Stci2(i)
         END IF
        END DO
       ELSE
        CALL divsub(frcfac,Stci,Stci2,Pos1ob,lstfrc)
       END IF
c     ------------------------------------------------------------------
C --- WRITE SEASONALLY ADJUSTED SERIES WITH REVISED YEARLY TOTALS D11A.
c     ------------------------------------------------------------------
       IF(Prttab(LFRFAC))
     &    CALL table(frcfac,Pos1ob,Posfob,11,6,1,dvec,LFRFAC)
       IF((.not.Lfatal).and.Savtab(LFRFAC))
     &    CALL punch(frcfac,Pos1ob,lstfrc,LFRFAC,F,F)
       IF((.not.Lfatal).and.Lgraf)
     &    CALL punch(frcfac,Pos1ob,lstfrc,LFRFAC,Lgraf,F)
       IF(Lfatal)RETURN
c     ------------------------------------------------------------------
       IF(negmsg)THEN
        CALL nWritln('Values <= 0 were found in the final forced '//
     &               'seasonally adjusted series.',Mt1,Mt2,T,F)
        CALL writln('      Forcing factors for these observations '//
     &              'were set to -999.',Mt1,Mt2,F,T)
       END IF
      ELSE
c     ------------------------------------------------------------------
c     copy Seasonally adjusted series into seasonally adjusted forced
c     series to allow formation of indirect forced seasonally adjusted
c     series (BCM May 2006)
c     ------------------------------------------------------------------
       CALL copy(Stci,Posffc,1,Stci2)
      END IF
c     ------------------------------------------------------------------
c     If option selected ensure the rounded seasonally adjusted values
c     equals the rounded seasonally adjusted total.
c     ------------------------------------------------------------------
      IF(Lrndsa)THEN
       CALL rndsa(Stci2,Stcirn,Pos1ob,Posfob,rndok)
c     ------------------------------------------------------------------
C --- WRITE rounded SEASONALLY ADJUSTED SERIES
c     ------------------------------------------------------------------
       IF(rndok)THEN
        IF(Prttab(LFCRND))
     &     CALL table(Stcirn,Pos1ob,Posfob,11,3,2,dvec,LFCRND)
        IF((.not.Lfatal).and.Savtab(LFCRND))
     &     CALL punch(Stcirn,Pos1ob,Posfob,LFCRND,F,F)
        IF((.not.Lfatal).and.Lgraf)
     &     CALL punch(Stcirn,Pos1ob,Posfob,LFCRND,Lgraf,F)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
C --- APPLY THE TEST FOR RESIDUAL SEASONALITY
c     ------------------------------------------------------------------
        CALL ftest(Stcirn,Pos1ob,Posfob,Ny,1,
     &             Prttab(LFCRND).and.Prttab(LXERSF),Savtab(LXERSF))
c     ------------------------------------------------------------------
c     Store value of rounded seasonally adjusted series for sliding
c     spans analysis.
c     ------------------------------------------------------------------
        IF(Issap.eq.2)THEN
         CALL ssrit(Stcirn,Pos1ob,Posfob,3,Series)
         RETURN
        END IF
       ELSE
        Lrndsa=F
        IF(Issap.eq.2)RETURN
       END IF
c     ------------------------------------------------------------------
c     Store value of seasonally adjusted series for revision history
c     analysis.
c     ------------------------------------------------------------------
       IF(Irev.eq.4.and.(Lrvsa.or.Lrvch))THEN
        CALL getrev(Stcirn,Posfob,Muladd,1,Ny,Iag,Iagr)
        IF(.not.(Lrvtrn.or.Lrvtch))RETURN
       END IF
      END IF
c     ------------------------------------------------------------------
C --- WRITE THE FINAL TREND CYCLE D12.  
c     ------------------------------------------------------------------
C --- If level shift outliers were
c     removed, put them in the final trend cycle.
c     ------------------------------------------------------------------
      IF(((.not.Finls).and.Adjls.eq.1).or.(Nustad.gt.0.and.Lprntr).or.
     &  ((.not.Fintc).and.Lttc.and.Adjtc.eq.1))THEN
       CALL copy(Stc,PLEN,1,stc2)
       IF((.not.Finls).and.Adjls.eq.1)
     &    CALL addmul(stc2,Facls,Stc,Pos1bk,Posffc)
       IF((.not.Fintc).and.Lttc.and.Adjtc.eq.1)
     &     CALL addmul(stc2,Factc,stc2,Pos1bk,Posffc)
c     ------------------------------------------------------------------
c     If there are temporary adjustment factors, put them back into the
c     Trend
c     ------------------------------------------------------------------
       IF(Nustad.gt.0.and.Lprntr)THEN
        DO i=Pos1bk,Posffc
         i2=Frstat+i-Pos1bk
         IF(Muladd.eq.1)THEN
          stc2(i)=stc2(i)+Usrtad(i2)
         ELSE
          stc2(i)=stc2(i)*Usrtad(i2)
         END IF
        END DO
       END IF
c     ------------------------------------------------------------------
C --- remove constant from final trend.   (added by BCM July 2005)
c     ------------------------------------------------------------------
       IF(.not.dpeq(Cnstnt,DNOTST))THEN
        CALL copy(stc2,Posffc,-1,stc2pc)
        DO i=Pos1ob,Posffc
         stc2(i)=stc2(i)-Cnstnt
        END DO
       END IF
c     ------------------------------------------------------------------
       IF(Prttab(LXED12))THEN
        IF(oktrn)THEN
         CALL table(stc2,Pos1ob,Posfob,12,1,2,dvec,LXED12)
        ELSE
         CALL prttrn(stc2,trnchr,Pos1ob,Posfob,12,LXED12)
        END IF
       END IF
       IF((.not.Lfatal).and.Savtab(LXED12))
     &    CALL punch(stc2,Pos1ob,Posfob,LXED12,F,F)
       IF((.not.Lfatal).and.Lgraf)
     &    CALL punch(stc2,Pos1ob,Posfob,LXED12,Lgraf,F)
       IF(Lfatal)RETURN
c     ------------------------------------------------------------------
       IF(Nfcst.gt.0)THEN
        IF(Prttab(LXETRF))THEN
         IF(oktrf)THEN
          CALL table(stc2,Posfob+1,Posffc,12,1,2,dvec,LXETRF)
         ELSE
          CALL prttrn(stc2,trnchr,Posfob+1,Posffc,12,LXETRF)
         END IF
        END IF
        IF((.not.Lfatal).and.Savtab(LXETRF))
     &     CALL punch(stc2,Posfob+1,Posffc,LXETRF,F,F)
        IF(Lfatal)RETURN
       END IF
c     ------------------------------------------------------------------
       IF(Prttab(LXETRP))
     &    CALL x11plt(stc2,stc2,Pos1ob,Posfob,LXETRP,1,0,6,1)
       IF((.not.Lfatal).and.Prttab(LXETAL))THEN
        IF(oktrn)THEN
         CALL table(Stc,Pos1ob,Posfob,12,1,2,dvec,LXETAL)
        ELSE
         CALL prttrn(Stc,trnchr,Pos1ob,Posfob,12,LXETAL)
        END IF
       END IF
       IF((.not.Lfatal).and.Savtab(LXETAL))
     &    CALL punch(Stc,Pos1ob,Posfob,LXETAL,F,F)
       IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     print and/or save trend with constant added (BCM, July 2005
c     ------------------------------------------------------------------
       IF(.not.dpeq(Cnstnt,DNOTST))THEN
        IF(Prttab(LXETAC))THEN
         IF(oktrn)THEN
          CALL table(stc2pc,Pos1ob,Posfob,12,1,2,dvec,LXETAC)
         ELSE
          CALL prttrn(stc2pc,trnchr,Pos1ob,Posfob,12,LXETAC)
         END IF
        END IF
        IF((.not.Lfatal).and.Savtab(LXETAC))
     &     CALL punch(stc2pc,Pos1ob,Posfob,LXETAC,F,F)
        IF(Lfatal)RETURN
       END IF
c     ------------------------------------------------------------------
      ELSE
c     ------------------------------------------------------------------
C --- remove constant from final trend.   (added by BCM July 2005)
c     ------------------------------------------------------------------
       IF(.not.dpeq(Cnstnt,DNOTST))THEN
        CALL copy(Stc,Posffc,-1,stc2pc)
        DO i=Pos1ob,Posffc
         Stc(i)=Stc(i)-Cnstnt
        END DO
       END IF
c     ------------------------------------------------------------------
       IF(Prttab(LXED12))THEN
        IF(oktrn)THEN
         CALL table(Stc,Pos1ob,Posfob,12,1,2,dvec,LXED12)
        ELSE
         CALL prttrn(Stc,trnchr,Pos1ob,Posfob,12,LXED12)
        END IF
       END IF
       IF((.not.Lfatal).and.Savtab(LXED12))
     &    CALL punch(Stc,Pos1ob,Posfob,LXED12,F,F)
       IF((.not.Lfatal).and.Lgraf)
     &    CALL punch(Stc,Pos1ob,Posfob,LXED12,Lgraf,F)
       IF(Lfatal)RETURN
c     ------------------------------------------------------------------
       IF(Nfcst.gt.0)THEN
        IF(Prttab(LXETRF))THEN
         IF(oktrf)THEN
          CALL table(Stc,Posfob+1,Posffc,12,1,2,dvec,LXETRF)
         ELSE
          CALL prttrn(Stc,trnchr,Posfob+1,Posffc,12,LXETRF)
         END IF
        END IF
        IF((.not.Lfatal).and.Savtab(LXETRF))
     &     CALL punch(Stc,Posfob+1,Posffc,LXETRF,F,F)
        IF(Lfatal)RETURN
       END IF
c     ------------------------------------------------------------------
       IF(Prttab(LXETRP))
     &    CALL x11plt(Stc,Stc,Pos1ob,Posfob,LXETRP,0,0,6,1)
       IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     print and/or save trend with constant added (BCM, July 2005
c     ------------------------------------------------------------------
       IF(.not.dpeq(Cnstnt,DNOTST))THEN
        IF(Prttab(LXETAC))THEN
         IF(oktrn)THEN
          CALL table(stc2pc,Pos1ob,Posfob,12,1,2,dvec,LXETAC)
         ELSE
          CALL prttrn(stc2pc,trnchr,Pos1ob,Posfob,12,LXETAC)
         END IF
        END IF
        IF((.not.Lfatal).and.Savtab(LXETAC))
     &     CALL punch(stc2pc,Pos1ob,Posfob,LXETAC,F,F)
        IF(Lfatal)RETURN
       END IF
      END IF
      IF(Irev.eq.4)THEN
       IF((.not.Finls).and.Adjls.eq.1)THEN
        CALL getrev(stc2,Posfob,Muladd,2,Ny,Iag,Iagr)
       ELSE
        CALL getrev(Stc,Posfob,Muladd,2,Ny,Iag,Iagr)
       END IF
       RETURN
      END IF
c     ------------------------------------------------------------------
C --- If this is a log-additive seasonal adjustment, print out the
C --- bias correction factors for the trend.
c     ------------------------------------------------------------------
      IF(Tmpma.eq.2)THEN
       IF(Prttab(LXEBCF))
     &    CALL table(biasfc,Pos1ob,Posfob,12,3,1,dvec,LXEBCF)
       IF((.not.Lfatal).and.Savtab(LXEBCF))
     &    CALL punch(biasfc,Pos1ob,Posfob,LXEBCF,F,F)
       IF(Lfatal)RETURN
      END IF
c     ------------------------------------------------------------------
C --- WRITE THE FINAL IRREGULAR D13.
c     ------------------------------------------------------------------
      IF(Adjao.eq.1.or.(Adjtc.eq.1.and.(.not.Lttc)))THEN
       CALL copy(Sti,Posffc,1,sti2)
       IF(Adjao.eq.1)CALL addmul(sti2,Facao,sti2,Pos1bk,Posffc)
       IF((.not.Lttc).and.Adjtc.eq.1)
     &    CALL addmul(sti2,Factc,sti2,Pos1bk,Posffc)
       IF(Prttab(LXED13).or.Prttab(LXEPIR))
     &    CALL table(sti2,Pos1ob,Posfob,13,1,3,dvec,LXED13)
       IF((.not.Lfatal).and.Savtab(LXED13))
     &    CALL punch(sti2,Pos1ob,Posfob,LXED13,F,F)
       IF((.not.Lfatal).and.Savtab(LXEPIR))
     &    CALL punch(sti2,Pos1ob,Posfob,LXEPIR,F,Muladd.ne.1)
       IF((.not.Lfatal).and.Lgraf)
     &    CALL punch(sti2,Pos1ob,Posfob,LXED13,Lgraf,F)
       IF((.not.Lfatal).and.Prttab(LXEIRP))
     &    CALL x11plt(sti2,sti2,Pos1ob,Posfob,LXEIRP,1,ifac,29,1)
       IF((.not.Lfatal).and.Prttab(LXEIAO))
     &    CALL table(Sti,Pos1ob,Posfob,13,2,3,dvec,LXEIAO)
       IF((.not.Lfatal).and.Savtab(LXEIAO))
     &    CALL punch(Sti,Pos1ob,Posfob,LXEIAO,F,F)
       IF(Lfatal)RETURN
c     ------------------------------------------------------------------
      ELSE
       IF(Prttab(LXED13).or.Prttab(LXEPIR))
     &    CALL table(Sti,Pos1ob,Posfob,13,1,3,dvec,LXED13)
       IF((.not.Lfatal).and.Savtab(LXED13))
     &    CALL punch(Sti,Pos1ob,Posfob,LXED13,F,F)
       IF((.not.Lfatal).and.Savtab(LXEPIR))
     &    CALL punch(sti2,Pos1ob,Posfob,LXEPIR,F,Muladd.ne.1)
       IF((.not.Lfatal).and.Lgraf)
     &    CALL punch(Sti,Pos1ob,Posfob,LXED13,Lgraf,F)
       IF((.not.Lfatal).and.Prttab(LXEIRP))
     &    CALL x11plt(Sti,Sti,Pos1ob,Posfob,LXEIRP,0,ifac,29,1)
       IF (Lfatal) RETURN
      END IF
      IF(Khol.eq.1)RETURN
c     ------------------------------------------------------------------
C --- IF TRADING DAY IS APPLIED WRITE COMBINED SEASONAL AND TRADING
C --- DAY FACTORS D16.
c     ------------------------------------------------------------------
      IF(Prttab(LXED16).or.Prttab(LXEPAF))
     &   CALL table(ststd,Pos1ob,Posfob,16,1,1,dvec,LXED16)
      IF((Savtab(LXED16).or.Savtab(LXEPAF).or.Lgraf).and.
     &   (.not.Lfatal))THEN
       IF(.not.Savfct)THEN
        lastsf=Posfob
       ELSE IF(Nfcst.gt.0)THEN
        lastsf=Posffc
       ELSE
        lastsf=Posfob+Ny
       END IF
       frstsf=Pos1ob
       IF(Savbct)frstsf=Pos1bk
       IF((.not.Lfatal).and.Savtab(LXED16))
     &    CALL punch(ststd,frstsf,lastsf,LXED16,F,F)
       IF((.not.Lfatal).and.Savtab(LXEPAF))
     &    CALL punch(ststd,frstsf,lastsf,LXEPAF,F,Muladd.ne.1)
       IF((.not.Lfatal).and.Lgraf)
     &    CALL punch(ststd,frstsf,lastsf,LXED16,Lgraf,F)
       IF(Lfatal)RETURN
      END IF
c     ------------------------------------------------------------------
c     If psuedo-additive seasonal adjustment is done, generate and print
c     out the final adjustment differences (Table D 16b).
c     ------------------------------------------------------------------
      IF(Psuadd.and.Kfulsm.lt.2)THEN
       DO i=Pos1bk,Posffc
        stsb(i)=Series(i)-Stci(i)
       END DO
       IF(Prttab(LXEFAD))
     &    CALL table(stsb,Pos1ob,Posfob,10,2,1,dvec,LXEFAD)
       IF((.not.Lfatal).and.Savtab(LXEFAD))THEN
        IF(Savfct.and.Nfcst.gt.0)THEN
         CALL punch(stsb,Pos1ob,Posffc,LXEFAD,F,F)
        ELSE
         CALL punch(stsb,Pos1ob,Posfob,LXEFAD,F,F)
        END IF
        IF(Lfatal)RETURN
       END IF
      END IF
c     ------------------------------------------------------------------
      IF((Adjtd.eq.1.or.(Ixreg.gt.0.and.Axrgtd)).or.(Finhol.and.
     &  (Ixreg.gt.0.and.Axrghl).or.Adjhol.eq.1))THEN
       IF(Prttab(LXED18))
     &    CALL table(Faccal,Pos1ob,Posfob,18,1,1,dvec,LXED18)
       IF((.not.Lfatal).and.(Savtab(LXED18).or.Lgraf))THEN
       IF(.not.Savfct)THEN
         lastsf=Posfob
        ELSE IF(Nfcst.gt.0)THEN
         lastsf=Posffc
        ELSE
         lastsf=Posfob+Ny
        END IF
        frstsf=Pos1ob
        IF(Savbct)frstsf=Pos1bk
        IF((.not.Lfatal).and.Savtab(LXED18))
     &     CALL punch(Faccal,frstsf,lastsf,LXED18,F,F)
        IF((.not.Lfatal).and.Lgraf)
     &     CALL punch(Faccal,frstsf,lastsf,LXED18,Lgraf,F)
        IF(Lfatal)RETURN
       END IF
      END IF
c     ------------------------------------------------------------------
C --- BEGIN PART E.
c     ------------------------------------------------------------------
      Kpart=5
c     ------------------------------------------------------------------
C --- COMPUTE THE MODIFIED ORIGINAL, SEASONALLY ADJUSTED, AND IRREGULAR
C --- SERIES BY REPLACING THOSE VALUES WHICH WERE ASSIGNED A WEIGHT OF
C --- ZERO IN TABLE C17.
c     ------------------------------------------------------------------
      DO i=Pos1bk,Posffc
       IF(Stwt(i).gt.ZERO)THEN
        Stome(i)=Series(i)
        Stime(i)=Sti(i)
        Stcime(i)=Stci(i)
        IF(.not.dpeq(Cnstnt,DNOTST))THEN
         Stome(i)=Stome(i)+Cnstnt
         Stcime(i)=Stcime(i)+Cnstnt
        END IF
       ELSE
c     ------------------------------------------------------------------
C ---  REPLACE THE SEASONALLY ADJUSTED SERIES WITH THE FINAL TREND CYCLE
c     ------------------------------------------------------------------
        IF(((.not.Finls).and.Adjls.eq.1).or.(Nustad.gt.0.and.
     &     Lprntr))THEN
         Stcime(i)=Stc2(i)
        ELSE
         Stcime(i)=Stc(i)
        END IF
        IF(.not.dpeq(Cnstnt,DNOTST))Stcime(i)=Stcime(i)+Cnstnt
c     ------------------------------------------------------------------
C ---  REPLACE THE IRREGULAR BY ITS EXPECTED VALUE.
c     ------------------------------------------------------------------
        Stime(i)=ebar
c     ------------------------------------------------------------------
C ---  REPLACE THE ORIGINAL SERIES BY COMBINING THE TREND CYCLE,
C ---  SEASONAL, TRADING DAY AND PRIOR COMPONENTS.
c     ------------------------------------------------------------------
        Stome(i)=Series(i)
        IF(.not.dpeq(Cnstnt,DNOTST))Stome(i)=Stome(i)+Cnstnt
        IF(Muladd.eq.1)THEN
         Stome(i)=Stome(i)-Sti(i)
        ELSE
         Stome(i)=Stome(i)/Sti(i)
        END IF
        IF(nadj2.gt.0)THEN
         IF(Muladd.eq.1)THEN
          Stcime(i)=Stcime(i)+Sprior(i)
         ELSE
          Stcime(i)=Stcime(i)*Sprior(i)
         END IF
        END IF
c     ------------------------------------------------------------------
c     change by brian monsell 9/96
c     ------------------------------------------------------------------
c     Check to see if level shift outlier is adjusted for; replace value
c     of outier in seasonally adjusted series only if Finls is false
c     ------------------------------------------------------------------
        IF(Finls)THEN
         IF(Muladd.eq.1)THEN
          Stcime(i)=Stcime(i)-Facls(i)
         ELSE
          Stcime(i)=Stcime(i)/Facls(i)
         END IF
        END IF
c     ------------------------------------------------------------------
c     end of change by brian monsell 9/96
c     ------------------------------------------------------------------
       END IF
      END DO
c     ------------------------------------------------------------------
c     change by brian monsell 9/96
c     ------------------------------------------------------------------
c     if ao outliers are removed from series prior to seasonal
c     adjustment, then remove them from these values as well.
c     ------------------------------------------------------------------
      IF(Adjao.eq.1)THEN
       CALL divsub(Stome,Stome,Facao,Pos1bk,Posffc)
       IF(.not.Finao)CALL divsub(Stcime,Stcime,Facao,Pos1bk,Posffc)
c       CALL divsub(Stime,Stime,Facao,Pos1bk,Posffc)
      END IF
      IF(Adjtc.eq.1)THEN
       CALL divsub(Stome,Stome,Factc,Pos1bk,Posffc)
       IF(.not.Fintc)CALL divsub(Stcime,Stcime,Factc,Pos1bk,Posffc)
c       CALL divsub(Stime,Stime,Factc,Pos1bk,Posffc)
      END IF
      IF(.not.dpeq(Cnstnt,DNOTST))THEN
       DO i=Pos1bk,Posffc
        Stome(i)=Stome(i)-Cnstnt
        Stcime(i)=Stcime(i)-Cnstnt
       END DO
      END IF
c     ------------------------------------------------------------------
c     end of change by brian monsell 9/96
c     ------------------------------------------------------------------
      IF(Adjls.eq.1.or.Adjusr.eq.1.or.Adjao.eq.1.or.Adjtc.eq.1)THEN
       CALL copy(sp2,PLEN,1,Sprior)
       IF(nadj2.eq.0)nadj2=Posffc-Pos1bk+1
       IF(Kfmt.eq.0)Kfmt=1
      END IF
      RETURN
c     ------------------------------------------------------------------
      END
