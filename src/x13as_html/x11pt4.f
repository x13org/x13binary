c     Last Change: Oct,2021- add a new argument trendtc in regression
C     previous change:  BCM  17 Apr 2003   11:12 pm
      SUBROUTINE x11pt4(Lgraf,Lttc)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'stdio.i'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11fac.cmn'
      INCLUDE 'adj.cmn'
      INCLUDE 'agr.cmn'
      INCLUDE 'agrsrs.cmn'
      INCLUDE 'orisrs.cmn'
      INCLUDE 'inpt.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'priusr.cmn'
      INCLUDE 'x11msc.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'x11log.cmn'
      INCLUDE 'x11srs.cmn'
      INCLUDE 'x11tbl.i'
      INCLUDE 'frctbl.i'
      INCLUDE 'cmptbl.i'
      INCLUDE 'inpt2.cmn'
      INCLUDE 'work2.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'adxser.cmn'
      INCLUDE 'force.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'goodob.cmn'
      INCLUDE 'tdtyp.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO,ONE,ONEHND,TWO,SIX
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.,ZERO=0D0,ONE=1D0,ONEHND=100D0,
     &          TWO=2D0,SIX=6D0)
c-----------------------------------------------------------------------
      LOGICAL lsame,Lgraf,pre18b,gudbak,allgud,Lttc
      DOUBLE PRECISION ebar,fn,rd1,rd2,Stime,Stmcd,Temp,tmp1,tmp2,totci,
     &                 totcim,toto,totom,trend,xrat,stcirb,ombar2,
     &                 ombrsq,ombrsd,imbar2,imbrsd,dvec,vo,vpp,thisob
c      DOUBLE PRECISION adjtic,adjstc
c      DOUBLE PRECISION adjstc(PLEN)
      INTEGER fext,i,ip,ifail,ij,j,jj,jyr,k,fhnote,l,ly2,m,mgrz,mlda,n,
     &        mfda,mldaf
      DIMENSION trend(PLEN),xrat(PLEN),rd1(PYRS),rd2(PYRS),Stmcd(PLEN),
     &          Stime(PLEN),imbrsd(PSP),ombrsq(PSP),ombar2(PSP),dvec(1),
     &          stcirb(PLEN),imbar2(PSP),Temp(PLEN),ombrsd(PSP),
     &          gudbak(PLEN)
c-----------------------------------------------------------------------
      LOGICAL dpeq,issame,isfals
      DOUBLE PRECISION vars,varian
      EXTERNAL dpeq,issame,vars,varian,isfals
c-----------------------------------------------------------------------
      COMMON /work  / Temp
      COMMON /mq5a  / Stmcd,Stime
c-----------------------------------------------------------------------
      dvec(1)=ZERO
c-----------------------------------------------------------------------
      fhnote=STDERR
      IF(Lquiet)fhnote=0
c-----------------------------------------------------------------------
      lsame=F
      IF((Iagr.lt.4.and.Prttab(LX11E0)).or.
     &   (Iagr.eq.4.and.Prttab(LCMPE0)))THEN
       mgrz=(Lstyr-Lyr)*Ny+Lstmo
       ip=15
       IF(Iagr.eq.4)THEN
        CALL x11plt(Orig,Stci,Pos1ob,mgrz,LCMPE0,0,0,ip,2)
       ELSE
        CALL x11plt(Orig,Stci,Pos1ob,mgrz,LX11E0,0,0,ip,2)
       END IF
       IF(Lfatal)RETURN
      END IF
*      IF(Kfulsm.eq.0.or.Kfulsm.eq.2)THEN
c-----------------------------------------------------------------------
C --- WRITE ORIGINAL SERIES MODIFIED FOR EXTREMES AND PRIORS E1.
c-----------------------------------------------------------------------
       CALL prtagr(Stome,Pos1ob,Posfob,Pos1ob,Posfob,1,1,2,Iagr,LX11E1,
     &             LCMPE1,dvec,Lgraf)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
C --- WRITE MODIFIED SEASONALLY ADJUSTED SERIES E2.
c-----------------------------------------------------------------------
*       IF(Kfulsm.eq.0)THEN
       CALL prtagr(Stcime,Pos1ob,Posfob,Pos1ob,Posfob,2,1,2,Iagr,LX11E2,
     &             LCMPE2,dvec,Lgraf)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
C --- DO TEST FOR RESIDUAL SEASONALITY.
c-----------------------------------------------------------------------
       IF(Iagr.lt.4)THEN
        CALL ftest(Stcime,Pos1ob,Posfob,Ny,1,Prttab(LX11E2),F)
       ELSE
        CALL ftest(Stcime,Pos1ob,Posfob,Ny,1,Prttab(LCMPE2),F)
       END IF
*       END IF
c-----------------------------------------------------------------------
C --- WRITE THE MODIFIED IRREGULAR SERIES E3.
c-----------------------------------------------------------------------
       CALL prtagr(Stime,Pos1ob,Posfob,Pos1ob,Posfob,3,1,3,Iagr,LX11E3,
     &             LCMPE3,dvec,Lgraf)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
C --- COMPUTE THE RATIOS (DIFFERENCES) OF THE ANNUAL TOTALS OF B1(A1)
C --- TO D11 AND E1 TO E2.
c-----------------------------------------------------------------------
       IF(Kfulsm.eq.0)THEN
        fext=LX11E4
        IF(Iagr.eq.4)fext=LCMPE4
        IF(Prttab(fext))THEN
         j=1
         jyr=Lstyr-Lyr+1
c         IF(Pos1bk.ne.1)THEN
c          j=Ny+j
c          jyr=jyr-1
c         END IF
         ly2=Lyr
         DO WHILE (j.lt.Pos1ob)
          j=Ny+j
          jyr=jyr-1
          Lyr=Lyr+1
         END DO
         IF(Lstmo.ne.Ny)jyr=jyr-1
c       IF (IFORC.NE.0) JYR=JYR+1
         DO jj=1,jyr
          toto=ZERO
          totci=ZERO
          totom=ZERO
          totcim=ZERO
          k=j+Ny-1
          DO l=j,k
           toto=toto+Series(l)
           totom=totom+Stome(l)
           totci=totci+Stci(l)
           totcim=totcim+Stcime(l)
          END DO
          IF(Muladd.ne.1)THEN
           rd1(jj)=toto/totci*ONEHND
           rd2(jj)=totom/totcim*ONEHND
          ELSE
           rd1(jj)=toto-totci
           rd2(jj)=totom-totcim
          END IF
          j=j+Ny
         END DO
c-----------------------------------------------------------------------
C --- WRITE RATIOS (DIFFERENCES)OF ANNUAL TOTALS E4.
c-----------------------------------------------------------------------
         CALL table(rd1,1,jyr,4,1,1,rd2,fext)
         Lyr=ly2
         IF(Lfatal)RETURN
        END IF
       END IF
*      END IF
c-----------------------------------------------------------------------
c --- if this is a multiplicative seasonal adjustment, check each of
c     the series that will have percent changes (differences) generated
c     from them to see if there is are zeroes in any of the series
c     (BCM March 2006).
c-----------------------------------------------------------------------
      IF(Iagr.eq.4)CALL divsub(O5,O,Faccal,Pos1ob,Posffc)
      IF(Muladd.ne.1.and.(Psuadd.or.(.not.dpeq(Cnstnt,DNOTST))))THEN
       IF(Iagr.eq.4)THEN
        CALL chkzro(Series,Stci,Stci2,Stcirn,O5,Pos1bk,Posffc,Kfulsm)
       ELSE
        CALL chkzro(Series,Stci,Stci2,Stcirn,Stocal,Pos1bk,Posffc,
     &              Kfulsm)
       END IF
      END IF
c-----------------------------------------------------------------------
C --- COMPUTE THE AVERAGE PERCENT CHANGES (DIFFERENCES) IN THE ORIGINAL
C --- SERIES.
c-----------------------------------------------------------------------
      mfda=Pos1ob+1
      CALL change(Series,Temp,mfda,Posfob)
c-----------------------------------------------------------------------
C --- WRITE THE CHANGES IN THE ORIGINAL SERIES E5.
c-----------------------------------------------------------------------
      CALL pragr2(Temp,mfda,Posfob,mfda,Posfob,5,1,1,Iagr,LX11E5,LCMPE5,
     &            LXEE5P,LCPE5P,Muladd,dvec,F)
      IF(Lfatal)RETURN
      IF(Lsumm.gt.0)CALL svchsd(Temp,mfda,Posfob,Iagr,Muladd,'e5')
      IF(Kfulsm.eq.0)THEN
c-----------------------------------------------------------------------
C --- CALCULATE THE CHANGES IN THE SEASONALLY ADJUSTED SERIES.
c-----------------------------------------------------------------------
       CALL change(Stci,Temp,mfda,Posfob)
c-----------------------------------------------------------------------
C --- WRITE THE CHANGES IN THE SEASONALLY ADJUSTED SERIES E6.
c-----------------------------------------------------------------------
       CALL pragr2(Temp,mfda,Posfob,mfda,Posfob,6,1,1,Iagr,LX11E6,
     &             LCMPE6,LXEE6P,LCPE6P,Muladd,dvec,F)
       IF(Lfatal)RETURN
       IF(Lsumm.gt.0)CALL svchsd(Temp,mfda,Posfob,Iagr,Muladd,'e6')
c-----------------------------------------------------------------------
C --- IF THE YEARLY TOTALS OF THE SEASONALLY ADJUSTED SERIES ARE
C --- MODIFIED or the seasonally adjusted values were rounded,
c --- CALCULATE THE CHANGES IN THE MODIFIED SERIES.
c     comment out setting of ify, allow changes to be computed for
c     earlier data - BCM, March 2006
c-----------------------------------------------------------------------
       IF(Iyrt.gt.0)THEN
*        ify=mod(Pos1ob,Ny)
*        IF(ify.gt.Begyrt)THEN
*         ify=(((Pos1ob+Ny-2)/Ny)*Ny)+Begyrt+1
*        ELSE
*         ify=(((Pos1ob-1)/Ny)*Ny)+Begyrt+1
*        END IF
c-----------------------------------------------------------------------
C --- WRITE THE CHANGES E6.A.
c-----------------------------------------------------------------------
        CALL change(Stci2,Temp,mfda,Posfob)
        CALL pragr2(Temp,mfda,Posfob,mfda,Posfob,6,2,1,Iagr,LFCE6A,
     &              LCPE6A,LFC6AP,LCP6AP,Muladd,dvec,F)
        IF(Lfatal)RETURN
        IF(Lsumm.gt.0)CALL svchsd(Temp,mfda,Posfob,Iagr,Muladd,'e6a')
       END IF
c-----------------------------------------------------------------------
C --- WRITE THE CHANGES E6.R.
c-----------------------------------------------------------------------
       IF(Lrndsa)THEN
*        IF(Iyrt.eq.0)ify=mfda
        CALL change(Stcirn,Temp,mfda,Posfob)
        CALL pragr2(Temp,mfda,Posfob,mfda,Posfob,6,3,1,Iagr,LFCE6R,
     &              LCPE6R,LFC6RP,LCP6RP,Muladd,dvec,F)
        IF(Lfatal)RETURN
        IF(Lsumm.gt.0)CALL svchsd(Temp,mfda,Posfob,Iagr,Muladd,'e6r')
       END IF
      END IF
c     ------------------------------------------------------------------
C --- WRITE THE CHANGES FOR THE FINAL TREND CYCLE D12.  If level shift
c     outliers were removed, put them in the final trend cycle.
c     ------------------------------------------------------------------
      IF((((.not.Finls).and.Adjls.eq.1).or.(Nustad.gt.0.and.Lprntr).and.
     &   Iagr.lt.4).or.(Iagr.eq.4.and.Lindls).or.(Lttc.and.Adjtc.eq.1
     &   .and.(.not.Fintc)))THEN
       CALL change(Stc2,Temp,mfda,Posfob)
      ELSE
       CALL change(Stc,Temp,mfda,Posfob)
      END IF
      CALL pragr2(Temp,mfda,Posfob,mfda,Posfob,7,3,1,Iagr,LX11E7,
     &            LCMPE7,LXEE7P,LCPE7P,Muladd,dvec,F)
      IF(Lfatal)RETURN
      IF(Lsumm.gt.0)CALL svchsd(Temp,mfda,Posfob,Iagr,Muladd,'e7')
c     ------------------------------------------------------------------
C --- WRITE THE CHANGES FOR THE original series adjusted for calendar
c     effects.  (added by BCM June 2005)  
c     ------------------------------------------------------------------
      IF(Iagr.eq.4)THEN
       CALL change(O5,Temp,mfda,Posfob)
      ELSE
       CALL change(Stocal,Temp,mfda,Posfob)
      END IF 
      CALL pragr2(Temp,mfda,Posfob,mfda,Posfob,8,1,1,Iagr,LX11E8,LCMPE8,
     &            LXEE8P,LCPE8P,Muladd,dvec,F)
      IF(Lfatal)RETURN
      IF(Lsumm.gt.0)CALL svchsd(Temp,mfda,Posfob,Iagr,Muladd,'e8')
      IF(Muladd.ne.1.and.(.not.dpeq(Cnstnt,DNOTST)))THEN
       CALL copylg(Gudval,POBS,1,gudbak)
       CALL setlg(T,POBS,Gudval)
      END IF
c-----------------------------------------------------------------------
c     Print a more robust estimate of the seasonally adjusted series.
c-----------------------------------------------------------------------
      DO i=Pos1ob,Posfob
       stcirb(i)=Series(i)-Stome(i)+Stcime(i)
      END DO
      CALL prtagr(stcirb,Pos1ob,Posfob,Pos1ob,Posfob,11,1,2,Iagr,LXEE11,
     &            LCPE11,dvec,F)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Print Final adjustment ratios - A1 / D11.
c-----------------------------------------------------------------------
      i=Pos1ob
      pre18b=F
      DO WHILE (i.le.Posffc)
       IF(Iagr.lt.4)THEN
        thisob=Series(i)
       ELSE
        thisob=O(i)
       END IF
       IF(dpeq(Stci(i),ZERO))THEN
        IF(dpeq(thisob,ZERO))THEN
         stcirb(i)=ONE
        ELSE
         stcirb(i)=DNOTST
         IF(.not.pre18b)pre18b=T
        END IF
       ELSE
        IF(dpeq(thisob,ZERO).or.thisob.lt.ZERO)pre18b=T
        stcirb(i)=thisob/Stci(i)
       END IF
       i=i+1
      END DO
      CALL prtagr(stcirb,Pos1ob,Posfob,Pos1bk,Posffc,18,1,1,Iagr,LXEE18,
     &            LCPE18,dvec,Lgraf)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c    Print/Save total adjustment factors (BCM - March 2004, revised
c    June 2008)
c-----------------------------------------------------------------------
      IF(pre18b.or.((Iagr.lt.4.and.Savtab(LXEEEB)).or.
     &              (Iagr.ge.4.and.Savtab(LCPEEB))))THEN
       IF(Iagr.lt.4)THEN
        CALL divsub(stcirb,Series,Stci,Pos1ob,Posffc)
       ELSE
        CALL divsub(stcirb,O,Stci,Pos1ob,Posffc)
       END IF
       CALL prtagr(stcirb,Pos1ob,Posfob,Pos1bk,Posffc,18,2,1,Iagr,
     &             LXEEEB,LCPEEB,dvec,Lgraf.and.pre18b)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
C --- PART F.
c-----------------------------------------------------------------------
      Kpart=6
c-----------------------------------------------------------------------
C --- COMPUTE MEASURES FOR THE SUMMARY MEASURES TABLE.
c-----------------------------------------------------------------------
c      IF(Adjhol.eq.1.or.Khol.eq.2.or.(Ixreg.gt.0.and.Axrghl))THEN
c       CALL addmul(Sprior,Sprior,Fachol,Pos1bk,Posffc)
c       Kfmt=1
c      END IF
c-----------------------------------------------------------------------
      allgud=T
      IF(Muladd.ne.1.and.(.not.dpeq(Cnstnt,DNOTST)))THEN
       CALL copylg(gudbak,POBS,1,Gudval)
       allgud=isfals(Gudval,Pos1ob,Posfob)
      END IF
c-----------------------------------------------------------------------
      IF(Kfmt.ne.0)THEN
       CALL sumry(Sprior,Pbar,dvec,Psq,dvec,2,Pos1ob,Posfob)
       vpp=vars(Sprior,Pos1ob,Posfob,0,Muladd)
       Vp=vpp
      ELSE
       DO i=1,Ny
        Pbar(i)=ZERO
        Psq(i)=ZERO
       END DO
       Vp=ZERO
      END IF
      IF((.not.Axrgtd.and.Kswv.eq.0.and.Adjtd.le.0).and.(.not.
     &  (Adjhol.eq.1.or.Khol.eq.2.or.(Ixreg.gt.0.and.Axrghl))).or.
     &  (Iagr.ge.4.and.(.not.Lindcl)))THEN
       Vtd=ZERO
       DO i=1,Ny
        Tdbar(i)=ZERO
        Tdsq(i)=ZERO
       END DO
      ELSE
       CALL sumry(Faccal,Tdbar,dvec,Tdsq,dvec,2,Pos1ob,Posfob)
       Vtd=vars(Faccal,Pos1ob,Posfob,1,Muladd)
      END IF
      CALL sumry(Sti,Ibar,Ibar2,Isq,Isd,0,Pos1ob,Posfob)
      CALL avedur(Sti,Pos1ob,Posfob,Adri)
c      Vi=vars(Sti,Pos1ob,Posfob,1,Muladd)
c     ------------------------------------------------------------------
*      IF(.not.dpeq(Cnstnt,DNOTST))THEN
*       DO i=Pos1ob,Posfob
*        Stome(i)=Stome(i)+Cnstnt
*        Stc(i)=Stc(i)+Cnstnt
*        Series(i)=Series(i)+Cnstnt
*        Stcime(i)=Stcime(i)+Cnstnt
*       END DO
*      END IF
      IF(allgud)THEN
       IF(Adjls.eq.1)CALL divsub(Stome,Stome,Facls,Pos1bk,Posffc)
       IF(Adjusr.eq.1)CALL divsub(Stome,Stome,Facusr,Pos1bk,Posffc)
      ELSE
       CALL copy(Stome,Posffc,1,Temp)
       IF(Adjls.eq.1)CALL divgud(Stome,Stome,Facls,Pos1bk,Posffc)
       IF(Adjusr.eq.1)CALL divgud(Stome,Stome,Facusr,Pos1bk,Posffc)
      END IF
c     ------------------------------------------------------------------
      CALL sumry(Stome,Ombar,ombar2,ombrsq,ombrsd,0,Pos1ob,Posfob)
      IF(issame(Stome,Pos1ob,Posfob))THEN
       CALL nWritln('Seasonal adjustment diagnostics cannot be '//
     &              'generated because',fhnote,Mt2,T,F)
       CALL writln('      the series listed below have either a '//
     &             'variance of zero or a ',fhnote,Mt2,F,F)
       CALL writln('      variance could not be computed:',
     &             fhnote,Mt2,F,T)
       CALL writTagClass(Mt2,'ul','indent')
       CALL writTag(Mt2,'<li>')
       CALL writln('        the original series adjusted for extreme '//
     &             'values.',fhnote,Mt2,F,F)
       CALL writTag(Mt2,'</li>')
       lsame=T
      END IF
      CALL sumry(Stime,Imbar,imbar2,Isq,imbrsd,0,Pos1ob,Posfob)
      Vi=vars(Stime,Pos1ob,Posfob,1,Muladd)
      IF(dpeq(Vi,ZERO).or.issame(Sti,Pos1ob,Posfob).or.
     &   dpeq(Vi,DNOTST))THEN
       IF(.not.lsame)THEN
        CALL nWritln('Seasonal adjustment diagnostics cannot be '//
     &               'generated because',fhnote,Mt2,T,F)
        CALL writln('      the series listed below have either a '//
     &              'variance of zero or a ',fhnote,Mt2,F,F)
        CALL writln('      variance could not be computed:',
     &              fhnote,Mt2,F,T)
        CALL writTagClass(Mt2,'ul','indent')
        lsame=T
       END IF
       CALL writTag(Mt2,'<li>')
       CALL writln('        the irregular component.',STDERR,Mt2,F,F)
       CALL writTag(Mt2,'</li>')
      END IF
c     ------------------------------------------------------------------
      IF(allgud)THEN
       IF(Adjls.eq.1)CALL addmul(Stome,Stome,Facls,Pos1bk,Posffc)
       IF(Adjusr.eq.1)CALL addmul(Stome,Stome,Facusr,Pos1bk,Posffc)
      ELSE
       CALL copy(Temp,Posffc,1,Stome)
      END IF
c     ------------------------------------------------------------------
      IF(Kfulsm.eq.2)THEN
       DO i=1,Ny
        Sbar(i)=ZERO
        Sbar2(i)=ZERO
        Ssd(i)=ZERO
        Ssq(i)=ZERO
       END DO
       Vs=ZERO
      ELSE
       CALL sumry(Sts,Sbar,Sbar2,Ssq,Ssd,0,Pos1ob,Posfob)
       Vs=vars(Sts,Pos1ob,Posfob,1,Muladd)
      END IF
      CALL sumry(Stc,Cbar,Cbar2,Csq,Csd,0,Pos1ob,Posfob)
      IF(issame(Stc,Pos1ob,Posfob))THEN
       IF(.not.lsame)THEN
        CALL nWritln('Seasonal adjustment diagnostics cannot be '//
     &               'generated because',fhnote,Mt2,T,F)
        CALL writln('      the series listed below have either a '//
     &              'variance of zero or a ',fhnote,Mt2,F,F)
        CALL writln('      variance could not be computed:',
     &              fhnote,Mt2,F,T)
        CALL writTagClass(Mt2,'ul','indent')
        lsame=T
       END IF
       CALL writTag(Mt2,'<li>')
       CALL writln('        the trend component.',STDERR,Mt2,F,F)
       CALL writTag(Mt2,'</li>')
      END IF
c-----------------------------------------------------------------------
C --- REMOVE LINEAR TREND (OR LINEAR PERCENTAGE GROWTH IF THE SERIES IS
C --- MULTIPLICATIVE) FROM TREND CYCLE.
c-----------------------------------------------------------------------
      IF(Muladd.ne.1)THEN
       IF(.not.dpeq(Cnstnt,DNOTST))THEN
        DO i=Pos1ob,Posfob
         Stc(i)=Stc(i)+Cnstnt
        END DO
       END IF
       CALL logar(Stc,Pos1ob,Posfob)
      END IF
      tmp1=DBLE(-Pos1ob-Posfob)
      tmp2=ZERO
      DO i=Pos1ob,Posfob
       tmp2=tmp2+Stc(i)*(TWO*DBLE(i)+tmp1)
      END DO
      fn=DBLE(Posfob-Pos1ob+1)
      tmp1=SIX*tmp2/(fn*(fn*fn-ONE))
      DO i=Pos1ob,Posffc
       trend(i)=tmp1*(DBLE(i-Pos1bk)+ONE)
      END DO
      IF(Muladd.ne.1)THEN
       CALL antilg(Stc,Pos1ob,Posfob)
       CALL antilg(trend,Pos1ob,Posfob)
      END IF
      CALL divsub(Temp,Stc,trend,Pos1ob,Posfob)
      IF(Muladd.ne.1)THEN
       IF(.not.dpeq(Cnstnt,DNOTST))THEN
        DO i=Pos1ob,Posfob
         Stc(i)=Stc(i)-Cnstnt
         trend(i)=trend(i)-Cnstnt
        END DO
       END IF
      END IF
      CALL avedur(Stc,Pos1ob,Posfob,Adrc)
      Vc=vars(Temp,Pos1ob,Posfob,0,Muladd)
c     ------------------------------------------------------------------
      IF(allgud)THEN
       IF(Adjls.eq.1)CALL divsub(Series,Series,Facls,Pos1bk,Posffc)
       IF(Adjao.eq.1)CALL divsub(Series,Series,Facao,Pos1bk,Posffc)
       IF(Adjtc.eq.1)CALL divsub(Series,Series,Factc,Pos1bk,Posffc)
       IF(Adjusr.eq.1)CALL divsub(Series,Series,Facusr,Pos1bk,Posffc)
      ELSE
       CALL copy(Series,Posffc,1,Temp)
       IF(Adjls.eq.1)CALL divgud(Series,Series,Facls,Pos1bk,Posffc)
       IF(Adjao.eq.1)CALL divgud(Series,Series,Facao,Pos1bk,Posffc)
       IF(Adjtc.eq.1)CALL divgud(Series,Series,Factc,Pos1bk,Posffc)
       IF(Adjusr.eq.1)CALL divgud(Series,Series,Facusr,Pos1bk,Posffc)
      END IF
c     ------------------------------------------------------------------
      IF(issame(Series,Pos1ob,Posfob))THEN
       IF(.not.lsame)THEN
        CALL nWritln('Seasonal adjustment diagnostics cannot be '//
     &               'generated because',fhnote,Mt2,T,F)
        CALL writln('      the series listed below have either a '//
     &              'variance of zero or a ',fhnote,Mt2,F,F)
        CALL writln('      variance could not be computed:',
     &              fhnote,Mt2,F,T)
        CALL writTagClass(Mt2,'ul','indent')
        lsame=T
       END IF
       IF((Adjls.eq.1.or.Adjao.eq.1.or.Adjtc.eq.1).and.Adjusr.eq.1)THEN
        CALL writTag(Mt2,'<li>')
        CALL writln('        the original series adjusted for '//
     &              'regARIMA outlier and user-defined',fhnote,Mt2,F,F)
        CALL writln('        regression effects.',STDERR,Mt2,F,F)
        CALL writTag(Mt2,'</li>')
       ELSE IF(Adjls.eq.1.or.Adjao.eq.1.or.Adjtc.eq.1)THEN
        CALL writTag(Mt2,'<li>')
        CALL writln('        the original series adjusted for '//
     &              'regARIMA outlier effects.',STDERR,Mt2,F,F)
        CALL writTag(Mt2,'</li>')
       ELSE IF(Adjusr.eq.1)THEN
        CALL writTag(Mt2,'<li>')
        CALL writln('        the original series adjusted for user-'//
     &              'defined regression',STDERR,Mt2,F,F)
        CALL writln('        effects.',STDERR,Mt2,F,F)
        CALL writTag(Mt2,'</li>')
       ELSE
        CALL writTag(Mt2,'<li>')
        CALL writln('        the original series.',STDERR,Mt2,F,F)
        CALL writTag(Mt2,'</li>')
       END IF
      END IF
      CALL sumry(Series,Obar,Obar2,Osq,Osd,0,Pos1ob,Posfob)
c     ------------------------------------------------------------------
      IF(allgud)THEN
       IF(Adjls.eq.1)CALL addmul(Series,Series,Facls,Pos1bk,Posffc)
       IF(Adjao.eq.1)CALL addmul(Series,Series,Facao,Pos1bk,Posffc)
       IF(Adjtc.eq.1)CALL addmul(Series,Series,Factc,Pos1bk,Posffc)
       IF(Adjusr.eq.1)CALL addmul(Series,Series,Facusr,Pos1bk,Posffc)
      ELSE
       CALL copy(Temp,Posffc,1,Series)
      END IF
c     ------------------------------------------------------------------
      CALL divsub(Temp,Stome,trend,Pos1ob,Posfob)
      vo=vars(Temp,Pos1ob,Posfob,0,Muladd)/ONEHND
c     ------------------------------------------------------------------
c     Check to see if seasonal adjustment diagnostics can be computed.
c     ------------------------------------------------------------------
      IF(dpeq(vo,ZERO).or.dpeq(vo,DNOTST))THEN
       IF(.not.lsame)THEN
        CALL nWritln('Seasonal adjustment diagnostics cannot be '//
     &               'generated because',fhnote,Mt2,T,F)
        CALL writln('      the series listed below have either a '//
     &              'variance of zero or a ',fhnote,Mt2,F,F)
        CALL writln('      variance could not be computed:',
     &              fhnote,Mt2,F,T)
        CALL writTagClass(Mt2,'ul','indent')
        lsame=T
       END IF
       CALL writTag(Mt2,'<li>')
       CALL writln('        the original series with the linear '//
     &             'trend removed.',fhnote,Mt2,F,F)
       CALL writTag(Mt2,'</li>')
      END IF
      IF(lsame)THEN
       CALL writTag(Mt2,'</ul>')
       RETURN
      END IF
      Vp=Vp/vo
      Vtd=Vtd/vo
      Vc=Vc/vo
      Vs=Vs/vo
      Vi=Vi/vo
      Rv=Vp+Vtd+Vc+Vs+Vi
      DO i=1,Ny
       IF(dpeq(Isq(i),DNOTST).or.dpeq(Csq(i),DNOTST).or.
     &    dpeq(Ssq(i),DNOTST).or.dpeq(Psq(i),DNOTST).or.
     &    dpeq(Tdsq(i),DNOTST))THEN
        IF(.not.dpeq(Isq(i),DNOTST))Isq(i)=DNOTST
        IF(.not.dpeq(Csq(i),DNOTST))Csq(i)=DNOTST
        IF(.not.dpeq(Ssq(i),DNOTST))Ssq(i)=DNOTST
        IF(.not.dpeq(Psq(i),DNOTST))Psq(i)=DNOTST
        IF(.not.dpeq(Tdsq(i),DNOTST))Tdsq(i)=DNOTST
        Osq2(i)=DNOTST
       ELSE
        Osq2(i)=Isq(i)+Csq(i)+Ssq(i)+Psq(i)+Tdsq(i)
        Isq(i)=Isq(i)/Osq2(i)
        Csq(i)=Csq(i)/Osq2(i)
        Ssq(i)=Ssq(i)/Osq2(i)
        Psq(i)=Psq(i)/Osq2(i)
        Tdsq(i)=Tdsq(i)/Osq2(i)
c       Osq2(i)=Osq2(i)/Osq(i)
        Osq2(i)=Osq2(i)/ombrsq(i)
       END IF
c-----------------------------------------------------------------------
C --- COMPUTE I/C RATIOS.
c-----------------------------------------------------------------------
       IF(dpeq(Cbar(i),DNOTST).or.dpeq(Ibar(i),DNOTST))THEN
        smic(i)=DNOTST
       ELSE
        Smic(i)=Ibar(i)/Cbar(i)
       END IF
      END DO
c     ------------------------------------------------------------------
      IF(allgud)THEN
       IF(.not.Finls.and.Adjls.eq.1)
     &    CALL divsub(Stci,Stci,Facls,Pos1bk,Posffc)
       IF(.not.Finao.and.Adjao.eq.1)
     &    CALL divsub(Stci,Stci,Facao,Pos1bk,Posffc)
       IF(.not.Fintc.and.Adjtc.eq.1)
     &    CALL divsub(Stci,Stci,Factc,Pos1bk,Posffc)
       IF(.not.Finusr.and.Adjusr.eq.1)
     &    CALL divsub(Stci,Stci,Facusr,Pos1bk,Posffc)
      ELSE
       CALL copy(Stci,Posffc,1,Temp)
       IF(.not.Finls.and.Adjls.eq.1)
     &    CALL divgud(Stci,Stci,Facls,Pos1bk,Posffc)
       IF(.not.Finao.and.Adjao.eq.1)
     &    CALL divgud(Stci,Stci,Facao,Pos1bk,Posffc)
       IF(.not.Fintc.and.Adjtc.eq.1)
     &    CALL divgud(Stci,Stci,Factc,Pos1bk,Posffc)
       IF(.not.Finusr.and.Adjusr.eq.1)
     &    CALL divgud(Stci,Stci,Facusr,Pos1bk,Posffc)
      END IF
c     ------------------------------------------------------------------
      CALL sumry(Stci,Cibar,Cibar2,dvec,Cisd,1,Pos1ob,Posfob)
      CALL avedur(Stci,Pos1ob,Posfob,Adrci)
c     ------------------------------------------------------------------
      IF(allgud)THEN
       IF(.not.Finls.and.Adjls.eq.1)
     &    CALL addmul(Stci,Stci,Facls,Pos1bk,Posffc)
       IF(.not.Finao.and.Adjao.eq.1)
     &    CALL addmul(Stci,Stci,Facao,Pos1bk,Posffc)
       IF(.not.Fintc.and.Adjtc.eq.1)
     &    CALL addmul(Stci,Stci,Factc,Pos1bk,Posffc)
       IF(.not.Finusr.and.Adjusr.eq.1)
     &    CALL addmul(Stci,Stci,Facusr,Pos1bk,Posffc)
      ELSE
       CALL copy(Temp,Posffc,1,Stci)
      END IF 
c     ------------------------------------------------------------------
      IF(allgud)THEN
       IF(.not.Finls.and.Adjls.eq.1)
     &    CALL divsub(Stcime,Stcime,Facls,Pos1bk,Posffc)
*       IF(.not.Finao.and.Adjao.eq.1)
*     &    CALL divsub(Stcime,Stcime,Facao,Pos1bk,Posffc)
*       IF(.not.Fintc.and.Adjtc.eq.1)
*     &    CALL divsub(Stcime,Stcime,Factc,Pos1bk,Posffc)
       IF(.not.Finusr.and.Adjusr.eq.1)
     &    CALL divsub(Stcime,Stcime,Facusr,Pos1bk,Posffc)
      ELSE
       CALL copy(Stcime,Posffc,1,Temp)
       IF(.not.Finls.and.Adjls.eq.1)
     &    CALL divgud(Stcime,Stcime,Facls,Pos1bk,Posffc)
*       IF(.not.Finao.and.Adjao.eq.1)
*     &    CALL divgud(Stcime,Stcime,Facao,Pos1bk,Posffc)
*       IF(.not.Fintc.and.Adjtc.eq.1)
*     &    CALL divgud(Stcime,Stcime,Factc,Pos1bk,Posffc)
       IF(.not.Finusr.and.Adjusr.eq.1)
     &    CALL divgud(Stcime,Stcime,Facusr,Pos1bk,Posffc)
      END IF
c     ------------------------------------------------------------------
      CALL sumry(Stcime,Cimbar,dvec,dvec,dvec,3,Pos1ob,Posfob)
c     ------------------------------------------------------------------
      IF(allgud)THEN
       IF(.not.Finls.and.Adjls.eq.1)
     &    CALL addmul(Stcime,Stcime,Facls,Pos1bk,Posffc)
*       IF(.not.Finao.and.Adjao.eq.1)
*     &    CALL addmul(Stcime,Stcime,Facao,Pos1bk,Posffc)
*       IF(.not.Fintc.and.Adjtc.eq.1)
*     &    CALL addmul(Stcime,Stcime,Factc,Pos1bk,Posffc)
       IF(.not.Finusr.and.Adjusr.eq.1)
     &    CALL addmul(Stcime,Stcime,Facusr,Pos1bk,Posffc)
      ELSE
       CALL copy(Stcime,Posffc,1,Temp)
      END IF
c     ------------------------------------------------------------------
*      IF(.not.dpeq(Cnstnt,DNOTST))THEN
*       DO i=Pos1ob,Posfob
*        Stome(i)=Stome(i)-Cnstnt
*        Stc(i)=Stc(i)-Cnstnt
*        Series(i)=Series(i)-Cnstnt
*        Stcime(i)=Stcime(i)-Cnstnt
*       END DO
*      END IF
c-----------------------------------------------------------------------
C --- COMPUTE MCD.
c-----------------------------------------------------------------------
      Mcd=Ny
      DO WHILE (Smic(Mcd).lt.ONE)
       IF(Mcd.eq.1)GO TO 10
       Mcd=Mcd-1
      END DO
      Mcd=Mcd+1
      IF(Mcd.gt.Ny)Mcd=Ny
   10 n=Mcd
      IF(n.gt.6)n=6
      m=2-n+n/2*2
c-----------------------------------------------------------------------
C --- APPLY THE MCD MOVING AVERAGE.
c-----------------------------------------------------------------------
      CALL averag(Stci,Stmcd,Pos1bk,Posffc,m,n)
      mfda=Pos1ob+n/2
      mlda=Posfob-n/2
      mldaf=Posffc-n/2
      IF(mldaf.gt.Posfob)mldaf=Posfob
      CALL sumry(Stmcd,Smbar,Smbar2,dvec,Smsd,1,mfda,mlda)
      CALL avedur(Stmcd,mfda,mlda,Adrmcd)
      Kpart=6
c-----------------------------------------------------------------------
C --- WRITE THE MCD MOVING AVERAGE F1.
c-----------------------------------------------------------------------
      CALL prtagr(Stmcd,mfda,mlda,mfda,mlda,1,1,2,Iagr,LX11F1,LCMPF1,
     &            dvec,F)
      IF(Lfatal)RETURN
      DO i=mfda,mldaf
       Stmcd(i)=Temp(i)
      END DO
c-----------------------------------------------------------------------
C --- CALCULATE THE AUTOCORRELATION FUNCTION OF THE IRREGULARS FOR SPANS
C --- 1 TO NY+2.
c-----------------------------------------------------------------------
      Nn=2-Muladd
      ebar=dble(1-Muladd)
      tmp1=varian(Sti,Pos1ob,Posfob,Nn)/fn
      n=Ny+2
      CALL setdp(ZERO,n,Autoc)
      DO i=1,n
       ij=Pos1ob+i
       DO j=ij,Posfob
        Autoc(i)=Autoc(i)+(Sti(j)-ebar)*(Sti(j-i)-ebar)
       END DO
       Autoc(i)=Autoc(i)/((fn-i)*tmp1)
      END DO
      CALL f3cal(Sts,ifail)
c-----------------------------------------------------------------------
C --- WRITE TABLES F2 AND F3.
c-----------------------------------------------------------------------
      IF(Iagr.eq.4)THEN
       IF(Prttab(LCMPF2).or.Prttab(LCMPF3))
     &    CALL fgen(Mt1,Kfmt,LCMPF2,LCMPF3,.False.)
       CALL svf2f3(Nform,Ng,Savtab(LCMPF2),Savtab(LCMPF3),'if')
       IF(Lfatal)RETURN
      ELSE
       IF(Prttab(LX11F2).or.Prttab(LX11F3))
     &    CALL fgen(Mt1,Kfmt,LX11F2,LX11F3,.True.)
       CALL svf2f3(Nform,Ng,Savtab(LX11F2),Savtab(LX11F3),'f')
       IF(Lfatal)RETURN
      END IF
c      CALL qcontr(Tmpma,Ny)
c-----------------------------------------------------------------------
c     Print out type of trading day table.
c-----------------------------------------------------------------------
      IF(Tdtbl.gt.0.and.Prttab(LXETDY).and.Iagr.lt.4)THEN
       CALL prtdtb(Tdtbl)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c     Produce ratio plots
c-----------------------------------------------------------------------
      IF(Iagr.lt.4.and.Prttab(LXERA1).or.Iagr.eq.4.and.Prttab(LCMPR1))
     &   THEN
       IF(Muladd.eq.1)THEN
        DO i=Posfob,Pos1bk+1,-1
         xrat(i)=Stome(i)-Stome(i-1)
        END DO
       ELSE
        DO i=Posfob,Pos1bk+1,-1
         xrat(i)=Stome(i)/Stome(i-1)
        END DO
       END IF
       ip=17
       IF(Iagr.eq.4)THEN
        CALL x11plt(xrat,xrat,Pos1bk+1,Posfob,LCMPR1,0,0,ip,1)
       ELSE
        CALL x11plt(xrat,xrat,Pos1bk+1,Posfob,LXERA1,0,0,ip,1)
       END IF
       IF(Lfatal)RETURN
      END IF
      IF(Iagr.lt.4.and.Prttab(LXERA2).or.Iagr.eq.4.and.Prttab(LCMPR2))
     &   THEN
       IF(Muladd.eq.1)THEN
        DO i=Posfob,Pos1bk+1,-1
         xrat(i)=Stcime(i)-Stcime(i-1)
        END DO
       ELSE
        DO i=Posfob,Pos1bk+1,-1
         xrat(i)=Stcime(i)/Stcime(i-1)
        END DO
       END IF
       ip=17
       IF(Iagr.eq.4)THEN
        CALL x11plt(xrat,xrat,Pos1bk+1,Posfob,LCMPR2,0,0,ip,1)
       ELSE
        CALL x11plt(xrat,xrat,Pos1bk+1,Posfob,LXERA2,0,0,ip,1)
       END IF
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
