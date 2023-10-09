      SUBROUTINE agr3s(Lgraf,Begspn,Lx11)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      LOGICAL T,F
      INTEGER MO,YR
      DOUBLE PRECISION ONE,ZERO
      PARAMETER(ONE=1D0,ZERO=0D0,T=.true.,F=.false.,MO=2,YR=1)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'lex.i'
      INCLUDE 'stdio.i'
      INCLUDE 'build.prm'
      INCLUDE 'agr.cmn'
      INCLUDE 'agrsrs.cmn'
      INCLUDE 'inpt.cmn'
      INCLUDE 'seatcm.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'cmptbl.i'
      INCLUDE 'extend.cmn'
      INCLUDE 'x11msc.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'force.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11fac.cmn'
      INCLUDE 'adxser.cmn'
      INCLUDE 'x11srs.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      CHARACTER dattim*(24),Cmpfil*(PFILCR),ctype*(4),xb*(65),str*(3)
      LOGICAL Lgraf,Lx11,rndok,pre18b
      INTEGER Cmptyp,Begspn,i,i1,i2,i3,ib,ie,ncfil,mfda,lnlen,ifac,ify,
     &        lastsf,lstfrc,frstsf,ipos
      DOUBLE PRECISION temp,dvec,Cmpwt,flsind,faoind,rinit,ststd,
     &                 stcirb,tempk,stbase,frcfac
      DIMENSION Begspn(2),temp(PLEN),dvec(1),Cmpwt(PSRS),Cmptyp(PSRS),
     &          Cmpfil(PSRS),ctype(4),flsind(PLEN),faoind(PLEN),
     &          ststd(PLEN),stcirb(PLEN),stbase(PLEN),frcfac(PLEN)
c-----------------------------------------------------------------------
      CHARACTER*24 cvdttm
      LOGICAL dpeq
      INTEGER nblank
      EXTERNAL dpeq,nblank,cvdttm
c-----------------------------------------------------------------------        
      COMMON /cmpsum/ Cmpwt,Cmptyp,Cmpfil
c-----------------------------------------------------------------------
      DATA ctype/'add ','sub ','mult','div'/
c     ------------------------------------------------------------------        
      dvec(1)=ZERO
      ifac=1
      IF(Muladd.eq.1)ifac=0
      IF(.not.(Savfct))THEN
       lastsf=Posfob
      ELSE IF(Nfcst.gt.0)THEN
       lastsf=Posffc
      ELSE
       lastsf=Posfob+Ny
      END IF
      frstsf=Pos1ob
      IF(Savbct)frstsf=Pos1bk
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
      Iagr=4
c-----------------------------------------------------------------------
      rinit=ONE
      IF(Muladd.eq.1)rinit=ZERO
c     ------------------------------------------------------------------
C --- SAVE DIRECT  SEASONALLY ADJUSTED SERIES.
c     ------------------------------------------------------------------
      IF(Lx11)THEN
       DO i=Pos1ob,Posffc
        Tem(i)=Stc(i)
        Stci(i)=Ci(i)
       END DO
      ELSE 
       DO i=Pos1ob,Posffc
        Tem(i)=Seattr(i)
        Stci(i)=Seatsa(i)
       END DO
      END IF
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
c     ------------------------------------------------------------------
      Kpart=1
      Ixreg=0
      Kswv=0
      IF(Tmpma.eq.2)Tmpma=0
c     ------------------------------------------------------------------
c     print out prior adjusted composite series (BCM March 2004)
c     ------------------------------------------------------------------
      IF(Prttab(LCMPA3))
     &   CALL table(O1,Pos1ob,Posfob,8,1,1,dvec,LCMPA3)
      IF(.not.Lfatal.and.Savtab(LCMPA3))
     &   CALL punch(O1,Pos1ob,Posfob,LCMPA3,F,F)
      IF(.not.Lfatal.and.Lgraf)
     &   CALL punch(O1,Pos1ob,Posfob,LCMPA3,Lgraf,F)
      IF(Lfatal)RETURN
c     ------------------------------------------------------------------
      DO i=Pos1ob,Posffc
       Stci(i)=Ci(i)
       Stome(i)=Omod(i)
      END DO
c     ------------------------------------------------------------------
c     Remove level changes from seasonally adjusted series before
c     calculating the final trend cycle (BCM - December 2002)
c     ------------------------------------------------------------------
      CALL setdp(rinit,PLEN,flsind)
      CALL setdp(rinit,PLEN,faoind)
      IF(Lindls)THEN
       CALL divsub(flsind,O,O3,Pos1bk,Posffc)
*       CALL divsub(Stci,Stci,flsind,Pos1bk,Posffc)
      END IF
      IF(Lindao)CALL divsub(faoind,O,O4,Pos1bk,Posffc)
c-----------------------------------------------------------------------
c     Print out indirect level change, AO factors here.
c-----------------------------------------------------------------------
      IF(Lindao)THEN
       IF(Prttab(LCPIAO))
     &    CALL table(faoind,Pos1ob,Posfob,8,1,1,dvec,LCPIAO)
       IF(.not.Lfatal.and.Savtab(LCPIAO))
     &    CALL punch(faoind,Pos1ob,Posfob,LCPIAO,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(faoind,Pos1ob,Posfob,LCPIAO,Lgraf,F)
       IF(Lfatal)RETURN
      END IF
      IF(Lindls)THEN
       IF(Prttab(LCPILS))
     &    CALL table(flsind,Pos1ob,Posfob,8,1,1,dvec,LCPILS)
       IF(.not.Lfatal.and.Savtab(LCPILS))
     &    CALL punch(flsind,Pos1ob,Posfob,LCPILS,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(flsind,Pos1ob,Posfob,LCPILS,Lgraf,F)
       IF(Lfatal)RETURN
      END IF
c     ------------------------------------------------------------------
C --- CALCULATE THE INDIRECT SEASONAL FACTORS.
c     ------------------------------------------------------------------
      Kpart=4
      CALL divsub(Sts,O5,Stci,Pos1bk,Posffc)
c     ------------------------------------------------------------------
      CALL divsub(ststd,O2,Stci,Pos1bk,Posffc)
      CALL divsub(Faccal,O2,O5,Pos1bk,Posffc)
c     ------------------------------------------------------------------
C --- WRITE THE INDIRECT SEASONAL FACTORS.
c     ------------------------------------------------------------------
      IF(Prttab(LCMPSF).or.Prttab(LCPIPS))
     &   CALL table(Sts,Pos1ob,Posfob,10,3,1,dvec,LCMPSF)
      IF((Savtab(LCMPSF).or.Savtab(LCPIPS).or.Lgraf).and.
     &   (.not.Lfatal))THEN
       IF(Savtab(LCMPSF))CALL punch(Sts,frstsf,lastsf,LCMPSF,F,F)
       IF(Savtab(LCPIPS))
     &    CALL punch(Sts,frstsf,lastsf,LCPIPS,F,Muladd.ne.1)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(Sts,frstsf,lastsf,LCMPSF,Lgraf,F)
      END IF
      IF(.not.Lfatal.and.Prttab(LCMPSP))
     &   CALL x11plt(Sts,Sts,Pos1ob,Posfob+Ny,LCMPSP,0,ifac,7,1)
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
c     ------------------------------------------------------------------
C --- DO A TEST FOR RESIDUAL SEASONALITY.
c     ------------------------------------------------------------------
      CALL ftest(Stci,Pos1ob,Posfob,Ny,1,Prttab(LCPRSF),Savtab(LCPRSF))
c     ------------------------------------------------------------------
C --- IF OPTION SELECTED ADJUST YEARLY TOTALS OF D11 TO EQUAL THE YEARLY
C --- TOTALS OF SERIES.
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
        IF(Iftrgt.eq.0)THEN
         CALL copy(O,lstfrc,1,stbase)
        ELSE IF(Iftrgt.eq.1)THEN
         CALL copy(O5,lstfrc,1,stbase)
        ELSE
         CALL copy(O1,lstfrc,1,stbase)
         IF(Iftrgt.eq.3)CALL divsub(stbase,stbase,Faccal,Pos1ob,lstfrc)
        END IF
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
       CALL ftest(Stci2,Pos1ob,Posfob,Ny,1,Prttab(LCPSAA).and.
     &            Prttab(LCPRSF),Savtab(LCPRSF))
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
        IF(Lx11)
     &     CALL ftest(Stcirn,Pos1ob,Posfob,Ny,1,Prttab(LCPRND).and.
     &                Prttab(LCPRSF),Savtab(LCPRSF))
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
      IF(.not.Lfatal)Kpart=5
c-----------------------------------------------------------------------
C --- COMPUTE THE AVERAGE PERCENT CHANGES (DIFFERENCES) IN THE ORIGINAL
C --- SERIES.
c-----------------------------------------------------------------------
      mfda=Pos1ob+1
      CALL change(Series,Temp,mfda,Posfob)
c-----------------------------------------------------------------------
C --- WRITE THE CHANGES IN THE ORIGINAL SERIES E5.
c-----------------------------------------------------------------------
      IF(Prttab(LCMPE5).or.Prttab(LCPE5P))
     &   CALL table(Temp,mfda,Posfob,5,1,1,dvec,LCMPE5)
      IF(.not.Lfatal.and.Savtab(LCMPE5))
     &   CALL punch(Temp,mfda,Posfob,LCMPE5,F,F)
      IF(.not.Lfatal.and.Savtab(LCPE5P))
     &   CALL punch(Temp,mfda,Posfob,LCPE5P,F,Muladd.ne.1)
      IF(Lfatal)RETURN
      IF(Kfulsm.eq.0)THEN
c-----------------------------------------------------------------------
C --- CALCULATE THE CHANGES IN THE SEASONALLY ADJUSTED SERIES.
c-----------------------------------------------------------------------
       CALL change(Stci,Temp,mfda,Posfob)
c-----------------------------------------------------------------------
C --- WRITE THE CHANGES IN THE SEASONALLY ADJUSTED SERIES E6.
c-----------------------------------------------------------------------
       IF(Prttab(LCMPE6).or.Prttab(LCPE6P))
     &    CALL table(Temp,mfda,Posfob,6,1,1,dvec,LCMPE6)
       IF(.not.Lfatal.and.Savtab(LCMPE6))
     &    CALL punch(Temp,mfda,Posfob,LCMPE6,F,F)
      IF(.not.Lfatal.and.Savtab(LCPE6P))
     &   CALL punch(Temp,mfda,Posfob,LCPE6P,F,Muladd.ne.1)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
C --- IF THE YEARLY TOTALS OF THE SEASONALLY ADJUSTED SERIES ARE
C --- MODIFIED or the seasonally adjusted values were rounded,
c --- CALCULATE THE CHANGES IN THE MODIFIED SERIES.
c-----------------------------------------------------------------------
       IF(Iyrt.gt.0)THEN
        ify=mod(Pos1ob,Ny)
        IF(ify.gt.Iyrt)THEN
         ify=(((Pos1ob+Ny-2)/Ny)*Ny)+Iyrt+1
        ELSE
         ify=(((Pos1ob-1)/Ny)*Ny)+Iyrt+1
        END IF
c-----------------------------------------------------------------------
C --- WRITE THE CHANGES E6.A.
c-----------------------------------------------------------------------
        CALL change(Stci2,Temp,ify,Posfob)
        IF(Prttab(LCPE6A).or.Prttab(LCP6AP))
     &     CALL table(Temp,ify,Posfob,6,2,1,dvec,LCPE6A)
        IF(.not.Lfatal.and.Savtab(LCPE6A))
     &     CALL punch(Temp,ify,Posfob,LCPE6A,F,F)
        IF(.not.Lfatal.and.Savtab(LCP6AP))
     &     CALL punch(Temp,ify,Posfob,LCP6AP,F,Muladd.ne.1)
        IF(Lfatal)RETURN
       END IF
c-----------------------------------------------------------------------
C --- WRITE THE CHANGES E6.R.
c-----------------------------------------------------------------------
       IF(Lrndsa)THEN
        IF(Iyrt.eq.0)ify=mfda
        CALL change(Stcirn,Temp,ify,Posfob)
        IF(Prttab(LCPE6R).or.Prttab(LCP6RP))
     &     CALL table(Temp,ify,Posfob,6,3,1,dvec,LCPE6R)
        IF(.not.Lfatal.and.Savtab(LCPE6R))
     &     CALL punch(Temp,ify,Posfob,LCPE6R,F,F)
        IF(.not.Lfatal.and.Savtab(LCP6RP))
     &     CALL punch(Temp,ify,Posfob,LCP6RP,F,Muladd.ne.1)
        IF(Lfatal)RETURN
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Savtab(LCPE18).or.Savtab(LCPEEB).or.Lgraf)THEN
       IF(Posffc.gt.Posfob)THEN
        lastsf=Posffc
       ELSE
        lastsf=Posfob
       END IF
      END IF
c-----------------------------------------------------------------------
c     Print Final adjustment ratios - A1 / D11.
c-----------------------------------------------------------------------
      pre18b=F
      IF(Prttab(LCPE18).or.Savtab(LCPEEB).or.Lgraf)THEN
       i=Pos1ob
       DO WHILE (i.le.lastsf)
        IF(dpeq(Stci(i),ZERO))THEN
         IF(dpeq(Series(i),ZERO))THEN
          stcirb(i)=ONE
         ELSE
          stcirb(i)=DNOTST
          IF(.not.pre18b)pre18b=T
         END IF
        ELSE
         IF(dpeq(Series(i),ZERO).or.Series(i).lt.ZERO)pre18b=T
         stcirb(i)=Series(i)/Stci(i)
        END IF
        i=i+1
       END DO
       IF(Prttab(LCPE18))
     &    CALL table(stcirb,Pos1ob,Posfob,18,1,2,dvec,LCPE18)
       IF(.not.Lfatal.and.Savtab(LCPE18))
     &    CALL punch(stcirb,frstsf,lastsf,LCPE18,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(stcirb,frstsf,lastsf,LCPE18,Lgraf,F)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c    Print/Save total adjustment factors (BCM - SEAT 2004)
c-----------------------------------------------------------------------
      IF(pre18b.and.(Prttab(LCPEEB).or.Savtab(LCPEEB).or.Lgraf))THEN
       CALL divsub(stcirb,Series,Stci,Pos1ob,lastsf)
       IF(Prttab(LCPEEB))
     &    CALL table(stcirb,Pos1ob,Posfob,18,2,2,dvec,LCPEEB)
       IF(.not.Lfatal.and.Savtab(LCPEEB))
     &    CALL punch(stcirb,frstsf,lastsf,LCPEEB,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(stcirb,frstsf,lastsf,LCPEEB,Lgraf,F)
       IF(Lfatal)RETURN
      END IF
c----------------------------------------------------------------------- 
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

      
