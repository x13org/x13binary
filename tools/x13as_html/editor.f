C     Last change:Mar 2021 Allow LS at the end of time
C     Last change:  BCM  20 May 1999    8:46 am
      SUBROUTINE editor(Sscut,Srsttl,Nsrscr,Ttlvec,Notc,Lchkin,Lcomp,
     &                  Lx11,Lseats,Lmodel,Ldata,Hvmfil,Mdlfil,Dattim,
     &                  Lgraf,Lexgrf,Readok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This subroutine checks the options entered by the user for errors
c     and inconsistencies.
c-----------------------------------------------------------------------
      LOGICAL F,T
      DOUBLE PRECISION ZERO,ONE,SEVEN,MINONE,TEN
      PARAMETER(F=.false.,T=.true.,MINONE=-1D0,ZERO=0D0,ONE=1D0,
     &          SEVEN=7D0,TEN=10D0)
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'ssap.prm'
*      INCLUDE 'rev.prm'
*      INCLUDE 'tfmts.cmn'
      INCLUDE 'model.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'usrxrg.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11log.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'work2.cmn'
      INCLUDE 'xrgmdl.cmn'
      INCLUDE 'adj.cmn'
      INCLUDE 'agr.cmn'
      INCLUDE 'agrsrs.cmn'
      INCLUDE 'lzero.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'ssap.cmn'
      INCLUDE 'xrgfct.cmn'
      INCLUDE 'xeastr.cmn'
      INCLUDE 'xtrm.cmn'
      INCLUDE 'mdltbl.i'
      INCLUDE 'spctbl.i'
      INCLUDE 'frctbl.i'
      INCLUDE 'cmptbl.i'
      INCLUDE 'sumtab.prm'
c-----------------------------------------------------------------------
c     Include seasonal adjustment common blocks
c-----------------------------------------------------------------------
      INCLUDE 'title.cmn'
      INCLUDE 'force.cmn'
c-----------------------------------------------------------------------
      INCLUDE 'units.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'inpt.cmn'
*      INCLUDE 'rev.cmn'
      INCLUDE 'sspinp.cmn'
      INCLUDE 'x11msc.cmn'
      INCLUDE 'x11reg.cmn'
      INCLUDE 'missng.cmn'
      INCLUDE 'xrgum.cmn'
      INCLUDE 'rho.cmn'
      INCLUDE 'goodob.cmn'
      INCLUDE 'htmlout.prm'
      INCLUDE 'htmlout.cmn'
      INCLUDE 'htmlfile.cmn'
      INCLUDE 'filetb.cmn'
c-----------------------------------------------------------------------
c     Include metadata common blocks
c-----------------------------------------------------------------------
      INCLUDE 'metadata.prm'
      INCLUDE 'metadata.cmn'
c-----------------------------------------------------------------------
c     Include arima modelling common blocks
c-----------------------------------------------------------------------
      INCLUDE 'arima.cmn'
c-----------------------------------------------------------------------
c     Include prior factor common blocks
c-----------------------------------------------------------------------
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'priusr.cmn'
c-----------------------------------------------------------------------
c     Include files for savelog command
c-----------------------------------------------------------------------
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'mdlsvl.i'
      INCLUDE 'x11svl.i'
      INCLUDE 'spcsvl.i'
      INCLUDE 'cmpsvl.i'
      INCLUDE 'setsvl.i'
      INCLUDE 'dgnsvl.i'
      INCLUDE 'sums.i'
c     ------------------------------------------------------------------
c     maximum length of henderson filter
c     ------------------------------------------------------------------
      INCLUDE 'hender.prm'
c     ------------------------------------------------------------------
      CHARACTER ctmp*(1),Ttlvec*(80),Srsttl*(PSRSCR),clen*(4),clim*(4),
     &          fil*(PFILCR),Mdlfil*(PFILCR),line*(PFILCR),Dattim*(24),
     &          igrptl*(PGRPCR),str*(12),perstr*(7),ostr*(3)
      DOUBLE PRECISION Sscut,Maxsrs,Minsrs,tmp,divfac
      LOGICAL allmss,argok,Readok,Lchkin,Lx11,Lmodel,Lcomp,prt,prterr,
     &        Hvmfil,lexsum,sav,Ldata,prtwrn,Lgraf,Lexgrf,lerr,lzero,
     &        tdfneg,alltdf,Lseats,lsadj,samepth,leaic0
      INTEGER ktd,Notc,i,itst,iyrs,nsp,j,Nsrscr,ip0,ip1,ip2,rgmgrp,idx1,
     &        nyr,nchr,igrp,ipos,begcol,endcol,ndtchr,iper,
     &        ndays,nspc,rtype,iusr,icol,rhol,idate,klm,klq,kly,kstd,
     &        adjold,ntd,nusr,nseas,nbeg,otlgrp,frstmd,endmd,nobxot,
     &        fhnote,nmiss,ndata,nwarn,id0,smpday,istock,ilag,endlag,
     &        typidx,begdat,enddat,n1,n2,nostr,nelim
c     ------------------------------------------------------------------
      DIMENSION Sscut(5),Ttlvec(10),idate(2),ndays(PEASTR),allmss(PSP),
     &          nmiss(PSP),ndata(PSP)
c     ------------------------------------------------------------------
      DOUBLE PRECISION setcv,setcvl
      INTEGER strinx,ctoi,nblank
      LOGICAL istrue,dpeq
      EXTERNAL setcv,strinx,istrue,ctoi,dpeq,nblank,setcvl
c-----------------------------------------------------------------------
      COMMON /maxmin/ Maxsrs,Minsrs
c-----------------------------------------------------------------------
      CHARACTER AICDIC*49
      INTEGER aicidx,aicptr,PAICTD
      PARAMETER(PAICTD=6)
      DIMENSION aicptr(0:PAICTD)
      PARAMETER(AICDIC='tdtdnolpyeartdstocktd1coeftd1nolpyeartdstock1coe
     &f')
c-----------------------------------------------------------------------
c     add local delotl vector to store the outlier which is beyond model
c     span and needed to be deleted in this routine
c-----------------------------------------------------------------------
      CHARACTER XAICDC*28
      INTEGER xaicpt,PXTAIC,delotl,ndelotl
      PARAMETER(PXTAIC=6)
      DIMENSION xaicpt(0:PXTAIC),delotl(PB)
      PARAMETER(XAICDC='tdtdstocktd1coeftdstock1coef')
c     ------------------------------------------------------------------
      DATA aicptr / 1,3,13,20,27,38,50 /
      DATA xaicpt / 1,3,10,17,17,17,29 /
c-----------------------------------------------------------------------
      INCLUDE 'sumtab.var'
c-----------------------------------------------------------------------
c     Check series, regression variables, model and seasonal adjustment
c     options.  Set default values.
c-----------------------------------------------------------------------
      Kfmt=0
      Ny=Sp
      Neasvx=0
      Neasvc=0
      fhnote=STDERR
      ndelotl=0
      IF(Lquiet)fhnote=0
      lsadj=Lx11.or.Lseats
      IF(Lx11)THEN
       tX11=tX11+1
      ELSE IF(Lseats)THEN
       tSeats=tSeats+1
      ELSE
       tNSA=tNSA+1
      END IF
c-----------------------------------------------------------------------
      IF(dpeq(Traicd,DNOTST))THEN
       IF(Sp.eq.4.or.Sp.eq.12)THEN
        Traicd=-2D0
       ELSE
        Traicd=ZERO
       END IF
      END IF
c-----------------------------------------------------------------------
c     Check to see if number of observations is > than 15 years.  If so,
c     set number of backcasts to zero
c-----------------------------------------------------------------------
      IF(Nbcst.gt.0)THEN
       IF(Ldestm)THEN
        CALL dfdate(Begmdl,Begspn,Sp,nbeg)
        IF(nbeg.gt.0)THEN
         CALL wWritln('The program will not generate backcasts for '//
     &                'series with a',fhnote,Mt2,T,F)
         CALL writln('         modelspan that starts after the start '//
     &               'of the span.',fhnote,Mt2,F,T)
         Nbcst=0
         IF(Nbcstx.gt.0)Nbcstx=0
        END IF
       END IF
*       IF(Nspobs.gt.(15*Sp).and.Nbcst.gt.0)THEN
*        CALL writln('WARNING: The program will not generate backcasts fo
*     &r series longer than',fhnote,Mt2,T)
*        CALL writln('         15 years.',fhnote,Mt2,F)
*        Nbcst=0
*        IF(Nbcstx.gt.0)Nbcstx=0
*       END IF
       IF(Lseats)THEN
        CALL wWritln('The program will not generate backcasts for '//
     &               'use with SEATS',fhnote,Mt2,T,F)
        CALL writln('         seasonal adjustments.',fhnote,Mt2,F,T)
        Nbcst=0
        IF(Nbcstx.gt.0)Nbcstx=0
       END IF
      END IF
      Length=Nspobs
c-----------------------------------------------------------------------
c     calculate beginning date of backcasts
c     ------------------------------------------------------------------
      CALL addate(Begspn,Sp,-Nbcst,Begbak)
c     ------------------------------------------------------------------
c     if first month of backcasts not = 1, increase number of backcasts
c     to accomodate.
c     ------------------------------------------------------------------
      IF(Begbak(MO).gt.1)THEN
       Nbcst2=Nbcst+Begbak(MO)-1
       Begbk2(MO)=1
      ELSE
       Nbcst2=Nbcst
       Begbk2(MO)=Begbak(MO)
      END IF
      Begbk2(YR)=Begbak(YR)
c-----------------------------------------------------------------------
c     provide "pointers" for X-11 to tell where backcasts, data,
c     forecasts begin and end.
c-----------------------------------------------------------------------
      CALL dfdate(Begspn,Begsrs,Sp,Frstsy)
      Frstsy=Frstsy+1
      Nomnfy=Nobs-Frstsy+1
      Nfdrp=Nfcst
      IF((.not.lsadj).and.Fctdrp.gt.0)Nfdrp=max(0,Nfcst-Fctdrp)
      Nobspf=min(Nspobs+Nfdrp,Nobs-Frstsy+1)
      Nofpob=Nspobs+Nfdrp
      Nbfpob=Nspobs+Nfdrp+Nbcst
      Lsp=1
      CALL setxpt(Nfdrp,lsadj,Fctdrp)
      IF(Iagr.eq.3)CALL agrxpt(Begspn,Sp)
      Lyr=Begspn(1)
      Lstyr=Endspn(1)
      Lstmo=Endspn(2)
      CALL dfdate(Begmdl,Begsrs,Sp,frstmd)
      CALL dfdate(Endmdl,Begsrs,Sp,endmd)
      frstmd=frstmd+1
      endmd=endmd+1
c-----------------------------------------------------------------------
c   Check to see if there is are outliers outside the model span.
c-----------------------------------------------------------------------
      IF(Nb.gt.0)THEN
       DO icol=1,Nb
        rtype=Rgvrtp(icol)
        IF(rtype.eq.PRSQLS.or.rtype.eq.PRSQAO)rtype=rtype-100
        IF((rtype.eq.PRGTLS).or.(rtype.eq.PRGTAO).or.
     &     (rtype.eq.PRGTTC).or.(rtype.eq.PRGTRP).or.
     &     (rtype.eq.PRGTQI).or.(rtype.eq.PRGTQD).or.
     &     (rtype.eq.PRGTMV).or.(rtype.eq.PRGTSO).or.
     &     (rtype.eq.PRGTTL))THEN

         CALL getstr(Colttl,Colptr,Nb,icol,igrptl,nchr)
         CALL rdotlr(igrptl(1:nchr),Begsrs,Sp,typidx,begdat,enddat,
     &                argok)

c-----------------------------------------------------------------------
c   Check to see if there is an LS outlier at the end of the model span
c  and save the regressions beyond model span points (updated on 6/7/19)
c-----------------------------------------------------------------------
         IF(rtype.eq.PRGTLS.or.rtype.eq.PRGTTL)THEN
          IF(rtype.eq.PRGTLS)THEN
            ostr='LS '
            nostr=2
          ELSE
            ostr='TLS'
            nostr=3
          END IF
          IF(begdat.eq.endmd.and.rtype.eq.PRGTTL)THEN
           ndelotl = ndelotl +1
           delotl(ndelotl) = icol
           CALL wWritln(ostr(1:nostr)//' regressor ('//igrptl(1:nchr)//
     &                  ') not within model span. ',fhnote,Mt2,T,F)
           CALL writln('       Change the regARIMA model and rerun.',
     &                 fhnote,Mt2,F,T)

          ELSE IF (begdat.gt.endmd)THEN
           ndelotl = ndelotl +1
           delotl(ndelotl) = icol
           CALL wWritln(ostr(1:nostr)//' regressor ('//igrptl(1:nchr)//
     &                  ') not within model span.',fhnote,Mt2,T,F)
           CALL writln('       Change the regARIMA model and rerun.',
     &                 fhnote,Mt2,F,T)

          ELSE IF(begdat.lt.frstmd+1)THEN
           ndelotl = ndelotl +1
           delotl(ndelotl) = icol
           CALL wWritln('Beginning of '//ostr(1:nostr)//' regressor ('//
     &                  igrptl(1:nchr)//') not within model span.',
     &                  fhnote,Mt2,T,F)
           CALL writln('       Change the regARIMA model and rerun.',
     &                 fhnote,Mt2,F,T)

          END IF
c-----------------------------------------------------------------------
c   Check to see if there is an AO outlier beyond the end of the 
c   model span.
c-----------------------------------------------------------------------
         ELSE IF(rtype.eq.PRGTAO.or.rtype.eq.PRGTMV)THEN

          IF(rtype.eq.PRGTAO)THEN
            ostr='AO '
            nostr=2
          ELSE
            ostr='MV '
            nostr=2
          END IF
          IF (begdat.gt.endmd.or.begdat.lt.frstmd)THEN
           ndelotl = ndelotl+1
           delotl(ndelotl) = icol
           CALL wWritln(ostr(1:nostr)//' regressor ('//igrptl(1:nchr)//
     &                  ') not within model span.',fhnote,Mt2,T,F)
           CALL writln('       Change the regARIMA model and rerun.',
     &                 fhnote,Mt2,F,T)

          END IF
c-----------------------------------------------------------------------
c   Check to see if there is a TC outlier beyond the end of the 
c   model span.
c-----------------------------------------------------------------------
         ELSE IF(rtype.eq.PRGTTC)THEN
          IF (begdat.gt.endmd.or.begdat.lt.frstmd)THEN
           ndelotl = ndelotl +1
           delotl(ndelotl) = icol
           CALL wWritln('TC regressor ('//igrptl(1:nchr)//
     &                  ') not within model span.',fhnote,Mt2,T,F)
           CALL writln('       Change the regARIMA model and rerun.',
     &                 fhnote,Mt2,F,T)

          END IF
c-----------------------------------------------------------------------
c   Check to see if there is a SO outlier beyond the end of the 
c   model span.
c-----------------------------------------------------------------------
         ELSE IF(rtype.eq.PRGTSO)THEN
          IF (begdat.gt.endmd.or.begdat.lt.frstmd)THEN
           ndelotl = ndelotl +1
           delotl(ndelotl) = icol
            CALL wWritln('SO regressor ('//igrptl(1:nchr)//
     &                  ') not within model span.',fhnote,Mt2,T,F)
           CALL writln('       Change the regARIMA model and rerun.',
     &                 fhnote,Mt2,F,T)

          END IF
c-----------------------------------------------------------------------
c   Check to see if there is a ramp outlier beyond the end of the 
c   model span.
c-----------------------------------------------------------------------
         ELSE IF((rtype.eq.PRGTRP).or.(rtype.eq.PRGTQI).or.
     &           (rtype.eq.PRGTQD))THEN
          IF(enddat.gt.endmd)THEN
           ndelotl = ndelotl +1
           delotl(ndelotl) = icol
            CALL wWritln('End of ramp ('//igrptl(1:nchr)//
     &                  ') not within model span.',fhnote,Mt2,T,F)
           CALL writln('       Change the regARIMA model and rerun.',
     &                 fhnote,Mt2,F,T)

c     ------------------------------------------------------------------
          ELSE IF(begdat.lt.frstmd)THEN
           ndelotl = ndelotl +1
           delotl(ndelotl) = icol
           CALL wWritln('Beginning of ramp ('//igrptl(1:nchr)//
     &                  ') not within model span.',fhnote,Mt2,T,F)
           CALL writln('       Change the regARIMA model and rerun.',
     &                 fhnote,Mt2,F,T)

          END IF
         END IF
        END IF
       END DO
c-----------------------------------------------------------------------
c     delete all the regressions beyond model span points
c-----------------------------------------------------------------------
       IF (ndelotl.gt.0) then
         do i=1,ndelotl
           CALL dlrgef(delotl(i),Nrxy,1)
           do j=i+1,ndelotl
            delotl(j)=delotl(j) -1
           end do
         end do
       END IF
      END IF
c-----------------------------------------------------------------------
c   For seats seasonal adjustments, check to see if forecasts need to
c   be extended, if so, reset pointers
c-----------------------------------------------------------------------
      IF(Lseats)THEN
       ip1=max(12,3*Sp)
       IF(Nfcst.lt.ip1)THEN
        Posffc=Posffc-Nfcst+ip1
        Nfcst=ip1
        Nfdrp=Nfcst
        Nobspf=min(Nspobs+Nfdrp,Nobs-Frstsy+1)
        Nofpob=Nspobs+Nfcst
        Nbfpob=Nspobs+Nfcst+Nbcst
        CALL setxpt(Nfdrp,Lseats,Fctdrp)
        ip2=1
        CALL itoc(ip1,clen,ip2)
        IF(Lfatal)RETURN
        CALL nWritln('A longer forecast horizon is required by the '//
     &               'SEATS signal extraction',Mt1,Mt2,T,F)
        CALL writln('      procedure, so the number of forecasts '//
     &              'generated by this run has',Mt1,Mt2,F,F)
        CALL writln('      been changed to '//clen(1:(ip2-1))//'.',
     &              Mt1,Mt2,F,T)
       END IF
c-----------------------------------------------------------------------
c   For seats seasonal adjustments, check to see if seasonal 
c   overdifferencing is tested for in the automatic model identification
c   proceedings, if so, turn off the test
c-----------------------------------------------------------------------
       IF(Lsovdf)THEN
        Lsovdf=F
        CALL nWritln('Since SEATS signal extraction is selected, the '//
     &               'seasonal overdifferencing test ',Mt1,Mt2,T,F)
        CALL writln('      of the automatic model identification '//
     &              'procedure is turned off.',Mt1,Mt2,F,T)
       END IF
      END IF
c-----------------------------------------------------------------------
c ---  Set up variables for sliding spans analysis
c-----------------------------------------------------------------------
      L0=1
      Ly0=Lyr
      Itd=0
      Ihol=0
c-----------------------------------------------------------------------
c     Add constant specified in transform spec (added by BCM, July 2005)
c-----------------------------------------------------------------------
      IF(.not.dpeq(Cnstnt,DNOTST))THEN
       DO i=1,Nobs
        IF(.not.(dpeq(Y(i),Mvcode)))Y(i)=Y(i)+Cnstnt
       END DO
      END IF
c-----------------------------------------------------------------------
c     Check for missing values
c-----------------------------------------------------------------------
      Maxsrs=Y(Frstsy)
      Minsrs=Y(Frstsy)
      lerr=T
      lzero=T
      CALL setlg(T,PSP,allmss)
      CALL setint(0,PSP,nmiss)
      CALL setint(0,PSP,ndata)
      ip0=Frstsy+Nspobs-1
      DO i=Frstsy,ip0
       CALL addate(Begsrs,Sp,i-1,idate)
       CALL wrtdat(idate,Sp,str,ndtchr)
       id0=idate(MO)
       IF(id0.eq.0)id0=1
       IF(dpeq(Y(i),Mvcode))THEN
        lzero=F
        IF(.not.Missng)Missng=T
        Y(i)=Mvval
        IF(i.ge.frstmd.and.i.le.endmd)THEN
c-----------------------------------------------------------------------
c     Create missing value regressor
c-----------------------------------------------------------------------
c        CALL addate(Begbak,Sp,i-1,idate)
         IF(.not.Lfatal)CALL adrgef(DNOTST,'MV'//str(1:ndtchr),
     &                              'Missing Value',PRGTMV,F,F)
         IF(Lfatal)RETURN
         nmiss(id0)=nmiss(id0)+1
        ELSE
         CALL eWritln('Missing value code found outside of model '//
     &                'span, where missing values',STDERR,Mt2,T,F)
         CALL writln('       cannot be replaced.',STDERR,Mt2,F,T)
         Readok=F
         lerr=F
        END IF
       ELSE
        IF(i.ge.frstmd.and.i.le.endmd)THEN
         allmss(id0)=F
         ndata(id0)=ndata(id0)+1
        END IF
        IF(.not.dpeq(Y(i),ZERO))lzero=F
c-----------------------------------------------------------------------
c     Determine largest and smallest value of the series
c-----------------------------------------------------------------------
        IF(Maxsrs.lt.Y(i))Maxsrs=Y(i)
        IF(Minsrs.gt.Y(i))Minsrs=Y(i)
c-----------------------------------------------------------------------
c     Test to see if all data is positive
c-----------------------------------------------------------------------
        IF(Y(i).le.ZERO.and.lerr)THEN
         IF(Fcntyp.eq.0)THEN
          CALL wWritln('Automatic transformation selection cannot be '//
     &                 'done on a',fhnote,Mt2,T,F)
          CALL writln('         series with zero or negative values.',
     &                fhnote,Mt2,F,T)
          lerr=F
          Muladd=1
          Fcntyp=4
          Lam=1D0
         ELSE IF(Lx11.and.Muladd.ne.1)THEN
          IF(Psuadd)THEN
           IF(Y(i).lt.ZERO)THEN
            CALL eWritln('Pseudo-additive seasonal adjustment '//
     &                   'cannot be done on a',STDERR,Mt2,T,F)
            CALL writln('       series with negative values.',STDERR,
     &                  Mt2,F,T)
            Readok=F
            lerr=F
           END IF
          ELSE
           CALL eWritln('Multiplicative or log-additive seasonal '//
     &                  'adjustment cannot be',STDERR,Mt2,T,F)
           CALL writln('       done with a series with zero or '//
     &                 'negative values.',STDERR,Mt2,F,T)
           Readok=F
           lerr=F
          END IF
         END IF
        END IF
       END IF
      END DO
      IF(.not.Lx11.and.(Fcntyp.eq.4.or.Fcntyp.eq.0.or.dpeq(Lam,1D0)))
     &   Muladd=1
c-----------------------------------------------------------------------
c  perform checks for the number of missing data codes read into the
c  program.
c-----------------------------------------------------------------------
      IF(Missng)THEN
       IF(istrue(allmss,1,Sp))THEN
        IF(Sp.eq.12)THEN
         CALL eWritln('All data values for at least one month are '//
     &                'missing values.',STDERR,Mt2,T,F)
        ELSE IF(Sp.eq.4)THEN
         CALL eWritln('All data values for at least one quarter are '//
     &                'missing values.',STDERR,Mt2,T,F)
        ELSE
         CALL eWritln('All data values for at least one period are '//
     &                'missing values.',STDERR,Mt2,T,F)
        END IF
        CALL writln('       regARIMA model cannot be estimated.',
     &              STDERR,Mt2,F,T)
        Readok=F
        Muladd=1
       ELSE
        nwarn=0
        DO i=1,Sp
         IF(nmiss(i).gt.ndata(i))nwarn=nwarn+1
        END DO
        IF(nwarn.gt.0)THEN
         ip2=1
         CALL itoc(nwarn,clim,ip2)
         IF(Sp.eq.12)THEN
          CALL wWritln('For '//clim(1:ip2-1)//' months, the '//
     &         'number of missing values for the month is',
     &         fhnote,Mt2,T,F)
         ELSE IF(Sp.eq.4)THEN
          CALL wWritln('For '//clim(1:ip2-1)//' quarters, the '//
     &         'number of missing values for the quarter is',
     &         fhnote,Mt2,T,F)
         ELSE
          CALL wWritln('For '//clim(1:ip2-1)//' periods, the '//
     &         'number of missing values for the period is',
     &         fhnote,Mt2,T,F)
         END IF
         CALL writln('          greater than the number of data '//
     &               'values specified.',
     &               fhnote,Mt2,F,T)
         CALL writln('          The missing value replacement '//
     &               'procedure used by '//PRGNAM,
     &               fhnote,Mt2,T,F)
         CALL writln('          cannot be considered optimal for '//
     &               'this situation, and the user',
     &               fhnote,Mt2,F,F)
         CALL writln('          should consider other methods of '//
     &               'missing value replacement.',
     &               fhnote,Mt2,F,T)
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(lzero.and.Kfulsm.ne.1)THEN
       CALL eWritln('All data values read into '//PRGNAM//
     &             ' are equal to zero.',STDERR,Mt2,T,T)
       Readok=F
      ELSE IF(dpeq(Maxsrs,Minsrs))THEN
       CALL wWritln('All data values read into '//PRGNAM//
     &             ' are the same.',fhnote,Mt2,T,T)
       Same=T
      ELSE
       Same=F
      END IF
c-----------------------------------------------------------------------
c    Check to see if data exists beyond the end of the span.  If so,
c    see if there are missing value codes or negative numbers that may
c    occur in the forecast period.
c-----------------------------------------------------------------------
      Lmvaft=F
      Ln0aft=F
      IF(Nobs.gt.ip0.and.Nfcst.gt.0)THEN
       ip1=ip0+1
       ip2=min(Nobs,ip0+Nfcst)
       DO i=ip1,ip2
        IF(dpeq(Y(i),Mvval))THEN
         Lmvaft=T
        ELSE IF(dpeq(Y(i),ZERO).or.Y(i).lt.ZERO)THEN
         IF(.not.(Fcntyp.eq.4.OR.dpeq(Lam,1D0)))Ln0aft=T
        END IF
       END DO
       IF((Lmvaft.or.Ln0aft).and.Fcntyp.gt.0)THEN
        CALL nWritln('At least one value that is either less than or '//
     &               'equal to zero or',fhnote,Mt2,T,F)
        CALL writln('      equal to the missing value code was found '//
     &              'after the span of data',fhnote,Mt2,F,F)
        CALL writln('      to be analyzed, but within the time frame '//
     &              'of the forecasts',fhnote,Mt2,F,F)
        CALL writln('      generated by the regARIMA model.',
     &              fhnote,Mt2,F,T)
        CALL writln('      In this situation, the forecast output '//
     &              'will not include a',fhnote,Mt2,T,F)
        IF(Fcntyp.eq.4.OR.dpeq(Lam,1D0))THEN
         CALL writln('      comparison of the forecasts with the '//
     &               'corresponding values of the',fhnote,Mt2,F,F)
         CALL writln('      original series.',fhnote,Mt2,F,T)
        ELSE
         CALL writln('      comparison of the transformed forecasts'//
     &               ' with the corresponding',fhnote,Mt2,F,F)
         CALL writln('      values of the transformed original series.',
     &               fhnote,Mt2,F,T)
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
C --- Adjust the series using DIVPOWER, if specified
c----------------------------------------------------------------------
      IF (.NOT.(Divpwr.eq.NOTSET)) THEN
       divfac=ONE
       IF (Divpwr.lt.0) THEN
        DO i = Divpwr, -1
         divfac=divfac/TEN
        END DO
       else
        DO i = 1, Divpwr
         divfac=divfac*TEN
        END DO
       END IF
       DO i = 1, PLEN
        IF(.NOT.(dpeq(Y(i),Mvval).or.dpeq(Y(i),DNOTST)))Y(i)=Y(i)/divfac
       END DO
       Maxsrs=Maxsrs/divfac
       Minsrs=Minsrs/divfac
      END IF
c-----------------------------------------------------------------------
C --- This subroutine generates the formats for subroutine tables.
c     BCM - check later to see if this routine is needed
c----------------------------------------------------------------------
      CALL tfmts(Sp,Kdec,Maxsrs,Minsrs,Muladd)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Check to see if modelling, trading day options are correct for
c     missing value regressors.
c-----------------------------------------------------------------------
      IF(Missng)THEN
       IF(.not.Lmodel)THEN
        CALL eWritln('Must specify a regARIMA model when the Missing'//
     &               ' Value procedure is used.',STDERR,Mt2,T,T)
        Readok=F
       ELSE IF(Ixreg.eq.2)THEN
        CALL eWritln('Cannot specify irregular component regression '//
     &               'with a',STDERR,Mt2,T,F)
        CALL writln('       regARIMA model when the Missing Value '//
     &              'procedure is used.',STDERR,Mt2,F,T)
        Readok=F
       END IF
c-----------------------------------------------------------------------
C --- If missing values are in series and the missing value code
c     used is small enough to be printed out, replace with a number 
c     that will be larger than the table print format
c----------------------------------------------------------------------
*       tmp=10D0**(Tblwid+1)
*       IF(Mvval.lt.tmp)THEN
*        DO i=Frstsy,Frstsy+Nspobs-1
*         IF(dpeq(Y(i),Mvval))Y(i)=tmp
*        END DO
*        Mvval=tmp
*       END IF
      END IF
c-----------------------------------------------------------------------
c     Test to see if length of the forecast extended series exceeds
c     program limit.
c-----------------------------------------------------------------------
      IF(Posffc.gt.PLEN)THEN
       ip1=1
       ip2=1
       CALL itoc(Posffc,clen,ip1)
       IF(.not.Lfatal)CALL itoc(PLEN,clim,ip2)
       IF(Lfatal)RETURN
       CALL eWritln('Length of forecast augmented series ('//
     &              clen(1:(ip1-1))//') exceeds program',STDERR,Mt2,T,F)
       CALL writln('       limit ('//clim(1:(ip2-1))//').  See '//
     &             LIMSEC//' of the '//PRGNAM//' '//DOCNAM//'.',
     &             STDERR,Mt2,F,T)
       Readok=F
      END IF
c-----------------------------------------------------------------------
c     Test to see if the number of years spanned by the forecast and
c     backcast extended series exceeds program limit.
c-----------------------------------------------------------------------
      nyr=Posffc/Ny
      IF(mod(Posffc,Ny).gt.0)nyr=nyr+1
      IF(nyr.gt.PYRS.and.lsadj)THEN
       ip1=1
       ip2=1
       CALL itoc(nyr,clen,ip1)
       IF(.not.Lfatal)CALL itoc(PYRS,clim,ip2)
       IF(Lfatal)RETURN
       CALL eWritln('Number of years spanned by the forecast '//
     &              'augmented series ('//clen(1:(ip1-1))//')',
     &              STDERR,Mt2,T,F)
       CALL writln('       exceeds program limit ('//clim(1:(ip2-1))//
     &             ').  See '//LIMSEC//' of the '//PRGNAM,
     &             STDERR,Mt2,F,F)
       CALL writln('       '//DOCNAM//'.',STDERR,Mt2,F,T)
       Readok=F
      END IF
c-----------------------------------------------------------------------
c     Test to see if series is too short
c-----------------------------------------------------------------------
      itst=3*Ny
      IF(Length.lt.itst)THEN
       CALL eWritln('Series to be modelled and/or seasonally adjusted'//
     &              ' must have at',STDERR,Mt2,T,F)
       CALL writln('       least 3 complete years of data.',
     &             STDERR,Mt2,F,T)
       Readok=F
      END IF
c-----------------------------------------------------------------------
c     Check to see if user-defined prior adjustments are specified when
c     an automatic transformation adjustment is used.
c-----------------------------------------------------------------------
      IF(Fcntyp.eq.0)THEN
       IF(Nprtyp.gt.0.OR.(Priadj.gt.1.AND.(.not.Picktd)))THEN
        CALL eWritln('Cannot specify prior adjustment factors when '//
     &               'automatic',STDERR,Mt2,T,F)
        CALL writln('       transformation selection is used.',
     &              STDERR,Mt2,F,T)
        IF(Nprtyp.gt.0)Nprtyp=0
        IF(Priadj.gt.1)Priadj=1
        Readok=F
       END IF
c-----------------------------------------------------------------------
c     Check to see if fixed regressors are specified when an automatic
c     transformation selection is requested.  (BCM June 2007)
c-----------------------------------------------------------------------
       IF(Iregfx.ge.2)THEN
        CALL eWritln('Cannot specify fixed regression coefficients '//
     &               'when automatic',STDERR,Mt2,T,F)
        CALL writln('       transformation selection is used.',STDERR,
     &              Mt2,F,T)
        Readok=F
       END IF
c-----------------------------------------------------------------------
      END IF
c-----------------------------------------------------------------------
c     Generate prior adjustment factor to be used to adjust series.
c-----------------------------------------------------------------------
      IF(Nprtyp.gt.0)THEN
       DO i=1,Nprtyp
        IF(Muladd.eq.1.and.Percnt(i).eq.NOTSET)THEN
         Percnt(i)=2
         Adjmod=2
        ELSE
         IF(Percnt(i).eq.NOTSET)Percnt(i)=0
         IF((Muladd.eq.1.and.Lx11).and.Percnt(i).lt.2)THEN
          CALL eWritln('Additive seasonal adjustment will not be '//
     &                 'performed when the',STDERR,Mt2,T,F)
          CALL writln(
     &'       prior adjustment factors are expressed as percentages.',
     &                STDERR,Mt2,F,T)
          Readok=F
         ELSE
          Adjmod=1
          IF(Percnt(i).eq.2)Adjmod=2
         END IF
        END IF
        IF(i.eq.1)THEN
         adjold=Adjmod
        ELSE IF(Readok)THEN
         IF(Adjmod.eq.adjold)THEN
          adjold=Adjmod
         ELSE
          CALL eWritln('Cannot combine prior adjustment factors '//
     &                 'expressed as differences',STDERR,Mt2,T,F)
          CALL writln('       with prior adjustment factors '//
     &                'expressed as percentages.',STDERR,Mt2,F,T)
         Readok=F
         END IF
        END IF
       END DO
      ELSE
       IF(Muladd.eq.1)THEN
        Adjmod=2
       ELSE
        Adjmod=1
       END IF
      END IF
c-----------------------------------------------------------------------
c  Check to see if leap year prior adjustments are specified with the
c  proper transformation/seasonal adjustment mode.
c-----------------------------------------------------------------------
      IF(Priadj.eq.4)THEN
       IF(.not.dpeq(Lam,ZERO))THEN
        CALL eWritln('Leap Year prior adjustment (adjust=lpyear) can'//
     &               ' only be specified',STDERR,Mt2,T,F)
        CALL writln('       when a log transformation is specified in'//
     &              ' the transform spec.',STDERR,Mt2,F,T)
        Readok=F
       ELSE IF(Muladd.eq.1.and.Lx11)THEN
        CALL eWritln('Leap Year prior adjustment (adjust=lpyear) can'//
     &               ' only be specified',STDERR,Mt2,T,F)
        CALL writln('       when a multiplicative seasonal adjustment'//
     &              ' is specified in the x11 spec.',STDERR,Mt2,F,T)
        Readok=F
       END IF
      ELSE IF (Priadj.gt.1) THEN
       IF(.not.dpeq(Lam,ZERO))THEN
        IF(Priadj.eq.2)THEN
         CALL eWritln('Length of month prior adjustment (adjust=lom)'//
     &                ' can only be specified',STDERR,Mt2,T,F)
        ELSE
         CALL eWritln('Length of quarter prior adjustment (adjust='//
     &                'loq) can only be specified',STDERR,Mt2,T,F)
        END IF
        CALL writln('       when a log transformation is specified '//
     &              'in the transform spec.',STDERR,Mt2,F,T)
        Readok=F
       ELSE IF(Muladd.eq.1.and.Lx11)THEN
        IF(Priadj.eq.2)THEN
         CALL eWritln('Length of month prior adjustment (adjust=lom)'//
     &                ' can not be specified',STDERR,Mt2,T,F)
        ELSE
         CALL eWritln('Length of quarter prior adjustment (adjust='//
     &                'loq) cannot be specified',STDERR,Mt2,T,F)
        END IF
        CALL writln('       when an additive seasonal adjustment is '//
     &              'specified in the x11 spec.',STDERR,Mt2,F,T)
        Readok=F
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Axrgtd)THEN
       IF(Priadj.gt.1)THEN
        IF(Priadj.eq.2)THEN
         CALL eWritln('Length of month prior adjustment (adjust=lom)'//
     &                ' can not be specified',STDERR,Mt2,T,F)
        ELSE IF(Priadj.eq.3)THEN
         CALL eWritln('Length of quarter prior adjustment (adjust='//
     &                'loq) cannot be specified',STDERR,Mt2,T,F)
        ELSE
         CALL eWritln('Leap year prior adjustment (adjust=lpyear)'//
     &                ' cannot be specified',STDERR,Mt2,T,F)
        END IF
        CALL writln('       when td or td1coef is specified in the '//
     &              'variables argument of the',STDERR,Mt2,F,F)
        CALL writln('       x11regression spec.',STDERR,Mt2,F,T)
        Priadj=1
        Readok=F
       END IF
       IF(Picktd)Picktd=F
      END IF
c-----------------------------------------------------------------------
      CALL adjsrs(Nspobs,Sp,Begspn,Fctdrp,Nfcst,Nbcst,Readok)
      IF(Lfatal)RETURN
      Setpri=Pos1bk
c-----------------------------------------------------------------------
c    Turn off print options for spectrum tables if not a monthly series
c-----------------------------------------------------------------------
      IF(Ny.ne.12)THEN
       DO i=LSPCS0,LSPS0C
        IF(Prttab(i))Prttab(i)=F
        IF(Savtab(i))Savtab(i)=F
       END DO
       IF(Prttab(LSPCTP))Prttab(LSPCTP)=F
       IF(Savtab(LSPCTP))Savtab(LSPCTP)=F
       IF(Prttab(LSPCQC))Prttab(LSPCQC)=F
       IF(Savtab(LSPCQC))Savtab(LSPCQC)=F
c-----------------------------------------------------------------------
c    check savelog 
c-----------------------------------------------------------------------
       IF(Svltab(LSLSPK))Svltab(LSLSPK)=F
       IF(Svltab(LSLDSP))Svltab(LSLDSP)=F
       IF(Svltab(LSLISP))Svltab(LSLISP)=F
       IF(Svltab(LSLTPK))Svltab(LSLTPK)=F
       IF(Svltab(LSLDTP))Svltab(LSLDTP)=F
       IF(Svltab(LSLITP))Svltab(LSLITP)=F
       IF(Svltab(LSLQCH))Svltab(LSLQCH)=F
c-----------------------------------------------------------------------
c    check if Lqchk if true - if so, print warning message and set
c    Lqchk=F 
c-----------------------------------------------------------------------
       IF(Lqchk)THEN
        CALL writln('WARNING: Can only use qcheck option with '// 
     &              'monthly series.',fhnote,Mt2,T,T)
        Lqchk=F
       END IF
c      ELSE
c       IF(Prttab(LSPCTP).or.Svltab(LSLTPK).or.Svltab(LSLDTP).or.
c     &    Svltab(LSLDTP))THEN
      END IF
c-----------------------------------------------------------------------
c     check to see if table D11A (final SA series with adjusted yearly
c     totals) and the rounded seasonally adjusted series are to be
c     printed out
c-----------------------------------------------------------------------
      IF(.not.Lrndsa)THEN
       IF(Prttab(LFCRND))Prttab(LFCRND)=F
       IF(Prttab(LCPRND))Prttab(LCPRND)=F
      END IF
      IF(Iyrt.le.0)THEN
       IF(Prttab(LFCSAA))Prttab(LFCSAA)=F
       IF(Prttab(LCPSAA))Prttab(LCPSAA)=F
      END IF
c-----------------------------------------------------------------------
c    IF Mxcklg = 0 and Lsumm > 0, set Mxcklg so that the diagnostics
c    can be generated, but turn off the printout for the tables in the
c    check spec, as the spec was not specified by the user.
c    BCM, August 30, 2006
c-----------------------------------------------------------------------
      IF(Lmodel)THEN
       IF(Lsumm.gt.0.and.Mxcklg.eq.0)THEN
        Mxcklg=2*Sp
        IF(Prttab(LCKACF))Prttab(LCKACF)=F
        IF(Prttab(LCKACF+1))Prttab(LCKACF+1)=F  
        IF(Prttab(LCKAC2))Prttab(LCKAC2)=F  
        IF(Prttab(LCKAC2+1))Prttab(LCKAC2+1)=F  
        IF(Prttab(LCKHST))Prttab(LCKHST)=F  
        IF(Prttab(LCKNRM))Prttab(LCKNRM)=F  
*        IF(Prttab(LSPCRS))Prttab(LSPCRS)=F
       END IF
c-----------------------------------------------------------------------
c     Make backup copy of initial ARMA coefficients
c-----------------------------------------------------------------------
       IF(Nopr.gt.0)THEN
        endlag=Opr(Nopr)-1
        DO ilag=1,endlag
         IF(.not.Arimaf(ilag))Ap1(ilag)=Arimap(ilag)
        END DO
       END IF
      END IF
c-----------------------------------------------------------------------
C       call profiler(2,'editor, Grpttl=')
C       write(Mtprof,*) Grpttl(1:(Grpptr(Ngrptl)-1))
C       call profiler(2,'editor, Grpptr=')
C       write(Mtprof,*) (Grpptr(i),i=0,Ngrptl)
C       call profiler(2,'editor, Colttl=')
C       write(Mtprof,*) Colttl(1:(Colptr(Ncoltl)-1))
C       call profiler(2,'editor, Colptr=')
C       write(Mtprof,*) (Colptr(i),i=0,Ncoltl)
C       call profiler(2,'editor, Rgvrtp=')
C       write(Mtprof,*) (Rgvrtp(i), i=1,Nb)
C       call profiler(2,'editor, Regfx=')
C       write(Mtprof,*) (Regfx(i), i=1,Nb)
c-----------------------------------------------------------------------
C       if (Itdtst.gt.0) THEN
C        call profiler(2,'editor, Itdtst=')
C        write(Mtprof,*) Itdtst
C 	  END IF
C       if (Leastr) THEN
C 	   write(Mtprof,*)' Leastr = .true. '
C        call profiler(2,'editor, Eastst=')
C        write(Mtprof,*) Eastst
C 	   IF(Lceaic) write(Mtprof,*)' Lceaic = .true. '
C 	  END IF
C 	  if (Luser) THEN
C 	   write(Mtprof,*)' Luser = .true. '
C 	  END IF
c-----------------------------------------------------------------------
c     Check to see if any tables are being printed out or saved
c-----------------------------------------------------------------------
      IF(.not.Ldata.and.Savtab(LSRSIN))Savtab(LSRSIN)=F
      prt=istrue(Prttab,1,NTBL)
      sav=istrue(Savtab,1,NTBL)
      IF(.not.(prt.or.sav))THEN
       CALL eWritln('No tables were specified for printing or saving.',
     &              STDERR,Mt2,T,T)
       Readok=F
      END IF
c-----------------------------------------------------------------------
c     if ktd > 0, model includes trading day regressors.
c-----------------------------------------------------------------------
      ktd=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Trading Day')
      IF(ktd.eq.0)ktd=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &                       '1-Coefficient Trading Day')
      klm=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Length-of-Month')
      klq=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Length-of-Quarter')
      kly=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Leap Year')
      kstd=strinx(T,Grpttx,Gpxptr,1,Ngrptx,'Stock Trading Day')
      IF(kstd.eq.0)kstd=strinx(T,Grpttx,Gpxptr,1,Ngrptx,
     &                         '1-Coefficient Stock Trading Day')
      IF((ktd.eq.0.or.klm.eq.0.or.klq.eq.0.or.kly.eq.0.or.kstd.eq.0)
     &    .and.Ncusrx.gt.0)THEN
       i=1
       DO WHILE (i.le.Ncusrx)
        IF(Usrtyp(i).eq.PRGUTD.and.ktd.eq.0)ktd=-i
        IF(Usrtyp(i).eq.PRGULM.and.klm.eq.0)klm=-i
        IF(Usrtyp(i).eq.PRGULQ.and.klq.eq.0)klq=-i
        IF(Usrtyp(i).eq.PRGULY.and.kly.eq.0)kly=-i
        IF((Isrflw.eq.1.and.Usrtyp(i).eq.PRGUTD).and.kstd.eq.0)kstd=-i
        i=i+1
       END DO
      END IF
      IF(ktd.lt.0.or.kstd.lt.0)THEN
c-----------------------------------------------------------------------
c     If user-defined trading day regressors found, set AIC test to
c     test for user-defined regressors rather than conventional trading
c     day.
c-----------------------------------------------------------------------
       IF(Itdtst.gt.0)THEN
        Itdtst=0
        Luser=T
       END IF
c     ------------------------------------------------------------------
c     If trading day regressor not specified, check to see if trading
c     day regressors can be generated for this run.
c     ------------------------------------------------------------------
      ELSE IF(ktd.eq.0.and.Itdtst.gt.0)THEN
       IF((Itdtst.eq.3.or.Itdtst.eq.6).and.Sp.ne.12)THEN
        CALL eWritln('Need monthly data to perform aictest for stock'//
     &               ' trading day.',STDERR,Mt2,T,T)
        Readok=F
       ELSE IF(Sp.ne.12.and.Sp.ne.4)THEN
        CALL eWritln('Need monthly or quarterly data to perform '//
     &               'aictest for trading day.',STDERR,Mt2,T,T)
        Readok=F
       ELSE IF(Begsrs(YR).lt.1776)THEN
        CALL eWritln('Cannot generate trading variables for aictest '//
     &               'before 1776.',Mt2,STDERR,T,F)
        CALL writln('       Either specify a starting date, or '//
     &              'include the century in the',Mt2,STDERR,F,F)
        CALL writln('       start or modelspan arguments of the '//
     &              'series spec.',Mt2,STDERR,F,T)
        Readok=F
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Isrflw.eq.1)THEN
       IF(Itdtst.eq.3)THEN
        CALL eWritln('Cannot perform an AICtest for stock trading day',
     &               STDERR,Mt2,T,F)
        CALL writln('        regressors on a flow time series.',
     &               STDERR,Mt2,F,T)
       ELSE IF(Itdtst.eq.6)THEN
        CALL eWritln('Cannot perform an AICtest for stock '//
     &               '1-coefficient trading day',
     &               STDERR,Mt2,T,F)
        CALL writln('        regressors on a flow time series.',
     &               STDERR,Mt2,F,T)
       END IF
      ELSE IF(Isrflw.eq.2)THEN
       IF(Itdtst.eq.2)THEN
        CALL eWritln('Cannot perform an AICtest for flow trading day',
     &               STDERR,Mt2,T,F)
        CALL writln('        regressors on a stock time series.',
     &               STDERR,Mt2,F,T)
       ELSE IF(Itdtst.eq.4.or.Itdtst.eq.5)THEN
        CALL eWritln('Cannot perform an AICtest for flow '//
     &               '1-coefficient trading day',
     &               STDERR,Mt2,T,F)
        CALL writln('        regressors on a stock time series.',
     &               STDERR,Mt2,F,T)
       END IF
      END IF
c-----------------------------------------------------------------------
c     If automatic trading day selection is performed, make sure that
c     if a trading day adjustment was specified in the variables
c     argument, it matches what was entered in the aictest argument.
c-----------------------------------------------------------------------
      IF(Itdtst.gt.0.and.Ngrp.gt.0)THEN
       prterr=F
       DO igrp=1,Ngrp
        begcol=Grp(igrp-1)
        endcol=Grp(igrp)-1
        IF((Rgvrtp(begcol).eq.PRGTST.or.Rgvrtp(begcol).eq.PRG1ST.or.
     &     Rgvrtp(begcol).eq.PRRTST.or.Rgvrtp(begcol).eq.PRR1ST.or.
     &     Rgvrtp(begcol).eq.PRATST.or.Rgvrtp(begcol).eq.PRA1ST).and.
     &     (.not.prterr))THEN
         aicidx=3
         IF(begcol.eq.endcol)aicidx=6
c-----------------------------------------------------------------------
c     See if td specified in aictest
c-----------------------------------------------------------------------
         IF(Itdtst.eq.1)THEN
          IF(aicidx.gt.1)Itdtst=aicidx
         ELSE IF(Itdtst.ne.aicidx)THEN
          CALL getstr(AICDIC,aicptr,PAICTD,aicidx,str,nchr)
          IF(Lfatal)RETURN
          CALL eWritln(str(1:nchr)//
     &     ' was specified in the variables argument of the regression',
     &                 STDERR,Mt2,T,F)
          CALL getstr(AICDIC,aicptr,PAICTD,Itdtst,str,nchr)
          CALL writln('        spec but '//str(1:nchr)//
     &              ' is given in the aictest argument.',STDERR,Mt2,F,F)
          CALL writln(
     &           '       The type of trading day regressor must agree.',
     &           STDERR,Mt2,F,T)
          Readok=F
          prterr=T
         END IF
c-----------------------------------------------------------------------
c     Set date for stock trading day equal to Aicstk
c-----------------------------------------------------------------------
         IF(.not.prterr)THEN
          CALL getstr(Grpttl,Grpptr,Ngrp,igrp,igrptl,nchr)
          IF(Lfatal)RETURN
          ipos=index(igrptl(1:nchr),'[')+1
          Aicstk=ctoi(igrptl(1:nchr),ipos)
         END IF
        ELSE IF((Rgvrtp(begcol).eq.PRGTTD.or.
     &           Rgvrtp(begcol).eq.PRG1TD.or.
     &           Rgvrtp(begcol).eq.PRRTTD.or.
     &           Rgvrtp(begcol).eq.PRR1TD.or.
     &           Rgvrtp(begcol).eq.PRATTD.or.
     &           Rgvrtp(begcol).eq.PRA1TD).and.(.not.prterr))THEN
c-----------------------------------------------------------------------
c     See if td specified correctly in aictest
c-----------------------------------------------------------------------
         aicidx=2
         IF(Picktd)aicidx=1
         IF(begcol.eq.endcol)aicidx=aicidx+3
         IF(Itdtst.eq.1)THEN
          IF(aicidx.gt.1)Itdtst=aicidx
         ELSE IF(Itdtst.ne.aicidx)THEN
          CALL getstr(AICDIC,aicptr,PAICTD,aicidx,str,nchr)
          IF(Lfatal)RETURN
          CALL eWritln(str(1:nchr)//
     &     ' was specified in the variables argument of the regression',
     &                 STDERR,Mt2,T,F)
          CALL getstr(AICDIC,aicptr,PAICTD,Itdtst,str,nchr)
          IF(Lfatal)RETURN
          CALL writln('        spec but '//str(1:nchr)//
     &              ' is given in the aictest argument.',STDERR,Mt2,F,F)
          CALL writln(
     &           '       The type of trading day regressor must agree.',
     &           STDERR,Mt2,F,T)
          Readok=F
          prterr=T
         END IF
c-----------------------------------------------------------------------
c     for td and td1coef, check to see if length of month,
c     length of quarter, or leap year regressors are specified.
c     If they are, print error message.
c-----------------------------------------------------------------------
        ELSE IF((.NOT.(Fcntyp.eq.0.OR.Fcntyp.eq.4.OR.dpeq(Lam,1D0)))
     &    .and.(Rgvrtp(begcol).eq.PRGTLM.or.Rgvrtp(begcol).eq.PRGTLQ.or.
     &          Rgvrtp(begcol).eq.PRGTLY.or.Rgvrtp(begcol).eq.PRGULM.or.
     &          Rgvrtp(begcol).eq.PRGULQ.or.Rgvrtp(begcol).eq.PRGULY)
     &    .AND.(Itdtst.eq.1.or.Itdtst.eq.4))THEN
         CALL eWritln('Can''t specify a length of month, quarter, or'//
     &                ' leap year variable when',STDERR,Mt2,T,F)
         IF(Itdtst.eq.1)THEN
          CALL writln('       using the td option of aictest.',
     &                STDERR,Mt2,F,T)
         ELSE
          CALL writln('       using the td1coef option of aictest.',
     &                STDERR,Mt2,F,T)
         END IF
         Readok=F
        END IF
       END DO
c-----------------------------------------------------------------------
       IF(.not.(Lextar.or.Lextma))THEN
        CALL eWritln('The aictest argument can only be specified '//
     &               'when the model is',STDERR,Mt2,T,F)
        CALL writln('       estimated using maximum likelihood '//
     &              'estimation.',STDERR,Mt2,F,T)
        Readok=F
       END IF
      END IF
c-----------------------------------------------------------------------
c    Check to see if prior length of month or length of quarter
c    adjustment factors have been specified.
c-----------------------------------------------------------------------
      IF((Itdtst.eq.1.or.Itdtst.eq.3.or.Itdtst.eq.4.or.Itdtst.eq.6).and.
     &   (Priadj.eq.2.or.Priadj.eq.3))THEN
       IF(Priadj.eq.2)THEN
        CALL eWritln('Length-of-month prior adjustment (adjust=lom'//
     &               ') cannot be specified',STDERR,Mt2,T,F)
       ELSE
        CALL eWritln('Length-of-quarter prior adjustment (adjust=loq'//
     &               ') cannot be specified',STDERR,Mt2,T,F)
       END IF
       CALL getstr(AICDIC,aicptr,PAICTD,Itdtst,str,nchr)
       IF(Lfatal)RETURN
       CALL writln('        when '//str(1:nchr)//
     &             ' is given in the aictest argument of the '//
     &             'regression spec.',STDERR,Mt2,F,T)
       Readok=F
c-----------------------------------------------------------------------
c      Else, check to see if leap year prior adjustments are used
c      when aic=tdstock
c-----------------------------------------------------------------------
      ELSE IF((Itdtst.eq.3.or.Itdtst.eq.6).and.Priadj.eq.4)THEN
       CALL eWritln('Leap year prior adjustment (adjust=lpyear'//
     &              ') cannot be specified',STDERR,Mt2,T,F)
       CALL writln('        when tdstock is given in the aictest '//
     &             'argument of the regression spec.',STDERR,Mt2,F,T)
       Readok=F
      END IF
c-----------------------------------------------------------------------
c     Set up Tdayvc and Ntdvec (BCM, 3-28-2011)
c-----------------------------------------------------------------------
      IF(Itdtst.gt.0)THEN
       Ntdvec=2
       Tdayvc(1)=0
       Tdayvc(2)=Itdtst
       IF((Itdtst.le.2.and.ktd.eq.0).or.(Itdtst.eq.3.and.kstd.eq.0))THEN
        Tdayvc(3)=Itdtst+3
        Ntdvec=Ntdvec+1
       END IF
       IF(Isrflw.eq.2)THEN
        IF((ktd.eq.0.and.kstd.eq.0).and.Itdtst.le.2)THEN
         Tdayvc(2)=3
         Tdayvc(3)=6
         Itdtst=3
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
c     Perform checks for AIC tests of length of month, quarter, or leap
c     year regressors (BCM, March 2008)
c-----------------------------------------------------------------------
      IF((klm.lt.0).or.(klq.lt.0).or.(kly.lt.0))THEN
c-----------------------------------------------------------------------
c     If user-defined regressors found, set AIC test to
c     test for user-defined regressors rather than conventional lom, loq
c     or lpyear regressors.
c-----------------------------------------------------------------------
       IF(Lomtst.gt.0)THEN
        Lomtst=0
        Luser=T
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Lomtst.gt.0)THEN
       IF(klm.gt.0.and.Lomtst.gt.1)THEN
        CALL eWritln('lom was specified in the variables argument '//
     &               'of the regression',STDERR,Mt2,T,F)
        IF(Lomtst.eq.2)THEN
         CALL writln('        spec but loq is given in the '//
     &               ' aictest argument.',STDERR,Mt2,F,F)
        ELSE
         CALL writln('        spec but lpyear is given in the '//
     &               ' aictest argument.',STDERR,Mt2,F,F)
        END IF
        CALL writln('       The type of regressor must agree.',
     &             STDERR,Mt2,F,T)
        Readok=F
        Lomtst=0
       END IF
       IF(klq.gt.0.and.(Lomtst.eq.1.or.Lomtst.eq.3))THEN
        CALL eWritln('loq was specified in the variables argument '//
     &               'of the regression',STDERR,Mt2,T,F)
        IF(Lomtst.eq.1)THEN
         CALL writln('        spec but lom is given in the '//
     &               ' aictest argument.',STDERR,Mt2,F,F)
        ELSE
         CALL writln('        spec but lpyear is given in the '//
     &               ' aictest argument.',STDERR,Mt2,F,F)
        END IF
        CALL writln('       The type of regressor must agree.',
     &              STDERR,Mt2,F,T)
        Readok=F
        Lomtst=0
       END IF
       IF(kly.gt.0.and.Lomtst.lt.3)THEN
        CALL eWritln('lpyear was specified in the variables argument '//
     &               'of the regression',STDERR,Mt2,T,F)
        IF(Lomtst.eq.2)THEN
         CALL writln('        spec but loq is given in the '//
     &               ' aictest argument.',STDERR,Mt2,F,F)
        ELSE
         CALL writln('        spec but lom is given in the '//
     &               ' aictest argument.',STDERR,Mt2,F,F)
        END IF
        CALL writln('       The type of regressor must agree.',
     &             STDERR,Mt2,F,T)
        Readok=F
        Lomtst=0
       END IF
c-----------------------------------------------------------------------
       IF(ktd.eq.0)THEN
        IF(Itdtst.eq.1.or.Itdtst.eq.4)THEN
         IF(Lomtst.eq.1.or.Lomtst.eq.2)THEN
          IF(Lomtst.eq.1)THEN
           CALL eWritln('AIC test for the length of month regressor '//
     &                  'cannot be specified when',Mt2,STDERR,T,F)
          ELSE IF(Lomtst.eq.2)THEN
           CALL eWritln('AIC test for the length of quarter '//
     &                  'regressor cannot be specified when',
     &                  Mt2,STDERR,T,F)
          END IF
          CALL writln('       the td or td1coef option is given in '//
     &                'the aictest argument.',Mt2,STDERR,F,T)
          Lomtst=0
          Readok=F
         ELSE IF(Lomtst.eq.3.and.(.not.dpeq(Lam,ONE)))THEN
          CALL eWritln('AIC test for the leap year regressor cannot '//
     &                 'be specified when the',Mt2,STDERR,T,F)
          CALL writln('       td or td1coef option is given in the '//
     &                'variables argument and a',Mt2,STDERR,F,F)
          CALL writln('       power transformation is performed.',
     &                Mt2,STDERR,F,T)
          Lomtst=0
          Readok=F
         END IF
        END IF
       END IF
c-----------------------------------------------------------------------
       IF(kstd.ne.0)THEN
        IF(Lomtst.eq.1)THEN
         CALL eWritln('AIC test for the length of month regressor '//
     &                'cannot be specified when',Mt2,STDERR,T,F)
        ELSE IF(Lomtst.eq.2)THEN
         CALL eWritln('AIC test for the length of quarter regressor '//
     &                'cannot be specified when',Mt2,STDERR,T,F)
        ELSE
         CALL eWritln('AIC test for the leap year regressor cannot '//
     &                'be specified when',Mt2,STDERR,T,F)
        END IF
        IF(kstd.gt.0)THEN
         CALL writln('       stock trading day is specified in the '//
     &               'regARIMA model.',Mt2,STDERR,F,F)
        ELSE
         CALL writln('       stock trading day is specified as a '//
     &               'user defined regressor.',Mt2,STDERR,F,F)
        END IF
        Lomtst=0
        Readok=F
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Leastr)THEN
       IF(Eastst.lt.2.and.Isrflw.eq.2)THEN
        Eastst=2
       ELSE IF(Eastst.eq.2.and.Isrflw.eq.1)THEN
        CALL eWritln('Cannot perform an AICtest for stock Easter',
     &               STDERR,Mt2,T,F)
        CALL writln('        regressors on a flow time series.',
     &               STDERR,Mt2,F,T)
        Readok=F
        Leastr=F
       END IF
      END IF
c-----------------------------------------------------------------------
c    Check to see if proper Easter regressor is specified in aictest
c-----------------------------------------------------------------------
      IF(Leastr.and.Ngrp.gt.0)THEN
       prterr=F
       DO igrp=1,Ngrp
        begcol=Grp(igrp-1)
        endcol=Grp(igrp)-1
        IF(Rgvrtp(begcol).eq.PRGTES)THEN
         IF(Eastst.eq.1)Eastst=2
        ELSE IF((Rgvrtp(begcol).eq.PRGTEA.or.Rgvrtp(begcol).eq.PRGTEC)
     &          .and.(Eastst.eq.2).and.(.not.prterr))THEN
         IF(Rgvrtp(begcol).eq.PRGTEA)THEN
          CALL eWritln('easter was specified in the variables '//
     &                 'argument of the regression',STDERR,Mt2,T,F)
         ELSE
          CALL eWritln('sceaster was specified in the variables '//
     &                 'argument of the regression',STDERR,Mt2,T,F)
         END IF
         CALL writln('        spec but easterstock'//
     &               ' is given in the aictest argument.',
     &               STDERR,Mt2,F,F)
         CALL writln('       The type of Easter regressor must agree.',
     &               STDERR,Mt2,F,T)
         Readok=F
         prterr=T
        END IF
       END DO
      END IF
c-----------------------------------------------------------------------
      IF(Lmodel.and.Nb.gt.0)THEN
c-----------------------------------------------------------------------
c     Check if stable seasonal regressors and seasonal differencing
c     to be done in automatic modeling procedure.  (BCM 6-2011)
c-----------------------------------------------------------------------
       IF(Lautom)THEN
        IF(Diffam(2).gt.0.and.Lseff)THEN
         Diffam(2)=0
         CALL nWritln('Stable seasonal regressors present in the '//
     &                'regARIMA model.',fhnote,Mt2,T,F)
         IF(Lautod)THEN
          CALL writln('      Maximum seasonal difference in '//
     &                'automatic model identification procedure set '//
     &                'to zero.',fhnote,Mt2,F,T)
         ELSE
          CALL writln('      Seasonal difference in automatic model '//
     &                'identification procedure set to zero.',
     &                fhnote,Mt2,F,T)
         END IF
        END IF
       END IF
c-----------------------------------------------------------------------
c     Make backup copy of user defined regressors if any of the user 
c     defined regressors are fixed.
c-----------------------------------------------------------------------
       IF(Userfx.or.((Ncusrx.gt.0).and.Lautom))
     &    CALL bakusr(Userx,Usrtyp,Usrptr,Ncusrx,Usrttl,Regfx,B,Rgvrtp,
     &                Ngrp,Grpttl,Grp,Grpptr,Ngrptl,0,T)
c-----------------------------------------------------------------------
c     Find out if easter regressors are in model
c-----------------------------------------------------------------------
       igrp=strinx(T,Grpttl,Grpptr,1,Ngrp,'Easter')
       IF(igrp.eq.0)igrp=strinx(T,Grpttl,Grpptr,1,Ngrp,'StatCanEaster')
       IF(igrp.eq.0)igrp=strinx(T,Grpttl,Grpptr,1,Ngrp,'StockEaster')
       IF(igrp.gt.0)THEN
        begcol=Grp(igrp-1)
        endcol=Grp(igrp)-1
c-----------------------------------------------------------------------
        Neas=endcol-begcol+1
        IF(Neas.gt.PEASTR)THEN
         CALL eWritln('Too many Easter regressors specified in '//
     &                'variables argument.',Mt2,STDERR,T,T)
         Readok=F
         IF(Leastr)Leastr=F
        ELSE 
c-----------------------------------------------------------------------
        DO icol=begcol,endcol
          CALL getstr(Colttl,Colptr,Nb,icol,igrptl,nchr)
          IF(Lfatal)RETURN
          ipos=index(igrptl(1:nchr),'[')+1
          ndays(icol-begcol+1)=ctoi(igrptl(1:nchr),ipos)
         END DO
        END IF
c-----------------------------------------------------------------------
c     If only one Easter regressor is in the regression matrix, then
c     reset regression group name to be the same as the effect name
c-----------------------------------------------------------------------
        IF((endcol-begcol).eq.0)THEN
         CALL delstr(igrp,Grpttl,Grpptr,Ngrp,PGRP)
         IF(.not.Lfatal)
     &      CALL insstr(igrptl(1:nchr),igrp,PGRP,Grpttl,Grpptr,Ngrp)
         IF(Lfatal)RETURN
        END IF
       END IF
c-----------------------------------------------------------------------
c    Check to see if stock trading day group, if specified, is end of
c    month stock trading day.  If not, print out error message.
c    (BCM December 2008)
c-----------------------------------------------------------------------
       istock=strinx(T,Grpttl,Grpptr,1,Ngrp,'StockEaster')
       IF(istock.gt.0.and.kstd.gt.0)THEN
        ipos=index(igrptl(1:nchr),'[')+1
        smpday=ctoi(igrptl(1:nchr),ipos)
        IF(smpday.ne.31)THEN
         CALL eWritln('Must use end-of-month stock trading day with '//
     &                'current stock Easter',Mt2,STDERR,T,F)
         CALL writln('       regressor.',Mt2,STDERR,F,T)
         CALL writln('       Specify tdstock[31] in the variables '//
     &               'argument of the regression spec.',Mt2,STDERR,T,T)
         Readok=F
        END IF
       END IF
c-----------------------------------------------------------------------
c     IF AIC test for easter is done, set up vector of easter window
c     choices to test over.
c-----------------------------------------------------------------------
       IF(Leastr)THEN
        leaic0=F
        Easvec(1)=-1
        IF(igrp.gt.0)THEN
         Neasvc=endcol-begcol+2
c-----------------------------------------------------------------------
c     Check if there are too many Easter regressors for AIC testing
c-----------------------------------------------------------------------
         nelim=Neasvc
         IF(Lceaic)nelim=nelim+1
         IF(nelim.gt.PAICEA)THEN
          CALL eWritln('Too many Easter regressors specified in '//
     &                 'variables argument to use',Mt2,STDERR,T,F)
          CALL writln('       aictest.',Mt2,STDERR,F,T)
          Readok=F
          Leastr=F
         ELSE
c-----------------------------------------------------------------------
          DO icol=2,Neasvc
           Easvec(icol)=ndays(icol-1)
          END DO
          IF(Lceaic)THEN
           Neasvc=Neasvc+1
           Easvec(Neasvc)=99
          END IF
         END IF
        ELSE
         Easvec(2)=1
         Easvec(3)=8
         Easvec(4)=15
         Neasvc=4
         IF(.not.Finhol)Finhol=T
         leaic0=T
        END IF
       END IF
      ELSE IF(Leastr)THEN
       Easvec(1)=-1
       Easvec(2)=1
       Easvec(3)=8
       Easvec(4)=15
       Neasvc=4
       IF(.not.Finhol)Finhol=T
       leaic0=T
      END IF
c     ------------------------------------------------------------------
c     If Easter regressor not specified for regARIMA model, check to see
c     if Easter regressors can be generated for this run.
c     ------------------------------------------------------------------
      IF(Leastr.and.leaic0)THEN
       CALL addate(Begsrs,Sp,Nofpob-1,idate)
       IF(Sp.ne.12.and.Sp.ne.4)THEN
        CALL eWritln('Need monthly or quarterly data to perform '//
     &               'aictest for Easter.',Mt2,STDERR,T,T)
        Readok=F
       ELSE IF(Begsrs(YR).lt.1901)THEN
        CALL eWritln('Cannot generate Easter variables for aictest '//
     &               'before 1901.',Mt2,STDERR,T,F)
        CALL writln('       Either specify a starting date, or '//
     &              'include the century in the',Mt2,STDERR,F,F)
        CALL writln('       start or modelspan arguments of the '//
     &              'series spec.',Mt2,STDERR,F,T)
        Readok=F
       ELSE IF(idate(YR).gt.2100)THEN
        CALL eWritln('Cannot generate Easter variables for aictest '//
     &               'after 2100.',Mt2,STDERR,T,T)
        Readok=F
       END IF
      END IF
c-----------------------------------------------------------------------
c     Set irregular regression variables to 0 before regression is done
c-----------------------------------------------------------------------
      Easgrp=0
      Tdgrp=0
      Holgrp=0
      Stdgrp=0
      Kswv=0
      IF(Lx11)THEN
       Kersa=0
c-----------------------------------------------------------------------
c     Check for errors in specifying prior trading day
c-----------------------------------------------------------------------
       IF(dpeq(Dwt(1),DNOTST))THEN
        CALL setdp(ZERO,7,Dwt)
        Kswv=0
       ELSE
        DO i=1,7
         IF(Dwt(i).lt.ZERO.and.Muladd.eq.0)THEN
          CALL eWritln('Prior Trading Day weights cannot be less '//
     &                 'than zero for a',STDERR,Mt2,T,F)
          CALL writln('       multiplicative seasonal adjustment.',
     &                STDERR,Mt2,F,T)
          Readok=F
         ELSE IF(.not.dpeq(Dwt(i),ZERO))THEN
          Kswv=1
         END IF
        END DO
       END IF
       IF(Kswv.eq.1)THEN
        IF((.not.Psuadd.and.Muladd.eq.0).or.Muladd.eq.2)THEN
C --- STANDARDIZE WEIGHTS TO TOTAL 7.0
         tmp=ZERO
         DO i=1,7
          IF(Dwt(i).LT.ZERO.and.Lxrneg)Dwt(I)=ZERO
          tmp=tmp+Dwt(i)
         END DO
         DO i=1,7
          Dwt(i)=Dwt(i)*(SEVEN/tmp)
         END DO
C --- Check to see if there are any negative weights
        ELSE
         IF(Fcntyp.eq.0)THEN
          CALL eWritln('Prior Trading Day weights cannot be specified'//
     &                 ' when automatic',STDERR,Mt2,T,F)
          CALL writln('       transformation selection is performed.',
     &                STDERR,Mt2,F,T)
         ELSE
          CALL eWritln('Prior Trading Day weights can only be '//
     &                 'specified for a',STDERR,Mt2,T,F)
          CALL writln('       multiplicative or log-additive '//
     &                'seasonal adjustment.',STDERR,Mt2,F,T)
         END IF
         Readok=F
        END IF
       ELSE IF(Muladd.eq.0)THEN
        DO i=1,7
         Dwt(i)=ONE
        END DO
       END IF
       IF(Ixreg.gt.0)THEN
c-----------------------------------------------------------------------
c     Make backup copy of user defined regressors if any of the user 
c     defined regressors are fixed.
c-----------------------------------------------------------------------
        IF(Usrxfx)
     &     CALL bakusr(Xuserx,Usxtyp,Usrxpt,Nusxrg,Usrxtt,Regfxx,Bx,
     &                 Rgxvtp,Nxgrp,Grpttx,Grpx,Gpxptr,Ngrptx,1,T)
c-----------------------------------------------------------------------
c     Find out if X-11 easter regressors are in model - if so, compute
c     monthly mean of easter effects.
c-----------------------------------------------------------------------
        Easgrp=strinx(T,Grpttx,Gpxptr,1,Ngrptx,'Easter')
        IF(Easgrp.eq.0)
     &     Easgrp=strinx(T,Grpttx,Gpxptr,1,Ngrptx,'StatCanEaster')
        IF(Easgrp.gt.0)THEN
         begcol=Grpx(Easgrp-1)
         endcol=Grpx(Easgrp)-1
         DO icol=begcol,endcol
          CALL getstr(Colttx,Clxptr,Nbx,icol,igrptl,nchr)
          IF(Lfatal)RETURN
          ipos=index(igrptl(1:nchr),'[')+1
          ndays(icol-begcol+1)=ctoi(igrptl(1:nchr),ipos)
         END DO
c-----------------------------------------------------------------------
c     If only one Easter regressor is in the regression matrix, then
c     reset regression group name to be the same as the effect name
c-----------------------------------------------------------------------
         IF((endcol-begcol).eq.0)THEN
          CALL delstr(Easgrp,Grpttx,Gpxptr,Ngrptx,PGRP)
          IF(.not.Lfatal)
     &      CALL insstr(igrptl(1:nchr),Easgrp,PGRP,Grpttx,Gpxptr,Ngrptx)
          IF(Lfatal)RETURN
         END IF
        END IF
c-----------------------------------------------------------------------
c     IF AIC test for easter is done, set up vector of easter window
c     choices to test over.
c-----------------------------------------------------------------------
        IF(Xeastr)THEN
         Xeasvc(1)=0
         IF(Easgrp.gt.0)THEN
          Neasvx=endcol-begcol+2
          DO icol=2,Neasvx
           Xeasvc(icol)=ndays(icol-1)
          END DO 
         ELSE
          Xeasvc(2)=1
          Xeasvc(3)=8
          Xeasvc(4)=15
          Neasvx=4
          IF(.not.Finhol)Finhol=T
         END IF
        END IF
c     ------------------------------------------------------------------
c     If Easter regressor not specified, check to see if Easter
c     regressors can be generated for this run.
c     ------------------------------------------------------------------
        IF(Xeastr.and.Neasvx.eq.4)THEN
         CALL addate(Begsrs,Sp,Nofpob-1,idate)
         IF(Sp.ne.12.and.Sp.ne.4)THEN
          CALL eWritln('Need monthly or quarterly data to perform '//
     &                 'aictest for Easter.',Mt2,STDERR,T,T)
          Readok=F
         ELSE IF(Begsrs(YR).lt.1901)THEN
          CALL eWritln('Cannot generate Easter variables for '//
     &                 'aictest before 1901.',Mt2,STDERR,T,F)
          CALL writln('       Either specify a starting date, or '//
     &                'include the century in the',Mt2,STDERR,F,F)
          CALL writln('       start or modelspan arguments of the '//
     &                'series spec.',Mt2,STDERR,F,T)
          Readok=F
         ELSE IF(idate(YR).gt.2100)THEN
          CALL eWritln('Cannot generate Easter variables for aictest '//
     &                 'after 2100.',Mt2,STDERR,T,T)
          Readok=F
         END IF
        END IF
c-----------------------------------------------------------------------
c     Set pointers that tell if there are Easter, other holiday or
c     trading day regressors in the model
c-----------------------------------------------------------------------
        Holgrp=Easgrp
        IF(Holgrp.eq.0)
     &   Holgrp=strinx(T,Grpttx,Gpxptr,1,Ngrptx,'Thanksgiving')
        IF(Holgrp.eq.0)
     &   Holgrp=strinx(T,Grpttx,Gpxptr,1,Ngrptx,'Labor')
        Tdgrp=strinx(T,Grpttx,Gpxptr,1,Ngrptx,'Trading Day')
        IF(Tdgrp.eq.0)
     &   Stdgrp=strinx(T,Grpttx,Gpxptr,1,Ngrptx,'Stock Trading Day')
c-----------------------------------------------------------------------
        IF(Stdgrp.gt.0.and.Holgrp.gt.0)THEN
         CALL eWritln('Stock trading day and holiday irregular '//
     &                'component regression',STDERR,Mt2,T,F)
         CALL writln(
     &      '       variables cannot be specified in the same run.',
     &      STDERR,Mt2,F,T)
         Readok=F
        END IF
c-----------------------------------------------------------------------
c    Check to see if, when rewieghting specificed, there are fixed
c    trading day regression coefficients that are < -1.0
c-----------------------------------------------------------------------
        IF(Tdgrp.gt.0.and.Lxrneg.and.Irgxfx.ge.2)THEN 
         begcol=Grpx(Tdgrp-1)
         endcol=Grpx(Tdgrp)-1
         tdfneg=F
         alltdf=T
         DO i=begcol,endcol
          IF(Regfxx(i))THEN
           IF(Bx(i).lt.MINONE)tdfneg=T
          ELSE
           IF(alltdf)alltdf=F
          END IF
         END DO
         IF(tdfneg)THEN
          CALL eWritln('Cannot specify fixed coefficients for the '//
     &                 'trading day regressors',STDERR,Mt2,T,F)
          CALL writln('       that imply daily weights less than '//
     &                'zero when specifying',STDERR,Mt2,F,F)
          CALL writln('       reweight=yes in the x11regression spec.',
     &                STDERR,Mt2,F,T)
          Readok=F
         ELSE IF(alltdf)then
          CALL nWritln('Cannot reweight trading day coefficients if '//
     &                 'all trading day',fhnote,Mt2,T,F)
          CALL writln('      regressors are fixed; reweighting of '//
     &                'daily weights will not',fhnote,Mt2,F,F)
          CALL writln('      be performed.',fhnote,Mt2,F,T)
          Lxrneg=F
         END IF
        END IF
c-----------------------------------------------------------------------
c    Check to see if there are fixed stock trading day regression
c    coefficients for the irregular regression that are <= -1.0, which
c    lead to nonpositive trading day factors for multiplicative seasonal
c    adjustments.
c-----------------------------------------------------------------------
        IF(Stdgrp.gt.0.and.Irgxfx.ge.2.and.Muladd.eq.0)THEN 
         begcol=Grpx(Tdgrp-1)
         endcol=Grpx(Tdgrp)-1
         tdfneg=F
         DO ic=begcol,endcol
          IF(Regfxx(i).and.(Bx(i).lt.MINONE.or.dpeq(Bx(i),MINONE)))
     &       tdfneg=T
         END DO
         IF(tdfneg)THEN
          CALL eWritln('Cannot specify fixed coefficients for stock '//
     &                 'trading day',STDERR,Mt2,T,F)
          CALL writln('       regressors in the x11regression spec '//
     &                'that produce a nonpositive',STDERR,Mt2,F,F)
          CALL writln('       trading day factor.  Use the '//
     &                'regression spec to estimate the',STDERR,Mt2,F,F)
          CALL writln('       stock trading day effect.',STDERR,Mt2,F,T)
          Readok=F
         END IF
        END IF
c-----------------------------------------------------------------------
        IF(Nusxrg.gt.0)THEN
         iusr=1
         DO icol=1,Nbx
          IF(Rgxvtp(icol).eq.PRGUTD.and.Nusxrg.gt.0)THEN
           rtype=Usxtyp(iusr)
           iusr=iusr+1
           IF(Tdgrp.eq.0)THEN
            Tdgrp=icol
            IF(Xtdtst.gt.0)THEN
             Xtdtst=0
             Xuser=T
            END IF
           ELSE IF(Stdgrp.eq.0.and.Isrflw.eq.1)THEN
            Stdgrp=icol
           END IF
          ELSE IF((.not.(Holgrp.gt.0.or.Axruhl)).and.
     &            rtype.ge.PRGTUH)THEN
           Holgrp=icol
           IF(.not.Axruhl)Axruhl=T
           IF(.not.Axrghl)Axrghl=T
          END IF
         END DO
c-----------------------------------------------------------------------
        END IF
c-----------------------------------------------------------------------
c     Check to see if X-11 and regARIMA adjustments are specified in the
c     same run.
c-----------------------------------------------------------------------
        IF(Axrgtd.and.(Tdgrp.eq.0.and.Stdgrp.eq.0))Axrgtd=F
        IF(Axrghl.and.Holgrp.eq.0)Axrghl=F
c-----------------------------------------------------------------------
c     Set options for extreme value treatment for X-11 regression
c-----------------------------------------------------------------------
        otlgrp=strinx(T,Grpttx,Gpxptr,1,Ngrptx,'AO')
        IF(dpeq(Sigxrg,DNOTST))THEN
         IF((Tdgrp.gt.0.or.Xtdtst.gt.0).AND.
     &      (Holgrp.eq.0.and..not.Xeastr.and.otlgrp.eq.0).and.
     &      dpeq(Critxr,DNOTST))THEN
          Sigxrg=2.5D0
         ELSE IF(dpeq(Critxr,DNOTST))THEN
          Otlxrg=T
         END IF
        ELSE IF(Tdgrp.eq.0.OR.(Holgrp.gt.0.or.Xeastr.or.otlgrp.gt.0))
     &          THEN
         CALL eWritln(
     &       'The sigma argument of the x11regression spec can only be',
     &               STDERR,Mt2,T,F)
         CALL writln(
     &  '       specified when flow trading day variables are the only',
     &               STDERR,Mt2,F,F)
         CALL writln('       regressors in the irregular regression.',
     &               STDERR,Mt2,F,T)
         Readok=F
        END IF
        IF(Otlxrg.and.dpeq(Critxr,DNOTST))THEN
         CALL dfdate(Endxot,Begxot,Sp,nobxot)
         nobxot=nobxot+1
         IF(Cvxtyp)THEN
          Critxr=setcvl(nobxot,Cvxalf)
         ELSE
          Critxr=setcv(nobxot,Cvxalf)
         END IF
         IF(dpeq(Critxr,DNOTST))Readok=F
        END IF
c-----------------------------------------------------------------------
c     Check options for AIC trading day test
c-----------------------------------------------------------------------
        IF(Xtdtst.gt.0)THEN
         IF(Stdgrp.gt.0.AND.Xtdtst.eq.1)THEN
          Xtdtst=2
         ELSE IF(Stdgrp.gt.0.AND.Xtdtst.eq.3)THEN
          CALL eWritln('A stocktd regressor has been specified in '//
     &                 'the variables argument',STDERR,Mt2,T,F)
          CALL writln('       of x11regression but td1coef is given '//
     &                'in the aictest argument.',STDERR,Mt2,F,F)
          CALL writln(
     &           '       The type of trading day regressor must agree.',
     &           STDERR,Mt2,F,T)
          Readok=F
         ELSE IF(Tdgrp.gt.0.and.Xtdtst.eq.2)THEN
          CALL eWritln('A td or td1coef regressors has been '//
     &                 'specified in the variables argument',
     &                 STDERR,Mt2,T,F)
          CALL writln('       of x11regression but tdstock is given '//
     &                'in the aictest argument. ',STDERR,Mt2,F,F)
          CALL writln(
     &           '       The type of trading day regressor must agree.',
     &           STDERR,Mt2,F,T)
          Readok=F
         ELSE IF (Xtdtst.eq.1.or.Xtdtst.eq.3)THEN
          begcol=Grpx(Tdgrp-1)
          endcol=Grpx(Tdgrp)-1
          IF((Xtdtst.eq.1).and.(begcol.eq.endcol))THEN
           Xtdtst=3
          ELSE IF((Xtdtst.eq.3).and.(begcol.ne.endcol))THEN
           CALL eWritln('A td regressor has been specified in the '//
     &                  'variables argument of',STDERR,Mt2,T,F)
           CALL writln('       x11regression but td1coef is given '//
     &                 'in the aictest argument. ',STDERR,Mt2,F,F)
          CALL writln(
     &           '       The type of trading day regressor must agree.',
     &           STDERR,Mt2,F,T)
           Readok=F
          END IF
         END IF
c-----------------------------------------------------------------------
c     Set date for stock trading day variable
c-----------------------------------------------------------------------
         IF(Readok)THEN
          IF(Stdgrp.gt.0)THEN
           CALL getstr(Grpttx,Gpxptr,Ngrptx,Stdgrp,igrptl,nchr)
           IF(Lfatal)RETURN
           ipos=index(igrptl(1:nchr),'[')+1
           Xaicst=ctoi(igrptl(1:nchr),ipos)
          END IF
c-----------------------------------------------------------------------
c     set change of regime date for trading day AIC test.
c-----------------------------------------------------------------------
          IF(Xrgmtd)THEN
           ipos=Gpxptr(Ngrptx)-1
           rgmgrp=index(Grpttx(1:ipos),'(before ')+8
           IF(rgmgrp.eq.8)
     &        rgmgrp=index(Grpttx(1:ipos),'(change for before ')+19
           IF(rgmgrp.eq.19)
     &        rgmgrp=index(Grpttx(1:ipos),'(starting ')+10
           IF(rgmgrp.eq.10)
     &        rgmgrp=index(Grpttx(1:ipos),'(change for after ')+18
           CALL ctodat(Grpttx(1:ipos),Sp,rgmgrp,Xaicrg,argok)
           Readok=argok.and.Readok
          END IF
c     ------------------------------------------------------------------
c     If trading day regressor not specified, check to see if trading
c     day regressors can be generated for this run.
c     ------------------------------------------------------------------
          IF(Tdgrp.eq.0.and.Stdgrp.eq.0)THEN
           IF((Xtdtst.eq.3.or.Xtdtst.eq.4).and.Sp.ne.12)THEN
            CALL eWritln('Need monthly data to perform aictest for'//
     &                   ' stock trading day.',STDERR,Mt2,T,T)
            Readok=F
           ELSE IF(Sp.ne.12.and.Sp.ne.4)THEN
            CALL eWritln('Need monthly or quarterly data to perform '//
     &                   'aictest for trading day.',STDERR,Mt2,T,T)
            Readok=F
           ELSE IF(Begsrs(YR).lt.1776)THEN
            CALL eWritln('Cannot generate trading variables for '//
     &                   'aictest before 1776.',Mt2,STDERR,T,F)
            CALL writln('       Either specify a starting date, or '//
     &                  'include the century in the',Mt2,STDERR,F,F)
            CALL writln('       start or modelspan arguments of the '//
     &                  'series spec.',Mt2,STDERR,F,T)
            Readok=F
           END IF
          END IF
         END IF
c-----------------------------------------------------------------------
c    Check to see if prior length of month or length of quarter
c    adjustment factors have been specified.
c-----------------------------------------------------------------------
         IF(Priadj.gt.1)THEN
          IF(Priadj.eq.2)THEN
           CALL eWritln('Length-of-month prior adjustment (adjust='//
     &                  'lom) cannot be specified',STDERR,Mt2,T,F)
          ELSE IF(Priadj.eq.3)THEN
           CALL eWritln('Length-of-quarter prior adjustment (adjust='//
     &                  'loq) cannot be specified',STDERR,Mt2,T,F)
          ELSE IF(Priadj.eq.4)THEN
           CALL eWritln('Leap year prior adjustment (adjust='//
     &                  'lpyear) cannot be specified',STDERR,Mt2,T,F)
          END IF
          CALL getstr(XAICDC,xaicpt,PXTAIC,Xtdtst,str,nchr)
          IF(Lfatal)RETURN
          CALL writln('        when '//str(1:nchr)//
     &                ' is given in the aictest argument of the ',
     &                STDERR,Mt2,F,F)
          CALL writln('        x11regression spec.',STDERR,Mt2,F,T)
          Readok=F
         END IF
c-----------------------------------------------------------------------
        END IF
       END IF
c-----------------------------------------------------------------------
c     Check to see if X-11 and regARIMA model based trading day
c     adjustments are in the same run.
c-----------------------------------------------------------------------
       IF(Adjtd.eq.1)THEN
        IF((Axrgtd.or.Kswv.eq.1))THEN
         CALL eWritln('Irregular component regression and regARIMA '//
     &                'model-based trading',STDERR,Mt2,T,F)
         CALL writln('       day adjustment cannot be specified in '//
     &               'the same run.',STDERR,Mt2,F,T)
         Readok=F
        END IF
       END IF
c-----------------------------------------------------------------------
c     Check to see if X-11 and regARIMA model based holiday
c     adjustments are in the same run.
c-----------------------------------------------------------------------
       IF(Adjhol.eq.1.and.Axrghl)THEN
        rhol=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Easter')
        IF(rhol.eq.0)
     &     rhol=strinx(T,Grpttl,Grpptr,1,Ngrptl,'StatCanEaster')
        IF(rhol.eq.0)
     &     rhol=strinx(T,Grpttl,Grpptr,1,Ngrptl,'StockEaster')
        IF(rhol.eq.0)rhol=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Labor')
        IF(rhol.eq.0)rhol=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &                           'Thanksgiving')
        IF(rhol.gt.0)THEN
         CALL eWritln('Irregular component regression and regARIMA '//
     &                'model-based holiday',STDERR,Mt2,T,F)
         CALL writln('       adjustment cannot be specified in the '//
     &               'same run.',STDERR,Mt2,F,T)
         Readok=F
        END IF
       END IF
c-----------------------------------------------------------------------
c     Check for errors in specifying holiday adjustment
c-----------------------------------------------------------------------
       IF(Khol.eq.0)THEN
        Lgenx=F
       ELSE IF(Haveum)THEN
        Lgenx=F
        Khol=0
        CALL nWritln('An X-11 holiday adjustment cannot be performed '//
     &               'when a user-defined',fhnote,Mt2,T,F)
        CALL writln('      mean is specified for the irregular '//
     &              'regression.',fhnote,Mt2,F,T)
       ELSE
        Lgenx=T
        Khol=1
        IF(Sp.eq.4)THEN
         CALL eWritln('Cannot calculate X-11 holiday adjustment for '//
     &                'a quarterly series.',STDERR,Mt2,T,T)
         Readok=F
        END IF
        IF(Khol.gt.0)THEN
         IF(Fcntyp.eq.0)THEN
          CALL eWritln('An X-11 holiday adjustment cannot be '//
     &                 'performed when the',STDERR,Mt2,T,F)
          CALL writln('       automatic transformation selection '//
     &                'option is chosen.',STDERR,Mt2,F,T)
          Readok=F
         ELSE IF(Muladd.gt.0)THEN
          CALL eWritln('An X-11 holiday adjustment cannot be '//
     &                 'performed unless the',STDERR,Mt2,T,F)
          CALL writln('       multiplicative seasonal adjustment '//
     &                'option is chosen.',STDERR,Mt2,F,T)
          Readok=F
         END IF
        END IF
        IF(Adjhol.eq.1)THEN
         rhol=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Easter')
         IF(rhol.eq.0)
     &      rhol=strinx(T,Grpttl,Grpptr,1,Ngrptl,'StatCanEaster')
         IF(rhol.eq.0)
     &      rhol=strinx(T,Grpttl,Grpptr,1,Ngrptl,'StockEaster')
         IF(rhol.gt.0)THEN
          CALL eWritln('X-11 and regARIMA model-based Easter '//
     &                 'adjustment cannot be',STDERR,Mt2,T,F)
          CALL writln('       specified in the same run.',STDERR,Mt2,F,
     &                T)
          Readok=F
         END IF
        END IF
        IF(Easgrp.gt.0.and.Axrghl)THEN
         CALL eWritln('X-11 and irregular component regression-'//
     &                'based Easter adjustment',STDERR,Mt2,T,F)
         CALL writln('       cannot be specified in the same run.',
     &               STDERR,Mt2,F,T)
         Readok=F
        END IF
        IF(Begspn(1).lt.1901)THEN
         CALL eWritln('No X-11 holiday effect before 1901.',
     &                STDERR,Mt2,T,F)
         CALL writln(
     &           '       Try including the century in the start date',
     &               STDERR,Mt2,F,T)
         Readok=F
        END IF
       END IF
c-----------------------------------------------------------------------
c     If X-11 holidays estimated and irregular regression performed or
c     if the 0.per setting is used in regspan, set Ixreg to indicate a
c     prior adjustment.
c-----------------------------------------------------------------------
       IF(Ixreg.eq.1)THEN
        CALL dfdate(Endspn,Endxrg,Ny,Xdsp)
        IF(Khol.ge.1.or.Fxprxr.gt.0.or.Xdsp.gt.0)Ixreg=2
       END IF
c-----------------------------------------------------------------------
c     Check to see if only trading day factors are to be removed from
c     final sesonally adjusted series when user mean is specified for
c     x11regression.
c-----------------------------------------------------------------------
       IF(Noxfac.and.(.not.Finhol))THEN
        CALL eWritln('Must remove both trading day and holiday from '//
     &               'final seasonally',STDERR,Mt2,T,F)
        CALL writln('       adjusted series when user-defined mean '//
     &              'is present.',STDERR,Mt2,F,T)
        Readok=F
       END IF
c-----------------------------------------------------------------------
c     If multiplicative seasonal adjustment, test if factors are < 0,
c     set factors = 0 to 1.0, and convert percentages to ratios.
c-----------------------------------------------------------------------
       IF(Readok)THEN
        IF(Muladd.ne.1)THEN
         DO i=1,Nadj
          IF(Adjmod.eq.2)THEN
           Adj(i)=exp(Adj(i))
          ELSE IF(Adj(i).lt.ZERO)THEN
           CALL eWritln('Negative prior adjustment factors cannot be '//
     &                  'used for a',STDERR,Mt2,T,F)
           CALL writln('       multiplicative or log-additive '//
     &                 'seasonal adjustment.',STDERR,Mt2,F,T)
           Readok=F
          ELSE IF(dpeq(Adj(i),ZERO))THEN
           Adj(i)=ONE
          END IF
         END DO
         IF(Adjmod.eq.2)Adjmod=0
        END IF
       END IF
       IF(Iyrt.le.0)THEN
        IF(Iyrt.eq.NOTSET)Iyrt=0
c-----------------------------------------------------------------------
c     check to see if there are at least 5 complete years for the
c     SA totals adjustment option
c-----------------------------------------------------------------------
       ELSE
        iyrs=Lstyr-Lyr+1
        IF(Pos1bk.ne.1)iyrs=iyrs-1
        IF(Lstmo.ne.Ny)iyrs=iyrs-1
        IF(iyrs.lt.5)THEN
         CALL eWritln('The series must have at least five complete '//
     &                'years to force the',STDERR,Mt2,T,F)
         CALL writln('       yearly totals of the seasonally '//
     &               'adjusted series.',STDERR,Mt2,F,T)
         Readok=F
        END IF
        IF(Mid.eq.NOTSET)THEN
         IF(Muladd.eq.1)THEN
          Mid=1
         ELSE
          Mid=0
         END IF
        END IF
       END IF
c-----------------------------------------------------------------------
c     If any of the individual seasonal filter lengths are selected
c     using the global MSR, set Lterm to 6
c-----------------------------------------------------------------------
       Lstabl=F
       L3x5=F
       IF(Kfulsm.eq.2)THEN
        IF(Lterm.ne.NOTSET)THEN
         CALL eWritln(
     &      'Cannot specify a seasonal filter when type=trend.',
     &      STDERR,Mt2,T,T)
         Readok=F
        END IF
       ELSE
        IF(Lterm.eq.NOTSET)THEN
         Lterm=6
         DO i=1,Ny
          Lter(i)=Lterm
         END DO
        END IF
        i=1
        DO WHILE (Lterm.lt.6.and.i.le.Ny)
         IF(Lter(i).eq.6)Lterm=6
         i=i+1
        END DO
c-----------------------------------------------------------------------
c     Check to see if any of the seasonal filters is a 3x15.  If so,
c     print out a warning message if the # of years is less than 20
c-----------------------------------------------------------------------
c     Set logical variables for use of a stable, 3x5 seasonal filters
c-----------------------------------------------------------------------
        IF(Lterm.eq.5)Lstabl=T
        IF(Lterm.eq.2.or.Lterm.eq.0)L3x5=T
c-----------------------------------------------------------------------
        prtwrn=T
        IF(Lterm.eq.4.and.nyr.lt.20)THEN
         CALL wWritln(
     &      'The program will not use a 3x15 seasonal filter for',
     &      fhnote,Mt2,T,F)
         CALL writln('         series shorter than 20 years.',fhnote,
     &               Mt2,F,T)
         Lterm=5
         prtwrn=F
         Lstabl=T
        END IF
        DO i=1,Ny
         IF(Lter(i).eq.4.and.nyr.lt.20)THEN
          Lter(i)=5
          IF(.not.Lstabl)Lstabl=T
          IF(prtwrn)THEN
           CALL wWritln('The program will not use a 3x15 seasonal '//
     &                  'filter for series',fhnote,Mt2,T,F)
           CALL writln('         series shorter than 20 years.',fhnote,
     &                 Mt2,F,T)
           prtwrn=F
          END IF
c-----------------------------------------------------------------------
         ELSE IF(.not.Lstabl.and.Lter(i).eq.5)THEN
          Lstabl=T
         END IF
c-----------------------------------------------------------------------
         IF(L3x5.and.(Lter(i).ne.2.and.Lter(i).ne.0))L3x5=F
        END DO
c-----------------------------------------------------------------------
c     Set Lmsr
c-----------------------------------------------------------------------
        Lmsr=0
        IF(Lterm.eq.6)Lmsr=6
       END IF
c-----------------------------------------------------------------------
c     Check to see if Henderson moving average specified is too long.
c-----------------------------------------------------------------------
       IF(Ktcopt.gt.PMXHND)THEN
        ip1=1
        CALL itoc(PMXHND,clen,ip1)
        CALL eWritln('Length of Henderson filter cannot exceed '//
     &               clen(1:(ip1-1))//'.',STDERR,Mt2,T,T)
        Readok=F
       ELSE IF(Ktcopt.eq.1)THEN
        CALL eWritln('Length of Henderson filter must exceed 1.',
     &               STDERR,Mt2,T,T)
        Readok=F
       ELSE IF(Nbfpob.lt.Ktcopt)THEN
        ip1=1
        CALL itoc(Ktcopt,clen,ip1)
        CALL eWritln('Not enough observations in the series to apply '//
     &               'a Henderson filter',STDERR,Mt2,T,F)
        CALL writln('       of length '//clen(1:(ip1-1))//'.',
     &              STDERR,Mt2,F,T)
        Readok=F
       END IF
c-----------------------------------------------------------------------
c     set up Trend I/C ratio for generating Henderson end weights.
c-----------------------------------------------------------------------
       IF(dpeq(Tic,ZERO))THEN
        Tic=3.5D0
        IF(Ktcopt.le.9.and.Ktcopt.gt.0)Tic=ONE
        IF(Ktcopt.gt.13)Tic=4.5D0
        IF(Ktcopt.le.5.and.Ny.eq.4)Tic=0.001D0
        IF(Ktcopt.ge.7.and.Ny.eq.4)Tic=4.5D0
c-----------------------------------------------------------------------
c     If Tic is preset and automatic trend selection option, bomb.
c-----------------------------------------------------------------------
       ELSE IF(Ktcopt.eq.0)THEN
        CALL eWritln(
     &     'I/C ratio cannot be specified when the automatic trend',
     &     STDERR,Mt2,T,F)
        CALL writln('       filter option is used.',STDERR,Mt2,F,T)
        Readok=F
       END IF
c-----------------------------------------------------------------------
c     If savelog=alldiagnostics in either the x11 or composite specs,
c     set svltab to allow all diagnostics to be saved to the log file
c-----------------------------------------------------------------------
       IF(Svltab(LSLALX))THEN
        DO i=LSLM1,LSLIDS
         Svltab(i)=T
        END DO
       END IF
       IF(Svltab(LSLALI))THEN
        DO i=LSLIM1,LSLITT
         Svltab(i)=T
        END DO
       END IF
      END IF
      IF(Lmodel)THEN
       IF(Svltab(LSLALA))THEN
        DO i=LSLAMD,LSLFUR
         Svltab(i)=T
        END DO
       END IF
       IF(Svltab(LSLALE))THEN
        DO i=LSLAIC,LSLAFC
         Svltab(i)=T
        END DO
       END IF
       IF(Svltab(LSLALC))THEN
        DO i=LSLNRM,LSLSFT
         Svltab(i)=T
        END DO
       END IF
      END IF
      IF(Svltab(LSLALR))THEN
       DO i=LSLASA,LSLAFE
        Svltab(i)=T
       END DO
      END IF
      IF(Svltab(LSLALP))THEN
       IF(Ny.eq.12)THEN
        DO i=LSLSPK,LSLISP
         Svltab(i)=T
        END DO
       END IF
       DO i=LSLTPK,LSLQCH
        Svltab(i)=T
       END DO
      END IF
c-----------------------------------------------------------------------
c     Set sliding spans indicator variable for trading day (itd)
c-----------------------------------------------------------------------
      IF(Issap.gt.0)THEN
       IF((Axrgtd.and.(.not.Noxfac)).or.(Adjtd.eq.1.and.Ssinit.ne.1))
     &     Itd=1
c-----------------------------------------------------------------------
c     Set sliding spans indicator variable for holiday (ihol)
c-----------------------------------------------------------------------
       IF(Khol.gt.0.or.((Adjhol.eq.1.or.Finhol).and.Ssinit.ne.1))Ihol=1
c-----------------------------------------------------------------------
c     Print error message if user-defined span length is not long
c     enough.
c-----------------------------------------------------------------------
       IF(Nlen.gt.0.and.Nlen.lt.Ny*3)THEN
        CALL wWritln('Length of sliding span must be at least 3 years.',
     &               fhnote,Mt2,T,F)
        CALL writln(
     &     '         Sliding spans analysis will not be performed.',
     &     fhnote,Mt2,F,T)
        Issap=0
       ELSE
c-----------------------------------------------------------------------
c     Otherwise, set up cutoff values.
c-----------------------------------------------------------------------
        DO i=1,5
         DO j=1,4
          IF(i.eq.4)THEN
           Cut(i,j)=Sscut(i)+(j-1)*2D0
          ELSE
           Cut(i,j)=Sscut(i)+(j-1)
          END IF
         END DO
        END DO
        Cut(4,4)=Cut(4,4)+ONE
       END IF
      END IF
c-----------------------------------------------------------------------
c     Initialize variables used for type-of-month trading day table.
c-----------------------------------------------------------------------
      IF(Posfob.eq.Posffc)THEN
       nsp=Sp
      ELSE IF(Nfcstx.gt.Nfcst)THEN
       nsp=Nfcstx-Nfcst
      ELSE IF(Sp.gt.Nfcst)THEN
       nsp=Sp-Nfcst
      ELSE
       nsp=0
      END IF
      CALL tdset(Sp,Tdgrp,Begbak,Pos1bk,Posffc+nsp,ktd,Ixreg,Adjtd,
     &           Adjusr,Kswv,Noxfac)
c-----------------------------------------------------------------------
c     Check composite adjustment options
c-----------------------------------------------------------------------
      IF(Iagr.eq.1.and.(Lchkin.or.Lcomp))THEN
c-----------------------------------------------------------------------
c     Set up indicator vector for beginning date, ending date and
c     seasonal period of composite adjustment
c-----------------------------------------------------------------------
       Iagr=2
       Itest(1)=Sp
       Itest(2)=Begspn(MO)
       Itest(3)=Endspn(MO)
       Itest(4)=Begspn(YR)
       Itest(5)=Endspn(YR)
c-----------------------------------------------------------------------
c     Check if proper dates were specified for composite adjustment
c-----------------------------------------------------------------------
      ELSE IF(Iagr.eq.2.and.Iag.ge.0.and.((Itest(2).ne.Begspn(MO))
     &        .or.(Itest(3).ne.Lstmo).or.(Itest(4).ne.Lyr)
     &        .or.(Itest(5).ne.Lstyr)))THEN
       CALL eWritln('Component series '//Serno(1:Nser)//
     &              ' to be aggregated has a different',STDERR,Mt2,T,F)
       CALL writln(
     &    '       time span.  Aggregation will not be computed.',
     &    STDERR,Mt2,F,T)
       Readok=F
       Iagr=-1
      END IF
c-----------------------------------------------------------------------
c     Check to see if regARIMA model will be fit when model based
c     regression effects are specified for adjustment.
c-----------------------------------------------------------------------
      IF(Adjtd.eq.1.or.Adjhol.eq.1.or.Adjls.eq.1.or.Adjao.eq.1.or.
     &    Adjtc.eq.1.or.Adjso.eq.1.or.Adjusr.eq.1.or.Adjsea.eq.1.or.
     &    Finhol.or.Finao.or.Finls.or.Fintc.or.Finusr)THEN
       IF((.not.Lmodel).OR.(.not.Ldestm))THEN
        IF(Adjtd.eq.1)Adjtd=0
        IF(Adjhol.eq.1)Adjhol=0
        IF(Adjao.eq.1)Adjao=0
        IF(Adjls.eq.1)Adjls=0
        IF(Adjtc.eq.1)Adjtc=0
        IF(Adjso.eq.1)Adjso=0
        IF(Adjusr.eq.1)Adjusr=0
        IF(Adjsea.eq.1)Adjsea=0
        IF(Finhol.and.(.NOT.(Axrghl.or.Axruhl.OR.Khol.ge.1.or.
     &     Leastr.or.Xeastr)))Finhol=F
        IF(Finao)Finao=F
        IF(Finls)Finls=F
        IF(Fintc)Fintc=F
        IF(Finusr)Finusr=F
       ELSE IF (Nb.gt.0) THEN
c     ------------------------------------------------------------------
c     Determine which of the regressors are currently present in the
c     regARIMA model.  First, initialize counters
c     ------------------------------------------------------------------
        nusr=0
        nseas=0
        ntd=0
        Nao=0
        Nls=0
        Ntc=0
        Nso=0
        Nramp=0
        Nln=0
        Nsln=0
        Nlp=0
        Nseq=0
        Nhol=0
        Neas=0
        iusr=1
c-----------------------------------------------------------------------
c     Determine type of regression variable
c-----------------------------------------------------------------------
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
     &      (rtype.eq.PRGUTD.or.rtype.eq.PRGULM.or.rtype.eq.PRGULQ.or.
     &      rtype.eq.PRGULY))THEN
            Ntd=Ntd+1
          IF(rtype.eq.PRGTTD.or.rtype.eq.PRRTTD.or.rtype.eq.PRATTD.or.
     &       rtype.eq.PRG1TD.or.rtype.eq.PRR1TD.or.rtype.eq.PRA1TD.or.
     &       (Isrflw.eq.0.and.rtype.eq.PRGUTD))
     &       Nflwtd=Nflwtd+1
          IF(rtype.eq.PRGTLM.or.rtype.eq.PRGTLQ.or.
     &       rtype.eq.PRRTLM.or.rtype.eq.PRRTLQ.or.
     &       rtype.eq.PRATLM.or.rtype.eq.PRATLQ.or.
     &       rtype.eq.PRGULM.or.rtype.eq.PRGULQ)
     &       Nln=Nln+1
          IF(rtype.eq.PRGTSL.or.rtype.eq.PRRTSL.or.rtype.eq.PRATSL.or.
     &      (Isrflw.eq.1.and.rtype.eq.PRGULM))Nsln=Nsln+1
          IF(rtype.eq.PRGTLY.or.rtype.eq.PRRTLY.or.rtype.eq.PRATLY.or.
     &       rtype.eq.PRGULY)Nlp=Nlp+1
         END IF
c-----------------------------------------------------------------------
c     regARIMA holiday regressors
c-----------------------------------------------------------------------
         IF(rtype.eq.PRGTEA.or.rtype.eq.PRGTEC.or.rtype.eq.PRGTES.or.
     &      rtype.eq.PRGTLD.or.rtype.eq.PRGTTH.or.(rtype.ge.PRGTUH.and.
     &      rtype.le.PRGUH5))THEN
          Nhol=Nhol+1
          IF(rtype.eq.PRGTEA.or.rtype.eq.PRGTEC.or.rtype.eq.PRGTES)
     &       Neas=Neas+1
         END IF
c-----------------------------------------------------------------------
c     regARIMA User-defined regressors
c-----------------------------------------------------------------------
         IF(rtype.eq.PRGTUD)nusr=nusr+1
c-----------------------------------------------------------------------
c     regARIMA seasonal regressors
c-----------------------------------------------------------------------
         IF(rtype.eq.PRGTUS)nseas=nseas+1
c-----------------------------------------------------------------------
c     regARIMA AO outlier regressors
c-----------------------------------------------------------------------
         IF(rtype.eq.PRGTAO.or.rtype.eq.PRSQAO.or.rtype.eq.PRGTMV.or.
     &      rtype.eq.PRGUAO)Nao=Nao+1
c-----------------------------------------------------------------------
c     regARIMA Level Change Outlier regressors
c-----------------------------------------------------------------------
         IF(rtype.eq.PRGTLS.or.rtype.eq.PRGTRP.or.rtype.eq.PRGTTL.or.
     &      rtype.eq.PRGTQI.or.rtype.eq.PRGTQD.or.rtype.eq.PRSQLS.or.
     &      rtype.eq.PRGULS)THEN
          Nls=Nls+1
          IF(rtype.eq.PRGTRP.or.rtype.eq.PRGTQI.or.rtype.eq.PRGTQD)
     &       Nramp=Nramp+1
         END IF
c-----------------------------------------------------------------------
c     regARIMA Temporary Change Outlier regressors
c-----------------------------------------------------------------------
         IF(rtype.eq.PRGTTC)Ntc=Ntc+1
c-----------------------------------------------------------------------
c     regARIMA Seasonal Outlier regressors
c-----------------------------------------------------------------------
         IF(rtype.eq.PRGTSO.and.rtype.eq.PRGUSO)Nso=Nso+1
c-----------------------------------------------------------------------
c     regARIMA Sequence Outlier regressors
c-----------------------------------------------------------------------
         IF(rtype.eq.PRSQAO.or.rtype.eq.PRSQLS)Nseq=Nseq+1
        END DO
c-----------------------------------------------------------------------
c     reset regression adjustment indicators if no regession effect
c     found and print warning messages.
c-----------------------------------------------------------------------
        IF(Adjtd.eq.1.and.Ntd.eq.0)Adjtd=0
        IF(Adjhol.eq.1.and.Nhol.eq.0)THEN
         Adjhol=0
         IF((.NOT.(Axrghl.or.Axruhl.or.Khol.ge.1.or.Leastr.or.Xeastr))
     &      .and.Finhol)Finhol=F
        END IF
        IF(Adjsea.eq.1.and.nseas.eq.0)Adjsea=0
        IF(nusr.eq.0)THEN
         IF(Adjusr.eq.1)Adjusr=0
         IF(Finusr)Finusr=F
        END IF
        IF((.not.Ltstao).and.Nao.eq.0)THEN
         IF(Adjao.eq.1)Adjao=0
         IF(Finao)Finao=F
        END IF
        IF((.not.Ltstls).and.Nls.eq.0)THEN
         IF(Adjls.eq.1)Adjls=0
         IF(Finls)Finls=F
        END IF
        IF((.not.Ltsttc).and.Ntc.eq.0)THEN
         IF(Adjtc.eq.1)Adjtc=0
         IF(Fintc)Fintc=F
        END IF
        IF(Adjso.eq.1.and.Nso.eq.0)Adjso=0
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Lmodel)THEN
c-----------------------------------------------------------------------
c    IF automatic outlier identification done, set variables to allow
c    prior adjustment of outlier regressors
c-----------------------------------------------------------------------
       IF(Adjao.eq.0.AND.Ltstao)Adjao=1
       IF(Adjls.eq.0.AND.Ltstls)Adjls=1
       IF(Adjtc.eq.0.AND.Ltsttc)Adjtc=1
*       IF(Adjso.eq.0.AND.Ltstso)Adjso=1
c-----------------------------------------------------------------------
c    If automatic modeling or testing procedures performed, disable
c    saving of model iteration information.
c-----------------------------------------------------------------------
       IF(((Lautom.or.Lautox).or.Itdtst.gt.0.or.Leastr.or.Luser.or.
     &    Fcntyp.eq.0).and.Savtab(LESTIT))THEN
        Savtab(LESTIT)=F
        CALL wWritln('Cannot save iteration iformation for regARIMA'//
     &               ' model estimation',fhnote,Mt2,T,F)
        CALL writln('         when automatic modeling, AIC tests, '//
     &              'or automatic transformation',fhnote,Mt2,F,F)
        CALL writln('         selection is used.',fhnote,Mt2,F,T)
       END IF
c-----------------------------------------------------------------------
c    If automatic modeling or testing procedures performed, exact
c    maximum likelihood estimation must be selected for both MA and AR.
c-----------------------------------------------------------------------
        IF(((Itdtst.gt.0.or.Leastr.or.Luser).or.Lautom.or.Fcntyp.eq.0)
     &       .and.(.not.(Lextar.and.Lextma)))THEN
        CALL eWritln(
     &     'Exact maximum likelihood estimation must be selected when',
     &     STDERR,Mt2,T,F)
        CALL writln('       AIC tests, automdl, or automatic '//
     &              'transformation selection is used.',STDERR,Mt2,F,T)
        Readok=F
       END IF
c-----------------------------------------------------------------------
c   Only test the forecast error of the identified model if X-11
c   seasonal adjustment is done (BCM July 2007)
c-----------------------------------------------------------------------
       IF((.not.Lx11).and.Lrejfc)THEN
        Lrejfc=F
        CALL nWritln('Since X-11 seasonal adjustment is not done, '//
     &               'the forecast error will not',fhnote,Mt2,T,F)
        CALL writln(
     &     '      be checked after the ARIMA model is identified.',
     &     fhnote,Mt2,F,T)
       END IF
c-----------------------------------------------------------------------
      END IF
c-----------------------------------------------------------------------
      IF((.not.Lx11).and.(Spcsrs.eq.3))THEN
       Spcsrs=2
       CALL nWritln('Since X-11 seasonal adjustment is not done, '//
     &              'the E1 table is not available.',fhnote,Mt2,T,F)
       CALL writln('      The B1 table will be used for the spectrum'//
     &             ' of the original series.',fhnote,Mt2,F,T)
      END IF
c-----------------------------------------------------------------------
c     Copy data into variables Series and Orig
c-----------------------------------------------------------------------
      Nomnfy=Nobs-Frstsy+1
*      write(Mtprof,*) ' Y(Frstsy) = ',Y(Frstsy)
      CALL copy(Y(Frstsy),Nomnfy,-1,Orig2(Pos1ob))
      CALL copy(Y(Frstsy),Nspobs,-1,Series(Pos1ob))
      CALL copy(Y(Frstsy),Nomnfy,-1,Orig(Pos1ob))
*      write(Mtprof,*) ' Orig(Pos1ob) = ',Orig(Pos1ob)
c-----------------------------------------------------------------------
C --- Set up logical vector which shows if all values for the series
c     is > 0 if pseudo-additive seasonal adjustment is performed.
c-----------------------------------------------------------------------
      DO i=Pos1ob,Posfob
       Gudval(i)=T
       IF(Psuadd.and.Series(i).le.ZERO)Gudval(i)=F
      END DO
c-----------------------------------------------------------------------
c     Check to see if pseudo-additive seasonal adjustment can be
c     performed.
c-----------------------------------------------------------------------
      IF(Psuadd)THEN
       IF(Adjtd.eq.1.or.Adjls.eq.1.or.Adjhol.eq.1.or.Adjao.eq.1.or.
     &    Adjtc.eq.1.or.Adjusr.eq.1.or.Adjsea.eq.1.or.Finhol.or.
     &    Finao.or.Finls.or.Fintc.or.Finusr)THEN
        CALL eWritln('Pseudo-additive seasonal adjustment cannot be '//
     &               'performed when',fhnote,Mt2,T,F)
        CALL writln('       preadjustment factors are derived from '//
     &              'a REGARIMA model.',fhnote,Mt2,F,T)
        Readok=F
       ELSE IF(Axrgtd.or.Axrghl)THEN
        CALL eWritln('Pseudo-additive seasonal adjustment and '//
     &               'irregular component',fhnote,Mt2,T,F)
        CALL writln('       calendar adjustment cannot be specified '//
     &              'in the same run.',fhnote,Mt2,F,T)                                        
        Readok=F
       ELSE IF(Priadj.gt.1.or.Nuspad.gt.0.or.Nustad.gt.0)THEN
        CALL eWritln('Cannot use prior adjustment factors in a '//
     &               'pseudo-additive seasonal',fhnote,Mt2,T,F)
        CALL writln('       adjustment.',fhnote,Mt2,F,T)
        Readok=F
       ELSE IF(Nfcst.eq.0)THEN
        CALL wWritln('Pseudo-additive seasonal adjustment will not '//
     &               'produce forecasts',fhnote,Mt2,T,F)
        CALL writln('         of the final seasonal difference '//
     &              'unless regARIMA forecasts are',fhnote,Mt2,F,F)
        CALL writln('         used to extend the series.',fhnote,Mt2,
     &              F,T)
        CALL writln('         The regARIMA model used to extend '//
     &              'the series cannot include',fhnote,Mt2,T,F)
        CALL writln('         regressors that result in '//
     &              'preadjustment factors (such as outlier,',
     &              fhnote,Mt2,F,F)
        CALL writln('         trading day or holiday regressors) '//
     &              'when pseudo-additive seasonal',fhnote,Mt2,F,F)
        CALL writln('         adjustment is used.  If your model '//
     &              'has such regressors, use the',fhnote,Mt2,F,F)
        CALL writln('         noapply argument of the regression spec.',
     &              fhnote,Mt2,F,T)
       END IF
      END IF
c-----------------------------------------------------------------------
c     If the series is a component series and the run is just checking
c     input, aggregate series before leaving routine.
c-----------------------------------------------------------------------
      IF(Lchkin)THEN
       IF(Iagr.eq.2.and.Iag.ge.0)THEN
        CALL setapt(0,0,Begspn,Sp)
        CALL agr(Series,O,Iag,Pos1ob,Posfob,Pos1ob,W)
       END IF
       RETURN
      END IF
c-----------------------------------------------------------------------
c     If composite adjustment chosen, set up data vectors.
c-----------------------------------------------------------------------
c      IF(Iagr.eq.3)THEN
cc       IF(Lcomp)Lcomp=F
c       IF(Pos1ob.gt.1)THEN
c        n=Posfob-Pos1ob+1
c        CALL copy(O,n,-1,O(Posfob))
c        CALL copy(Omod,n,-1,Omod(Posfob))
c        CALL copy(Ci,n,-1,Ci(Posfob))
c       END IF
c      END IF
c-----------------------------------------------------------------------
c     set prior adjustment indicator according to whether prior
c     adjustment is done to original series.
c-----------------------------------------------------------------------
      Lpradj=F
      IF(Kfmt.gt.0)Lpradj=T
c-----------------------------------------------------------------------
c   make changes to selected input parameters if SEATS is used for
c   seasonal adjustment
c-----------------------------------------------------------------------
      IF(Lseats)THEN
       IF(Maxord(1).eq.4)THEN
        Maxord(1)=3
        CALL nWritln('The maximum regular ARIMA order that the '//
     &               'automatic model selection',STDERR,Mt2,T,F)
        CALL writln('      procedure will identify has been changed '//
     &              'to three (3) since SEATS',STDERR,Mt2,F,F)
        CALL writln(
     &     '      seasonal adjustments are generated in this run.',
     &     STDERR,Mt2,F,T)
       END IF
c-----------------------------------------------------------------------
c     If seats seasonal adjustment is to be done, and stable seasonal
c     regressors are specified, allow X-13A-S to generate seasonal
c     factors from the regressors and remove before doing signal
c     extraction (added by BCM 04-10-05)
c-----------------------------------------------------------------------
       IF(Lseff)THEN
        IF(Adjsea.eq.0)Adjsea=1
       END IF
c-----------------------------------------------------------------------
c     If savelog=alldiagnostics in the seats spec, set svltab to allow
c     all diagnostics to be saved to the log file
c-----------------------------------------------------------------------
       IF(Svltab(LSLALS))THEN
        DO i=LSLSMD,LSLSSG
         Svltab(i)=T
        END DO
       END IF
c-----------------------------------------------------------------------
c     reset Iyrt to 0
c-----------------------------------------------------------------------
       IF(Iyrt.eq.NOTSET)Iyrt=0
      END IF
c-----------------------------------------------------------------------
      IF(Nbcst2.gt.0)Lyr=Begbk2(YR)
c-----------------------------------------------------------------------
c     Generate peak indexes, frequencies for spectral estimates
c-----------------------------------------------------------------------
      IF (Ny.eq.12) THEN
       CALL mkpeak(Peakwd,Lfqalt)
       CALL mkfreq(Peakwd,Lfqalt,Lprsfq)
      END IF
c     ------------------------------------------------------------------
c     Try to open file to store seasonal adjustment diagnostics.
c     ------------------------------------------------------------------
      lexsum=F
      IF(Lgraf.or.Lsumm.gt.0.or.Hvmtdt)THEN
       IF(Lgraf)THEN
        fil=Curgrf(1:Ngrfcr)//'.udg'
        nchr=Ngrfcr+4
       ELSE 
        fil=Cursrs(1:Nfilcr)//'.udg'
        nchr=Nfilcr+4
       END IF
       INQUIRE(FILE=fil(1:nchr),EXIST=lexsum)
       IF(Lgraf.or.Lsumm.gt.0)THEN
        CALL fopen(fil(1:nchr),
     &            'seasonal adjustment and modeling diagnostics',
     &            'UNKNOWN',Nform,argok)
       ELSE
        CALL fopen(fil(1:nchr),'user specified metadata',
     &             'UNKNOWN',Nform,argok)
       END IF
       Readok=argok.and.Readok
       IF(argok)Opnudg=T
       IF(Readok)THEN
        IF(Lgraf.or.Lsumm.gt.0)THEN
         WRITE(STDERR,1010)'diagnostics output',fil(1:nchr)
        ELSE IF(Hvmtdt)THEN
         WRITE(STDERR,1010)'metadata',fil(1:nchr)
        ELSE
         WRITE(STDERR,1020)' '
        END IF
       END IF
C-----------------------------------------------------------------------
c     If graphics and diagnostic option specified in same run, print
c     warning message that diagnostic file(s) will be written to
c     graphics file directory rather than output directory.
C-----------------------------------------------------------------------
       IF(Readok.and.Lgraf)THEN
        IF(Lsumm.gt.0)THEN
         CALL nWritln('The '//PRGNAM//' diagnostic file (.udg) '//
     &                'has been stored',
     &                fhnote,Mt2,T,F)
        ELSE IF(Hvmtdt)THEN
         CALL nWritln('The '//PRGNAM//' metadata file (.udg) '//
     &                'has been stored',
     &                fhnote,Mt2,T,F)
        END IF
        IF(Lsumm.gt.0.or.Hvmtdt)THEN
         CALL writln('      in the directory specified by the '//
     &               'graphics (-g) option.',
     &                fhnote,Mt2,F,T)
        END IF
       END IF
      ELSE IF(Readok)THEN
       IF(Lgraf.or.Lsumm.gt.0)THEN
        WRITE(STDERR,1010)'diagnostics output',fil(1:nchr)
       ELSE IF(Hvmtdt)THEN
        WRITE(STDERR,1010)'metadata',fil(1:nchr)
       ELSE
        WRITE(STDERR,1020)' '
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Opnudg.and.Lsumm.eq.1)THEN
       IF(Ltimer.and.Lgraf)THEN
        WRITE(STDERR,1200)PRGNAM
       ELSE IF(Ltimer)THEN
        WRITE(STDERR,1300)PRGNAM,'timer (-t)'
       ELSE IF(Lgraf)THEN
        WRITE(STDERR,1300)PRGNAM,'graphics (-g)'
       END IF
      END IF
c-----------------------------------------------------------------------
c     Print header page and Title info
c-----------------------------------------------------------------------
      IF(Readok)THEN
       CALL x12hdr(Nfcst,Srsttl,Nsrscr,Ttlvec,Notc,Lx11,Lmodel,Lseats,
     &             Begspn,Nuspad,Nustad,Iqtype,Fcntyp,Lam,Ciprob,Dattim,
     &             Cnstnt,Isrflw,Lognrm)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Write out summary of files saved by this run of X-13A-S (including
c     the main output, error, and seasonal adj. diagnostic files).
c-----------------------------------------------------------------------
       IF(sav.or.Lexout.or.Lexerr.OR.(Lsumm.gt.0))THEN
c-----------------------------------------------------------------------
c     If no tables are printed out, see if there are any tables being
c     overwritten.
c-----------------------------------------------------------------------
        IF(.not.Prttab(LSRSSV).or.Lnoprt)THEN
         IF(Lexout.or.Lexerr.or.((Lsumm.gt.0).and.lexsum))THEN
          Fhandl=Mt2
          Lnoind=F
         ELSE
          Fhandl=0
          Lnoind=.not.Lnoprt
          Lexist=F
          i=1
          DO WHILE (.not.Lexist.and.i.le.NTBL)
           IF(.not.sumtab(i))THEN
            IF(Savtab(i))CALL opnfil(F,F,i,Fhandl,argok)
           END IF
           i=i+1
          END DO
          IF(Lexist)Fhandl=Mt2
         END IF
c-----------------------------------------------------------------------
c     IF no table printed out and files are overwritten, print warning
c     message.
c-----------------------------------------------------------------------
         IF(Fhandl.gt.0)THEN
          IF(Lquiet)THEN
           WRITE(Mt1,1060)PRGNAM,Cursrs(1:Nfilcr)
          ELSE
           WRITE(STDERR,1070)PRGNAM,Cursrs(1:Nfilcr)
          END IF
         END IF
        ELSE
         Fhandl=Mt1
        END IF
c-----------------------------------------------------------------------
c     Print entries for save files
c-----------------------------------------------------------------------
        IF(Fhandl.gt.0)THEN
         IF(sav)THEN
          Lfrtop=T
          DO i=1,NTBL
           IF(.not.sumtab(i))THEN
            IF(Savtab(i))CALL opnfil(F,F,i,Fhandl,argok)
           END IF
          END DO
          IF(Lfrtop)THEN
           WRITE(Fhandl,1080)
           CALL mkPClass(Fhandl,'indent')
           Lfrtop=F
          END IF
         ELSE
          WRITE(Fhandl,1080)
          CALL mkPClass(Fhandl,'indent')
         END IF
c-----------------------------------------------------------------------
c     Print entries for the main output, error, and seasonal adj.
c     diagnostic files.
c-----------------------------------------------------------------------
         ctmp=' '
         IF(Lexout)ctmp='*'
         WRITE(Fhandl,1090)Cursrs(1:Nfilcr)//'.html',ctmp,
     &                     'program output file'//Cbr
         ctmp=' '
         IF(Lexerr)ctmp='*'
         WRITE(Fhandl,1090)Cursrs(1:Nfilcr)//'_err.html',ctmp,
     &                     'program error file'//Cbr
         IF(Lsumm.gt.0)THEN
          ctmp=' '
          IF(Lexsum)ctmp='*'
          WRITE(Fhandl,1090)Cursrs(1:Nfilcr)//'.udg',ctmp,
     &             'seasonal adjustment and model diagnostics file'//Cbr
         END IF
         IF(Lgraf)THEN
          ctmp=' '
          IF(Lexgrf)ctmp='*'
          WRITE(Fhandl,1090)Cursrs(1:Nfilcr)//'.gmt',ctmp,
     &                      'graphics metafile'//Cbr
         END IF
         CALL writTag(Fhandl,'</p>')
         CALL mkPOneLine(Fhandl,'@','&nbsp;')
         IF(Lnoind)CALL genSkip(1031)
         CALL writTagOneLine(Fhandl,'h3','@',
     &                       'LINKS TO OTHER HTML FILES')
         CALL writTagClass(Fhandl,'ul','indent')
         idx1=Idxlog-1
         n1=Ncslast+1
         CALL makeIndexLink(Fhandl,-1,CsrsHTML(n1:NcsHTML)//'_err.html',
     &                      'Error file for '//Serno(1:Nser),F,T)
         n2=Nlflast+1
         samepth=n1.eq.n2
         IF (samepth) THEN
          IF (n1.gt.1) samepth=Cursrs(1:n1).eq.Logfil(1:n2)
         END IF
         IF (samepth) THEN
          CALL makeIndexLink(Fhandl,idx1,
     &                       LogfHTML(1:NlfHTML)//'_log.html',
     &                       'Log entry for '//Serno(1:Nser),F,T)
         ELSE
          Infoot=Infoot+1
          Vfoot(Infoot)=PLGLNK
          WRITE(Fhandl,1100)'*',Infoot,'*',Infoot,
     &                      LogfHTML(1:NlfHTML)//'_log.html',idx1,
     &                      'Log entry for '//Serno(1:Nser)
         END IF
         CALL writTag(Fhandl,'</ul>')
         IF(.not.samepth)THEN
          CALL weWritln('(*) Link to Log File specified above only '//
     &                 'valid if complete paths specified for output '//
     &                'and meta files.','Link to Log File in the Main'//
     &                ' Output valid only if complete paths specified'//
     &                ' for output and meta files',Mt1,Mt2,T,T)
         END IF
        END IF
       END IF
c-----------------------------------------------------------------------
c     Print and/or Save contents of input spc file
c-----------------------------------------------------------------------
       IF(Savtab(LSRSIN))THEN
        fil=Cursrs(1:Nfilcr)//'.spc'
        nchr=Nfilcr+4
        CALL fopen(fil(1:nchr),'input specification file',
     &             'UNKNOWN',nspc,argok)
        Readok=argok.and.Readok
       END IF
       IF(Prttab(LSRSIN).or.Savtab(LSRSIN))THEN
        REWIND(Mt)
*        IF(Prttab(LSRSIN).and.Lpage)THEN
*         WRITE(Mt1,Ttlfmt)Newpg,Title(1:Ntitle),Kpage,Serno(1:Nser)
*         Kpage=Kpage+1
*        END IF
        IF(Prttab(LSRSIN))THEN
         CALL genSkip(1032)
         CALL makeSkipLink(Mt1,Idxtab,
     &                     'CONTENTS OF INPUT SPECIFICATION FILE',T)
         WRITE(Mt1,1030)'spc',Infile(1:nblank(Infile))
        END IF
        i=1
        DO WHILE (T)
         READ(Mt,1040,END=10)line
         IF(nblank(line).gt.0)THEN
          IF(Savtab(LSRSIN))WRITE(nspc,1020)line(1:nblank(line))
          IF(Prttab(LSRSIN))WRITE(Mt1,1050)i,line(1:nblank(line))
         ELSE
          IF(Savtab(LSRSIN))WRITE(nspc,1020)' '
          IF(Prttab(LSRSIN))WRITE(Mt1,1050)i,' '
         END IF
         i=i+1
        END DO
   10   CALL fclose(Mt)
        IF(Savtab(LSRSIN))CALL fclose(nspc)
        CALL writTag(Mt1,'</pre>')
        IF(Hvmfil)THEN
         REWIND(Mtm)
*         IF(Lpage)THEN
*          WRITE(Mt1,Ttlfmt)Newpg,Title(1:Ntitle),Kpage,Serno(1:Nser)
*          Kpage=Kpage+1
*         END IF
         CALL genSkip(1033)
         CALL makeSkipLink(Mt1,Idxtab,'CONTENTS OF SAVED MODEL FILE',T)
         WRITE(Mt1,1030)'model',Mdlfil(1:nblank(Mdlfil))
         i=1
         DO WHILE (T)
          READ(Mtm,1040,END=20)line
          IF(nblank(line).gt.0)THEN
           WRITE(Mt1,1050)i,line(1:nblank(line))
          ELSE
           WRITE(Mt1,1050)i,' '
          END IF
          i=i+1
         END DO
   20    CALL fclose(Mtm)
         CALL writTag(Mt1,'</pre>')
        END IF
       END IF
c-----------------------------------------------------------------------
c     If using compositing option to derive composite total without
c     adjusting series, update the direct original series
c-----------------------------------------------------------------------
       IF(Lcomp.and.Iagr.eq.2.and.Iag.ge.0)THEN
        CALL setapt(0,0,Begspn,Sp)
        CALL agr(Series,O,Iag,Pos1ob,Posfob,Pos1ob,W)
       END IF
c-----------------------------------------------------------------------
      ELSE IF(.not.Readok)THEN
       CALL writln('No seasonal adjustment this run',STDERR,Mt2,T,T)
      END IF
c-----------------------------------------------------------------------
 1010 FORMAT('  Storing any ',a,' into ',a,/)
 1020 FORMAT(a)
 1030 FORMAT(/,'<p>Contents of ',a,' file ',a,'</p>',/,'<pre>',/,
     &         ' Line #',/,' ------')
 1040 FORMAT(a120)
 1050 FORMAT(1x,i6,': ',a)
 1060 FORMAT(/,' <p><strong>WARNING:</strong> Existing files will ',
     &         'be overwritten by this run of ',a,'.',/,
     &         ' A complete listing of all the files produced by this',
     &         ' run can be found in ',a,'.err</p>')
 1070 FORMAT(/,' WARNING: Existing files will be overwritten by ',
     &         'this run of ',a,'.',/,10x,
     &         'A complete listing of all the files produced by this',
     &         ' run ',/,10x,'can be found in ',a,'.err')
 1080 FORMAT(/,'<p><strong>FILE SAVE REQUESTS</strong>',
     &         ' (* indicates file exists and will be overwritten)</p>')
 1090 FORMAT('  ',a,a,' ',a)
 1100 FORMAT('<li> ',a,' <a href="#footnote',i4.4,'" class=',
     &       '"longdesc">Link to definition of ',a,'</a><a name="foot',
     &       i4.4,'"></a>',/,'<a href="',a,'#pos',i5.5,'">',a,
     &       '</a></li>')
 1200 FORMAT(/,'  NOTE: The ',a,' diagnostic file (.udg) is generated ',
     &       /,'        since both the graphics (-g) and timer (-t) ',
     &       'options were',/,'        specified.')
 1300 FORMAT(/,'  NOTE: The ',a,' diagnostic file (.udg) is generated ',
     &       /,'        since the ',a,' option was specified.')
c-----------------------------------------------------------------------
      RETURN
      END
      
