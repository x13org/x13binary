C     Last change:  BCM  19 May 2003    9:46 am
      SUBROUTINE spcdrv(Muladd,Iagr,Kswv,Ny,Lx11,Kfulsm,X11agr,Lseats,
     &                  Psuadd,Lgraf,Lmodel)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Routine which computes the spectrum for the detrended original
c     series, detrended seasonally adjusted series, and the irregular
c     component modified for extreme values.  If there are peaks
c     detected at the trading day or seasonal frequencies, these are
c     noted.
c-----------------------------------------------------------------------
c     AR-Spectrum routines originally appeared in the BAYSEA program,
c     developed by H. Akaike and G. Kitagawa of the Institute for
c     Statistical Mathematics.
c-----------------------------------------------------------------------
      CHARACTER STTDIC*74
      INTEGER PSTT,IONE,SIXONE,IZERO,ITEN,NTDLIM
      DOUBLE PRECISION ZERO,ONE
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.,PSTT=4,IONE=1,SIXONE=61,IZERO=0,
     &          ZERO=0D0,ONE=1D0,ITEN=10,NTDLIM=60,
     &          STTDIC='differencedundifferenceddifferenced, transformed
     &undifferenced, transformed')
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'tbltitle.prm'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11fac.cmn'
      INCLUDE 'inpt.cmn'
      INCLUDE 'model.cmn'
      INCLUDE 'seatcm.cmn'
      INCLUDE 'seatlg.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'rho.cmn'
      INCLUDE 'spcidx.cmn'
      INCLUDE 'x11srs.cmn'
      INCLUDE 'x11log.cmn'
      INCLUDE 'orisrs.cmn'
      INCLUDE 'adxser.cmn'
      INCLUDE 'tukey.cmn'
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'spcsvl.i'
      INCLUDE 'mdltbl.i'
      INCLUDE 'spctbl.i'
c-----------------------------------------------------------------------
      CHARACTER ttl*(PTTLEN),pkstr*(24),begstr*(10),cttl*(26),arstr*(2),
     &          ctype*(19),endstr*(10),spcstr*(36),csatbl*(11),
     &          skystr*(10)
      DOUBLE PRECISION Lam,orisxx,sasxx,irrsxx,orsxx2,sasxx2,irsxx2,
     &                 Stime,Stmcd,trnsrs,srs,tmpsrs,Stex,Htmp,mvtmp
      INTEGER i,l0,l1,Ny,Muladd,Iagr,Kswv,ipk,ipos,Fcntyp,nchr1,nchr2,
     &        sttptr,nttl,ittl,ntype,itbl,numttl,mtspc,ltdori,ltdsa,
     &        ltdirr,lsori,lssa,lsirr,frqidx,icode,Endspn,Begxy,Nrxy,
     &        nplot,nspstr,nspst2,domfqs,domfqt,nobspc,fhnote,nsatbl,
     &        Kfulsm,istr,ldsp,l2,igrp,mtmp,nsrs,nkystr
      LOGICAL goori,gosa,goirr,Lx11,X11agr,Lseats,prtori,prtsa,prtirr,
     &        Psuadd,Lgraf,ltdfrq,nosa,Lmodel,bkori
      DIMENSION orisxx(61),sasxx(61),irrsxx(61),orsxx2(76),sasxx2(76),
     &          irsxx2(76),trnsrs(PLEN),tmpsrs(PLEN),
     &          Stex(PLEN),Stmcd(PLEN),Stime(PLEN),sttptr(0:PSTT),
     &          numttl(2),ttl(2),srs(PLEN),Begxy(2),Endspn(2),
     &          Htmp(0:PLEN),mvtmp(14)
c-----------------------------------------------------------------------
      COMMON /armalm/ Lam,Fcntyp
      COMMON /armaxy/ Endspn,Begxy,Nrxy
      COMMON /mq5a  / Stmcd,Stime
      COMMON /mq10  / Stex
c-----------------------------------------------------------------------
      LOGICAL dpeq,ispos
      INTEGER strinx
      EXTERNAL dpeq,ispos,strinx
c-----------------------------------------------------------------------
      DATA sttptr/1,12,25,49,75/
c-----------------------------------------------------------------------
c     Set logical variables for printing out spectra
c-----------------------------------------------------------------------
c      write(*,*)'  entered spcdrv'
      prtsa=(Iagr.lt.4.and.((Lseats.and.Prttab(LSPS1S)).or.
     &      (Lx11.and.Kfulsm.eq.0.and.Prttab(LSPCS1)))).or.
     &      (Iagr.eq.4.and.Prttab(LSPS1I))
      prtirr=(Iagr.lt.4.and.((Lseats.and.Prttab(LSPS2S)).or.
     &       (Lx11.and.Kfulsm.eq.0.and.Prttab(LSPCS2)))).or.
     &       (Iagr.eq.4.and.Prttab(LSPS2I).and.X11agr)
      prtori=(Iagr.lt.3.and.Prttab(LSPCS0)).or.
     &       (Iagr.eq.3.and.Prttab(LSPS0C))
c      IF(prtori)THEN
c       write(*,*)'  prtori = TRUE'
c      ELSE
c       write(*,*)'  prtori = FALSE'
c      END IF
c-----------------------------------------------------------------------
      fhnote=STDERR
      IF(Lquiet)fhnote=0
      nosa=.not.((Lx11.and.Kfulsm.eq.0).or.Lseats)
c-----------------------------------------------------------------------
c     Initialize indicator variable for spectral plot title
c-----------------------------------------------------------------------
      ittl=2
      IF(Spcdff)THEN
       IF(Spdfor.eq.NOTSET)THEN
        IF(Lmodel)THEN
         Spdfor=max(Nnsedf+Nseadf-1,1)
        ELSE
         Spdfor=1
        END IF
       END IF
      END IF
      IF(Lsumm.gt.0)THEN
       IF(Spcdff)THEN
        WRITE(Nform,1000)'diffspec: ','yes'
        WRITE(Nform,1010)'diffspecorder: ',Spdfor
        IF(Lstdff)THEN
         WRITE(Nform,1000)'diffspecstart: ','yes'
        ELSE
         WRITE(Nform,1000)'diffspecstart: ','no'
        END IF
       ELSE
        WRITE(Nform,1000)'diffspec: ','no'
       END IF
      END IF
      IF(Spdfor.eq.0)THEN
       IF(Spcdff)Spcdff=F
      ELSE
       ittl=1
      END IF
c-----------------------------------------------------------------------
c     Get relative position of starting point for spectrums
c-----------------------------------------------------------------------
c      CALL dfdate(Bgspec,Begbak,Ny,ipos)
      CALL dfdate(Bgspec,Begbk2,Ny,ipos)
      ipos=ipos+1
      IF(Lstdff)THEN
       l1=ipos
       l0=l1-Spdfor
       IF(l0.lt.Pos1ob)THEN
        ldsp=Pos1ob-l0
        l1=l1+ldsp
        CALL addate(Bgspec,Ny,ldsp,Bgspec)
        l0=Pos1ob
       END IF
      ELSE
       l0=ipos
       l1=ipos+Spdfor
      END IF
      nobspc=Posfob-l1+1
      ltdfrq=nobspc.gt.NTDLIM
c-----------------------------------------------------------------------
c     Check to see if all observations are good for logged original 
c     series
c-----------------------------------------------------------------------
      goori=Iagr.le.3
      gosa=F
      goirr=F
      IF(goori)THEN
c-----------------------------------------------------------------------
       IF(Spcsrs.ge.2)THEN
        CALL copy(Stcsi,PLEN,1,srs)
        IF(Lx11.and.(Spcsrs.eq.2))THEN
         IF(Psuadd)THEN
          DO i=Pos1ob,Posfob
           IF(Kfulsm.eq.2)THEN
            srs(i)=Stc(i)*Sti(i)
           ELSE
            srs(i)=Stc(i)*(Sts(i)+(Sti(i)-ONE))
           END IF
          END DO
         ELSE
          CALL addmul(srs,srs,Stex,Pos1bk,Posffc)
         END IF
        END IF
       ELSE
        CALL copy(Series,PLEN,1,srs)
        IF(Spcsrs.eq.1)THEN
         IF(Adjls.eq.1)CALL divsub(srs,srs,Facls,ipos,Posfob)
         IF(Adjao.eq.1)CALL divsub(srs,srs,Facao,ipos,Posfob)
         IF(Adjtc.eq.1)CALL divsub(srs,srs,Factc,ipos,Posfob)
         IF(Adjso.eq.1)CALL divsub(srs,srs,Facso,ipos,Posfob)
        END IF
       END IF
       IF(Muladd.eq.0)goori=ispos(srs,ipos,Posfob)
      END IF
      IF(goori)THEN
c-----------------------------------------------------------------------
c     Detrend original series before computing spectrum
c-----------------------------------------------------------------------
       IF(Lx11)THEN
        IF(Muladd.ne.1)ittl=ittl+2
        CALL gendff(srs,l0,Posfob,tmpsrs,l2,Muladd.ne.1,F,Spdfor)
       ELSE
        IF(dpeq(Lam,ZERO))THEN
         CALL gendff(srs,l0,Posfob,tmpsrs,l2,T,F,Spdfor)
         ittl=ittl+2
        ELSE
         CALL gendff(srs,l0,Posfob,tmpsrs,l2,F,F,Spdfor)
        END IF
       END IF
       IF(Spctyp.eq.0)THEN
c-----------------------------------------------------------------------
c     Compute the AR-spectrum for the detrended original series
c-----------------------------------------------------------------------
        CALL spgrh(tmpsrs,orsxx2,frqpk,Thtapr,l1,Posfob,nfreq,Ny,Mxarsp,
     &             Ldecbl,goori)
        CALL spgrh(tmpsrs,orisxx,frq,Thtapr,l1,Posfob,61,Ny,Mxarsp,
     &             Ldecbl,goori)
       ELSE
c-----------------------------------------------------------------------
c     Else, compute the periodogram for the detrended original series
c-----------------------------------------------------------------------
        CALL spgrh2(tmpsrs,orsxx2,frqpk,l1,Posfob,nfreq,Ldecbl)
        CALL spgrh2(tmpsrs,orisxx,frq,l1,Posfob,61,Ldecbl)
       END IF
c-----------------------------------------------------------------------
c     Save spectrum of the detrended original series
c-----------------------------------------------------------------------
       itbl=LSPCS0
       IF(Iagr.eq.4)itbl=LSPS0C
       IF((Savtab(itbl).or.Lgraf).and.goori)THEN
        CALL mksplb(itbl,spcstr,nspstr,Spcsrs,Ldecbl)
        IF(Svallf)THEN
         IF(Savtab(itbl).and.goori)
     &      CALL savspp(itbl,orsxx2,frqpk,nfreq,spcstr(1:nspstr),F)
         IF(.not.Lfatal.and.Lgraf.and.goori)
     &      CALL savspp(itbl,orsxx2,frqpk,nfreq,spcstr(1:nspstr),Lgraf)
        ELSE
         IF(Savtab(itbl).and.goori)
     &      CALL savspp(itbl,orisxx,frq,61,spcstr(1:nspstr),F)
         IF(.not.Lfatal.and.Lgraf.and.goori)
     &      CALL savspp(itbl,orisxx,frq,61,spcstr(1:nspstr),Lgraf)
        END IF
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
       END IF
c-----------------------------------------------------------------------
c     peaks for Tukey filtered spectrum from TRAMO/SEATS
c-----------------------------------------------------------------------
       IF(Iagr.eq.4)THEN
        itbl=LSPT0C
       ELSE 
        itbl=LSPTS0
       END IF
       IF(Prttab(LSPCTP).or.Savtab(LSPCTP).or.Svltab(LSLTPK).or.
     &    Savtab(itbl).or.Lgraf)THEN
        nsrs=Posfob-l1+1
        IF(nsrs.gt.80)THEN
          DO i=l1,Posfob
           srs(i-l1+1)=tmpsrs(i)
          END DO
          CALL getTPeaks(srs,nsrs,Ny,Htmp,mtmp,Pttdo,Ptso,mvtmp)
          IF(Prttab(LSPCTP).or.Savtab(LSPCTP).or.Svltab(LSLTPK))THEN
           Ntukey=Ntukey+1
           Itukey(Ntukey)=itbl
          END IF
          IF(Savtab(itbl).or.Lgraf)THEN
           CALL mkstlb(itbl,spcstr,nspstr,Spcsrs)
           IF(Savtab(itbl))
     &       CALL savstp(itbl,Htmp,mtmp,spcstr(1:nspstr),Ldecbl,F)
           IF((.not.Lfatal).and.Lgraf)
     &       CALL savstp(itbl,Htmp,mtmp,spcstr(1:nspstr),Ldecbl,Lgraf)
           IF(Lfatal)RETURN
          END IF
          IF(Lsumm.gt.0)THEN
           CALL mkspky(1,skystr,nkystr,Iagr,Lseats)
           write(Nform,1080)skystr(1:nkystr),mtmp
          END IF
        END IF
       END IF
c       goori=Iagr.le.3
c      prtori=(Iagr.lt.3.and.Prttab(LSPCS0)).or.
c     &       (Iagr.eq.3.and.Prttab(LSPS0C))
c-----------------------------------------------------------------------
c     Print out error message for spectral plot of logged original
c     series if data value is less than zero.
c-----------------------------------------------------------------------
      ELSE IF(Iagr.le.3)THEN
       CALL eWritln('Spectral plot for the logged original series '//
     &              'cannot be done for',STDERR,Mt2,T,F)
       CALL writln('       a series with values less than or equal '//
     &             'to zero.',STDERR,Mt2,F,T)
      END IF
      IF((Lx11.and.Kfulsm.eq.0).or.Lseats)THEN
c-----------------------------------------------------------------------
c     Check to see if all observations are good for logged seasonally
c     adjusted series
c-----------------------------------------------------------------------
       gosa=T
       IF(Lx11)THEN
        IF(Lrbstsa)THEN
         IF(Muladd.eq.0)THEN
          DO i=ipos,Posfob
           gosa=gosa.and.Stcime(i).gt.ZERO
          END DO
         END IF
        END IF
       ELSE
        gosa=Hvstsa
       END IF
       IF(gosa)THEN
        IF(Iagr.eq.4)THEN
         IF(X11agr)THEN
          IF(Lrbstsa)THEN
           CALL copy(Stcime,PLEN,1,srs)
           IF(Adjls.eq.1)CALL divsub(srs,srs,Facls,ipos,Posfob)
          ELSE
           CALL copy(Stci,PLEN,1,srs)
          END IF
         ELSE
          CALL copy(Stci,PLEN,1,srs)
          IF(Adjls.eq.1.and.Lrbstsa)
     &       CALL divsub(srs,srs,Facls,ipos,Posfob)
         END IF
        ELSE IF(Lx11)THEN
         IF(Lrbstsa)THEN
          CALL copy(Stcime,PLEN,1,srs)
          IF(Adjls.eq.1)CALL divsub(srs,srs,Facls,ipos,Posfob)
         ELSE
          CALL copy(Stci,PLEN,1,srs)
         END IF
        ELSE
         IF(Lrbstsa)THEN
          CALL copy(Stocsa,PLEN,1,srs)
         ELSE
          CALL copy(Seatsa,PLEN,1,srs)
         END IF
        END IF
c-----------------------------------------------------------------------
c     Detrend seasonally adjusted series before computing spectrum
c-----------------------------------------------------------------------
        CALL gendff(srs,l0,Posfob,tmpsrs,l2,Muladd.ne.1,F,Spdfor)
c-----------------------------------------------------------------------
c     Compute the AR-spectrum for the detrended seasonally adjusted
c     series
c-----------------------------------------------------------------------
        IF(Spctyp.eq.0)THEN
         CALL spgrh(tmpsrs,sasxx2,frqpk,Thtapr,l1,Posfob,nfreq,Ny,
     &              Mxarsp,Ldecbl,gosa)
         CALL spgrh(tmpsrs,sasxx,frq,Thtapr,l1,Posfob,61,Ny,Mxarsp,
     &              Ldecbl,gosa)
        ELSE
c-----------------------------------------------------------------------
c     Else, compute the periodogram for the detrended seasonally 
c     adjusted series
c-----------------------------------------------------------------------
         CALL spgrh2(tmpsrs,sasxx2,frqpk,l1,Posfob,nfreq,Ldecbl)
         CALL spgrh2(tmpsrs,sasxx,frq,l1,Posfob,61,Ldecbl)
        END IF
c-----------------------------------------------------------------------
c     Save spectrum of the detrended seasonally adjusted series
c-----------------------------------------------------------------------
        IF(Iagr.eq.4)THEN
         itbl=LSPS1I
        ELSE IF(Lseats)THEN
         itbl=LSPS1S
        ELSE 
         itbl=LSPCS1
        END IF
        IF((Savtab(itbl).or.Lgraf).and.gosa)THEN
         CALL mksplb(itbl,spcstr,nspstr,Spcsrs,Ldecbl)
         IF(Svallf)THEN
          IF(Savtab(itbl).and.gosa)
     &       CALL savspp(itbl,sasxx2,frqpk,nfreq,spcstr(1:nspstr),F)
          IF((.not.Lfatal).and.Lgraf.and.gosa)
     &       CALL savspp(itbl,sasxx2,frqpk,nfreq,spcstr(1:nspstr),Lgraf)
         ELSE
          IF(Savtab(itbl).and.gosa)
     &       CALL savspp(itbl,sasxx,frq,61,spcstr(1:nspstr),F)
          IF((.not.Lfatal).and.Lgraf.and.gosa)
     &       CALL savspp(itbl,sasxx,frq,61,spcstr(1:nspstr),Lgraf)
         END IF
         IF(Lfatal)RETURN
        END IF
c-----------------------------------------------------------------------
c     peaks for Tukey filtered spectrum from TRAMO/SEATS
c-----------------------------------------------------------------------
        IF(Iagr.eq.4)THEN
         itbl=LSPT1I
        ELSE IF(Lseats)THEN
         itbl=LSPT1S
        ELSE 
         itbl=LSPTS1
        END IF
        IF((Prttab(LSPCTP).or.Savtab(LSPCTP).or.Svltab(LSLTPK).or.
     &     Savtab(itbl).or.Lgraf).and.gosa)THEN
         nsrs=Posfob-l1+1
         IF(nsrs.gt.80)THEN
          DO i=l1,Posfob
           srs(i-l1+1)=tmpsrs(i)
          END DO
          CALL getTPeaks(srs,nsrs,Ny,Htmp,mtmp,Pttda,Ptsa,mvtmp)
          IF(Prttab(LSPCTP).or.Savtab(LSPCTP).or.Svltab(LSLTPK))THEN
           Ntukey=Ntukey+1
           Itukey(Ntukey)=itbl
          END IF
          IF(Savtab(itbl).or.Lgraf)THEN
           CALL mkstlb(itbl,spcstr,nspstr,Spcsrs)
           IF(Savtab(itbl))
     &       CALL savstp(itbl,Htmp,mtmp,spcstr(1:nspstr),Ldecbl,F)
           IF((.not.Lfatal).and.Lgraf)
     &       CALL savstp(itbl,Htmp,mtmp,spcstr(1:nspstr),Ldecbl,Lgraf)
           IF(Lfatal)RETURN
          END IF
          IF(Lsumm.gt.0)THEN
           CALL mkspky(2,skystr,nkystr,Iagr,Lseats)
           write(Nform,1080)skystr(1:nkystr),mtmp
          END IF
         END IF
        END IF
c-----------------------------------------------------------------------
c     Print out error message for spectral plot of logged original
c     series if data value is less than zero.
c-----------------------------------------------------------------------
       ELSE IF(Lx11)THEN
        CALL eWritln('Spectral plot for the logged seasonally '//
     &               'adjusted series cannot',STDERR,Mt2,T,F)
        CALL writln('       be done for a seasonal adjustment with '//
     &              'values less than or equal',STDERR,Mt2,F,F)
        CALL writln('       to zero.',STDERR,Mt2,F,T)
c-----------------------------------------------------------------------
c     Print out warning message when SEATS cannot supply a seasonal 
c     adjustment.
c-----------------------------------------------------------------------
       ELSE 
        CALL nWritln('Spectral plot for the seasonally adjusted '//
     &               'series cannot be done',fhnote,Mt2,T,F)
        CALL writln('      when SEATS cannot perform a signal '//
     &              'extraction.',fhnote,Mt2,F,T)
        RETURN
       END IF
c-----------------------------------------------------------------------
c     Compute the AR-spectrum for the irregular component
c-----------------------------------------------------------------------
       goirr=T
       IF(Lseats)goirr=Hvstir
       IF(Iagr.eq.4)goirr=goirr.and.X11agr
       IF(goirr)THEN
        DO i=ipos,Posfob
         IF(Lx11)THEN
          IF(Lrbstsa)THEN
           tmpsrs(i)=Stime(i)
          ELSE
           tmpsrs(i)=Sti(i)
          END IF          
         ELSE
          IF(Lrbstsa)THEN
           tmpsrs(i)=Stocir(i)
          ELSE
           tmpsrs(i)=Seatir(i)
          END IF
         END IF
         IF(Muladd.ne.1)tmpsrs(i)=tmpsrs(i)-1D0
        END DO
c-----------------------------------------------------------------------
        IF(Spctyp.eq.0)THEN
         CALL spgrh(tmpsrs,irsxx2,frqpk,Thtapr,ipos,Posfob,nfreq,Ny,
     &              Mxarsp,Ldecbl,goirr)
         CALL spgrh(tmpsrs,irrsxx,frq,Thtapr,ipos,Posfob,61,Ny,Mxarsp,
     &              Ldecbl,goirr)
        ELSE
c-----------------------------------------------------------------------
c     Else, compute the periodogram for the modified irregular component
c-----------------------------------------------------------------------
         CALL spgrh2(tmpsrs,irsxx2,frqpk,ipos,Posfob,nfreq,Ldecbl)
         CALL spgrh2(tmpsrs,irrsxx,frq,ipos,Posfob,61,Ldecbl)
        END IF
c-----------------------------------------------------------------------
c     Save spectrum of the modified irregular component
c-----------------------------------------------------------------------
        IF(Iagr.eq.4)THEN
         itbl=LSPS2I
        ELSE IF(Lseats)THEN
         itbl=LSPS2S
        ELSE 
         itbl=LSPCS2
        END IF
        IF((Savtab(itbl).or.Lgraf).and.goirr)THEN
         CALL mksplb(itbl,spcstr,nspstr,Spcsrs,Ldecbl)
         IF(Svallf)THEN
          IF(Savtab(itbl).and.goirr)
     &       CALL savspp(itbl,irsxx2,frqpk,nfreq,spcstr(1:nspstr),F)
          IF(.not.Lfatal.and.Lgraf.and.goirr)
     &       CALL savspp(itbl,irsxx2,frqpk,nfreq,spcstr(1:nspstr),Lgraf)
         ELSE
          IF(Savtab(itbl).and.goirr)
     &       CALL savspp(itbl,irrsxx,frq,61,spcstr(1:nspstr),F)
          IF(.not.Lfatal.and.Lgraf.and.goirr)
     &       CALL savspp(itbl,irrsxx,frq,61,spcstr(1:nspstr),Lgraf)
         END IF
         IF(Lfatal)RETURN
        END IF
c-----------------------------------------------------------------------
c     peaks for Tukey filtered spectrum from TRAMO/SEATS
c-----------------------------------------------------------------------
        IF(Iagr.eq.4)THEN
         itbl=LSPT2I
        ELSE IF(Lseats)THEN
         itbl=LSPT2S
        ELSE 
         itbl=LSPTS2
        END IF
        IF((Prttab(LSPCTP).or.Savtab(LSPCTP).or.Svltab(LSLTPK).or.
     &     Savtab(itbl).or.Lgraf).and.goirr)THEN
         nsrs=Posfob-ipos+1
         IF(nsrs.gt.80)THEN
          DO i=ipos,Posfob
           srs(i-ipos+1)=tmpsrs(i)
          END DO
          CALL getTPeaks(srs,nsrs,Ny,Htmp,mtmp,Pttdi,Ptsi,mvtmp)
          IF(Prttab(LSPCTP).or.Savtab(LSPCTP).or.Svltab(LSLTPK))THEN
           Ntukey=Ntukey+1
           Itukey(Ntukey)=itbl
          END IF
          IF(Savtab(itbl).or.Lgraf)THEN
           CALL mkstlb(itbl,spcstr,nspstr,Spcsrs)
           IF(Savtab(itbl))
     &       CALL savstp(itbl,Htmp,mtmp,spcstr(1:nspstr),Ldecbl,F)
           IF((.not.Lfatal).and.Lgraf)
     &       CALL savstp(itbl,Htmp,mtmp,spcstr(1:nspstr),Ldecbl,Lgraf)
           IF(Lfatal)RETURN
          END IF
          IF(Lsumm.gt.0)THEN
           CALL mkspky(3,skystr,nkystr,Iagr,Lseats)
           write(Nform,1080)skystr(1:nkystr),mtmp
          END IF
         END IF
        END IF
c-----------------------------------------------------------------------
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(.not.(goori.or.gosa.or.goirr))RETURN
c-----------------------------------------------------------------------
c     Determine if there are peaks in the spectral plots.
c-----------------------------------------------------------------------
      ltdsa=0
      ltdirr=0
      lssa=0
      lsirr=0
      IF((Lx11.and.Kfulsm.eq.0).or.Lseats)THEN
       IF(gosa)THEN
        CALL idpeak(sasxx,sasxx2,Spclim,Ny,tpeak,tlow,tup,ntfreq,speak,
     &              slow,sup,nsfreq,ltdsa,lssa,frqpk,Plocal,Ldecbl,
     &              ltdfrq)
        IF(Lsumm.gt.0)
     &     CALL svpeak(sasxx,sasxx2,2,Iagr,tpeak,tlow,tup,ntfreq,speak,
     &                 slow,sup,nsfreq,Lseats,Ldecbl,ltdfrq)
       END IF
       IF(goirr)THEN
        CALL idpeak(irrsxx,irsxx2,Spclim,Ny,tpeak,tlow,tup,ntfreq,speak,
     &              slow,sup,nsfreq,ltdirr,lsirr,frqpk,Plocal,Ldecbl,
     &              ltdfrq)
        IF(Lsumm.gt.0)
     &     CALL svpeak(irrsxx,irsxx2,3,Iagr,tpeak,tlow,tup,ntfreq,speak,
     &                 slow,sup,nsfreq,Lseats,Ldecbl,ltdfrq)
       END IF
      END IF
c-----------------------------------------------------------------------
c     If TD adjustment done, do not search for trading day peaks 
c     in the original series.  Search for seasonal peaks only when there
c     is no seasonal adjustment performed.
c-----------------------------------------------------------------------
      IF(prtsa)THEN
       prtsa=prtsa.and.gosa
      ELSE
       prtsa=(ltdsa.gt.0.or.lssa.gt.0).and.(.not.Lnoprt)
      END IF
      IF(prtirr)THEN
       prtirr=prtirr.and.goirr
      ELSE
       prtirr=(ltdirr.gt.0.or.lsirr.gt.0).and.(.not.Lnoprt)
      END IF
      ltdori=0
      lsori=0
      IF(.not.(goori.or.gosa.or.goirr))RETURN
      IF(goori)THEN
       CALL idpeak(orisxx,orsxx2,Spclim,Ny,tpeak,tlow,tup,ntfreq,speak,
     &             slow,sup,nsfreq,ltdori,lsori,frqpk,Plocal,Ldecbl,
     &             ltdfrq)
       IF(Lsumm.gt.0)
     &    CALL svpeak(orisxx,orsxx2,1,Iagr,tpeak,tlow,tup,ntfreq,speak,
     &                slow,sup,nsfreq,Lseats,Ldecbl,ltdfrq)
      END IF
c-----------------------------------------------------------------------
c     Get descriptor for spectral plot title
c-----------------------------------------------------------------------
      CALL getstr(STTDIC,STTPTR,PSTT,ittl,cttl,nttl)
      IF(.not.Lfatal)CALL wrtdat(Bgspec,Ny,begstr,nchr1)
      IF(.not.Lfatal)CALL wrtdat(Endspn,Ny,endstr,nchr2)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Print a warning message if a peak is found in any of the spectral
c     plots.
*c-----------------------------------------------------------------------
      IF((ltdori.gt.0.or.ltdsa.gt.0.or.ltdirr.gt.0).and.
     &   (lssa.gt.0.or.lsirr.gt.0.or.(nosa.and.lsori.gt.0)))THEN
       CALL wWritln(' Visually significant seasonal and trading day '//
     &              'peaks have ',fhnote,Mt2,T,F)
       CALL writln( 
     &   '         been found in one or more of the estimated spectra.',
     &             fhnote,Mt2,F,T)
       pkstr='trading day and seasonal'
       ipk=24
      ELSE IF((ltdori.gt.0.or.ltdsa.gt.0.or.ltdirr.gt.0).or.
     &   (lssa.gt.0.or.lsirr.gt.0.or.(nosa.and.lsori.gt.0)))THEN
       IF(ltdori.gt.0.or.ltdsa.gt.0.or.ltdirr.gt.0)THEN
        pkstr='trading day'
        ipk=11
       ELSE
        pkstr='seasonal'
        ipk=8
       END IF
       CALL wWritln('At least one visually significant '//
     &              pkstr(1:ipk)//' peak has been',fhnote,Mt2,T,F)
       CALL writln( 
     &    '         found in one or more of the estimated spectra.',
     &             fhnote,Mt2,F,T)
      END IF
c-----------------------------------------------------------------------
      mtspc=Mt1
      IF(ltdori.gt.0.or.ltdsa.gt.0.or.ltdirr.gt.0.or.lssa.gt.0.or.
     &   lsirr.gt.0.or.(nosa.and.lsori.gt.0))THEN
       IF((.not.(prtori.or.prtsa.or.prtirr)).and.Lnoprt)THEN
        mtspc=Mt2
        WRITE(STDERR,1070)
       END IF
       nplot=0
       IF(ltdori.gt.0.or.(nosa.and.lsori.gt.0))nplot=nplot+1
       IF(ltdsa.gt.0.or.lssa.gt.0)nplot=nplot+1
       IF(ltdirr.gt.0.or.lsirr.gt.0)nplot=nplot+1
       IF(nplot.eq.1)THEN
        WRITE(mtspc,1020)pkstr(1:ipk),'plot',begstr(1:nchr1)
       ELSE
        WRITE(mtspc,1020)pkstr(1:ipk),'plots',begstr(1:nchr1)
       END IF
       CALL writTagClass(mtspc,'ul','indent')
c-----------------------------------------------------------------------
c     Warning for detrended original
c-----------------------------------------------------------------------
       CALL mkspst(Spcsrs,spcstr,nspstr,nspst2,F)
       IF((.not.((Lx11.and.Kfulsm.eq.0).or.Lseats).and.lsori.gt.0).and.
     &    ltdori.gt.0)THEN
        WRITE(mtspc,1040)cttl(1:nttl)//spcstr(1:nspstr),lsori,ltdori
       ELSE IF(ltdori.gt.0)THEN
        WRITE(mtspc,1030)cttl(1:nttl)//spcstr(1:nspstr),ltdori
       ELSE IF((.not.Lx11).and.lsori.gt.0)THEN
        WRITE(mtspc,1050)cttl(1:nttl)//spcstr(1:nspstr),lsori
       END IF
c-----------------------------------------------------------------------
c     Warning for detrended seasonally adjusted series
c-----------------------------------------------------------------------
       IF(Lseats)THEN
        csatbl(1:7) = '(SEATS)'
        nsatbl=7
       ELSE IF(Iagr.le.3.or.X11agr)THEN
        IF(Lrbstsa)THEN
         csatbl(1:10) = '(Table E2)'
         nsatbl=10
        ELSE
         csatbl(1:11) = '(Table D11)'
         nsatbl=11
        END IF
       ELSE
        csatbl(1:11) = '(Table D11)'
        nsatbl=11
       END IF 
       IF(ltdsa.gt.0.and.lssa.gt.0)THEN
        WRITE(mtspc,1040)cttl(1:nttl)//' seasonally adjusted series '//
     &                   csatbl(1:nsatbl),lssa,ltdsa
       ELSE IF(ltdsa.gt.0)THEN
        WRITE(mtspc,1030)cttl(1:nttl)//' seasonally adjusted series '//
     &                   csatbl(1:nsatbl),ltdsa
       ELSE IF(lssa.gt.0)THEN
        WRITE(mtspc,1050)cttl(1:nttl)//' seasonally adjusted series '//
     &                   csatbl(1:nsatbl),lssa
       END IF
c-----------------------------------------------------------------------
c     Warning for irregular series
c-----------------------------------------------------------------------
       IF(ltdirr.gt.0.and.lsirr.gt.0)THEN
        IF(Lx11)THEN
         IF(Lrbstsa)THEN
          WRITE(mtspc,1040)'Modified irregular component (Table E3)',
     &                     lsirr,ltdirr
         ELSE
          WRITE(mtspc,1040)'Irregular component (Table D13)',
     &                     lsirr,ltdirr
         END IF
        ELSE
         IF(Lrbstsa)THEN
          WRITE(mtspc,1040)'Stochastic irregular component (SEATS)',
     &                     lsirr,ltdirr
         ELSE
          WRITE(mtspc,1040)'Irregular component (SEATS)',lsirr,ltdirr
         END IF
        END IF
       ELSE IF(ltdirr.gt.0)THEN
        IF(Lx11)THEN
         IF(Lrbstsa)THEN
          WRITE(mtspc,1040)'Modified irregular component (Table E3)',
     &                     ltdirr
         ELSE
          WRITE(mtspc,1040)'Irregular component (Table D13)',
     &                     ltdirr
         END IF
        ELSE
         IF(Lrbstsa)THEN
          WRITE(mtspc,1040)'Stochastic irregular component (SEATS)',
     &                     ltdirr
         ELSE
          WRITE(mtspc,1040)'Irregular component (SEATS)',ltdirr
         END IF
        END IF
       ELSE IF(lsirr.gt.0)THEN
        IF(Lx11)THEN
         IF(Lrbstsa)THEN
          WRITE(mtspc,1040)'Modified irregular component (Table E3)',
     &                     lsirr
         ELSE
          WRITE(mtspc,1040)'Irregular component (Table D13)',
     &                     lsirr
         END IF
        ELSE
         IF(Lrbstsa)THEN
          WRITE(mtspc,1040)'Stochastic irregular component (SEATS)',
     &                     lsirr
         ELSE
          WRITE(mtspc,1040)'Irregular component (SEATS)',lsirr
         END IF
        END IF
       END IF
       CALL writTag(mtspc,'</ul>')
      END IF
c-----------------------------------------------------------------------
c    Print warning message if no seasonal peaks is found in original
c    series and x11 spec present.
c-----------------------------------------------------------------------
      IF(((Lx11.and.Kfulsm.eq.0).or.Lseats).and.lsori.eq.0.and.
     &     goori)THEN
       IF(Lnoprt.and.mtspc.eq.Mt1)mtspc=Mt2
       CALL mkspst(Spcsrs,spcstr,nspstr,nspst2,F)
       CALL wWritln('Series should not be a candidate for seasonal '//
     &              'adjustment',fhnote,mtspc,T,F)
       CALL writln('          because the spectrum of the'//
     &             spcstr(1:nspstr),fhnote,mtspc,F,F)
       CALL writln(
     &    '          has no visually significant seasonal peaks.',
     &               fhnote,mtspc,F,T)
       IF(Iagr.lt.4)WRITE(mtspc,1060)
      END IF
c-----------------------------------------------------------------------
      IF(Lsavpk)THEN
       IF(ltdori.gt.0)THEN
        Ctpeak((Ntpeak+1):(Ntpeak+3))='ori'
        Ntpeak=Ntpeak+4
       END IF
       IF(nosa.and.lsori.gt.0)THEN
        Cspeak((Nspeak+1):(Nspeak+3))='ori'
        Nspeak=Nspeak+4
       END IF
       IF(ltdsa.gt.0)THEN
        IF(Iagr.le.3)THEN
         Ctpeak((Ntpeak+1):(Ntpeak+2))='sa'
         Ntpeak=Ntpeak+3
        ELSE
         Ctpeak((Ntpeak+1):(Ntpeak+5))='indsa'
         Ntpeak=Ntpeak+6
        END IF
       END IF
       IF(lssa.gt.0)THEN
        IF(Iagr.le.3)THEN
         Cspeak((Nspeak+1):(Nspeak+2))='sa'
         Nspeak=Nspeak+3
        ELSE
         Cspeak((Nspeak+1):(Nspeak+5))='indsa'
         Nspeak=Nspeak+6
        END IF
       END IF
       IF(ltdirr.gt.0)THEN
        IF(Iagr.le.3)THEN
         Ctpeak((Ntpeak+1):(Ntpeak+3))='irr'
         Ntpeak=Ntpeak+4
        ELSE
         Ctpeak((Ntpeak+1):(Ntpeak+6))='indirr'
         Ntpeak=Ntpeak+7
        END IF
       END IF
       IF(lsirr.gt.0)THEN
        IF(Iagr.le.3)THEN
         Cspeak((Nspeak+1):(Nspeak+3))='irr'
         Nspeak=Nspeak+4
        ELSE
         Cspeak((Nspeak+1):(Nspeak+6))='indirr'
         Nspeak=Nspeak+7
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
c     Print plots of spectrums.  If peaks are found, produce plots
c     even if they are not selected by the user.
c-----------------------------------------------------------------------
      IF(.not.(prtori.or.prtsa.or.prtirr).and.Lnoprt)RETURN
c-----------------------------------------------------------------------
c     Spectrum plot for detrended original series
c-----------------------------------------------------------------------
      icode=20
      IF(.not.ltdfrq)icode=21
      IF(Ldecbl)THEN
       IF(Spctyp.eq.0)THEN
        ctype='10*LOG(SPECTRUM)'
        ntype=16
       ELSE
        ctype='10*LOG(PERIODOGRAM)'
        ntype=19
       END IF
      ELSE
       IF(Spctyp.eq.0)THEN
        ctype='SPECTRUM'
        ntype=8
       ELSE
        ctype='PERIODOGRAM'
        ntype=11
       END IF
      END IF
      IF(goori.and.prtori)THEN
c-----------------------------------------------------------------------
       CALL mkspst(Spcsrs,spcstr,nspstr,nspst2,T)
*       IF(Lwdprt)THEN
        ttl(1)='G 0  '//ctype(1:ntype)//' of the '//cttl(1:nttl)//' '//
     &         spcstr(1:nspstr)
        numttl(1)=ntype+nttl+nspstr+14
        ttl(2)='     Spectrum estimated from '//begstr(1:nchr1)//
     &         ' to '//endstr(1:nchr2)//'.'
        numttl(2)=34+nchr1+nchr2
*       ELSE
*        ttl(1)='G 0  '//ctype(1:ntype)//' of the '//cttl(1:nttl)//
*     &         spcstr(1:nspst2) 
*        numttl(1)=ntype+nttl+nspst2+13
*        ttl(2)='    '//spcstr(nspst2+1:nspstr)//
*     &         '.  Spectrum estimated from '//begstr(1:nchr1)//' to '//
*     &         endstr(1:nchr2)//'.'
*        numttl(2)=nspstr-nspst2+nchr1+nchr2+36
*       END IF
c-----------------------------------------------------------------------
       IF(((Lx11.and.Kfulsm.eq.0).or.Lseats).and.prtori.and.prtsa.and.
     &      Axsame)THEN
        CALL grzlst(IONE,SIXONE,IZERO,orisxx,sasxx,SIXONE,ITEN,IZERO)
       ELSE
        CALL grzlst(IONE,SIXONE,IZERO,orisxx,orisxx,SIXONE,ITEN,IZERO)
       END IF
       IF(Iagr.lt.3)THEN
        CALL genSkip(LSPCS0)
       ELSE IF (Iagr.eq.3)THEN
        CALL genSkip(LSPS0C)
       END IF
       CALL chrt(ttl,numttl,icode,1,120/Ny)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
      IF(nosa)RETURN
c-----------------------------------------------------------------------
c     Spectrum plot for detrended seasonally adjusted series
c-----------------------------------------------------------------------
      IF(prtsa)THEN
*       IF(Lpage)THEN
*        WRITE(Mt1,Ttlfmt)Newpg,Title(1:Ntitle),Kpage,Serno(1:Nser)
*        Kpage=Kpage+1
*       END IF
*       IF(Lwdprt)THEN
        IF(Lseats)THEN
         ttl(1)='G 1  '//ctype(1:ntype)//' of the '//cttl(1:nttl)//
     &          ' Seasonally Adjusted Data (SEATS).'
         numttl(1)=ntype+nttl+47
        ELSE IF(Iagr.le.3)THEN
         IF(Lrbstsa)THEN
          ttl(1)='G 1  '//ctype(1:ntype)//' of the '//cttl(1:nttl)//
     &           ' Seasonally Adjusted Data (Table E2).'
          numttl(1)=ntype+nttl+50
         ELSE
          ttl(1)='G 1  '//ctype(1:ntype)//' of the '//cttl(1:nttl)//
     &           ' Seasonally Adjusted Data (Table D11).'
          numttl(1)=ntype+nttl+51
         END IF
        ELSE IF(X11agr)THEN
         IF(Lrbstsa)THEN
          ttl(1)='G 1  '//ctype(1:ntype)//' of the '//cttl(1:nttl)//
     &           ' Seasonally Adjusted Data (Table E2).'
          numttl(1)=ntype+nttl+50
         ELSE
          ttl(1)='G 1  '//ctype(1:ntype)//' of the '//cttl(1:nttl)//
     &           ' Seasonally Adjusted Data (Table D11).'
          numttl(1)=ntype+nttl+51
         END IF
        ELSE
         ttl(1)='G 1  '//ctype(1:ntype)//' of the '//cttl(1:nttl)//
     &          ' indirect seasonally adjusted data (Table D11).'
         numttl(1)=ntype+nttl+60
        END IF
        ttl(2)='     Spectrum estimated from '//begstr(1:nchr1)//
     &         ' to '//endstr(1:nchr2)//'.'
        numttl(2)=34+nchr1+nchr2
*       ELSE
*        IF(Iagr.le.3)THEN
*         ttl(1)='G 1  '//ctype(1:ntype)//' of the '//cttl(1:nttl)//
*     &          ' seasonally adjusted'
*         numttl(1)=ntype+nttl+34
*        ELSE
*         ttl(1)='G.1  '//ctype(1:ntype)//' of the '//cttl(1:nttl)//
*     &          ' indirect seasonally adjusted'
*         numttl(1)=ntype+nttl+43
*        END IF
*        IF(Lseats)THEN
*         ttl(2)='     data (SEATS).  Spectrum estimated from '//
*     &          begstr(1:nchr1)//' to '//endstr(1:nchr2)//'.'
*         numttl(2)=49+nchr1+nchr2
*        ELSE
*         ttl(2)='     data (Table E2).  Spectrum estimated from '//
*     &          begstr(1:nchr1)//' to '//endstr(1:nchr2)//'.'
*         numttl(2)=52+nchr1+nchr2
*        END IF
*       END IF
       IF(goori.and.prtori.and.prtsa.and.Axsame)THEN
        CALL grzlst(IONE,SIXONE,IZERO,sasxx,orisxx,SIXONE,ITEN,IZERO)
       ELSE
        CALL grzlst(IONE,SIXONE,IZERO,sasxx,sasxx,SIXONE,ITEN,IZERO)
       END IF
       IF(Lseats)THEN
        CALL genSkip(LSPS1S)
       ELSE IF(Iagr.le.3)THEN
        CALL genSkip(LSPCS1)
       ELSE
        CALL genSkip(LSPS1I)
       END IF
       CALL chrt(ttl,numttl,icode,1,120/Ny)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c     Spectrum plot for modified irregular series
c-----------------------------------------------------------------------
      IF(prtirr)THEN
*       IF(Lpage)THEN
*        WRITE(Mt1,Ttlfmt)Newpg,Title(1:Ntitle),Kpage,Serno(1:Nser)
*        Kpage=Kpage+1
*       END IF
       IF(Lseats)THEN
        IF(Lrbstsa)THEN
         ttl(1)='G 2  '//ctype(1:ntype)//
     &          ' of the Stochastic Irregular (SEATS).'
         numttl(1)=ntype+42
        ELSE
         ttl(1)='G 2  '//ctype(1:ntype)//
     &          ' of the Irregular (SEATS).'
         numttl(1)=ntype+31
        END IF
       ELSE IF(Iagr.le.3)THEN
        IF(Lrbstsa)THEN
         ttl(1)='G 2  '//ctype(1:ntype)//
     &          ' of the Modified Irregular (Table E3).'
         numttl(1)=ntype+45
        ELSE
         ttl(1)='G 2  '//ctype(1:ntype)//
     &          ' of the Irregular (Table D13).'
         numttl(1)=ntype+37
        END IF
       ELSE
        IF(Lrbstsa)THEN
         ttl(1)='G 2  '//ctype(1:ntype)//
     &          ' of the Indirect Modified Irregular (Table E3).'
         numttl(1)=ntype+54
        ELSE
         ttl(1)='G 2  '//ctype(1:ntype)//
     &          ' of the Indirect Irregular (Table D13).'
         numttl(1)=ntype+46
        END IF
       END IF
       ttl(2)='     Spectrum estimated from '//begstr(1:nchr1)//
     &        ' to '//endstr(1:nchr2)//'.'
       numttl(2)=34+nchr1+nchr2
       CALL grzlst(IONE,SIXONE,IZERO,irrsxx,irrsxx,SIXONE,ITEN,IZERO)
       IF(Lseats)THEN
        CALL genSkip(LSPS2S)
       ELSE IF(Iagr.le.3)THEN
        CALL genSkip(LSPCS2)
       ELSE
        CALL genSkip(LSPS2I)
       END IF
       CALL chrt(ttl,numttl,icode,1,120/Ny)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
      RETURN
c-----------------------------------------------------------------------
 1000 FORMAT(a,a)
 1010 FORMAT(a,i4)
 1020 FORMAT(/,'<p>Visually significant residual ',a,' peaks have been',
     &        /,'  found in the spectral ',a,' of the following series',
     &          ' starting in ',a,':</p>',/)
 1030 FORMAT('<li>',a,' (',i1,' Trading Day peak(s))</li>')
 1040 FORMAT('<li>',a,' (',i1,' Seasonal and ',i1,
     &       ' Trading Day peaks)</li>')
 1050 FORMAT('<li>',a,' (',i1,' Seasonal peak(s))</li>')
 1060 FORMAT(/,' <p>If this is a component series of an ',
     &         'indirectly adjusted composite series,',/,
     &         ' consider using type = trend or',
     &         ' type = summary in the x11 spec.</p>')
 1070 FORMAT(/,'          Rerun the input file without the output ',
     &         'suppression option',/,
     &         '          (-n flag) for more details.')
 1080 FORMAT(a,'.tukey.m: ',i5)
c-----------------------------------------------------------------------
      END
