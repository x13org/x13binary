c     Last Change: 10,2021,pass additional paramter to seatpr, x11pt3,
c     x11pt4 because there is new argument trendtc in regression
c     add LOGICAL Lrvarma,Lrvtdrg,Rvtrfc to avid some variables to be
c     ignored it is the same as rev.cmn- Jan. 2021
C     previous change:  BCM  23 Mar 2005    3:41 pm
      SUBROUTINE x11ari(Lmodel,Lx11,X11agr,Lseats,Lcomp,Issap,Irev,
     &                  Irevsa,Ixreg,Lsumm,Ltimer,Lgraf)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- THIS SUBROUTINE PARTITIONS X-11 AND ARIMA ROUTINES TO
C --- IMPROVE THE OVERLAY STRUCTURE.
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'model.prm'
      INCLUDE 'arima.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'agr.cmn'
      INCLUDE 'lzero.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11log.cmn'
      INCLUDE 'x11msc.cmn'
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'mdltbl.i'
      INCLUDE 'spctbl.i'
      INCLUDE 'spcsvl.i'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'priusr.cmn'
      INCLUDE 'rho.cmn'
      INCLUDE 'tukey.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'htmlout.cmn'
      INCLUDE 'nsums.i'
c-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c-----------------------------------------------------------------------
      CHARACTER errext*180,blank*180,outd*180,cstuk*35,cttuk*35,
     &          cstk90*35,cttk90*35,cstuki*35,cttuki*35,csti90*35,
     &          ctti90*35
      LOGICAL Lmodel,Lx11,havmdl,Lrvsa,Lrvsf,Lrvch,Lrvaic,Lrvfct,Lcomp,
     &        Lrvtrn,Lrfrsh,Revfix,Revfxx,extok,Lrvtch,Rvtran,Cnctar,
     &        Lseats,lfm,Lgraf,Rvxotl,X11agr,lsadj,Lsvtpk,Ltimer,lpkhdr
      INTEGER ierr,nf2,ncur,icur,nspdir,ntpdir,Issap,Irev,Ixreg,Lsumm,
     &        Irevsa,ixrbak,kswbak,ntky
      LOGICAL Lrvarma,Lrvtdrg,Rvtrfc
c-----------------------------------------------------------------------
      INTEGER nblank,lstpth
      EXTERNAL nblank,lstpth
c-----------------------------------------------------------------------
      REAL ticks
c-----------------------------------------------------------------------
      COMMON /revlog/ Lrvsa,Lrvsf,Lrvch,Lrvtrn,Lrvtch,Lrvaic,Lrvfct,
     &                Lrvarma,Lrvtdrg,Lrfrsh,Revfix,Revfxx,Rvtran,
     &                Cnctar,Rvxotl,Rvtrfc
c-----------------------------------------------------------------------
      Lsavpk=((Iagr.lt.3.and.Svltab(LSLSPK)).or.(Iagr.eq.3.and.
     &       (Svltab(LSLSPK).or.Svltab(LSLDSP).or.Svltab(LSLISP))).OR.
     &       (Lsumm.gt.0)).and.Ny.eq.12
      Lsvtpk=((Iagr.lt.3.and.Svltab(LSLTPK)).or.(Iagr.eq.3.and.
     &       (Svltab(LSLTPK).or.Svltab(LSLDTP).or.Svltab(LSLITP))).OR.
     &       (Lsumm.gt.0)).and.Ny.eq.12
      lpkhdr=((Iagr.lt.3.and.(Svltab(LSLSPK).or.Svltab(LSLTPK))).or.
     &        (Iagr.eq.3.and.
     &        (Svltab(LSLSPK).or.Svltab(LSLDSP).or.Svltab(LSLISP).or.
     &         Svltab(LSLTPK).or.Svltab(LSLDTP).or.Svltab(LSLITP))))
     &        .and.Ny.eq.12
*      IF(Lsvtpk)THEN
*       CALL setchr(' ',35,cstuk)
*       CALL setchr(' ',35,cttuk)
*       CALL setchr(' ',35,cstk90)
*       CALL setchr(' ',35,cttk90)
*       CALL setchr(' ',35,cstuki)
*       CALL setchr(' ',35,cttuki)
*       CALL setchr(' ',35,csti90)
*       CALL setchr(' ',35,ctti90)
*      END IF
      nspdir=0
      ntpdir=0
      havmdl=F
      IF((.not.(Lautom.or.Lautox)).and.Lmodel)havmdl=T
      lsadj=Lx11.or.Lseats
c-----------------------------------------------------------------------
c     perform automatic transformation test 
c     (BCM, July 1997, moved here Nov. 1999)
c-----------------------------------------------------------------------
      IF(Fcntyp.eq.0)THEN
       Nspobs=Nofpob-Nfcst
       lfm=Prttab(LTRAIC).and.Prttab(LESTFM)
       CALL trnaic(Lx11,Lmodel,Prttab(LTRAIC),lfm)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
C --- PRIOR-SIMULTANEOUS TRADING DAY AND HOLIDAY ADJUSTMENTS.
c-----------------------------------------------------------------------
      IF(Lx11)THEN
       Axhol=F
       IF(Issap.lt.2.or.Irev.lt.4)Kh2=Khol
       IF(Ixreg.eq.2.or.Khol.eq.1)THEN
        CALL xrgdrv(Lmodel,Lx11,Kh2,Lgraf)
        IF(Lfatal)RETURN
       END IF
      END IF
c-----------------------------------------------------------------------
C --- PRIOR ADJUSTMENTS.
c-----------------------------------------------------------------------
      CALL x11pt1(Lmodel,Lgraf,Lgraf)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c --- REG-ARIMA MODELLING.
c-----------------------------------------------------------------------
      IF(Lmodel)THEN
       IF(Same)THEN
        CALL writln('The program will not estimate a regARIMA model '//
     &              'for a constant series.',STDERR,Mt2,T,T)
        IF(Adjtd.eq.1)Adjtd=0
        IF(Adjhol.eq.1)Adjhol=0
        IF(Adjao.eq.1)Adjao=0
        IF(Adjls.eq.1)Adjls=0
        IF(Adjtc.eq.1)Adjtc=0
        IF(Adjso.eq.1)Adjso=0
        IF(Adjusr.eq.1)Adjusr=0
        IF(Adjsea.eq.1)Adjsea=0
        IF((.NOT.(Axrghl.or.Axruhl.or.Khol.ge.1)).and.Finhol)Finhol=F
        IF(Finao)Finao=F
        IF(Finls)Finls=F
        IF(Fintc)Fintc=F
        IF(Finusr)Finusr=F
       ELSE
        extok=T
        IF(Ltimer)THEN
         CALL cpu_time(ticks)
         IF(Issap.lt.2.and.Irev.lt.4)THEN
          WRITE(Nform,9000) 'barima:',ticks
         ELSE
          WRITE(Nform,9000) 'barima.diag:',ticks
         END IF
        END IF
        CALL arima(havmdl,extok,Lx11,Lseats,Lgraf)
        IF(Ltimer)THEN
         IF(Lfatal)RETURN
         CALL cpu_time(ticks)
         IF(Issap.lt.2.and.Irev.lt.4)THEN
          WRITE(Nform,9000) 'earima:',ticks
         ELSE
          WRITE(Nform,9000) 'earima.diag:',ticks
         END IF
        END IF
       END IF
       IF(Lfatal)RETURN
       IF((Same.or.(.not.havmdl).or.(.not.extok)).and.Lmodel)THEN
        nf2=Nfcst
        Nfcst=0
        Nbcst=0
        IF(Nfdrp.gt.0)Nfdrp=0
        CALL setxpt(nf2,lsadj,Fctdrp)
        IF(Nuspad.gt.0)THEN
         CALL dfdate(Begspn,Bgupad,Ny,Frstap)
         Frstap=Frstap+1
        END IF
        IF(Nustad.gt.0)THEN
         CALL dfdate(Begspn,Bgutad,Ny,Frstat)
         Frstat=Frstat+1
        END IF
       END IF
       IF(.not.havmdl.and.Lmodel)Lmodel=F
*       IF(.NOT.(Ny.eq.36.or.Ny.eq.24.or.Ny.eq.12.or.Ny.eq.4))RETURN
       IF(Lx11)THEN
        IF(.NOT.(Ny.eq.12.or.Ny.eq.4))THEN
         CALL writln('NOTE: The program will only generate an X-11 '//
     &               'seasonal adjustment for ',STDERR,Mt2,T,F)
         CALL writln('      monthly or quarterly series.',
     &               STDERR,Mt2,F,T)
         RETURN
        END IF
       ELSE IF(Lseats)THEN
        IF(.NOT.(Ny.eq.12.or.Ny.eq.6.or.Ny.eq.4.or.Ny.eq.2.or.
     &           Ny.eq.1))THEN
         CALL writln('NOTE: The program will only generate a SEATS '//
     &               'adjustment for ',STDERR,Mt2,T,F)
         CALL writln('      monthly, bimonthly, quarterly, biannual '//
     &               'or annual.',STDERR,Mt2,F,T)
         RETURN
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
c     If this is a revisions run and no seasonal adjustment diagnostics
c     are being analyzed, exit routine.
c-----------------------------------------------------------------------
      IF((.not.(Lrvsa.or.Lrvsf.or.Lrvch.or.Lrvtrn).or.(.not.lsadj)).and.
     &    Irev.eq.4)RETURN
c-----------------------------------------------------------------------
c --- X-11 Seasonal Adjustment
c-----------------------------------------------------------------------
C --- X-11 PARTS B1 TO D7.
c-----------------------------------------------------------------------
      IF(Ltimer.and.Lx11)THEN
       CALL cpu_time(ticks)
       IF(Issap.lt.2.and.Irev.lt.4)THEN
        WRITE(Nform,9000) 'bx11:',ticks
       ELSE
        WRITE(Nform,9000) 'bx11.diag:',ticks
       END IF
      END IF
      IF((.not.Lcmpaq).or.Lx11)
     &    CALL x11pt2(Lmodel,Lx11,Lseats,Lgraf,Lgraf)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c    Seats seasonal adjustment
c-----------------------------------------------------------------------
      IF(Lseats)THEN
       CALL chksmd(3)
       IF(.not.Lfatal)CALL initdg(Lsumm,Irev,Issap,Muladd)
       IF(Lfatal)RETURN
       CALL setchr(' ',180,blank)
       CALL setchr(' ',180,outd)
       ncur=nblank(Cursrs)
       icur=lstpth(Cursrs,ncur)
       IF(icur.gt.0)outd(1:icur)=Cursrs(1:icur)
       IF(Ltimer)THEN
        CALL cpu_time(ticks)
        IF(Issap.lt.2.and.Irev.lt.4)THEN
         WRITE(Nform,9000) 'bseats:',ticks
        ELSE
         WRITE(Nform,9000) 'bseats.diag:',ticks
        END IF
       END IF
*       call profiler(1,'entering SEATS')
       nSeatsSer=nSeatsSer+1
       CALL seats(blank,blank,outd,blank,0,0,ierr,errext,Lgraf)
       IF(Lfatal.and.ierr.le.0)RETURN
       IF(ierr.gt.0)THEN
        Lfatal=T
        CALL writln(errext,STDERR,Mt2,T,T)
        CALL abend()
        RETURN
       ELSE
        CALL seatad(Muladd,Ny,Nfcst)
        CALL seatfc(Ny,Iagr)
        CALL seatdg(Issap,Irev,Irevsa,Ny,Iag,Iagr,Muladd,Lsumm,Lseats,
     &              Lgraf,Lam,Nfcst,Length)
        CALL seatpr(Begspn,Endspn,Ny,Muladd,Kpart,Kdec,Lsumm,Lgraf,Lam,
     &              Lttc)
       END IF
       IF(Ltimer)THEN
        CALL cpu_time(ticks)
        IF(Issap.lt.2.and.Irev.lt.4)THEN
         WRITE(Nform,9000) 'eseats:',ticks
        ELSE
         WRITE(Nform,9000) 'eseats.diag:',ticks
        END IF
       END IF
       IF(Lfatal.or.ABS(Issap).eq.2.or.ABS(Irev).eq.4)RETURN
      ELSE IF(Lx11)THEN
c-----------------------------------------------------------------------
C --- X-11 PARTS D8 TO D16.
c-----------------------------------------------------------------------
       CALL x11pt3(Lgraf,Lttc)
       IF(Lfatal.or.Issap.eq.2.or.Irev.eq.4)RETURN
c-----------------------------------------------------------------------
C --- X-11 PARTS E1 TO F4.
c-----------------------------------------------------------------------
       CALL x11pt4(Lgraf,Lttc)
       IF(Lfatal)RETURN
       IF(Ltimer)THEN
        CALL cpu_time(ticks)
        IF(Issap.lt.2.and.Irev.lt.4)THEN
         WRITE(Nform,9000) 'ex11:',ticks
        ELSE
         WRITE(Nform,9000) 'ex11.diag:',ticks
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
c     Produce spectral plots
c-----------------------------------------------------------------------
      IF(.not.Same)THEN
       IF(Prttab(LSPCQS).or.Savtab(LSPCQS).or.Svltab(LSLQS).or.
     &    Svltab(LSLDQS))THEN
        CALL genqs(Lmodel,Lseats,Lx11,X11agr,Psuadd,Muladd,Kfulsm,Iagr,
     &             Ny,LSPCQS,Svltab(LSLQS).or.Svltab(LSLDQS),T)
       END IF
       IF(Ny.eq.12)THEN
        IF(Ltimer)THEN
         CALL cpu_time(ticks)
         WRITE(Nform,9000) 'bspectrum:',ticks
        END IF
        CALL spcdrv(Muladd,Iagr,Kswv,Ny,Lx11,Kfulsm,X11agr,Lseats,
     &              Psuadd,Lgraf,Lmodel)
        IF(Lfatal)RETURN
        IF(Nspeak.gt.0)nspdir=Nspeak-1
        IF(Ntpeak.gt.0)ntpdir=Ntpeak-1
c-----------------------------------------------------------------------
        ntky=Ntukey
        IF(Ntukey.gt.0)THEN
         IF(Prttab(LSPCTP))CALL prtukp(Mt1,Iagr,Ny,Inspc,F)
         IF(Svltab(LSLTPK).or.Svltab(LSLDTP))
     &      CALL prtukp(Ng,Iagr,Ny,Inlgfl,T)
         IF(Lsumm.gt.0.or.(Svltab(LSLTPK).or.Svltab(LSLDTP)))
     &      CALL svtukp(Iagr,Lsumm,cstuk,cttuk,cstk90,cttk90,lsadj)
c-----------------------------------------------------------------------
         IF(Prttab(LSPCTP).or.(Svltab(LSLTPK).or.Svltab(LSLDTP)).or.
     &      Lsumm.gt.0)THEN
          IF(Iagr.eq.3)THEN
            Ntukey=0
            CALL setint(NOTSET,4,Itukey)
          END IF
         END IF
        END IF
c-----------------------------------------------------------------------
c    Perform tests for quarterly seasonality in monthly series
c-----------------------------------------------------------------------
        IF(Lqchk)THEN
         CALL chqsea(Lmodel,Lseats,Lx11,Prttab(LSPCQC),Svltab(LSLQCH),
     &               Lsumm,LSPCQC)
        END IF
c-----------------------------------------------------------------------
        IF(Ltimer)THEN
         CALL cpu_time(ticks)
         WRITE(Nform,9000) 'espectrum:',ticks
        END IF
       END IF
c-----------------------------------------------------------------------
       IF(Prttab(LSPNPA).or.Savtab(LSPNPA).or.Svltab(LSLNPA).or.
     &    Svltab(LSLDNP))THEN
        CALL gennpsa(Lmodel,Lseats,Lx11,X11agr,Muladd,Kfulsm,Iagr,Ny,
     &               LSPNPA,Svltab(LSLNPA).or.Svltab(LSLDNP))
       END IF
c-----------------------------------------------------------------------
      END IF
      IF((.not.Lcomp).and.Iagr.gt.0)THEN
c-----------------------------------------------------------------------
C --- INDIRECT AGGREGATE SEASONAL ADJUSTMENT.
c-----------------------------------------------------------------------
       IF(Iagr.eq.3)THEN
        ixrbak=Ixreg
        kswbak=Kswv
        Ixreg=0
        Kswv=0
        IF(X11agr)THEN
         CALL agr3(Lgraf,Begspn,Lx11)
         IF(.not.Lfatal)CALL x11pt4(Lgraf,Lttc)
        ELSE
         CALL agr3s(Lgraf,Begspn,Lx11)
        END IF
        IF(.not.Lfatal)THEN
         IF(Prttab(LSPQSI).or.Savtab(LSPQSI).or.Svltab(LSLQS).or.
     &      Svltab(LSLIQS))THEN
          CALL genqs(Lmodel,Lseats,Lx11,X11agr,Psuadd,Muladd,Kfulsm,
     &               Iagr,Ny,LSPQSI,Svltab(LSLQS).or.Svltab(LSLIQS),
     &               .not.(Svltab(LSLQS).or.Svltab(LSLDQS)))
         END IF
         IF(Ny.eq.12)THEN
          CALL spcdrv(Muladd,Iagr,Kswv,Ny,Lx11,Kfulsm,X11agr,Lseats,
     &                Psuadd,Lgraf,Lmodel)
          IF(Ntukey.gt.0)THEN
           ntky=ntky+Ntukey
           IF(Prttab(LSPCTP))CALL prtukp(Mt1,Iagr,Ny,Inspc,F)
           IF(Svltab(LSLTPK).or.Svltab(LSLITP))
     &        CALL prtukp(Ng,Iagr,Ny,Inlgfl,T)
           IF(Lsumm.gt.0.or.(Svltab(LSLTPK).or.Svltab(LSLITP)))
     &        CALL svtukp(Iagr,Lsumm,cstuki,cttuki,csti90,ctti90,lsadj)
          END IF
         END IF
         IF(Prttab(LSPNPI).or.Savtab(LSPNPI).or.Svltab(LSLNPA).or.
     &      Svltab(LSLINP))THEN
          CALL gennpsa(Lmodel,Lseats,Lx11,X11agr,Muladd,Kfulsm,Iagr,Ny,
     &                 LSPNPI,Svltab(LSLNPA).or.Svltab(LSLINP))
         END IF
c-----------------------------------------------------------------------
        END IF
        IF(Lfatal)RETURN
        Ixreg=ixrbak
        Kswv= kswbak
       END IF
       IF(Iag.ge.0.or.Iagr.eq.4)
     &    CALL agr2(Issap,Irev,Lsavpk,Begspn,Lx11,X11agr)
      END IF
c-----------------------------------------------------------------------
      IF(Lsavpk.or.Lsvtpk)THEN
       IF(lpkhdr)
     &    CALL writTagOneLine(Ng,'h3','@','Spectral Peak Summaries')
       IF(Lsavpk)CALL savpk(Iagr,Lsumm,nspdir,ntpdir)
       IF(Lsvtpk.and.ntky.gt.0)THEN
        CALL savtpk(Iagr,Lsumm,cstuk,cttuk,cstk90,cttk90,cstuki,
     &              cttuki,csti90,ctti90)
       END IF
       IF(Issap.eq.0.and.Irev.eq.0.and.Iagr.gt.3)Iagr=0
      END IF
c-----------------------------------------------------------------------
 9000 FORMAT(a,e15.8)
      RETURN
      END
