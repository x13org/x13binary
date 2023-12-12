C     Last change:  BCM   9 Dec 1998    4:30 pm
      SUBROUTINE spcrsd(A,Na,Begrsd,Sp,Endspn,Tblptr,Lseats,Lsumm,Lgraf)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Routine which computes the spectrum for the regARIMA model
c     residuals.  If there are peaks detected at the trading day or
c     seasonal frequencies, these are noted.
c-----------------------------------------------------------------------
c     AR-Spectrum routines originally appeared in the BAYSEA program,
c     developed by H. Akaike and G. Kitagawa of the Institute for
c     Statistical Mathematics.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'tbltitle.prm'
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'rho.cmn'
      INCLUDE 'tukey.cmn'
      INCLUDE 'spcidx.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'spctbl.i'
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'spcsvl.i'
c-----------------------------------------------------------------------
      INTEGER IONE,SIXONE,IZERO,ITEN,PA,NTDLIM
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.,IONE=1,SIXONE=61,IZERO=0,ITEN=10,
     &          NTDLIM=60,PA=PLEN+2*PORDER)
c-----------------------------------------------------------------------
      CHARACTER begstr*(10),ctype*(19),endstr*(10),ttl*(PTTLEN),
     &          pkstr*(24),slbl*(36),arstr*(2),skystr*(10)
      DOUBLE PRECISION A,Temp,rsdsxx,rssxx2,tmpsxx,pklim,star1,Hrsd,
     &                 mvrsd
      INTEGER Begrsd,Endspn,frqidx,i,ipos,Na,ltdrsd,lsrsd,nchr1,nchr2,
     &        ntype,icode,numttl,Sp,ipk,Tblptr,Lsumm,domfqt,domfqs,ns,
     &        fhnote,nobspc,istr,mrsd,ntmp,nkystr
      LOGICAL gorsd,Lgraf,ltdfrq,Lseats
      DIMENSION A(*),Begrsd(2),Temp(PA),rsdsxx(61),rssxx2(76),mvrsd(14),
     &          tmpsxx(61),Endspn(2),ttl(2),numttl(2),Hrsd(0:PLEN)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      fhnote=STDERR
      IF(Lquiet)fhnote=0
c-----------------------------------------------------------------------
      IF(.not.(Sp.eq.12.or.Sp.eq.4))THEN
       CALL eWritln('Spectral plots currently can only be generated '//
     &              'for monthly or',STDERR,Mt2,T,F)
       CALL writln('       quarterly time series.',STDERR,Mt2,F,T)
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Get relative position of starting point for spectrums
c-----------------------------------------------------------------------
      CALL dfdate(Bgspec,Begrsd,Sp,ipos)
      IF(ipos.lt.0)THEN
       ipos=1
       CALL wrtdat(Begrsd,Sp,begstr,nchr1)
      ELSE
       ipos=ipos+1
       CALL wrtdat(Bgspec,Sp,begstr,nchr1)
      END IF
      IF(.not.Lfatal)CALL wrtdat(Endspn,Sp,endstr,nchr2)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
      gorsd=T
      nobspc=Na-ipos+1
      ltdfrq=nobspc.gt.NTDLIM
c-----------------------------------------------------------------------
      CALL copy(A,Na,-1,Temp)
      IF(Spctyp.eq.0)THEN
       CALL spgrh(Temp,rssxx2,frqpk,Thtapr,ipos,Na,nfreq,Sp,Mxarsp,
     &            Ldecbl,gorsd)
       CALL spgrh(Temp,rsdsxx,frq,Thtapr,ipos,Na,61,Sp,Mxarsp,
     &            Ldecbl,gorsd)
      ELSE
c-----------------------------------------------------------------------
c     Else, compute the periodogram for the regARIMA model residuals
c-----------------------------------------------------------------------
       CALL spgrh2(Temp,rssxx2,frqpk,ipos,Na,nfreq,Ldecbl)
       CALL spgrh2(Temp,rsdsxx,frq,ipos,Na,61,Ldecbl)
      END IF
c-----------------------------------------------------------------------
c     Save spectrum of the regARIMA model residuals
c-----------------------------------------------------------------------
      IF(.not.gorsd)RETURN
c-----------------------------------------------------------------------
      IF(Savtab(Tblptr).or.Lgraf)THEN
       CALL mksplb(Tblptr,slbl,ns,0,Ldecbl)
       IF(Svallf)THEN
        IF(Savtab(Tblptr))
     &     CALL savspp(Tblptr,rssxx2,frqpk,nfreq,slbl(1:ns),F)
        IF(.not.Lfatal.and.Lgraf)
     &     CALL savspp(Tblptr,rssxx2,frqpk,nfreq,slbl(1:ns),Lgraf)
       ELSE
        IF(Savtab(Tblptr))
     &     CALL savspp(Tblptr,rsdsxx,frq,61,slbl(1:ns),F)
        IF(.not.Lfatal.and.Lgraf)
     &     CALL savspp(Tblptr,rsdsxx,frq,61,slbl(1:ns),Lgraf)
       END IF
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
      IF((Prttab(LSPCTP).or.Savtab(LSPCTP).or.Svltab(LSLTPK).or.
     &    Lsumm.gt.0.or.Savtab(Tblptr).or.Lgraf).AND.(Sp.eq.12))THEN
       ntmp=na-ipos+1
       IF(ntmp.ge.80)THEN
        IF(ipos.gt.1)THEN
         DO i=ipos,Na
          Temp(i-ipos+1)=a(i)
         END DO
        END IF
        CALL getTPeaks(a,ntmp,Sp,Hrsd,mrsd,Pttdr,Ptsr,mvrsd)
        IF(Prttab(LSPCTP).or.Savtab(LSPCTP).or.Svltab(LSLTPK).or.
     &    Lsumm.gt.0)THEN
         Ntukey=Ntukey+1
         Itukey(Ntukey)=Tblptr
        END IF
        IF(Savtab(Tblptr).or.Lgraf)THEN
         CALL mkstlb(Tblptr,slbl,ns,Spcsrs)
         IF(Savtab(Tblptr))
     &      CALL savstp(LSPTRS,Hrsd,mrsd,slbl(1:ns),Ldecbl,F)
         IF((.not.Lfatal).and.Lgraf)
     &      CALL savstp(LSPTRS,Hrsd,mrsd,slbl(1:ns),Ldecbl,Lgraf)
         IF(Lfatal)RETURN
        END IF
        IF(Lsumm.gt.0)THEN
         CALL mkspky(4,skystr,nkystr,0,Lseats)
         write(Nform,1080)skystr(1:nkystr),mrsd
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
      ltdrsd=0
      lsrsd=0
      CALL idpeak(rsdsxx,rssxx2,Spclim,Sp,tpeak,tlow,tup,ntfreq,speak,
     &            slow,sup,nsfreq,ltdrsd,lsrsd,frqpk,Plocal,Ldecbl,
     &            ltdfrq)
      IF(Lsumm.gt.0)
     &   CALL svpeak(rsdsxx,rssxx2,0,0,tpeak,tlow,tup,ntfreq,speak,
     &               slow,sup,nsfreq,Lseats,Ldecbl,ltdfrq)
c-----------------------------------------------------------------------
      IF(ltdrsd.gt.0.and.lsrsd.gt.0.and.Prttab(Tblptr))THEN
       CALL wWritln('Visually significant seasonal and trading day '//
     &              'peaks have ',fhnote,Mt2,T,F)
       IF(Lseats)THEN
        CALL writln('         been found in the estimated spectrum '//
     &              'of the SEATS extended residuals.',fhnote,Mt2,F,T)
       ELSE
        CALL writln('         been found in the estimated spectrum '//
     &              'of the regARIMA residuals.',fhnote,Mt2,F,T)
       END IF
       pkstr='trading day and seasonal'
       ipk=24
      ELSE IF(ltdrsd.gt.0.and.Prttab(Tblptr))THEN
       CALL wWritln('At least one visually significant trading day '//
     &              'peak has been',fhnote,Mt2,T,F)
       IF(Lseats)THEN
        CALL writln('         found in the estimated spectrum of '//
     &              'the SEATS extended residuals.',fhnote,Mt2,F,T)
       ELSE
        CALL writln('         found in the estimated spectrum of '//
     &              'the regARIMA residuals.',fhnote,Mt2,F,T)
       END IF
       pkstr='trading day'
       ipk=11
      ELSE IF(lsrsd.gt.0.and.Prttab(Tblptr))THEN
       CALL wWritln('At least one visually significant seasonal peak '//
     &              'has been found',fhnote,Mt2,T,F)
       IF(Lseats)THEN
        CALL writln('         in the estimated spectrum of the SEATS'//
     &              ' extended residuals.',fhnote,Mt2,F,T)
       ELSE
        CALL writln('         in the estimated spectrum of the '//
     &              'regARIMA residuals.',fhnote,Mt2,F,T)
       END IF
       pkstr='seasonal'
       ipk=8
      END IF
c-----------------------------------------------------------------------
      IF(lsrsd.gt.0.or.ltdrsd.gt.0)THEN
       IF(Prttab(Tblptr))THEN
        ns=24
        IF(Lseats)THEN
         slbl(1:ns)='SEATS extended residuals'
        ELSE
         slbl(1:ns)='regARIMA model residuals'
        END IF
        IF(ltdrsd.gt.0.and.lsrsd.gt.0)THEN
         WRITE(Mt1,1020)'V',pkstr(1:ipk),'have',begstr(1:nchr1)
         CALL writTagClass(Mt1,'ul','indent')
         WRITE(Mt1,1040)slbl(1:ns),lsrsd,ltdrsd
         CALL writTag(Mt1,'</ul>')
        ELSE
         WRITE(Mt1,1020)'At least one v',pkstr(1:ipk),'has',
     &                  begstr(1:nchr1)       
         CALL writTagClass(Mt1,'ul','indent')
         IF(ltdrsd.gt.0)THEN
          WRITE(Mt1,1030)slbl(1:ns),ltdrsd
         ELSE IF(lsrsd.gt.0)THEN
          WRITE(Mt1,1050)slbl(1:ns),lsrsd
         END IF
         CALL writTag(Mt1,'</ul>')
        END IF
       END IF
       IF(Lsavpk)THEN
        IF(Lseats)THEN
         ns=6
         slbl(1:ns)='extrsd'
        ELSE
         ns=3
         slbl(1:ns)='rsd'
        END IF
        IF(ltdrsd.gt.0)THEN
         Ctpeak((Ntpeak+1):(Ntpeak+ns))=slbl(1:ns)
         Ntpeak=Ntpeak+ns+1
        END IF
        IF(lsrsd.gt.0)THEN
         Cspeak((Nspeak+1):(Nspeak+ns))=slbl(1:ns)
         Nspeak=Nspeak+ns+1
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
c     Spectrum plot for detrended original series
c-----------------------------------------------------------------------
      IF(.not.Prttab(Tblptr))RETURN
      icode=20
      IF(.not.ltdfrq)icode=21
      IF(Spctyp.eq.0)THEN
       ctype='10*LOG(SPECTRUM)'
       ntype=16
      ELSE
       ctype='10*LOG(PERIODOGRAM)'
       ntype=19
      END IF
      IF((ltdrsd.gt.0.or.ltdrsd.gt.0).and.Prttab(Tblptr))
     &   WRITE(Mt1,'(//)')
      IF(Lseats)THEN
       ttl(1)='     '//ctype(1:ntype)//
     &        ' of the SEATS extended residuals.'
      ELSE
       ttl(1)='     '//ctype(1:ntype)//
     &        ' of the regARIMA model residuals.'
      END IF
      numttl(1)=ntype+37
      ttl(2)='     Spectrum estimated from '//begstr(1:nchr1)//
     &       ' to '//endstr(1:nchr2)//'.'
      numttl(2)=34+nchr1+nchr2
      CALL grzlst(IONE,SIXONE,IZERO,rsdsxx,rsdsxx,SIXONE,ITEN,IZERO)
      CALL genSkip(Tblptr)
      CALL chrt(ttl,numttl,icode,1,120/Sp)
c-----------------------------------------------------------------------
 1020 FORMAT(/,'<p>',a,'isually significant residual ',a,' peaks ',a,
     &          ' been',
     &        /,'  found in the spectral plot of the following series',
     &          ' starting in ',a,':</p>')
 1030 FORMAT(' <li>',a,' (',i1,' Trading Day peak(s))</li>')
 1040 FORMAT(' <li>',a,' (',i1,' Seasonal and ',i1,
     &       ' Trading Day peak(s))</li>')
 1050 FORMAT(' <li>',a,' (',i1,' Seasonal peak(s))</li>')
* 1070 FORMAT(/,'          Rerun the input file without the output ',
*     &         'suppression option',/,
*     &         '          (-n flag) for more details.')
 1080 FORMAT(a,'.tukey.m: ',i5)
c-----------------------------------------------------------------------
      RETURN
      END
