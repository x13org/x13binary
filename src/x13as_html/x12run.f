C     Last change:Mar. 2021 if there is history or sliding spans ,
C     rest Tabtbl,skip _tbs file
C     Last change:  BCM  23 Mar 2005    3:38 pm
      SUBROUTINE x12run(Isrs,Unopnd,Nopen,Lchkin,Lcomp,Rok,Fok,N1,Nfail,
     &                  Ldata,Dtafil,Mtafil,Nmf,Dattim,X11agr,Lgraf,
     &                  Lexgrf,l1stcomp)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     preform and x12 run on one series
C-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'lex.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'agr.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'htmlout.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'dgnsvl.i'
      INCLUDE 'seatop.cmn'
C-----------------------------------------------------------------------
      INTEGER FILLIM
      PARAMETER(FILLIM=64)
      CHARACTER blnk*(63)
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
C-----------------------------------------------------------------------
      CHARACTER ttlvec*(80),filenm*(FILLIM),srsttl*(PSRSCR),Dattim*(24),
     &          mdlfil*(PFILCR),Dtafil*(PFILCR),Mtafil*(PFILCR)
      REAL bticks,ticks
      LOGICAL Lfatal,Fok,Lchkin,lx11,lmodel,hvmfil,Lcomp,Ldata,Rok,
     &        X11agr,Lgraf,Lexgrf,lseats,l1stcomp
      DOUBLE PRECISION Lam,sscut
      INTEGER i,Fcntyp,Isrs,Nmf,Unopnd,Nopen,ltmax,N1,notc,ncur,
     &        icur,nsrscr,Nfail
      DIMENSION Unopnd(*),ttlvec(10),sscut(5)
C-----------------------------------------------------------------------
      INTEGER lstpth,nblank,sfmax
      LOGICAL dpeq,istrue
      EXTERNAL lstpth,nblank,sfmax,dpeq,istrue
c-----------------------------------------------------------------------
      COMMON /fcnerr/ Lfatal
      COMMON /armalm/ Lam,Fcntyp
C-----------------------------------------------------------------------
c     Process a series.
C-----------------------------------------------------------------------
      CALL setchr(' ',63,blnk)
      IF(Ltimer)THEN
       CALL cpu_time(bticks)
      END IF
      Lfatal=F
      Opnudg=F
c-----------------------------------------------------------------------
c     Extract filename from Cursrs.
c-----------------------------------------------------------------------
      ncur=nblank(Cursrs)
      icur=lstpth(Cursrs,ncur)
      ncur=min(ncur-icur,FILLIM)
      filenm(1:ncur)=Cursrs(icur+1:ncur+icur)
      IF(ncur.lt.FILLIM)filenm(ncur+1:FILLIM)=blnk(1:(FILLIM-ncur))
C-----------------------------------------------------------------------
c     generate file names for output files
C-----------------------------------------------------------------------
      CALL genfor(Fok,Lchkin,Isrs)
C-----------------------------------------------------------------------
c     Generate error message for genfor
C-----------------------------------------------------------------------
      IF(.not.Fok)THEN
       WRITE(STDOUT,*)
     &    ' error in genfor - unable to open input/output files for '//
     &    PRGNAM
       Nopen=Nopen+1
       Unopnd(Nopen)=Isrs
       Lfatal=T
       CALL writTagOneLine(Ng,'p','center','No log entry for <strong>'//
     &                     filenm(1:ncur)//'</strong> due to errors.')
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Set up variables for HTML output
c-----------------------------------------------------------------------
      Inpe=0
      Inse=0
      Intv=0
      Ingr=0
      Invl=0
      Inss=0
      Inspn=0
      Indf=0
      Inms=0
      Infv=0
      Inbt=0
      Inrs=0
      Intl=0
      Inrl=0
      Inim=0
      Infq=0
      Inmd=0
      Inmu=0
      Insd=0
      Inmq=0
      Inqs=0
      Innp=0
      Inspc=0
      Inrv=0
      Infoot=0
      DO i=1,1000
       Vfoot(i)=0
       IF(i.le.7)Indy(i)=0
       IF(i.le.4)Inssp(i)=0
      END DO
      Idxtab=1
      Inpmdl=0
      Inplkh=0
      Inpacf=0
C-----------------------------------------------------------------------
      CALL mdlint()
C-----------------------------------------------------------------------
c     If all files have been opened, read in X-13A-S options
C-----------------------------------------------------------------------
      nsrscr=0
      CALL gtinpt(sscut,srsttl,nsrscr,ttlvec,notc,lx11,X11agr,lseats,
     &            lmodel,Ldata,Dtafil,l1stcomp,hvmfil,mdlfil,Rok)
      IF(Lfatal)RETURN
C-----------------------------------------------------------------------
c     If series name hasn't been set, set to be the filename.  Then
c     set the number of characters in the series name, truncated to 16.
C-----------------------------------------------------------------------
      IF(Nser.eq.0)THEN
       Serno=filenm
       Nser=nblank(Serno)
*       Nser=min(nblank(Serno),16)
*      ELSE
*       Nser=min(16,Nser)
      END IF
C-----------------------------------------------------------------------
c     If there are no input errors, check the input options.
C-----------------------------------------------------------------------
c      IF(Rok)THEN
      IF(Rok.and.Lexok)THEN
c-----------------------------------------------------------------------
c     If an error is found in a previous spec, print an error message 
c     and do not perform the direct and indirect adjustment of the 
c     aggregate total.
c-----------------------------------------------------------------------
       IF((Iagr.eq.3.and.Nfail.gt.0).or.Iagr.eq.NOTSET)THEN
        IF(Iagr.eq.3)THEN
         CALL eWritln('Error(s) were found while executing the spec'//
     &                ' file(s) of component ',STDERR,Mt2,T,F)
         CALL writln('       series used for this composite '//
     &               'adjustment.  The direct and indirect',
     &               STDERR,Mt2,F,F)
         CALL writln('       seasonal adjustment of the total series '//
     &               'will not be performed.',STDERR,Mt2,F,T)
        END IF
        CALL writln('       Correct the error(s) for the component '//
     &              'series and rerun the ',STDERR,Mt2,T,F)
        CALL writln('       metafile '//Mtafil(1:Nmf)//'.',
     &              STDERR,Mt2,F,T)
        Lfatal=T
        RETURN
c-----------------------------------------------------------------------
c     Else, check the input options and print the header.
c-----------------------------------------------------------------------
       ELSE
        CALL editor(sscut,srsttl,nsrscr,ttlvec,notc,Lchkin,Lcomp,lx11,
     &              lseats,lmodel,Ldata,hvmfil,mdlfil,Dattim,
     &              Lgraf,Lexgrf,Rok)
        IF(Lfatal)RETURN
       END IF
      END IF
      Lhiddn=.false.
C-----------------------------------------------------------------------
c     If there are errors in the input, print out a message to correct
c     the input.
C-----------------------------------------------------------------------
c      IF(.not.Rok)THEN
      IF(.NOT.(Lexok.and.Rok))THEN
       WRITE(STDOUT,*)' **Correct input and rerun '//Infile(1:N1)//
     &                '.spc**'
       Lfatal=T
C-----------------------------------------------------------------------
c     If there are no errors in the input and the check input option
c     is selected, print out a message.
C-----------------------------------------------------------------------
      ELSE IF(Lchkin)THEN
       WRITE(STDOUT,*)' Input checking complete for ',Infile(1:N1),
     &                '.spc'
C-----------------------------------------------------------------------
c     If there are no errors in the input and the check input option
c     is not selected, call the X-13A-S main driver routine.
C-----------------------------------------------------------------------
      ELSE 
C-----------------------------------------------------------------------
       IF(Lsumm.gt.0)THEN
        IF(lmodel)THEN
         WRITE(Nform,1010)'yes'
        ELSE
         WRITE(Nform,1010)'no'
        END IF
       END IF
C-----------------------------------------------------------------------
       CALL ssprep(lmodel,T,T)
       CALL qcontr(Ny)
       CALL x11int
       IF(Ltimer)THEN
        CALL cpu_time(ticks)
        WRITE(Nform,9000) 'bx12run:',bticks
        WRITE(Nform,9000) 'bx11ari:',ticks
       END IF
       CALL x11ari(lmodel,lx11,X11agr,lseats,Lcomp,Issap,Irev,Irevsa,
     &             Ixreg,Lsumm,Ltimer,Lgraf)
       IF(Lfatal)RETURN
       IF(Ltimer)THEN
        CALL cpu_time(ticks)
        WRITE(Nform,9000) 'ex11ari:',ticks
       END IF
C-----------------------------------------------------------------------
c     IF not doing either sliding spans or revisons history, return
C-----------------------------------------------------------------------
       IF(Issap.eq.0.and.Irev.eq.0)THEN
        IF(Lsumm.gt.0)THEN
         WRITE(Nform,1000)'no'
         WRITE(Nform,1005)'no'
         WRITE(Nform,1015)'no'
        END IF
        IF(Ltimer.and.Rok)THEN
         CALL cpu_time(ticks)
         WRITE(Nform,9000) 'ex12run:',ticks
         WRITE(Nform,9000) 'run.time:',ticks-bticks
        END IF
        RETURN
       END IF
C-----------------------------------------------------------------------
c     determine themaximum seasonal filter length.
C----------------------------------------------------------------------
       ltmax=sfmax(Lterm,Lter,Ny)
c-----------------------------------------------------------------------
       IF(Issap.gt.0.and.Irev.gt.0)
     &    CALL ss2rv(Lmodel,Lx11,Ixreg.gt.0,lseats)
C-----------------------------------------------------------------------
c     Perform sliding spans analysis
C-----------------------------------------------------------------------
       IF(Issap.eq.1)THEN
        IF(Ltimer)THEN
         CALL cpu_time(ticks)
         WRITE(Nform,9000) 'bsspan:',ticks
        END IF
c change Mar.2021 if there is sliding span ,rest Tabtbl,skip _tbs file
        CALL setchr(' ',100,Tabtbl)
        CALL sspdrv(ltmax,lmodel,lx11,X11agr,lseats,Lcomp,Lgraf,Iagr,
     &              Ncomp)
        IF(Lfatal.or.Issap.le.0)THEN
         IF(Issap.le.0.and.Nfile.gt.0)THEN
          IF(Lsumm.gt.0)WRITE(Nform,1005)'failed'
          IF(Svltab(LSLPCT))
     &       CALL mkPOneLine(Ng,'center',
     &              'Sliding spans analysis failed : check error file.')

         END IF
         IF(Lfatal)RETURN
        END IF
        IF(Ltimer)THEN
         CALL cpu_time(ticks)
         WRITE(Nform,9000) 'esspan:',ticks
        END IF
        IF(Irev.gt.0)THEN
         CALL rv2ss(Lmodel,Lx11,Ixreg.gt.0,lseats)
         CALL restor(Lmodel,Lx11,Ixreg.gt.0)
        END IF
        Issap=0
       ELSE IF(Lsumm.gt.0)THEN
        WRITE(Nform,1005)'no'
       END IF
C-----------------------------------------------------------------------
c     Perform revisions analysis
C-----------------------------------------------------------------------
       IF(Irev.gt.0)THEN
        IF(Ltimer)THEN
         CALL cpu_time(ticks)
         WRITE(Nform,9000) 'bhist:',ticks
        END IF
c change Mar.2021 if there is history ,rest Tabtbl,skip _tbs file
        CALL setchr(' ',100,Tabtbl)
        CALL revdrv(ltmax,lmodel,lx11,X11agr,lseats,Lcomp,Lgraf,Iagr,
     &              Ncomp)
*        IF(Lfatal)RETURN
        IF(Lfatal.or.Irev.eq.0)THEN
         IF(Irev.eq.0.and.Nfile.gt.0)THEN
          IF(Lsumm.gt.0)THEN
           WRITE(Nform,1000)'failed'
           IF(Irevsa.lt.0)WRITE(Nform,1015)'failed'
          END IF
          IF(istrue(Svltab,LSLASA,LSLAFE))
     &       CALL mkPOneLine(Ng,'center',
     &                    'History analysis failed : check error file.')
         END IF
         IF(Ltimer)THEN
          CALL cpu_time(ticks)
          WRITE(Nform,9000) 'ehist:',ticks
         END IF
        END IF
       ELSE IF(Lsumm.gt.0)THEN
        WRITE(Nform,1000)'no'
       END IF
      END IF
C-----------------------------------------------------------------------
      IF(Ltimer.and.Rok)THEN
       CALL cpu_time(ticks)
       WRITE(Nform,9000) 'ex12run:',ticks
       WRITE(Nform,9000) 'run.time:',ticks-bticks
      END IF
C-----------------------------------------------------------------------
 1000 FORMAT('history: ',a)
 1005 FORMAT('sspans: ',a)
 1010 FORMAT('mdg: ',a)
 1015 FORMAT('historysa: ',a)
 9000 FORMAT(a,e15.8)
      RETURN
      END
