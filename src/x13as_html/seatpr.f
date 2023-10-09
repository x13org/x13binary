c     Last change: 10/2021- add if trendtc=yes, treat tc as ls, print/
c     save long trend table without outlier effects
c     previous Change, fix .dor issue- Mar. 2021
      SUBROUTINE seatpr(Begspn,Endspn,Ny,Muladd,Kpart,Kdec,Lsumm,Lgraf,
     &                  Lam,Lttc)
      IMPLICIT NONE
C-----------------------------------------------------------------------
c     Driver routine for the printing and saving tables saved from the
c     SEATS seasonal adjustment routines
C-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'dimensions.i'
      INCLUDE 'notset.prm'
      INCLUDE 'seatcm.cmn'
      INCLUDE 'seatmd.cmn'
      INCLUDE 'seatdg.cmn'
      INCLUDE 'seatlg.cmn'
      INCLUDE 'stcfcm.cmn'
      INCLUDE 'adj.cmn'
      INCLUDE 'inpt.cmn'
      INCLUDE 'force.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'priusr.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11fac.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'seattb.i'
      INCLUDE 'frctbl.i'
      INCLUDE 'mdltbl.i'
      INCLUDE 'spctbl.i'
      INCLUDE 'sig.i'
C-----------------------------------------------------------------------
      INCLUDE 'tbltitle.prm'
      INCLUDE 'desset.prm'
      INCLUDE 'filext.prm'
C-----------------------------------------------------------------------
      LOGICAL F,T
      DOUBLE PRECISION ONE,ZERO
      PARAMETER(ONE=1D0,ZERO=0D0,F=.false.,T=.true.)
c     ------------------------------------------------------------------
      INTEGER N1,N12
      PARAMETER (N12 = 12, N1 = 1)
      INCLUDE 'calc.i'
C-----------------------------------------------------------------------
      CHARACTER tblttl*(PTTLEN)
      DOUBLE PRECISION dvec,Lam,setsac,settrc,stcirb,frcfac,sfsum,
     &                 sadiff,trdiff,trcy,trcydf,stadsa,stadtr,stadsf,
     &                 saoadj,iroadj,setadj
      LOGICAL Lgraf,pre18b,taklog,Lttc
      INTEGER Begspn,Endspn,Lsumm,Ny,Muladd,Kpart,kp2,i,lastob,lstfrc,
     &        frstob,sf1ob,sa1ob,tr1ob,idate,ntbttl,ndiff,Kdec,outdec,
     &        thisd,nlast,begfct
      DIMENSION Begspn(2),Endspn(2),dvec(1),setsac(PLEN),settrc(PLEN),
     &          stcirb(PLEN),frcfac(PLEN),sfsum(PLEN),sadiff(PLEN),
     &          trdiff(PLEN),trcy(PLEN),trcydf(PLEN),idate(2),begfct(2),
     &          stadsa(PLEN),stadtr(PLEN),stadsf(PLEN),saoadj(PLEN),
     &          iroadj(PLEN),setadj(PLEN)
c-----------------------------------------------------------------------
      CHARACTER getAna,getTMCS
      LOGICAL dpeq
      EXTERNAL dpeq,getAna,getTMCS
C-----------------------------------------------------------------------
      DOUBLE PRECISION Ckhs
      DIMENSION Ckhs(PLEN)
      COMMON /kcser / Ckhs
C-----------------------------------------------------------------------
      INCLUDE 'desset.var'
      INCLUDE 'filext.var'
C-----------------------------------------------------------------------
      kp2=Kpart
      Kpart=-1
      lastob=Posfob
      IF(Savfct)THEN
       lastob=Posffc
*       IF(Nfcst.gt.2*Ny)lastob=lastob-(Nfcst-2*Ny)
      END IF
      nlast=Posffc
*      IF(Nfcst.gt.2*Ny)nlast=nlast-(Nfcst-2*Ny)
      frstob=Pos1ob
      IF(Savbct)frstob=Pos1bk
      outdec=Kdec
      IF((.not.dpeq(Lam,ONE)).and.outdec.lt.3)outdec=3
      CALL addate(Begspn,Ny,Posfob-Pos1ob+1,begfct)
C-----------------------------------------------------------------------
      IF(Savtab(LSEMDC))THEN
       IF(.not.(dpeq(Tcvar,DNOTST).and.dpeq(Svar,DNOTST).and.
     &    dpeq(Savar,DNOTST).and.dpeq(Trvar,DNOTST).and.
     &    dpeq(Irrvar,DNOTST)))CALL savmdc(LSEMDC)
      END IF
C-----------------------------------------------------------------------
      IF(Savtab(LSEWKF))THEN
       IF(.not.(Ntcwkf.eq.NOTSET.and.Nsawkf.eq.NOTSET.and.
     &    Nswkf.eq.NOTSET.and.Ntrwkf.eq.NOTSET.and.Nirwkf.eq.NOTSET))
     &    CALL savwkf(LSEWKF)
      END IF
C-----------------------------------------------------------------------
      IF(Savtab(LSEPIN).and.Hpitrc)THEN
       CALL punch(Spitrc,Pos1ob,Posfob,LSEPSI,F,F)
       IF(Lfatal)RETURN
      END IF
      i=LSEPIN+1
      IF(Savtab(i).and.Hpis)THEN
       CALL punch(Spis,Pos1ob,Posfob,i,F,F)
       IF(Lfatal)RETURN
      END IF
      i=LSEPIN+2
      IF(Savtab(i).and.Hpitra)THEN
       CALL punch(Spitra,Pos1ob,Posfob,i,F,F)
       IF(Lfatal)RETURN
      END IF
      i=LSEPIN+3
      IF(Savtab(i).and.Hpisa)THEN
       CALL punch(Spisa,Pos1ob,Posfob,i,F,F)
       IF(Lfatal)RETURN
      END IF
c     ------------------------------------------------------------------
c     write differenced original series (Nov 2013 BCM)
c     changed Mar,2021, make thisd = Bd*Ny+D
c     only Nodiff > 0 , print or save table
c     ------------------------------------------------------------------
       IF(Prttab(LSEDOR).or.Savtab(LSEDOR).or.Lgraf)THEN
c        IF(Idssm.ne.NOTSET.and.Idrsm.ne.NOTSET)THEN
c         thisd=Idssm+Idrsm
c        ELSE
         thisd=Bd*Ny+D
c        end if
c     ------------------------------------------------------------------
        CALL addate(Begspn,Ny,thisd,idate)
        IF(Prttab(LSEDOR).and.Nodiff.gt.0)THEN
         CALL genSkip(LSEDOR)
         CALL makttl(DSEDIC,dseptr,PDSE,LSEDOR,PDSUM10,tblttl,ntbttl,
     &               T,F)
         IF(.not.Lfatal)CALL prtshd(tblttl(1:ntbttl),idate,Ny,Nodiff)
         IF(.not.Lfatal)CALL prttbl(idate,Ny,Odiff,Nodiff,'Data',outdec,
     &                              tbxdic(LSEDOR))
        END IF
        IF(.not.Lfatal.and.Savtab(LSEDOR).and.Nodiff.gt.0)
     &     CALL savtbl(LSEDOR,idate,1,Nodiff,Ny,Odiff,Serno,Nser,F)
        IF(.not.Lfatal.and.Lgraf.and.Nodiff.gt.0)
     &     CALL savtbl(LSEDOR,idate,1,Nodiff,Ny,Odiff,Serno,Nser,Lgraf)
        IF(Lfatal)RETURN
       END IF
C-----------------------------------------------------------------------
      IF((Prttab(LSPERS).or.Savtab(LSPERS).or.Lgraf).and.Nrsdex.gt.0
     &    .and.((getAna().eq.'Y').or.(getTMCS().eq.'Y')).and.
     &    Ny.eq.12)THEN
       CALL spcrsd(Srsdex,Nrsdex,Begspn,Ny,Endspn,LSPERS,T,Lsumm,Lgraf)
       IF(Lfatal)RETURN
      END IF
C-----------------------------------------------------------------------
      IF(Havesf)THEN
       IF(Prttab(LSESEA).or.Prttab(LSEPSS))
     &    CALL table(Seatsf,Pos1ob,Posfob,10,1,1,dvec,LSESEA)
       IF(.not.Lfatal.and.Savtab(LSESEA))
     &    CALL punch(Seatsf,frstob,lastob,LSESEA,F,F)
       IF(.not.Lfatal.and.Savtab(LSEPSS))
     &    CALL punch(Seatsf,frstob,lastob,LSEPSS,F,Muladd.ne.1)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(Seatsf,frstob,lastob,LSESEA,Lgraf,F)
       IF(Lfatal)RETURN
C-----------------------------------------------------------------------
c      save seasonal se
C-----------------------------------------------------------------------
       IF(Hvsfse)THEN
        IF(Savtab(LSESSF))CALL punch(Stsfse,frstob,Lstsse,LSESSF,F,F)
        IF(Lgraf)CALL punch(Stsfse,frstob,Lstsse,LSESSF,Lgraf,F)
       END IF
C-----------------------------------------------------------------------
c     compute seasonal sums, print out and save (BCM, Feb 2008)
C-----------------------------------------------------------------------
       IF((Prttab(LSESSM).or.Savtab(LSESSM).or.Lgraf).and.Havesf)THEN
c     ------------------------------------------------------------------
c     Remove outliers from series before differencing
c     ------------------------------------------------------------------
        CALL copy(Seatsf(Pos1ob),nlast-Pos1ob+1,1,stadsf(Pos1ob))
        IF(Adjso.eq.1)CALL divsub(stadsf,stadsf,Facso,Pos1ob,nlast)
c     ------------------------------------------------------------------
        CALL genssm(stadsf,Pos1ob,nlast,sfsum,sf1ob,Ny,Lam)
        IF(Prttab(LSESSM))THEN
         CALL genSkip(LSESSM)
         ndiff=Posfob-sf1ob+1
         CALL addate(Begspn,Ny,sf1ob-Pos1ob,idate)
         CALL makttl(DSEDIC,dseptr,PDSE,LSESSM,PDSUM10,tblttl,ntbttl,
     &               T,F)
         IF(.not.Lfatal)CALL prtshd(tblttl(1:ntbttl),idate,Ny,ndiff)
         IF(.not.Lfatal)CALL prttbl(idate,Ny,sfsum(sf1ob),ndiff,'Data',
     &                              outdec,tbxdic(LSESSM))
        END IF
        IF(.not.Lfatal.and.Savtab(LSESSM))
     &     CALL punch(sfsum,sf1ob,lastob,LSESSM,F,F)
        IF(.not.Lfatal.and.Lgraf)
     &     CALL punch(sfsum,sf1ob,lastob,LSESSM,Lgraf,F)
        IF(Lfatal)RETURN
       END IF
C-----------------------------------------------------------------------
C      Save seasonal stochastic forecasts
C-----------------------------------------------------------------------
       IF(Havfsf)THEN
        i=LSEFCD+1
        CALL addate(Endspn,Ny,1,idate)
        IF(Savtab(i))
     &     CALL savtbl(i,begfct,1,Nsfsf,Ny,Setfsf,Serno,Nser,F)
        IF(.not.Lfatal.and.Lgraf)
     &     CALL savtbl(i,begfct,1,Nsfsf,Ny,Setfsf,Serno,Nser,Lgraf)
        IF(Lfatal)RETURN
       END IF
      END IF
C-----------------------------------------------------------------------
      IF(Havesa)THEN
c     ------------------------------------------------------------------
C --- remove constant from seasonally adjusted series.
c     (added by BCM July 2005)
c     ------------------------------------------------------------------
       IF(.not.dpeq(Cnstnt,DNOTST))THEN
        CALL copy(Seatsa,nlast,-1,setsac)
        DO i=Pos1ob,nlast
         Seatsa(i)=Seatsa(i)-Cnstnt
        END DO
       END IF
c     ------------------------------------------------------------------
       IF(Prttab(LSESE2).or.Savtab(LSESE2).or.Lgraf)THEN
        CALL copy(Seatsa,nlast,-1,saoadj)
        IF((.not.Finao).and.Adjao.eq.1)
     &    CALL divsub(saoadj,saoadj,Facao,Pos1bk,nlast)
        IF((.not.Finls).and.Adjls.eq.1)
     &    CALL divsub(saoadj,saoadj,Facls,Pos1bk,nlast)
       END IF
c     ------------------------------------------------------------------
       IF(Prttab(LSESA).or.(Out.ne.2.and.Nustad.gt.0))
     &    CALL table(Seatsa,Pos1ob,Posfob,11,1,1,dvec,LSESA)
       IF(.not.Lfatal.and.Savtab(LSESA))
     &    CALL punch(Seatsa,frstob,lastob,LSESA,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(Seatsa,frstob,lastob,LSESA,Lgraf,F)
       IF(Lfatal)RETURN
c     ------------------------------------------------------------------
       CALL copy(Seatsa(Pos1ob),nlast-Pos1ob+1,1,Ckhs(Pos1ob))
c     ------------------------------------------------------------------
       IF(Prttab(LSESE2).or.(Out.ne.2.and.Nustad.gt.0))
     &    CALL table(saoadj,Pos1ob,Posfob,11,1,1,dvec,LSESE2)
       IF(.not.Lfatal.and.Savtab(LSESE2))
     &    CALL punch(saoadj,frstob,lastob,LSESE2,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(saoadj,frstob,lastob,LSESE2,Lgraf,F)
       IF(Lfatal)RETURN
C-----------------------------------------------------------------------
c      save sa se
C-----------------------------------------------------------------------
       IF(Hvsase)THEN
        IF(Savtab(LSESSA))CALL punch(Stsase,frstob,Lstase,LSESSA,F,F)
        IF(Lgraf)CALL punch(Stsase,frstob,Lstase,LSESSA,Lgraf,F)
       END IF
c     ------------------------------------------------------------------
       IF(.not.dpeq(Cnstnt,DNOTST))THEN
        IF(Prttab(LSESAC))
     &     CALL table(setsac,Pos1ob,Posfob,11,1,1,dvec,LSESAC)
        IF(.not.Lfatal.and.Savtab(LSESAC))
     &     CALL punch(setsac,frstob,lastob,LSESAC,F,F)
        IF(.not.Lfatal.and.Lgraf)
     &     CALL punch(setsac,frstob,lastob,LSESAC,Lgraf,F)
        IF(Lfatal)RETURN
       END IF
c     ------------------------------------------------------------------
c     write differenced seasonally adjusted series (Feb 2008 BCM)
c     ------------------------------------------------------------------
       IF(Prttab(LSEDSA).or.Savtab(LSEDSA).or.Lgraf)THEN
c     ------------------------------------------------------------------
        IF(Idssm.ne.NOTSET.and.Idrsm.ne.NOTSET)THEN
         thisd=Idssm+Idrsm
        ELSE
         thisd=Bd+D
        ENDIF
        taklog=dpeq(Lam,ZERO)
c     ------------------------------------------------------------------
c     Remove outliers from series before differencing
c     ------------------------------------------------------------------
        CALL copy(Seatsa(Pos1ob),Posfob-Pos1ob+1,1,stadsa(Pos1ob))
        IF(Adjao.eq.1)CALL divsub(stadsa,stadsa,Facao,Pos1ob,Posfob)
        IF(Adjls.eq.1)CALL divsub(stadsa,stadsa,Facls,Pos1ob,Posfob)
        IF(Adjtc.eq.1)CALL divsub(stadsa,stadsa,Factc,Pos1ob,Posfob)
        IF(Adjso.eq.1)CALL divsub(stadsa,stadsa,Facso,Pos1ob,Posfob)
c     ------------------------------------------------------------------
        CALL gendff(stadsa,Pos1ob,Posfob,sadiff,sa1ob,taklog,T,thisd)
        IF(Prttab(LSEDSA))THEN
         CALL genSkip(LSEDSA)
         ndiff=Posfob-sa1ob+1
         CALL addate(Begspn,Ny,sa1ob-Pos1ob,idate)
         CALL makttl(DSEDIC,dseptr,PDSE,LSEDSA,PDSUM10,tblttl,ntbttl,
     &               T,F)
         IF(.not.Lfatal)CALL prtshd(tblttl(1:ntbttl),idate,Ny,ndiff)
         IF(.not.Lfatal)CALL prttbl(idate,Ny,sadiff(sa1ob),ndiff,'Data',
     &                              outdec,tbxdic(LSEDSA))
        END IF
        IF(.not.Lfatal.and.Savtab(LSEDSA))
     &     CALL punch(sadiff,sa1ob,Posfob,LSEDSA,F,F)
        IF(.not.Lfatal.and.Lgraf)
     &     CALL punch(sadiff,sa1ob,Posfob,LSEDSA,Lgraf,F)
        IF(Lfatal)RETURN
       END IF
c     ------------------------------------------------------------------
C --- WRITE SEASONALLY ADJUSTED SERIES WITH REVISED YEARLY TOTALS D11A.
c     ------------------------------------------------------------------
       IF(Iyrt.gt.0)THEN
c     ------------------------------------------------------------------
c     use Lfctfr to set last observation to be forced (BCM, May 2006)
c     ------------------------------------------------------------------
        IF(Lfctfr)THEN
         lstfrc=nlast
        ELSE
         lstfrc=Posfob
        END IF
        IF(Prttab(LFCSAA))
     &     CALL table(Setsa2,Pos1ob,Posfob,11,2,2,dvec,LFCSAA)
        IF(.not.Lfatal.and.Savtab(LFCSAA))
     &     CALL punch(Setsa2,Pos1ob,Posfob,LFCSAA,F,F)
        IF(.not.Lfatal.and.Lgraf)
     &     CALL punch(Setsa2,Pos1ob,Posfob,LFCSAA,Lgraf,F)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     compute forcing factor from seasonally adjusted series
c     (BCM May 2006)
c     ------------------------------------------------------------------
        CALL divsub(frcfac,Seatsa,Setsa2,Posfob,lstfrc)
c     ------------------------------------------------------------------
C --- WRITE SEASONALLY ADJUSTED SERIES WITH REVISED YEARLY TOTALS D11A.
c     ------------------------------------------------------------------
        IF(Prttab(LFRFAC))
     &     CALL table(frcfac,Pos1ob,Posfob,11,6,1,dvec,LFRFAC)
        IF((.not.Lfatal).and.Savtab(LFRFAC))
     &     CALL punch(frcfac,Pos1ob,lstfrc,LFRFAC,F,F)
        IF((.not.Lfatal).and.Lgraf)
     &     CALL punch(frcfac,Pos1ob,lstfrc,LFRFAC,Lgraf,F)
        IF(Lfatal)RETURN
       END IF
c     ------------------------------------------------------------------
c     If option selected ensure the rounded seasonally adjusted values
c     equals the rounded seasonally adjusted total.
c     ------------------------------------------------------------------
       IF(Lrndsa)THEN
c     ------------------------------------------------------------------
C --- WRITE rounded SEASONALLY ADJUSTED SERIES
c     ------------------------------------------------------------------
        IF(Prttab(LFCRND))
     &     CALL table(Stsarn,Pos1ob,Posfob,11,3,2,dvec,LFCRND)
        IF(.not.Lfatal.and.Savtab(LFCRND))
     &     CALL punch(Stsarn,Pos1ob,Posfob,LFCRND,F,F)
        IF(.not.Lfatal.and.Lgraf)
     &     CALL punch(Stsarn,Pos1ob,Posfob,LFCRND,Lgraf,F)
        IF(Lfatal)RETURN
       END IF
C-----------------------------------------------------------------------
C      Save seasonally adj series stochastic forecasts
C-----------------------------------------------------------------------
       IF(Havfsa)THEN
        i=LSEFCD+3
        IF(Savtab(i))
     &    CALL savtbl(i,begfct,1,Nsfsa,Ny,Setfsa,Serno,Nser,F)
        IF(.not.Lfatal.and.Lgraf)
     &    CALL savtbl(i,begfct,1,Nsfsa,Ny,Setfsa,Serno,Nser,Lgraf)
        IF(Lfatal)RETURN
       END IF
C-----------------------------------------------------------------------
      END IF
C-----------------------------------------------------------------------
      IF(Havetr)THEN
       CALL copy(Seattr(Pos1ob),Posfob-Pos1ob+1,1,setadj(Pos1ob))
c     ------------------------------------------------------------------
C --- remove constant from trend component.  (added by BCM July 2005)
c     ------------------------------------------------------------------
       IF(.not.dpeq(Cnstnt,DNOTST))THEN
        CALL copy(Seattr,nlast,-1,settrc)
        DO i=Pos1ob,nlast
         Seattr(i)=Seattr(i)-Cnstnt
        END DO
       END IF
       IF(Prttab(LSETRN))
     &    CALL table(Seattr,Pos1ob,Posfob,12,1,1,dvec,LSETRN)
       IF(.not.Lfatal.and.Savtab(LSETRN))
     &    CALL punch(Seattr,frstob,lastob,LSETRN,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(Seattr,frstob,lastob,LSETRN,Lgraf,F)
       IF(Lfatal)RETURN
C-----------------------------------------------------------------------
c      save trend se
C-----------------------------------------------------------------------
       IF(Hvtrse)THEN
        IF(Savtab(LSESTR))CALL punch(Sttrse,frstob,Lsttse,LSESTR,F,F)
        IF(Lgraf)CALL punch(Sttrse,frstob,Lsttse,LSESTR,Lgraf,F)
       END IF
c     ------------------------------------------------------------------
       IF(.not.dpeq(Cnstnt,DNOTST))THEN
        IF(Prttab(LSETAC))
     &     CALL table(settrc,Pos1ob,Posfob,11,1,1,dvec,LSETAC)
        IF(.not.Lfatal.and.Savtab(LSETAC))
     &     CALL punch(settrc,frstob,lastob,LSETAC,F,F)
        IF(.not.Lfatal.and.Lgraf)
     &     CALL punch(settrc,frstob,lastob,LSETAC,Lgraf,F)
        IF(Lfatal)RETURN
       END IF
c     ------------------------------------------------------------------
c     Remove LS outliers or TC outlier if trendtc = yes from trend
c     print trend without outlier effects table
c     ------------------------------------------------------------------
        IF((.not.Finls).and.Adjls.eq.1)
     &     CALL divsub(setadj,setadj,Facls,Pos1ob,Posfob)
        IF((.not.Fintc).and.Lttc.and.Adjtc.eq.1)
     &     CALL divsub(setadj,setadj,Factc,Pos1ob,Posfob)
c     ------------------------------------------------------------------
c     write final trend cycel without outlier effects trend (Sep 2021)
c     ------------------------------------------------------------------
       IF(Prttab(LSESTL))THEN
         CALL table(setadj,Pos1ob,Posfob,12,1,1,dvec,LSESTL)
       END IF
       IF(.not.Lfatal.and.Savtab(LSESTL))
     &     CALL punch(setadj,Pos1ob,Posfob,LSESTL,F,F)
c     ------------------------------------------------------------------
c     write differenced trend (Feb 2008 BCM)
c     ------------------------------------------------------------------
       IF(Prttab(LSEDTR).or.Savtab(LSEDTR).or.Lgraf)THEN
        IF(Idssm.ne.NOTSET.and.Idrsm.ne.NOTSET)THEN
         thisd=Idssm+Idrsm
        ELSE
         thisd=Bd+D
        ENDIF
c     ------------------------------------------------------------------
c     Remove LS outliers from series before differencing
c     ------------------------------------------------------------------
        CALL copy(Seattr(Pos1ob),Posfob-Pos1ob+1,1,stadtr(Pos1ob))
        IF(Adjls.eq.1)CALL divsub(stadtr,stadtr,Facls,Pos1ob,Posfob)
c     ------------------------------------------------------------------
        taklog=dpeq(Lam,ZERO)
        CALL gendff(stadtr,Pos1ob,Posfob,trdiff,tr1ob,taklog,T,thisd)
        IF(Prttab(LSEDTR))THEN
         CALL genSkip(LSEDTR)
         ndiff=Posfob-tr1ob+1
         CALL addate(Begspn,Ny,tr1ob-Pos1ob,idate)
         CALL makttl(DSEDIC,dseptr,PDSE,LSEDTR,PDSUM10,tblttl,ntbttl,
     &               T,F)
         IF(.not.Lfatal)CALL prtshd(tblttl(1:ntbttl),idate,Ny,ndiff)
         IF(.not.Lfatal)CALL prttbl(idate,Ny,trdiff(tr1ob),ndiff,'Data',
     &                              outdec,tbxdic(LSEDTR))
        END IF        
        IF(.not.Lfatal.and.Savtab(LSEDTR))
     &     CALL punch(trdiff,tr1ob,Posfob,LSEDTR,F,F)
        IF(.not.Lfatal.and.Lgraf)
     &     CALL punch(trdiff,tr1ob,Posfob,LSEDTR,Lgraf,F)
        IF(Lfatal)RETURN
       END IF
C-----------------------------------------------------------------------
C      Save trend series stochastic forecasts
C-----------------------------------------------------------------------
       IF(Havftr)THEN
        IF(Savtab(LSEFCD))
     &     CALL savtbl(LSEFCD,begfct,1,Nsftr,Ny,Setftr,Serno,Nser,F)
        IF(.not.Lfatal.and.Lgraf)
     &     CALL savtbl(LSEFCD,begfct,1,Nsftr,Ny,Setftr,Serno,Nser,Lgraf)
        IF(Lfatal)RETURN
       END IF
      END IF
C-----------------------------------------------------------------------
      IF(Haveir)THEN
c     ------------------------------------------------------------------
       IF(Prttab(LSESE3).or.Savtab(LSESE3).or.Lgraf)THEN
        CALL copy(Seatir,nlast,-1,iroadj)
        IF((.not.Finao).and.Adjao.eq.1)
     &    CALL divsub(iroadj,iroadj,Facao,Pos1bk,nlast)
       END IF
c     ------------------------------------------------------------------
       IF(Prttab(LSEIRR).or.Prttab(LSEPSI))
     &    CALL table(Seatir,Pos1ob,Posfob,13,1,1,dvec,LSEIRR)
       IF(.not.Lfatal.and.Savtab(LSEIRR))
     &    CALL punch(Seatir,frstob,lastob,LSEIRR,F,F)
       IF(.not.Lfatal.and.Savtab(LSEPSI))
     &    CALL punch(Seatir,frstob,lastob,LSEPSI,F,Muladd.ne.1)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(Seatir,frstob,lastob,LSEIRR,Lgraf,F)
       IF(Lfatal)RETURN
c     ------------------------------------------------------------------
       IF(Prttab(LSESE3))
     &    CALL table(iroadj,Pos1ob,Posfob,13,1,1,dvec,LSESE3)
       IF(.not.Lfatal.and.Savtab(LSESE3))
     &    CALL punch(iroadj,frstob,lastob,LSESE3,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(iroadj,frstob,lastob,LSESE3,Lgraf,F)
       IF(Lfatal)RETURN
      END IF
C-----------------------------------------------------------------------
      IF(Havecy)THEN
       IF(Prttab(LSETRA).or.Prttab(LSEPSC))
     &    CALL table(Seatcy,Pos1ob,Posfob,14,1,1,dvec,LSETRA)
       IF(.not.Lfatal.and.Savtab(LSETRA))
     &    CALL punch(Seatcy,frstob,lastob,LSETRA,F,F)
       IF(.not.Lfatal.and.Savtab(LSEPSC))
     &    CALL punch(Seatcy,frstob,lastob,LSEPSC,F,Muladd.ne.1)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(Seatcy,frstob,lastob,LSETRA,Lgraf,F)
       IF(Lfatal)RETURN
C-----------------------------------------------------------------------
c      save transitory se
C-----------------------------------------------------------------------
       IF(Hvcyse)THEN
        IF(Savtab(LSESCY))CALL punch(Stcyse,frstob,Lstyse,LSESCY,F,F)
        IF(Lgraf)CALL punch(Stcyse,frstob,Lstyse,LSESCY,Lgraf,F)
       END IF
C-----------------------------------------------------------------------
C      Save transitory series stochastic forecasts
C-----------------------------------------------------------------------
       IF(Hvstfc)THEN
        i=LSEFCD+4
        CALL addate(Endspn,Ny,1,idate)
        IF(Savtab(i))
     &     CALL savtbl(i,begfct,1,Nsfcy,Ny,Setfcy,Serno,Nser,F)
        IF(.not.Lfatal.and.Lgraf)
     &     CALL savtbl(i,begfct,1,Nsfcy,Ny,Setfcy,Serno,Nser,Lgraf)
        IF(Lfatal)RETURN
       END IF
      END IF
      IF(Hvscyc)THEN
       IF(Prttab(LSECYC).or.Prttab(LSECYC))
     &    CALL table(Setcyc,Pos1ob,Posfob,14,2,1,dvec,LSECYC)
       IF(.not.Lfatal.and.Savtab(LSECYC))
     &    CALL punch(Setcyc,frstob,lastob,LSECYC,F,F)
       IF(Lfatal)RETURN
      END IF
      IF(Hvsltt)THEN
       IF(Prttab(LSELTT).or.Prttab(LSELTT))
     &    CALL table(Setltt,Pos1ob,Posfob,14,3,1,dvec,LSELTT)
       IF(.not.Lfatal.and.Savtab(LSELTT))
     &    CALL punch(Setltt,frstob,lastob,LSELTT,F,F)
       IF(Lfatal)RETURN
      END IF
C-----------------------------------------------------------------------
      IF(Haveaf)THEN
*       IF(Muladd.ne.1)THEN
*        DO i=Pos1ob,Posffc
*         Seataf(i)=Seataf(i)/100D0
*        END DO
*       END IF
       IF(Prttab(LSECAF).or.Prttab(LSEPSI))
     &    CALL table(Seataf,Pos1ob,Posfob,16,1,1,dvec,LSECAF)
       IF(.not.Lfatal.and.Savtab(LSECAF))
     &    CALL punch(Seataf,frstob,lastob,LSECAF,F,F)
       IF(.not.Lfatal.and.Savtab(LSEPSI))
     &    CALL punch(Seataf,frstob,lastob,LSEPSI,F,Muladd.ne.1)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(Seataf,frstob,lastob,LSECAF,Lgraf,F)
       IF(Lfatal)RETURN
      END IF
C-----------------------------------------------------------------------
c     Print Final adjustment ratios - A1 / D11.
c-----------------------------------------------------------------------
      IF(Havesa)THEN
       i=Pos1ob
       pre18b=F
       DO WHILE (i.le.nlast)
        IF(dpeq(Seatsa(i),ZERO))THEN
         IF(dpeq(Series(i),ZERO))THEN
          stcirb(i)=ONE
         ELSE
          stcirb(i)=DNOTST
          IF(.not.pre18b)pre18b=T
         END IF
        ELSE
         IF(dpeq(Series(i),ZERO).or.Series(i).lt.ZERO)pre18b=T
         stcirb(i)=Series(i)/Seatsa(i)
        END IF
        i=i+1
       END DO
       IF(Prttab(LSES18))
     &    CALL table(stcirb,Pos1ob,Posfob,18,1,1,dvec,LSES18)
       IF(.not.Lfatal.and.Savtab(LSES18))
     &    CALL punch(stcirb,frstob,lastob,LSES18,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(stcirb,frstob,lastob,LSES18,Lgraf,F)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c    Print/Save total adjustment factors (BCM - July 2005)
c-----------------------------------------------------------------------
       IF(pre18b)THEN
        CALL divsub(stcirb,Series,Seatsa,Pos1ob,nlast)
        IF(Prttab(LSESEB))
     &     CALL table(stcirb,Pos1ob,Posfob,18,2,1,dvec,LSESEB)
        IF(.not.Lfatal.and.Savtab(LSESEB))
     &     CALL punch(stcirb,frstob,lastob,LSESEB,F,F)
        IF(.not.Lfatal.and.Lgraf)
     &     CALL punch(stcirb,frstob,lastob,LSESEB,Lgraf,F)
        IF(Lfatal)RETURN
       END IF
      END IF
C-----------------------------------------------------------------------
C      Save original series stochastic forecasts
C-----------------------------------------------------------------------
      IF(Hvstfo)THEN
       i=LSEFCD+2
       CALL addate(Endspn,Ny,1,idate)
       IF(Savtab(i))
     &    CALL savtbl(i,idate,1,Nfcfor,Ny,Stcfor,Serno,Nser,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL savtbl(i,idate,1,Nfcfor,Ny,Stcfor,Serno,Nser,Lgraf)
       IF(Lfatal)RETURN
      END IF
C-----------------------------------------------------------------------
      Kpart=kp2
      RETURN
      END
