C     Last change: LS Nov. 1 2022- saved .b1 file
C     previous change:  BCM  20 May 1999    8:58 am
      SUBROUTINE x11pt2(Lmodel,Lx11,Lseats,Lgraf,Lgrfxr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Performs B, C, and D (up to table D7) iterations of X-11 seasonal
c     adjustment method
c-----------------------------------------------------------------------
      DOUBLE PRECISION ONE,ZERO
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.,ONE=1D0,ZERO=0D0)
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11fac.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'cmptbl.i'
      INCLUDE 'mdltbl.i'
      INCLUDE 'x11tbl.i'
      INCLUDE 'xrgtbl.i'
      INCLUDE 'error.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'inpt.cmn'
      INCLUDE 'x11msc.cmn'
      INCLUDE 'x11reg.cmn'
      INCLUDE 'x11log.cmn'
      INCLUDE 'x11srs.cmn'
      INCLUDE 'xrgum.cmn'
      INCLUDE 'agr.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'xtrm.cmn'
      INCLUDE 'orisrs.cmn'
      INCLUDE 'tdtyp.cmn'
      INCLUDE 'ssap.prm'
      INCLUDE 'ssap.cmn'
c-----------------------------------------------------------------------
      CHARACTER pristr*(17),trnchr*1
      LOGICAL gudrun,Lx11,Lseats,Lmodel,Lgraf,goodlm,oktrn,chkfct,Lgrfxr
      DOUBLE PRECISION Stex,Temp,dvec,Lam,dtemp,rbar,adjtmp
      INTEGER Fcntyp,i,posfex,pos1ex,kersa1,ksdev1,khclda,klda,lfd,lld,
     &        fext,mfd1,mfda,mlda,iv,ksav,ny2,n2,lsthol,ntype,npri,
     &        indhol,fexp,bsav
      DIMENSION dtemp(PLEN),dvec(1),Stex(PLEN),Temp(PLEN),
     &          trnchr(PLEN),adjtmp(PLEN)
c-----------------------------------------------------------------------
c      INTEGER mqu,Iwt,Kexopt
c      COMMON /oldopt/ Iwt,Kexopt
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      COMMON /work  / Temp
      COMMON /mq10  / Stex
      COMMON /armalm/ Lam,Fcntyp
c-----------------------------------------------------------------------
      rbar=1D0
      IF(Muladd.eq.1)rbar=0D0
      dvec(1)=ZERO
      Length=Posfob-Pos1ob+1
      ny2=Ny/2
      kersa1=Kersa
      ksdev1=Ksdev
      goodlm=dpeq(Lam,0D0).or.dpeq(Lam,1D0)
      gudrun=Issap.lt.2.and.Irev.lt.4.and.(Khol.ne.1)
c-----------------------------------------------------------------------
c     If saving xdg file, save prior adjustment information here.
c-----------------------------------------------------------------------
      IF(Lsumm.gt.0.and.gudrun)THEN
       IF(Kfmt.gt.0.and.Ixreg.ne.2)THEN
        pristr=' '
        npri=0
        IF(Priadj.eq.2)THEN
         pristr((npri+1):(npri+4))=' lom'
         npri=4
        ELSE IF(Priadj.eq.3) THEN
         pristr((npri+1):(npri+4))=' loq'
         npri=4
        ELSE IF(Priadj.eq.4)THEN
         pristr((npri+1):(npri+7))=' lpyear'
         npri=7
        END IF
        IF(Nprtyp.gt.0)THEN
         DO i=1,Nprtyp
          IF(Prtype(i).eq.1)THEN
           pristr((npri+1):(npri+5))=' temp'
          ELSE
           pristr((npri+1):(npri+5))=' perm'
          END IF
          npri=npri+5
         END DO
        END IF
        WRITE(Nform,1650)pristr
       ELSE
        WRITE(Nform,1650)' none'
       END IF
      END IF
 1650 FORMAT('prioradj:',a)
c-----------------------------------------------------------------------
c     Adjust series and prior effects when model based trading day and
c     lom adjustment are done together.
c-----------------------------------------------------------------------
      IF(Nfcst.lt.Ny)THEN
       n2=Posfob+Ny
      ELSE
       n2=Posffc
      END IF
      IF(Ixreg.ne.2.AND.Priadj.gt.1.and.goodlm)THEN
       CALL makadj(adjtmp,Muladd)
       IF(Nflwtd.gt.0)THEN
        CALL tdlom(Stcsi,Stocal,Sprior,Factd,Pos1bk,n2,Muladd,Adjtd,
     &             adjtmp)
c-----------------------------------------------------------------------
c     If flow trading day not in model and prior adjustment for
c     lom or loq or leap year is done, remove that effect from Stocal
c     (BCM, Oct 2009)
c-----------------------------------------------------------------------
       ELSE
        CALL divsub(Stocal,Stocal,Sprior,Pos1bk,n2)
        CALL addmul(Stocal,Stocal,adjtmp,Pos1bk,n2)
       END IF
      END IF
c-----------------------------------------------------------------------
c     Store regression trading day factors for sliding spans analysis
c-----------------------------------------------------------------------
      IF(Issap.eq.2)THEN
       IF(Itd.eq.1)THEN
        IF(Adjtd.eq.1)THEN
         CALL ssrit(Factd,Pos1ob,Posfob,1,Series)
        ELSE
         Itd=0
         IF(Axrgtd)Itd=1
        END IF
       END IF
c-----------------------------------------------------------------------
c     Check to see if holiday adjustment done of series is done during
c     sliding spans
c-----------------------------------------------------------------------
       IF((.not.(Adjhol.eq.1.or.Finhol)).and.
     &    (Ihol.eq.1.and.Khol.eq.0))Ihol=0
      END IF
c-----------------------------------------------------------------------
c     Print model based adjustment factors
c-----------------------------------------------------------------------
      khclda=Posffc-Nfdrp
      IF(Nfdrp.eq.0)khclda=Posfob
      ksav=khclda
      IF(Savfct)ksav=n2
      bsav=Pos1ob
      IF(Savbct)bsav=Pos1bk
      IF(Adjtd.eq.1.and.goodlm)THEN
       IF(Prttab(LREGTD))
     &    CALL table(Factd,Pos1ob,khclda,6,1,1,dvec,LREGTD)
       IF(.not.Lfatal.and.Savtab(LREGTD))
     &    CALL punch(Factd,bsav,ksav,LREGTD,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(Factd,bsav,ksav,LREGTD,Lgraf,F)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c      Combine with Calendar effect factors  (BCM May 1999)
c-----------------------------------------------------------------------
       CALL addmul(Faccal,Faccal,Factd,Pos1bk,n2)
c-----------------------------------------------------------------------
      END IF
c-----------------------------------------------------------------------
      IF((Adjhol.eq.1.or.(Finhol.and.Nhol.gt.0)).and.Khol.ne.1.and.
     &   goodlm.and.Lmodel.AND.(.not.Axrghl))THEN
       IF(Prttab(LRGHOL))
     &    CALL table(Fachol,Pos1ob,khclda,7,1,1,dvec,LRGHOL)
       IF(.not.Lfatal.and.Savtab(LRGHOL))
     &    CALL punch(Fachol,bsav,ksav,LRGHOL,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(Fachol,bsav,ksav,LRGHOL,Lgraf,F)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c      Combine with Calendar effect factors  (BCM May 1999)
c-----------------------------------------------------------------------
       CALL addmul(Faccal,Faccal,Fachol,Pos1bk,n2)
      END IF
c-----------------------------------------------------------------------
      ntype=0
      IF(((Adjao.eq.1.or.Finao).or.(Adjls.eq.1.or.Finls).or.
     &    (Adjtc.eq.1.or.Fintc).or.Adjso.eq.1).and.goodlm)THEN
       CALL setdp(rbar,PLEN,dtemp)
       IF(Adjao.eq.1.or.Finao)THEN
        CALL addmul(dtemp,dtemp,Facao,Pos1ob,Posffc)
        ntype=ntype+1
       END IF
       IF(Adjls.eq.1.or.Finls)THEN
        CALL addmul(dtemp,dtemp,Facls,Pos1ob,Posffc)
        ntype=ntype+1
       END IF
       IF(Adjtc.eq.1.or.Fintc)THEN
        CALL addmul(dtemp,dtemp,Factc,Pos1ob,Posffc)
        ntype=ntype+1
       END IF
       IF(Adjso.eq.1)THEN
        CALL addmul(dtemp,dtemp,Facso,Pos1ob,Posffc)
        ntype=ntype+1
       END IF
       IF(Prttab(LRGOTL).or.(ntype.eq.1.AND.
     &   (((Adjao.eq.1.or.Finao).and.Prttab(LREGAO)).or.
     &    ((Adjls.eq.1.or.Finls).and.Prttab(LREGLC)).or.
     &    ((Adjtc.eq.1.or.Fintc).and.Prttab(LREGTC)).or.
     &    (Adjso.eq.1.and.Prttab(LREGSO)))))
     &    CALL table(dtemp,Pos1ob,khclda,8,1,1,dvec,LRGOTL)
       IF(.not.Lfatal.and.Savtab(LRGOTL))
     &    CALL punch(dtemp,bsav,ksav,LRGOTL,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(dtemp,bsav,ksav,LRGOTL,Lgraf,F)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
      IF((Adjao.eq.1.or.Finao).and.goodlm)THEN
       IF(Prttab(LREGAO).and.ntype.gt.1)
     &    CALL table(Facao,Pos1ob,Posfob,8,2,1,dvec,LREGAO)
       IF(.not.Lfatal.and.Savtab(LREGAO))
     &    CALL punch(Facao,Pos1ob,Posfob,LREGAO,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(Facao,Pos1ob,Posfob,LREGAO,Lgraf,F)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
      IF((Adjls.eq.1.or.Finls).and.goodlm)THEN
       IF(Prttab(LREGLC).and.ntype.gt.1)
     &    CALL table(Facls,Pos1ob,Posfob,8,3,1,dvec,LREGLC)
       IF(.not.Lfatal.and.Savtab(LREGLC))
     &    CALL punch(Facls,Pos1ob,Posfob,LREGLC,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(Facls,Pos1ob,Posfob,LREGLC,Lgraf,F)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
      IF((Adjtc.eq.1.or.Fintc).and.goodlm)THEN
       IF(Prttab(LREGTC).and.ntype.gt.1)
     &    CALL table(Factc,Pos1ob,khclda,8,4,1,dvec,LREGTC)
       IF(.not.Lfatal.and.Savtab(LREGTC))
     &    CALL punch(Factc,bsav,ksav,LREGTC,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(Factc,bsav,ksav,LREGTC,Lgraf,F)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
      IF(Adjso.eq.1.and.goodlm)THEN
       IF(Prttab(LREGSO).and.ntype.gt.1)
     &    CALL table(Facso,Pos1ob,Posfob,8,5,1,dvec,LREGSO)
       IF(.not.Lfatal.and.Savtab(LREGSO))
     &    CALL punch(Facso,Pos1ob,Posfob,LREGSO,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(Facso,Pos1ob,Posfob,LREGSO,Lgraf,F)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
      IF((Adjusr.eq.1.or.Finusr).and.goodlm)THEN
       IF(Prttab(LRGUSR))
     &    CALL table(Facusr,Pos1ob,khclda,9,1,1,dvec,LRGUSR)
       IF(.not.Lfatal.and.Savtab(LRGUSR))
     &    CALL punch(Facusr,bsav,ksav,LRGUSR,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(Facusr,bsav,ksav,LRGUSR,Lgraf,F)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
      IF(Adjsea.eq.1.and.goodlm)THEN
       IF(Prttab(LRGA10))
     &    CALL table(Facsea,Pos1ob,khclda,10,1,1,dvec,LRGA10)
       IF(.not.Lfatal.and.Savtab(LRGA10))
     &    CALL punch(Facsea,bsav,ksav,LRGA10,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(Facsea,bsav,ksav,LRGA10,Lgraf,F)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c   Print out regARIMA transitory component for SEATS adjustments
c   (Aug 2004 - BCM)
c-----------------------------------------------------------------------
      IF(Adjcyc.eq.1.and.goodlm.and.Lseats)THEN
       IF(Prttab(LRGA13))
     &    CALL table(Faccyc,Pos1ob,khclda,13,1,1,dvec,LRGA13)
       IF(.not.Lfatal.and.Savtab(LRGA13))
     &    CALL punch(Faccyc,bsav,ksav,LRGA13,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(Faccyc,bsav,ksav,LRGA13,Lgraf,F)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c     Print combined holiday effect
c-----------------------------------------------------------------------
      IF(.not.Noxfac)THEN
       indhol=0
       IF(Khol.eq.2)indhol=indhol+1
       IF(Ixreg.gt.2.and.Axrghl.and.(.not.Haveum))indhol=indhol+1
       IF(Adjhol.eq.1)indhol=indhol+1
       IF(indhol.gt.0.and.goodlm)THEN
        IF(Nfcst.eq.0)THEN
         lsthol=Posfob+Ny
        ELSE
         lsthol=Posffc
        END IF
        IF(Ixreg.gt.2.and.Axrghl)
     &     CALL addmul(Fachol,Fachol,Facxhl,Pos1bk,lsthol)
        IF(Khol.eq.2)CALL addmul(Fachol,Fachol,X11hol,Pos1bk,lsthol)
        IF(Prttab(LXECHL).and.indhol.gt.1)
     &     CALL table(Fachol,Pos1ob,khclda,16,1,1,dvec,LXECHL)
        IF(.not.Lfatal.and.Savtab(LXECHL))
     &     CALL punch(Fachol,bsav,ksav,LXECHL,F,F)
        IF(.not.Lfatal.and.Lgraf)
     &     CALL punch(Fachol,bsav,ksav,LXECHL,Lgraf,F)
        IF(Lfatal)RETURN
       END IF
      END IF
c-----------------------------------------------------------------------
c     Generate and print the calendar adjusted original series
c-----------------------------------------------------------------------
      IF(Adjhol.eq.1.or.Adjtd.eq.1.or.Khol.eq.2.or.
     &  (Ixreg.gt.2.and.(Axrgtd.or.Axrghl)).or.
     &   Ixreg.ne.2.and.Priadj.gt.1.and.goodlm)THEN
       fexp=LSRA18
       IF(Iagr.eq.3)fexp=LCPA18
*       CALL divsub(Stocal,Series,Faccal,Pos1ob,Posfob)
       IF(Prttab(fexp))CALL table(Stocal,Pos1ob,Posfob,18,1,2,dvec,fexp)
       IF(.not.Lfatal.and.Savtab(fexp))
     &    CALL punch(Stocal,bsav,ksav,fexp,F,F)
*     &    CALL punch(Stocal,Pos1ob,Posfob,fexp,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(Stocal,bsav,ksav,fexp,Lgraf,F)
*     &    CALL punch(Stocal,Pos1ob,Posfob,fexp,Lgraf,F)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c     Generate and print the outlier adjusted original series
c-----------------------------------------------------------------------
      fexp=LSRA19
      IF(Iagr.eq.3)fexp=LCPA19
      IF((Savtab(fexp).or.Prttab(fexp).or.Lgraf).and.
     &  (Adjls.eq.1.or.Adjtc.eq.1.or.Adjao.eq.1.or.Adjso.eq.1))THEN
       IF(Prttab(fexp))
     &    CALL table(Temp,Pos1ob,Posfob,19,1,2,dvec,fexp)
       IF(.not.Lfatal.and.Savtab(fexp))
     &    CALL punch(Temp,Pos1ob,Posfob,fexp,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(Temp,Pos1ob,Posfob,fexp,Lgraf,F)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
C --- PART B
c-----------------------------------------------------------------------
      Kpart=2
      Ksect=1
c-----------------------------------------------------------------------
C --- WRITE ORIGINAL OR PRIOR ADJUSTED B1.
C      IF (IFORC.NE.0.AND.LENGTH.LE.IFTNY.AND.KEBACK.EQ.0) LYR=LYR-1
c-----------------------------------------------------------------------
      IF(.NOT.(Ixreg.eq.2.or.Khol.eq.1))THEN
       fexp=LSRSB1
       IF(Iagr.eq.3)fexp=LCMPB1
       IF(Prttab(fexp).AND.(Lmodel.or.Kfmt.gt.0.or.Kswv.gt.0.or.
     &    Khol.eq.2.or.Ixreg.ge.3))
     &   CALL table(Stcsi,Pos1bk,khclda,1,1,2,dvec,fexp)
c     only print to the number of forecast if specified
       IF(.not.Lfatal.and.Savtab(fexp)) then
        IF(Savfct) then
            CALL punch(Stcsi,bsav,Posffc,fexp,F,F)
        else
            CALL punch(Stcsi,bsav,Posfob,fexp,F,F)
        end if
       end if
       IF(.not.Lfatal.and.Lgraf) then
        IF(Savfct) then
            CALL punch(Stcsi,bsav,Posffc,fexp,Lgraf,F)
        else
            CALL punch(Stcsi,bsav,Posfob,fexp,Lgraf,F)
        end if
       end if
       fexp=LSRB1P
       IF(Iagr.eq.3)fexp=LCPB1P
       IF(.not.Lfatal.and.Prttab(fexp))
     &   CALL x11plt(Stcsi,Stcsi,Pos1bk,Posffc,fexp,0,0,6,1)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c     If X-11 Seasonal adjustment options not specified, return
c-----------------------------------------------------------------------
      IF(.not.Lx11)RETURN
c-----------------------------------------------------------------------
C      IF(Nfcst.ne.0)THEN
C       llaf=Posffc-Nfcst+1
C       CALL copy(Stcsi(llaf),Posffc-llaf+1,1,Series(llaf))
C       llaf=Pos1ob-1
C       CALL copy(Stcsi(Pos1bk),llaf-Pos1bk+1,1,Series(Pos1bk))
C       tmp=ONE
C       IF(Muladd.eq.1)tmp=ZERO
C       CALL setdp(tmp,Posffc-Posfob,Sprior(Posfob+1))
C      END IF
c-----------------------------------------------------------------------
C --- TAKE LOGARITHM OF B1 IF THE LOGARITHMIC MODEL IS SELECTED.
c-----------------------------------------------------------------------
      Muladd=Tmpma
      IF(Muladd.eq.2)CALL logar(Stcsi,Pos1bk,Posffc)
      CALL copy(Stcsi(Pos1bk),Posffc-Pos1bk+1,1,Sto(Pos1bk))
      IF(.not.(Kswv.eq.0.or.Nfcst.eq.0.or.Adjtd.eq.1))THEN
       DO i=1,Ny
        Series(Posfob+i)=Series(Posfob+i)*Stptd(Posfob+i)
       END DO
      END IF
c-----------------------------------------------------------------------
C --- APPLY A CENTERED NY-TERM MOVING AVERAGE.
c-----------------------------------------------------------------------
*      IF(Ny.eq.12)mqu=1
*      IF(Ny.eq.4)mqu=2
      DO WHILE (T)
c       IF(Iwt.eq.1)CALL weight(Stcsi,Stc,Pos1bk,Posffc,mqu)
c       IF(Iwt.eq.0)CALL averag(Stcsi,Stc,Pos1bk,Posffc,2,Ny)
       CALL averag(Stcsi,Stc,Pos1bk,Posffc,2,Ny)
       mfda=Pos1bk+ny2
       mfd1=Pos1ob+ny2
       mlda=Posffc-ny2
       klda=Posfob-ny2
c-----------------------------------------------------------------------
C --- DIVIDE (SUBTRACT) ORIGINAL BY TREND CYCLE FOR SI RATIOS.
c-----------------------------------------------------------------------
       CALL divsub(Stsi,Stcsi,Stc,mfda,mlda)
c-----------------------------------------------------------------------
C --- SECTION 1. APPLY AN F-TEST TO THE SI TO TEST FOR THE PRESENCE
C --- OF SEASONALITY.
c-----------------------------------------------------------------------
c       IF(Prttab(LXEB1F).and.
       IF(Ksect.eq.1.and.Kpart.eq.2.and.Ixreg.ne.2.and.Khol.ne.1)
     &    CALL ftest(Stsi,mfd1,klda,Ny,2,Prttab(LXEB1F),F)
       lfd=Pos1ob+(Ny/2)
       lld=Posfob-(Ny/2)
       fext=LXEITN+Kpart-2
       IF(Prttab(fext))THEN
c-----------------------------------------------------------------------
C --- WRITE TREND CYCLE B2,C2,D2.
c-----------------------------------------------------------------------
        IF(Nfcst.ne.0)THEN
         CALL table(Stc,lfd,Posfob,2,1,2,dvec,fext)
        ELSE
         CALL table(Stc,lfd,lld,2,1,2,dvec,fext)
        END IF
        IF(Lfatal)RETURN
       END IF
c-----------------------------------------------------------------------
C --- SAVE TREND CYCLE B2,C2,D2.
c-----------------------------------------------------------------------
       IF(Savtab(fext))THEN
        IF(Nfcst.ne.0)THEN
         CALL punch(Stc,lfd,Posfob,fext,F,F)
        ELSE
         CALL punch(Stc,lfd,lld,fext,F,F)
        END IF
        IF(Lfatal)RETURN
       END IF
       IF(Kpart.eq.2)THEN
c-----------------------------------------------------------------------
C --- PART B. REPLACE EXTREME SI RATIOS (DIFFERENCES)
c-----------------------------------------------------------------------
        posfex=mlda
        pos1ex=mfda
        IF(Noxfct)THEN
         posfex=klda
         pos1ex=mfd1
        END IF
        CALL si(Ksect,mfda,mlda,Ny,Nfcst,Nbcst,kersa1,ksdev1,Pos1ob,
     &          Posfob,Kfulsm,pos1ex,posfex)
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
C --- PART C AND D. COMPUTE A 5-TERM MOVING AVERAGE OF EACH MONTH
C --- (QUARTER) FOR ESTIMATE OF SEASONAL FACTORS.
c-----------------------------------------------------------------------
       ELSE
        IF(Kfulsm.lt.2)CALL vsfb(Sts,Stsi,mfda,mlda,Ny)
c-----------------------------------------------------------------------
C --- WRITE MODIFIED SI RATIOS (DIFFERENCES) C4 AND D4.
c-----------------------------------------------------------------------
        fext=LXEIMS+Kpart-3
        IF(Prttab(fext))THEN
         IF(Nfcst.ne.0)THEN
          IF(Nbcst.eq.0)THEN
           CALL table(Stsi,lfd,Posfob,4,1,1,dvec,fext)
          ELSE
           CALL table(Stsi,Pos1ob,Posfob,4,1,1,dvec,fext)
          END IF
         ELSE
          CALL table(Stsi,lfd,lld,4,1,1,dvec,fext)
         END IF
         IF(Lfatal)RETURN
        END IF
        IF(Savtab(fext))THEN
         IF(Nfcst.ne.0)THEN
          IF(Nbcst.eq.0)THEN
           CALL punch(Stsi,lfd,Posfob,fext,F,F)
          ELSE
           CALL punch(Stsi,Pos1ob,Posfob,fext,F,F)
          END IF
         ELSE
          CALL punch(Stsi,lfd,lld,fext,F,F)
         END IF
         IF(Lfatal)RETURN
        END IF
       END IF
c-----------------------------------------------------------------------
C --- REPLACE MISSING VALUES AT EACH END DUE TO CENTERED NY-TERM MOVING
C --- AVERAGE.
c-----------------------------------------------------------------------
       CALL forcst(Sts,mfda,mlda,Posffc,Ny,1,ZERO,ONE)
c-----------------------------------------------------------------------
C --- DIVIDE (SUBTRACT) THE ORIGINAL SERIES BY THE SEASONAL FACTORS TO
C --- GET A PRELIMINARY ESTIMATE OF THE SEASONALLY ADJUSTED SERIES.
c-----------------------------------------------------------------------
       IF(Psuadd)THEN
        DO i=Pos1bk,Posffc
         IF(i.lt.mfda.or.i.gt.mlda)THEN
          IF(dpeq(Sts(i),ZERO))THEN
           IF(.not.Lhiddn)THEN
            CALL eWritln('Initial seasonal factor is equal to zero - '//
     &                   'cannot continue with',STDERR,Mt2,T,F)
            CALL writln('        pseudo-additive seasonal adjustment.',
     &                  STDERR,Mt2,F,T)
           END IF
           CALL abend
           RETURN
          ELSE
           Stci(i)=Stcsi(i)/Sts(i)
          END IF
         ELSE
          Stci(i)=Stcsi(i)-Stc(i)*(Sts(i)-ONE)
         END IF
        END DO
       ELSE
        CALL divsub(Stci,Stcsi,Sts,Pos1bk,Posffc)
       END IF
c-----------------------------------------------------------------------
C --- WRITE SEASONAL FACTORS B5,C5, AND D5.
c-----------------------------------------------------------------------
       IF(Kfulsm.lt.2)THEN
        fext=LXEISF+Kpart-2
        IF(Prttab(fext))CALL table(Sts,Pos1ob,Posfob,5,1,1,dvec,fext)
        IF(.not.Lfatal.and.Savtab(fext))
     &     CALL punch(Sts,Pos1ob,Posfob,fext,F,F)
        IF(Lfatal)RETURN
       END IF
c-----------------------------------------------------------------------
C --- WRITE SEASONALLY ADJUSTED SERIES B6,C6, AND D6.
c-----------------------------------------------------------------------
       fext=LXEISA+Kpart-2
       IF(Prttab(fext))CALL table(Stci,Pos1ob,Posfob,6,1,2,dvec,fext)
       IF(.not.Lfatal.and.Savtab(fext))
     &    CALL punch(Stci,Pos1ob,Posfob,fext,F,F)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
C --- SECTION 2
c-----------------------------------------------------------------------
       Ksect=2
c-----------------------------------------------------------------------
C --- APPLY VARIABLE TREND-CYCLE ROUTINE TO SEASONALLY ADJUSTED SERIES.
c-----------------------------------------------------------------------
       CALL vtc(Stc,Stci)
c       IF(Kexopt.eq.1.and.Kpart.eq.2)THEN
c-----------------------------------------------------------------------
C --- MODIFY CI VALUES BEFORE TREND-CYCLE SELECTED IF THE STRIKE OPTION
C --- IS SELECTED.
c-----------------------------------------------------------------------
C --- DIVIDE SEASONALLY ADJUSTED SERIES BY THE TREND-CYCLE FOR AN
C --- ESTIMATE OF THE IRREGULAR.
c-----------------------------------------------------------------------
c        CALL divsub(Sti,Stci,Stc,Pos1bk,Posffc)
c-----------------------------------------------------------------------
C --- IDENTIFY EXTREME IRREGULAR VALUES.
c-----------------------------------------------------------------------
c        pos1ex=Pos1bk
c        posfex=Posffc
c        IF(Noxfct)THEN
c         pos1ex=Pos1ob
c         posfex=Posfob
c        END IF
c        CALL xtrm(Sti,Pos1bk,Posffc,pos1ob,posfex)
c-----------------------------------------------------------------------
C --- REPLACE EXTREME VALUES.
c-----------------------------------------------------------------------
c        CALL replac(Stci,Temp,Stwt,Pos1bk,Posfob,1)
c-----------------------------------------------------------------------
C --- APPLY THE VARIABLE TREND-CYCLE ROUTINE TO THE MODIFIED CI VALUES.
c-----------------------------------------------------------------------
c        CALL vtc(Stc,Stci)
c       END IF
c-----------------------------------------------------------------------
c     For multiplicative seasonal adjustment, check to see if any
c     of the trend values are negative.
c-----------------------------------------------------------------------
       oktrn=T
       IF(Muladd.eq.0)THEN
        chkfct=F
        CALL chktrn(Stc,Kpart,7,trnchr,chkfct,oktrn)
       END IF
c-----------------------------------------------------------------------
C --- WRITE THE TREND-CYCLE B7,C7, AND D7.
c-----------------------------------------------------------------------
       fext=LXEPTN+Kpart-2
       IF(Prttab(fext))THEN
        IF(oktrn)THEN
         CALL table(Stc,Pos1ob,Posfob,7,1,2,dvec,fext)
        ELSE
         CALL prttrn(Stc,trnchr,Pos1ob,Posfob,7,fext)
        END IF
       END IF
       IF(.not.Lfatal.and.Savtab(fext))
     &    CALL punch(Stc,Pos1ob,Posfob,fext,F,F)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
C --- DIVIDE B1 BY B7,CI BY C7, AND D1 BY D7 TO OBTAIN SI RATIOS.
c-----------------------------------------------------------------------
       CALL divsub(Stsi,Stcsi,Stc,Pos1bk,Posffc)
       IF(Kpart.eq.2)THEN
c-----------------------------------------------------------------------
C --- PART B. REPLACE EXTREME SI RATIOS (DIFFERENCES).
c-----------------------------------------------------------------------
        pos1ex=Pos1bk
        posfex=Posffc
        IF(Noxfct)THEN
         pos1ex=Pos1ob
         posfex=Posfob
        END IF
        CALL si(Ksect,Pos1bk,Posffc,Ny,Nfcst,Nbcst,kersa1,ksdev1,Pos1ob,
     &          Posfob,Kfulsm,pos1ex,posfex)
        IF(Lfatal)RETURN
       ELSE IF(Kpart.eq.4)THEN
c-----------------------------------------------------------------------
c     Variable seasonal routine moved to x11pt3 to correctly adjust for
c     extreme with calendarsigma options (BCM, 11/98)
c-----------------------------------------------------------------------
        RETURN
       ELSE
c-----------------------------------------------------------------------
C --- PART C. APPLY THE VARIABLE SEASONAL FACTOR ROUTINE FOR AN
C --- ESTIMATE OF THE SEASONAL FACTORS.
c-----------------------------------------------------------------------
        IF(Kfulsm.lt.2)THEN
         CALL vsfa(Stsi,Pos1bk,Posfob,Ny)
         CALL vsfb(Sts,Stsi,Pos1bk,Posffc,Ny)
        END IF
c-----------------------------------------------------------------------
C --- WRITE THE MODIFIED SI C9.
c-----------------------------------------------------------------------
        IF(Prttab(LX11C9))THEN
         CALL table(Stsi,Pos1ob,Posfob,9,1,1,dvec,LX11C9)
         IF(Lfatal)RETURN
        END IF
       END IF
c-----------------------------------------------------------------------
C --- OBTAIN A PRELIMINARY SEASONALLY ADJUSTED SERIES.
c-----------------------------------------------------------------------
       IF(Kfulsm.eq.2)THEN
        CALL copy(Sto,Posffc,1,Stci)
       ELSE
        IF(Psuadd)THEN
         DO i=Pos1bk,Posffc
          Stci(i)=Sto(i)-Stc(i)*(Sts(i)-ONE)
         END DO
        ELSE
         CALL divsub(Stci,Sto,Sts,Pos1bk,Posffc)
        END IF
       END IF
c-----------------------------------------------------------------------
C --- DIVIDE SEASONALLY ADJUSTED SERIES BY THE TREND CYCLE TO OBTAIN A
C --- PRELIMINARY IRREGULAR SERIES.
c-----------------------------------------------------------------------
       CALL divsub(Sti,Stci,Stc,Pos1bk,Posffc)
c-----------------------------------------------------------------------
C --- WRITE SEASONAL FACTORS B10 AND C10.
c-----------------------------------------------------------------------
       IF(Kfulsm.lt.2)THEN
        fext=LXEB10+Kpart-2
        IF(Prttab(fext))CALL table(Sts,Pos1ob,Posfob,10,1,1,dvec,fext)
        IF(.not.Lfatal.and.Savtab(fext))
     &     CALL punch(Sts,Pos1ob,Posfob,fext,F,F)
        IF(Lfatal)RETURN
       END IF
c-----------------------------------------------------------------------
C --- WRITE SEASONALLY ADJUSTED SERIES B11 AND C11.
c-----------------------------------------------------------------------
       fext=LXEPSA+Kpart-2
       IF(Prttab(fext))CALL table(Stci,Pos1ob,Posfob,11,1,2,dvec,fext)
       IF(.not.Lfatal.and.Savtab(fext))
     &    CALL punch(Stci,Pos1ob,Posfob,fext,F,F)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
C --- WRITE IRREGULAR SERIES B13 AND C13.
c-----------------------------------------------------------------------
       fext=LX11PI+Kpart-2
       IF(Prttab(fext))CALL table(Sti,Pos1ob,Posfob,13,1,3,Stdev,fext)
       IF(.not.Lfatal.and.Savtab(fext))
     &    CALL punch(Sti,Pos1ob,Posfob,fext,F,F)
       IF(Lfatal)RETURN
       IF(Ixreg.eq.1.or.Ixreg.eq.2)THEN
c-----------------------------------------------------------------------
C --- X-11 REGRESSION OPTION.
c-----------------------------------------------------------------------
C --- COMPUTE X-11 REGRESSION FACTORS IF TRADING DAY OPTION
C --- SELECTED.
c-----------------------------------------------------------------------
c       CALL traday(Tday)
C  ****  CHANGE BY B. C. MONSELL
        IF(Ixreg.eq.1)CALL loadxr(F)
        CALL x11mdl(Sti,Muladd,Tmpma,Psuadd,Kpart,Kswv,Lgrfxr)
        IF(Ixreg.eq.1)THEN
         CALL loadxr(T)
         IF(Lmodel)CALL restor(Lmodel,F,F)
        ELSE IF(Khol.eq.0.and.Kpart.eq.3)THEN
         RETURN
        END IF
        IF((Holgrp.eq.0.and.Tdgrp.eq.0.and.Stdgrp.eq.0).and.
     &      Kpart.eq.2)THEN
         Ixreg=0-Ixreg
         IF(Ixreg.eq.-2)RETURN
        END IF
       END IF
c-----------------------------------------------------------------------
C --- For B Iteration, do BUNDESBANK outlier test
c-----------------------------------------------------------------------
       IF(Kpart.eq.2.and.Ksdev.lt.4)THEN
        CALL vtest(Sti,iv,Pos1bk,Posfob)
        CALL entsch(kersa1,ksdev1,Kersa,Ksdev,iv)
       END IF
c-----------------------------------------------------------------------
C --- COMPUTE WEIGHTS FOR IRREGULAR COMPONENT.
c-----------------------------------------------------------------------
       pos1ex=Pos1bk
       posfex=Posffc
       IF(Noxfct)THEN
        pos1ex=Pos1ob
        posfex=Posfob
       END IF
       CALL xtrm(Sti,Pos1bk,Posffc,pos1ex,posfex)
c-----------------------------------------------------------------------
C --- WRITE WEIGHTS FOR IRREGULAR COMPONENT B17 OR C17.
c-----------------------------------------------------------------------
       fext=LX11IW+Kpart-2
       IF(Prttab(fext).AND.(Ixreg.ne.2.or.Kpart.eq.2))THEN
        IF(Ksdev.eq.0)THEN
         CALL table(Stwt,Pos1ob,Posfob,17,1,4,Stdev,fext)
        ELSE
         CALL table(Stwt,Pos1ob,Posfob,17,1,5,dvec,fext)
        END IF
       END IF
       IF(.not.Lfatal.and.Savtab(fext))
     &    CALL punch(Stwt,Pos1ob,Posfob,fext,F,F)
       IF(.not.Lfatal.and.Lgraf.and.Kpart.eq.3)
     &    CALL punch(Stwt,Pos1ob,Posfob,fext,Lgraf,F)
       IF(Lfatal)RETURN
       IF(Kpart.eq.3.and.Lsumm.gt.0.and.gudrun.and.Ksdev.gt.0.and.
     &    Ixreg.ne.2.and.Khol.ne.1)THEN
        DO i=1,Ny
         WRITE(Nform,1010)i,Stdper(i)
        END DO
 1010   FORMAT('calendarsigma.',i2.2,': ',f10.2)
       END IF
c-----------------------------------------------------------------------
      IF(Nfcst.gt.0.and.Kpart.eq.3.and.(.not.Noxfct))THEN
       IF(Prttab(LXEIWF).AND.Ixreg.ne.2)THEN
        IF(Ksdev.eq.0)THEN
         CALL table(Stwt,Posfob+1,Posffc,17,2,4,Stdev,LXEIWF)
        ELSE
         CALL table(Stwt,Posfob+1,Posffc,17,2,5,dvec,LXEIWF)
        END IF
       END IF
       IF((.not.Lfatal).and.Savtab(LXEIWF))
     &    CALL punch(Stwt,Posfob+1,Posffc,LXEIWF,F,F)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c     Print out combined trading day, calendar effect
c-----------------------------------------------------------------------
       IF(Axrgtd.and.Kswv.eq.4)THEN
        fext=LXRTDC+Kpart-2
        IF(Prttab(fext))CALL table(Factd,Pos1ob,Posfob,18,1,1,Dx11,fext)
        IF(Savtab(fext).OR.(Lgraf.and.Kpart.eq.3))THEN
         IF(Savfct.or.Savbct)THEN
          IF(Savtab(fext))CALL punch(Factd,bsav,ksav,fext,F,F)
          IF(Lgraf.and.Kpart.eq.3)
     &       CALL punch(Factd,bsav,ksav,fext,Lgraf,F)
         ELSE
          IF(Savtab(fext))CALL punch(Factd,Pos1ob,Posfob,fext,F,F)
          IF(Lgraf.and.Kpart.eq.3)
     &       CALL punch(Factd,Pos1ob,Posfob,fext,Lgraf,F)
         END IF
        END IF
        IF(Axrghl)THEN
         fext=LXRCLC+Kpart-2
         IF(Prttab(fext))
     &      CALL table(Faccal,Pos1ob,Posfob,22,2,1,dvec,fext)
         IF(Savtab(fext).OR.(Lgraf.and.Kpart.eq.3))THEN
          IF(Savfct.or.Savbct)THEN
           IF(Savtab(fext))CALL punch(Faccal,bsav,ksav,fext,F,F)
           IF(Lgraf.and.Kpart.eq.3)
     &        CALL punch(Faccal,bsav,ksav,fext,Lgraf,F)
          ELSE
           IF(Savtab(fext))CALL punch(Faccal,Pos1ob,Posfob,fext,F,F)
           IF(Lgraf.and.Kpart.eq.3)
     &        CALL punch(Faccal,Pos1ob,Posfob,fext,Lgraf,F)
          END IF
         END IF
        END IF
       END IF
c-----------------------------------------------------------------------
c     Return if X-11 regression done as prior adjustment
c-----------------------------------------------------------------------
       IF(Lfatal.or.(Ixreg.eq.2.and.Kpart.eq.3.and.Khol.ne.1))RETURN
c-----------------------------------------------------------------------
C --- COMPUTE EXTREME COMPONENT.
c-----------------------------------------------------------------------
       IF(Muladd.gt.0)THEN
c-----------------------------------------------------------------------
C --- ADDITIVE AND LOGARITHMIC MODEL.
c-----------------------------------------------------------------------
        DO i=Pos1bk,Posffc
         Stex(i)=Sti(i)*(ONE-Stwt(i))
        END DO
       ELSE
c-----------------------------------------------------------------------
C --- MULTIPLICATIVE MODEL.
c-----------------------------------------------------------------------
        DO i=Pos1bk,Posffc
         Stex(i)=Sti(i)/(ONE+Stwt(i)*(Sti(i)-ONE))
        END DO
       END IF
c-----------------------------------------------------------------------
c       IF((Axrgtd.or.Axrghl).and.Ixreg.eq.1)THEN
       IF((Axrgtd.or.Axrghl).AND.(Ixreg.eq.1.or.Ixreg.eq.2))THEN
c-----------------------------------------------------------------------
C --- SET STCSI EQUAL TO THE RAW SERIES.
c-----------------------------------------------------------------------
        CALL copy(Series(Pos1bk),Posffc-Pos1bk+1,1,Stcsi(Pos1bk))
        IF(Adjls.eq.1)CALL divsub(Stcsi,Stcsi,Facls,Pos1bk,Posffc)
        IF(Adjao.eq.1)CALL divsub(Stcsi,Stcsi,Facao,Pos1bk,Posffc)
        IF(Adjtc.eq.1)CALL divsub(Stcsi,Stcsi,Factc,Pos1bk,Posffc)
        IF(Adjso.eq.1)CALL divsub(Stcsi,Stcsi,Facso,Pos1bk,Posffc)
        IF(Adjsea.eq.1)CALL divsub(Stcsi,Stcsi,Facsea,Pos1bk,Posffc)
*        IF(Adjpls.eq.1)CALL divsub(Stcsi,Stcsi,Facpls,Pos1bk,Posffc)
*        IF(Adjplt.eq.1)CALL divsub(Stcsi,Stcsi,Facplt,Pos1bk,Posffc)
*        IF(Adjplo.eq.1)CALL divsub(Stcsi,Stcsi,Facplo,Pos1bk,Posffc)
        IF(Adjusr.eq.1)CALL divsub(Stcsi,Stcsi,Facusr,Pos1bk,Posffc)
        IF(Muladd.eq.2)Muladd=0
c-----------------------------------------------------------------------
C --- DIVIDE (SUBTRACT) BY THE PRIOR ADJUSTMENT SERIES.
c-----------------------------------------------------------------------
        IF(Kfmt.gt.0)CALL divsub(Stcsi,Stcsi,Sprior,Pos1ob,Posfob)
c        IF(Adjhol.eq.1.or.Khol.gt.1.or.Axrghl)THEN
c         IF(Axrghl.AND.((Khol.eq.1.and.Ixreg.eq.2).OR.Ixreg.eq.1))THEN
c          CALL divsub(Stcsi,Stcsi,Facxhl,Pos1ob,Posfob)
c         ELSE
c          CALL divsub(Stcsi,Stcsi,Fachol,Pos1ob,Posfob)
c         END IF
c        END IF
c-----------------------------------------------------------------------
C --- DIVIDE (SUBTRACT) THE PRIOR MONTHLY ADJUSTED SERIES A3 OR A1 BY
C --- THE COMBINED TRADING DAY FACTORS TO OBTAIN THE CALENDAR ADJUSTED
C --- SERIES.
c-----------------------------------------------------------------------
        IF(Adjtd.eq.1.or.Axrgtd.or.Adjhol.eq.1.or.Khol.gt.1.or.Axrghl
     &     .or.Kswv.gt.0)THEN
         IF(Psuadd)THEN
          DO i=Pos1bk,Posffc
           IF(Kfulsm.lt.2)THEN
            Stcsi(i)=Stc(i)*(Sts(i)+Sti(i)/Faccal(i)-ONE)
           ELSE
            Stcsi(i)=Stc(i)*(Sti(i)/Faccal(i))
           END IF
          END DO
         ELSE
          CALL divsub(Stcsi,Stcsi,Faccal,Pos1bk,Posffc)
         END IF
        END IF
        IF(Tmpma.eq.2)THEN
         Muladd=2
         CALL logar(Stcsi,Pos1bk,Posffc)
        END IF
c-----------------------------------------------------------------------
C --- WRITE THE ORIGINAL SERIES ADJUSTED FOR CALENDAR VARIATION B19 OR
C --- C19.
c-----------------------------------------------------------------------
        fext=LXETDO+Kpart-2
        IF(Prttab(fext))CALL table(Stcsi,Pos1ob,Posfob,19,1,2,dvec,fext)
        IF(.not.Lfatal.and.Savtab(fext))
     &     CALL punch(Stcsi,Pos1ob,Posfob,fext,F,F)
        IF(Lfatal)RETURN
       ELSE
c-----------------------------------------------------------------------
C --- SET STCSI EQUAL TO STO.
c-----------------------------------------------------------------------
        DO i=Pos1bk,Posffc
         Stcsi(i)=Sto(i)
        END DO
       END IF
       IF(Kpart.eq.3.and.Ixreg.eq.1)
     &    CALL divsub(Stocal,Series,Faccal,Pos1ob,Posfob)
c-----------------------------------------------------------------------
C --- MODIFY THE ORIGINAL SERIES ADJUSTED FOR CALENDAR VARIATION TO
C --- ELIMINATE THE EXTREMES.
c-----------------------------------------------------------------------
       IF(Psuadd)THEN
        DO i=Pos1bk,Posffc
         IF(Kfulsm.eq.2)THEN
          Stcsi(i)=Stc(i)*(Sti(i)/Stex(i))
         ELSE
          Stcsi(i)=Stc(i)*(Sts(i)+((Sti(i)/Stex(i))-ONE))
         END IF
        END DO
       ELSE
        CALL divsub(Stcsi,Stcsi,Stex,Pos1bk,Posffc)
       END IF
c-----------------------------------------------------------------------
C --- WRITE EXTREME SERIES B20 AND C20.
c-----------------------------------------------------------------------
       fext=LX11EV+Kpart-2
       IF(Prttab(fext))CALL table(Stex,Pos1ob,Posfob,20,1,3,dvec,fext)
       IF(.not.Lfatal.and.Savtab(fext))
     &    CALL punch(Stex,Pos1ob,Posfob,fext,F,F)
       IF(Lgraf.and.Kpart.eq.3)
     &    CALL punch(Stex,Pos1ob,Posfob,fext,Lgraf,F)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
C --- INCREASE ITERATION STEP BY 1.
c-----------------------------------------------------------------------
       Kpart=Kpart+1
       Ksect=1
c-----------------------------------------------------------------------
C --- WRITE THE MODIFIED ORIGINAL SERIES C1 OR D1.
c-----------------------------------------------------------------------
       fext=LX11MO+Kpart-3
       IF(Prttab(fext))CALL table(Stcsi,Pos1ob,Posfob,1,1,2,dvec,fext)
       IF(.not.Lfatal.and.Savtab(fext))
     &    CALL punch(Stcsi,Pos1ob,Posfob,fext,F,F)
       IF(Lfatal)RETURN
      END DO
c-----------------------------------------------------------------------
      END
