C     Last change:  BCM  22 Feb 1999    3:02 pm
      SUBROUTINE mdlset(Nrar,Nrdiff,Nrma,Nsar,Nsdiff,Nsma,Locok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c      INCLUDE 'lex.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      CHARACTER str*(POPRCR)
      LOGICAL arfix,argok,dffix,Locok,mafix
      INTEGER arlag,dflag,itmp,malag,MULT,naimcf,nchr,ndcoef,Nrar,
     &        Nrdiff,Nrma,Nsar,Nsdiff,Nsma
      DOUBLE PRECISION arcoef,dfcoef,macoef
      PARAMETER(MULT=3)
      DIMENSION arcoef(PORDER),arfix(PORDER),arlag(PORDER),
     &          dfcoef(PDIFOR),dffix(PDIFOR),dflag(PDIFOR),
     &          macoef(PORDER),mafix(PORDER),malag(PORDER)
c-----------------------------------------------------------------------
c     Get factors (AR DIFF MA)SP until the next name.
c-----------------------------------------------------------------------
      Locok=T
      Nseadf=Nsdiff
      Nnsedf=Nrdiff
      naimcf=0
c-----------------------------------------------------------------------
      CALL mkmdsn(Nrar,Nrdiff,Nrma,Nsar,Nsdiff,Nsma,Mdldsn,Nmddcr)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     set up model operators based on values input...
c     nonseasonal AR
c-----------------------------------------------------------------------
      IF(Nrar.gt.0)THEN
       CALL setopr(AR,arcoef,arlag,arfix,Nrar,itmp,naimcf,argok,Locok)
       IF(Lfatal)RETURN
       IF(Locok)THEN
        CALL iscrfn(MULT,1,arlag,Nrar,PORDER,arlag)
        CALL mkoprt(AR,1,Sp,str,nchr)
        IF(.not.Lfatal)CALL insopr(AR,arcoef,arlag,arfix,Nrar,1,
     &                             str(1:nchr),argok,Locok)
        IF(Lfatal)RETURN
        CALL maxlag(Arimal,Opr,Mdl(AR-1),Mdl(AR)-1,Mxarlg)
       END IF
      END IF
c-----------------------------------------------------------------------
c       nonseasonal differencing
c-----------------------------------------------------------------------
      IF(Nrdiff.gt.0)THEN
       ndcoef=Nrdiff
       CALL setopr(DIFF,dfcoef,dflag,dffix,ndcoef,Nrdiff,naimcf,argok,
     &             Locok)
       IF(Lfatal)RETURN
       IF(ndcoef.gt.PDIFOR)THEN
        CALL eWritln('Order of the differencing operator is too '//
     &               'large.',STDERR,Mt2,T,T)
        Locok=F
       ELSE
        CALL iscrfn(MULT,1,dflag,ndcoef,PDIFOR,dflag)
        CALL mkoprt(DIFF,1,Sp,str,nchr)
        IF(.not.Lfatal)CALL insopr(DIFF,dfcoef,dflag,dffix,ndcoef,
     &                             1,str(1:nchr),argok,Locok)
        IF(Lfatal)RETURN
       END IF
       CALL maxlag(Arimal,Opr,Mdl(DIFF-1),Mdl(DIFF)-1,Mxdflg)
      END IF
c-----------------------------------------------------------------------
c       nonseasonal MA
c-----------------------------------------------------------------------
      IF(Nrma.gt.0)THEN
       CALL setopr(MA,macoef,malag,mafix,Nrma,itmp,naimcf,argok,Locok)
       IF(Lfatal)RETURN
       IF(Locok)THEN
        CALL iscrfn(MULT,1,malag,Nrma,PORDER,malag)
        CALL mkoprt(MA,1,Sp,str,nchr)
        IF(.not.Lfatal)CALL insopr(MA,macoef,malag,mafix,Nrma,1,
     &                             str(1:nchr),argok,Locok)
        IF(Lfatal)RETURN
        CALL maxlag(Arimal,Opr,Mdl(MA-1),Mdl(MA)-1,Mxmalg)
       END IF
      END IF
c-----------------------------------------------------------------------
c       seasonal AR
c-----------------------------------------------------------------------
      IF(Nsar.gt.0)THEN
       CALL setopr(AR,arcoef,arlag,arfix,Nsar,itmp,naimcf,argok,Locok)
       IF(Lfatal)RETURN
       IF(Locok)THEN
        CALL iscrfn(MULT,Sp,arlag,Nsar,PORDER,arlag)
        CALL mkoprt(AR,Sp,Sp,str,nchr)
        IF(.not.Lfatal)CALL insopr(AR,arcoef,arlag,arfix,Nsar,Sp,
     &                             str(1:nchr),argok,Locok)
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c      Check the of maximum lag of all the AR operators added so far
c does not exceed the maximum order otherwise is will exceed temporary
c storage in the filtering operations where the operators are
c expanded/multiplied into just the coefficients of one full operator.
c This is only going to be a problem for seasonal models.
c-----------------------------------------------------------------------
        CALL maxlag(Arimal,Opr,Mdl(AR-1),Mdl(AR)-1,Mxarlg)
        IF(Mxarlg.gt.PORDER)THEN
         CALL eWritln('Order of the AR operator is too large.',
     &                STDERR,Mt2,T,T)
         Locok=F
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
c       seasonal differencing
c-----------------------------------------------------------------------
      IF(Nsdiff.gt.0)THEN
       ndcoef=Nsdiff
       CALL setopr(DIFF,dfcoef,dflag,dffix,ndcoef,Nsdiff,naimcf,argok,
     &             Locok)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Check that we don't have a seasonal difference and a seasonal
c effect variables or a U(B) operator.
c-----------------------------------------------------------------------
       Lseadf=(Sp.gt.1).or.(Sp.eq.1.and.ndcoef.eq.Sp-1)
       IF(Lseadf.and.Lseff)THEN
        CALL eWritln('Cannot have a seasonal difference with seasonal'//
     &               ' regression effects.',STDERR,Mt2,T,T)
        Locok=F
       END IF
c     ------------------------------------------------------------------
       IF(ndcoef.gt.PDIFOR)THEN
        CALL eWritln('Order of the differencing operator is too '//
     &               'large.',STDERR,Mt2,T,T)
        Locok=F
       ELSE
        CALL iscrfn(MULT,Sp,dflag,ndcoef,PDIFOR,dflag)
        CALL mkoprt(DIFF,Sp,Sp,str,nchr)
        IF(.not.Lfatal)CALL insopr(DIFF,dfcoef,dflag,dffix,ndcoef,
     &                             Sp,str(1:nchr),argok,Locok)
        IF(Lfatal)RETURN
       END IF
c-----------------------------------------------------------------------
c     Check the maximum lag of all the differencing operators added
c does not exceed the maximum order otherwise it will exceed
c temporary storage in the filtering operations where the operators
c are expanded/multiplied into the coefficients of one full
c operator.  This is only a problem for seasonal models.
c-----------------------------------------------------------------------
       CALL maxlag(Arimal,Opr,Mdl(DIFF-1),Mdl(DIFF)-1,Mxdflg)
       IF(Mxdflg.gt.PDIFOR)THEN
        CALL eWritln('Order of the full differencing operator is too '//
     &               'large.',STDERR,Mt2,T,T)
        Locok=F
       END IF
      END IF
c-----------------------------------------------------------------------
c      seasonal MA
c-----------------------------------------------------------------------
      IF(Nsma.gt.0)THEN
       CALL setopr(MA,macoef,malag,mafix,Nsma,itmp,naimcf,argok,Locok)
       IF(Lfatal)RETURN
       IF(Locok)THEN
        CALL iscrfn(MULT,Sp,malag,Nsma,PORDER,malag)
        CALL mkoprt(MA,Sp,Sp,str,nchr)
        IF(.not.Lfatal)CALL insopr(MA,macoef,malag,mafix,Nsma,Sp,
     &                             str(1:nchr),argok,Locok)
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c      Check the maximum lag of all the MA operators added does not
c exceed the maximum order otherwise it will exceed temporary storage
c in the filtering operations where the operators are expanded/
c multiplied into one full operator. This is only going to be a problem
c for seasonal models.
c-----------------------------------------------------------------------
        CALL maxlag(Arimal,Opr,Mdl(MA-1),Mdl(MA)-1,Mxmalg)
        IF(Mxmalg.gt.PORDER)THEN
         CALL eWritln('Order of the MA operator is too large.',
     &                STDERR,Mt2,T,T)
         Locok=F
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
c     Compute the number of effective observations and initialize |G'G|
c-----------------------------------------------------------------------
      Lar=Lextar.and.Mxarlg.gt.0
      Lma=Lextma.and.Mxmalg.gt.0
c     ------------------------------------------------------------------
      IF(Lextar)THEN
       Nintvl=Mxdflg
       Nextvl=Mxarlg+Mxmalg
c     ------------------------------------------------------------------
      ELSE
       Nintvl=Mxdflg+Mxarlg
c     ------------------------------------------------------------------
       Nextvl=0
       IF(Lextma)Nextvl=Mxmalg
      END IF
c-----------------------------------------------------------------------
c     We processed the last operator so start wrapping up.
c     Increment NMDL, indicating we have an ARIMA model.
c-----------------------------------------------------------------------
      IF(Locok)Nmdl=Nmdl+1
c     ------------------------------------------------------------------
      RETURN
      END
