C     Last change:  BCM  19 May 1998    1:31 pm
      SUBROUTINE gtestm(Havreg,Hvarma,Nspobs,Mxitr,Mxnlit,Lestim,Outest,
     &                  Mdlfil,Hvmfil,Eick,Rmcnst,Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     gtestm.f, Release 1, Subroutine Version 1.7, Modified 14 Feb 1995.
c-----------------------------------------------------------------------
c     Get a function
c-----------------------------------------------------------------------
c     Variable typing and parameters initialization
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'lex.i'
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdltbl.i'
      INCLUDE 'tbllog.i'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'svllog.i'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c-----------------------------------------------------------------------
      CHARACTER Mdlfil*(PFILCR)
      LOGICAL argok,hvnltl,hvtol,Lestim,Hvarma,Hvmfil,Havreg,Inptok,
     &        Rmcnst
      INTEGER Mxitr,Mxnlit,nelt,Nspobs,ivec,Outest,itmpvc
      DOUBLE PRECISION dvec,mprec,Eick
      DIMENSION dvec(1),ivec(1),itmpvc(0:1)
c-----------------------------------------------------------------------
      LOGICAL gtarg
      DOUBLE PRECISION dpmpar
      EXTERNAL dpmpar,gtarg
c-----------------------------------------------------------------------
c     Argument dictionary
c-----------------------------------------------------------------------
      CHARACTER ARGDIC*87
      INTEGER arglog,argidx,argptr,PARG
      PARAMETER(PARG=15)
      DIMENSION argptr(0:PARG),arglog(2,PARG)
      PARAMETER(ARGDIC='maxitermaxnlitertolnltolparmsexactoutofsamplepri
     &ntsavesavelogfilefixstepkremoveconstant')
c-----------------------------------------------------------------------
      CHARACTER EXTDIC*10
      INTEGER extptr,PEXT
      PARAMETER(PEXT=3)
      DIMENSION extptr(0:PEXT)
      PARAMETER(EXTDIC='armamanone')
c-----------------------------------------------------------------------
      CHARACTER ESTDIC*14
      INTEGER estptr,PEST
      PARAMETER(PEST=2)
      DIMENSION estptr(0:PEST)
      PARAMETER(ESTDIC='fixedestimated')
c-----------------------------------------------------------------------
      CHARACTER YSNDIC*5
      INTEGER ysnptr,PYSN
      PARAMETER(PYSN=2)
      DIMENSION ysnptr(0:PYSN)
      PARAMETER(YSNDIC='yesno')
c-----------------------------------------------------------------------
      CHARACTER FIXDIC*22
      INTEGER fixptr,PRMFIX
      PARAMETER(PRMFIX=5)
      DIMENSION fixptr(0:PRMFIX)
      PARAMETER(FIXDIC='nochangenonearmaregall')
c-----------------------------------------------------------------------
      DATA argptr/1,8,17,20,25,30,35,46,51,55,62,66,69,73,74,88/
      DATA extptr/1,5,7,11/
      DATA estptr/1,6,15/
      DATA ysnptr/1,4,6/
      DATA fixptr/1,9,13,17,20,23/
c-----------------------------------------------------------------------
      mprec=dpmpar(1)
c-----------------------------------------------------------------------
c     Argument get loop
c-----------------------------------------------------------------------
      hvtol=F
      hvnltl=F
      CALL setint(NOTSET,2*PARG,arglog)
c-----------------------------------------------------------------------
      DO WHILE (T)
       IF(gtarg(ARGDIC,argptr,PARG,argidx,arglog,Inptok))THEN
        IF(Lfatal)RETURN
        GO TO(10,20,30,40,50,60,65,70,80,90,100,110,115,116,117),argidx
c-----------------------------------------------------------------------
c     Maximum overall iterations
c-----------------------------------------------------------------------
   10   CALL getivc(LPAREN,T,1,ivec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        Mxitr=ivec(1)
        GO TO 120
c-----------------------------------------------------------------------
c     Maximum nonlinear iterations
c-----------------------------------------------------------------------
   20   CALL getivc(LPAREN,T,1,ivec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        Mxnlit=ivec(1)
        GO TO 120
c-----------------------------------------------------------------------
c     Overall convergence tolerance, The actual tolerance is for the
c reletive deviance of the objective function which is 2/nefobs * the
c tolerance of the log likelihood.
c-----------------------------------------------------------------------
   30   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        Tol=dvec(1)
        IF(nelt.gt.0)THEN
         IF(1D0/Nspobs*Tol.lt.mprec)THEN
          CALL inpter(PERROR,Errpos,
     &          'Overall tolerance is smaller than machine precision',F)
          WRITE(STDERR,1010)mprec*Nspobs
          WRITE(Mt2,1011)mprec*Nspobs
 1010     FORMAT('        Make larger than ',e10.3)
 1011     FORMAT(' Make larger than ',e10.3,'</p>')
          hvtol=F
          Inptok=F
c-----------------------------------------------------------------------
         ELSE
          hvtol=T
         END IF
        END IF
        GO TO 120
c-----------------------------------------------------------------------
c     Nonlinear convergence tolerance
c-----------------------------------------------------------------------
   40   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(nelt.gt.0)THEN
         Nltol=dvec(1)
         IF(Nltol.lt.1D2*mprec)THEN
          CALL inpter(PERROR,Errpos,
     &        'Nonlinear tolerance is smaller than machine precision',F)
          WRITE(STDERR,1010)mprec*Nspobs
          WRITE(Mt2,1010)mprec*Nspobs
          Inptok=F
          hvnltl=F
c-----------------------------------------------------------------------
         ELSE
          hvnltl=T
         END IF
        END IF
        GO TO 120
c-----------------------------------------------------------------------
c     Specify whether the parameters are fixed and the likelihood
c and the parameters are estimated or the parameters and the likelihood
c are estimated
c-----------------------------------------------------------------------
   50   CALL gtdcvc(LPAREN,T,1,ESTDIC,estptr,PEST,
     &              'Choices are fixed or estimated',ivec,nelt,T,argok,
     &              Inptok)
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
        IF(nelt.gt.0)THEN
         IF(ivec(1).ne.1)THEN
          Lestim=T
         ELSE IF(Imdlfx.ge.1)THEN
          Lestim=F
c-----------------------------------------------------------------------
         ELSE
          CALL inpter(PERROR,Errpos,
     &                'Must specify all ARMA parameters to evaluate',T)
          Inptok=F
c-----------------------------------------------------------------------
         END IF
        END IF
        GO TO 120
c-----------------------------------------------------------------------
c     Method of estimation exact MA only or ARMA, or conditional
c-----------------------------------------------------------------------
   60   CALL gtdcvc(LPAREN,T,1,EXTDIC,extptr,PEXT,
     &              'Choices are ARMA, MA, or NONE (conditional)',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
        IF(nelt.gt.0)THEN
         IF(ivec(1).eq.1)THEN
          Lextar=T
          Lextma=T
         ELSE IF(ivec(1).eq.2)THEN
          Lextar=F
          Lextma=T
         ELSE IF(ivec(1).eq.3)THEN
          Lextar=F
          Lextma=F
         END IF
        END IF
        GO TO 120
c-----------------------------------------------------------------------
c     outofsample argument
c-----------------------------------------------------------------------
   65   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for outofsample are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Outest=ivec(1)
        GO TO 120
c-----------------------------------------------------------------------
c     Print argument
c-----------------------------------------------------------------------
   70   CALL getprt(LSPEST,NSPEST,Inptok)
        Lprier=Prttab(LESTIE)
        GO TO 120
c-----------------------------------------------------------------------
c     Save argument
c-----------------------------------------------------------------------
   80   CALL getsav(LSPEST,NSPEST,Inptok)
        GO TO 120
c-----------------------------------------------------------------------
c     Save argument
c-----------------------------------------------------------------------
   90   CALL getsvl(LSLEST,NSLEST,Inptok)
        GO TO 120
c-----------------------------------------------------------------------
c     file argument
c-----------------------------------------------------------------------
  100   IF(Hvarma.or.Havreg)THEN
         CALL inpter(PERROR,Errpos,
     &               'Cannot specify a model file when a regARIMA '//
     &               'model is specified in',F)
         CALL writln('the arima and/or regression specs.',
     &               Mt2,STDERR,F,T)
         Inptok=F
        END IF
        CALL gtnmvc(LPAREN,T,1,Mdlfil,itmpvc,nelt,PFILCR,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.eq.1)Hvmfil=T
        GO TO 120
c-----------------------------------------------------------------------
c     fix argument
c-----------------------------------------------------------------------
  110   CALL gtdcvc(LPAREN,T,1,FIXDIC,fixptr,PRMFIX,
     &       'Acceptable entries are nochange, none, arma, reg or all.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.eq.1)Fixmdl=ivec(1)-2
        GO TO 120
c-----------------------------------------------------------------------
c     Step size of numerical derivatives
c-----------------------------------------------------------------------
  115   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(nelt.gt.0)THEN
         Stepln=dvec(1)
         IF(Stepln.lt.0D0)THEN
          CALL inpter(PERROR,Errpos,
     &                'Step size of numerical derivatives cannot be '//
     &                'less than zero.',T)
          Inptok=F
         END IF
        END IF
        GO TO 120
c-----------------------------------------------------------------------
c     Penalty term for EIC
c-----------------------------------------------------------------------
  116   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(nelt.gt.0)THEN
         Eick=dvec(1)
         IF(Eick.le.0D0)THEN
          CALL inpter(PERROR,Errpos,
     &                'Penalty term for EIC cannot be less than or '//
     &                'equal to zero.',T)
          Inptok=F
         END IF
        END IF
        GO TO 120
c-----------------------------------------------------------------------
c     removeconstant argument
c-----------------------------------------------------------------------
  117   CALL gtdcvc(LPAREN,F,1,YSNDIC,ysnptr,PYSN,
     &              'Choices for removeconstant are yes and no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Rmcnst=ivec(1).eq.1
        GO TO 120
       END IF
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Set the ARMA and initial ARMA convergence tolerances depending
c on which tolerances have been input.
c-----------------------------------------------------------------------
       IF(hvtol.and..not.hvnltl)THEN
        Nltol=Tol
        Nltol0=100D0*Tol
c----------------------------------------------------------------------
       ELSE IF(hvnltl)THEN
        Nltol0=Nltol
       END IF
c----------------------------------------------------------------------
       RETURN
  120  CONTINUE
      END DO
c----------------------------------------------------------------------
      END
