      SUBROUTINE GTSEAT(Qmax2,Out2,Maxit2,Epsph2,Xl2,Rmod2,Epsiv2,
     &                  Hplan2,Lseats,Lnoadm,Kmean,Lhp,Lstsea,Bias2,
     &                  Lfinit,Iphtrf,Tabtbl,Hptrgt,Lhprmls,InptOK)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Input routine for seats adjustment.  This routine is basically 
c     empty now - we will add other options (such as print, save, and
c     savelog) later when list of tables and diagnostics have been 
c     developed, and we see what other options will need to be 
c     incorporated.
c-----------------------------------------------------------------------
      INCLUDE 'lex.i'
      INCLUDE 'stdio.i'
      INCLUDE 'notset.prm'
      INCLUDE 'tbllog.i'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'svllog.i'
*      INCLUDE 'mdltbl.i'
*      INCLUDE 'hiddn.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO,ONE,PT5
      LOGICAL F,T
      PARAMETER (F=.false.,T=.true.,ZERO=0D0,ONE=1D0,PT5=0.5D0)
c-----------------------------------------------------------------------
      CHARACTER Tabtbl*(100)
      DOUBLE PRECISION Epsph2,Xl2,Rmod2,Epsiv2,Hplan2
      LOGICAL Lseats,Lnoadm,Lhp,Lstsea,Inptok,Lfinit,Lmdsum,Lhprmls
      INTEGER Qmax2,Out2,Maxit2,Kmean,Bias2,Iphtrf,Hptrgt
c-----------------------------------------------------------------------
      DOUBLE PRECISION dvec
      LOGICAL argok
      INTEGER nelt,ivec,tmpptr
      DIMENSION dvec(1),ivec(1),tmpptr(0:1)
c-----------------------------------------------------------------------
      LOGICAL dpeq,gtarg
      EXTERNAL dpeq,gtarg
c-----------------------------------------------------------------------
c     Argument dictionary was made with the following command
c ../../dictionary/strary < ../../dictionary/check.dic
c-----------------------------------------------------------------------
      CHARACTER ARGDIC*131
      INTEGER arglog,argidx,argptr,PARG
      PARAMETER(PARG=22)
      DIMENSION argptr(0:PARG),arglog(2,PARG)
      PARAMETER(ARGDIC='printsavesavelogappendfcstnoadmissimeanqmaxoutep
     &sphixlrmodepsivmaxithplanhpcyclestatseastabtablesbiasfiniteprintph
     &trfhptargethprmls')
c-----------------------------------------------------------------------
c     data dictionary of yes/no choice
c-----------------------------------------------------------------------
      CHARACTER YSNDIC*5
      INTEGER ysnptr,PYSN
      PARAMETER(PYSN=2)
      DIMENSION ysnptr(0:PYSN)
      PARAMETER(YSNDIC='yesno')
c-----------------------------------------------------------------------
c     data dictionary of hptarget choice
c-----------------------------------------------------------------------
      CHARACTER HPTDIC*13
      INTEGER hptptr,PHPT
      PARAMETER(PHPT=3)
      DIMENSION hptptr(0:PHPT)
      PARAMETER(HPTDIC='trendsadjorig')
c-----------------------------------------------------------------------
      DATA argptr/1,6,10,17,27,35,40,44,47,53,55,59,64,69,74,81,89,98,
     &            102,108,118,126,132 /
      DATA ysnptr/1,4,6 /
      DATA hptptr/1,6,10,14 /
c-----------------------------------------------------------------------
c     Initialize variables
c-----------------------------------------------------------------------
      argok=T
      CALL setint(NOTSET,2*PARG,arglog)
c-----------------------------------------------------------------------
c     Argument get loop
c-----------------------------------------------------------------------
      DO WHILE (T)
       IF(gtarg(ARGDIC,argptr,PARG,argidx,arglog,argok))THEN
        IF(Lfatal)RETURN
        GO TO(20,30,40,50,60,70,90,100,110,120,130,140,150,160,170,190,
     &        200,240,260,270,280,290),argidx
c-----------------------------------------------------------------------
c     Print argument
c-----------------------------------------------------------------------
   20   CALL getprt(LSPSET,NSPSET,Inptok)
        GO TO 300
c-----------------------------------------------------------------------
c     Save argument
c-----------------------------------------------------------------------
   30   CALL getsav(LSPSET,NSPSET,Inptok)
        GO TO 300
c-----------------------------------------------------------------------
c     savelog  argument
c-----------------------------------------------------------------------
   40   CALL getsvl(LSLSET,NSLSET,Inptok)
        GO TO 300
c-----------------------------------------------------------------------
c     appendfcst argument
c-----------------------------------------------------------------------
   50   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &       'Available options for appending forecasts are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Savfct=ivec(1).eq.1
        GO TO 300
c-----------------------------------------------------------------------
c     noadmiss argument
c-----------------------------------------------------------------------
   60   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for noadmiss are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lnoadm=ivec(1).eq.1
        GO TO 300
c-----------------------------------------------------------------------
c     imean argument
c-----------------------------------------------------------------------
   70   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for imean are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Kmean=2-ivec(1)
        GO TO 300
c-----------------------------------------------------------------------
c     qmax argument
c-----------------------------------------------------------------------
   90   CALL getivc(LPAREN,T,1,ivec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(nelt.gt.0)THEN
         IF(.not.argok)THEN
          CALL inpter(PERROR,Errpos,'Invalid limit for Ljung-Box Q.',T)
         ELSE IF(ivec(1).lt.0)THEN
          CALL inpter(PERROR,Errpos,
     &                'Limit for Ljung-Box Q must be > 0.',T)
          argok=F
         ELSE
          Qmax2=ivec(1)
         END IF
        END IF
        GO TO 300
c-----------------------------------------------------------------------
c     Out argument
c-----------------------------------------------------------------------
  100   CALL getivc(LPAREN,T,1,ivec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(nelt.gt.0)THEN
         IF(.not.argok)THEN
          CALL inpter(PERROR,Errpos,'Invalid limit for out.',T)
          argok=F
         ELSE IF(ivec(1).lt.0.or.ivec(1).gt.2)THEN
          CALL inpter(PERROR,Errpos,
     &                'Out must be either 0, 1, or 2.',T)
          argok=F
         ELSE
          Out2=ivec(1)
         END IF
        END IF
        GO TO 300
c-----------------------------------------------------------------------
c     epsphi argument
c-----------------------------------------------------------------------
  110   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).lt.ZERO)THEN
          CALL inpter(PERROR,Errpos,
     &                'Epsphi must be greater than or equal to zero.',T)
          Inptok=F
         ELSE
          Epsph2=dvec(1)
         END IF
        END IF
        GO TO 300
c-----------------------------------------------------------------------
c     xl argument
c-----------------------------------------------------------------------
  120   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).lt.PT5.or.dvec(1).gt.ONE)THEN
          CALL inpter(PERROR,Errpos,'Xl must be greater than or equal to
     & 0.5 and less than or ',T)
          CALL writln('       equal to one.',STDERR,Mt2,F,T)
          Inptok=F
         ELSE
          Xl2=dvec(1)
         END IF
        END IF
        GO TO 300
c-----------------------------------------------------------------------
c     rmod argument
c-----------------------------------------------------------------------
  130   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).lt.ZERO.or.dvec(1).gt.ONE)THEN
          CALL inpter(PERROR,Errpos,'Rmod must be greater than or equal 
     &to zero and less than or ',F)
          CALL writln('       equal to one.',STDERR,Mt2,F,T)
          Inptok=F
         ELSE
          Rmod2=dvec(1)
         END IF
        END IF
        GO TO 300
c-----------------------------------------------------------------------
c     Epsiv argument
c-----------------------------------------------------------------------
  140   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).le.ZERO)THEN
          CALL inpter(PERROR,Errpos,
     &                'Epsiv must be greater than zero.',T)
          Inptok=F
         ELSE
          Epsiv2=dvec(1)
         END IF
        END IF
        GO TO 300
c-----------------------------------------------------------------------
c     maxit argument
c-----------------------------------------------------------------------
  150   CALL getivc(LPAREN,T,1,ivec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(nelt.gt.0)THEN
         IF(.not.argok)THEN
          CALL inpter(PERROR,Errpos,'Invalid value for maxit.',T)
         ELSE IF(ivec(1).le.0)THEN
          CALL inpter(PERROR,Errpos,
     &                'Value for maxit must be > 0.',T)
          argok=F
         ELSE
          Maxit2=ivec(1)
         END IF
        END IF
        GO TO 300
c-----------------------------------------------------------------------
c     Hplan argument
c-----------------------------------------------------------------------
  160   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).le.ZERO)THEN
          CALL inpter(PERROR,Errpos,
     &                'Hplan must be greater than zero.',T)
          Inptok=F
         ELSE
          Hplan2=dvec(1)
         END IF
        END IF
        GO TO 300
c-----------------------------------------------------------------------
c     hpcycle argument
c-----------------------------------------------------------------------
  170   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for hpcycle are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lhp=ivec(1).eq.1
        GO TO 300
c-----------------------------------------------------------------------
c     statseas argument
c-----------------------------------------------------------------------
  190   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for statseas are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lstsea=ivec(1).eq.1
        GO TO 300
c-----------------------------------------------------------------------
c     tabtables argument
c-----------------------------------------------------------------------
  200   CALL gtnmvc(LPAREN,T,1,Tabtbl,tmpptr,nelt,100,argok,Inptok)
        IF(Lfatal)RETURN
        GO TO 300
c-----------------------------------------------------------------------
c     bias argument
c-----------------------------------------------------------------------
  240   CALL getivc(LPAREN,T,1,ivec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(nelt.gt.0)THEN
         IF(.not.argok)THEN
          CALL inpter(PERROR,Errpos,'Invalid limit for bias.',T)
          argok=F
         ELSE IF(ivec(1).lt.-1.or.ivec(1).gt.1)THEN
          CALL inpter(PERROR,Errpos,
     &                'Bias must be either -1, 0, or 1.',T)
          argok=F
         ELSE
          Bias2=ivec(1)
         END IF
        END IF
        GO TO 300
c-----------------------------------------------------------------------
c     finite argument
c-----------------------------------------------------------------------
  260   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for finite are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lfinit=ivec(1).eq.1
        GO TO 300
c-----------------------------------------------------------------------
c     printphtrf argument
c-----------------------------------------------------------------------
  270   CALL getivc(LPAREN,T,1,ivec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(nelt.gt.0)THEN
         IF(.not.argok)THEN
          CALL inpter(PERROR,Errpos,'Invalid limit for printphtrf.',T)
          argok=F
         ELSE IF(ivec(1).lt.0.or.ivec(1).gt.1)THEN
          CALL inpter(PERROR,Errpos,
     &                'printphtrf must be either 0 or 1.',T)
          argok=F
         ELSE
          Iphtrf=ivec(1)
         END IF
        END IF
        GO TO 300
c-----------------------------------------------------------------------
c     hptarget argument
c-----------------------------------------------------------------------
  280   CALL gtdcvc(LPAREN,T,1,HPTDIC,hptptr,PHPT,
     &        'Available options for hptarget are trend, sadj or orig.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Hptrgt=ivec(1)
        GO TO 300
c-----------------------------------------------------------------------
c     hprmls argument
c-----------------------------------------------------------------------
  290   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for hprmls are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lhprmls=ivec(1).eq.1
        GO TO 300
       END IF
c     -----------------------------------------------------------------
       Inptok=Inptok.and.argok
       IF(Inptok)Lseats=T
       RETURN
  300  CONTINUE
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
