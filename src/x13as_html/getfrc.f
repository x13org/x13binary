      SUBROUTINE getfrc(Havesp,Iyrt,Lrndsa,Iftrgt,Begyrt,Mid,Lamda,Rol,
     &                  Sp,Lindfr,Lfctfr,Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Get the forcing options for X-13ARIMA-SEATS.
c----------------------------------------------------------------------
c     Variable typing and parameters initialization
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'lex.i'
      INCLUDE 'stdio.i'
      INCLUDE 'tbllog.i'
      INCLUDE 'error.cmn'
      INCLUDE 'units.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION ONE,ZERO,PT9,THREE,MINUS3
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.,ONE=1D0,ZERO=0D0,PT9=0.9D0,THREE=3D0,
     &          MINUS3=-3D0)
c-----------------------------------------------------------------------
      DOUBLE PRECISION dvec,Lamda,Rol
      LOGICAL argok,Inptok,Lrndsa,Havesp,Lfctfr,Lindfr
      INTEGER Sp,nelt,Begyrt,Iftrgt,Iyrt,ivec,Mid
      DIMENSION dvec(1),ivec(1)
c-----------------------------------------------------------------------
      LOGICAL gtarg,dpeq
      EXTERNAL gtarg,dpeq
c-----------------------------------------------------------------------
c   arguments for force spec.
c-----------------------------------------------------------------------
      CHARACTER ARGDIC*57
      INTEGER argidx,argptr,PARG,arglog
      PARAMETER(PARG=11)
      DIMENSION argptr(0:PARG),arglog(2,PARG)
      PARAMETER(ARGDIC='typeroundtargetstartlambdarhomodeprintsaveindfor
     &ceusefcst')
c-----------------------------------------------------------------------
c   type data dictionary
c-----------------------------------------------------------------------
      CHARACTER FRCDIC*17
      INTEGER frcptr,PFRC
      PARAMETER(PFRC=3)
      DIMENSION frcptr(0:PFRC)
      PARAMETER(FRCDIC='nonedentonregress')
c-----------------------------------------------------------------------
c     forcesums data dictionary
c-----------------------------------------------------------------------
      CHARACTER SUMDIC*118
      INTEGER sumptr,PSUM
      PARAMETER(PSUM=28)
      DIMENSION sumptr(0:PSUM)
      PARAMETER(SUMDIC=
     &'janfebmaraprmayjunjulaugsepoctnovdecjanuaryfebruarymarchaprilmayj
     &unejulyaugustseptemberoctobernovemberdecemberq1q2q3q4')
c-----------------------------------------------------------------------
c     data dictionary of yes/no choice
c-----------------------------------------------------------------------
      CHARACTER YSNDIC*5
      INTEGER ysnptr,PYSN
      PARAMETER(PYSN=2)
      DIMENSION ysnptr(0:PYSN)
      PARAMETER(YSNDIC='yesno')
c-----------------------------------------------------------------------
c     data dictionary of ratio/diff choice
c-----------------------------------------------------------------------
      CHARACTER FMDDIC*9
      INTEGER fmdptr,PFMD
      PARAMETER(PFMD=2)
      DIMENSION fmdptr(0:PFMD)
      PARAMETER(FMDDIC='ratiodiff')
c     ------------------------------------------------------------------
c     data dictionary for force target
c     ------------------------------------------------------------------
      CHARACTER FRTDIC*35
      INTEGER frtptr,PFRT
      PARAMETER(PFRT=4)
      DIMENSION frtptr(0:PFRT)
      PARAMETER(FRTDIC='originalcalendaradjpermprioradjboth')
c     ------------------------------------------------------------------
c     Define data dictionary pointers
c     ------------------------------------------------------------------
      DATA argptr/1,5,10,16,21,27,30,34,39,43,51,58/
      DATA sumptr/1,4,7,10,13,16,19,22,25,28,31,34,37,44,52,57,62,65,69,
     &            73,79,88,95,103,111,113,115,117,119/
      DATA ysnptr/1,4,6/
      DATA frcptr/1,5,11,18/
      DATA fmdptr/1,6,10/
      DATA frtptr/1,9,20,32,36/
c     ------------------------------------------------------------------
      argok=T
      CALL setint(NOTSET,2*PARG,arglog)
      DO WHILE (T)
c-----------------------------------------------------------------------
c     Argument get loop
c-----------------------------------------------------------------------
       IF(gtarg(ARGDIC,argptr,PARG,argidx,arglog,Inptok))THEN
        IF(Lfatal)RETURN
        GO TO(10,20,30,40,90,100,110,120,130,70,80),argidx
c-----------------------------------------------------------------------
c      type argument
c-----------------------------------------------------------------------
   10   CALL gtdcvc(LPAREN,T,1,FRCDIC,frcptr,PFRC,
     &       'Entry for type argument must be none, denton or regress.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Iyrt=ivec(1)-1
        GO TO 140
c-----------------------------------------------------------------------
c     round argument
c-----------------------------------------------------------------------
   20   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for round are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lrndsa=ivec(1).eq.1
        GO TO 140
c-----------------------------------------------------------------------
c      forcetarget argument
c-----------------------------------------------------------------------
   30   CALL gtdcvc(LPAREN,T,1,FRTDIC,frtptr,PFRT,
     &  'Entry for forcetarget argument must be original, calendaradj,',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(.not.argok)
     &     CALL writln('        permprioradj, or both.',STDERR,Mt2,F,T)
        IF(argok.and.nelt.gt.0)Iftrgt=ivec(1)-1
        GO TO 140
c-----------------------------------------------------------------------
c      start argument
c-----------------------------------------------------------------------
   40   CALL gtdcvc(LPAREN,T,1,SUMDIC,sumptr,PSUM,
     &          'Choices for start are the name of a month or quarter.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         IF(.not.Havesp)THEN
          CALL inpter(PERROR,Errpos,
     &                'No seasonal period specified in series spec.',T)
          Inptok=F
         ELSE 
          IF(ivec(1).ge.1.and.ivec(1).le.24.and.Sp.eq.12)THEN
           Begyrt=ivec(1)
           IF(ivec(1).gt.12)Begyrt=Begyrt-12
          ELSE IF(ivec(1).ge.25.and.ivec(1).le.28.and.Sp.eq.4)THEN
           Begyrt=ivec(1)-24
          ELSE
           IF(Sp.eq.12)THEN
            CALL inpter(PERROR,Errpos,
     &                  'This entry for start only valid for monthly '//
     &                  'data.',T)
           ELSE 
            CALL inpter(PERROR,Errpos,
     &                  'This entry for start only valid for '//
     &                  'quarterly data.',T)
           END IF
           Inptok=F
          END IF
         END IF
        END IF
        GO TO 140
c-----------------------------------------------------------------------
c     indforce argument
c-----------------------------------------------------------------------
   70   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for indforce are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lindfr=ivec(1).eq.1
        GO TO 140
c-----------------------------------------------------------------------
c     usefcst argument
c-----------------------------------------------------------------------
   80   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for usefcst are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lfctfr=ivec(1).eq.1
        GO TO 140
c-----------------------------------------------------------------------
c     lambda argument
c-----------------------------------------------------------------------
   90   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Error Checking for lambda
c-----------------------------------------------------------------------
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).lt.MINUS3.or.dvec(1).gt.THREE)THEN
          CALL inpter(PERROR,Errpos,
     &                'Value of lambda must be between -3 and 3.',T)
          Inptok=F
         ELSE
          Lamda=dvec(1)
         END IF
        END IF
        GO TO 140
c-----------------------------------------------------------------------
c     rho argument
c-----------------------------------------------------------------------
  100   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Error Checking for rho
c-----------------------------------------------------------------------
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).lt.ZERO.or.dvec(1).gt.ONE.or.dpeq(dvec(1),ZERO))THEN
          CALL inpter(PERROR,Errpos,
     &                'Value of rho must be greater than 0 and less '//
     &                'than or equal to 1.',T)
          Inptok=F
         ELSE
          Rol=dvec(1)
         END IF
        END IF
        GO TO 140
c-----------------------------------------------------------------------
c     mode argument
c-----------------------------------------------------------------------
  110   CALL gtdcvc(LPAREN,T,1,FMDDIC,fmdptr,PFMD,
     &              'Available options for mode are ratio or diff.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Mid=ivec(1)-1
        GO TO 140
c-----------------------------------------------------------------------
c     print argument
c-----------------------------------------------------------------------
  120   CALL getprt(LSPFRC,NSPFRC,Inptok)
        GO TO 140
c-----------------------------------------------------------------------
c     save  argument
c-----------------------------------------------------------------------
  130   CALL getsav(LSPFRC,NSPFRC,Inptok)
        GO TO 140
c     ------------------------------------------------------------------
       END IF
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
       IF(Iyrt.eq.NOTSET)THEN
        IF((.not.dpeq(Lamda,DNOTST)).or.(.not.dpeq(Rol,DNOTST)).or.
     *     (Mid.ne.NOTSET))THEN
         Iyrt=2
        ELSE IF((Begyrt.ne.NOTSET).or.(Iftrgt.ne.NOTSET))THEN
         Iyrt=1
        ELSE
         Iyrt=0
        END IF
       END IF
c-----------------------------------------------------------------------
       IF(Iyrt.le.0)THEN
        Iftrgt=0
        Begyrt=0
       ELSE
        IF(Iftrgt.eq.NOTSET)Iftrgt=0
        IF(Begyrt.eq.NOTSET)Begyrt=1
        IF(Iyrt.eq.2)THEN
         IF(dpeq(Lamda,DNOTST))Lamda=ZERO
         IF(dpeq(Rol,DNOTST))THEN
          Rol=PT9
          IF(Sp.ne.12)Rol=PT9**(12D0/dble(Sp))
         END IF
        END IF
       END IF
c-----------------------------------------------------------------------
       Inptok=Inptok.and.argok
c-----------------------------------------------------------------------
       RETURN
  140  CONTINUE
      END DO
c     ------------------------------------------------------------------
      END
      
