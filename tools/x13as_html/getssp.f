C     Last change:  BCM  26 Feb 1999    9:47 am
**==getssp.f    processed by SPAG 4.03F  at 10:14 on 23 Aug 1994
      SUBROUTINE getssp(Havesp,Sp,Issap,Otlidx,Intidx,Strtss,Sscut,Nlen,
     &                  Ncol,Sstran,Ssfxrg,Nssfxr,Ssdiff,Ssxotl,Ssxint,
     &                  Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Read options which control X-13A-S output format
c-----------------------------------------------------------------------
c     Variable typing and parameters initialization
c-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c     ------------------------------------------------------------------
      INCLUDE 'lex.i'
      INCLUDE 'tbllog.i'
      INCLUDE 'svllog.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'ssap.prm'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      CHARACTER str*(10)
      LOGICAL Havesp,Inptok,argok,Ssxotl,Sstran,Ssdiff,Ssxint
      INTEGER nelt,Strtss,Issap,Sp,Nlen,ivec,Ncol,ipos
      DOUBLE PRECISION Sscut,dvec
      DIMENSION Sscut(5),Strtss(2),ivec(1),dvec(1)
c-----------------------------------------------------------------------
      LOGICAL gtarg
      EXTERNAL gtarg
c-----------------------------------------------------------------------
      CHARACTER ARGDIC*113
      INTEGER arglog,argidx,argptr,PARG
      PARAMETER(PARG=16)
      DIMENSION argptr(0:PARG),arglog(2,PARG)
      PARAMETER(ARGDIC='startcutseascutchngcuttdoutlierfixmdlprintsavele
     &ngthtransparentsavelogfixregadditivesanumspansx11outlierfixx11reg'
     &)
c-----------------------------------------------------------------------
      CHARACTER OTLDIC*13
      INTEGER Otlidx,otlptr,POTLSS
      PARAMETER(POTLSS=3)
      DIMENSION otlptr(0:POTLSS)
      PARAMETER(OTLDIC='removekeepyes')
c-----------------------------------------------------------------------
      CHARACTER INTDIC*10
      INTEGER Intidx,intptr,PINT
      PARAMETER(PINT=3)
      DIMENSION intptr(0:PINT)
      PARAMETER(INTDIC='noyesclear')
c-----------------------------------------------------------------------
      CHARACTER YSNDIC*5
      INTEGER ysnptr,PYSN
      PARAMETER(PYSN=2)
      DIMENSION ysnptr(0:PYSN)
      PARAMETER(YSNDIC='yesno')
c-----------------------------------------------------------------------
      CHARACTER FXRDIC*20
      INTEGER Ssfxrg,fxrptr,PFXR,Nssfxr
      PARAMETER(PFXR=4)
      DIMENSION fxrptr(0:PFXR),Ssfxrg(PFXR)
      PARAMETER(FXRDIC='tdholidayuseroutlier')
c-----------------------------------------------------------------------
      CHARACTER ADDDIC*17
      INTEGER addptr,PADD
      PARAMETER(PADD=2)
      DIMENSION addptr(0:PADD)
      PARAMETER(ADDDIC='differencepercent')
c-----------------------------------------------------------------------
      DATA otlptr/1,7,11,14/
      DATA intptr/1,3,6,11/
      DATA argptr/1,6,13,20,25,32,38,43,47,53,64,71,77,87,95,105,114/
      DATA ysnptr/1,4,6/
      DATA fxrptr/1,3,10,14,21/
      DATA addptr/1,11,18/
c-----------------------------------------------------------------------
      argok=T
      CALL setint(NOTSET,2*PARG,arglog)
      DO WHILE (T)
c-----------------------------------------------------------------------
c     Argument get loop
c-----------------------------------------------------------------------
       IF(gtarg(ARGDIC,argptr,PARG,argidx,arglog,Inptok))THEN
        IF(Lfatal)RETURN
        GO TO(10,20,30,40,50,60,70,80,90,110,120,130,140,150,100,160),
     &        argidx
c     ------------------------------------------------------------------
c     start argument
c     ------------------------------------------------------------------
   10   CALL gtdtvc(Havesp,Sp,LPAREN,F,1,Strtss,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        GO TO 170
c     ------------------------------------------------------------------
c     cutseas argument
c     ------------------------------------------------------------------
   20   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).le.0)THEN
          CALL inpter(PERROR,Errpos,
     &                'Value of cutseas must be greater than zero.',T)
          Inptok=F
         ELSE
          Sscut(1)=dvec(1)
          Sscut(3)=dvec(1)
         END IF
        END IF
        GO TO 170
c     -----------------------------------------------------------------
c     cutchng argument
c     -----------------------------------------------------------------
   30   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).le.0)THEN
          CALL inpter(PERROR,Errpos,
     &                'Value of cutchng must be greater than zero.',T)
          Inptok=F
         ELSE
          Sscut(4)=dvec(1)
          Sscut(5)=dvec(1)
         END IF
        END IF
        GO TO 170
c     -----------------------------------------------------------------
c     cuttd argument
c     -----------------------------------------------------------------
   40   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).le.0)THEN
          CALL inpter(PERROR,Errpos,
     &               'Value of cuttd must be greater than zero.',T)
          Inptok=F
         ELSE
          Sscut(2)=dvec(1)
         END IF
        END IF
        GO TO 170
c     ------------------------------------------------------------------
c     Outlier Identification argument
c-----------------------------------------------------------------------
   50   CALL gtdcvc(LPAREN,T,1,OTLDIC,otlptr,POTLSS,
     &            'Available options for outlier are no, keep, or yes.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Otlidx=ivec(1)-1
        GO TO 170
c     ------------------------------------------------------------------
c     regARIMA model parameter starting value argument
c-----------------------------------------------------------------------
   60   CALL gtdcvc(LPAREN,T,1,INTDIC,intptr,PINT,
     &            'Available options for fixmdl are no, clear, or yes.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Intidx=ivec(1)-1
        GO TO 170
c     -----------------------------------------------------------------
c     Print argument
c     -----------------------------------------------------------------
   70   CALL getprt(LSPSSP,NSPSSP,Inptok)
        GO TO 170
c     -----------------------------------------------------------------
c     Save argument
c     -----------------------------------------------------------------
   80   CALL getsav(LSPSSP,NSPSSP,Inptok)
        GO TO 170
c-----------------------------------------------------------------------
c     length argument
c-----------------------------------------------------------------------
   90   CALL getivc(LPAREN,T,1,ivec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
        IF(ivec(1).lt.3*Sp)THEN
         CALL inpter(PERROR,Errpos,
     &        'Length of sliding spans must be greater than or equal '//
     &        'to 3 years.',T)
         Inptok=F
        ELSE IF(ivec(1).gt.MXYR*Sp)THEN
         CALL itoc(MXYR,str,ipos)
         IF(Lfatal)RETURN
         CALL inpter(PERROR,Errpos,
     &        'Length of sliding spans must be less than or equal to '//
     &        str(1:(ipos-1))//' years.',T)
         Inptok=F
        ELSE IF(argok)THEN
         Nlen=ivec(1)
        END IF
        GO TO 170
c     ------------------------------------------------------------------
c     x11outlier argument
c-----------------------------------------------------------------------
  100   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for x11outlier are no or yes.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Ssxotl=ivec(1).eq.1
        GO TO 170
c     ------------------------------------------------------------------
c     transparent argument
c-----------------------------------------------------------------------
  110   CALL gtdcvc(LPAREN,.true.,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for transparent are no or yes.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Sstran=ivec(1).eq.1
        GO TO 170
c     -----------------------------------------------------------------
c     Savelog argument
c     -----------------------------------------------------------------
  120   CALL getsvl(LSLSSP,NSLSSP,Inptok)
        GO TO 170
c     ------------------------------------------------------------------
c     regression parameter fixing argument
c-----------------------------------------------------------------------
  130   CALL gtdcvc(LPAREN,T,PFXR,FXRDIC,fxrptr,PFXR,
     &              'Available options for fixreg are td, holiday, '//
     &              'outlier, or user.',Ssfxrg,Nssfxr,T,argok,Inptok)
        IF(Lfatal)RETURN
        GO TO 170
c     ------------------------------------------------------------------
c     additivesa argument
c-----------------------------------------------------------------------
  140   CALL gtdcvc(LPAREN,T,1,ADDDIC,addptr,PADD,
     &    'Available options for additivesa are difference or percent.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Ssdiff=ivec(1).eq.1
        GO TO 170
c-----------------------------------------------------------------------
c     numspans  argument
c-----------------------------------------------------------------------
  150   CALL getivc(LPAREN,T,1,ivec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
        IF(ivec(1).le.1)THEN
         CALL inpter(PERROR,Errpos,
     &               'Value of numspans must be greater than one.',T)
         Inptok=F
        ELSE IF(ivec(1).gt.MXCOL)THEN
         CALL itoc(MXCOL,str,ipos)
         IF(Lfatal)RETURN
         CALL inpter(PERROR,Errpos,
     &              'Value of numspans must be less than or equal to '//
     &               str(1:(ipos-1))//'.',T)
         Inptok=F
        ELSE IF(argok)THEN
         Ncol=ivec(1)
        END IF
        GO TO 170
c     ------------------------------------------------------------------
c     fixx11reg argument
c-----------------------------------------------------------------------
  160   CALL gtdcvc(LPAREN,.true.,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for fixx11reg are no or yes.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Ssxint=ivec(1).eq.1
        GO TO 170
       END IF
       IF(Lfatal)RETURN
c----------------------------------------------------------------------
       Inptok=Inptok.and.argok
       IF(Inptok)Issap=1
c----------------------------------------------------------------------
       RETURN
  170  CONTINUE
      END DO
c     -----------------------------------------------------------------
      END
