C     Last change:  BCM  29 Jan 1999   11:01 am
      SUBROUTINE gtfldt(Plen,Datfil,Ndfl,Havfmt,Datfmt,Nfmt,Ltrim,Y,
     &                  Nobs,Hvfreq,Freq,Hvname,Srsnam,Nser,Havttl,
     &                  Title,Nttlcr,Indec,Hvstrt,Start,Ncol,Begzro,
     &                  Endzro,Lreg,Argok,Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Inputs the series, y, number of observations, nobs, from file
c Datfil and returns the series as best it can.  Assumes y(i)=0 are to
c be trimmed off the end of the series if the input is formatted.
c-----------------------------------------------------------------------
c Input Arguments
c Name  Type Description
c-----------------------------------------------------------------------
c Datfil   c  Name of the file including the path
c Datfmt    c  FORTRAN format of the input including the parentheses
c Plen   i  Maximum length of the series
c-----------------------------------------------------------------------
c Local Arguments
c Name  Type Description
c-----------------------------------------------------------------------
c chnl    i  channel number
c i       i  Do loop index
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'lex.i'
      INCLUDE 'notset.prm'
      INCLUDE 'units.cmn'
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
*      INTEGER YR
*      PARAMETER(YR=1)
c     ------------------------------------------------------------------
      CHARACTER Datfil*(PFILCR),Datfmt*(PFILCR),Srsnam*(64),Title*(*)
      LOGICAL Argok,Havfmt,Hvname,Inptok,Hvstrt,Hvfreq,Lreg,Havttl
      INTEGER chnl,i,Nobs,Plen,Freq,Start,Indec,tstrt,tdiff,Ncol,tmpfrq,
     &        Nser,Nttlcr,Begzro,Endzro,tend,nz,Ndfl,Ltrim,Nfmt
      DOUBLE PRECISION Y
      DIMENSION Y(Plen),Start(2),tstrt(2),Begzro(2),Endzro(2),tend(2)
c-----------------------------------------------------------------------
      INTEGER strinx
      LOGICAL dpeq
      EXTERNAL dpeq,strinx
c-----------------------------------------------------------------------
      CHARACTER X12FMT*84
      INTEGER xfmind,x12ptr,PX12F
      PARAMETER(PX12F=16)
      DIMENSION x12ptr(0:PX12F)
      PARAMETER(X12FMT='1r2r1l2lcansimdatevaluex12savecstramocansim2cs22
     &l2freecommadatevaluecommafreex13save')
      DATA x12ptr/1,3,5,7,9,15,24,31,33,38,45,48,51,60,74,78,85/
c-----------------------------------------------------------------------
c     Set notset values for the input parameters so we know what has
c been input.
c-----------------------------------------------------------------------
      Argok=T
      chnl=NOTSET
      CALL setdp(DNOTST,Plen,Y)
      xfmind=0
c-----------------------------------------------------------------------
c     read the series if it is read from a file, formatted or not.
c Note that if y has been input through the namelist and a file is
c specified the file will be read.  We might want to print a warning.
c-----------------------------------------------------------------------
      CALL fopen(Datfil(1:Ndfl),'data','OLD',chnl,Argok)
      IF(Argok)THEN
c-----------------------------------------------------------------------
c     If there is no format assume free format
c-----------------------------------------------------------------------
       IF(Havfmt)THEN
        xfmind=strinx(F,X12FMT,x12ptr,1,PX12F,Datfmt(1:Nfmt))
        IF(.not.Hvfreq.and.Hvstrt.and.xfmind.ne.9)THEN
         Freq=12
         Hvfreq=T
        END IF
c     ------------------------------------------------------------------
        IF(xfmind.le.0)THEN
         READ(chnl,Datfmt(1:Nfmt),END=30,ERR=10)(Y(i),i=1,Plen)
         GO TO 30
c     ------------------------------------------------------------------
   10    CALL eWritln('Problem reading '//Datfil(1:Ndfl)//
     &                ' using format='//Datfmt(1:Nfmt)//';',
     &                STDERR,Mt2,T,F)
         CALL writln('        the program expects a Fortran format.',
     &               STDERR,Mt2,F,F)
         CALL writln('        Check your input file and format.',
     &               STDERR,Mt2,F,T)
         Argok=F
         Nobs=0
c     ------------------------------------------------------------------
        ELSE IF(xfmind.eq.6.or.xfmind.eq.14)THEN
         CALL gtedit(Plen,Datfil,Ndfl,Y,tstrt,chnl,Nobs,Ncol,Freq,
     &               Srsnam,Nser,xfmind.eq.14,Argok)
c     ------------------------------------------------------------------
        ELSE IF(xfmind.eq.7.or.xfmind.eq.16)THEN
         CALL gtx12s(Plen,Datfil,Ndfl,Y,tstrt,chnl,Nobs,Ncol,Freq,
     &               Srsnam,Nser,Argok)
c     ------------------------------------------------------------------
        ELSE IF(xfmind.eq.9)THEN
         IF(Lreg)THEN
          Argok=F
          CALL inpter(PERRNP,Pos,
     &                'Cannot use the tramo format to read in user-'//
     &                'defined regressors.',T)
         ELSE
          CALL gttrmo(Plen,Datfil,Ndfl,Y,tstrt,chnl,Nobs,tmpfrq,
     &                Havttl,Title,Nttlcr,Hvname,Srsnam,Nser,Argok)
          IF(.not.Hvfreq)THEN
           Freq=tmpfrq
           Hvfreq=T
          ELSE IF(Freq.ne.tmpfrq)THEN
           Argok=F
           CALL inpter(PERRNP,Pos,
     &                 'Seasonal period given in series spec does '//
     &                 'not match seasonal period',F)
           CALL writln('        of series as defined in '//
     &                 Datfil(1:Ndfl)//'.',STDERR,Mt2,F,T)
          END IF
         END IF
c     ------------------------------------------------------------------
        ELSE IF(xfmind.eq.13)THEN
         CALL gtfrcm(Plen,Datfil,Ndfl,Y,chnl,Nobs,Argok)
        ELSE IF(xfmind.eq.15)THEN
         CALL gtfree(Plen,Datfil,Ndfl,Y,chnl,Freq,Nobs,Hvfreq,Hvstrt,
     &               Argok)
        ELSE IF(Hvname)THEN
         IF(Lreg)THEN
          Argok=F
          CALL inpter(PERRNP,Pos,
     &                'Cannot use X-11 formats to read in user-'//
     &                'defined regressors.',T)
         ELSE IF(.not.(Freq.eq.12.or.Freq.eq.4))THEN
          Argok=F
          CALL inpter(PERRNP,Pos,
     &                'Can only use X-11 formats to read monthly or '//
     &                'quarterly data.',T)
         ELSE
          CALL gtx11d(Plen,Freq,Indec,xfmind,chnl,tstrt,tend,Nobs,Y,
     &                Srsnam,Datfil,Ndfl,Argok)
         END IF
c     ------------------------------------------------------------------
        ELSE
         Argok=F
         CALL inpter(PERRNP,Pos,
     &              ' ***Must have series name to use X-11 format***',T)
        END IF
c     ------------------------------------------------------------------
       ELSE
        CALL gtfree(Plen,Datfil,Ndfl,Y,chnl,Freq,Nobs,Hvfreq,Hvstrt,
     &              Argok)
       END IF
      END IF
c-----------------------------------------------------------------------
c     Find the number of input values (nobs).
c-----------------------------------------------------------------------
   30 IF(Argok)THEN
       CALL lendp(Y,Plen,Nobs)
       IF(Havfmt.AND.(xfmind.ne.6.and.xfmind.ne.7.and.xfmind.ne.9.and.
     &                xfmind.lt.13))THEN
        IF(xfmind.eq.0)CALL addate(Start,Freq,Nobs,tend)
        IF(Ltrim.le.1)THEN
         IF(Ltrim.eq.0)THEN
          IF(Freq.eq.4.and.(xfmind.eq.5.or.xfmind.eq.8.or.xfmind.eq.10
     &       .or.xfmind.eq.11))THEN
           nz=4*Freq
          ELSE
           nz=2*Freq
          END IF
         ELSE
          CALL dfdate(tend,Endzro,Freq,nz)
         END IF
         IF(nz.gt.0)THEN
          nz=(nz-1)*Ncol
          IF(nz.gt.Nobs)nz=1
          DO i=Nobs,Nobs-nz,-1
           IF(.not.dpeq(Y(i),0D0))GO TO 40
          END DO
   40     Nobs=i
         END IF
        END IF
c     ------------------------------------------------------------------
        IF(Ltrim.le.1)THEN
         IF(Ltrim.eq.0)THEN
          IF(Freq.eq.4.and.(xfmind.eq.5.or.xfmind.eq.8.or.xfmind.eq.10
     &       .or.xfmind.eq.11))THEN
           nz=4*Freq
          ELSE
           nz=2*Freq
          END IF
         ELSE IF(Hvstrt)THEN
          CALL dfdate(Start,Begzro,Freq,nz)
         ELSE
          CALL dfdate(tstrt,Begzro,Freq,nz)
         END IF
         IF(nz.gt.0)THEN
          IF((nz*Ncol).gt.Ncol)nz=Nobs
          DO i=1,nz*Ncol
           IF(.not.dpeq(Y(i),0D0))GO TO 50
          END DO
   50     Nobs=Nobs-i+1
          IF(i.gt.1)THEN
           CALL copy(Y(i),Nobs,1,Y)
           IF((xfmind.lt.6.and.xfmind.gt.0).or.(xfmind.eq.8).or.
     &        (xfmind.ge.10.and.xfmind.le.12))
     &         CALL addate(tstrt,Freq,i-1,tstrt)
          END IF
         END IF
        END IF
       END IF
c     ------------------------------------------------------------------
       IF(Havfmt.and.xfmind.gt.0.and.xfmind.ne.13.and.xfmind.ne.15)THEN
        IF(Hvstrt)THEN
c-----------------------------------------------------------------------
c     Check if starting date given in series spec is the same as in the
c     series.
c-----------------------------------------------------------------------
         CALL dfdate(tstrt,Start,Freq,tdiff)
         IF(tdiff.ne.0)THEN
          Argok=F
          CALL inpter(PERRNP,Pos,
     &                'Starting date in series spec does not match '//
     &                'starting date of series',F)
          CALL writln('        as defined in '//Datfil(1:Ndfl)//'.',
     &                STDERR,Mt2,F,T)
         END IF
c-----------------------------------------------------------------------
c     If no starting date, set Hvstrt variable to true.
c-----------------------------------------------------------------------
        ELSE
         Hvstrt=T
        END IF
c     ------------------------------------------------------------------
        CALL cpyint(tstrt,2,1,Start)
        IF(.not.Hvfreq)Hvfreq=T
       END IF
c     ------------------------------------------------------------------
       IF(Nobs.eq.0)THEN
        CALL eWritln('Input series is empty.',STDERR,Mt2,T,T)
        Argok=F
       END IF
      END IF
c     ------------------------------------------------------------------
      Inptok=Inptok.and.Argok
      IF(chnl.ne.NOTSET)CALL fclose(chnl)
      RETURN
      END
