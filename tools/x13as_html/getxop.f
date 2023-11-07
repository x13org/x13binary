C     Last Change: Mar. 2021 - update Messages on program flags
C     Last change:  BCM  14 May 1998    9:00 am
      SUBROUTINE getxop(Lmeta,Lchkin,Lcomp,Lsumm,Lmdsum,Lnoprt,Ldata,
     &                  Dtafil,Lgraf,Grfdir,Lcmpaq,Ltimer)
      IMPLICIT NONE
C-----------------------------------------------------------------------
c     Get program options entered on the command line.  This is the
c     PC version of this routine, compiled with Lahey FORTRAN.
C-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'error.cmn'
C-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(T=.true.,F=.false.)
C-----------------------------------------------------------------------
      LOGICAL gtifil,gtofil,Lnoprt,Lmeta,Lchkin,Lcomp,lok,Lmdsum,
     &        Ldata,Lgraf,Ltimer,Lcmpaq
      INTEGER Lsumm,numopt,narg
      CHARACTER arg*(PFILCR),Dtafil*(PFILCR),Grfdir*(PFILCR)
C-----------------------------------------------------------------------
      INTEGER nblank
      EXTERNAL nblank
C-----------------------------------------------------------------------
C     Initialize program execution options
C-----------------------------------------------------------------------
      Lmeta=F
      Ldata=F
      Lchkin=F
      Lcomp=F
      Lsumm=0
      Lmdsum=F
      Lnoprt=F
      gtifil=F
      gtofil=F
      Lgraf=F
      Lcmpaq=F
      Lquiet=F
      Lxhtml=F
      Ltimer=F
      lok=T
C-----------------------------------------------------------------------
C     Initialize argument counter and start processing program arguments
C-----------------------------------------------------------------------
      numopt=1
      CALL setchr(' ',PFILCR,arg)
      CALL getarg(numopt,arg)
      IF(Lfatal)RETURN
      DO WHILE (arg(1:1).ne.' ')
       IF(arg(1:1).eq.'-')THEN
        IF(arg.eq.'-M'.or.arg.eq.'-m')THEN
         IF(Ldata)THEN
          WRITE(STDERR,1010)' ERROR: Cannot specify data (-d) and input 
     &(-m) metafiles in the same run.'
 1010     FORMAT(/,a)
          CALL abend
          RETURN
         ELSE
          Lmeta=T
         END IF
         numopt=numopt+1
         CALL getarg(numopt,Infile)
         IF(Infile(1:1).eq.' '.or.Infile(1:1).eq.'-')THEN
          WRITE(STDERR,1010)' ERROR: An input metafile name must immedia
     &tely follow the -m flag.'
          CALL abend
          RETURN
         END IF          
        ELSE IF(arg.eq.'-D'.or.arg.eq.'-d')THEN
         IF(Lmeta)THEN
          WRITE(STDERR,1010)' ERROR: Cannot specify data (-d) and input 
     &(-m) metafiles in the same run.'
          CALL abend
          RETURN
         ELSE
          Lmeta=T
         END IF
         Ldata=T
         numopt=numopt+1
         CALL getarg(numopt,Dtafil)
         IF(Dtafil(1:1).eq.' '.or.Dtafil(1:1).eq.'-')THEN
          WRITE(STDERR,1010)' ERROR: A data metafile name must immediate
     &ly follow the -d flag.'
          CALL abend
          RETURN
         END IF          
        ELSE IF(arg.eq.'-I'.or.arg.eq.'-i')THEN
         gtifil=T
         numopt=numopt+1
         CALL getarg(numopt,Infile)
         IF(Infile(1:1).eq.' '.or.Infile(1:1).eq.'-')THEN
          WRITE(STDERR,1010)' ERROR: An input spec file name must immedi
     &ately follow the -i flag.'
          CALL abend
          RETURN
         END IF          
        ELSE IF(arg.eq.'-O'.or.arg.eq.'-o')THEN
         gtofil=T
         numopt=numopt+1
         CALL getarg(numopt,Cursrs)
         IF(Cursrs(1:1).eq.' '.or.Cursrs(1:1).eq.'-')THEN
          WRITE(STDERR,1010)' ERROR: An output file name must immediatel
     &y follow the -o flag.'
          CALL abend
          RETURN
         END IF          
        ELSE IF(arg.eq.'-G'.or.arg.eq.'-g')THEN
         Lgraf=T
         numopt=numopt+1
         CALL getarg(numopt,Grfdir)
         IF(Grfdir(1:1).eq.' '.or.Grfdir(1:1).eq.'-')THEN
          WRITE(STDERR,1010)' ERROR: A graphics file directory name must
     & immediately follow the -g flag.'
          CALL abend
          RETURN
         END IF          
        ELSE IF(arg.eq.'-V'.or.arg.eq.'-v')THEN
         Lchkin=T
        ELSE IF(arg.eq.'-C'.or.arg.eq.'-c')THEN
         Lcomp=T
        ELSE IF(arg.eq.'-S'.or.arg.eq.'-s')THEN
         Lsumm=2
         Lmdsum=T
        ELSE IF(arg.eq.'-N'.or.arg.eq.'-n')THEN
         Lnoprt=T
        ELSE IF(arg.eq.'-W'.or.arg.eq.'-w'.or.
     &          arg.eq.'-P'.or.arg.eq.'-p')THEN
         WRITE(STDERR,1021)arg(1:2)
 1021    FORMAT(/,' NOTE: Program option ',a,' no longer defined.',/,
     &            '       This option will be ignored.')
        ELSE IF(arg.eq.'-R'.or.arg.eq.'-r')THEN
         Lcmpaq=T
        ELSE IF(arg.eq.'-Q'.or.arg.eq.'-q')THEN
         Lquiet=T
        ELSE IF(arg.eq.'-X'.or.arg.eq.'-x')THEN
         Lxhtml=T
        ELSE IF(arg.eq.'-T'.or.arg.eq.'-t')THEN
         Ltimer=T
        ELSE IF(arg.eq.'-')THEN
         WRITE(STDERR,1010)' ERROR: No program option specified after th
     &e dash (-).'
         CALL abend
         RETURN
        ELSE
         narg=nblank(arg)
         WRITE(STDERR,1020)arg(1:narg)
 1020    FORMAT(/,' ERROR: Program option ',a,' not defined.  ',
     &            'Valid program options are ',/,'        -I, -O, -M, ',
     &            '-D, -C, -S, -N, -V, -R, -Q, -X, -G, -T.')
         CALL abend
         RETURN
        END IF
       ELSE IF(numopt.eq.1)THEN
        gtifil=T
        Infile=arg
       ELSE IF(numopt.eq.2.and.gtifil)THEN
        gtofil=T
        Cursrs=arg
       ELSE
        narg=nblank(arg)
        WRITE(STDERR,1030)arg(1:narg)
 1030   FORMAT(/,' ERROR: Program option ',a,' not defined; valid ',
     &           'options must be preceded',/,'        by a dash (-): ',
     &           '-I, -O, -M, -D, -C, -S, -N, -V, -R, -Q, -X, -G, -T.')
        CALL abend
        RETURN
       END IF
C-----------------------------------------------------------------------
       numopt=numopt+1
       CALL getarg(numopt,arg)
      END DO
C-----------------------------------------------------------------------
c     If compositing option is selected, check to see if metafile input 
c     is specified; if no, print error message.
C-----------------------------------------------------------------------
      IF(Lcomp.and.(.not.Lmeta))THEN
       WRITE(STDERR,1040)
 1040  FORMAT(/,' ERROR: Must specify metafile input (-m metafile)',
     &        ' when using composite',/,'        option (-c).')
       lok=F
      END IF
      IF(Lcomp.and.Ldata)THEN
       WRITE(STDERR,1050)
 1050  FORMAT(/,' ERROR: Cannot specify data metafile input (-d ',
     &        'datametafile) when using',/,
     &        '        composite option (-c).')
       lok=F
      END IF
C-----------------------------------------------------------------------
c     If input verification option is selected, check to see if the -s, 
c     -c or -n options are present; if so, print an error message.
C-----------------------------------------------------------------------
      IF((Lchkin.and.(Lcomp.OR.(Lsumm.gt.0).or.Lnoprt)).and.
     &   (.not.Lquiet))WRITE(STDERR,1060)
 1060 FORMAT(/,' NOTE: Input verification option (-v) is specified ',
     &         'in the same run as',/,
     &         '       the composite (-c), diagnostic storage (-s),',
     &         'or no print (-n)',/,
     &         '       options. These other flags have been ignored.')
C-----------------------------------------------------------------------
c     If graphics option is specified in a run without the diagnostic 
c     option specified in same run, diagnostic option to true.
C-----------------------------------------------------------------------
      IF(Lgraf.and.Lsumm.eq.0)THEN
       Lsumm=1
       Lmdsum=T
      END IF
C-----------------------------------------------------------------------
c     If timer option is specified in a run without the diagnostic 
c     option specified in same run, diagnostic option to true.
C-----------------------------------------------------------------------
      IF(Ltimer.and.Lsumm.eq.0)THEN
       Lsumm=1
       Lmdsum=T
      END IF
C-----------------------------------------------------------------------
c     Ensure that metafile input and an alternate output filename are
c     not selected in the same run.
C-----------------------------------------------------------------------
      IF(Lmeta)THEN
       IF(gtofil)THEN
        WRITE(STDERR,1070)
 1070   FORMAT(/,' ERROR: Cannot specify metafile input (-m metafile)',
     &         ' when using an ',/,'        alternate output file ',
     &         'name (-o outfile).')
        lok=F
       END IF
C-----------------------------------------------------------------------
       IF(.not.Ldata.and.gtifil)THEN
        WRITE(STDERR,1080)
 1080   FORMAT(/,' ERROR: Cannot specify metafile input (-m metafile)',
     &         ' and single file input',/,'        (-i infile or ',
     &         'infile) in the same run.')
        lok=F
       END IF
      ELSE
C-----------------------------------------------------------------------
c     If this is not a metafile run, set values for the Input and Output
c     file names to default values if they have not already been set.
C-----------------------------------------------------------------------
       IF(.not.gtifil)THEN
        WRITE(STDERR,1090)RUNSEC,PRGNAM,DOCNAM,PRGNAM
 1090   FORMAT(/,' ERROR: Must specify either an input specification ',
     &           'file name',
     &         /,'        (-i infile or infile) or an input metafile ',
     &           'name (-m metafile).',/,
     &         /,'        See ',a,' of the ',a,' ',a,' for more',
     &         /,'        information on how to run ',a,'.')
        lok=F
       END IF
       IF(.not.gtofil)Cursrs=Infile
      END IF
C-----------------------------------------------------------------------
      IF(.not.lok)CALL abend
      RETURN
      END
