c-----------------------------------------------------------------------
c     Include file that keeps track of the files and global varables
c used with input and output.
c-----------------------------------------------------------------------
c Name  Type Description
c-----------------------------------------------------------------------
c ALLFIL  i  PARAMETER for all files to be closed
c Infile  c  PFILCR character name of the command file read off of the
c             command line
c Intfil  l  Initialize the list of possible files to open
c Fillst  i  List of unit numbers of files, the first nfiles are used
c Nfile   i  Number of files currently open
c Opnsin  l  Indicates that STDIN is open to a file and will need to
c             be closed
c Opnsot  l  Indicates that STDOUT is open to a file and will need to
c             be closed
c PFILCR  i  PARAMETER for the maximum number of characters in a
c             file name including path and format title
c STDERR  i  Standard fortran error output
c STDIN   i  Standard fortran input unit
c STDOUT  i  Standard fortran output unit
c-----------------------------------------------------------------------
      integer PFILCR,NPRGNM
      CHARACTER VERNUM*3,PRGNAM*15,SPCSEC*9,LIMSEC*11,RUNSEC*9,MDLSEC*9,
     &          DOCNAM*16
      parameter(PFILCR=512,VERNUM='1.1',PRGNAM='X-13ARIMA-SEATS',
     &          SPCSEC='Section 7',LIMSEC='Section 2.7',
     &          RUNSEC='Section 2',MDLSEC='Section 5',
     &          DOCNAM='Reference Manual',NPRGNM=15)
      integer ALLFIL,PFILE,STDERR,STDIN,STDOUT,PSRS
cfame
cfame      parameter(ALLFIL=-1,STDERR=6,STDIN=5,STDOUT=6,PFILE=200,
cfame     &          PSRS=500)
cdos
      parameter(ALLFIL=-1,STDIN=5,STDOUT=6,PFILE=10,PSRS=10000)
C
C COMMON variables
C
      character*(PFILCR) Infile,Cursrs,Curgrf,Logfil
      LOGICAL Opnsin,Opnsot,Opnudg,Lquiet,Lxhtml
      INTEGER Fillst(PFILE),Nfile,Nfilcr,Ngrfcr,Grfout,Imeta,Nlgfil
      COMMON /cstdio/Nfile,Nfilcr,Ngrfcr,Grfout,Imeta,Fillst,Nlgfil,
     &               STDERR
      COMMON /cstdc/Infile,Cursrs,Curgrf,Logfil
      COMMON /cstdlg/Opnsin,Opnsot,Opnudg,Lquiet,Lxhtml
