C     Last change:  BCM  30 Sep 2005   11:28 am
C
C*PLOTSERIES
C+
C       SUBROUTINE PLOTSERIES(FNAME,SUBTITLE,A,NA,FLAG,XSE)
C
C THIS ROUTINE PROVIDE TO WRITE THE FILE FOR THE GRAPHICS
C IN THE "SERIES" SUBDIRECTORY OF GRAPH.
C
C     FNAME    : CHARACTER*12 file name of the external file
C     SUBTITLE : CHARACTER*50 subtitle of the graph
C     A        : REAL*8 ARRAY the data to plot
C     NA       : INTEGER dimension of A
C     FLAG     : INTEGER option used by graph routine
C     XSE      : REAL*8  option used by graph routine
C
C THIS SUBROUTINE USE ALSO THE FOLLOWING COMMON VARIABLE
C    TITLEG    : CHARACTER*19 the title of the series
C    GRAPHDIR  : CHARACTER*80 the path of the directory graph
C    MQ        : the frequency of the series
C
C THE FOLLOWING IS THE COMMON DECLARATION USED :
C
C        COMMON /DIR/    OUTDIR,GRAPHDIR
C        COMMON /TITL/   TITLEG
C        COMMON /CALFOR/ PSTAR,QSTAR,MQ
C
C--
*      subroutine PLOTSERIES(fname,subtitle,a,na,flag,xse)
*C
*C.. Implicits ..
*      implicit none
*C
*C.. Formal Arguments ..
*C.. In/Out Status: Read, Not Written ..
*      integer na
*C.. In/Out Status: Maybe Read, Maybe Written ..
*      character*30 fname
*C.. In/Out Status: Maybe Read, Not Written ..
*      character*50 subtitle
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 a(na)
*C.. In/Out Status: Maybe Read, Not Written ..
*      integer flag
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 xse
*C
*C.. Local Scalars ..
*      integer i,ireturn,lfname,ngraphdir,nsub
*      character filename*180
*C
*C.. External Functions ..
*      integer ISTRLEN
*      external ISTRLEN
*C
*C.. External Calls ..
*      external CLOSEDEVICE, OPENDEVICE, STRTOLOW
*      include 'calfor.i'
*      include 'sform.i'
*      include 'dirs.i'
*      include 'titl.i'
*C
*C ... Executable Statements ...
*C
*      lfname = ISTRLEN(fname)
*      nsub = ISTRLEN(subtitle)
*      ngraphdir = ISTRLEN(Graphdir)
*      call STRTOLOW(fname)
*cdos
*      filename = Graphdir(1:ngraphdir) // '\\series\\' //
*     &           fname(1:lfname)
*cunix
*c      filename = Graphdir(1:ngraphdir) // '/series/' //
*c     &           fname(1:lfname)
*      call OPENDEVICE(filename,48,0,ireturn)
*      if (ireturn .eq. 0) then
*       write (48,'(I5,/,I1,/,F11.4,/,2X,A)') na, flag, xse, Titleg
*       write (48,'(2X,A,/,I3)') subtitle(1:nsub), Mq
* 7000  format (g16.8)
*       write (48,7000) (a(i), i = 1,na)
*       if (flag.eq.555) then
*        write(48,'(i3)')  nz-na
*       end if
*       call CLOSEDEVICE(48)
*      end if
*      end
*C
*C
*C
*C*PLOTLSERIES
*C+
*C       SUBROUTINE PLOTLSERIES(FNAME,SUBTITLE,A,NA,FLAG,XSE)
*C
*C THIS ROUTINE PROVIDE TO WRITE THE FILE FOR THE GRAPHICS
*C IN THE "SERIES\LOGS" SUBDIRECTORY OF GRAPH.
*C
*C     FNAME    : CHARACTER*12 file name of the external file
*C     SUBTITLE : CHARACTER*50 subtitle of the graph
*C     A        : REAL*8 ARRAY the data to plot
*C     NA       : INTEGER dimension of A
*C     FLAG     : INTEGER option used by graph routine
*C     XSE      : REAL*8  option used by graph routine
*C
*C THIS SUBROUTINE USE ALSO THE FOLLOWING COMMON VARIABLE
*C    TITLEG    : CHARACTER*19 the title of the series
*C    GRAPHDIR  : CHARACTER*80 the path of the directory graph
*C    MQ        : the frequency of the series
*C
*C THE FOLLOWING IS THE COMMON DECLARATION USED :
*C
*C        COMMON /DIR/    OUTDIR,GRAPHDIR
*C        COMMON /TITL/   TITLEG
*C        COMMON /CALFOR/ PSTAR,QSTAR,MQ
*C
*C--
*      subroutine PLOTLSERIES(fname,subtitle,a,na,flag,xse)
*C
*C.. Implicits ..
*      implicit none
*C
*C.. Formal Arguments ..
*C.. In/Out Status: Read, Not Written ..
*      integer na
*C.. In/Out Status: Maybe Read, Maybe Written ..
*      character*30 fname
*C.. In/Out Status: Maybe Read, Not Written ..
*      character*50 subtitle
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 a(na)
*C.. In/Out Status: Maybe Read, Not Written ..
*      integer flag
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 xse
*C
*C.. Local Scalars ..
*      integer i,ireturn,lfname,ngraphdir,nsub
*      character filename*180
*C
*C.. External Functions ..
*      integer ISTRLEN
*      external ISTRLEN
*C
*C.. External Calls ..
*      external CLOSEDEVICE, OPENDEVICE, STRTOLOW
*      include 'calfor.i'
*      include 'sform.i'
*      include 'dirs.i'
*      include 'titl.i'
*C
*C ... Executable Statements ...
*C
*      lfname = ISTRLEN(fname)
*      nsub = ISTRLEN(subtitle)
*      ngraphdir = ISTRLEN(Graphdir)
*      call STRTOLOW(fname)
*cdos
*      filename = Graphdir(1:ngraphdir) // 
*     &                '\\series\\' // fname(1:lfname)
*cunix
*c      filename = Graphdir(1:ngraphdir) // '/series/' //
*c     &           fname(1:lfname)
*      call OPENDEVICE(filename,48,0,ireturn)
*      if (ireturn .eq. 0) then
*       write (48,'(I5,/,I3,/,F11.4,/,2X,A)') na, flag, xse, Titleg
*       write (48,'(2X,A,/,I3)') subtitle(1:nsub), Mq
* 7000  format (g16.8)
*       write (48,7000) (a(i), i = 1,na)
*       if (flag.eq.555) then
*        write(48,'(i3)')  nz-na
*       end if
*       call CLOSEDEVICE(48)
*      end if
*      end
*cc
*c
*cc
*      subroutine PLOTRSERIES(fname,subtitle,a,na,flag,xse)
*C
*C.. Implicits ..
*      implicit none
*C
*C.. Formal Arguments ..
*C.. In/Out Status: Read, Not Written ..
*      integer na
*C.. In/Out Status: Maybe Read, Maybe Written ..
*      character*30 fname
*C.. In/Out Status: Maybe Read, Not Written ..
*      character*50 subtitle
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 a(na)
*C.. In/Out Status: Maybe Read, Not Written ..
*      integer flag
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 xse
*C
*C.. Local Scalars ..
*      integer i,ireturn,lfname,ngraphdir,nsub
*      character filename*180
*C
*C.. External Functions ..
*      integer ISTRLEN
*      external ISTRLEN
*C
*C.. External Calls ..
*      external CLOSEDEVICE, OPENDEVICE, STRTOLOW
*      include 'calfor.i'
*      include 'sform.i'
*      include 'dirs.i'
*      include 'titl.i'
*C
*C ... Executable Statements ...
*C
*      lfname = ISTRLEN(fname)
*      nsub = ISTRLEN(subtitle)
*      ngraphdir = ISTRLEN(Graphdir)
*      call STRTOLOW(fname)
*cdos
*      filename = Graphdir(1:ngraphdir) // 
*     &                '\\series\\' // fname(1:lfname)
*cunix
*c      filename = Graphdir(1:ngraphdir) // '/series/' //
*c     &           fname(1:lfname)
*      call OPENDEVICE(filename,48,0,ireturn)
*      if (ireturn .eq. 0) then
*       write (48,'(I3,/,I3,/,F11.4,/,2X,A)') na, flag, xse, Titleg
*       write (48,'(2X,A,/,I3)') subtitle(1:nsub), Mq
* 7000  format (g16.8)
*       write (48,7000) (a(i), i = 1,na)
*       if (flag.eq.555) then
*        write(48,'(i3)')  nz-na
*       end if
*       call CLOSEDEVICE(48)
*      end if
*      end
*cc
*c
*cc
*      subroutine PLOTSERIESCI(fname,subtitle,a,b,na,flag,xse)
*C
*C.. Implicits ..
*      implicit none
*C
*C.. Formal Arguments ..
*C.. In/Out Status: Read, Not Written ..
*      integer na
*C.. In/Out Status: Maybe Read, Maybe Written ..
*      character*30 fname
*C.. In/Out Status: Maybe Read, Not Written ..
*      character*50 subtitle
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 a(na),b(na)
*C.. In/Out Status: Maybe Read, Not Written ..
*      integer flag
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 xse
*C
*C.. Local Scalars ..
*      integer i,ireturn,lfname,ngraphdir,nsub
*      character filename*180
*C
*C.. External Functions ..
*      integer ISTRLEN
*      external ISTRLEN
*C
*C.. External Calls ..
*      external CLOSEDEVICE, OPENDEVICE, STRTOLOW
*      include 'calfor.i'
*      include 'sform.i'
*      include 'dirs.i'
*      include 'titl.i'
*C
*C ... Executable Statements ...
*C
*      lfname = ISTRLEN(fname)
*      nsub = ISTRLEN(subtitle)
*      ngraphdir = ISTRLEN(Graphdir)
*      call STRTOLOW(fname)
*cdos
*      filename = Graphdir(1:ngraphdir) // '\\series\\' //
*     c           fname(1:lfname)
*cunix
*c      filename = Graphdir(1:ngraphdir) // '/series/' //
*c     &           fname(1:lfname)
*      call OPENDEVICE(filename,48,0,ireturn)
*      if (ireturn .eq. 0) then
*       write (48,'(I3,/,I1,/,F11.4,/,2X,A)') na, flag, xse, Titleg
*       write (48,'(2X,A,/,I3)') subtitle(1:nsub), Mq
* 7000  format (g16.8)
*       write (48,7000) (a(i), i = 1,na)
*       write (48,7000) (a(i)+1.96d0*b(i), i = 1,na)
*       write (48,7000) (a(i)-1.96d0*b(i), i = 1,na)
*       call CLOSEDEVICE(48)
*      end if
*      end
*C
*C
*C*PLOTACF
*C+
*C       SUBROUTINE PLOTACF(FNAME,SUBTITLE,A,NA,FLAG,FLAG1)
*C THIS ROUTINE PROVIDE TO WRITE THE FILE FOR THE GRAPHICS
*C IN THE "ACF" SUBDIRECTORY OF GRAPH.
*C
*C     FNAME    : CHARACTER*12 file name of the external file
*C     SUBTITLE : CHARACTER*50 subtitle of the graph
*C     A        : REAL*8 ARRAY the data to plot
*C     NA       : INTEGER dimension of A
*C     FLAG     : INTEGER option used by graph routine
*C     FLAG1    : INTEGER option used by graph routine
*C
*C THIS SUBROUTINE USE ALSO THE FOLLOWING COMMON VARIABLE
*C    TITLEG    : CHARACTER*19 the title of the series
*C    GRAPHDIR  : CHARACTER*80 the path of the directory graph
*C    MQ        : the frequency of the series
*C
*C THE FOLLOWING IS THE COMMON DECLARATION USED :
*C
*C        COMMON /DIR/    OUTDIR,GRAPHDIR
*C        COMMON /TITL/   TITLEG
*C        COMMON /CALFOR/ PSTAR,QSTAR,MQ
*C--
*      subroutine PLOTACF(fname,subtitle,a,na,flag,flag1)
*C
*C.. Implicits ..
*      implicit none
*C
*C.. Formal Arguments ..
*C.. In/Out Status: Read, Not Written ..
*      integer na
*C.. In/Out Status: Maybe Read, Maybe Written ..
*      character*30 fname
*C.. In/Out Status: Maybe Read, Not Written ..
*      character*50 subtitle
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 a(na)
*C.. In/Out Status: Maybe Read, Not Written ..
*      integer flag
*C.. In/Out Status: Maybe Read, Not Written ..
*      integer flag1
*C
*C.. Local Scalars ..
*      integer i,ireturn,lfname,ngraphdir,nsub
*      character filename*180
*C
*C.. External Functions ..
*      integer ISTRLEN
*      external ISTRLEN
*C
*C.. External Calls ..
*      external CLOSEDEVICE, OPENDEVICE, STRTOLOW
*      include 'calfor.i'
*      include 'dirs.i'
*      include 'titl.i'
*C
*C ... Executable Statements ...
*C
*      lfname = ISTRLEN(fname)
*      nsub = ISTRLEN(subtitle)
*      call STRTOLOW(fname)
*      ngraphdir = ISTRLEN(Graphdir)
*cdos
*      filename = Graphdir(1:ngraphdir) // '\\acf\\' // fname(1:lfname)
*cunix
*c      filename = Graphdir(1:ngraphdir) // '/acf/' // fname(1:lfname)
*      call OPENDEVICE(filename,48,0,ireturn)
*      if (ireturn .eq. 0) then
*       write (48,'(I3,/,I3,/,I3,/,2X,A)') na, flag, flag1, Titleg
*       write (48,'(2X,A,/,I3)') subtitle(1:nsub), Mq
* 7000  format (g16.8)
*       write (48,7000) (a(i), i = 1,na)
*       call CLOSEDEVICE(48)
*      end if
*      end
*C
*C
*C
*C*PLOTACF0
*C+
*C       SUBROUTINE PLOTACF0(FNAME,SUBTITLE,A,NA,FLAG,FLAG1)
*C THIS ROUTINE PROVIDE TO WRITE THE FILE FOR THE GRAPHICS
*C IN THE "ACF" SUBDIRECTORY OF GRAPH.
*C
*C     FNAME    : CHARACTER*12 file name of the external file
*C     SUBTITLE : CHARACTER*50 subtitle of the graph
*C     A        : REAL*8 ARRAY the data to plot which index begin from 0
*C     NA       : INTEGER dimension of A
*C     FLAG     : INTEGER option used by graph routine
*C     FLAG1    : INTEGER option used by graph routine
*C
*C THIS SUBROUTINE USE ALSO THE FOLLOWING COMMON VARIABLE
*C    TITLEG    : CHARACTER*19 the title of the series
*C    GRAPHDIR  : CHARACTER*80 the path of the directory graph
*C    MQ        : the frequency of the series
*C
*C THE FOLLOWING IS THE COMMON DECLARATION USED :
*C
*C        COMMON /DIR/    OUTDIR,GRAPHDIR
*C        COMMON /TITL/   TITLEG
*C        COMMON /CALFOR/ PSTAR,QSTAR,MQ
*C--
*      subroutine PLOTACF0(fname,subtitle,a,na,flag,flag1)
*C
*C.. Implicits ..
*      implicit none
*C
*C.. Formal Arguments ..
*C.. In/Out Status: Read, Not Written ..
*      integer na
*C.. In/Out Status: Maybe Read, Maybe Written ..
*      character*12 fname
*C.. In/Out Status: Maybe Read, Not Written ..
*      character*50 subtitle
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 a(0:na)
*C.. In/Out Status: Maybe Read, Not Written ..
*      integer flag
*C.. In/Out Status: Maybe Read, Not Written ..
*      integer flag1
*C
*C.. Local Scalars ..
*      integer i,ireturn,lfname,ngraphdir,nsub
*      character filename*180
*C
*C.. External Functions ..
*      integer ISTRLEN
*      external ISTRLEN
*C
*C.. External Calls ..
*      external CLOSEDEVICE, OPENDEVICE, STRTOLOW
*      include 'calfor.i'
*      include 'dirs.i'
*      include 'titl.i'
*C
*C ... Executable Statements ...
*C
*      lfname = ISTRLEN(fname)
*      nsub = ISTRLEN(subtitle)
*      call STRTOLOW(fname)
*      ngraphdir = ISTRLEN(Graphdir)
*cdos
*      filename = Graphdir(1:ngraphdir) // '\\acf\\' // fname(1:lfname)
*cunix
*c      filename = Graphdir(1:ngraphdir) // '/acf/' // fname(1:lfname)
*      call OPENDEVICE(filename,48,0,ireturn)
*      if (ireturn .eq. 0) then
*       write (48,'(I3,/,I3,/,I3,/,2X,A)') na, flag, flag1, Titleg
*       write (48,'(2X,A,/,I3)') subtitle(1:nsub), Mq
* 7000  format (g16.8)
*       write (48,7000) (a(i), i = 1,na)
*       call CLOSEDEVICE(48)
*      end if
*      end
*C
*C
*C
*C*PLOTSPECTRUM
*C+
*C       SUBROUTINE PLOTSPECTRUM(FNAME,SUBTITLE,A,NA,FLAG,XSE,withTD)
*C THIS ROUTINE PROVIDE TO WRITE THE FILE FOR THE GRAPHICS
*C IN THE "SPECTRA" SUBDIRECTORY OF GRAPH.
*C
*C     FNAME    : CHARACTER*12 file name of the external file
*C     SUBTITLE : CHARACTER*50 subtitle of the graph
*C     A        : REAL*8 ARRAY the data to plot
*C     NA       : INTEGER dimension of A
*C     FLAG     : INTEGER option used by graph routine
*C     XSE      : REAL*8  option used by graph routine
*C     WithTD   : 1=> With vertical lines in the frequencies of TD
*C
*C THIS SUBROUTINE USE ALSO THE FOLLOWING COMMON VARIABLE
*C    TITLEG    : CHARACTER*19 the title of the series
*C    GRAPHDIR  : CHARACTER*80 the path of the directory graph
*C    MQ        : the frequency of the series
*C
*C THE FOLLOWING IS THE COMMON DECLARATION USED :
*C
*C        COMMON /DIR/    OUTDIR,GRAPHDIR
*C        COMMON /TITL/   TITLEG
*C        COMMON /CALFOR/ PSTAR,QSTAR,MQ
*C--
*      subroutine PLOTSPECTRUM(fname,subtitle,a,nar,flag,xse,withTD)
*C
*C.. Implicits ..
*      implicit none
*C
*C.. Formal Arguments ..
*C.. In/Out Status: Read, Not Written ..
*      real*8 nar
*C.. In/Out Status: Maybe Read, Maybe Written ..
*      character*30 fname
*C.. In/Out Status: Maybe Read, Not Written ..
*      character*50 subtitle
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 a(*)
*C.. In/Out Status: Maybe Read, Not Written ..
*      integer flag,withTD
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 xse
*C
*C.. Local Scalars ..
*      integer i,ireturn,lfname,ngraphdir,nsub,na
*      real*8 pi
*      character filename*180
*C
*C.. External Functions ..
*      integer ISTRLEN
*      external ISTRLEN
*C
*C.. External Calls ..
*      external CLOSEDEVICE, OPENDEVICE, STRTOLOW
*      include 'dirs.i'
*      include 'titl.i'
*C
*C ... Executable Statements ...
*C
*c      na=floor(nar)
*      na=nar
*      pi = acos(-1.0d0)
*      lfname = ISTRLEN(fname)
*      nsub = ISTRLEN(subtitle)
*      call STRTOLOW(fname)
*      ngraphdir = ISTRLEN(Graphdir)
*cdos
*      filename = Graphdir(1:ngraphdir) // '\\spectra\\' //
*     &           fname(1:lfname)
*cunix
*c      filename = Graphdir(1:ngraphdir) // '/spectra/' //
*c     &           fname(1:lfname)
*      call OPENDEVICE(filename,48,0,ireturn)
*      if (ireturn .eq. 0) then
*       write (48,'(F6.2,/,F11.4,/,2X,A)') nar, xse, Titleg
*       write (48,'(2X,A,/,I4,/,g16.8,/,I4)') 
*     $              subtitle(1:nsub), flag, pi,withTD
* 6999  FORMAT(2X,A,/,I4,/,f16.8)
* 7000  format (g16.8)
*       write (48,7000) (a(i), i = 1,na)
*       call CLOSEDEVICE(48)
*      end if
*      end
*
*
*c
*      subroutine PLOTSPCT(fname,subtitle,a,na,peak,npeak,flag,xse,
*     $                     withTD)
*C
*C.. Implicits ..
*      implicit none
*C
*C.. Formal Arguments ..
*C.. In/Out Status: Read, Not Written ..
*      integer na,npeak
*C.. In/Out Status: Maybe Read, Maybe Written ..
*      character*30 fname
*C.. In/Out Status: Maybe Read, Not Written ..
*      character*50 subtitle
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 a(na)
*C.. In/Out Status: Maybe Read, Not Written ..
*      integer flag,withTD,peak(npeak)
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 xse
*C
*C.. Local Scalars ..
*      integer i,ireturn,lfname,ngraphdir,nsub
*      real*8 pi
*      character filename*180
*C
*C.. External Functions ..
*      integer ISTRLEN
*      external ISTRLEN
*C
*C.. External Calls ..
*      external CLOSEDEVICE, OPENDEVICE, STRTOLOW
*      include 'dirs.i'
*      include 'titl.i'
*C
*C ... Executable Statements ...
*C
*      pi = acos(-1.0d0)
*      lfname = ISTRLEN(fname)
*      nsub = ISTRLEN(subtitle)
*      call STRTOLOW(fname)
*      ngraphdir = ISTRLEN(Graphdir)
*cdos
*      filename = Graphdir(1:ngraphdir) // '\\spectra\\' //
*     &           fname(1:lfname)
*cunix
*cunix      filename = Graphdir(1:ngraphdir) // '/spectra/' // fname(1:lfname)
*      call OPENDEVICE(filename,48,0,ireturn)
*      if (ireturn .eq. 0) then
*       write (48,'(I3,/,F11.4,/,2X,A)') na, xse, Titleg
*       write (48,'(2X,A,/,I4,/,g16.8,/,I4)') 
*     $            subtitle(1:nsub), flag, pi, withTD
* 7000  format (g16.8)
*       write (48,7000) (a(i), i = 1,na)
*       write (48,'(i3)') npeak
*       write(8,'(i3)') (peak(i),i=1,npeak)
*       call CLOSEDEVICE(48)
*      end if
*      end
*
*
*
*
*
*C
*C
*C
*C*PLOTFILTERS
*C+
*C       SUBROUTINE PLOTFILTERS(FNAME,SUBTITLE,A,NA,FLAG,XSE)
*C THIS ROUTINE PROVIDE TO WRITE THE FILE FOR THE GRAPHICS
*C IN THE "FILTERS" SUBDIRECTORY OF GRAPH.
*C
*C     FNAME    : CHARACTER*12 file name of the external file
*C     SUBTITLE : CHARACTER*50 subtitle of the graph
*C     A        : REAL*8 ARRAY the data to plot
*C     NA       : INTEGER dimension of A
*C     FLAG     : INTEGER option used by graph routine (MQ)
*C     XSE      : REAL*8  option used by graph routine  (MaxY)
*C     XMAX     : max value in X ordinates
*C     withTD   : (1 with frequency TD vertical lines)
*c      
*C
*C THIS SUBROUTINE USE ALSO THE FOLLOWING COMMON VARIABLE
*C    TITLEG    : CHARACTER*19 the title of the series
*C    GRAPHDIR  : CHARACTER*80 the path of the directory graph
*C    MQ        : the frequency of the series
*C
*C THE FOLLOWING IS THE COMMON DECLARATION USED :
*C
*C        COMMON /DIR/    OUTDIR,GRAPHDIR
*C        COMMON /TITL/   TITLEG
*C        COMMON /CALFOR/ PSTAR,QSTAR,MQ
*C--
*      subroutine PLOTFILTERS(fname,subtitle,a,na,flag,xse,xmax,withTD)
*C
*C.. Implicits ..
*      implicit none
*C
*C.. Formal Arguments ..
*C.. In/Out Status: Read, Not Written ..
*      integer na
*C.. In/Out Status: Maybe Read, Maybe Written ..
*      character*30 fname
*C.. In/Out Status: Maybe Read, Not Written ..
*      character*50 subtitle
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 a(na)
*C.. In/Out Status: Maybe Read, Not Written ..
*      integer flag,withTD
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 xse
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 xmax
*C
*C.. Local Scalars ..
*      integer i,ireturn,lfname,ngraphdir,nsub
*      character filename*180
*C
*C.. External Functions ..
*      integer ISTRLEN
*      external ISTRLEN
*C
*C.. External Calls ..
*      external CLOSEDEVICE, OPENDEVICE, STRTOLOW
*      include 'dirs.i'
*      include 'titl.i'
*C
*C ... Executable Statements ...
*C
*      lfname = ISTRLEN(fname)
*      nsub = ISTRLEN(subtitle)
*      call STRTOLOW(fname)
*      ngraphdir = ISTRLEN(Graphdir)
*cdos
*      filename = Graphdir(1:ngraphdir) // '\\filters\\' //
*     &           fname(1:lfname)
*cunix
*c      filename = Graphdir(1:ngraphdir) // '/filters/' //
*c     &           fname(1:lfname)
*      call OPENDEVICE(filename,48,0,ireturn)
*      if (ireturn .eq. 0) then
*       write (48,'(I4,/,F11.4,/,2X,A)') na, xse,Titleg
*       write (48,'(2X,A,/,I3,/,g16.8,/,I4)') 
*     $             subtitle(1:nsub), flag, xmax, withTD
* 7000  format (g16.8)
*       write (48,7000) (a(i), i = 1,na)
*       call CLOSEDEVICE(48)
*      end if
*      end
*C
*C
*C
*C*PLOTFLT
*C+
*C       SUBROUTINE PLOTFLT(FNAME,SUBTITLE,A,NA,FLAG,FLAG1)
*C
*C THIS ROUTINE PROVIDE TO WRITE THE FILE FOR THE GRAPHICS
*C IN THE "FILTERS" SUBDIRECTORY OF GRAPH.
*C
*C     FNAME    : CHARACTER*12 file name of the external file
*C     SUBTITLE : CHARACTER*50 subtitle of the graph
*C     A        : REAL*8 ARRAY the data to plot
*C     NA       : INTEGER dimension of A
*C     FLAG     : INTEGER option used by graph routine
*C     FLAG1    : INTEGER option used by graph routine
*C                        (this parameter is the difference
*C                         form the subroutine PLOTFILTERS)
*C
*C THIS SUBROUTINE USE ALSO THE FOLLOWING COMMON VARIABLE
*C    TITLEG    : CHARACTER*19 the title of the series
*C    GRAPHDIR  : CHARACTER*80 the path of the directory graph
*C    MQ        : the frequency of the series
*C
*C THE FOLLOWING IS THE COMMON DECLARATION USED :
*C
*C        COMMON /DIR/    OUTDIR,GRAPHDIR
*C        COMMON /TITL/   TITLEG
*C        COMMON /CALFOR/ PSTAR,QSTAR,MQ
*C--
*      subroutine PLOTFLT(fname,subtitle,a,na,flag,flag1)
*C
*C.. Implicits ..
*      implicit none
*C
*C.. Formal Arguments ..
*C.. In/Out Status: Read, Not Written ..
*      integer na
*C.. In/Out Status: Maybe Read, Maybe Written ..
*      character*30 fname
*C.. In/Out Status: Maybe Read, Not Written ..
*      character*50 subtitle
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 a(na)
*C.. In/Out Status: Maybe Read, Not Written ..
*      integer flag
*C.. In/Out Status: Maybe Read, Not Written ..
*      integer flag1
*C
*C.. Local Scalars ..
*      integer i,ireturn,lfname,ngraphdir,nsub
*      character filename*180
*C
*C.. External Functions ..
*      integer ISTRLEN
*      external ISTRLEN
*C
*C.. External Calls ..
*      external CLOSEDEVICE, OPENDEVICE, STRTOLOW
*      include 'calfor.i'
*      include 'dirs.i'
*      include 'titl.i'
*C
*C ... Executable Statements ...
*C
*      lfname = ISTRLEN(fname)
*      nsub = ISTRLEN(subtitle)
*      call STRTOLOW(fname)
*      ngraphdir = ISTRLEN(Graphdir)
*cdos
*      filename = Graphdir(1:ngraphdir) // '\\filters\\' //
*     &           fname(1:lfname)
*cunix
*c      filename = Graphdir(1:ngraphdir) // '/filters/' //
*c     &           fname(1:lfname)
*      call OPENDEVICE(filename,48,0,ireturn)
*      if (ireturn .eq. 0) then
*       write (48,'(I3,/,I3,/I3,/,2X,A)') na, flag, flag1, Titleg
*       write (48,'(2X,A,/,I3)') subtitle(1:nsub), Mq
* 7000  format (g16.8)
*       write (48,7000) (a(i), i = 1,na)
*       call CLOSEDEVICE(48)
*      end if
*      end
*C
*C
*C
*C*PLOTFLT1
*C+
*C       SUBROUTINE PLOTFLT1(FNAME,SUBTITLE,A,NA,LF,FLAG,FLAG1)
*C
*C THIS ROUTINE PROVIDE TO WRITE THE FILE FOR THE GRAPHICS
*C IN THE "FILTERS" SUBDIRECTORY OF GRAPH.
*C
*C     FNAME    : CHARACTER*12 file name of the external file
*C     SUBTITLE : CHARACTER*50 subtitle of the graph
*C     A        : REAL*8 ARRAY the data to plot
*C     NA       : INTEGER dimension of data to plot, 2*NA+1
*C     LF       : INTEGER starting point of the data
*C                        use the data from LF-NA to LF+NA
*C                        (this parameter is the difference
*C                         form the subroutine PLOTFLT)
*C     FLAG     : INTEGER option used by graph routine
*C     FLAG1    : INTEGER option used by graph routine
*C
*C THIS SUBROUTINE USE ALSO THE FOLLOWING COMMON VARIABLE
*C    TITLEG    : CHARACTER*19 the title of the series
*C    GRAPHDIR  : CHARACTER*80 the path of the directory graph
*C    MQ        : the frequency of the series
*C
*C THE FOLLOWING IS THE COMMON DECLARATION USED :
*C
*C        COMMON /DIR/    OUTDIR,GRAPHDIR
*C        COMMON /TITL/   TITLEG
*C        COMMON /CALFOR/ PSTAR,QSTAR,MQ
*C--
*      subroutine PLOTFLT1(fname,subtitle,a,na,lf,flag,flag1)
*C
*C.. Implicits ..
*      implicit none
*C
*C.. Parameters ..
**      INCLUDE 'srslen.prm'
**      integer nfl
**      parameter (nfl = POBS*2)
*C
*C.. Formal Arguments ..
*      integer na,lf,flag,flag1
*      character fname*30,subtitle*50
*      real*8 a(*)
*C
*C.. Local Scalars ..
*      integer i,ireturn,lfname,ngraphdir,nsub
*      integer Nper2, Nyer2
*      character filename*180
*C
*C.. External Functions ..
*      integer ISTRLEN
*      external ISTRLEN
*C
*C.. External Calls ..
*      external CLOSEDEVICE, OPENDEVICE, STRTOLOW
*      include 'calfor.i'
*      include 'sform.i'
*      include 'dirs.i'
*      include 'titl.i'
*C
*C ... Executable Statements ...
*C
*      Nyer2=Nyer
*      Nper2=Nper
*      do i=2,lf-na
*       Nper2=Nper2+1
*       if (Nper2 .gt. Mq) then
*        Nper2 = 1
*        Nyer2 = Nyer2 + 1
*       end if
*      end do
*      lfname = ISTRLEN(fname)
*      nsub = ISTRLEN(subtitle)
*      call STRTOLOW(fname)
*      ngraphdir = ISTRLEN(Graphdir)
*cdos
*      filename = Graphdir(1:ngraphdir) // '\\filters\\' //
*     &           fname(1:lfname)
*cunix
*c      filename = Graphdir(1:ngraphdir) // '/filters/' //
*c     &           fname(1:lfname)
*      call OPENDEVICE(filename,48,0,ireturn)
*      if (ireturn .eq. 0) then
*       write (48,'(I3,/,I3,/I3,/,2X,A)') 2*na+1, flag, flag1, Titleg
*       write (48,'(2X,A,/,I3)') subtitle(1:nsub), Mq
* 7000  format (g16.8)
*       write (48,7000) (a(i), i = lf-na+1,lf+na+1)
*       call CLOSEDEVICE(48)
*      end if
*      end
*C
*C
*C*PLOTFCAST
*C+
*C       SUBROUTINE PLOTFCAST (FNAME,SUBTITLE,A,NA,LF,FLAG)
*C
*C THIS ROUTINE PROVIDE TO WRITE THE FILE FOR THE GRAPHICS
*C IN THE "FORECAST" SUBDIRECTORY OF GRAPH.
*C
*C     FNAME    : CHARACTER*12 file name of the external file
*C     SUBTITLE : CHARACTER*50 subtitle of the graph
*C     A        : REAL*8 ARRAY the data to plot
*C     NA       : INTEGER dimension of data to plot, 2*NA+1
*C     LF       : INTEGER starting point of the data
*C                        use the data from LF-NA to LF+NA
*C     FLAG     : INTEGER option used by graph routine
*C
*C THIS SUBROUTINE USE ALSO THE FOLLOWING COMMON VARIABLE
*C    TITLEG    : CHARACTER*19 the title of the series
*C    GRAPHDIR  : CHARACTER*80 the path of the directory graph
*C    MQ        : the frequency of the series
*C
*C THE FOLLOWING IS THE COMMON DECLARATION USED :
*C
*C        COMMON /DIR/    OUTDIR,GRAPHDIR
*C        COMMON /TITL/   TITLEG
*C        COMMON /CALFOR/ PSTAR,QSTAR,MQ
*C--
*      subroutine PLOTFCAST(fname,subtitle,a,na,lf,flag)
*C
*C.. Implicits ..
*      implicit none
*C
*C.. Formal Arguments ..
*C.. In/Out Status: Read, Not Written ..
*      integer lf
*C.. In/Out Status: Read, Not Written ..
*      integer na
*C.. In/Out Status: Maybe Read, Maybe Written ..
*      character*30 fname
*C.. In/Out Status: Maybe Read, Not Written ..
*      character*50 subtitle
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 a(lf+na)
*C.. In/Out Status: Maybe Read, Not Written ..
*      integer flag
*C
*C.. Local Scalars ..
*      integer i,ireturn,lfname,ngraphdir,nsub
*      character filename*180
*      integer Nper2, Nyer2
*C
*C.. External Functions ..
*      integer ISTRLEN
*      external ISTRLEN
*C
*C.. External Calls ..
*      external OPENDEVICE, STRTOLOW
*      include 'calfor.i'
*      include 'sform.i'
*      include 'dirs.i'
*      include 'titl.i'
*C
*C ... Executable Statements ...
*C
*      Nyer2=Nyer
*      Nper2=Nper
*      do i=2,flag-na
*       Nper2=Nper2+1
*       if (Nper2 .gt. Mq) then
*        Nper2 = 1
*        Nyer2 = Nyer2 + 1
*       end if
*      end do
*      lfname = ISTRLEN(fname)
*      nsub = ISTRLEN(subtitle)
*      call STRTOLOW(fname)
*      ngraphdir = ISTRLEN(Graphdir)
*cdos
*      filename = Graphdir(1:ngraphdir) // '\\forecast\\' //
*     $           fname(1:lfname)
*cunix
*c      filename = Graphdir(1:ngraphdir) // '/forecast/' //
*c     $           fname(1:lfname)
*      call OPENDEVICE(filename,48,0,ireturn)
*      if (ireturn .eq. 0) then
*       write (48,'(I3,/,I3,/I3,/,2X,A)') 2*na+1, lf, flag, Titleg
*       write (48,'(2X,A,/,I3)') subtitle(1:nsub), Mq
* 7000  format (g16.8)
*       write (48,7000) (a(i), i = lf-na,lf+na)
*       close (8)
*      end if
*      end
*C
*C*PLOTFCAST1
*C+
*C       SUBROUTINE PLOTFCAST1 (FNAME,SUBTITLE,A,NA,FLAG,FLAG1)
*C
*C THIS ROUTINE PROVIDE TO WRITE THE FILE FOR THE GRAPHICS
*C IN THE "FORECAST" SUBDIRECTORY OF GRAPH.
*C
*C     FNAME    : CHARACTER*12 file name of the external file
*C     SUBTITLE : CHARACTER*50 subtitle of the graph
*C     A        : REAL*8 ARRAY the data to plot
*C     NA       : INTEGER dimension of A(-NA:NA),
*C                        the data to plot are 2*NA+1
*C     FLAG     : INTEGER option used by graph routine
*C     FLAG1    : INTEGER option used by graph routine
*C
*C THIS SUBROUTINE USE ALSO THE FOLLOWING COMMON VARIABLE
*C    TITLEG    : CHARACTER*19 the title of the series
*C    GRAPHDIR  : CHARACTER*80 the path of the directory graph
*C    MQ        : the frequency of the series
*C
*C THE FOLLOWING IS THE COMMON DECLARATION USED :
*C
*C        COMMON /DIR/    OUTDIR,GRAPHDIR
*C        COMMON /TITL/   TITLEG
*C        COMMON /CALFOR/ PSTAR,QSTAR,MQ
*C
*C--
*      subroutine PLOTFCAST1(fname,subtitle,a,na,flag,flag1)
*C
*C.. Implicits ..
*      implicit none
*      INCLUDE 'srslen.prm'
*      integer kp
*      parameter (kp = PFCST)
*C
*C.. Formal Arguments ..
*C.. In/Out Status: Maybe Read, Maybe Written ..
*      character*30 fname
*C.. In/Out Status: Maybe Read, Not Written ..
*      character*50 subtitle
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 a(-kp:kp)
*C.. In/Out Status: Maybe Read, Not Written ..
*      integer na
*C.. In/Out Status: Maybe Read, Not Written ..
*      integer flag
*C.. In/Out Status: Maybe Read, Not Written ..
*      integer flag1
*C
*C.. Local Scalars ..
*      integer i,ireturn,lfname,ngraphdir,nsub
*      character filename*180
*C
*C.. External Functions ..
*      integer ISTRLEN
*      external ISTRLEN
*C
*C.. External Calls ..
*      external CLOSEDEVICE, OPENDEVICE, STRTOLOW
*      include 'calfor.i'
*      include 'dirs.i'
*      include 'titl.i'
*C
*C ... Executable Statements ...
*C
*      lfname = ISTRLEN(fname)
*      nsub = ISTRLEN(subtitle)
*      call STRTOLOW(fname)
*      ngraphdir = ISTRLEN(Graphdir)
*cdos
*      filename = Graphdir(1:ngraphdir) // '\\forecast\\' //
*     $           fname(1:lfname)
*cunix
*c      filename = Graphdir(1:ngraphdir) // '/forecast/' //
*c     $           fname(1:lfname)
*      call OPENDEVICE(filename,48,0,ireturn)
*      if (ireturn .eq. 0) then
*       write (48,'(I3,/,I3,/I3,/,2X,A)') 2*na+1, flag, flag1, Titleg
*       write (48,'(2X,A,/,I3)') subtitle(1:nsub), Mq
* 7000  format (g16.8)
*       write (48,7000) (a(i), i = (-na),na)
*       call CLOSEDEVICE(48)
*      end if
*      end
*C
*C*PLOTFCAST2
*C+
*C       SUBROUTINE PLOTFCAST2 (FNAME,SUBTITLE,A,NA,FLAG,FLAG1)
*C
*C THIS ROUTINE PROVIDE TO WRITE THE FILE FOR THE GRAPHICS
*C IN THE "FORECAST" SUBDIRECTORY OF GRAPH.
*C
*C     FNAME    : CHARACTER*12 file name of the external file
*C     SUBTITLE : CHARACTER*50 subtitle of the graph
*C     A        : REAL*8 MATRIX the data to plot A(51,3)
*C     NA       : INTEGER the data to plot are (2*NA+1)*3
*C     FLAG     : INTEGER option used by graph routine
*C     FLAG1    : INTEGER option used by graph routine
*C
*C THIS SUBROUTINE USE ALSO THE FOLLOWING COMMON VARIABLE
*C    TITLEG    : CHARACTER*19 the title of the series
*C    GRAPHDIR  : CHARACTER*80 the path of the directory graph
*C    MQ        : the frequency of the series
*C
*C THE FOLLOWING IS THE COMMON DECLARATION USED :
*C
*C        COMMON /DIR/    OUTDIR,GRAPHDIR
*C        COMMON /TITL/   TITLEG
*C        COMMON /CALFOR/ PSTAR,QSTAR,MQ
*C
*C--
*      subroutine PLOTFCAST2(fname,subtitle,a,na,flag,flag1)
*C
*C.. Implicits ..
*      implicit none
*      INCLUDE 'srslen.prm'
*      integer kp
*      parameter (kp = PFCST)
*C
*C.. Formal Arguments ..
*C.. In/Out Status: Maybe Read, Maybe Written ..
*      character*30 fname
*C.. In/Out Status: Maybe Read, Not Written ..
*      character*50 subtitle
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 a(2*kp+1,3)
*C.. In/Out Status: Maybe Read, Not Written ..
*      integer na
*C.. In/Out Status: Maybe Read, Not Written ..
*      integer flag
*C.. In/Out Status: Maybe Read, Not Written ..
*      integer flag1
*C
*C.. Local Scalars ..
*      integer i,ireturn,j,lfname,ngraphdir,nsub
*      character filename*180
*      integer Nper2, Nyer2
*C
*C.. External Functions ..
*      integer ISTRLEN
*      external ISTRLEN
*C
*C.. External Calls ..
*      external CLOSEDEVICE, OPENDEVICE, STRTOLOW
*      include 'dirs.i'
*      include 'calfor.i'
*      include 'sform.i'
*      include 'titl.i'
*C
*C ... Executable Statements ...
*C
*      Nyer2=Nyer
*      Nper2=Nper
*      do i=2,flag-na
*       Nper2=Nper2+1
*       if (Nper2 .gt. Mq) then
*        Nper2 = 1
*        Nyer2 = Nyer2 + 1
*       end if
*      end do
*      lfname = ISTRLEN(fname)
*      nsub = ISTRLEN(subtitle)
*      call STRTOLOW(fname)
*      ngraphdir = ISTRLEN(Graphdir)
*cdos
*      filename = Graphdir(1:ngraphdir) // '\\forecast\\' //
*     $           fname(1:lfname)
*cunix
*c      filename = Graphdir(1:ngraphdir) // '/forecast/' //
*c     $           fname(1:lfname)
*      call OPENDEVICE(filename,48,0,ireturn)
*      if (ireturn .eq. 0) then
*       write (48,'(I3,/,I3,/I3,/,2X,A)') 2*na+1, flag, flag1, Titleg
*       write (48,'(2X,A,/,I3)') subtitle(1:nsub), na/2
*       do j = 1,3
* 7000   format (g16.8)
*        write (48,7000) (a(i,j), i = (kp-na),kp+na)
*       end do
*       call CLOSEDEVICE(48)
*      end if
*      end
*cc
*c
*cc
*      subroutine PLOTFCAST3(fname,subtitle,a,na,flag,flag1)
*C
*C.. Implicits ..
*      implicit none
*C
*C.. Parameters ..
*      INCLUDE 'srslen.prm'
*      integer kp
*      parameter (kp = PFCST)
*C
*C.. Formal Arguments ..
*C.. In/Out Status: Maybe Read, Maybe Written ..
*      character*30 fname
*C.. In/Out Status: Maybe Read, Not Written ..
*      character*50 subtitle
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 a(-kp:kp)
*C.. In/Out Status: Maybe Read, Not Written ..
*      integer na
*C.. In/Out Status: Maybe Read, Not Written ..
*      integer flag
*C.. In/Out Status: Maybe Read, Not Written ..
*      integer flag1
*C
*C.. Local Scalars ..
*      integer i,ireturn,lfname,ngraphdir,nsub
*      character filename*180
*      integer Nper2, Nyer2
*C
*C.. External Functions ..
*      integer ISTRLEN
*      external ISTRLEN
*C
*C.. External Calls ..
*      external CLOSEDEVICE, OPENDEVICE, STRTOLOW
*      include 'calfor.i'
*      include 'sform.i'
*      include 'dirs.i'
*      include 'titl.i'
*C
*C ... Executable Statements ...
*C
*      Nyer2=Nyer
*      Nper2=Nper
*      do i=2,flag-na
*       Nper2=Nper2+1
*       if (Nper2 .gt. Mq) then
*        Nper2 = 1
*        Nyer2 = Nyer2 + 1
*       end if
*      end do
*      lfname = ISTRLEN(fname)
*      nsub = ISTRLEN(subtitle)
*      call STRTOLOW(fname)
*      ngraphdir = ISTRLEN(Graphdir)
*cdos
*      filename = Graphdir(1:ngraphdir) // '\\forecast\\' //
*     $           fname(1:lfname)
*cunix
*cunix      filename = Graphdir(1:ngraphdir) // '/forecast/' //
*cunix     $           fname(1:lfname)
*      call OPENDEVICE(filename,48,0,ireturn)
*      if (ireturn .eq. 0) then
*       write (48,'(I3,/,I3,/I3,/,2X,A)') 2*na+1, flag, flag1, Titleg
*       write (48,'(2X,A,/,I3)') subtitle(1:nsub), Mq
*CUNX#ifdef TSW
*!DEC$ IF DEFINED (TSW)
*       write (48,'(2X,I3,/,I4,/,I3)') Nper2, Nyer2, Mq
*CUNX#end if
*!DEC$ end if
* 7000  format (g16.8)
*       write (48,7000) (a(i), i = (-na),na)
*       call CLOSEDEVICE(48)
*      end if
*      end
C
C*ISTRLEN
C+
C       INTEGER FUNCTION ISTRLEN (STRING)
C
C THIS FUNCTION COMPUTE THE LENGTH OF THE CHARACTER ARRAY STRING
C WITHOUT THE TRAILING LAST BLANK CHARACTER
C
C    STRING : CHARACTER*(*)
C
C--
      integer function ISTRLEN(string)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Not Written ..
      character*(*) string
C
C.. Local Scalars ..
      integer i,lstr
C
C.. Intrinsic Functions ..
      intrinsic LEN,ACHAR,ichar
C
C ... Executable Statements ...
C
      lstr = LEN(string)
      if (lstr.eq.0) then
        ISTRLEN=0
        return
      end if
      i=1
      do while( ichar(string(i:i)) .ne.0 .and. i.lt.lstr)
        i=i+1
      end do
      do while (string(i:i) .eq. ' '.or.
     $          string(i:i).eq.achar(0))
       i = i - 1
       if (i .eq. 0) goto 5000
      end do
 5000 ISTRLEN = i
      end
C
C*STRTOLOW
C+
C       SUBROUTINE STRTOLOW (STRING)
C
C THIS SUBROUTINE TRANSFORM ALL THE CHARACTER OF THE INPUT
C CHARACTER ARRAY IN LOWER CASE
C
C   STRING : CHARACTER*(*)
C
C--
      subroutine STRTOLOW(string)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Maybe Written ..
      character*(*) string
C
C.. Local Scalars ..
      integer i,iasc,j
C
C.. External Functions ..
      integer ISTRLEN
      external ISTRLEN
C
C.. Intrinsic Functions ..
      intrinsic CHAR, ICHAR
C
C ... Executable Statements ...
C
      j = ISTRLEN(string)
      do i = 1,j
       iasc = ICHAR(string(i:i))
       if ((iasc.gt.64) .and. (iasc.lt.91)) then
        string(i:i) = CHAR(iasc+32)
       end if
      end do
      end
C
C
C
C*OPENDEVICE
C+
C       SUBROUTINE OPENDEVICE(FILENAME,DEVNUM,icheck,IRETURN)
C
C THIS SUBROUTINE TRY TO OPEN A FILE WITH A SPECIFIC DEVICE
C NUMBER AND WRITES IN THE FIRST LINE ITS VERSION AND Building DATE.
C
C    FILENAME : CHARACTER*180 file name
C    DEVNUM   : INTEGER device number to open
C    icheck    : INTEGER 1 check if file exist, 0 no check
C    IRETURN  : RETURN VALUE  :
C                               0 ok
C                               1 an error condition during the open
C                               2 check if file exist failed
C
C
C--
      subroutine OPENDEVICE(filename,devnum,icheck,ireturn)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Not Written ..
      character*180 filename
C.. In/Out Status: Maybe Read, Not Written ..
      integer devnum
C.. In/Out Status: Read, Not Written ..
      integer icheck
C.. In/Out Status: Not Read, Overwritten ..
      integer ireturn
C
C.. Local Scalars ..
      integer err,flen
      logical bool
C.. Local Arrays ..
C
      character dir*180
C.. External Functions ..
      integer ISTRLEN
      external ISTRLEN
      include 'stream.i'
      include 'dirs.i'
      include 'build.i'
C
C ... Executable Statements ...
C
      if (icheck .ne. 2) then
       close(devnum)
      end if
      inquire (unit=devnum,opened=bool)
      if (bool) then
       return
      end if
      flen = ISTRLEN(filename)
*      call STRTOLOW(filename)
      if (icheck .eq. 1) then
       inquire (FILE = filename(1:flen),EXIST = bool)
       if (.not. bool) then
        ireturn = 2
        return
       end if
      end if
      if (icheck.eq.2) then
       inquire (FILE = filename(1:flen),EXIST = bool)
       if (.not. bool) then
        open (devnum,FILE = filename(1:flen),IOSTAT = err)
        if (err .ne. 0) then
         ireturn = 1
        end if
       else 
CUNX#ifdef TSW
!DEC$ IF DEFINED (TSW)
c        write(Nprof,*)'  devnum(append) = ',devnum
cdos
        open(devnum,file=filename(1:flen),status='old',
     $                              position='append')
cunix
cunix        open(devnum,file=filename(1:flen),status='old')
        dir = outdir
*        call STRTOLOW(dir)
cc
c Write the Build date in the Output files 
cc
        if (index(filename,dir(1:ISTRLEN(dir))) .ge. 1) then
         write ( Devnum,'(2x,''*** Seats Build date :'',A,'' ***'')')
     $           CompDate
        end if

CUNX#else
!DEC$ ELSE
        open(devnum,file=filename(1:flen))
        dir = outdir
*        call STRTOLOW(dir)
cc
c Write the Build date in the Output files 
cc
        if (index(filename,dir(1:ISTRLEN(dir))) .ge. 1) then
         write ( Devnum,'(2x,''*** Seats Build date :'',A,'' ***'')')
     $           CompDate
        end if
CUNX#end if
!DEC$ end if
        ireturn=0
       end if
       return
      end if
      open (devnum,FILE = filename(1:flen),IOSTAT = err)
      if (err .ne. 0) then
       ireturn = 1
      else
       ireturn = 0
      end if
      if (ireturn .eq. 0) then
        dir = outdir
*        call STRTOLOW(dir)
cc
c Write the Build date in the Output files 
cc
*        if (index(filename,dir(1:ISTRLEN(dir))) .ge. 1) then
*         if (HTML .ne. 1) then
*         write ( Devnum,'(2x,''*** Seats Build date :'',A,'' ***'')')
*     $           CompDate
*        end if
*        end if
      end if
CUNX#end if
c!DEC$ end if
      return
      end
      logical function isopen(ndevice)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
      integer ndevice
C
C.. Local Scalars ..
      logical qopen
      inquire (unit=ndevice,opened=qopen)
      isopen=qopen
      return
      end
C
C*CLOSEDEVICE
C+
C       SUBROUTINE CLOSEDEVICE(IDEVICE)
C
C  THIS SUBROUTINE PROVIDE TO CLOSE THE DEVICE ASSOCIATED TO THE
C  DEVICE NUMBER IDEVICE
C
C    IDEVICE : INTEGER the device number to close
C--
      subroutine CLOSEDEVICE(idevice)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
C.. In/Out Status: Read, Not Written ..
      integer idevice
C
C ... Executable Statements ...
C
      close (idevice)
      end
C
C
      subroutine CLOSEDEVICE2(idevice)
C
C.. Implicits ..
      implicit none
c
      include 'stream.i'
C
C.. Formal Arguments ..
C.. In/Out Status: Read, Not Written ..
      integer idevice
C 
C.. External Functions .. 
      logical isopen
      external isopen
C
C ... Executable Statements ...
C
      if (.not. isopen(idevice)) then
        return
      end if
      CALL writTag(idevice,'</body>')
      CALL writTag(idevice,'</html>')
      close (idevice)
      end
C*OPENDEVSCRATCH
C+
C       SUBROUTINE OPENDEVSCRATCH(DEVNUM)
C
C THIS SUBROUTINE TRY TO OPEN A SCRATCH FILE WITH A SPECIFIC DEVICE
C NUMBER.
C
C    DEVNUM   : INTEGER device number to open
C
C--
      subroutine OPENDEVSCRATCH(devnum)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
C.. In/Out Status: Read, Not Written ..
      integer devnum
*      integer ipos
*      character*3 devstr
C
C ... Executable Statements ...
C
      close(devnum)
      open (devnum,STATUS = 'SCRATCH')
*      ipos=1
*      CALL itoc(devnum,devstr,ipos)
*      open(devnum,file="unit"//devstr(1:(ipos-1)),status="UNKNOWN")
      end
C
CC
C
CC
C THIS SUBROUTINE TRY TO OPEN A FILE WITH A SPECIFIC DEVICE
C NUMBER THE FILE NAME IS NOT TRANSFORMED IN LOWERCASE
C
C    FILENAME : CHARACTER*180 file name
C    DEVNUM   : INTEGER device number to open
C    CHECK    : INTEGER 1 check if file exist, 0 no check
C    IRETURN  : RETURN VALUE  :
C                               0 ok
C                               1 an error condition during the open
C                               2 check if file exist failed
C
C
C--
      subroutine OPENDEVICEASIS(filename,devnum,icheck,ireturn)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Not Written ..
      character*180 filename
C.. In/Out Status: Maybe Read, Not Written ..
      integer devnum
C.. In/Out Status: Read, Not Written ..
      integer icheck
C.. In/Out Status: Not Read, Overwritten ..
      integer ireturn
C
C.. Local Scalars ..
      integer err,flen
      logical bool
C
C.. External Functions ..
      integer ISTRLEN
      external ISTRLEN
C
C ... Executable Statements ...
C
      close(devnum)
CUNX#ifdef NBB && !DEBUG
c!DEC$ IF DEFINED (NBB) .AND. .NOT. DEFINED (DEBUG)
       call OPENDEVSCRATCH(devnum)
CUNX#else
c!DEC$ ELSE
      flen = ISTRLEN(filename)
      if (icheck .eq. 1) then
       inquire (FILE = filename(1:flen),EXIST = bool)
       if (.not. bool) then
        ireturn = 2
        return
       end if
      end if
      if (icheck.eq.2) then
       inquire (FILE = filename(1:flen),EXIST = bool)
       if (.not. bool) then
        open (devnum,FILE = filename(1:flen),IOSTAT = err)
        if (err .ne. 0) then
         ireturn = 1
        end if
       else
CUNX#ifdef TSW
c!DEC$ IF DEFINED (TSW)
c        open(devnum,file=filename(1:flen),status='old',
c     $                              position='append')
CUNX#else
c!DEC$ ELSE
        open(devnum,file=filename(1:flen))
CUNX#end if
c!DEC$ end if
        ireturn=0
       end if
       return
      end if
      open (devnum,FILE = filename(1:flen),IOSTAT = err)
      if (err .ne. 0) then
       ireturn = 1
      else
       ireturn = 0
      end if
CUNX#end if
c!DEC$ end if
      return
      end
cc
c
cc
*      subroutine PlotPureMA(oz,sa,p,s,trans,ir,iter,out,ioneout,title,
*     $                      ntitle) 
*C.. Parameters ..
*      include 'srslen.prm'
*      integer kl,mp,kp
*      parameter (kl = 80,kp = PFCST, mp = POBS)      
*c Formal parameters  
*      real*8 oz(mp+kp),sa(mp+kp),p(mp+kp),s(mp+kp),trans(mp+kp),
*     $       ir(mp+kp)
*      integer out,iter,ioneout,ntittle
*      character TITLE*80
*      include 'preadtr.i'
*      include 'calfor.i'
*      include 'sform.i'
*      include 'dirs.i'
*      include 'titl.i'
*C Local variables
*      character subtitle*50,fname*30
*c.. 
*c
*      if (tramo.gt.0) then
*       if (iter.eq.0) then
*        if (out.lt.3) then
*         if ( Neast.ne.0 .or. Neff(2).ne.0 .or.
*     $      Neff(0).ne.0 .or. Nous.ne.0 .or. Npatd.ne.0) then
*          fname = 'SAFIN.T'
*          subtitle = 'FINAL SA SERIES'
*          call PLOTSERIES(fname,subtitle,sa,nz,1,0.0d0)
*          fname = 'Sasadjo.t'
*          subtitle = 'STOCHASTIC SA SERIES'
*          call PLOTSERIES(fname,subtitle,oz,nz,1,0.0d0)              
*         else
*          fname = 'Sasadjo.t'
*          subtitle = 'FINAL SA SERIES'
*          call PLOTSERIES(fname,subtitle,oz,nz,1,0.0d0)        
*         end if        
*         if (Noutr.ne.0 .or. Neff(1).ne.0 .or. Neff(7).ne.0) then
*          fname = 'TRFIN.T'
*          subtitle = 'FINAL TREND-CYCLE'
*          call PLOTSERIES(fname,subtitle,p,nz,1,0.0d0)
*         end if 
*         if (Neast.ne.0.or.Neff(2).ne.0.or.Npatd.ne.0.or.Nous.ne.0) 
*     $       then
*          fname = 'SFIN.T'
*          subtitle = 'FINAL SEASONAL'
*          call PLOTSERIES(fname,subtitle,s,nz,1,0.0d0)
*         end if
*         if (Neff(5).eq.1) then
*          fname = 'TRAFIN.T'
*          subtitle = 'FINAL TRANSITORY COMPONENT'
*          call PLOTSERIES(fname,subtitle,trans,nz,1,0.0d0)
*         end if
*         if (Nouir.ne.0 .or. Neff(3).ne.0) then
*          fname = 'IRFIN.T'
*          subtitle = 'FINAL IRREGULAR'
*          call PLOTSERIES(fname,subtitle,ir,nz,1,0.0d0)
*         end if
*        end if
*       else
*        if (out.lt.2 .and. ioneout.eq.0) then
*         if ( Neast.ne.0 .or. Neff(2).ne.0 .or.
*     $    Neff(0).ne.0 .or. Nous.ne.0 .or. Npatd.ne.0) then
*          fname = title(1:ntitle) // '.SA'
*          subtitle = 'FINAL SA SERIES'
*          call PLOTSERIES(fname,subtitle,sa,nz,1,0.0d0)
*          write (17,'(A)') fname
*         end if
*         if (Noutr.ne.0 .or. Neff(1).ne.0 .or. Neff(7).ne.0) then
*          fname = title(1:ntitle) // '.TRE'
*          subtitle = 'FINAL TREND-CYCLE'
*          call PLOTSERIES(fname,subtitle,p,nz,1,0.0d0)
*         end if 
*         if (Neast.ne.0.or.Neff(2).ne.0.or.Npatd.ne.0.or.Nous.ne.0) 
*     $    then
*          fname = title(1:ntitle) // '.SF'
*          subtitle = 'FINAL SEASONAL'
*          call PLOTSERIES(fname,subtitle,s,nz,1,0.0d0)
*         end if     
*         if (Nouir.ne.0 .or. Neff(3).ne.0) then
*          fname = title(1:ntitle) // '.FIR'
*          subtitle = 'FINAL IRREGULAR'
*          call PLOTSERIES(fname,subtitle,ir,nz,1,0.0d0)
*         end if
*        end if     
*        if (out.eq.0) then 
*         if (Neff(5).eq.1) then
*          fname = title(1:ntitle) // '.CYC'
*          subtitle = 'FINAL TRANSITORY COMPONENT'
*          call PLOTSERIES(fname,subtitle,trans,nz,1,0.0d0)
*         end if
*        end if
*       end if
*      else
*       if (iter.eq.0) then
*        if (out.lt.3) then
*         fname = 'Sasadjo.t'
*         subtitle = 'FINAL SA SERIES'
*         call PLOTSERIES(fname,subtitle,oz,nz,1,0.0d0)  
*        end if
*       else
*        if (out.lt.2 .and. ioneout.eq.0) then
*         fname = title(1:ntitle) // '.SA'
*          subtitle = 'FINAL SA SERIES'
*          call PLOTSERIES(fname,subtitle,sa,nz,1,0.0d0)
*          write (17,'(A)') fname
*        end if
*       end if
*      end if
*      end
*cc
*c
*cc
*      subroutine PlotFitted(serie,eres,nz,nres,lam,nyear,nper,mq)
*      include 'srslen.prm'
*      integer kp,mp
*      parameter (kp = PFCST, mp = POBS)
*      integer nz,nres,lam,nyear,nper,mq
*      real*8 serie(mp+kp),eres(mp)
*C.. Local Scalars ..
*      integer k,i,na,ifault
*      character filename*180,subtitle*50,fname*30
*C
*C.. External Functions ..
*      integer ISTRLEN
*      external ISTRLEN
*C
*C.. External Calls ..
*      external CLOSEDEVICE, OPENDEVICE, STRTOLOW
*      include 'dirs.i'
*      include 'titl.i'
*c
*c
*        fname = 'FITT.T'
*        subtitle = 'FITTED VALUE'
*        call STRTOLOW(fname)
*cdos
*        filename = GRAPHDIR(1:ISTRLEN(GRAPHDIR)) // '\\series\\'
*     $                       // fname(1:ISTRLEN(fname))
*cunix
*cunix        filename = GRAPHDIR(1:ISTRLEN(GRAPHDIR)) // '/series/'
*cunix     $                       // fname(1:ISTRLEN(fname))
*        call OPENDEVICE(filename,48,0,ifault)
*        if (ifault .eq. 0) then           
*          na=min(nz,nres)
*          write(8,'(I3,/,I2,/f8.3,/,2X,A)')na,1,-0.0d0,TITLEG
*          write(8,'(2X,A,/,I3)') subtitle(1:ISTRLEN(subtitle)),mq
*
*          if (Lam .eq. 1) then
*              if (nres .lt. nz) then
*              do i=1,nres
*                  k=nz-nres+i 
*                write (48,'(g16.8)') serie(k)-eres(i)
*              end do
*            else
*              do i=1,nz
*                  k=nres-nz+i 
*                write (48,'(g16.8)') serie(i)-eres(k)
*              end do
*            end if
*          else
*              if (nres .lt. nz) then
*              do i=1,nres
*                  k=nz-nres+i 
*                write (48,'(g16.8)') Dlog(serie(k))-eres(i)
*              end do
*            else
*              do i=1,nz
*                  k=nres-nz+i 
*                write (48,'(g16.8)') Dlog(serie(i))-eres(k)
*              end do
*            end if
*          end if
*          call CLOSEDEVICE(48)
*        end if
*      end
*      