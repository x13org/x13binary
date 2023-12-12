C     Last change:  BCM  14 May 1998    8:45 am
**==wrtotl.f    processed by SPAG 4.03F  at 09:55 on  1 Mar 1994
      SUBROUTINE wrtotl(Itype,Begotl,Endotl,Begdat,Sp,Otlttl,Nchr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c    Writes an outlier specifier 'AOyr.mo', 'LSyr.mo', or
c 'ROyr.mo-yr.mo' given the type, dates (yr,mo) and the reference date.
c Write or AOyr, LSyr, or ROyr-yr for annual or nonseasonal data.
c-----------------------------------------------------------------------
c Name   Type Description
c-----------------------------------------------------------------------
c begdat  i  Input 2 long vector (yr,mo) for the begining data of the
c             data or regression variables, used as a reference to
c             calculate t0.
c begotl  i  Input begining point for a ramp outlier and t0 for an AO
c             or level shift.
c endotl  i  Input end point for a ramp outlier and undefined for an
c             AO or LS
c itype   i  Input type of outlier 1=AO, 2=LS, 3=TC, 4=SO, 5=RO, 6=MV
c otldat  i  Local 4 long vector for the begining (yr,mo) and possibly
c             the ending (yr,mo) of the outlier.
c otlttl  c  Output outlier specifier to be read
c otltyp  c  Local 3 long pcolcr character vector of character codes
c              for the outlier types, AO, LS, and RO, for additive,
c             level shift, or ramp outlier respectively.
c sp      i  Input length of the seasonal period
c-----------------------------------------------------------------------
c     Data typing and variable initialization
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      LOGICAL T
      INTEGER NOTYPE
      PARAMETER(NOTYPE=6,T=.true.)
c-----------------------------------------------------------------------
      CHARACTER Otlttl*(PCOLCR),otltyp*2,cstr1*(5)
      INTEGER Begdat,Begotl,Endotl,itmp,Itype,Nchr,otldat,Sp,nstr1
      DIMENSION Begdat(2),otldat(2),otltyp(NOTYPE)
      DATA otltyp/'AO','LS','TC','SO','Rp','MV'/
c-----------------------------------------------------------------------
c     Read the type and date(s)
c-----------------------------------------------------------------------
      CALL addate(Begdat,Sp,Begotl-1,otldat)
c     ------------------------------------------------------------------
      CALL wrtdat(otldat,Sp,Otlttl(3:),Nchr)
      IF(Lfatal)RETURN
      Nchr=Nchr+2
c     ------------------------------------------------------------------
      IF(Itype.eq.(NOTYPE-1))THEN
       Nchr=Nchr+1
       Otlttl(Nchr:Nchr)='-'
       Nchr=Nchr+1
       CALL addate(Begdat,Sp,Endotl-1,otldat)
       CALL wrtdat(otldat,Sp,Otlttl(Nchr:),itmp)
       IF(Lfatal)RETURN
       Nchr=Nchr+itmp-1
      END IF
c     ------------------------------------------------------------------
      CALL setchr(' ',len(Otlttl)-Nchr,Otlttl(Nchr+1:))
c     ------------------------------------------------------------------
      IF(Itype.lt.1.or.Itype.gt.NOTYPE)THEN
       CALL itoc(Itype,cstr1,nstr1)
       IF(Lfatal)RETURN
       CALL errhdr
       CALL eWritln('Invalid outlier type, '//cstr1(1:(nstr1-1))//' '//
     &              Otlttl(1:Nchr)//'.',STDERR,Mt2,T,T)
       CALL abend
       RETURN
      ELSE
       Otlttl(1:2)=otltyp(Itype)
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
