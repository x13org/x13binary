C     Last change:  BCM  12 Mar 98   12:30 pm
**==month.f    processed by SPAG 4.03F  at 09:51 on  1 Mar 1994
      SUBROUTINE month(Icode,Jx)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'error.cmn'
      INCLUDE 'chrt.cmn'
c      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
      INTEGER Icode,Jx,l,nyr2,i
      CHARACTER itype*1
c      DOUBLE PRECISION Ab2(61)
c-----------------------------------------------------------------------
C  PLOT CHART TYPES 7 AND 8 (MONTHLY CHARTS)
c-----------------------------------------------------------------------
      CALL grzmth(Ibeg2,Ienda)
      DO l=1,Nseas
       nyr2=Nyr
       IF((l.lt.Ifrst).or.(l.gt.Last))nyr2=Nyr-1
c       write(Mtprof,*)' entering grzmyr, l = ', l
       CALL grzmyr(l)
c       write(Mtprof,*)' ab1 = ',(ab1(i),i=1,nyr2)
       itype=Ialpha(l)
       IF(Nseas.eq.4)itype=Ialphq(l)
       CALL aver(Ab1,nyr2,itype,Icode,0,Jx)
       IF(Lfatal)RETURN
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
