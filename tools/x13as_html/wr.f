      subroutine WRTOUT3(title,nz,mq,tramo,lam,npatd,neast,p,q,bp,bq,d,
     $                   imean,sqf,nou3,bjstat2)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
      integer nz,mq,tramo,lam,npatd,neast,p,q,bp,bq,d,imean,nou3
      character title*80
      real*8 sqf,bjstat2
C
C.. Local Scalars ..
      character adj
      include 'titl.i'
      include 'transcad.i'
C
C ... Executable Statements ...
C
      if (lam .eq. 1) then
       adj = 'A'
      end if
      if (lam .eq. 0) then
       adj = 'M'
      end if
 7000 format (
     $///,2x,'SERIES  | # OF OBS. |   # OF   |   TYPE OF  |',
     $'          OUTLIERS        |',/,2x,
     $'        |           | OBS/YEAR | ADJUSTMENT |',
     $' LEVEL SHIFT | ',A,' |')
      write (nou3,7000) transLcad(1:nTransLcad)
      write (nou3,7000)
 7001 format (
     $2x,'--------|-----------|----------|------------|',
     $'-------------|------------|')
      write (nou3,7001)
 7002 format (10x,'|',11x,'|',10x,'|',12x,'|',13x,'|',12x,'|')
      write (nou3,7002)
 7003 format (2x,a8,'|',4x,i3,4x,'|',4x,i2,4x,'|',5x,a1,6x,'|')
      write (nou3,7003) titleg, nz, mq, adj
      write (nou3,7002)
      write (nou3,7002)
      end
      subroutine SETTIME
C
C.. Implicits ..
      implicit none
      include 'xxxs.i'
C
C ... Executable Statements ...
C
      COMPILA='19-JUL-2001'
      end
