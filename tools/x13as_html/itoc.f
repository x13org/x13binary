C     Last change:  BCM  27 Jan 98   10:54 am
**==itoc.f    processed by SPAG 4.03F  at 09:50 on  1 Mar 1994
      SUBROUTINE itoc(Inum,Str,Ipos)
      IMPLICIT NONE
c     -----------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
c     -----------------------------------------------------------------
      CHARACTER digits*(10),Str*(*)
      INTEGER begchr,d,intval,Inum,Ipos,nchr,nleft
      DOUBLE PRECISION tmp
      DATA digits/'0123456789'/
c     -----------------------------------------------------------------
      nleft=len(Str(Ipos:))
c     -----------------------------------------------------------------
      begchr=Ipos
      IF(Inum.lt.0)THEN
       Str(begchr:begchr)='-'
       begchr=begchr+1
      END IF
c     -----------------------------------------------------------------
      intval=abs(Inum)
c     -----------------------------------------------------------------
      IF(intval.eq.0)THEN
       nchr=1
c     -----------------------------------------------------------------
      ELSE
       tmp=dble(float(intval))
       tmp=log10(tmp)
       tmp=tmp+1D0
       nchr=ifix(sngl(tmp))+begchr-Ipos
      END IF
c     -----------------------------------------------------------------
      IF(nchr.gt.nleft)THEN
       WRITE(STDERR,*)' Error:  Can''t write ',Inum,' in ',len(Str),
     &                ' spaces'
       CALL errhdr
       WRITE(Mt2,*)'<p><strong>Error:</strong>  Can''t write ',Inum,
     &             ' in ',len(Str),' spaces</p>'
       CALL abend
       RETURN
      END IF
c     -----------------------------------------------------------------
      nchr=Ipos-1+nchr
      DO Ipos=nchr,begchr,-1
       d=mod(intval,10)+1
       Str(Ipos:Ipos)=digits(d:d)
       intval=intval/10
      END DO
      Ipos=nchr+1
c     -----------------------------------------------------------------
      RETURN
      END
