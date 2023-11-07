C     Last change:  BCM  22 Dec 97    4:32 pm
      SUBROUTINE tfmts3(Outdec,Muladd,Tblwid,Ifmt3)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Generate format for summary statistics at the end of the printout
c-----------------------------------------------------------------------
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      CHARACTER Ifmt3*(76),wid*(2),wid2*(2)
      LOGICAL Lwidpr
      INTEGER Outdec,fac,Muladd,ipos,ipos2,Tblwid
c-----------------------------------------------------------------------
      fac=Outdec
      IF(Muladd.eq.0.or.Muladd.eq.2.and.fac.eq.0)fac=2
      ipos=1
      ipos2=1
      CALL itoc(Tblwid+2,wid,ipos)
      IF(.not.Lfatal)CALL itoc(Tblwid+4,wid2,ipos2)
      IF(Lfatal)RETURN
      ipos=ipos-1
      ipos2=ipos2-1
      WRITE(Ifmt3,1010)wid2(1:ipos2),fac,wid(1:ipos),fac,wid(1:ipos),
     &                 fac,wid(1:ipos),fac
 1010 FORMAT('(a,1x,f',a,'.',i1,',1x,2(a,1x,a,1x,f',a,'.',i1,',1x),a,',
     &       '/,a,1x,f',a,'.',i1,',1x,a,1x,a,1x,f',a,'.',i1,',1x,a)')
c-----------------------------------------------------------------------
      RETURN
      END
      
