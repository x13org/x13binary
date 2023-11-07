**==setxpt.f    processed by SPAG 4.03F  at 09:53 on  1 Mar 1994
      SUBROUTINE setxpt(Nf2,Lsadj,Fctdrp)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Set up "pointers" variables for X-11 to tell where backcasts,
c     data, forecasts begin and end.
c-----------------------------------------------------------------------
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'lzero.cmn'
c-----------------------------------------------------------------------
      INTEGER Fctdrp,nspobs,Nf2
      LOGICAL Lsadj
c-----------------------------------------------------------------------
c     Set X-11 Pointer values
c-----------------------------------------------------------------------
      nspobs=Nofpob-Nf2
      Pos1bk=Nbcst2-Nbcst+Lsp
      Pos1ob=Nbcst2+Lsp
      Posfob=Nbcst2+nspobs+Lsp-1
      Posffc=Nbcst2+nspobs+Nfcst+Lsp-1
      IF((.not.Lsadj).and.Fctdrp.gt.0)Posffc=max(Posfob,Posffc-Fctdrp)
c-----------------------------------------------------------------------
      RETURN
      END
