      SUBROUTINE genSkip(LinkCode)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     Generate entries for index, skip links
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER (T=.true.,F=.false.)
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'htmlout.cmn'
c     ------------------------------------------------------------------
      INTEGER n1,LinkCode
c-----------------------------------------------------------------------
c     Return if this is a transparent seasonal adjustment for sliding
c     spans, revisions, or X-11 Holiday adjustment.
c-----------------------------------------------------------------------
      IF(Lhiddn.and.(Issap.eq.2.or.Irev.eq.4.or.Khol.eq.1))RETURN
c     ------------------------------------------------------------------
      CALL makeAnchor(Mt1,Idxtab,'pos')
      CALL makeSkipLink(Mt1,Idxtab,'Table',F)
      CALL makeAnchor(Mt1,Idxtab,'skip')
      Vindx(Idxtab)=LinkCode
      Idxtab=Idxtab+1
c     ------------------------------------------------------------------
      RETURN
      END
      