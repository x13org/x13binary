C     Last change:  BCM  21 May 1998    7:27 am
      SUBROUTINE x11int
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'adj.cmn'
      INCLUDE 'inpt.cmn'
      INCLUDE 'x11fac.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'x11srs.cmn'
      INCLUDE 'xtrm.cmn'
c-----------------------------------------------------------------------
      INTEGER PY1
      PARAMETER(PY1=PYRS+1)
c-----------------------------------------------------------------------
      DOUBLE PRECISION rinit,Stex,Temp
      DIMENSION Temp(PLEN),Stex(PLEN)
c-----------------------------------------------------------------------
      COMMON /work  / Temp
      COMMON /mq10  / Stex
c-----------------------------------------------------------------------
c        INITIALIZE ARRAYS
c-----------------------------------------------------------------------
      rinit=1D0
      IF(Muladd.eq.1)rinit=0D0
c-----------------------------------------------------------------------
      CALL setdp(rinit,PLEN,Sts)
      CALL setdp(rinit,PLEN,Stsi)
      CALL setdp(rinit,PLEN,Sti)
      CALL setdp(rinit,PLEN,Stptd)
      CALL setdp(rinit,PLEN,Temp)
c-----------------------------------------------------------------------
      CALL setdp(rinit,PLEN,Factd)
      CALL setdp(rinit,PLEN,Facao)
      CALL setdp(rinit,PLEN,Facls)
      CALL setdp(rinit,PLEN,Factc)
      CALL setdp(rinit,PLEN,Facso)
      CALL setdp(rinit,PLEN,Facsea)
      CALL setdp(rinit,PLEN,Facusr)
      CALL setdp(rinit,PLEN,Fachol)
      CALL setdp(rinit,PLEN,Facxhl)
      CALL setdp(rinit,PLEN,X11hol)
      CALL setdp(rinit,PLEN,Faccal)
c-----------------------------------------------------------------------
      CALL setdp(0D0,PLEN,Stc)
      CALL setdp(0D0,PLEN,Stwt)
      CALL setdp(0D0,PLEN,Stci)
      CALL setdp(0D0,PLEN,Stex)
      CALL setdp(0D0,PY1,Stdev)
c-----------------------------------------------------------------------
c     Copy adjustment factors in Sprior
c-----------------------------------------------------------------------
      IF(Nadj.gt.0)CALL copy(Adj,PLEN-Setpri+1,-1,Sprior(Setpri))
c-----------------------------------------------------------------------
      RETURN
      END
