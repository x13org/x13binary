C     Last change:  BCM  10 Jul 1998    2:34 pm
**==exctma.f    processed by SPAG 4.03F  at 09:48 on  1 Mar 1994
      SUBROUTINE exctma(Nc,A,Nelta,Nata)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Filters the matrix, A, using the MA operators
c                     a=[1/th(B)]*z,
c-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO,MONE,M300
      PARAMETER(ZERO=0D0,MONE=-1D0,M300=1D-300)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
c     ------------------------------------------------------------------
      INTEGER PXA
      PARAMETER(PXA=(PB+1)*(PLEN+2*PORDER))
c     ------------------------------------------------------------------
      INTEGER Nc,Nelta,neltq,Nata
      DOUBLE PRECISION A,work
      DIMENSION A(Nata),work(PXA)
c-----------------------------------------------------------------------
c     If there are no lag operators then there is no ARIMA model so
c the input doesn't need to be filtered
c-----------------------------------------------------------------------
      Nopr=Mdl(MA)-1
      IF(Nopr.gt.0)THEN
c-----------------------------------------------------------------------
c     Exact filtering requires calculating the initial values of the
c differenced series, w*=-(G'G)^-1 G'Hw.
c-----------------------------------------------------------------------
       IF(Lma)THEN
        neltq=Mxmalg*Nc
        CALL copy(A,Nelta,-1,A(neltq+1))
c-----------------------------------------------------------------------
c     Form Hw
c-----------------------------------------------------------------------
        CALL copy(A(neltq+1),Nelta,1,work(neltq+1))
        CALL setdp(ZERO,neltq,work)
        Nelta=neltq+Nelta
c     ------------------------------------------------------------------
        CALL ratpos(Nelta,Arimap,Arimal,Opr,Mdl(MA-1),Mdl(MA)-1,Nelta,
     &              work)
c-----------------------------------------------------------------------
c     Form G'Hw
c-----------------------------------------------------------------------
        CALL ratneg(Nelta,Arimap,Arimal,Opr,Mdl(MA-1),Mdl(MA)-1,work)
c-----------------------------------------------------------------------
c     Get the w*'s=-inv(G'G) * G'Hw.
c-----------------------------------------------------------------------
        CALL dsolve(Chlgpg,Mxmalg,Nc,.true.,work)
c     ------------------------------------------------------------------
        CALL scrmlt(MONE,neltq,work)
        CALL copy(work,neltq,1,A)
       END IF
c-----------------------------------------------------------------------
c     MA filter what's left
c-----------------------------------------------------------------------
       CALL ratpos(Nelta,Arimap,Arimal,Opr,Mdl(MA-1),Mdl(MA)-1,Nelta,A)
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
