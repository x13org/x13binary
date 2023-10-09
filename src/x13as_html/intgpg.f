C     Last change:  BCM  25 Nov 97   11:59 am
      SUBROUTINE intgpg(Nextma,Info)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Initializes the G'G matrix
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION ONE
      INTEGER PA
      PARAMETER(PA=PLEN+2*PORDER,ONE=1D0)
c      LOGICAL T,F
c      PARAMETER(T=.true.,F=.false.,PA=PLEN+PORDER,ONE=1D0)
c     ------------------------------------------------------------------
      INTEGER i,ielt,Info,j,Nextma,nap2,ntmp
      DOUBLE PRECISION piwght,ssqpwt
      DIMENSION piwght(PA),ssqpwt(PA)
c-----------------------------------------------------------------------
c     Calculate the cholesky decomposition of G'G and |G'G| by first
c by calculating the pi weights in piwght and the cross products
c of the pi weights in ssqpwt.
c-----------------------------------------------------------------------
      IF(Lma)THEN
       piwght(1)=ONE
       ntmp=1
       CALL ratpos(ntmp,Arimap,Arimal,Opr,Mdl(MA-1),Mdl(MA)-1,Nextma,
     &             piwght)
c     ------------------------------------------------------------------
       CALL copy(piwght,Nextma,1,ssqpwt)
       CALL ratneg(Nextma,Arimap,Arimal,Opr,Mdl(MA-1),Mdl(MA)-1,ssqpwt)
c-----------------------------------------------------------------------
c     The first row of G'G is stored in ssqpwt(1:mxmalg)
c-----------------------------------------------------------------------
       nap2=Nextma+2
       ielt=1
       Chlgpg(ielt)=ssqpwt(ielt)
c     ------------------------------------------------------------------
c       CALL under0(T)
c     ------------------------------------------------------------------
       DO j=2,Mxmalg
        ielt=ielt+1
        Chlgpg(ielt)=ssqpwt(j)
c     ------------------------------------------------------------------
        DO i=2,j
         ielt=ielt+1
         Chlgpg(ielt)=Chlgpg(ielt-j)-piwght(nap2-i)*piwght(nap2-j)
        END DO
       END DO
c-----------------------------------------------------------------------
c     Calculate the cholesky decomposition and determinate of G'G.
c-----------------------------------------------------------------------
       CALL dppfa(Chlgpg,Mxmalg,Info)
c     ------------------------------------------------------------------
c       CALL under0(F)
c     ------------------------------------------------------------------
       IF(Info.le.0)CALL logdet(Chlgpg,Mxmalg,Lndtcv)
c     ------------------------------------------------------------------
      ELSE
       Lndtcv=0D0
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
