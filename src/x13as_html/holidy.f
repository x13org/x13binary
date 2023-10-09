C     Last change:  BCM  15 Apr 2005   12:12 pm
**==holidy.f    processed by SPAG 4.03F  at 10:07 on  4 Oct 1994
      SUBROUTINE holidy(Yhat,Nyear,Lfda,Lfbk,Lyr,Llda,Numfct,Fstatl,
     &                  Fstatt,Ndfl,Ndft,Plevl,Plevt,Keastr,Khol)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'xeastr.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO
      PARAMETER(ZERO=0D0)
c-----------------------------------------------------------------------
      DOUBLE PRECISION Fstatl,Fstatt,Plevl,Plevt,Yhat
      INTEGER i,j,k,l,l2,Lfda,lfdam,Llda,Lyr,m,mar,Ndfl,Ndft,Nyear,
     &        Numfct,apr,Keastr,Khol,Lfbk,lfdam2,marbk
      DIMENSION Yhat(PLEN)
c-----------------------------------------------------------------------
      INCLUDE 'kdate.prm'
c-----------------------------------------------------------------------
      m=(Lfbk/12)*12+1
      IF(mod(Lfbk,12).eq.0)m=m-12
c-----------------------------------------------------------------------
c     Generate X indicator vector
c-----------------------------------------------------------------------
      IF(Lgenx)THEN
       l=Lyr-1900
       l2=l+Nyear-1
       DO i=l,l2
        DO j=1,3
         DO k=1,4
          Xhol((m+(i-l)*12)+4*j-4+k)=dble(float((kdate(i,j))))
         END DO
        END DO
       END DO
      END IF
c-----------------------------------------------------------------------
C --- LOCATE FIRST MARCH,AUGUST AND NOVEMBER IN X AND Y. SUBTRACT 100
C     FROM FIRST MONTH. SUBTRACT SECOND MONTH FROM 100.
c-----------------------------------------------------------------------
c      MAR = 15+M-1
c      AUG = 20+M-1
c      NOV = 23+M-1
      lfdam=mod(Lfda,12)
      IF(lfdam.eq.0)lfdam=12
      IF(Lfda.eq.Lfbk)THEN
       mar=3+m-1
       IF(lfdam.gt.3)mar=mar+12
       marbk=mar
      ELSE
       lfdam2=mod(Lfbk,12)
       IF(lfdam2.eq.0)lfdam2=12
       marbk=3+m-1
       IF(lfdam2.gt.3)mar=mar+12
       m=(Lfda/12)*12+1
       IF(mod(Lfda,12).eq.0)m=m-12
       mar=3+m-1
       IF(lfdam.gt.3)mar=mar+12
      END IF
      apr=0
      IF(lfdam.eq.4)apr=3
      Fstatl=ZERO
      Ndfl=0
      Plevl=ZERO
      Fstatt=ZERO
      Ndft=0
      Plevt=ZERO
c-----------------------------------------------------------------------
c     Generate Easter factors
c-----------------------------------------------------------------------
      IF(Keastr.ge.1)THEN
       CALL easter(Yhat,mar,marbk,apr,Llda,Keastr,Numfct)
      ELSE
       CALL setint(-99,4,Ieast)
      END IF
c-----------------------------------------------------------------------
c     Set holiday adjustment variable so that holiday factors will
c     be treated like prior adjustment factors
c-----------------------------------------------------------------------
      Khol=2
c-----------------------------------------------------------------------
      RETURN
      END
