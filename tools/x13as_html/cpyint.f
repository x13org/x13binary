C     Last change:  BCM  25 Nov 97   10:33 am
**==cpyint.f    processed by SPAG 4.03F  at 09:47 on  1 Mar 1994
      SUBROUTINE cpyint(Invec,N,Inc,Outvec)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Subroutine to copy one vector into another.  Note, that you must
c have enough space in both arrays to copy.  This routine cannot check
c this.  Inc controls the direction if you are copying vector on to
c another part of itself for example back to front copy(a,na,-1,a(nqstar))
c or front to back copy(a(nqstar),na,1,a), nqstar positive.
c Inc also controls the intervals between for example
c
c     double precision x(5,n),y(5,n)
c     call cpyint(x,n,5,y)      @ to copy the 1st column
c     call cpyint(x(3),n,5,y(2))@ to copy the 3rd column to the 2ond
c
c-----------------------------------------------------------------------
      INTEGER i,Invec(*),Outvec(*),N,Inc,begelt,endelt
c     ------------------------------------------------------------------
      IF(Inc.gt.0)THEN
       begelt=1
       endelt=N
      ELSE
       begelt=N
       endelt=1
      END IF
c     ------------------------------------------------------------------
      DO i=begelt,endelt,Inc
       Outvec(i)=Invec(i)
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
