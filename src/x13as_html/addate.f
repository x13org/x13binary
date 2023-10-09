C     Last change:  BCM   6 Oct 97   11:15 am
**==addate.f    processed by SPAG 4.03F  at 09:46 on  1 Mar 1994
      SUBROUTINE addate(Indate,Sp,B10,Outdat)
c-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER Indate(2),Sp,B10,iadd,Outdat(2)
c-----------------------------------------------------------------------
c     If the seasonal period is one, indate(2) and outdat(2) have
c no meaning.
c-----------------------------------------------------------------------
      IF(Sp.eq.1)THEN
       Outdat(1)=Indate(1)+B10
       Outdat(2)=0
      ELSE
c-----------------------------------------------------------------------
c     Else treat as dates.  First, convert to base 10.
c-----------------------------------------------------------------------
       iadd=Indate(1)*Sp+Indate(2)+B10
c-----------------------------------------------------------------------
c     Convert the result back to the date form.  Note this will handle
c negative dates.
c-----------------------------------------------------------------------
       Outdat(1)=iadd/Sp
       Outdat(2)=mod(iadd,Sp)
c-----------------------------------------------------------------------
       IF(Outdat(2).lt.0)THEN
        Outdat(1)=Outdat(1)-1
        Outdat(2)=Sp+Outdat(2)
c-----------------------------------------------------------------------
       ELSE IF(Outdat(2).eq.0.and.Outdat(1).eq.0)THEN
        Outdat(1)=-1
        Outdat(2)=Sp
       ELSE IF(Outdat(2).eq.0)THEN
        Outdat(1)=Outdat(1)-1
        Outdat(2)=Sp
       END IF
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
