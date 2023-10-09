C     Last change:  BCM  20 Jan 98   11:44 am
      SUBROUTINE addadj(Nspobs,Begspn,Sp,Begadj,Bgusra,Nusrad,Frstad,
     &                  Usradj,Adj,Nadj,Base,Adjtyp,Percnt,Ok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Add a user-defined prior adjustment series to the main prior
c     adjustment file Adj.
c-----------------------------------------------------------------------
      DOUBLE PRECISION ONEHND,ZERO
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.,ONEHND=100D0,ZERO=0D0)
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      CHARACTER Adjtyp*(9)
      INTEGER Nspobs,Begspn,Sp,Bgusra,Nusrad,Frstad,iprd,Begadj,Percnt,
     &        Nadj,iprd2
      LOGICAL Ok
      DOUBLE PRECISION Usradj,Adj,Base
      DIMENSION Adj(*),Usradj(*),Begspn(2),Bgusra(2),Begadj(2)
c-----------------------------------------------------------------------
      LOGICAL chkcvr
      EXTERNAL chkcvr
c-----------------------------------------------------------------------
c     Check to see if the span of the adjustments is large enough to 
c-----------------------------------------------------------------------
      IF(.not.chkcvr(Bgusra,Nusrad,Begspn,Nspobs,Sp))THEN
       CALL cvrerr(Adjtyp//' adjustments',Bgusra,Nusrad,'span',Begspn,
     &             Nspobs,Sp)
       IF(Lfatal)RETURN
       Ok=F
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Get the difference between the user start date and the total
c adjustment start date .  If this difference is negative, assume this is 
c due to backcasts and append 1.0 (or zero) to the user-defined 
c backcasts.
c-----------------------------------------------------------------------
      CALL dfdate(Begadj,Bgusra,Sp,Frstad)
      IF(Frstad.lt.0)THEN
       DO iprd=Nusrad,1,-1
        iprd2=iprd-Frstad
        Usradj(iprd2)=Usradj(iprd)
        IF(Percnt.eq.0)Usradj(iprd2)=Usradj(iprd2)/ONEHND
        IF(iprd.le.abs(Frstad))Usradj(iprd)=Base
       END DO
       Nusrad=Nusrad-Frstad
       Frstad=0
       CALL cpyint(Begadj,2,1,Bgusra)
      ELSE IF(Percnt.eq.0)THEN
       DO iprd=Nusrad,1,-1
        Usradj(iprd)=Usradj(iprd)/ONEHND
       END DO
      END IF
c     ------------------------------------------------------------------
c     Now combine the adjustments.
c     ------------------------------------------------------------------
      DO iprd=1,Nadj
       iprd2=iprd+Frstad
       IF(iprd2.le.Nusrad)THEN
        IF(Percnt.lt.2)THEN
         IF(Usradj(iprd2).le.ZERO)THEN
          IF(Percnt.eq.0)THEN
           CALL eWritln('Prior adjustment factors expressed as '//
     &                  'percentages cannot have values',STDERR,Mt2,T,F)
          ELSE
           CALL eWritln('Prior adjustment factors expressed as '//
     &                  'ratios cannot have values',STDERR,Mt2,T,F)
          END IF
          CALL writln('       less than or equal to zero.',
     &                STDERR,Mt2,F,T)
          CALL writln('       Check the '//Adjtyp//
     &             ' prior adjustment factors given in your spec file.',
     &                STDERR,Mt2,T,T)
          CALL abend
          RETURN
         END IF
         Adj(iprd)=Adj(iprd)*Usradj(iprd2)
        ELSE
         Adj(iprd)=Adj(iprd)+Usradj(iprd2)
        END IF
       ELSE
        Usradj(iprd2)=Base
       END IF
      END DO
c-----------------------------------------------------------------------
c     Make Frstad the time point the adjustments begin on in the user
c defined adjustment series, not a displacement.
c-----------------------------------------------------------------------
      Frstad=Frstad+1
c-----------------------------------------------------------------------
      RETURN
      END
