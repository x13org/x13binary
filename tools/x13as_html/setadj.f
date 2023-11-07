C     Last change:  BCM  23 Dec 97    9:58 am
      SUBROUTINE setadj(Usr,Nusr,Usrsrs,Nusrs,Usrbeg,Havusr,Nprtyp,
     &                  Adjtmp,Nadtmp,Bgusra,Srsnam,Nsrs,Isrs,Argok)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      CHARACTER Usrsrs*(*),Srsnam*(*)
      LOGICAL Havusr,Argok
      DOUBLE PRECISION Adjtmp,Usr
      INTEGER Bgusra,Isrs,j,j2,Nusr,Nusrs,Usrbeg,Nadtmp,Nprtyp,
     &        Nsrs
      DIMENSION Adjtmp(*),Bgusra(2),Usr(*),Usrbeg(2)
c     ------------------------------------------------------------------
      IF(Isrs.gt.0)THEN
       j2=0
       DO j=Isrs,Nadtmp,Nprtyp
        j2=j2+1
        Usr(j2)=Adjtmp(j)
       END DO
       Nusr=j2
      ELSE
       CALL copy(Adjtmp,Nadtmp,1,Usr)
       Nusr=Nadtmp
      END IF
c     ------------------------------------------------------------------
      Usrsrs=Srsnam
      Nusrs=Nsrs
c     ------------------------------------------------------------------
      CALL cpyint(Bgusra,2,1,Usrbeg)
c     ------------------------------------------------------------------
      IF(Argok)Havusr=.true.
c     ------------------------------------------------------------------
      RETURN
      END
