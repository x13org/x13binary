**==insint.f    processed by SPAG 4.03F  at 09:50 on  1 Mar 1994
      SUBROUTINE insint(Subvec,Ielt,Ptrvec,Nelt,Vec)
      IMPLICIT NONE
c----------------------------------------------------------------------
c     Inserts the integer vector subvec into position ielt to vec
c using the pointers that are assumed to be updated in insptr.
c ptrvec is used to determine the length of vec.
c----------------------------------------------------------------------
      INCLUDE 'error.cmn'
c----------------------------------------------------------------------
      INTEGER nielt,Ielt,Nelt,nrest,Ptrvec
      INTEGER Subvec,Vec
      DIMENSION Ptrvec(0:Nelt),Subvec(*),Vec(*)
c     -----------------------------------------------------------------
      CALL eltlen(Ielt,Ptrvec,Nelt,nielt)
      IF(Lfatal)RETURN
      nrest=Ptrvec(Nelt)-Ptrvec(Ielt)
      CALL cpyint(Vec(Ptrvec(Ielt-1)),nrest,-1,Vec(Ptrvec(Ielt)))
c     -----------------------------------------------------------------
      CALL cpyint(Subvec,nielt,1,Vec(Ptrvec(Ielt-1)))
c     -----------------------------------------------------------------
      RETURN
      END
