**==insdbl.f    processed by SPAG 4.03F  at 09:50 on  1 Mar 1994
      SUBROUTINE insdbl(Subvec,Ielt,Ptrvec,Nelt,Vec)
      IMPLICIT NONE
c----------------------------------------------------------------------
c     Inserts the character string elt into position ielt to vec
c using the pointers that are assumed to be updated in insptr.
c ptrvec is used to determine the length of vec.
c----------------------------------------------------------------------
      INCLUDE 'error.cmn'
c----------------------------------------------------------------------
      INTEGER nielt,Ielt,Nelt,nrest,Ptrvec
      DOUBLE PRECISION Subvec,Vec
      DIMENSION Ptrvec(0:Nelt),Subvec(*),Vec(*)
c     -----------------------------------------------------------------
      CALL eltlen(Ielt,Ptrvec,Nelt,nielt)
      IF(Lfatal)RETURN
      nrest=Ptrvec(Nelt)-Ptrvec(Ielt)
      CALL copy(Vec(Ptrvec(Ielt-1)),nrest,-1,Vec(Ptrvec(Ielt)))
c     -----------------------------------------------------------------
      CALL copy(Subvec,nielt,1,Vec(Ptrvec(Ielt-1)))
c     -----------------------------------------------------------------
      RETURN
      END
