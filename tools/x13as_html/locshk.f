C     Last change:  BCM   7 May 2003    2:24 pm
      SUBROUTINE locshk(Sts,V,Ny)
      IMPLICIT NONE
C-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'x11ptr.cmn'
*      INCLUDE 'error.cmn'
      INCLUDE 'units.cmn'
C-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO,ONE
      PARAMETER (ZERO=0D0,ONE=1D0)
C-----------------------------------------------------------------------
      DOUBLE PRECISION dny,lmat,lsum,Sts,temps,V,w
      INTEGER i,i1,iend,ij,it1,j,k,Ny,ny2,j2,i2
      DIMENSION lmat(PLEN,PSP),Sts(PLEN),temps(PLEN),w(PSP,PSP)
C-----------------------------------------------------------------------
      DOUBLE PRECISION lkshnk
      EXTERNAL lkshnk
C-----------------------------------------------------------------------
c     initialize variables
C-----------------------------------------------------------------------
      ny2 = Ny/2
      i1 = Pos1ob + ny2
*      iend = Posffc - ny2 + 1
      iend = Posfob
*      if (iend.gt.(Posffc-ny2+1))THEN
*       CALL writln(
*      END IF
C-----------------------------------------------------------------------
C     Copy seasonal factors into double precision variable.
C-----------------------------------------------------------------------
      CALL copy(Sts,Posffc,1,temps)
C-----------------------------------------------------------------------
C     compute the moving likelihoods for observations i2 to iend, and
c     normalize for the sum of the moving likelihoods.
C-----------------------------------------------------------------------
      DO i = i1, iend
       lsum = ZERO
       DO j = 1, Ny
        j2 = i + j - (Ny2 + 1)
        lmat(i,j) = lkshnk(temps(i),temps(j2),V)
        lsum = lsum + lmat(i,j)
       END DO
       DO j = 1, Ny
        lmat(i,j) = lmat(i,j) / lsum
       END DO
      END DO
C-----------------------------------------------------------------------
C     initialize W matrix
C-----------------------------------------------------------------------
      CALL setdp(ZERO,PSP*PSP,w)
C-----------------------------------------------------------------------
c     compute shrinkage weights by adding the weighted likelihoods
c     for each calendar month/quarter, and dividing by the number of
c     years for each month/quarter.
C-----------------------------------------------------------------------
      DO i=i1,i1+Ny-1
       it1 = mod(i,Ny)
       IF (it1.eq.0) it1 = Ny
       dny = ZERO
       DO i2 = i, iend, Ny
        dny = dny + ONE
        DO k = 1, Ny
         W(it1,k) = W(it1,k) + lmat(i2,k)
        END DO
       END DO
       DO k = 1, Ny
        W(it1,k) = W(it1,k) / dny
       END DO
      END DO
C-----------------------------------------------------------------------
C     initialize temps to zero
C-----------------------------------------------------------------------
      CALL setdp(ZERO,PLEN,temps)
C-----------------------------------------------------------------------
c      compute local seasonals 
C-----------------------------------------------------------------------
      it1 = mod(Pos1ob,Ny)
      IF (it1.eq.0) it1 = Ny
      DO i=Pos1ob,Posfob
       DO j = -ny2,ny2-1
        ij=i+j
        write(ng,*)i,j,ij
        if(ij.lt.Pos1ob)ij=Ny+ij
        write(ng,*)i,j,ij
        temps(i)=temps(i)+Sts(ij)*w(it1,j+ny2+1)
*        if(i.lt.Pos1ob+ny2)
*     &     write(mtprof,1)"i,j,ij,Sts(ij),w(",it1,",",j+ny2+1,"),",
*     &           "temps(i)=",i,j,ij,Sts(ij),w(it1,j+ny2+1),temps(i)
       END DO
       it1 = it1 + 1
       IF (it1.gt.Ny) it1 = it1 - Ny
      END DO
C-----------------------------------------------------------------------
C     copy seasonals into Sts 
C-----------------------------------------------------------------------
      DO i=Pos1ob,Posfob
       Sts(i)=temps(i)
      END DO
C-----------------------------------------------------------------------
*    1 FORMAT(a,i3,a,i3,a,3i4,f12.4,e20.6,f12.4)
      RETURN
      END
      