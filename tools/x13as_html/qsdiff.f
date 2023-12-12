      SUBROUTINE qsDiff(Srs,Pos1,Posf,Lmodel,Nnsedf,Nseadf,Ny,QS)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
c-----------------------------------------------------------------------
      DOUBLE PRECISION Srs,aux,Qs,xmean
      INTEGER Pos1,Posf,Nnsedf,Nseadf,nz,i,j,k,ndif,posCorr,Ny
      LOGICAL Lmodel
      DIMENSION srs(PLEN),aux(PLEN)
c-----------------------------------------------------------------------
      nz=Posf-Pos1+1
      do i=Pos1,Posf-1
       aux(i-Pos1+1)=srs(i+1)-srs(i)
      end do
      k=nz-1
      IF(Lmodel)THEN
       ndif=max(min(2,(Nnsedf+Nseadf)),1)
      ELSE
       ndif=1
      END IF
      IF(ndif.gt.1)THEN
       do j=1,ndif-1
        k=k-1
        do i=1,k
         aux(i)=aux(i+1)-aux(i)
        end do             
       end do
      END IF
      CALL smeadl(aux,1,k,k,xmean)
      call calcQS2(aux,k,ny,QS,posCorr)
c-----------------------------------------------------------------------
      IF(posCorr.eq.1.and.ndif.eq.1)THEN
       k=k-1
       do i=1,k
        aux(i)=aux(i+1)-aux(i)
       end do             
       call calcQS2(aux,k,ny,QS,posCorr)
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
