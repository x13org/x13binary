      integer function NPsa(sa,n1,nz,lmodel,d,bd,mq,llog)
      implicit none
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
c-----------------------------------------------------------------------
      DOUBLE PRECISION sa(*),media,aux(mpkp),SNP
      INTEGER ndif,d,bd,nz,k,i,n1,mq,j
      LOGICAL lmodel,llog
c-----------------------------------------------------------------------
      real*8 KENDALLS
      external KENDALLS
c-----------------------------------------------------------------------
      IF(lmodel)THEN
       ndif=max(min(2,d+bd),1)  
      ELSE
       ndif=1
      END IF
      if(llog)then
       do i=n1,nz
        aux(i-n1+1)=log(sa(i))
       end do
      ELSE
       do i=n1,nz
        aux(i-n1+1)=sa(i)
       end do
      END IF
c-----------------------------------------------------------------------
      k=nz-n1+1
      do j=1,ndif
       k=k-1
       do i=1,k
        aux(i)=aux(i+1)-aux(i)
       end do            
      end do
c-----------------------------------------------------------------------
      media=0
      do i=1,k
       media=media+aux(i) 
      end do 
      media=media/k
      do i=1,k
       aux(i)=aux(i)-media
      end do      
c-----------------------------------------------------------------------
      SNP=kendalls(aux,k,mq)
      if (SNP.gt.24.73d0.and.mq.eq.12.or.
     $    SNP.gt.11.35d0.and.mq.eq.4) then        
       NPsa=1
      else
       NPsa=0 
      end if
c-----------------------------------------------------------------------
      return
      end
      