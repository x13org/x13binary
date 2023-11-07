      SUBROUTINE endsf(Simon,Savg,K,W,Nend)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C --- APPLY END WEIGHTS FOR THE 3X9 or 3X15
C-----------------------------------------------------------------------
      DOUBLE PRECISION Simon,Savg,W,totals,Sumwt
      INTEGER K,kk,jj,j1,j2,jk,l,Nend
      DIMENSION Simon(*),Savg(*),W(*)
      EXTERNAL totals
C-----------------------------------------------------------------------
      kk=0
      jj=1
      j1=jj
      j2=K
      DO WHILE (jj.le.Nend.and.j1.le.j2)
       jk=jj+Nend
       IF(jk.gt.K)THEN
        Savg(j1)=totals(Simon,1,K,1,1)
        IF(j1.ne.j2)Savg(j2)=Savg(j1)
       ELSE
        Savg(j1)=0D0
        Savg(j2)=0D0
        Sumwt=0D0
        DO l=1,jk
         Savg(j1)=Savg(j1)+W(kk+l)*Simon(l)
         IF(j1.ne.j2)Savg(j2)=Savg(j2)+W(kk+l)*Simon(K-l+1)
         Sumwt=Sumwt+W(kk+l)
        END DO
        Savg(j1)=Savg(j1)/Sumwt
        Savg(j2)=Savg(j2)/Sumwt
       END IF
       kk=kk+jk
       jj=jj+1
       j1=jj
       j2=K-jj+1
      END DO
C-----------------------------------------------------------------------
      RETURN
      END
