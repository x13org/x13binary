C     Last change:  BCM  16 Feb 1999   11:17 am
**==prfcrv.f    processed by SPAG 4.03F  at 16:46 on 14 Nov 1994
      SUBROUTINE gnfcrv(Fcter,Fctss,Orig)
C-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Generate revisions history of forecast errors for all lags
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'rev.prm'
      INCLUDE 'rev.cmn'
      INCLUDE 'revsrs.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION Orig,Fcter,Fctss
      INTEGER i,j,k
      DIMENSION Fcter(PFCLAG,PREV),Fctss(PFCLAG,PREV),Orig(PLEN)
c-----------------------------------------------------------------------
      CALL setdp(0D0,PFCLAG*PREV,Fcter)
      CALL setdp(0D0,PFCLAG*PREV,Fctss)
c-----------------------------------------------------------------------
c     Start loop to print/save forecast error information.
c-----------------------------------------------------------------------
      j=0
      DO i=Begrev+Rfctlg(1),Endrev
       Revptr=i-Begrev+1
       j=j+1
c-----------------------------------------------------------------------
c     Calculate forcast errors, accumulated sum of squares.
c-----------------------------------------------------------------------
       DO k=1,Nfctlg
        IF(Nfctlg.eq.1.or.(Nfctlg.gt.1.and.Rfctlg(k).le.j))THEN
         Fcter(k,j)=Orig(i)-Cncfct(k,Revptr)
         Fctss(k,j)=fctss(k,j)+(fcter(k,j)*fcter(k,j))
        END IF
       END DO
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
