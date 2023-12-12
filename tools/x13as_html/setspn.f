C     Last change:  BCM  10 Mar 98   10:40 am
      SUBROUTINE setspn(Sp,Nend,Nbeg,Begspn,Endspn,Begmdl,Endmdl,Nspobs,
     &                  Frstsy,Nobspf,Begsrs,Nobs,Nfcst,Fctdrp,Nomnfy,
     &                  Begadj,Adj1st)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     If a model span has been used, reset beginning and ending dates 
c     for span and reset span length and other pointers.
c-----------------------------------------------------------------------
      INTEGER Sp,Nend,Nbeg,Begspn,Endspn,Begmdl,Endmdl,Nspobs,Frstsy,
     &        Nobspf,Begsrs,Nobs,Nfcst,Fctdrp,Nomnfy,Begadj,Adj1st
      DIMENSION Begspn(2),Endspn(2),Begmdl(2),Endmdl(2),Begsrs(2),
     &          Begadj(2)
c-----------------------------------------------------------------------
      IF(Nend.gt.0)CALL addate(Endmdl,Sp,Nend,Endspn)
      IF(Nbeg.gt.0)CALL addate(Begmdl,Sp,-Nbeg,Begspn)
c-----------------------------------------------------------------------
c     Reset length of series, other pointers
c-----------------------------------------------------------------------
      CALL dfdate(Endspn,Begspn,Sp,Nspobs)
      Nspobs=Nspobs+1
      CALL dfdate(Begspn,Begsrs,Sp,Frstsy)
      Frstsy=Frstsy+1
      Nomnfy=Nobs-Frstsy+1
      Nobspf=min(Nspobs+max(Nfcst-Fctdrp,0),Nomnfy)
      CALL dfdate(Begspn,Begadj,Sp,Adj1st)
      Adj1st=Adj1st+1
c-----------------------------------------------------------------------
      RETURN
      END
