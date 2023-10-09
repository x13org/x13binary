**==copycl.f    processed by SPAG 4.03F  at 09:47 on  1 Mar 1994
      SUBROUTINE copycl(From,Nr,Nfrmcl,Ifrmcl,Ntocl,Itocl,To)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Copies the column ifrmcl from from, an nr by nfrmcl matrix, to
c column itocl of to, an nr by ntocl matrix
c-----------------------------------------------------------------------
c Name   Type Description
c-----------------------------------------------------------------------
c eltfrm  i  Local element of the from matrix
c eltto   i  Local element of the to matrix
c from    d  Input nr by nfrmcl matrix that column iform is copied from
c i       i  Local do loop index
c ifrmcl  i  Input column of from to copy
c itocl    i  Input column of to to put column ifrmcl of from in
c nfrmcl  i  Input number of from columns
c nr      i  Input number of rows in to and from
c ntocl    i  Input number of to columns
c-----------------------------------------------------------------------
c     Data typing and definition
c-----------------------------------------------------------------------
      INTEGER eltfrm,eltto,i,Ifrmcl,Itocl,Nfrmcl,Nr,Ntocl
      DOUBLE PRECISION From,To
      DIMENSION From(Nr*Nfrmcl),To(Nr*Ntocl)
c-----------------------------------------------------------------------
c     Initialize the column indices then copy the colomns
c-----------------------------------------------------------------------
      eltfrm=Ifrmcl-Nfrmcl
      eltto=Itocl-Ntocl
c-----------------------------------------------------------------------
      DO i=1,Nr
       eltfrm=eltfrm+Nfrmcl
       eltto=eltto+Ntocl
       To(eltto)=From(eltfrm)
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
