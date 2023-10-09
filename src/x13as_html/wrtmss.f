C     Last change:  BCM  20 May 1998   11:08 am
**==wrtmss.f    processed by SPAG 4.03F  at 12:24 on 21 Jun 1994
      SUBROUTINE wrtmss(M,Iy,X,Dmax,Ncol,Nopt,Iobs,Fnote,L2Big)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Routine which prints out the sliding spans for each observation.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'ssap.prm'
      INCLUDE 'ssap.cmn'
      INCLUDE 'notset.prm'
      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
      INTEGER hicode,icode,jcode,M,Iy,i,i2,Nopt,Iobs,Ncol,n1,n2
      LOGICAL L2Big
      CHARACTER cfmt*(80),starz*(9),star0*(10),Fnote*(10)
      DOUBLE PRECISION Dmax,X
      DIMENSION X(MXLEN,MXCOL),Dmax(MXLEN,NEST)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      DATA starz/'*********'/
      DATA star0/'**********'/
c-----------------------------------------------------------------------
      hicode=0
      jcode=1
      icode=0
      DO i=Ncol,1,-1
       IF(dpeq(X(Iobs,i),DNOTST))icode=icode+jcode
       hicode=hicode+jcode
       IF(i.gt.1)jcode=jcode*10
      END DO
c-----------------------------------------------------------------------
      IF(icode.eq.0)THEN
       IF(L2Big)THEN
        WRITE(Mt1,F3)M,'-',Iy,(X(Iobs,i),i=1,Ncol),Dmax(Iobs,Nopt),Fnote
       ELSE
        WRITE(Mt1,F1)M,'-',Iy,(X(Iobs,i),i=1,Ncol),Dmax(Iobs,Nopt),Fnote
       END IF
       RETURN
      END IF
c-----------------------------------------------------------------------
      IF(icode.eq.hicode)THEN
       IF(L2big)THEN
        WRITE(cfmt,910)Ncol
  910   FORMAT('(1X,I2,A1,I4,2X,',i1,'(A10,1X),3X,A9,2x,a10)')
        WRITE(Mt1,cfmt)M,'-',Iy,(star0,i=1,Ncol),starz,Fnote
       ELSE
        WRITE(cfmt,1010)Ncol
 1010   FORMAT('(1X,I2,A1,I4,2X,',i1,'(A9,2X),3X,A9,2x,a10)')
        WRITE(Mt1,cfmt)M,'-',Iy,(starz,i=1,Ncol),starz,Fnote
       END IF
c-----------------------------------------------------------------------
      ELSE IF(icode.eq.(hicode-jcode))THEN
       IF(L2big)THEN
        WRITE(cfmt,920)Ncol-1
  920   FORMAT('(1X,I2,A1,I4,2X,E10.4,1X,',i1,'(A10,1X),3X,A9,2x,a10)')
        WRITE(Mt1,cfmt)M,'-',Iy,X(Iobs,1),(star0,i=1,Ncol-1),starz,Fnote
       ELSE
        WRITE(cfmt,1020)Ncol-1
 1020   FORMAT('(1X,I2,A1,I4,2X,F9.2,2X,',i1,'(A9,2X),3X,A9,2x,a10)')
        WRITE(Mt1,cfmt)M,'-',Iy,X(Iobs,1),(starz,i=1,Ncol-1),starz,Fnote
       END IF
c-----------------------------------------------------------------------
      ELSE IF(icode.eq.(hicode-1))THEN
       IF(L2big)THEN
        WRITE(cfmt,930)Ncol-1
  930   FORMAT('(1X,I2,A1,I4,2X,',i1,'(A10,1X),E10.4,1X,3X,A9,2x,a10)')
        WRITE(Mt1,cfmt)M,'-',Iy,(star0,i=1,Ncol-1),X(Iobs,Ncol),starz,
     &                 Fnote
       ELSE
        WRITE(cfmt,1030)Ncol-1
 1030   FORMAT('(1X,I2,A1,I4,2X,',i1,'(A9,2X),F9.2,2X,3X,A9,2x,a10)')
        WRITE(Mt1,cfmt)M,'-',Iy,(starz,i=1,Ncol-1),X(Iobs,Ncol),starz,
     &                 Fnote
       END IF
c-----------------------------------------------------------------------
      ELSE IF(icode.lt.jcode)THEN
       n2=1
       IF(Ncol.eq.4.and.icode.gt.1)n2=2
       n1=Ncol-n2
       IF(L2big)THEN
        WRITE(cfmt,940)n1,n2
  940   FORMAT('(1X,I2,A1,I4,2X,',i1,'(E10.4,1X),',i1,
     &         '(A10,1X),3X,F9.2,2x,a10)')
        WRITE(Mt1,cfmt)M,'-',Iy,(X(Iobs,i),i=1,n1),(star0,i2=1,n2),
     &                 Dmax(Iobs,Nopt),Fnote
       ELSE
        WRITE(cfmt,1040)n1,n2
 1040   FORMAT('(1X,I2,A1,I4,2X,',i1,'(F9.2,2X),',i1,
     &         '(A9,2X),3X,F9.2,2x,a10)')
        WRITE(Mt1,cfmt)M,'-',Iy,(X(Iobs,i),i=1,n1),(starz,i2=1,n2),
     &                 Dmax(Iobs,Nopt),Fnote
       END IF
c-----------------------------------------------------------------------
      ELSE
       n1=1
       IF(Ncol.eq.4.and.icode.gt.1000)n1=2
       n2=Ncol-n1
       IF(L2big)THEN
        WRITE(cfmt,950)n1,n2
  950   FORMAT('(1X,I2,A1,I4,2X,',i1,'(A10,1X),',i1,
     &         '(E10.4,1X),3X,F9.2,2x,a10)')
        WRITE(Mt1,cfmt)M,'-',Iy,(star0,i2=1,n1),(X(Iobs,i),i=n1+1,Ncol),
     &                 Dmax(Iobs,Nopt),Fnote
       ELSE
        WRITE(cfmt,1050)n1,n2
 1050   FORMAT('(1X,I2,A1,I4,2X,',i1,'(A9,2X),',i1,
     &         '(F9.2,2X),3X,F9.2,2x,a10)')
        WRITE(Mt1,cfmt)M,'-',Iy,(starz,i2=1,n1),(X(Iobs,i),i=n1+1,Ncol),
     &                 Dmax(Iobs,Nopt),Fnote
       END IF
      END IF
c-----------------------------------------------------------------------
      RETURN
      END

