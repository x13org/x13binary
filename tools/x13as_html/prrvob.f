C     Last change:  BCM  26 Apr 1998    2:45 pm
      SUBROUTINE prrvob(Rev,Ncol,First,Last,By,Ipby,Ihdr,Nihd,Hdrttl,
     &                  Hdrptr,Nhdrtl,Hd2ttl,Hd2ptr,Nhd2tl,I0,Tfmt,Jyp,
     &                  Pcol,Pcol1,Colwid,Tblttl,Lblank)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'tfmts.cmn'
      INCLUDE 'title.cmn'
c-----------------------------------------------------------------------
      INTEGER LINLIM
      LOGICAL F
      PARAMETER(LINLIM=60,F=.false.)
c-----------------------------------------------------------------------
      CHARACTER cblank*(22),dash*(22),Ihdr*(13),Tblttl*(*),Tfmt*(*),
     &          hvec*(5),fmt1*(7),fmt2*(9),Hdrttl*(*),Hd2ttl*(*)
      LOGICAL Lblank
      DOUBLE PRECISION Rev,trv
      INTEGER Ncol,First,Last,By,Ipby,Jyp,j,Pcol,Pcol1,Nhd2tl,k,Colwid,
     &        I0,ielt,Nhdrtl,Hdrptr,Hd2ptr,nc,i,Nihd
c-----------------------------------------------------------------------
      DIMENSION Rev(0:Pcol,*),trv(7),hvec(5),Hdrptr(0:Pcol1),
     &          Hd2ptr(0:Pcol1)
c-----------------------------------------------------------------------
      DATA hvec  / ' Min ',' 25% ',' Med ',' 75% ',' Max ' /
      DATA cblank / '                      ' /
      DATA dash  / '----------------------' /
c-----------------------------------------------------------------------
c     IF page breaks printed out, make sure this subtable is printed on
c     a full page.
c-----------------------------------------------------------------------
      IF(I0.gt.0)THEN
       IF(I0.gt.0)THEN
        I0=I0+(Last-First)/By+2
        IF(I0.gt.LINLIM)I0=-1
       END IF
       IF(I0.le.0)THEN
        IF(I0.lt.0)THEN
*         WRITE(Mt1,Ttlfmt)Newpg,Title(1:Ntitle),Kpage,Serno(1:Nser)
*         Kpage=Kpage+1
         WRITE(Mt1,1000)Tblttl
 1000    FORMAT(/,1x,a,/)
        END IF
        WRITE(fmt1,1010)'a',2*Nhdrtl
 1010   FORMAT('(',a,',',i2,'a)')
        WRITE(Mt1,fmt1)'  Date ',
     &        (cblank(1:max(Colwid+Hdrptr(ielt-1)-Hdrptr(ielt)+1,1)),
     &         Hdrttl(Hdrptr(ielt-1):Hdrptr(ielt)-1),ielt=1,Nhdrtl)
        IF(Nhd2tl.gt.0)THEN
         WRITE(fmt2,1010)'t8',2*Nhd2tl
         WRITE(Mt1,fmt2)
     &         (cblank(1:max(Colwid+Hd2ptr(ielt-1)-Hd2ptr(ielt)+1,1)),
     &          Hd2ttl(Hd2ptr(ielt-1):(Hd2ptr(ielt)-1)),ielt=1,Nhd2tl)
         WRITE(Mt1,fmt1)'  ---- ',
     &         (cblank(1:max(Colwid+Hd2ptr(ielt-1)-Hd2ptr(ielt)+1,1)),
     &          dash(1:(Hd2ptr(ielt)-Hd2ptr(ielt-1))),ielt=1,Nhdrtl)
        ELSE
         WRITE(Mt1,fmt1)'  ---- ',
     &         (cblank(1:max(Colwid+Hdrptr(ielt-1)-Hdrptr(ielt)+1,1)),
     &          dash(1:(Hdrptr(ielt)-Hdrptr(ielt-1))),ielt=1,Nhdrtl)
        END IF
        I0=6
       END IF
      END IF
c-----------------------------------------------------------------------
c     Print header for subtable
c-----------------------------------------------------------------------
      WRITE(Mt1,1020)cblank(1:2)//Ihdr(1:Nihd)
 1020 FORMAT(a)
c-----------------------------------------------------------------------
c     Print out entries for each subtable
c-----------------------------------------------------------------------
      j=Jyp
      nc=Ncol+1
      DO i=First,Last,By
       DO k=0,Ncol
        trv(k+1)=Rev(k,i)
       END DO
       j=j+1
       IF(Ipby.eq.1)THEN
        CALL wrttbl(trv,Jyp,Colhdr(j)((Tblwid-4):Tblwid),nc,2,Mt1,
     &              Tfmt,0,F)
       ELSE IF(Ipby.eq.2)THEN
        CALL wrttbl(trv,j,'XXXXX',nc,2,Mt1,Tfmt,0,F)
       ELSE IF(Ipby.eq.3)THEN
        CALL wrttbl(trv,j,hvec(i),nc,2,Mt1,Tfmt,0,F)
       ELSE
        CALL wrttbl(trv,j,'     ',nc,2,Mt1,Tfmt,0,F)
       END IF
      END DO
c-----------------------------------------------------------------------
      IF(Lblank)WRITE(Mt1,1020)cblank(1:2)
c-----------------------------------------------------------------------
      RETURN
      END

