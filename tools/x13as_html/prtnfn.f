C     Last change:  BCM  27 Oct 97    1:07 pm
      SUBROUTINE prtnfn(Fcntyp,Lam,Pcode)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     prtnfn.f, Release 1, Subroutine Version 1.4, Modified 24 Oct 1994.
c-----------------------------------------------------------------------
C     Prints the Logit or Box-Cox transformation formulae
c-----------------------------------------------------------------------
      DOUBLE PRECISION ONE,ZERO
      PARAMETER(ONE=1D0,ZERO=0D0)
c     ------------------------------------------------------------------
      INCLUDE 'title.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'htmlout.cmn'
c     ------------------------------------------------------------------
      CHARACTER ytrans*(29)
      INTEGER Fcntyp,nytrns,Pcode
      DOUBLE PRECISION Lam
c     ------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c     ------------------------------------------------------------------
      call setchr(' ',29,ytrans)
      IF(Fcntyp.eq.3)THEN
       nytrns=12
       ytrans(1:nytrns)='log(y/(1-y))'
c     ------------------------------------------------------------------
      ELSE IF(Fcntyp.eq.0)THEN
       nytrns=19
       ytrans(1:nytrns)='Automatic selection'
c     ------------------------------------------------------------------
      ELSE IF(dpeq(Lam,ZERO))THEN
       nytrns=6
       ytrans(1:nytrns)='Log(y)'
c     ------------------------------------------------------------------
      ELSE IF(dpeq(Lam,ONE))THEN
       nytrns=17
       ytrans(1:nytrns)='No transformation'
c     ------------------------------------------------------------------
      ELSE IF(dpeq(Lam,0.5D0))THEN
       nytrns=7
       ytrans(1:nytrns)='sqrt(y)'
c     ------------------------------------------------------------------
      ELSE IF(Lam.ge.ZERO)THEN
       nytrns=28
       WRITE(ytrans(1:nytrns),1010)Lam,Lam,Lam
 1010  FORMAT(f5.2,'^2+((y^',f5.2,')-1)/',f5.2)
      ELSE
       nytrns=29
       WRITE(ytrans(1:nytrns),1020)Lam,Lam,Lam
 1020  FORMAT(f5.2,'^2+((y^',f5.2,')-1)/(',f5.2,')')
      END IF
c     ------------------------------------------------------------------
      IF(Pcode.eq.0)THEN
       IF(Lcmpaq)THEN
        CALL mkPOneLine(Mt1,'@',
     &            '<strong>Transformation:</strong> '//ytrans(1:nytrns))
       ELSE
        WRITE(Mt1,1030)Cbr,ytrans(1:nytrns)
       END IF
      ELSE IF(Pcode.eq.1)THEN
       WRITE(Nform,1040)'transform',ytrans(1:nytrns)
      ELSE
       WRITE(Nform,1040)'aictrans',ytrans(1:nytrns)
      END IF
 1030 FORMAT(' <p><strong>Transformation:</strong> ',a,/,
     &       ' &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;',a,'</p>')
 1040 FORMAT(a,': ',a)
c     ------------------------------------------------------------------
      RETURN
      END
