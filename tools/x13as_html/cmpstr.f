**==cmpstr.f    processed by SPAG 4.03F  at 09:47 on  1 Mar 1994
      LOGICAL FUNCTION cmpstr(Toktyp,Str1,Str2)
      IMPLICIT NONE
c----------------------------------------------------------------------
      INCLUDE 'lex.i'
c----------------------------------------------------------------------
      CHARACTER chr1*1,chr2*1,Str1*(*),Str2*(*)
      INTEGER Toktyp,ichr,nchr1,nchr2
c----------------------------------------------------------------------
      IF(Toktyp.eq.QUOTE)THEN
       cmpstr=Str1.eq.Str2
c----------------------------------------------------------------------
      ELSE IF(Toktyp.eq.NAME)THEN
       nchr1=len(Str1)
       nchr2=len(Str2)
c----------------------------------------------------------------------
       IF(nchr1.eq.nchr2)THEN
        cmpstr=.true.
        DO ichr=1,nchr1
         CALL map(UCASE,LCASE,Str1(ichr:ichr),chr1)
         CALL map(UCASE,LCASE,Str2(ichr:ichr),chr2)
         IF(chr1.ne.chr2)GO TO 10
        END DO
        GO TO 20
       END IF
c----------------------------------------------------------------------
   10  cmpstr=.false.
c----------------------------------------------------------------------
      ELSE
       cmpstr=Str1.eq.Str2
      END IF
c----------------------------------------------------------------------
   20 RETURN
      END
