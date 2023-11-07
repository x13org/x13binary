      SUBROUTINE mkspst(Spcsrs,Spcstr,Numchr,Numch2,Lcap)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11log.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'hiddn.cmn'
c-----------------------------------------------------------------------
      CHARACTER Spcstr*(36)
      INTEGER Spcsrs,Numchr,Numch2
      LOGICAL Lcap
c-----------------------------------------------------------------------
      Numch2=0
      IF(Spcsrs.eq.0)THEN
       Numchr=27
       IF(Lcap)THEN
        Spcstr(1:Numchr)=' Original Series (Table A1)'
        Numch2=16
       ELSE
        Spcstr(1:Numchr)=' original series (Table A1)'
       END IF
      ELSE IF(Spcsrs.eq.1)THEN
       IF(Adjls.eq.1.or.Adjao.eq.1.or.Adjtc.eq.1)THEN
        Numchr=36
        IF(Lcap)THEN
         Spcstr(1:Numchr)=' Outlier Adjusted Series (Table A19)'
         Numch2=24
        ELSE
         Spcstr(1:Numchr)=' outlier adjusted series (Table A19)'
        END IF
       ELSE
        Numchr=34
        IF(Lcap)THEN
         Spcstr(1:Numchr)=' Original Series (Table A1 or A19)'
         Numch2=16
        ELSE
         Spcstr(1:Numchr)=' original series (Table A1 or A19)'
        END IF
       END IF
      ELSE IF(Spcsrs.eq.2)THEN
       Numchr=33
       IF(Adjls.eq.1.or.Adjao.eq.1.or.Adjtc.eq.1.or.Adjtd.eq.1.or.
     &    Adjhol.eq.1.or.Adjsea.eq.1.or.Adjusr.eq.1.or.Nprtyp.gt.0.or.
     &    Kswv.ne.0.or.(Ixreg.ge.2.and.Axrgtd))THEN
        IF(Lcap)THEN
         Spcstr(1:Numchr)=' Prior Adjusted Series (Table B1)'
         Numch2=22
        ELSE
         Spcstr(1:Numchr)=' prior adjusted series (Table B1)'
        END IF
       ELSE
        IF(Lcap)THEN
         Spcstr(1:Numchr)=' Original Series (Table A1 or B1)'
         Numch2=16
        ELSE
         Spcstr(1:Numchr)=' original series (Table A1 or B1)'
        END IF
       END IF
      ELSE IF(Spcsrs.eq.3)THEN
       Numchr=36
       IF(Lcap)THEN
        Spcstr(1:Numchr)=' Modified Original Series (Table E1)'
        Numch2=25
       ELSE
        Spcstr(1:Numchr)=' modified original series (Table E1)'
       END IF
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
      