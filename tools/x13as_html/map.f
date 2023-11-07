**==map.f    processed by SPAG 4.03F  at 09:51 on  1 Mar 1994
      SUBROUTINE map(Frcset,Tocset,Str1,Str2)
      IMPLICIT NONE
c----------------------------------------------------------------------
c     Maps fromcset characters to tocset characters within the domain
c of fromcset and copies otherwise.
c----------------------------------------------------------------------
      LOGICAL T
      PARAMETER(T=.true.)
c     ------------------------------------------------------------------
      INCLUDE 'lex.i'
c     -----------------------------------------------------------------
      CHARACTER Frcset*(*),Tocset*(*),Str1*(*),Str2*(*)
      INTEGER ichr,indx,mapind
      EXTERNAL indx
c     -----------------------------------------------------------------
      IF(len(Frcset).ne.len(Tocset))THEN
       CALL inpter(PERROR,Pos,'Map cset''s not the same length',T)
c     -----------------------------------------------------------------
      ELSE IF(len(Str2).lt.len(Str1))THEN
       CALL inpter(PERROR,Pos,'Map output string not long enough',T)
c     -----------------------------------------------------------------
      ELSE
       Str2=Str1
c     -----------------------------------------------------------------
       DO ichr=1,len(Str1)
        mapind=indx(Frcset,Str1(ichr:ichr))
        IF(mapind.gt.0)Str2(ichr:ichr)=Tocset(mapind:mapind)
       END DO
      END IF
c     -----------------------------------------------------------------
      RETURN
      END
