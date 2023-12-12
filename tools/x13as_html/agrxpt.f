**==agrxpt.f    processed by SPAG 4.03F  at 09:53 on  1 Mar 1994
      SUBROUTINE agrxpt(Begspn,Sp)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Ensure that the "pointers" variables for direct and indirect
c     adjustments are X-11 to tell where backcasts,
c     data, forecasts begin and end.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'agr.cmn'
      INCLUDE 'agrsrs.cmn'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'extend.cmn'
*      INCLUDE 'lzero.cmn'
c-----------------------------------------------------------------------
      INTEGER Begspn,Sp,i,i2
      DIMENSION Begspn(2)
c-----------------------------------------------------------------------
      IF(Pos1ob.gt.Ind1ob)THEN
c-----------------------------------------------------------------------
c   Copy the aggregated series used for the indirect adjustment so that
c   the data for the starting and ending dates of the aggregated series
c   match those set for the direct adjustment
c-----------------------------------------------------------------------
       DO i=Indffc,1,-1
        i2=i+(Pos1ob-Ind1ob)
        O(i2)=O(i)
        O2(i2)=O2(i)
        O3(i2)=O3(i)
        O4(i2)=O4(i)
        O5(i2)=O5(i)
        Ci(i2)=Ci(i)
        Omod(i2)=Omod(i)
        Tem(i2)=Tem(i)
       END DO
c-----------------------------------------------------------------------
c   Update pointers so that the data portion matches those of the
c   direct
c-----------------------------------------------------------------------
       Ind1ob=Pos1ob
       Indfob=Posfob
       Ind1bk=Ind1ob-Indnbc
       Indffc=Indfob+Indnfc
       CALL cpyint(Begbk2,2,1,Ibgbk2)
c-----------------------------------------------------------------------
      ELSE IF(Pos1ob.lt.Ind1ob)THEN
c-----------------------------------------------------------------------
c   Update pointers so that the data portion matches those of the
c   indirect series
c-----------------------------------------------------------------------
       Pos1ob=Ind1ob
       Posfob=Indfob
       Pos1bk=Pos1ob-Nbcst
       Posffc=Posfob+Nfcst
       CALL cpyint(Ibgbk2,2,1,Begbk2)
       CALL dfdate(Begspn,Begbk2,Sp,Nbcst2)
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
