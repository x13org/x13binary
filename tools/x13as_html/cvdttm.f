      CHARACTER*24 FUNCTION cvdttm(datstr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Function to convert Lahey fortran (PC) date and time convention
c     to a common format for date and time information
c-----------------------------------------------------------------------
      CHARACTER datstr*24,cmonth*3,cm*3
      INTEGER d,y,h,minute,s
      INTEGER mon
      DIMENSION cmonth(12)
      DATA cmonth/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep',
     &            'Oct','Nov','Dec'/
c      write(*,*)'*',datstr,'*'
      if(datstr(3:3).eq.'/')THEN
       READ(datstr,1010)mon,d,y,h,minute,s
 1010  FORMAT(6(i2,1x))
       WRITE(cvdttm,1020)cmonth(mon),d,y+2000,h,minute,s
      ELSE
       READ(datstr,1011)cm,d,h,minute,s,y
 1011  FORMAT(4x,a3,4(1x,i2),1x,i4)
       WRITE(cvdttm,1020)cm,d,y,h,minute,s
      END IF
 1020 FORMAT(1x,a3,1x,i2,', ',i4,2x,2(i2.2,'.'),i2.2,1x)
      RETURN
      END
c Jan 30, 2003  11.56.55
