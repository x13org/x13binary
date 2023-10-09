      SUBROUTINE fdate(Dattim)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Mimics the SUN system routine that prints the date and time
c-----------------------------------------------------------------------
      CHARACTER Dattim*24
c-----------------------------------------------------------------------
c      date is a Lahey DOS interface routine that returns the 
c      current time of day in MM/DD/YY format, left-justified and
c      space filled.
c-----------------------------------------------------------------------
      call date_and_time(date=Dattim(3:10))
c-----------------------------------------------------------------------
c      time is a Lahey DOS interface routine that returns the 
c      current time of day in HH:MM:SS.hh format, left-justified and
c      space filled.
c-----------------------------------------------------------------------
      CALL date_and_time(time=Dattim(13:24))
c     ------------------------------------------------------------------
      RETURN
      END
