      SUBROUTINE cvcmma(Chrstr,Ncomma)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Replace commas with periods in text string, return number of
c     commas found.
c     Created by : BCMonsell, April 2003
c-----------------------------------------------------------------------
      CHARACTER Chrstr*(*)
      INTEGER clen,i,i1,Ncomma
c-----------------------------------------------------------------------
      INTEGER nblank
      EXTERNAL nblank
c-----------------------------------------------------------------------
c     Initialize variables for number of commas and length of string -
c     return if length of string = 0
c-----------------------------------------------------------------------
      i1=1
      Ncomma=0
      clen=nblank(Chrstr)
      IF(clen.eq.0)RETURN
c-----------------------------------------------------------------------
c     search for a comma in Chrstr
c-----------------------------------------------------------------------
      i=index(Chrstr(1:clen),',')
c-----------------------------------------------------------------------
c     each time a comma is found, generate the position of that comma
c     in the string and replace it with a '.'
c-----------------------------------------------------------------------
      DO WHILE (i.gt.0)
       i1=i+i1-1
       Chrstr(i1:i1) = '.'
c-----------------------------------------------------------------------
c     update number of commas (data observations) found
c-----------------------------------------------------------------------
       Ncomma=Ncomma+1
c-----------------------------------------------------------------------
c      if position of last comma found is the end of the character
c      string, set i to 0
c-----------------------------------------------------------------------
       if (i1.eq.clen) then
        i=0
c-----------------------------------------------------------------------
c     else, search for a comma in rest of Chrstr, and store the
c     position relative to the last comma found.
c-----------------------------------------------------------------------
       else
        i1 = i1+1
        i=index(Chrstr(i1:clen),',')
       end if
      end do
c-----------------------------------------------------------------------
      RETURN
      END
