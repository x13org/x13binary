**==shlsrt.f    processed by SPAG 4.03F  at 17:22 on 11 Mar 1994
      SUBROUTINE shlsrt(Nr,Vecx)
c-----------------------------------------------------------------------
c     Returns sorted Vecx. Uses a shell sort.
c-----------------------------------------------------------------------
c Name  Type Description
c-----------------------------------------------------------------------
c abss    d  Work pa long nr used vector to hold the sorted absolute
c             values
c bot     i  Local index to the element at the bottom of the gap, i.e.
c             index with the lower value.
c gap     i  Local distance between the records that are being compared.
c             gap starts out at half the number of records and is halved
c             until it reaches 1.
c i       i  Local do loop
c median  d  Output median of the absolute differences
c nabss   i  Work PARAMETER for the length of abss
c nr      i  Input row dimension of s
c nsrt    i  Local number of comparisons to make on one pass through the
c             records
c pa      i  Local PARAMETER for the maximum number of innovation errors
c s       d  Input nr long vector to be sorted.
c tmp     d  Local temporary scalar
c top     i  Local index to the element at the top of the gap, i.e.
c             index with the higher value and gap higher than bot.
c-----------------------------------------------------------------------
c     Type the variables
c-----------------------------------------------------------------------
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INTEGER bot,gap,Nr,nsrt,top
      DOUBLE PRECISION Vecx,tmp
      DIMENSION Vecx(*)
c-----------------------------------------------------------------------
c     Use a Shell sort the nr records of Vecx.  Compares records half
c the number of records apart, then keep halving the gap size until
c records next to eachother are compared.
c-----------------------------------------------------------------------
      gap=Nr
      DO WHILE (.true.)
       gap=gap/2
       IF(gap.gt.0)THEN
        nsrt=Nr-gap
c-----------------------------------------------------------------------
c     Compare and sort nsrt records that are gap records apart.
c-----------------------------------------------------------------------
        bot=0
        DO WHILE (.true.)
         bot=bot+1
         IF(bot.le.nsrt)THEN
          DO WHILE (.true.)
c     ------------------------------------------------------------------
           top=bot+gap
c-----------------------------------------------------------------------
c     See if Vecx(top) and Vecx(bot) need to be exchanged and switch
c them if they do.
c-----------------------------------------------------------------------
           IF(Vecx(bot).le.Vecx(top))GO TO 10
           tmp=Vecx(top)
           Vecx(top)=Vecx(bot)
           Vecx(bot)=tmp
c     ------------------------------------------------------------------
           IF(bot.le.gap)GO TO 10
           bot=bot-gap
          END DO
         END IF
         GO TO 20
   10    CONTINUE
        END DO
       END IF
c     ------------------------------------------------------------------
       bot=Nr/2
c     ------------------------------------------------------------------
       RETURN
   20  CONTINUE
      END DO
      END
