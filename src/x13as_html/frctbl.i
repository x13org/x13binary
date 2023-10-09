c-----------------------------------------------------------------------
c     Table pointer variables used for prttbl and savtbl are of the form
c L<spec><type> where the spec codes are 
c-----------------------------------------------------------------------
c force      FRC or FC
c-----------------------------------------------------------------------
c and the types are
c-----------------------------------------------------------------------
c change in adjusted seasonal adjusted              E6A 
c change in rounded seasonal adjusted               E6R 
c seasonal adjusted, yr totals adj                  SAA
c rounded seasonal adjusted                         RND
c-----------------------------------------------------------------------
      INTEGER LFCSAA,LFCRND,LFCE6A,LFC6AP,LFCE6R,LFC6RP,LFRCCR,LFRCRR,
     &        LFRFAC
      PARAMETER(
     &          LFCSAA=209,LFCRND=210,LFCE6A=211,LFC6AP=212,LFCE6R=213,
     &          LFC6RP=214,LFRCCR=215,LFRCRR=216,LFRFAC=217)
