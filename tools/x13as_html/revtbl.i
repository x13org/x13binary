c-----------------------------------------------------------------------
c     Table pointer variables used for prttbl and savtbl are of the form
c L<spec><type> where the spec codes are 
c-----------------------------------------------------------------------
c revision       REV or RV
c-----------------------------------------------------------------------
c and the types are
c-----------------------------------------------------------------------
c table of outliers identified during revision analysis      OT
c table of msr seasonal filters in revision period           R0
c revision of s.a. data                                      R1
c conc and final est. of s. a. data                          R1A
c revision of seasonal                                       R2
c conc and final est. of seasonal                            R2A
c revision of change in s.a.                                 R3
c conc and final est. of change in s.a.                      R3A
c revision of trend                                          R4
c conc and final est. of trend                               R4A
c likelihood revisions                                       R5
c forecast revisions                                         R6
c-----------------------------------------------------------------------
      INTEGER LRVHDR,LREVOT,LREVR0,LREVR1,LREVR2,LREVR3,LREVR4,LREVR5,
     &        LREVR6,LREVR7,LREVR8,LREVR9,LRVSSH,LRVR9A,LRVR9B
      PARAMETER(
     &          LRVHDR=240,LREVOT=241,LREVR0=242,LREVR1=243,LREVR2=246,
     &          LREVR3=249,LREVR4=252,LREVR5=255,LREVR6=258,LREVR7=261,
     &          LREVR8=262,LREVR9=264,LRVSSH=265,LRVR9A=266,LRVR9B=267)
