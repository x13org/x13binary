c-----------------------------------------------------------------------
c     Table pointer variables used for svltbl are of the form LSL<type>
c where the types are 
c-----------------------------------------------------------------------
c Average absolute revision for seasonally adjusted series        ASA
c Average absolute revision for seasonal factors                  ASF
c Average absolute revision for projected seasonal factors        ASP
c Percent flagged for seasonal factors                            SFP
c-----------------------------------------------------------------------
      INTEGER LSLASA,LSLASF,LSLASP,LSLAFE,LSLALR,LSLPCT
      PARAMETER(
     &          LSLASA= 50,LSLASF= 55,LSLASP= 56,LSLAFE= 57,LSLALR= 58,
     &          LSLPCT= 59)
