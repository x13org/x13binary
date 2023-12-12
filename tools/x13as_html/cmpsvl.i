c-----------------------------------------------------------------------
c     Table pointer variables used for svltbl are of the form LSL<type>
c where the types are 
c-----------------------------------------------------------------------
c ind M1                                         M1
c ind Q                                          Q
c ind Q without M2                               Q2
c ind Moving seasonality ratio                   MSR
c ind I/C Ratio                                  ICR
c ind F-test for stable seasonality, D8          FD8
c ind F-test for moving seasonality, D8          MSF
c test for aggregation smoothness                ITT
c-----------------------------------------------------------------------
      INTEGER LSLIM1,LSLISR,LSLIIR,LSLID8,LSLISF,LSLIID,LSLITT,LSLALI
      PARAMETER(
     &          LSLIM1= 75,LSLISR= 88,LSLIIR= 89,LSLID8= 90,LSLISF= 91,
     &          LSLIID= 92,LSLITT= 93,LSLALI= 94)
