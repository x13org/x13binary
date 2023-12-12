c-----------------------------------------------------------------------
c     Table pointer variables used for svltbl are of the form LSL<type>
c where the types are 
c-----------------------------------------------------------------------
c aic tests                         ATS
c automatic model selected          AMD
c AIC                               AIC
c BIC                               BIC
c Forecast error diagnostic         AFC
c-----------------------------------------------------------------------
      INTEGER LSLTRN,LSLAMD,LSLADF,LSLB5M,LSLMU,LSLFUR,LSLALA,LSLAMX,
     &        LSLAIC,LSLACC,LSLBIC,LSLHQ,LSLEIC,LSLAFC,LSLRTS,LSLALE,
     &        LSLTST,LSLCTS,LSLAOT,LSLNRM,LSLSAC,LSLLBQ,LSLSFT,LSLTFT,
     &        LSLCDW,LSLCFR,LSLALC
      PARAMETER(
     &          LSLTRN=  1,LSLAMD=  2,LSLADF=  3,LSLB5M=  4,LSLMU=  5,
     &          LSLFUR=  6,LSLALA=  7,LSLAMX=  8,LSLAIC=  9,LSLACC= 10,
     &          LSLBIC= 11,LSLHQ= 12,LSLEIC= 13,LSLAFC= 14,LSLRTS= 15,
     &          LSLALE= 16,LSLTST= 17,LSLCTS= 18,LSLAOT= 19,LSLNRM= 20,
     &          LSLSAC= 21,LSLLBQ= 22,LSLSFT= 24,LSLTFT= 25,LSLCDW= 26,
     &          LSLCFR= 27,LSLALC= 28)
