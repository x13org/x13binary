c-----------------------------------------------------------------------
c     Table pointer variables used for svltbl are of the form LSL<type>
c where the types are 
c-----------------------------------------------------------------------
c seats model                                 smd
c normality test                              nrm
c total squared error                         tse
c component variance                          cvr
c concurrent estimate error                   cee
c percent reduction se                        prs
c average abs. diff. in annual averages       aad
c-----------------------------------------------------------------------
      INTEGER LSLSMD,LSLXMD,LSLSNR,LSLTSE,LSLCVR,LSLCEE,LSLPRS,LSLAAD,
     &        LSLOUE,LSLOUS,LSLSSG,LSLDW,LSLFRS,LSLALS
      PARAMETER(
     &          LSLSMD= 95,LSLXMD= 96,LSLSNR= 98,LSLTSE= 99,LSLCVR=100,
     &          LSLCEE=101,LSLPRS=102,LSLAAD=103,LSLOUE=104,LSLOUS=105,
     &          LSLSSG=106,LSLDW=107,LSLFRS=108,LSLALS=109)
