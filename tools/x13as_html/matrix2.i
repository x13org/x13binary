C
C... Variables in Common Block /matrix2/ ...
C
C Sdt Standard Deviation of Trend Innovation
C Sds Standard Deviation of Seasonal Innovation
C Sdc Standard Deviation of Transitory Innovation
C Sdi Standard Deviation of Irregular Innovation
C Sdsa Standard Deviation of SA Innovation
C SeCect Standard Error Trend Concurrent Estimator
C SeCecsa Standard Error SA Concurrent Estimator
C RSeCect Revision Standard Error Trend Concurrent Estimator
C RSeCecsa Revision Standard Error SA Concurrent Estimator
C Covt1 Convergence Trend after 1 year
C Covsa1 Convergence SA after 1 year
C Covt5 Convergence Trend after 5 year
C Covsa5 Convergence SA after 5 year
C Ssh Significance of seasonality Historical Estimator
C Ssp Significance of seasonality Preliminary Estimator
C Ssf Significance of seasonality Forecast Estimator
C ESS Enough significance of Seasonality 
C T11t total SE Period-to-Period Trend rate of growth
C T11sa total SE Period-to-Period SA rate of growth
C T112t total SE Annual Trend rate of growth
C T112sa total SE Annual SA rate of growth
C T112x total SE Annual Series rate of growth
C Daat Difference in Annuaol Average Trend
C Daasa Difference in Annuaol Average SA
C
      integer Ssh,SSp,SSf,ESS
      real*8 Sdt,Sds,Sdc,Sdi,Sdsa,SeCect,SeCecSa,RseCect,RseCecSa,
     $       Covt1,Covsa1,Covt5,Covsa5,T11t,T11sa,T112t,
     $       T112sa,T112x,Daat,Daasa
      common /matrix2/ Sdt,Sds,Sdc,Sdi,Sdsa,SeCect,
     $                 SeCecSa,RseCect,RseCecSa,Covt1,Covsa1,
     $                 Covt5,Covsa5,T11t,T11sa,T112t,T112sa,
     $                 T112x,Daat,Daasa,Ssh,SSp,SSf,ESS
