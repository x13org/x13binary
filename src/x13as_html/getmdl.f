C     Last change:  BCM  15 Jan 98   12:12 pm
      SUBROUTINE getmdl(Locok,Inptok,Lauto)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'lex.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      CHARACTER str*(POPRCR)
      LOGICAL arfix,argok,dffix,getint,havreg,hvsea,Inptok,Locok,mafix,
     &        Lauto
      INTEGER arerr,arlag,begmdl,dferr,dflag,facsp,itmp,malag,MULT,nar,
     &        naimcf,nchr,ndcoef,ndf,nma,numopr,maerr
      DOUBLE PRECISION arcoef,dfcoef,macoef
      PARAMETER(MULT=3)
      DIMENSION arcoef(PORDER),arerr(2),arfix(PORDER),arlag(PORDER),
     &          dfcoef(PDIFOR),dferr(2),dffix(PDIFOR),dflag(PDIFOR),
     &          macoef(PORDER),maerr(2),mafix(PORDER),malag(PORDER),
     &          begmdl(2)
      EXTERNAL getint
c-----------------------------------------------------------------------
c     Get factors (AR DIFF MA)SP until the next name.
c-----------------------------------------------------------------------
      Locok=T
      havreg=F
      hvsea=F
      CALL cpyint(Lstpos,2,1,begmdl)
      Mdldsn(1:(1+Lineln-begmdl(PCHAR)))=Linex(begmdl(PCHAR):Lineln)
      Nseadf=0
      Nnsedf=0
      naimcf=0
      numopr=0
c-----------------------------------------------------------------------
c     Get factors (AR DIFF MA)SP until the next name.  Note that the
c seasonal period (SP) after the parenthesis is optional in most cases.  
c There may also be commas between the operators.  Also, we cannot 
c insert each operator into the model and check for errors as they 
c return from GETOPR because we need to have the periodicity of the 
c factor first.
c-----------------------------------------------------------------------
      DO WHILE (Nxtktp.eq.LPAREN)
c     ------------------------------------------------------------------
       CALL lex()
       CALL cpyint(Lstpos,2,1,arerr)
       CALL getopr(AR,arcoef,arlag,arfix,nar,itmp,naimcf,argok,Locok)
       IF(Lfatal)RETURN
       IF(Nxtktp.eq.COMMA)CALL lex()
       CALL cpyint(Lstpos,2,1,dferr)
       CALL getopr(DIFF,dfcoef,dflag,dffix,ndcoef,ndf,naimcf,argok,
     &             Locok)
       IF(Lfatal)RETURN
       IF(Nxtktp.eq.COMMA)CALL lex()
       CALL cpyint(Lstpos,2,1,maerr)
       CALL getopr(MA,macoef,malag,mafix,nma,itmp,naimcf,argok,Locok)
       IF(Lfatal)RETURN
c     ------------------------------------------------------------------
       IF(Nxtktp.ne.RPAREN)THEN
        CALL inpter(PERROR,Lstpos,'Expected ")" after (AR DIFF MA',T)
        Locok=F
c-----------------------------------------------------------------------
c     Save the end of the model string so we can print out the model
c description as is.  Note we are in trouble if the model is written
c over one line.  Unfortunately it would be too hard to print out the
c ARIMA model description from the data stored since all the AR
c operators from all the factors are stored together, all the
c differening, and MA.  Also, we would have to figure the seasonal
c period of each operator by figuring the least common denominator.
c-----------------------------------------------------------------------
       ELSE
        Nmddcr=Pos(PCHAR)-begmdl(PCHAR)
        CALL lex()
c-----------------------------------------------------------------------
c     Get the period of the factor if it exists.  If it is implied,
c determine it from the following rules:
c-----------------------------------------------------------------------
        IF(Nxtktp.eq.INTGR)THEN
         Nmddcr=Pos(PCHAR)-begmdl(PCHAR)
         argok=getint(facsp)
         IF(facsp.le.0)THEN
          CALL inpter(PERROR,Lstpos,
     &                'Period specified in (AR DIFF MA)period must be'//
     &                ' greater than zero.',T)
          Locok=F
         END IF
c-----------------------------------------------------------------------
c     (1) If there has been no previous regular factor, then it is a
c regular factor;
c-----------------------------------------------------------------------
        ELSE IF(.not.havreg)THEN
         facsp=1
         havreg=T
c-----------------------------------------------------------------------
c     (2) If there has been a previous regular factor and the data has
c a seasonal periodicity, then the factor has the seasonal periodicity,
c Sp;
c-----------------------------------------------------------------------
        ELSE IF(.not.hvsea.and.Sp.gt.1)THEN
         facsp=Sp
         hvsea=T
c-----------------------------------------------------------------------
c     (3) Since only one regular and one seasonal factor are allowed per
c model, anyother implicit factor is an error.
c-----------------------------------------------------------------------
        ELSE
         CALL inpter(PERROR,Lstpos,
     &     'Must explicitly specify the period in (AR DIFF MA)period',T)
         Locok=F
        END IF
c-----------------------------------------------------------------------
c     Check the the order of this AR operator and insert it if it is not
c too large.  If there is not enough room left in the model variables
c to store the operator, INSOPR will report that error.
c-----------------------------------------------------------------------
        IF(Locok)THEN
         IF(nar.gt.0)THEN
          numopr=numopr+1
          IF(numopr.gt.POPR)THEN
           CALL inpter(PERROR,arerr,
     &                 'Too many operators in specified ARIMA model',T)
           Locok=F
           GO TO 20
          END IF
          CALL iscrfn(MULT,facsp,arlag,nar,PORDER,arlag)
          CALL mkoprt(AR,facsp,Sp,str,nchr)
          IF(.not.Lfatal)CALL insopr(AR,arcoef,arlag,arfix,nar,facsp,
     &                               str(1:nchr),argok,Locok)
          IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c      Check the of maximum lag of all the AR operators added so far
c does not exceed the maximum order otherwise is will exceed temporary
c storage in the filtering operations where the operators are
c expanded/multiplied into just the coefficients of one full operator.
c This is only going to be a problem for seasonal and missing lag
c models.
c-----------------------------------------------------------------------
          CALL maxlag(Arimal,Opr,Mdl(AR-1),Mdl(AR)-1,Mxarlg)
          IF(Mxarlg.gt.PORDER)THEN
           CALL inpter(PERROR,arerr,
     &                 'Order of the AR operator is too large.',T)
           Locok=F
          END IF
         ELSE IF(nar.lt.0)THEN
          CALL inpter(PERROR,arerr,
     &           'Order of the AR operator cannot be less than zero.',T)
          Locok=F
         END IF
c-----------------------------------------------------------------------
c     Check that we don't have a seasonal difference and a seasonal
c effect variables or a U(B) operator.
c-----------------------------------------------------------------------
         IF(ndcoef.gt.0)THEN
          numopr=numopr+1
          IF(numopr.gt.POPR)THEN
           CALL inpter(PERROR,dferr,
     &                 'Too many operators in specified ARIMA model',T)
           Locok=F
           GO TO 20
          END IF
          Lseadf=(Sp.gt.1.and.facsp.eq.Sp).or.
     &           (Sp.eq.1.and.ndcoef.eq.Sp-1)
c     ------------------------------------------------------------------
          IF(Lseadf.and.Lseff)THEN
           CALL inpter(PERROR,dferr,'Cannot have a '//
     &        'seasonal difference with seasonal regression effects.',T)
           Locok=F
          END IF
c     ------------------------------------------------------------------
c     Accumulate the number of seasonal and nonseasonal differences to
c print out in the table of estimates.
c-----------------------------------------------------------------------
          IF(facsp.eq.1)Nnsedf=Nnsedf+ndf
          IF(facsp.eq.Sp.and.Sp.gt.1)Nseadf=Nseadf+ndf
c-----------------------------------------------------------------------
c     Check that the number of coefficients for the new differencing
c operator is not too large so it won't exceed the temporary storage
c when it is inserted.
c-----------------------------------------------------------------------
          IF(ndcoef.gt.PDIFOR)THEN
           CALL inpter(PERROR,dferr,
     &             'Order of the differencing operator is too large.',T)
           Locok=F
          ELSE
           CALL iscrfn(MULT,facsp,dflag,ndcoef,PDIFOR,dflag)
           CALL mkoprt(DIFF,facsp,Sp,str,nchr)
           IF(.not.Lfatal)CALL insopr(DIFF,dfcoef,dflag,dffix,ndcoef,
     &                                facsp,str(1:nchr),argok,Locok)
           IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Check the of maximum lag of all the differencing operators added
c so far does not exceed the maximum order otherwise is will exceed
c temporary storage in the filtering operations where the operators
c are expanded/multiplied into just the coefficients of one full
c operator.  This is only going to be a problem for seasonal and missing
c lag models.
c-----------------------------------------------------------------------
           CALL maxlag(Arimal,Opr,Mdl(DIFF-1),Mdl(DIFF)-1,Mxdflg)
           IF(Mxdflg.gt.PDIFOR)THEN
            CALL inpter(PERROR,dferr,
     &        'Order of the full differencing operator is too large.',T)
            Locok=F
           END IF
          END IF
         ELSE IF(ndf.lt.0)THEN
          CALL inpter(PERROR,dferr,
     &                'Order of the differencing operator cannot be '//
     &                'less than zero.',T)
          Locok=F
         END IF
c-----------------------------------------------------------------------
c     Check that the number of coefficients for the new MA operator is
c not too large so it won't exceed the temporary storage when it is
c inserted.
c-----------------------------------------------------------------------
         IF(nma.gt.PORDER)THEN
          CALL inpter(PERROR,maerr,
     &                'Order of the MA operator is too large',T)
          Locok=F
         ELSE IF(nma.gt.0)THEN
          numopr=numopr+1
          IF(numopr.gt.POPR)THEN
           CALL inpter(PERROR,maerr,
     &                 'Too many operators in specified ARIMA model',T)
           Locok=F
           GO TO 20
          END IF
          CALL iscrfn(MULT,facsp,malag,nma,PORDER,malag)
          CALL mkoprt(MA,facsp,Sp,str,nchr)
          IF(.not.Lfatal)CALL insopr(MA,macoef,malag,mafix,nma,facsp,
     &                               str(1:nchr),argok,Locok)
          IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c      Check the of maximum lag of all the MA operators added so far
c does not exceed the maximum order otherwise is will exceed temporary
c storage in the filtering operations where the operators are
c expanded/multiplied into just the coefficients of one full operator.
c This is only going to be a problem for seasonal and missing lag
c models.
c-----------------------------------------------------------------------
          CALL maxlag(Arimal,Opr,Mdl(MA-1),Mdl(MA)-1,Mxmalg)
          IF(Mxmalg.gt.PORDER)THEN
           CALL inpter(PERROR,Errpos,
     &                 'Order of the MA operator is too large.',T)
           Locok=F
          END IF
         ELSE IF(nma.lt.0)THEN
          CALL inpter(PERROR,Errpos,
     &           'Order of the MA operator cannot be less than zero.',T)
          Locok=F
         END IF
c-----------------------------------------------------------------------
c     If there is another factor, process it.  I don't know that the
c error below the END DO will ever be used.
c-----------------------------------------------------------------------
         IF(Nxtktp.eq.LPAREN)GO TO 10
        END IF
       END IF
c-----------------------------------------------------------------------
c     We processed the last factor so break out of the factor processing
c while loop.  Increment NMDL, indicating we have an ARIMA model.  If
c there is an error, skip the rest of the ARIMA model description to
c process the rest of the input file for errors.
c-----------------------------------------------------------------------
       GO TO 20
   10  CONTINUE
      END DO
      CALL inpter(PERROR,Lstpos,'Expected "(" in  (AR DIFF MA)',T)
      Locok=F
c     ------------------------------------------------------------------
   20 IF(Locok)THEN
       Nmdl=Nmdl+1
      ELSE
       CALL skparm(Lauto)
      END IF
c     ------------------------------------------------------------------
      Inptok=Inptok.and.Locok
c     ------------------------------------------------------------------
      RETURN
      END
