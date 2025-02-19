      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : ARTIF   
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , PAR   , FACT  , S     , DSDX  
      DOUBLE PRECISION D2SDX2, D     , THAT  
      INTRINSIC ATAN  , SIN   , COS   , MOD   
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  ELEMENT TYPE : TRIG      
C
       X      = XVALUE(IELVAR(ILSTRT+     1))
       PAR    = EPVALU(IPSTRT+     1)
       THAT   = 100.0                                    
       FACT   = MOD( PAR, THAT )                         
       S      = SIN( FACT * X )                          
       DSDX   = FACT * COS( FACT * X )                   
       D2SDX2 = - FACT * FACT * S                        
       D      = 1.0 + S * S                              
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= ATAN( S )                                
       ELSE
        FUVALS(IGSTRT+     1)= DSDX / D                                 
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=( D2SDX2 * D - 2.0 * S * DSDX**2 ) /D**2 
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
