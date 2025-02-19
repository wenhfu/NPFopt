      SUBROUTINE ELFUN ( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, LTYPEE, LSTAEV, LELVAR, LNTVAR, 
     *                   LSTADH, LSTEPA, LCALCF, LFVALU, LXVALU, 
     *                   LEPVLU, IFFLAG, IFSTAT )
      INTEGER NCALCF, IFFLAG, LTYPEE, LSTAEV, LELVAR, LNTVAR
      INTEGER LSTADH, LSTEPA, LCALCF, LFVALU, LXVALU, LEPVLU
      INTEGER IFSTAT
      INTEGER ITYPEE(LTYPEE), ISTAEV(LSTAEV), IELVAR(LELVAR)
      INTEGER INTVAR(LNTVAR), ISTADH(LSTADH), ISTEPA(LSTEPA)
      INTEGER ICALCF(LCALCF)
      DOUBLE PRECISION FUVALS(LFVALU), XVALUE(LXVALU), EPVALU(LEPVLU)
C
C  Problem name : ARTIF     
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , PAR   , FACT  , S     , DSDX  
      DOUBLE PRECISION D2SDX2, D     , THAT  
      INTRINSIC ATAN  , SIN   , COS   , MOD   
      IFSTAT = 0
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG == 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  Element type : TRIG      
C
       X      = XVALUE(IELVAR(ILSTRT+     1))
       PAR    = EPVALU(IPSTRT+     1)
       THAT   = 100.0                                    
       FACT   = MOD( PAR, THAT )                         
       S      = SIN( FACT * X )                          
       DSDX   = FACT * COS( FACT * X )                   
       D2SDX2 = - FACT * FACT * S                        
       D      = 1.0 + S * S                              
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= ATAN( S )                                
       ELSE
        FUVALS(IGSTRT+     1)= DSDX / D                                 
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=( D2SDX2 * D - 2.0 * S * DSDX**2 ) /D**2 
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
