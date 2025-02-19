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
C  Problem name : POLAK1    
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION XX1   , XX2   , S     , TX1   , DTX1  
      DOUBLE PRECISION TX2   , DTX2  , EARG  
      INTRINSIC EXP   
      IFSTAT = 0
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG == 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  Element type : EL        
C
       XX1    = XVALUE(IELVAR(ILSTRT+     1))
       XX2    = XVALUE(IELVAR(ILSTRT+     2))
       S      = EPVALU(IPSTRT+     1)
       TX1    = 0.001 * XX1 * XX1                        
       DTX1   = 0.002 * XX1                              
       TX2    = ( XX2 + S ) ** 2                         
       DTX2   = 2.0 * ( XX2 + S )                        
       EARG   = EXP( TX1 + TX2 )                         
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= EARG                                     
       ELSE
        FUVALS(IGSTRT+     1)= DTX1 * EARG                              
        FUVALS(IGSTRT+     2)= DTX2 * EARG                              
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=( 0.002 + DTX1 * DTX1 ) * EARG           
         FUVALS(IHSTRT+     2)=DTX1 * DTX2 * EARG                       
         FUVALS(IHSTRT+     3)=( 2.0 + DTX2 * DTX2 ) * EARG             
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
