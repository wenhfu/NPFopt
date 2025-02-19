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
C  Problem name : OET5      
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X1    , X2    , X3    , W     , W2    
      DOUBLE PRECISION TERM  
      IFSTAT = 0
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG == 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  Element type : QUAD      
C
       X1     = XVALUE(IELVAR(ILSTRT+     1))
       X2     = XVALUE(IELVAR(ILSTRT+     2))
       X3     = XVALUE(IELVAR(ILSTRT+     3))
       W      = EPVALU(IPSTRT+     1)
       W2     = W * W                                    
       TERM   = W2 * X1 + W * X2 + X3                    
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= TERM * TERM                              
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * TERM * W2                          
        FUVALS(IGSTRT+     2)= 2.0 * TERM * W                           
        FUVALS(IGSTRT+     3)= 2.0 * TERM                               
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0 * W2 * W2                            
         FUVALS(IHSTRT+     2)=2.0 * W2 * W                             
         FUVALS(IHSTRT+     4)=2.0 * W2                                 
         FUVALS(IHSTRT+     3)=2.0 * W2                                 
         FUVALS(IHSTRT+     5)=2.0 * W                                  
         FUVALS(IHSTRT+     6)=2.0                                      
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
