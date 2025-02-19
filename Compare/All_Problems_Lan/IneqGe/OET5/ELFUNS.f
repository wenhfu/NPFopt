      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : OET5    
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X1    , X2    , X3    , W     , W2    
      DOUBLE PRECISION TERM  
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  ELEMENT TYPE : QUAD      
C
       X1     = XVALUE(IELVAR(ILSTRT+     1))
       X2     = XVALUE(IELVAR(ILSTRT+     2))
       X3     = XVALUE(IELVAR(ILSTRT+     3))
       W      = EPVALU(IPSTRT+     1)
       W2     = W * W                                    
       TERM   = W2 * X1 + W * X2 + X3                    
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= TERM * TERM                              
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * TERM * W2                          
        FUVALS(IGSTRT+     2)= 2.0 * TERM * W                           
        FUVALS(IGSTRT+     3)= 2.0 * TERM                               
        IF ( IFFLAG .EQ. 3 ) THEN
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
