      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : POLAK1  
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION XX1   , XX2   , S     , TX1   , DTX1  
      DOUBLE PRECISION TX2   , DTX2  , EARG  
      INTRINSIC EXP   
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  ELEMENT TYPE : EL        
C
       XX1    = XVALUE(IELVAR(ILSTRT+     1))
       XX2    = XVALUE(IELVAR(ILSTRT+     2))
       S      = EPVALU(IPSTRT+     1)
       TX1    = 0.001 * XX1 * XX1                        
       DTX1   = 0.002 * XX1                              
       TX2    = ( XX2 + S ) ** 2                         
       DTX2   = 2.0 * ( XX2 + S )                        
       EARG   = EXP( TX1 + TX2 )                         
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= EARG                                     
       ELSE
        FUVALS(IGSTRT+     1)= DTX1 * EARG                              
        FUVALS(IGSTRT+     2)= DTX2 * EARG                              
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=( 0.002 + DTX1 * DTX1 ) * EARG           
         FUVALS(IHSTRT+     2)=DTX1 * DTX2 * EARG                       
         FUVALS(IHSTRT+     3)=( 2.0 + DTX2 * DTX2 ) * EARG             
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
