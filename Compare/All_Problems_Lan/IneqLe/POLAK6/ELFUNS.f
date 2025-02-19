      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : POLAK6  
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION XX    , YY    , ZZ    , A     , B     
      DOUBLE PRECISION C     , DCDY  , DADY  , DADZ  , DCDZ  
      DOUBLE PRECISION D2CDZZ, D2ADYY, D2ADYZ, D2ADZZ
      DO     6 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2,    3,    4,    5
     *                                                        ), IELTYP
C
C  ELEMENT TYPE : EL42      
C
    2  CONTINUE
       XX     = XVALUE(IELVAR(ILSTRT+     1))
       YY     = XVALUE(IELVAR(ILSTRT+     2))
       B      = YY + 1.0                                 
       A      = XX - B ** 4                              
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= A * A                                    
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * A                                  
        FUVALS(IGSTRT+     2)= - 8.0 * A * B ** 3                       
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
         FUVALS(IHSTRT+     2)=- 8.0 * B ** 3                           
         FUVALS(IHSTRT+     3)=32.0 * B ** 6 - 24.0 * A * B ** 2        
        END IF
       END IF
       GO TO     6
C
C  ELEMENT TYPE : EL442     
C
    3  CONTINUE
       XX     = XVALUE(IELVAR(ILSTRT+     1))
       YY     = XVALUE(IELVAR(ILSTRT+     2))
       ZZ     = XVALUE(IELVAR(ILSTRT+     3))
       B      = ZZ + 1.0                                 
       C      = YY - B ** 4                              
       DCDZ   = - 4.0 * B ** 3                           
       D2CDZZ = - 12.0 * B ** 2                          
       A      = XX - C ** 4                              
       DADY   = - 4.0 * C ** 3                           
       DADZ   = DADY * DCDZ                              
       D2ADYY = - 12.0 * C ** 2                          
       D2ADYZ = D2ADYY * DCDZ                            
       D2ADZZ = D2ADYZ * DCDZ + DADY * D2CDZZ            
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= A * A                                    
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * A                                  
        FUVALS(IGSTRT+     2)= 2.0 * A * DADY                           
        FUVALS(IGSTRT+     3)= 2.0 * A * DADZ                           
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
         FUVALS(IHSTRT+     2)=2.0 * DADY                               
         FUVALS(IHSTRT+     4)=2.0 * DADZ                               
         FUVALS(IHSTRT+     3)=2.0 * ( DADY * DADY + A * D2ADYY )       
         FUVALS(IHSTRT+     5)=2.0 * ( DADZ * DADY + A * D2ADYZ )       
         FUVALS(IHSTRT+     6)=2.0 * ( DADZ * DADZ + A * D2ADZZ )       
        END IF
       END IF
       GO TO     6
C
C  ELEMENT TYPE : EL4       
C
    4  CONTINUE
       XX     = XVALUE(IELVAR(ILSTRT+     1))
       B      = XX + 1.0                                 
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= B ** 4                                   
       ELSE
        FUVALS(IGSTRT+     1)= 4.0 * B ** 3                             
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=12.0 * B ** 2                            
        END IF
       END IF
       GO TO     6
C
C  ELEMENT TYPE : EL44      
C
    5  CONTINUE
       XX     = XVALUE(IELVAR(ILSTRT+     1))
       YY     = XVALUE(IELVAR(ILSTRT+     2))
       B      = YY + 1.0                                 
       A      = XX - B ** 4                              
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= A ** 4                                   
       ELSE
        FUVALS(IGSTRT+     1)= 4.0 * A ** 3                             
        FUVALS(IGSTRT+     2)= - 16.0 * ( A * B ) ** 3                  
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=12.0 * A ** 2                            
         FUVALS(IHSTRT+     2)=- 48.0 * A ** 2 * B ** 3                 
         FUVALS(IHSTRT+     3)=- 48.0 * ( A * B ) ** 2 *                
     *                          ( A - 4.0 * B ** 4 )                    
        END IF
       END IF
       GO TO     6
C
C  ELEMENT TYPE : SQ        
C
    1  CONTINUE
       XX     = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= XX * XX                                  
       ELSE
        FUVALS(IGSTRT+     1)= XX + XX                                  
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
    6 CONTINUE
      RETURN
      END
