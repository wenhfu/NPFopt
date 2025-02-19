      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : HALDMADS
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION V1    , V2    , V3    , V4    , V5    
      DOUBLE PRECISION Y     , N     , D     , YY    , YYY   
      DOUBLE PRECISION DD    , DDD   
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  ELEMENT TYPE : HM        
C
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       V3     = XVALUE(IELVAR(ILSTRT+     3))
       V4     = XVALUE(IELVAR(ILSTRT+     4))
       V5     = XVALUE(IELVAR(ILSTRT+     5))
       Y      = EPVALU(IPSTRT+     1)
       YY     = Y * Y                                    
       YYY    = YY * Y                                   
       N      = V1 + Y * V2                              
       D      = 1.0 + V3 * Y + V4 * YY + V5 * YYY        
       DD     = D * D                                    
       DDD    = DD * D                                   
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= N / D                                    
       ELSE
        FUVALS(IGSTRT+     1)= 1.0 / D                                  
        FUVALS(IGSTRT+     2)= Y / D                                    
        FUVALS(IGSTRT+     3)= - N * Y / DD                             
        FUVALS(IGSTRT+     4)= - N * YY / DD                            
        FUVALS(IGSTRT+     5)= - N * YYY / DD                           
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     4)=- Y / DD                                 
         FUVALS(IHSTRT+     7)=- YY / DD                                
         FUVALS(IHSTRT+    11)=- YYY / DD                               
         FUVALS(IHSTRT+     5)=- YY / DD                                
         FUVALS(IHSTRT+     8)=- YYY / DD                               
         FUVALS(IHSTRT+    12)=- YY * YY / DD                           
         FUVALS(IHSTRT+     6)=2.0 * N * YY / DDD                       
         FUVALS(IHSTRT+     9)=2.0 * N * YYY / DDD                      
         FUVALS(IHSTRT+    13)=2.0 * N * YY * YY / DDD                  
         FUVALS(IHSTRT+    10)=2.0 * N * YY * YY / DDD                  
         FUVALS(IHSTRT+    14)=2.0 * N * YY * YYY / DDD                 
         FUVALS(IHSTRT+    15)=2.0 * N * YYY * YYY / DDD                
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     2)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
