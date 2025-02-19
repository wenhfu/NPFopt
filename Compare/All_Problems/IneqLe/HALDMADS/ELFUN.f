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
C  Problem name : HALDMADS  
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION V1    , V2    , V3    , V4    , V5    
      DOUBLE PRECISION Y     , N     , D     , YY    , YYY   
      DOUBLE PRECISION DD    , DDD   
      IFSTAT = 0
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG == 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  Element type : HM        
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
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= N / D                                    
       ELSE
        FUVALS(IGSTRT+     1)= 1.0 / D                                  
        FUVALS(IGSTRT+     2)= Y / D                                    
        FUVALS(IGSTRT+     3)= - N * Y / DD                             
        FUVALS(IGSTRT+     4)= - N * YY / DD                            
        FUVALS(IGSTRT+     5)= - N * YYY / DD                           
        IF ( IFFLAG == 3 ) THEN
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
