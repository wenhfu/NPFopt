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
C  Problem name : HS46      
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      INTEGER IPOW  
      DOUBLE PRECISION U1    , V1    , V2    , POW   , V1M1  
      DOUBLE PRECISION S     , V1SQ  , V1FR  , V2SQ  
      INTRINSIC SIN   , COS   
      IFSTAT = 0
      DO     6 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG == 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2,    3,    4,    5
     *                                                        ), IELTYP
C
C  Element type : ISQ       
C
    1  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       U1     =   V1    
     *          - V2    
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= U1 * U1                                  
       ELSE
        FUVALS(IGSTRT+     1)= U1 + U1                                  
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     6
C
C  Element type : SPW       
C
    2  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       POW    = EPVALU(IPSTRT+     1)
       V1M1   = V1 - 1.0                                 
       IPOW   = POW                                      
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= V1M1**IPOW                               
       ELSE
        FUVALS(IGSTRT+     1)= POW * V1M1**( IPOW - 1 )                 
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=POW * ( POW - 1.0 ) * V1M1**(IPOW - 2)   
        END IF
       END IF
       GO TO     6
C
C  Element type : V2W       
C
    3  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= V1 * V1 * V2                             
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * V1 * V2                            
        FUVALS(IGSTRT+     2)= V1 * V1                                  
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=V2 + V2                                  
         FUVALS(IHSTRT+     2)=V1 + V1                                  
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
       GO TO     6
C
C  Element type : ISIN      
C
    4  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       U1     =   V1    
     *          - V2    
       S      = SIN( U1 )                                
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= S                                        
       ELSE
        FUVALS(IGSTRT+     1)= COS( U1 )                                
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=-S                                       
        END IF
       END IF
       GO TO     6
C
C  Element type : V4W2      
C
    5  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       V1SQ   = V1 * V1                                  
       V1FR   = V1SQ * V1SQ                              
       V2SQ   = V2 * V2                                  
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= V1FR * V2SQ                              
       ELSE
        FUVALS(IGSTRT+     1)= 4.0 * V1 * V1SQ * V2SQ                   
        FUVALS(IGSTRT+     2)= 2.0 * V1FR * V2                          
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=12.0 * V1SQ * V2SQ                       
         FUVALS(IHSTRT+     2)=8.0 * V1 * V1SQ * V2                     
         FUVALS(IHSTRT+     3)=2.0 * V1FR                               
        END IF
       END IF
    6 CONTINUE
      RETURN
      END
