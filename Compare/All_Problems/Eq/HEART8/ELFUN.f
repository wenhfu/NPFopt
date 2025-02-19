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
C  Problem name : HEART8    
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , Y     , Z     , ALPHA , ZERO  
      DOUBLE PRECISION TWO   , THREE , SIX   , DIFF  , DFSQ  
      DOUBLE PRECISION TWOD  , TWOX  
      IFSTAT = 0
      DO     9 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG == 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2,    3,    4,    5,    6,    7,    8
     *                                                        ), IELTYP
C
C  Element type : 2PROD     
C
    1  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       ZERO   = 0.0                                      
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= X*Y                                      
       ELSE
        FUVALS(IGSTRT+     1)= Y                                        
        FUVALS(IGSTRT+     2)= X                                        
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=ZERO                                     
         FUVALS(IHSTRT+     3)=ZERO                                     
         FUVALS(IHSTRT+     2)=1.0                                      
        END IF
       END IF
       GO TO     9
C
C  Element type : 3PROD     
C
    2  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       Z      = XVALUE(IELVAR(ILSTRT+     3))
       ZERO   = 0.0                                      
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= X*Y*Z                                    
       ELSE
        FUVALS(IGSTRT+     1)= Y*Z                                      
        FUVALS(IGSTRT+     2)= X*Z                                      
        FUVALS(IGSTRT+     3)= X*Y                                      
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=ZERO                                     
         FUVALS(IHSTRT+     3)=ZERO                                     
         FUVALS(IHSTRT+     6)=ZERO                                     
         FUVALS(IHSTRT+     2)=Z                                        
         FUVALS(IHSTRT+     4)=Y                                        
         FUVALS(IHSTRT+     5)=X                                        
        END IF
       END IF
       GO TO     9
C
C  Element type : VPV       
C
    3  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       ALPHA  = EPVALU(IPSTRT+     1)
       ZERO   = 0.0                                      
       DIFF   = ALPHA - Y                                
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= X*DIFF                                   
       ELSE
        FUVALS(IGSTRT+     1)= DIFF                                     
        FUVALS(IGSTRT+     2)= -X                                       
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=ZERO                                     
         FUVALS(IHSTRT+     3)=ZERO                                     
         FUVALS(IHSTRT+     2)=-1.0                                     
        END IF
       END IF
       GO TO     9
C
C  Element type : ADFSQ     
C
    4  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       Z      = XVALUE(IELVAR(ILSTRT+     3))
       ZERO   = 0.0                                      
       TWO    = 2.0                                      
       DFSQ   = Y**2 - Z**2                              
       TWOX   = TWO*X                                    
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= X*DFSQ                                   
       ELSE
        FUVALS(IGSTRT+     1)= DFSQ                                     
        FUVALS(IGSTRT+     2)= TWOX*Y                                   
        FUVALS(IGSTRT+     3)= -TWOX*Z                                  
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=ZERO                                     
         FUVALS(IHSTRT+     3)=TWOX                                     
         FUVALS(IHSTRT+     6)=-TWOX                                    
         FUVALS(IHSTRT+     2)=TWO*Y                                    
         FUVALS(IHSTRT+     4)=-TWO*Z                                   
         FUVALS(IHSTRT+     5)=ZERO                                     
        END IF
       END IF
       GO TO     9
C
C  Element type : PDFSQ     
C
    5  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       Z      = XVALUE(IELVAR(ILSTRT+     3))
       ALPHA  = EPVALU(IPSTRT+     1)
       ZERO   = 0.0                                      
       TWO    = 2.0                                      
       DIFF   = ALPHA - X                                
       DFSQ   = Y**2 - Z**2                              
       TWOX   = TWO*X                                    
       TWOD   = TWO*DIFF                                 
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= DIFF*DFSQ                                
       ELSE
        FUVALS(IGSTRT+     1)= -DFSQ                                    
        FUVALS(IGSTRT+     2)= TWOD*Y                                   
        FUVALS(IGSTRT+     3)= -TWOD*Z                                  
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=ZERO                                     
         FUVALS(IHSTRT+     3)=TWOD                                     
         FUVALS(IHSTRT+     6)=-TWOD                                    
         FUVALS(IHSTRT+     2)=-TWO*Y                                   
         FUVALS(IHSTRT+     4)=TWO*Z                                    
         FUVALS(IHSTRT+     5)=ZERO                                     
        END IF
       END IF
       GO TO     9
C
C  Element type : P3PRD     
C
    6  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       Z      = XVALUE(IELVAR(ILSTRT+     3))
       ALPHA  = EPVALU(IPSTRT+     1)
       ZERO   = 0.0                                      
       TWO    = 2.0                                      
       DIFF   = ALPHA - X                                
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= DIFF*Y*Z                                 
       ELSE
        FUVALS(IGSTRT+     1)= -Y*Z                                     
        FUVALS(IGSTRT+     2)= DIFF*Z                                   
        FUVALS(IGSTRT+     3)= DIFF*Y                                   
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=ZERO                                     
         FUVALS(IHSTRT+     3)=ZERO                                     
         FUVALS(IHSTRT+     6)=ZERO                                     
         FUVALS(IHSTRT+     2)=-Z                                       
         FUVALS(IHSTRT+     4)=-Y                                       
         FUVALS(IHSTRT+     5)=DIFF                                     
        END IF
       END IF
       GO TO     9
C
C  Element type : 3DPRD     
C
    7  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       Z      = XVALUE(IELVAR(ILSTRT+     3))
       ZERO   = 0.0                                      
       TWO    = 2.0                                      
       THREE  = 3.0                                      
       SIX    = 6.0                                      
       DIFF   = Y**2 - THREE*Z**2                        
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= X*Y*DIFF                                 
       ELSE
        FUVALS(IGSTRT+     1)= Y*DIFF                                   
        FUVALS(IGSTRT+     2)= X*DIFF + TWO*X*Y**2                      
        FUVALS(IGSTRT+     3)= -SIX*X*Y*Z                               
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=ZERO                                     
         FUVALS(IHSTRT+     3)=SIX*X*Y                                  
         FUVALS(IHSTRT+     6)=-SIX*X*Y                                 
         FUVALS(IHSTRT+     2)=DIFF + TWO*Y**2                          
         FUVALS(IHSTRT+     4)=-SIX*Y*Z                                 
         FUVALS(IHSTRT+     5)=-SIX*X*Z                                 
        END IF
       END IF
       GO TO     9
C
C  Element type : D3PRD     
C
    8  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       Z      = XVALUE(IELVAR(ILSTRT+     3))
       ALPHA  = EPVALU(IPSTRT+     1)
       ZERO   = 0.0                                      
       TWO    = 2.0                                      
       THREE  = 3.0                                      
       SIX    = 6.0                                      
       DFSQ   = Y**2 - THREE*Z**2                        
       DIFF   = ALPHA - x                                
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= DIFF*Y*DFSQ                              
       ELSE
        FUVALS(IGSTRT+     1)= -Y*DFSQ                                  
        FUVALS(IGSTRT+     2)= DIFF*(DFSQ + TWO*Y**2)                   
        FUVALS(IGSTRT+     3)= -SIX*Y*Z*DIFF                            
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=ZERO                                     
         FUVALS(IHSTRT+     3)=SIX*Y*DIFF                               
         FUVALS(IHSTRT+     6)=-SIX*Y*DIFF                              
         FUVALS(IHSTRT+     2)=-DFSQ - TWO*Y**2                         
         FUVALS(IHSTRT+     4)=SIX*Y*Z                                  
         FUVALS(IHSTRT+     5)=-SIX*DIFF*Z                              
        END IF
       END IF
    9 CONTINUE
      RETURN
      END
