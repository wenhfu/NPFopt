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
C  Problem name : HS47      
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION U     , V1    , V2    
      IFSTAT = 0
      DO     7 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG == 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2,    3,    4,    5,    6
     *                                                        ), IELTYP
C
C  Element type : DIFF2     
C
    1  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       U      =   V1    
     *          - V2    
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= U ** 2                                   
       ELSE
        FUVALS(IGSTRT+     1)= U + U                                    
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     7
C
C  Element type : DIFF3     
C
    2  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       U      =   V1    
     *          - V2    
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= U ** 3                                   
       ELSE
        FUVALS(IGSTRT+     1)= 3.0 * U ** 2                             
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=6.0 * U                                  
        END IF
       END IF
       GO TO     7
C
C  Element type : DIFF4     
C
    3  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       U      =   V1    
     *          - V2    
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= U ** 4                                   
       ELSE
        FUVALS(IGSTRT+     1)= 4.0 * U ** 3                             
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=12.0 * U ** 2                            
        END IF
       END IF
       GO TO     7
C
C  Element type : PROD      
C
    4  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= V1*V2                                    
       ELSE
        FUVALS(IGSTRT+     1)= V2                                       
        FUVALS(IGSTRT+     2)= V1                                       
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     2)=1.0                                      
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
       GO TO     7
C
C  Element type : SQ        
C
    5  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= V1**2                                    
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * V1                                 
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     7
C
C  Element type : CUBE      
C
    6  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= V1**3                                    
       ELSE
        FUVALS(IGSTRT+     1)= 3.0 * V1 * V1                            
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=6.0 * V1                                 
        END IF
       END IF
    7 CONTINUE
      RETURN
      END
