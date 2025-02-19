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
C  Problem name : HS26      
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION U1    , V1    , V2    , V1V1P1
      IFSTAT = 0
      DO     5 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG == 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2,    3,    4
     *                                                        ), IELTYP
C
C  Element type : SQ        
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
       GO TO     5
C
C  Element type : SQSQ      
C
    2  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       U1     =   V1    
     *          - V2    
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= U1**4                                    
       ELSE
        FUVALS(IGSTRT+     1)= 4.0 * U1**3                              
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=12.0 * U1 * U1                           
        END IF
       END IF
       GO TO     5
C
C  Element type : SQL       
C
    3  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       V1V1P1 = 1.0 + V1*V1                              
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= V1V1P1 * V2                              
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * V1 * V2                            
        FUVALS(IGSTRT+     2)= V1V1P1                                   
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=V2 + V2                                  
         FUVALS(IHSTRT+     2)=V1 + V1                                  
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
       GO TO     5
C
C  Element type : QRT       
C
    4  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= V1**4                                    
       ELSE
        FUVALS(IGSTRT+     1)= 4.0 * V1**3                              
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=12.0 * V1 * V1                           
        END IF
       END IF
    5 CONTINUE
      RETURN
      END
