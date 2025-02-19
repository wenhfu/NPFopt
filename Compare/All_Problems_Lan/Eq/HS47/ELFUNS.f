      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : HS47    
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION U     , V1    , V2    
      DO     7 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2,    3,    4,    5,    6
     *                                                        ), IELTYP
C
C  ELEMENT TYPE : DIFF2     
C
    1  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       U      =   V1    
     *          - V2    
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= U ** 2                                   
       ELSE
        FUVALS(IGSTRT+     1)= U + U                                    
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     7
C
C  ELEMENT TYPE : DIFF3     
C
    2  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       U      =   V1    
     *          - V2    
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= U ** 3                                   
       ELSE
        FUVALS(IGSTRT+     1)= 3.0 * U ** 2                             
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=6.0 * U                                  
        END IF
       END IF
       GO TO     7
C
C  ELEMENT TYPE : DIFF4     
C
    3  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       U      =   V1    
     *          - V2    
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= U ** 4                                   
       ELSE
        FUVALS(IGSTRT+     1)= 4.0 * U ** 3                             
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=12.0 * U ** 2                            
        END IF
       END IF
       GO TO     7
C
C  ELEMENT TYPE : PROD      
C
    4  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= V1*V2                                    
       ELSE
        FUVALS(IGSTRT+     1)= V2                                       
        FUVALS(IGSTRT+     2)= V1                                       
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=1.0                                      
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
       GO TO     7
C
C  ELEMENT TYPE : SQ        
C
    5  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= V1**2                                    
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * V1                                 
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     7
C
C  ELEMENT TYPE : CUBE      
C
    6  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= V1**3                                    
       ELSE
        FUVALS(IGSTRT+     1)= 3.0 * V1 * V1                            
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=6.0 * V1                                 
        END IF
       END IF
    7 CONTINUE
      RETURN
      END
