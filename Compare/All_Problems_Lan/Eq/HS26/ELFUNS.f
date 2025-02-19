      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : HS26    
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION U1    , V1    , V2    , V1V1P1
      DO     5 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2,    3,    4
     *                                                        ), IELTYP
C
C  ELEMENT TYPE : SQ        
C
    1  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       U1     =   V1    
     *          - V2    
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= U1 * U1                                  
       ELSE
        FUVALS(IGSTRT+     1)= U1 + U1                                  
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     5
C
C  ELEMENT TYPE : SQSQ      
C
    2  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       U1     =   V1    
     *          - V2    
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= U1**4                                    
       ELSE
        FUVALS(IGSTRT+     1)= 4.0 * U1**3                              
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=12.0 * U1 * U1                           
        END IF
       END IF
       GO TO     5
C
C  ELEMENT TYPE : SQL       
C
    3  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       V1V1P1 = 1.0 + V1*V1                              
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= V1V1P1 * V2                              
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * V1 * V2                            
        FUVALS(IGSTRT+     2)= V1V1P1                                   
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=V2 + V2                                  
         FUVALS(IHSTRT+     2)=V1 + V1                                  
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
       GO TO     5
C
C  ELEMENT TYPE : QRT       
C
    4  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= V1**4                                    
       ELSE
        FUVALS(IGSTRT+     1)= 4.0 * V1**3                              
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=12.0 * V1 * V1                           
        END IF
       END IF
    5 CONTINUE
      RETURN
      END
