      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : HS52    
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION U1    , V1    , V2    , V1M1  
      DO     4 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2,    3
     *                                                        ), IELTYP
C
C  ELEMENT TYPE : SQ1M2     
C
    1  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       U1     =   V1     *      4.00000
     *          - V2    
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= U1**2                                    
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * U1                                 
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     4
C
C  ELEMENT TYPE : SQ1P2     
C
    2  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       U1     =   V1    
     *          + V2    
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= (U1 - 2.0)**2                            
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * (U1 - 2.0)                         
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     4
C
C  ELEMENT TYPE : SQM1      
C
    3  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V1M1   = V1 - 1.0                                 
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= V1M1 **2                                 
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * V1M1                               
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
    4 CONTINUE
      RETURN
      END
