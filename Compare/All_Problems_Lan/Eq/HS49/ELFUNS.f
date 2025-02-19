      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : HS49    
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION U     , V     , V1    , V2    , VM1   
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
C  ELEMENT TYPE : SQ1M2     
C
    1  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       U      =   V1    
     *          - V2    
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= U ** 2                                   
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * U                                  
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     5
C
C  ELEMENT TYPE : VM12      
C
    2  CONTINUE
       V      = XVALUE(IELVAR(ILSTRT+     1))
       VM1    = V - 1.0                                  
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= VM1 ** 2                                 
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * VM1                                
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     5
C
C  ELEMENT TYPE : VM14      
C
    3  CONTINUE
       V      = XVALUE(IELVAR(ILSTRT+     1))
       VM1    = V - 1.0                                  
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= VM1  ** 4                                
       ELSE
        FUVALS(IGSTRT+     1)= 4.0 * VM1 ** 3                           
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=12.0 * VM1 ** 2                          
        END IF
       END IF
       GO TO     5
C
C  ELEMENT TYPE : VM16      
C
    4  CONTINUE
       V      = XVALUE(IELVAR(ILSTRT+     1))
       VM1    = V - 1.0                                  
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= VM1  ** 6                                
       ELSE
        FUVALS(IGSTRT+     1)= 6.0 * VM1 ** 5                           
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=30.0 * VM1 ** 4                          
        END IF
       END IF
    5 CONTINUE
      RETURN
      END
