      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : HS27    
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION V1    , V2    , WMV1  , TM1SQ 
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
C  ELEMENT TYPE : SQSQ      
C
    2  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       TM1SQ  = V2 - V1 * V1                             
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= TM1SQ * TM1SQ                            
       ELSE
        FUVALS(IGSTRT+     1)= -4.0 * TM1SQ * V1                        
        FUVALS(IGSTRT+     2)= 2.0 * TM1SQ                              
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=12.0 * V1 * V1 - 4.0 * V2                
         FUVALS(IHSTRT+     2)=-4.0 * V1                                
         FUVALS(IHSTRT+     3)=2.0                                      
        END IF
       END IF
       GO TO     4
C
C  ELEMENT TYPE : SQ-1      
C
    3  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       WMV1   = 1.0 - V1                                 
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= WMV1 * WMV1                              
       ELSE
        FUVALS(IGSTRT+     1)= -2.0 * WMV1                              
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     4
C
C  ELEMENT TYPE : SQ        
C
    1  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= V1 * V1                                  
       ELSE
        FUVALS(IGSTRT+     1)= V1 + V1                                  
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
    4 CONTINUE
      RETURN
      END
