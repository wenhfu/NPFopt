      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : GRIDNETE
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , Y     , A     , SQRA  , ASQRA 
      DOUBLE PRECISION ISQRA , B     , C     
      INTRINSIC SQRT  
      DO     3 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2
     *                                                        ), IELTYP
C
C  ELEMENT TYPE : SQ        
C
    1  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= X * X                                    
       ELSE
        FUVALS(IGSTRT+     1)= X + X                                    
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     3
C
C  ELEMENT TYPE : SQR       
C
    2  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       A      = 1.0 + 2.0 * X * ( X - Y) + Y * Y         
       SQRA   = SQRT( A )                                
       ASQRA  = A * SQRA                                 
       ISQRA  = 1.0 / SQRA                               
       B      = X + X - Y                                
       C      = Y - X                                    
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= SQRA                                     
       ELSE
        FUVALS(IGSTRT+     1)= B / SQRA                                 
        FUVALS(IGSTRT+     2)= C / SQRA                                 
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=- B * B / ASQRA + ISQRA + ISQRA          
         FUVALS(IHSTRT+     2)=- B * C / ASQRA - ISQRA                  
         FUVALS(IHSTRT+     3)=- C * C / ASQRA + ISQRA                  
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
