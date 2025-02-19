      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : POWELLBS
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION V1    , V2    , BIG   , BIGV1 , EXPMV 
      INTRINSIC EXP   
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
C  ELEMENT TYPE : PROD      
C
    1  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       BIG    = 10000.0                                  
       BIGV1  = BIG * V1                                 
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= BIGV1 * V2                               
       ELSE
        FUVALS(IGSTRT+     1)= BIG * V2                                 
        FUVALS(IGSTRT+     2)= BIGV1                                    
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=0.0                                      
         FUVALS(IHSTRT+     2)=BIG                                      
         FUVALS(IHSTRT+     3)=0.0                                      
        END IF
       END IF
       GO TO     3
C
C  ELEMENT TYPE : EXPN      
C
    2  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       EXPMV  = EXP( - V1 )                              
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= EXPMV                                    
       ELSE
        FUVALS(IGSTRT+     1)= - EXPMV                                  
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=EXPMV                                    
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
