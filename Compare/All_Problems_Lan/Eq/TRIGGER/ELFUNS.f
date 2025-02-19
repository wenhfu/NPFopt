      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : TRIGGER 
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , U     , XX1   , XX3   , EXA   
      DOUBLE PRECISION B1    , B2    , BSQ   , BU2   
      INTRINSIC EXP   , ATAN  
      B1     = 5.6D-8                                   
      B2     = 1962.0                                   
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
C  ELEMENT TYPE : DIODE     
C
    1  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       EXA    = B1 * EXP( 25.0 * X )                     
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= EXA - B1                                 
       ELSE
        FUVALS(IGSTRT+     1)= 25.0 * EXA                               
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=625.0 * EXA                              
        END IF
       END IF
       GO TO     3
C
C  ELEMENT TYPE : OPAMP     
C
    2  CONTINUE
       XX1    = XVALUE(IELVAR(ILSTRT+     1))
       XX3    = XVALUE(IELVAR(ILSTRT+     2))
       U      = - XX1   
     *          + XX3   
       BSQ    = B2 * B2                                  
       BU2    = 1.0 + BSQ * U * U                        
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= 7.65 * ATAN( B2 * U )                    
       ELSE
        FUVALS(IGSTRT+     1)= 7.65 * B2 / BU2                          
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=-15.3 * BSQ * B2 * U / ( BU2 * BU2 )     
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
