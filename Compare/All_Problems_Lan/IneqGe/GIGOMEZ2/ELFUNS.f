      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : GIGOMEZ2
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , YMX   , Y     , E     
      INTRINSIC EXP   
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
       X      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= X * X                                    
       ELSE
        FUVALS(IGSTRT+     1)= X + X                                    
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     5
C
C  ELEMENT TYPE : FR        
C
    2  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= X**4                                     
       ELSE
        FUVALS(IGSTRT+     1)= 4.0 * X** 3                              
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=12.0 * X**2                              
        END IF
       END IF
       GO TO     5
C
C  ELEMENT TYPE : SSQ       
C
    3  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= ( 2.0 - X )**2                           
       ELSE
        FUVALS(IGSTRT+     1)= -4.0 + 2.0 * X                           
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     5
C
C  ELEMENT TYPE : IEXP      
C
    4  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       YMX    = - X     
     *          + Y     
       E      = EXP( YMX )                               
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= E                                        
       ELSE
        FUVALS(IGSTRT+     1)= E                                        
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=E                                        
        END IF
       END IF
    5 CONTINUE
      RETURN
      END
