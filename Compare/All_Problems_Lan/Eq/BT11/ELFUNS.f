      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : BT11    
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , Z     , Y     , P     , T     
      DO     6 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2,    3,    4,    5
     *                                                        ), IELTYP
C
C  ELEMENT TYPE : SQ        
C
    1  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= X * X                                    
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * X                                  
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     6
C
C  ELEMENT TYPE : CB        
C
    3  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= X * X * X                                
       ELSE
        FUVALS(IGSTRT+     1)= 3.0 * X * X                              
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=6.0 * X                                  
        END IF
       END IF
       GO TO     6
C
C  ELEMENT TYPE : SSQ       
C
    2  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       P      = EPVALU(IPSTRT+     1)
       T      = X - P                                    
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= T * T                                    
       ELSE
        FUVALS(IGSTRT+     1)= T + T                                    
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     6
C
C  ELEMENT TYPE : ISQ       
C
    4  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       Z      =   X     
     *          - Y     
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= Z * Z                                    
       ELSE
        FUVALS(IGSTRT+     1)= Z + Z                                    
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     6
C
C  ELEMENT TYPE : IFR       
C
    5  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       Z      =   X     
     *          - Y     
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= Z**4                                     
       ELSE
        FUVALS(IGSTRT+     1)= 4.0 * Z**3                               
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=12.0 * Z**2                              
        END IF
       END IF
    6 CONTINUE
      RETURN
      END
