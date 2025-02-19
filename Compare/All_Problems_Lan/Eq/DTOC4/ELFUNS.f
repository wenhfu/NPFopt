      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : DTOC4   
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION Z     , A     , B     
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
       Z      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= Z * Z                                    
       ELSE
        FUVALS(IGSTRT+     1)= Z + Z                                    
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     3
C
C  ELEMENT TYPE : AAB       
C
    2  CONTINUE
       A      = XVALUE(IELVAR(ILSTRT+     1))
       B      = XVALUE(IELVAR(ILSTRT+     2))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= A * A * B                                
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * A * B                              
        FUVALS(IGSTRT+     2)= A * A                                    
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=B + B                                    
         FUVALS(IHSTRT+     2)=A + A                                    
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
