      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : BROYDN3D
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION V1    , K1    , TEMP1 , TEMP2 
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  ELEMENT TYPE : BROY      
C
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       K1     = EPVALU(IPSTRT+     1)
       TEMP1  = - K1 * V1                                
       TEMP2  = 3.0 + TEMP1                              
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= TEMP2 * V1                               
       ELSE
        FUVALS(IGSTRT+     1)= TEMP2 + TEMP1                            
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=- K1 - K1                                
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
