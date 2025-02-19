      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : BDVALUE 
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION V     , B     , VPLUSB
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  ELEMENT TYPE : WCUBE     
C
       V      = XVALUE(IELVAR(ILSTRT+     1))
       B      = EPVALU(IPSTRT+     1)
       VPLUSB = V + B                                    
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= VPLUSB**3                                
       ELSE
        FUVALS(IGSTRT+     1)= 3.0 * VPLUSB**2                          
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=6.0 * VPLUSB                             
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
