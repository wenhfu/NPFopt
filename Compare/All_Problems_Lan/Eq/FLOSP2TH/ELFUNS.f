      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : FLOSP2TH
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION PSIDIF, PHIDIF, PSIM  , PSIP  , PHIM  
      DOUBLE PRECISION PHIP  
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  ELEMENT TYPE : PROD      
C
       PSIM   = XVALUE(IELVAR(ILSTRT+     1))
       PSIP   = XVALUE(IELVAR(ILSTRT+     2))
       PHIM   = XVALUE(IELVAR(ILSTRT+     3))
       PHIP   = XVALUE(IELVAR(ILSTRT+     4))
       PSIDIF = - PSIM  
     *          + PSIP  
       PHIDIF = - PHIM  
     *          + PHIP  
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= PSIDIF * PHIDIF                          
       ELSE
        FUVALS(IGSTRT+     1)= PHIDIF                                   
        FUVALS(IGSTRT+     2)= PSIDIF                                   
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=1.0                                      
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
