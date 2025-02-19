      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : POLAK5  
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION XX1   , XX2   , XX    , S     , A     
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
C  ELEMENT TYPE : EL        
C
    1  CONTINUE
       XX1    = XVALUE(IELVAR(ILSTRT+     1))
       XX2    = XVALUE(IELVAR(ILSTRT+     2))
       S      = EPVALU(IPSTRT+     1)
       A      = XX1 - XX2 ** 4 + S                       
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= A * A                                    
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * A                                  
        FUVALS(IGSTRT+     2)= - 8.0 * A * XX2 ** 3                     
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
         FUVALS(IHSTRT+     2)=- 8.0 * XX2 ** 3                         
         FUVALS(IHSTRT+     3)=32.0 * XX2 ** 6 - 24.0 * A * XX2 ** 2    
        END IF
       END IF
       GO TO     3
C
C  ELEMENT TYPE : SQ        
C
    2  CONTINUE
       XX     = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= XX * XX                                  
       ELSE
        FUVALS(IGSTRT+     1)= XX + XX                                  
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
