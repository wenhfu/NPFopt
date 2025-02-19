      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : YATP2SQ 
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION XX    , YPZ   , X     , Y     , Z     
      DOUBLE PRECISION C     , S     
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
C  ELEMENT TYPE : ATP2      
C
    1  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       Z      = XVALUE(IELVAR(ILSTRT+     3))
       XX     =   X     
       YPZ    =   Y     
     *          + Z     
       C      = COS( XX )                                
       S      = SIN( XX )                                
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= YPZ * C                                  
       ELSE
        FUVALS(IGSTRT+     2)= C                                        
        FUVALS(IGSTRT+     1)= - YPZ * S                                
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=- S                                      
         FUVALS(IHSTRT+     1)=- YPZ * C                                
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
       GO TO     3
C
C  ELEMENT TYPE : SINX      
C
    2  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       S      = SIN( X )                                 
       C      = COS( X )                                 
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= S                                        
       ELSE
        FUVALS(IGSTRT+     1)= C                                        
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=- S                                      
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
