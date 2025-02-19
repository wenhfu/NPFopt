      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : OET6    
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , Y     , W     , EYW   
      INTRINSIC EXP   
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  ELEMENT TYPE : XEYW      
C
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       W      = EPVALU(IPSTRT+     1)
       EYW    = EXP( Y * W )                             
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= X * EYW                                  
       ELSE
        FUVALS(IGSTRT+     1)= EYW                                      
        FUVALS(IGSTRT+     2)= X * W * EYW                              
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=W * EYW                                  
         FUVALS(IHSTRT+     3)=X * W * W * EYW                          
         FUVALS(IHSTRT+     1)=0.0D+0
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
