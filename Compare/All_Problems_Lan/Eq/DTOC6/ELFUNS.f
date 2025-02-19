      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : DTOC6   
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION Z     , EZ    
      INTRINSIC EXP   
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  ELEMENT TYPE : EXP       
C
       Z      = XVALUE(IELVAR(ILSTRT+     1))
       EZ     = EXP( Z )                                 
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= EZ                                       
       ELSE
        FUVALS(IGSTRT+     1)= EZ                                       
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=EZ                                       
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
