      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : POLAK3  
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION XX    , A     , B     , V     , EV    
      INTRINSIC EXP   
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  ELEMENT TYPE : EL        
C
       XX     = XVALUE(IELVAR(ILSTRT+     1))
       A      = EPVALU(IPSTRT+     1)
       B      = EPVALU(IPSTRT+     2)
       V      = XX - SIN( A + B + B )                    
       EV     = EXP( V * V )                             
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= EV                                       
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * V * EV                             
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=( 4.0 * V * V  + 2.0 ) * EV              
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
