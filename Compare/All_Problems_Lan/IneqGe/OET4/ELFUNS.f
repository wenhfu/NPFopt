      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : OET4    
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X1    , X2    , X3    , W     , NUMER 
      DOUBLE PRECISION DENOM 
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  ELEMENT TYPE : RATIO     
C
       X1     = XVALUE(IELVAR(ILSTRT+     1))
       X2     = XVALUE(IELVAR(ILSTRT+     2))
       X3     = XVALUE(IELVAR(ILSTRT+     3))
       W      = EPVALU(IPSTRT+     1)
       NUMER  = X1 + X2 * W                              
       DENOM  = 1.0 + X3 * W                             
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= NUMER / DENOM                            
       ELSE
        FUVALS(IGSTRT+     1)= 1.0 / DENOM                              
        FUVALS(IGSTRT+     2)= W / DENOM                                
        FUVALS(IGSTRT+     3)= - NUMER * W / DENOM ** 2                 
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     4)=- W / DENOM ** 2                         
         FUVALS(IHSTRT+     5)=- W * W / DENOM ** 2                     
         FUVALS(IHSTRT+     6)=2.0 * NUMER * W * W / DENOM ** 3         
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     2)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
