      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : ARGTRIG 
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION XJ    , XI    , SX    , CX    
      INTRINSIC SIN   , COS   
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
C  ELEMENT TYPE : COSINE    
C
    1  CONTINUE
       XJ     = XVALUE(IELVAR(ILSTRT+     1))
       CX     = COS( XJ )                                
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= CX                                       
       ELSE
        FUVALS(IGSTRT+     1)= - SIN( XJ )                              
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=- CX                                     
        END IF
       END IF
       GO TO     3
C
C  ELEMENT TYPE : SINCOS    
C
    2  CONTINUE
       XI     = XVALUE(IELVAR(ILSTRT+     1))
       CX     = COS( XI )                                
       SX     = SIN( XI )                                
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= CX + SX                                  
       ELSE
        FUVALS(IGSTRT+     1)= - SX + CX                                
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=- CX - SX                                
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
