      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : PENTAGON
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION DX    , DY    , XA    , YA    , XB    
      DOUBLE PRECISION YB    , D     , D9    , D10   
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  ELEMENT TYPE : IDIST     
C
       XA     = XVALUE(IELVAR(ILSTRT+     1))
       YA     = XVALUE(IELVAR(ILSTRT+     2))
       XB     = XVALUE(IELVAR(ILSTRT+     3))
       YB     = XVALUE(IELVAR(ILSTRT+     4))
       DX     =   XA    
     *          - XB    
       DY     =   YA    
     *          - YB    
       D      = DX * DX + DY * DY                        
       D9     = D**9                                     
       D10    = D9 * D                                   
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= 1.0 / D**8                               
       ELSE
        FUVALS(IGSTRT+     1)= -16.0 * DX / D9                          
        FUVALS(IGSTRT+     2)= -16.0 * DY / D9                          
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=16.0 * ( 18.0 * DX * DX - D ) / D10      
         FUVALS(IHSTRT+     2)=288.0 * DX * DY / D10                    
         FUVALS(IHSTRT+     3)=16.0 * ( 18.0 * DY * DY - D ) / D10      
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
