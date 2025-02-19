      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : ORTHRDM2
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION DX    , DY    , ZZ    , X     , Y     
      DOUBLE PRECISION ZA    , ZB    , ZC    , T     , T1    
      DOUBLE PRECISION T1SQ  , ZZSQ  
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
C  ELEMENT TYPE : TA        
C
    1  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       ZA     = XVALUE(IELVAR(ILSTRT+     3))
       ZB     = XVALUE(IELVAR(ILSTRT+     4))
       DX     =   X     
     *          - ZA    
       DY     =   Y     
     *          - ZB    
       T      = DX * DX + DY * DY                        
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= T * T                                    
       ELSE
        FUVALS(IGSTRT+     1)= 4.0 * T * DX                             
        FUVALS(IGSTRT+     2)= 4.0 * T * DY                             
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=4.0 * ( T + 2.0 * DX * DX )              
         FUVALS(IHSTRT+     2)=8.0 * DX * DY                            
         FUVALS(IHSTRT+     3)=4.0 * ( T + 2.0 * DY * DY )              
        END IF
       END IF
       GO TO     3
C
C  ELEMENT TYPE : TB        
C
    2  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       ZA     = XVALUE(IELVAR(ILSTRT+     3))
       ZB     = XVALUE(IELVAR(ILSTRT+     4))
       ZC     = XVALUE(IELVAR(ILSTRT+     5))
       DX     =   X     
     *          - ZA    
       DY     =   Y     
     *          - ZB    
       ZZ     =   ZC    
       T      = DX * DX + DY * DY                        
       ZZSQ   = ZZ * ZZ                                  
       T1     = 1.0 + ZZSQ                               
       T1SQ   = T1 * T1                                  
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= T * T1SQ                                 
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * DX * T1SQ                          
        FUVALS(IGSTRT+     2)= 2.0 * DY * T1SQ                          
        FUVALS(IGSTRT+     3)= 4.0 * T * T1 * ZZ                        
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0 * T1SQ                               
         FUVALS(IHSTRT+     4)=8.0 * DX * T1 * ZZ                       
         FUVALS(IHSTRT+     3)=2.0 * T1SQ                               
         FUVALS(IHSTRT+     5)=8.0 * DY * T1 * ZZ                       
         FUVALS(IHSTRT+     6)=4.0 * T * ( 2.0 * ZZSQ + T1)             
         FUVALS(IHSTRT+     2)=0.0D+0
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
