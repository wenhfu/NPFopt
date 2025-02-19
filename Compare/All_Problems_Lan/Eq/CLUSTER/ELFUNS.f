      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : CLUSTER 
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , Y     , SX    , SY    , CX    
      DOUBLE PRECISION CY    , F1    , F2    , DF1DY , DF2DY 
      DOUBLE PRECISION DF2DX 
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
C  ELEMENT TYPE : TA        
C
    1  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       SY     = SIN( Y )                                 
       CY     = COS( Y )                                 
       F1     = X - Y * Y                                
       F2     = X - SY                                   
       DF1DY  = -2.0 * Y                                 
       DF2DY  = - CY                                     
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= F1 * F2                                  
       ELSE
        FUVALS(IGSTRT+     1)= F2 + F1                                  
        FUVALS(IGSTRT+     2)= DF1DY * F2 + F1 * DF2DY                  
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
         FUVALS(IHSTRT+     2)=DF1DY + DF2DY                            
         FUVALS(IHSTRT+     3)=2.0 * ( DF1DY * DF2DY -  F2 )            
     *                         + F1 * SY                                
        END IF
       END IF
       GO TO     3
C
C  ELEMENT TYPE : TB        
C
    2  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       SY     = SIN( Y )                                 
       CY     = COS( Y )                                 
       CX     = COS( X )                                 
       SX     = SIN( X )                                 
       F1     = CY - X                                   
       F2     = Y - CX                                   
       DF1DY  = -SY                                      
       DF2DX  = SX                                       
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= F1 * F2                                  
       ELSE
        FUVALS(IGSTRT+     1)= F1 * DF2DX - F2                          
        FUVALS(IGSTRT+     2)= F1 + F2 * DF1DY                          
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=- DF2DX + F1 * CX - DF2DX                
         FUVALS(IHSTRT+     2)=- SY * DF2DX - 1.0                       
         FUVALS(IHSTRT+     3)=2.0 * DF1DY - F2 * CY                    
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
