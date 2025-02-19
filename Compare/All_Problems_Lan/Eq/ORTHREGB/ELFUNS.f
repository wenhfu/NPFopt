      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : ORTHREGB
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION H     , X     , Y     , G     
      DO     4 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2,    3
     *                                                        ), IELTYP
C
C  ELEMENT TYPE : HXX       
C
    1  CONTINUE
       H      = XVALUE(IELVAR(ILSTRT+     1))
       X      = XVALUE(IELVAR(ILSTRT+     2))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= H * X * X                                
       ELSE
        FUVALS(IGSTRT+     1)= X * X                                    
        FUVALS(IGSTRT+     2)= 2.0 * H * X                              
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=X + X                                    
         FUVALS(IHSTRT+     3)=H + H                                    
         FUVALS(IHSTRT+     1)=0.0D+0
        END IF
       END IF
       GO TO     4
C
C  ELEMENT TYPE : HXY       
C
    2  CONTINUE
       H      = XVALUE(IELVAR(ILSTRT+     1))
       X      = XVALUE(IELVAR(ILSTRT+     2))
       Y      = XVALUE(IELVAR(ILSTRT+     3))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= H * X * Y                                
       ELSE
        FUVALS(IGSTRT+     1)= X * Y                                    
        FUVALS(IGSTRT+     2)= H * Y                                    
        FUVALS(IGSTRT+     3)= H * X                                    
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=Y                                        
         FUVALS(IHSTRT+     4)=X                                        
         FUVALS(IHSTRT+     5)=H                                        
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
         FUVALS(IHSTRT+     6)=0.0D+0
        END IF
       END IF
       GO TO     4
C
C  ELEMENT TYPE : GX        
C
    3  CONTINUE
       G      = XVALUE(IELVAR(ILSTRT+     1))
       X      = XVALUE(IELVAR(ILSTRT+     2))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= G * X                                    
       ELSE
        FUVALS(IGSTRT+     1)= X                                        
        FUVALS(IGSTRT+     2)= G                                        
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=1.0                                      
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
    4 CONTINUE
      RETURN
      END
