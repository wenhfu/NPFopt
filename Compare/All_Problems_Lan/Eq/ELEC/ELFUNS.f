      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : ELEC    
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION DIFFX , DIFFY , DIFFZ , X     , XI    
      DOUBLE PRECISION XJ    , YI    , YJ    , ZI    , ZJ    
      DOUBLE PRECISION SS    , ROOTSS
      INTRINSIC SQRT  
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
C  ELEMENT TYPE : PE        
C
    1  CONTINUE
       XI     = XVALUE(IELVAR(ILSTRT+     1))
       XJ     = XVALUE(IELVAR(ILSTRT+     2))
       YI     = XVALUE(IELVAR(ILSTRT+     3))
       YJ     = XVALUE(IELVAR(ILSTRT+     4))
       ZI     = XVALUE(IELVAR(ILSTRT+     5))
       ZJ     = XVALUE(IELVAR(ILSTRT+     6))
       DIFFX  =   XI    
     *          - XJ    
       DIFFY  =   YI    
     *          - YJ    
       DIFFZ  =   ZI    
     *          - ZJ    
       SS     = DIFFX ** 2 + DIFFY ** 2 + DIFFZ ** 2     
       ROOTSS = SQRT( SS )                               
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= 1.0 / ROOTSS                             
       ELSE
        FUVALS(IGSTRT+     1)= - DIFFX / ROOTSS ** 3                    
        FUVALS(IGSTRT+     2)= - DIFFY / ROOTSS ** 3                    
        FUVALS(IGSTRT+     3)= - DIFFZ / ROOTSS ** 3                    
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=3.0 * DIFFX ** 2 / ROOTSS ** 5           
     *                         - 1.0 / ROOTSS ** 3                      
         FUVALS(IHSTRT+     2)=3.0 * DIFFX * DIFFY / ROOTSS ** 5        
         FUVALS(IHSTRT+     4)=3.0 * DIFFX * DIFFZ / ROOTSS ** 5        
         FUVALS(IHSTRT+     3)=3.0 * DIFFY ** 2 / ROOTSS ** 5           
     *                         - 1.0 / ROOTSS ** 3                      
         FUVALS(IHSTRT+     5)=3.0 * DIFFY * DIFFZ / ROOTSS ** 5        
         FUVALS(IHSTRT+     6)=3.0 * DIFFZ ** 2 / ROOTSS ** 5           
     *                         - 1.0 / ROOTSS ** 3                      
        END IF
       END IF
       GO TO     3
C
C  ELEMENT TYPE : SQR       
C
    2  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= X * X                                    
       ELSE
        FUVALS(IGSTRT+     1)= X + X                                    
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
