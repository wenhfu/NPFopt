      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : HS91    
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , X1    , X2    , X3    , X4    
      DOUBLE PRECISION X5    , F     , G(5)  , H(5,5), EVAL91
      EXTERNAL EVAL91
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
C  ELEMENT TYPE : SQR       
C
    1  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= X * X                                    
       ELSE
        FUVALS(IGSTRT+     1)= X + X                                    
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0D+0                                   
        END IF
       END IF
       GO TO     3
C
C  ELEMENT TYPE : H         
C
    2  CONTINUE
       X1     = XVALUE(IELVAR(ILSTRT+     1))
       X2     = XVALUE(IELVAR(ILSTRT+     2))
       X3     = XVALUE(IELVAR(ILSTRT+     3))
       X4     = XVALUE(IELVAR(ILSTRT+     4))
       X5     = XVALUE(IELVAR(ILSTRT+     5))
       F      = EVAL91( X1, X2, X3, X4, X5,              
     *                  G, H )                           
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= F                                        
       ELSE
        FUVALS(IGSTRT+     1)= G(1)                                     
        FUVALS(IGSTRT+     2)= G(2)                                     
        FUVALS(IGSTRT+     3)= G(3)                                     
        FUVALS(IGSTRT+     4)= G(4)                                     
        FUVALS(IGSTRT+     5)= G(5)                                     
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=H(1,1)                                   
         FUVALS(IHSTRT+     2)=H(1,2)                                   
         FUVALS(IHSTRT+     3)=H(2,2)                                   
         FUVALS(IHSTRT+     4)=H(1,3)                                   
         FUVALS(IHSTRT+     5)=H(2,3)                                   
         FUVALS(IHSTRT+     6)=H(3,3)                                   
         FUVALS(IHSTRT+     7)=H(1,4)                                   
         FUVALS(IHSTRT+     8)=H(2,4)                                   
         FUVALS(IHSTRT+     9)=H(3,4)                                   
         FUVALS(IHSTRT+    10)=H(4,4)                                   
         FUVALS(IHSTRT+    11)=H(1,5)                                   
         FUVALS(IHSTRT+    12)=H(2,5)                                   
         FUVALS(IHSTRT+    13)=H(3,5)                                   
         FUVALS(IHSTRT+    14)=H(4,5)                                   
         FUVALS(IHSTRT+    15)=H(5,5)                                   
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
