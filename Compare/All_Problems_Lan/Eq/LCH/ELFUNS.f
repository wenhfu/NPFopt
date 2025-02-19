      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : LCH     
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION V     , Y     , Z     , U     , W     
      DOUBLE PRECISION X     , A     , B     , C     , P1    
      DOUBLE PRECISION P2    
      DO     5 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2,    3,    4
     *                                                        ), IELTYP
C
C  ELEMENT TYPE : SQ        
C
    1  CONTINUE
       V      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= V * V                                    
       ELSE
        FUVALS(IGSTRT+     1)= V + V                                    
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     5
C
C  ELEMENT TYPE : DIFSQ     
C
    2  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       V      =   X     
     *          - Y     
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= V * V                                    
       ELSE
        FUVALS(IGSTRT+     1)= V + V                                    
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     5
C
C  ELEMENT TYPE : PROD      
C
    3  CONTINUE
       U      = XVALUE(IELVAR(ILSTRT+     1))
       V      = XVALUE(IELVAR(ILSTRT+     2))
       W      = XVALUE(IELVAR(ILSTRT+     3))
       Y      =   U     
       Z      =   V     
     *          + W     
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= Y * Z                                    
       ELSE
        FUVALS(IGSTRT+     1)= Z                                        
        FUVALS(IGSTRT+     2)= Y                                        
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=1.0                                      
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
       GO TO     5
C
C  ELEMENT TYPE : PROD2     
C
    4  CONTINUE
       U      = XVALUE(IELVAR(ILSTRT+     1))
       V      = XVALUE(IELVAR(ILSTRT+     2))
       W      = XVALUE(IELVAR(ILSTRT+     3))
       X      = XVALUE(IELVAR(ILSTRT+     4))
       Y      = XVALUE(IELVAR(ILSTRT+     5))
       A      = EPVALU(IPSTRT+     1)
       B      = EPVALU(IPSTRT+     2)
       C      = EPVALU(IPSTRT+     3)
       P1     = A * U + B * V + C * W                    
       P2     = X * X + Y * Y                            
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= P1 * P2                                  
       ELSE
        FUVALS(IGSTRT+     1)= A * P2                                   
        FUVALS(IGSTRT+     2)= B * P2                                   
        FUVALS(IGSTRT+     3)= C * P2                                   
        FUVALS(IGSTRT+     4)= 2.0 * P1 * X                             
        FUVALS(IGSTRT+     5)= 2.0 * P1 * Y                             
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     7)=2.0 * A * X                              
         FUVALS(IHSTRT+    11)=2.0 * A * Y                              
         FUVALS(IHSTRT+     8)=2.0 * B * X                              
         FUVALS(IHSTRT+    12)=2.0 * B * Y                              
         FUVALS(IHSTRT+     9)=2.0 * C * X                              
         FUVALS(IHSTRT+    13)=2.0 * C * Y                              
         FUVALS(IHSTRT+    10)=2.0 * P1                                 
         FUVALS(IHSTRT+    15)=2.0 * P1                                 
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     2)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
         FUVALS(IHSTRT+     4)=0.0D+0
         FUVALS(IHSTRT+     5)=0.0D+0
         FUVALS(IHSTRT+     6)=0.0D+0
         FUVALS(IHSTRT+    14)=0.0D+0
        END IF
       END IF
    5 CONTINUE
      RETURN
      END
