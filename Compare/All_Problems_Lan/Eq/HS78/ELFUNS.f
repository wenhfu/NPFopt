      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : HS78    
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION V1    , V2    , V3    , V4    , V5    
      DOUBLE PRECISION X     , Y     
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
C  ELEMENT TYPE : 5PR       
C
    1  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       V3     = XVALUE(IELVAR(ILSTRT+     3))
       V4     = XVALUE(IELVAR(ILSTRT+     4))
       V5     = XVALUE(IELVAR(ILSTRT+     5))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= V1 * V2 * V3 * V4 * V5                   
       ELSE
        FUVALS(IGSTRT+     1)= V2 * V3 * V4 * V5                        
        FUVALS(IGSTRT+     2)= V1 * V3 * V4 * V5                        
        FUVALS(IGSTRT+     3)= V1 * V2 * V4 * V5                        
        FUVALS(IGSTRT+     4)= V1 * V2 * V3 * V5                        
        FUVALS(IGSTRT+     5)= V1 * V2 * V3 * V4                        
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=V3 * V4 * V5                             
         FUVALS(IHSTRT+     4)=V2 * V4 * V5                             
         FUVALS(IHSTRT+     7)=V2 * V3 * V5                             
         FUVALS(IHSTRT+    11)=V2 * V3 * V4                             
         FUVALS(IHSTRT+     5)=V1 * V4 * V5                             
         FUVALS(IHSTRT+     8)=V1 * V3 * V5                             
         FUVALS(IHSTRT+    12)=V1 * V3 * V4                             
         FUVALS(IHSTRT+     9)=V1 * V2 * V5                             
         FUVALS(IHSTRT+    13)=V1 * V2 * V4                             
         FUVALS(IHSTRT+    14)=V1 * V2 * V3                             
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
         FUVALS(IHSTRT+     6)=0.0D+0
         FUVALS(IHSTRT+    10)=0.0D+0
         FUVALS(IHSTRT+    15)=0.0D+0
        END IF
       END IF
       GO TO     5
C
C  ELEMENT TYPE : CB        
C
    2  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= X**3                                     
       ELSE
        FUVALS(IGSTRT+     1)= 3.0 * X**2                               
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=6.0 * X                                  
        END IF
       END IF
       GO TO     5
C
C  ELEMENT TYPE : SQ        
C
    3  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= X * X                                    
       ELSE
        FUVALS(IGSTRT+     1)= X + X                                    
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     5
C
C  ELEMENT TYPE : 2PR       
C
    4  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= X * Y                                    
       ELSE
        FUVALS(IGSTRT+     1)= Y                                        
        FUVALS(IGSTRT+     2)= X                                        
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=1.0                                      
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
    5 CONTINUE
      RETURN
      END
