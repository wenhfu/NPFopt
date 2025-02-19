      SUBROUTINE ELFUN ( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, LTYPEE, LSTAEV, LELVAR, LNTVAR, 
     *                   LSTADH, LSTEPA, LCALCF, LFVALU, LXVALU, 
     *                   LEPVLU, IFFLAG, IFSTAT )
      INTEGER NCALCF, IFFLAG, LTYPEE, LSTAEV, LELVAR, LNTVAR
      INTEGER LSTADH, LSTEPA, LCALCF, LFVALU, LXVALU, LEPVLU
      INTEGER IFSTAT
      INTEGER ITYPEE(LTYPEE), ISTAEV(LSTAEV), IELVAR(LELVAR)
      INTEGER INTVAR(LNTVAR), ISTADH(LSTADH), ISTEPA(LSTEPA)
      INTEGER ICALCF(LCALCF)
      DOUBLE PRECISION FUVALS(LFVALU), XVALUE(LXVALU), EPVALU(LEPVLU)
C
C  Problem name : HS92      
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , X1    , X2    , X3    , X4    
      DOUBLE PRECISION X5    , X6    , F     , G(6)  , H(6,6)
      DOUBLE PRECISION EVAL92
      EXTERNAL EVAL92
      IFSTAT = 0
      DO     3 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG == 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2
     *                                                        ), IELTYP
C
C  Element type : SQR       
C
    1  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= X * X                                    
       ELSE
        FUVALS(IGSTRT+     1)= X + X                                    
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0D+0                                   
        END IF
       END IF
       GO TO     3
C
C  Element type : H         
C
    2  CONTINUE
       X1     = XVALUE(IELVAR(ILSTRT+     1))
       X2     = XVALUE(IELVAR(ILSTRT+     2))
       X3     = XVALUE(IELVAR(ILSTRT+     3))
       X4     = XVALUE(IELVAR(ILSTRT+     4))
       X5     = XVALUE(IELVAR(ILSTRT+     5))
       X6     = XVALUE(IELVAR(ILSTRT+     6))
       F      = EVAL92( X1, X2, X3, X4, X5, X6,          
     *                  G, H )                           
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= F                                        
       ELSE
        FUVALS(IGSTRT+     1)= G(1)                                     
        FUVALS(IGSTRT+     2)= G(2)                                     
        FUVALS(IGSTRT+     3)= G(3)                                     
        FUVALS(IGSTRT+     4)= G(4)                                     
        FUVALS(IGSTRT+     5)= G(5)                                     
        FUVALS(IGSTRT+     6)= G(6)                                     
        IF ( IFFLAG == 3 ) THEN
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
         FUVALS(IHSTRT+    16)=H(1,6)                                   
         FUVALS(IHSTRT+    17)=H(2,6)                                   
         FUVALS(IHSTRT+    18)=H(3,6)                                   
         FUVALS(IHSTRT+    19)=H(4,6)                                   
         FUVALS(IHSTRT+    20)=H(5,6)                                   
         FUVALS(IHSTRT+    21)=H(6,6)                                   
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
