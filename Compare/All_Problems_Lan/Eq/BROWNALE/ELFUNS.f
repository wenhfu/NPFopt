      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : BROWNALE
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION V1    , V2    , V3    , V4    , V5    
      DOUBLE PRECISION V6    , V7    , V8    , V9    , V10   
      DOUBLE PRECISION V12   , V34   , V56   , V78   , V910  
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  ELEMENT TYPE : PROD      
C
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       V3     = XVALUE(IELVAR(ILSTRT+     3))
       V4     = XVALUE(IELVAR(ILSTRT+     4))
       V5     = XVALUE(IELVAR(ILSTRT+     5))
       V6     = XVALUE(IELVAR(ILSTRT+     6))
       V7     = XVALUE(IELVAR(ILSTRT+     7))
       V8     = XVALUE(IELVAR(ILSTRT+     8))
       V9     = XVALUE(IELVAR(ILSTRT+     9))
       V10    = XVALUE(IELVAR(ILSTRT+    10))
       V12    = V1 * V2                                  
       V34    = V3 * V4                                  
       V56    = V5 * V6                                  
       V78    = V7 * V8                                  
       V910   = V9 * V10                                 
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= V12 * V34 * V56 * V78 * V910             
       ELSE
        FUVALS(IGSTRT+     1)= V2  * V34 * V56 * V78 * V910             
        FUVALS(IGSTRT+     2)= V1  * V34 * V56 * V78 * V910             
        FUVALS(IGSTRT+     3)= V12 * V4  * V56 * V78 * V910             
        FUVALS(IGSTRT+     4)= V12 * V3  * V56 * V78 * V910             
        FUVALS(IGSTRT+     5)= V12 * V34 * V6  * V78 * V910             
        FUVALS(IGSTRT+     6)= V12 * V34 * V5  * V78 * V910             
        FUVALS(IGSTRT+     7)= V12 * V34 * V56 * V8  * V910             
        FUVALS(IGSTRT+     8)= V12 * V34 * V56 * V7  * V910             
        FUVALS(IGSTRT+     9)= V12 * V34 * V56 * V78 * V10              
        FUVALS(IGSTRT+    10)= V12 * V34 * V56 * V78 * V9               
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=      V34 * V56 * V78 * V910             
         FUVALS(IHSTRT+     4)=V2  * V4  * V56 * V78 * V910             
         FUVALS(IHSTRT+     7)=V2  * V3  * V56 * V78 * V910             
         FUVALS(IHSTRT+    11)=V2  * V34 * V6  * V78 * V910             
         FUVALS(IHSTRT+    16)=V2  * V34 * V5  * V78 * V910             
         FUVALS(IHSTRT+    22)=V2  * V34 * V56 * V8  * V910             
         FUVALS(IHSTRT+    29)=V2  * V34 * V56 * V7  * V910             
         FUVALS(IHSTRT+    37)=V2  * V34 * V56 * V78 * V10              
         FUVALS(IHSTRT+    46)=V2  * V34 * V56 * V78 * V9               
         FUVALS(IHSTRT+     5)=V1  * V4  * V56 * V78 * V910             
         FUVALS(IHSTRT+     8)=V1  * V3  * V56 * V78 * V910             
         FUVALS(IHSTRT+    12)=V1  * V34 * V6  * V78 * V910             
         FUVALS(IHSTRT+    17)=V1  * V34 * V5  * V78 * V910             
         FUVALS(IHSTRT+    23)=V1  * V34 * V56 * V8  * V910             
         FUVALS(IHSTRT+    30)=V1  * V34 * V56 * V7  * V910             
         FUVALS(IHSTRT+    38)=V1  * V34 * V56 * V78 * V10              
         FUVALS(IHSTRT+    47)=V1  * V34 * V56 * V78 * V9               
         FUVALS(IHSTRT+     9)=V12 *       V56 * V78 * V910             
         FUVALS(IHSTRT+    13)=V12 * V4  * V6  * V78 * V910             
         FUVALS(IHSTRT+    18)=V12 * V4  * V5  * V78 * V910             
         FUVALS(IHSTRT+    24)=V12 * V4  * V56 * V8  * V910             
         FUVALS(IHSTRT+    31)=V12 * V4  * V56 * V7  * V910             
         FUVALS(IHSTRT+    39)=V12 * V4  * V56 * V78 * V10              
         FUVALS(IHSTRT+    48)=V12 * V4  * V56 * V78 * V9               
         FUVALS(IHSTRT+    14)=V12 * V3  * V6  * V78 * V910             
         FUVALS(IHSTRT+    19)=V12 * V3  * V5  * V78 * V910             
         FUVALS(IHSTRT+    25)=V12 * V3  * V56 * V8  * V910             
         FUVALS(IHSTRT+    32)=V12 * V3  * V56 * V7  * V910             
         FUVALS(IHSTRT+    40)=V12 * V3  * V56 * V78 * V10              
         FUVALS(IHSTRT+    49)=V12 * V3  * V56 * V78 * V9               
         FUVALS(IHSTRT+    20)=V12 * V34 *       V78 * V910             
         FUVALS(IHSTRT+    26)=V12 * V34 * V6  * V8  * V910             
         FUVALS(IHSTRT+    33)=V12 * V34 * V6  * V7  * V910             
         FUVALS(IHSTRT+    41)=V12 * V34 * V6  * V78 * V10              
         FUVALS(IHSTRT+    50)=V12 * V34 * V6  * V78 * V9               
         FUVALS(IHSTRT+    27)=V12 * V34 * V5  * V8  * V910             
         FUVALS(IHSTRT+    34)=V12 * V34 * V5  * V7  * V910             
         FUVALS(IHSTRT+    42)=V12 * V34 * V5  * V78 * V10              
         FUVALS(IHSTRT+    51)=V12 * V34 * V5  * V78 * V9               
         FUVALS(IHSTRT+    35)=V12 * V34 * V56       * V910             
         FUVALS(IHSTRT+    43)=V12 * V34 * V56 * V8  * V10              
         FUVALS(IHSTRT+    52)=V12 * V34 * V56 * V8  * V9               
         FUVALS(IHSTRT+    44)=V12 * V34 * V56 * V7  * V10              
         FUVALS(IHSTRT+    53)=V12 * V34 * V56 * V7  * V9               
         FUVALS(IHSTRT+    54)=V12 * V34 * V56 * V78                    
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
         FUVALS(IHSTRT+     6)=0.0D+0
         FUVALS(IHSTRT+    10)=0.0D+0
         FUVALS(IHSTRT+    15)=0.0D+0
         FUVALS(IHSTRT+    21)=0.0D+0
         FUVALS(IHSTRT+    28)=0.0D+0
         FUVALS(IHSTRT+    36)=0.0D+0
         FUVALS(IHSTRT+    45)=0.0D+0
         FUVALS(IHSTRT+    55)=0.0D+0
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
