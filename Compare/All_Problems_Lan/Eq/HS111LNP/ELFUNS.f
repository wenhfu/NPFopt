      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : HS111LNP
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION V1    , V2    , V3    , V4    , V5    
      DOUBLE PRECISION V6    , V7    , V8    , V9    , V10   
      DOUBLE PRECISION X     , C     , LOGX  , EX    , E1    
      DOUBLE PRECISION E2    , E3    , E4    , E5    , E6    
      DOUBLE PRECISION E7    , E8    , E9    , E10   , SUM   
      INTRINSIC EXP   , LOG   
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
C  ELEMENT TYPE : EXP       
C
    2  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       EX     = EXP( X )                                 
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= EX                                       
       ELSE
        FUVALS(IGSTRT+     1)= EX                                       
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=EX                                       
        END IF
       END IF
       GO TO     3
C
C  ELEMENT TYPE : OBJ       
C
    1  CONTINUE
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
       C      = EPVALU(IPSTRT+     1)
       E1     = EXP( V1 )                                
       E2     = EXP( V2 )                                
       E3     = EXP( V3 )                                
       E4     = EXP( V4 )                                
       E5     = EXP( V5 )                                
       E6     = EXP( V6 )                                
       E7     = EXP( V7 )                                
       E8     = EXP( V8 )                                
       E9     = EXP( V9 )                                
       E10    = EXP( V10 )                               
       SUM    = E1 + E2 + E3 + E4 + E5 +                 
     *          E6 + E7 + E8 + E9 + E10                  
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= E1 * ( C + V1 - LOG( SUM ) )             
       ELSE
        FUVALS(IGSTRT+     1)= E1 * ( C + V1 - LOG( SUM ) ) +           
     *                         E1 * ( 1.0D+0 - E1 / SUM )               
        FUVALS(IGSTRT+     2)= - E1 * E2  / SUM                         
        FUVALS(IGSTRT+     3)= - E1 * E3  / SUM                         
        FUVALS(IGSTRT+     4)= - E1 * E4  / SUM                         
        FUVALS(IGSTRT+     5)= - E1 * E5  / SUM                         
        FUVALS(IGSTRT+     6)= - E1 * E6  / SUM                         
        FUVALS(IGSTRT+     7)= - E1 * E7  / SUM                         
        FUVALS(IGSTRT+     8)= - E1 * E8  / SUM                         
        FUVALS(IGSTRT+     9)= - E1 * E9  / SUM                         
        FUVALS(IGSTRT+    10)= - E1 * E10 / SUM                         
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=E1 * ( C + V1 - LOG( SUM ) ) +           
     *                         E1 * ( 1.0D+0 - E1 / SUM ) +             
     *                         E1 * ( 1.0D+0 - E1 / SUM ) +             
     *                         E1 * ( - E1 / SUM ) +                    
     *                         E1 * ( E1 ** 2 / SUM ** 2 )              
         FUVALS(IHSTRT+     2)=( -1.0D+0 + E1 / SUM) * E1 * E2 / SUM    
         FUVALS(IHSTRT+     3)=(- 1.0D+0 + E2 / SUM) * E1 * E2 / SUM    
         FUVALS(IHSTRT+     4)=( -1.0D+0 + E1 / SUM) * E1 * E3 / SUM    
         FUVALS(IHSTRT+     5)= E1 * E2 * E3 / SUM ** 2                 
         FUVALS(IHSTRT+     6)=(- 1.0D+0 + E3 / SUM) * E1 * E3 / SUM    
         FUVALS(IHSTRT+     7)=( -1.0D+0 + E1 / SUM) * E1 * E4 / SUM    
         FUVALS(IHSTRT+     8)= E1 * E2 * E4 / SUM ** 2                 
         FUVALS(IHSTRT+     9)= E1 * E3 * E4 / SUM ** 2                 
         FUVALS(IHSTRT+    10)=(- 1.0D+0 + E4 / SUM) * E1 * E4 / SUM    
         FUVALS(IHSTRT+    11)=( -1.0D+0 + E1 / SUM) * E1 * E5 / SUM    
         FUVALS(IHSTRT+    12)= E1 * E2 * E5 / SUM ** 2                 
         FUVALS(IHSTRT+    13)= E1 * E3 * E5 / SUM ** 2                 
         FUVALS(IHSTRT+    14)= E1 * E4 * E5 / SUM ** 2                 
         FUVALS(IHSTRT+    15)=(- 1.0D+0 + E5 / SUM) * E1 * E5 / SUM    
         FUVALS(IHSTRT+    16)=( -1.0D+0 + E1 / SUM) * E1 * E6 / SUM    
         FUVALS(IHSTRT+    17)= E1 * E2 * E6 / SUM ** 2                 
         FUVALS(IHSTRT+    18)= E1 * E3 * E6 / SUM ** 2                 
         FUVALS(IHSTRT+    19)= E1 * E4 * E6 / SUM ** 2                 
         FUVALS(IHSTRT+    20)= E1 * E5 * E6 / SUM ** 2                 
         FUVALS(IHSTRT+    21)=(- 1.0D+0 + E6 / SUM) * E1 * E6 / SUM    
         FUVALS(IHSTRT+    22)=( -1.0D+0 + E1 / SUM) * E1 * E7 / SUM    
         FUVALS(IHSTRT+    23)= E1 * E2 * E7 / SUM ** 2                 
         FUVALS(IHSTRT+    24)= E1 * E3 * E7 / SUM ** 2                 
         FUVALS(IHSTRT+    25)= E1 * E4 * E7 / SUM ** 2                 
         FUVALS(IHSTRT+    26)= E1 * E5 * E7 / SUM ** 2                 
         FUVALS(IHSTRT+    27)= E1 * E6 * E7 / SUM ** 2                 
         FUVALS(IHSTRT+    28)=(- 1.0D+0 + E7 / SUM) * E1 * E7 / SUM    
         FUVALS(IHSTRT+    29)=( -1.0D+0 + E1 / SUM) * E1 * E8 / SUM    
         FUVALS(IHSTRT+    30)= E1 * E2 * E8 / SUM ** 2                 
         FUVALS(IHSTRT+    31)= E1 * E3 * E8 / SUM ** 2                 
         FUVALS(IHSTRT+    32)= E1 * E4 * E8 / SUM ** 2                 
         FUVALS(IHSTRT+    33)= E1 * E5 * E8 / SUM ** 2                 
         FUVALS(IHSTRT+    34)= E1 * E6 * E8 / SUM ** 2                 
         FUVALS(IHSTRT+    35)= E1 * E7 * E8 / SUM ** 2                 
         FUVALS(IHSTRT+    36)=(- 1.0D+0 + E8 / SUM) * E1 * E8 / SUM    
         FUVALS(IHSTRT+    37)=( -1.0D+0 + E1 / SUM) * E1 * E9 / SUM    
         FUVALS(IHSTRT+    38)= E1 * E2 * E9 / SUM ** 2                 
         FUVALS(IHSTRT+    39)= E1 * E3 * E9 / SUM ** 2                 
         FUVALS(IHSTRT+    40)= E1 * E4 * E9 / SUM ** 2                 
         FUVALS(IHSTRT+    41)= E1 * E5 * E9 / SUM ** 2                 
         FUVALS(IHSTRT+    42)= E1 * E6 * E9 / SUM ** 2                 
         FUVALS(IHSTRT+    43)= E1 * E7 * E9 / SUM ** 2                 
         FUVALS(IHSTRT+    44)= E1 * E8 * E9 / SUM ** 2                 
         FUVALS(IHSTRT+    45)=(- 1.0D+0 + E9 / SUM) * E1 * E9 / SUM    
         FUVALS(IHSTRT+    46)=( -1.0D+0 + E1 / SUM) * E1 * E10/ SUM    
         FUVALS(IHSTRT+    47)= E1 * E2 * E10/ SUM ** 2                 
         FUVALS(IHSTRT+    48)= E1 * E3 * E10/ SUM ** 2                 
         FUVALS(IHSTRT+    49)= E1 * E4 * E10/ SUM ** 2                 
         FUVALS(IHSTRT+    50)= E1 * E5 * E10/ SUM ** 2                 
         FUVALS(IHSTRT+    51)= E1 * E6 * E10/ SUM ** 2                 
         FUVALS(IHSTRT+    52)= E1 * E7 * E10/ SUM ** 2                 
         FUVALS(IHSTRT+    53)= E1 * E8 * E10/ SUM ** 2                 
         FUVALS(IHSTRT+    54)= E1 * E9 * E10/ SUM ** 2                 
         FUVALS(IHSTRT+    55)=(- 1.0D+0 + E10/ SUM) * E1 * E10/ SUM    
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
