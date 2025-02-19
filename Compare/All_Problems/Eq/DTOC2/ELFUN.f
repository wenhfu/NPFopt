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
C  Problem name : DTOC2     
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION YY1   , YY2   , YY3   , YY4   , XX1   
      DOUBLE PRECISION XX2   , YY    , ZZ    , XN2   , YN2   
      DOUBLE PRECISION SZ    , CZ    , SZ2   , SC    , CCSS  
      INTRINSIC SIN   , COS   
      IFSTAT = 0
      DO     4 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG == 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2,    3
     *                                                        ), IELTYP
C
C  Element type : SQ        
C
    2  CONTINUE
       YY     = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= YY * YY                                  
       ELSE
        FUVALS(IGSTRT+     1)= YY + YY                                  
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     4
C
C  Element type : SINE      
C
    3  CONTINUE
       ZZ     = XVALUE(IELVAR(ILSTRT+     1))
       SZ     = SIN( ZZ )                                
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= SZ                                       
       ELSE
        FUVALS(IGSTRT+     1)= COS( ZZ )                                
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=- SZ                                     
        END IF
       END IF
       GO TO     4
C
C  Element type : OEL       
C
    1  CONTINUE
       YY1    = XVALUE(IELVAR(ILSTRT+     1))
       YY2    = XVALUE(IELVAR(ILSTRT+     2))
       YY3    = XVALUE(IELVAR(ILSTRT+     3))
       YY4    = XVALUE(IELVAR(ILSTRT+     4))
       XX1    = XVALUE(IELVAR(ILSTRT+     5))
       XX2    = XVALUE(IELVAR(ILSTRT+     6))
       XN2    = XX1 * XX1 + XX2 * XX2                    
       YN2    = YY1 * YY1 + YY2 * YY2 + YY3 * YY3        
     *           + YY4 * YY4                             
       SZ     = SIN( 0.5 * XN2 )                         
       CZ     = COS( 0.5 * XN2 )                         
       SZ2    = SZ * SZ + 1.0                            
       SC     = SZ * CZ                                  
       CCSS   = CZ * CZ - SZ * SZ                        
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= YN2 * SZ2                                
       ELSE
        FUVALS(IGSTRT+     5)= 2.0 * YN2 * SC * XX1                     
        FUVALS(IGSTRT+     6)= 2.0 * YN2 * SC * XX2                     
        FUVALS(IGSTRT+     1)= 2.0 * YY1 * SZ2                          
        FUVALS(IGSTRT+     2)= 2.0 * YY2 * SZ2                          
        FUVALS(IGSTRT+     3)= 2.0 * YY3 * SZ2                          
        FUVALS(IGSTRT+     4)= 2.0 * YY4 * SZ2                          
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+    15)=2.0 * YN2 * ( SC + XX1 * XX1 * CCSS )    
         FUVALS(IHSTRT+    20)=2.0 * YN2 * XX1 * XX2 * CCSS             
         FUVALS(IHSTRT+    11)=4.0 * YY1 * SC * XX1                     
         FUVALS(IHSTRT+    12)=4.0 * YY2 * SC * XX1                     
         FUVALS(IHSTRT+    13)=4.0 * YY3 * SC * XX1                     
         FUVALS(IHSTRT+    14)=4.0 * YY4 * SC * XX1                     
         FUVALS(IHSTRT+    21)=2.0 * YN2 * ( SC + XX2 * XX2 * CCSS )    
         FUVALS(IHSTRT+    16)=4.0 * YY1 * SC * XX2                     
         FUVALS(IHSTRT+    17)=4.0 * YY2 * SC * XX2                     
         FUVALS(IHSTRT+    18)=4.0 * YY3 * SC * XX2                     
         FUVALS(IHSTRT+    19)=4.0 * YY4 * SC * XX2                     
         FUVALS(IHSTRT+     1)=2.0 * SZ2                                
         FUVALS(IHSTRT+     3)=2.0 * SZ2                                
         FUVALS(IHSTRT+     6)=2.0 * SZ2                                
         FUVALS(IHSTRT+    10)=2.0 * SZ2                                
         FUVALS(IHSTRT+     2)=0.0D+0
         FUVALS(IHSTRT+     4)=0.0D+0
         FUVALS(IHSTRT+     5)=0.0D+0
         FUVALS(IHSTRT+     7)=0.0D+0
         FUVALS(IHSTRT+     8)=0.0D+0
         FUVALS(IHSTRT+     9)=0.0D+0
        END IF
       END IF
    4 CONTINUE
      RETURN
      END
