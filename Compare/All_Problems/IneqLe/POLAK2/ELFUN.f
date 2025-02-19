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
C  Problem name : POLAK2    
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION XX1   , XX2   , XX3   , XX4   , XX5   
      DOUBLE PRECISION XX6   , XX7   , XX8   , XX9   , XX10  
      DOUBLE PRECISION P     , A     , EA    
      INTRINSIC EXP   
      IFSTAT = 0
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG == 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  Element type : EL        
C
       XX1    = XVALUE(IELVAR(ILSTRT+     1))
       XX2    = XVALUE(IELVAR(ILSTRT+     2))
       XX3    = XVALUE(IELVAR(ILSTRT+     3))
       XX4    = XVALUE(IELVAR(ILSTRT+     4))
       XX5    = XVALUE(IELVAR(ILSTRT+     5))
       XX6    = XVALUE(IELVAR(ILSTRT+     6))
       XX7    = XVALUE(IELVAR(ILSTRT+     7))
       XX8    = XVALUE(IELVAR(ILSTRT+     8))
       XX9    = XVALUE(IELVAR(ILSTRT+     9))
       XX10   = XVALUE(IELVAR(ILSTRT+    10))
       P      = EPVALU(IPSTRT+     1)
       A      = 1.0D-8 * XX1 * XX1 + ( XX2 + P ) **2     
       A      = A + XX3 * XX3 + 4.0 * XX4 * XX4          
       A      = A + XX5 * XX5 + XX6 * XX6 + XX7 * XX7    
       A      = A + XX8 * XX8 + XX9 * XX9 + XX10 * XX10  
       EA     = EXP( A )                                 
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= EA                                       
       ELSE
        FUVALS(IGSTRT+     1)= 2.0D-8 * XX1 * EA                        
        FUVALS(IGSTRT+     2)= 2.0 * ( XX2 + P ) * EA                   
        FUVALS(IGSTRT+     3)= 2.0 * XX3 * EA                           
        FUVALS(IGSTRT+     4)= 8.0 * XX4 * EA                           
        FUVALS(IGSTRT+     5)= 2.0 * XX5 * EA                           
        FUVALS(IGSTRT+     6)= 2.0 * XX6 * EA                           
        FUVALS(IGSTRT+     7)= 2.0 * XX7 * EA                           
        FUVALS(IGSTRT+     8)= 2.0 * XX8 * EA                           
        FUVALS(IGSTRT+     9)= 2.0 * XX9 * EA                           
        FUVALS(IGSTRT+    10)= 2.0 * XX10 * EA                          
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0D-8 * EA * ( 1.0 + 2.0D-8 * XX1 ** 2 )
         FUVALS(IHSTRT+     2)=4.0D-8 * XX1 * ( XX2 + P ) * EA          
         FUVALS(IHSTRT+     4)=4.0D-8 * XX1 * XX3 * EA                  
         FUVALS(IHSTRT+     7)=1.6D-7 * XX1 * XX4 * EA                  
         FUVALS(IHSTRT+    11)=4.0D-8 * XX1 * XX5 * EA                  
         FUVALS(IHSTRT+    16)=4.0D-8 * XX1 * XX6 * EA                  
         FUVALS(IHSTRT+    22)=4.0D-8 * XX1 * XX7 * EA                  
         FUVALS(IHSTRT+    29)=4.0D-8 * XX1 * XX8 * EA                  
         FUVALS(IHSTRT+    37)=4.0D-8 * XX1 * XX9 * EA                  
         FUVALS(IHSTRT+    46)=4.0D-8 * XX1 * XX10 * EA                 
         FUVALS(IHSTRT+     3)=2.0 * EA * ( 1.0 + 2.0 * ( XX2 + P)**2 ) 
         FUVALS(IHSTRT+     5)=4.0 * ( XX2 + P ) * XX3 * EA             
         FUVALS(IHSTRT+     8)=16.0 * ( XX2 + P ) * XX4 * EA            
         FUVALS(IHSTRT+    12)=4.0 * ( XX2 + P ) * XX5 * EA             
         FUVALS(IHSTRT+    17)=4.0 * ( XX2 + P ) * XX6 * EA             
         FUVALS(IHSTRT+    23)=4.0 * ( XX2 + P ) * XX7 * EA             
         FUVALS(IHSTRT+    30)=4.0 * ( XX2 + P ) * XX8 * EA             
         FUVALS(IHSTRT+    38)=4.0 * ( XX2 + P ) * XX9 * EA             
         FUVALS(IHSTRT+    47)=4.0 * ( XX2 + P ) * XX10 * EA            
         FUVALS(IHSTRT+     6)=2.0 * EA * ( 1.0 + 2.0 * XX3 * XX3 )     
         FUVALS(IHSTRT+     9)=16.0 * XX3 * XX4 * EA                    
         FUVALS(IHSTRT+    13)=4.0 * XX3 * XX5 * EA                     
         FUVALS(IHSTRT+    18)=4.0 * XX3 * XX6 * EA                     
         FUVALS(IHSTRT+    24)=4.0 * XX3 * XX7 * EA                     
         FUVALS(IHSTRT+    31)=4.0 * XX3 * XX8 * EA                     
         FUVALS(IHSTRT+    39)=4.0 * XX3 * XX9 * EA                     
         FUVALS(IHSTRT+    48)=4.0 * XX3 * XX10 * EA                    
         FUVALS(IHSTRT+    10)=8.0 * EA * ( 1.0 + 8.0 * XX4 * XX4 )     
         FUVALS(IHSTRT+    14)=16.0 * XX4 * XX5 * EA                    
         FUVALS(IHSTRT+    19)=16.0 * XX4 * XX6 * EA                    
         FUVALS(IHSTRT+    25)=16.0 * XX4 * XX7 * EA                    
         FUVALS(IHSTRT+    32)=16.0 * XX4 * XX8 * EA                    
         FUVALS(IHSTRT+    40)=16.0 * XX4 * XX9 * EA                    
         FUVALS(IHSTRT+    49)=16.0 * XX4 * XX10 * EA                   
         FUVALS(IHSTRT+    15)=2.0 * EA * ( 1.0 + 2.0 * XX5 * XX5 )     
         FUVALS(IHSTRT+    20)=4.0 * XX5 * XX6 * EA                     
         FUVALS(IHSTRT+    26)=4.0 * XX5 * XX7 * EA                     
         FUVALS(IHSTRT+    33)=4.0 * XX5 * XX8 * EA                     
         FUVALS(IHSTRT+    41)=4.0 * XX5 * XX9 * EA                     
         FUVALS(IHSTRT+    50)=4.0 * XX5 * XX10 * EA                    
         FUVALS(IHSTRT+    21)=2.0 * EA * ( 1.0 + 2.0 * XX6 * XX6 )     
         FUVALS(IHSTRT+    27)=4.0 * XX6 * XX7 * EA                     
         FUVALS(IHSTRT+    34)=4.0 * XX6 * XX8 * EA                     
         FUVALS(IHSTRT+    42)=4.0 * XX6 * XX9 * EA                     
         FUVALS(IHSTRT+    51)=4.0 * XX6 * XX10 * EA                    
         FUVALS(IHSTRT+    28)=2.0 * EA * ( 1.0 + 2.0 * XX7 * XX7 )     
         FUVALS(IHSTRT+    35)=4.0 * XX7 * XX8 * EA                     
         FUVALS(IHSTRT+    43)=4.0 * XX7 * XX9 * EA                     
         FUVALS(IHSTRT+    52)=4.0 * XX7 * XX10 * EA                    
         FUVALS(IHSTRT+    36)=2.0 * EA * ( 1.0 + 2.0 * XX8 * XX8 )     
         FUVALS(IHSTRT+    44)=4.0 * XX8 * XX9 * EA                     
         FUVALS(IHSTRT+    53)=4.0 * XX8 * XX10 * EA                    
         FUVALS(IHSTRT+    45)=2.0 * EA * ( 1.0 + 2.0 * XX9 * XX9 )     
         FUVALS(IHSTRT+    54)=4.0 * XX9 * XX10 * EA                    
         FUVALS(IHSTRT+    55)=2.0 * EA * ( 1.0 + 2.0 * XX10 * XX10 )   
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
