      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : METHANB8
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION V1    , V2    , V3    , P1    , P2    
      DOUBLE PRECISION P6    , P7    , P8    , P3    , P4    
      DOUBLE PRECISION P5    , EXPROD, F     , POLY  , DPOLY 
      DOUBLE PRECISION TERM  
      INTRINSIC EXP   
      DO     7 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2,    3,    4,    5,    6
     *                                                        ), IELTYP
C
C  ELEMENT TYPE : 2PROD     
C
    1  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       P1     = EPVALU(IPSTRT+     1)
       P2     = EPVALU(IPSTRT+     2)
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= P1 * V1 * ( V2 + P2 )                    
       ELSE
        FUVALS(IGSTRT+     1)= P1 * ( V2 + P2 )                         
        FUVALS(IGSTRT+     2)= P1 * V1                                  
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=P1                                       
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
       GO TO     7
C
C  ELEMENT TYPE : POLY1PRD  
C
    2  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       P1     = EPVALU(IPSTRT+     1)
       P6     = EPVALU(IPSTRT+     2)
       P7     = EPVALU(IPSTRT+     3)
       P8     = EPVALU(IPSTRT+     4)
       POLY   = P6 + P7 * V2 + P8 * V2 * V2              
       DPOLY  = P7 + 2.0 * P8 * V2                       
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= P1 * V1 * POLY                           
       ELSE
        FUVALS(IGSTRT+     1)= P1 * POLY                                
        FUVALS(IGSTRT+     2)= P1 * V1 * DPOLY                          
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=P1 * DPOLY                               
         FUVALS(IHSTRT+     3)=P1 * V1 * 2.0D+0 * P8                    
         FUVALS(IHSTRT+     1)=0.0D+0
        END IF
       END IF
       GO TO     7
C
C  ELEMENT TYPE : POLY2PRD  
C
    3  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       V3     = XVALUE(IELVAR(ILSTRT+     3))
       P1     = EPVALU(IPSTRT+     1)
       P2     = EPVALU(IPSTRT+     2)
       P6     = EPVALU(IPSTRT+     3)
       P7     = EPVALU(IPSTRT+     4)
       P8     = EPVALU(IPSTRT+     5)
       POLY   = P6 + P7 * V3 + P8 * V3 * V3              
       DPOLY  = P7 + 2.0 * P8 * V3                       
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= P1 * V1 * ( P2 + V2 ) * POLY             
       ELSE
        FUVALS(IGSTRT+     1)= P1 * ( P2 + V2 ) * POLY                  
        FUVALS(IGSTRT+     2)= P1 * V1 * POLY                           
        FUVALS(IGSTRT+     3)= P1 * V1 * ( P2 + V2 ) * DPOLY            
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=P1 * POLY                                
         FUVALS(IHSTRT+     4)=P1 * ( P2 + V2 ) * DPOLY                 
         FUVALS(IHSTRT+     5)=P1 * V1 * DPOLY                          
         FUVALS(IHSTRT+     6)=P1 * V1 * ( P2 + V2 ) * 2.0D+0 * P8      
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
       GO TO     7
C
C  ELEMENT TYPE : EXP2PROD  
C
    4  CONTINUE
       V2     = XVALUE(IELVAR(ILSTRT+     1))
       V3     = XVALUE(IELVAR(ILSTRT+     2))
       P1     = EPVALU(IPSTRT+     1)
       P2     = EPVALU(IPSTRT+     2)
       P3     = EPVALU(IPSTRT+     3)
       P4     = EPVALU(IPSTRT+     4)
       P5     = EPVALU(IPSTRT+     5)
       EXPROD = P1 * P2 * EXP(P3 + (P4 / (V3 + P5)))     
       F      = V2 * EXPROD                              
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= F                                        
       ELSE
        FUVALS(IGSTRT+     1)= EXPROD                                   
        FUVALS(IGSTRT+     2)= - V2 * EXPROD * P4 / (P5 + V3)**2        
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=- EXPROD * P4 / (P5 + V3)**2             
         FUVALS(IHSTRT+     3)=F * (P4 / (P5 + V3)**2)**2               
     *                         + 2.0D+0 * F * P4 / (P5 + V3)**3         
         FUVALS(IHSTRT+     1)=0.0D+0
        END IF
       END IF
       GO TO     7
C
C  ELEMENT TYPE : EXP3PROD  
C
    5  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       V3     = XVALUE(IELVAR(ILSTRT+     3))
       P1     = EPVALU(IPSTRT+     1)
       P2     = EPVALU(IPSTRT+     2)
       P3     = EPVALU(IPSTRT+     3)
       P4     = EPVALU(IPSTRT+     4)
       P5     = EPVALU(IPSTRT+     5)
       EXPROD = P1 * P2 * EXP(P3 + (P4 / (V3 + P5)))     
       F      = V1 * V2 * EXPROD                         
       TERM   = - P4 / (P5 + V3) ** 2                    
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= F                                        
       ELSE
        FUVALS(IGSTRT+     1)= V2 * EXPROD                              
        FUVALS(IGSTRT+     2)= V1 * EXPROD                              
        FUVALS(IGSTRT+     3)= F * TERM                                 
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=EXPROD                                   
         FUVALS(IHSTRT+     4)=V2 * EXPROD * TERM                       
         FUVALS(IHSTRT+     5)=V1 * EXPROD * TERM                       
         FUVALS(IHSTRT+     6)=F * ( TERM * TERM                        
     *                         + 2.0D+0 * P4 / (P5 + V3)**3 )           
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
       GO TO     7
C
C  ELEMENT TYPE : EXP4PROD  
C
    6  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       V3     = XVALUE(IELVAR(ILSTRT+     3))
       P1     = EPVALU(IPSTRT+     1)
       P2     = EPVALU(IPSTRT+     2)
       P3     = EPVALU(IPSTRT+     3)
       P4     = EPVALU(IPSTRT+     4)
       P5     = EPVALU(IPSTRT+     5)
       P6     = EPVALU(IPSTRT+     6)
       P7     = EPVALU(IPSTRT+     7)
       P8     = EPVALU(IPSTRT+     8)
       EXPROD = P1 * P2 * EXP(P3 + (P4 / (V3 + P5)))     
       F      = V1 * V2 * EXPROD                         
       POLY   = P6 + P7 * V3 + P8 * V3 * V3              
       DPOLY  = P7 + 2.0 * P8 * V3                       
       TERM   = DPOLY - POLY * P4 / (P5 + V3) ** 2       
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= F * POLY                                 
       ELSE
        FUVALS(IGSTRT+     1)= V2 * EXPROD * POLY                       
        FUVALS(IGSTRT+     2)= V1 * EXPROD * POLY                       
        FUVALS(IGSTRT+     3)= F * TERM                                 
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=EXPROD * POLY                            
         FUVALS(IHSTRT+     4)=V2 * EXPROD * TERM                       
         FUVALS(IHSTRT+     5)=V1 * EXPROD * TERM                       
         FUVALS(IHSTRT+     6)=F * ( - (P4 / (P5 + V3)**2) * TERM       
     *                         + 2.0 * P8 - DPOLY * P4 / (P5 + V3)**2   
     *                         + 2.0D+0 * POLY * P4 / (P5 + V3)**3 )    
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
    7 CONTINUE
      RETURN
      END
