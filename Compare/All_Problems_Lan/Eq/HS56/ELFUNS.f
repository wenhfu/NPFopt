      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : HS56    
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION V1    , V2    , V3    , P     , SUM   
      INTRINSIC SIN   , COS   
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
C  ELEMENT TYPE : 3PROD     
C
    1  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       V3     = XVALUE(IELVAR(ILSTRT+     3))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= V1 * V2 * V3                             
       ELSE
        FUVALS(IGSTRT+     1)= V2 * V3                                  
        FUVALS(IGSTRT+     2)= V1 * V3                                  
        FUVALS(IGSTRT+     3)= V1 * V2                                  
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=V3                                       
         FUVALS(IHSTRT+     4)=V2                                       
         FUVALS(IHSTRT+     5)=V1                                       
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
         FUVALS(IHSTRT+     6)=0.0D+0
        END IF
       END IF
       GO TO     3
C
C  ELEMENT TYPE : PSNSQ     
C
    2  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       P      = EPVALU(IPSTRT+     1)
       SUM    = V1 + V1                                  
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= -P * SIN(V1)**2                          
       ELSE
        FUVALS(IGSTRT+     1)= -P * SIN(SUM)                            
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=-2.0 * P * COS(SUM)                      
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
