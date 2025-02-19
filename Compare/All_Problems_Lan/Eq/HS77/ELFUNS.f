      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : HS77    
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION V1    , V2    , U1    , SV1MV2, CV1MV2
      INTRINSIC SIN   , COS   
      DO     4 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2,    3
     *                                                        ), IELTYP
C
C  ELEMENT TYPE : SQLN      
C
    1  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= V1**2 * V2                               
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * V1 * V2                            
        FUVALS(IGSTRT+     2)= V1**2                                    
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0 * V2                                 
         FUVALS(IHSTRT+     2)=2.0 * V1                                 
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
       GO TO     4
C
C  ELEMENT TYPE : SINE      
C
    2  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       U1     =   V1    
     *          - V2    
       SV1MV2 = SIN(U1)                                  
       CV1MV2 = COS(U1)                                  
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= SV1MV2                                   
       ELSE
        FUVALS(IGSTRT+     1)= CV1MV2                                   
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=-SV1MV2                                  
        END IF
       END IF
       GO TO     4
C
C  ELEMENT TYPE : QDSQ      
C
    3  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= V1**4 * V2**2                            
       ELSE
        FUVALS(IGSTRT+     1)= 4.0 * V1**3 * V2**2                      
        FUVALS(IGSTRT+     2)= 2.0 * V1**4 * V2                         
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=12.0 * V1**2 * V2**2                     
         FUVALS(IHSTRT+     2)=8.0 * V1**3 * V2                         
         FUVALS(IHSTRT+     3)=2.0 * V1**4                              
        END IF
       END IF
    4 CONTINUE
      RETURN
      END
