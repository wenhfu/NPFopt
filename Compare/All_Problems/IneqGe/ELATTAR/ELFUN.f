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
C  Problem name : ELATTAR   
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION V1    , V2    , V3    , V4    , V5    
      DOUBLE PRECISION V6    , T     , A     , B     , EA    
      DOUBLE PRECISION CB    , SB    , EACB  , EASB  , V1EACB
      DOUBLE PRECISION V1EASB
      INTRINSIC EXP   , SIN   , COS   
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
C  Element type : ET1       
C
    1  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       V3     = XVALUE(IELVAR(ILSTRT+     3))
       V4     = XVALUE(IELVAR(ILSTRT+     4))
       T      = EPVALU(IPSTRT+     1)
       A      = - V2 * T                                 
       B      = V3 * T + V4                              
       EA     = EXP( A )                                 
       CB     = COS( B )                                 
       SB     = SIN( B )                                 
       EACB   = EA * CB                                  
       EASB   = EA * SB                                  
       V1EACB = V1 * EACB                                
       V1EASB = V1 * EASB                                
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= V1EACB                                   
       ELSE
        FUVALS(IGSTRT+     1)= EACB                                     
        FUVALS(IGSTRT+     2)= - T * V1EACB                             
        FUVALS(IGSTRT+     3)= - T * V1EASB                             
        FUVALS(IGSTRT+     4)= - V1EASB                                 
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     2)=- T * EACB                               
         FUVALS(IHSTRT+     4)=- T * EASB                               
         FUVALS(IHSTRT+     7)=- EASB                                   
         FUVALS(IHSTRT+     3)=T * T * V1EACB                           
         FUVALS(IHSTRT+     5)=T * T * V1EASB                           
         FUVALS(IHSTRT+     8)=T * V1EASB                               
         FUVALS(IHSTRT+     6)=- T * T * V1EACB                         
         FUVALS(IHSTRT+     9)=- T * V1EACB                             
         FUVALS(IHSTRT+    10)=- V1EACB                                 
         FUVALS(IHSTRT+     1)=0.0D+0
        END IF
       END IF
       GO TO     3
C
C  Element type : ET2       
C
    2  CONTINUE
       V5     = XVALUE(IELVAR(ILSTRT+     1))
       V6     = XVALUE(IELVAR(ILSTRT+     2))
       T      = EPVALU(IPSTRT+     1)
       A      = - V6 * T                                 
       EA     = EXP( A )                                 
       B      = V5 * EA                                  
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= B                                        
       ELSE
        FUVALS(IGSTRT+     1)= EA                                       
        FUVALS(IGSTRT+     2)= - T * B                                  
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     2)=- T * EA                                 
         FUVALS(IHSTRT+     3)=T * T * B                                
         FUVALS(IHSTRT+     1)=0.0D+0
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
