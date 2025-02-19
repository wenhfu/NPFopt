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
C  Problem name : ORTHRDS2  
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION DX    , DY    , ZZ    , X     , Y     
      DOUBLE PRECISION ZA    , ZB    , ZC    , T     , T1    
      DOUBLE PRECISION T1SQ  , ZZSQ  
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
C  Element type : TA        
C
    1  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       ZA     = XVALUE(IELVAR(ILSTRT+     3))
       ZB     = XVALUE(IELVAR(ILSTRT+     4))
       DX     =   X     
     *          - ZA    
       DY     =   Y     
     *          - ZB    
       T      = DX * DX + DY * DY                        
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= T * T                                    
       ELSE
        FUVALS(IGSTRT+     1)= 4.0 * T * DX                             
        FUVALS(IGSTRT+     2)= 4.0 * T * DY                             
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=4.0 * ( T + 2.0 * DX * DX )              
         FUVALS(IHSTRT+     2)=8.0 * DX * DY                            
         FUVALS(IHSTRT+     3)=4.0 * ( T + 2.0 * DY * DY )              
        END IF
       END IF
       GO TO     3
C
C  Element type : TB        
C
    2  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       ZA     = XVALUE(IELVAR(ILSTRT+     3))
       ZB     = XVALUE(IELVAR(ILSTRT+     4))
       ZC     = XVALUE(IELVAR(ILSTRT+     5))
       DX     =   X     
     *          - ZA    
       DY     =   Y     
     *          - ZB    
       ZZ     =   ZC    
       T      = DX * DX + DY * DY                        
       ZZSQ   = ZZ * ZZ                                  
       T1     = 1.0 + ZZSQ                               
       T1SQ   = T1 * T1                                  
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= T * T1SQ                                 
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * DX * T1SQ                          
        FUVALS(IGSTRT+     2)= 2.0 * DY * T1SQ                          
        FUVALS(IGSTRT+     3)= 4.0 * T * T1 * ZZ                        
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0 * T1SQ                               
         FUVALS(IHSTRT+     4)=8.0 * DX * T1 * ZZ                       
         FUVALS(IHSTRT+     3)=2.0 * T1SQ                               
         FUVALS(IHSTRT+     5)=8.0 * DY * T1 * ZZ                       
         FUVALS(IHSTRT+     6)=4.0 * T * ( 2.0 * ZZSQ + T1)             
         FUVALS(IHSTRT+     2)=0.0D+0
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
