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
C  Problem name : PENTAGON  
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION DX    , DY    , XA    , YA    , XB    
      DOUBLE PRECISION YB    , D     , D9    , D10   
      IFSTAT = 0
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG == 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  Element type : IDIST     
C
       XA     = XVALUE(IELVAR(ILSTRT+     1))
       YA     = XVALUE(IELVAR(ILSTRT+     2))
       XB     = XVALUE(IELVAR(ILSTRT+     3))
       YB     = XVALUE(IELVAR(ILSTRT+     4))
       DX     =   XA    
     *          - XB    
       DY     =   YA    
     *          - YB    
       D      = DX * DX + DY * DY                        
       D9     = D**9                                     
       D10    = D9 * D                                   
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= 1.0 / D**8                               
       ELSE
        FUVALS(IGSTRT+     1)= -16.0 * DX / D9                          
        FUVALS(IGSTRT+     2)= -16.0 * DY / D9                          
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=16.0 * ( 18.0 * DX * DX - D ) / D10      
         FUVALS(IHSTRT+     2)=288.0 * DX * DY / D10                    
         FUVALS(IHSTRT+     3)=16.0 * ( 18.0 * DY * DY - D ) / D10      
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
