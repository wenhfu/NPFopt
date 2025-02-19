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
C  Problem name : HAGER3    
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION XA    , XB    , XX    , UU    , U     
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
C  Element type : LINSQ     
C
    1  CONTINUE
       XA     = XVALUE(IELVAR(ILSTRT+     1))
       XB     = XVALUE(IELVAR(ILSTRT+     2))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= XA * XA +  XA * XB + XB * XB             
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * XA +  XB                           
        FUVALS(IGSTRT+     2)= 2.0 * XB +  XA                           
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
         FUVALS(IHSTRT+     2)=1.0                                      
         FUVALS(IHSTRT+     3)=2.0                                      
        END IF
       END IF
       GO TO     3
C
C  Element type : LINU      
C
    2  CONTINUE
       XA     = XVALUE(IELVAR(ILSTRT+     1))
       XB     = XVALUE(IELVAR(ILSTRT+     2))
       U      = XVALUE(IELVAR(ILSTRT+     3))
       XX     =   XA    
     *          + XB    
       UU     =   U     
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= XX * UU                                  
       ELSE
        FUVALS(IGSTRT+     1)= UU                                       
        FUVALS(IGSTRT+     2)= XX                                       
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     2)=1.0                                      
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
