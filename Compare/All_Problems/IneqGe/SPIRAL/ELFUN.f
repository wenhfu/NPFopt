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
C  Problem name : SPIRAL    
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , Y     , XX    , YY    , R     
      DOUBLE PRECISION DRDX  , DRDY  , D2RDXX, D2RDYY, D2RDXY
      DOUBLE PRECISION C     , DCDX  , DCDY  , D2CDXX, D2CDYY
      DOUBLE PRECISION D2CDXY, S     , DSDX  , DSDY  , D2SDXX
      DOUBLE PRECISION D2SDYY, D2SDXY, Z     , DZDX  , DZDY  
      DOUBLE PRECISION D2ZDXX, D2ZDYY, D2ZDXY, R3    
      INTRINSIC SIN   , SQRT  , COS   
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
    1  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= X * X                                    
       ELSE
        FUVALS(IGSTRT+     1)= X + X                                    
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     4
C
C  Element type : BADCOS    
C
    2  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       XX     =  X * X                                   
       YY     =  Y * Y                                   
       R      =  SQRT( XX + YY )                         
       DRDX   =  X / R                                   
       DRDY   =  Y / R                                   
       R3     =  R**3                                    
       D2RDXX =  1.0 / R - XX / R3                       
       D2RDYY =  1.0 / R - YY / R3                       
       D2RDXY =  - X * Y / R3                            
       C      =  COS( R )                                
       S      =  SIN( R )                                
       DCDX   =  - S * DRDX                              
       DCDY   =  - S * DRDY                              
       D2CDXX =  - C * DRDX * DRDX - S * D2RDXX          
       D2CDYY =  - C * DRDY * DRDY - S * D2RDYY          
       D2CDXY =  - C * DRDX * DRDY - S * D2RDXY          
       DSDX   =  C * DRDX                                
       DSDY   =  C * DRDY                                
       D2SDXX =  - S * DRDX * DRDX + C * D2RDXX          
       D2SDYY =  - S * DRDY * DRDY + C * D2RDYY          
       D2SDXY =  - S * DRDX * DRDY + C * D2RDXY          
       Z      =  X - R * C                               
       DZDX   =  1.0 - DRDX * C - R * DCDX               
       DZDY   =  - DRDY * C - R * DCDY                   
       D2ZDXX =  - D2RDXX * C - 2.0 * DRDX * DCDX        
     *           - R * D2CDXX                            
       D2ZDYY =  - D2RDYY * C - 2.0 * DRDY * DCDY        
     *           - R * D2CDYY                            
       D2ZDXY =  - D2RDXY * C - DRDX * DCDY              
     *           - DRDY * DCDX - R * D2CDXY              
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)=  Z * Z                                   
       ELSE
        FUVALS(IGSTRT+     1)=  2.0 * DZDX *  Z                         
        FUVALS(IGSTRT+     2)=  2.0 * DZDY *  Z                         
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)= 2.0 * ( D2ZDXX * Z + DZDX * DZDX )      
         FUVALS(IHSTRT+     2)= 2.0 * ( D2ZDXY * Z + DZDX * DZDY )      
         FUVALS(IHSTRT+     3)= 2.0 * ( D2ZDYY * Z + DZDY * DZDY )      
        END IF
       END IF
       GO TO     4
C
C  Element type : BADSIN    
C
    3  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       XX     =  X * X                                   
       YY     =  Y * Y                                   
       R      =  SQRT( XX + YY )                         
       DRDX   =  X / R                                   
       DRDY   =  Y / R                                   
       R3     =  R**3                                    
       D2RDXX =  1.0 / R - XX / R3                       
       D2RDYY =  1.0 / R - YY / R3                       
       D2RDXY =  - X * Y / R3                            
       C      =  COS( R )                                
       S      =  SIN( R )                                
       DCDX   =  - S * DRDX                              
       DCDY   =  - S * DRDY                              
       D2CDXX =  - C * DRDX * DRDX - S * D2RDXX          
       D2CDYY =  - C * DRDY * DRDY - S * D2RDYY          
       D2CDXY =  - C * DRDX * DRDY - S * D2RDXY          
       DSDX   =  C * DRDX                                
       DSDY   =  C * DRDY                                
       D2SDXX =  - S * DRDX * DRDX + C * D2RDXX          
       D2SDYY =  - S * DRDY * DRDY + C * D2RDYY          
       D2SDXY =  - S * DRDX * DRDY + C * D2RDXY          
       Z      =  Y - R * S                               
       DZDX   =  - DRDX * S - R * DSDX                   
       DZDY   =  1.0 - DRDY * S - R * DSDY               
       D2ZDXX =  - D2RDXX * S - 2.0 * DRDX * DSDX        
     *           - R * D2SDXX                            
       D2ZDYY =  - D2RDYY * S - 2.0 * DRDY * DSDY        
     *           - R * D2SDYY                            
       D2ZDXY =  - D2RDXY * S - DRDX * DSDY              
     *           - DRDY * DSDX - R * D2SDXY              
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)=  Z * Z                                   
       ELSE
        FUVALS(IGSTRT+     1)=  2.0 * DZDX *  Z                         
        FUVALS(IGSTRT+     2)=  2.0 * DZDY *  Z                         
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)= 2.0 * ( D2ZDXX * Z + DZDX * DZDX )      
         FUVALS(IHSTRT+     2)= 2.0 * ( D2ZDXY * Z + DZDX * DZDY )      
         FUVALS(IHSTRT+     3)= 2.0 * ( D2ZDYY * Z + DZDY * DZDY )      
        END IF
       END IF
    4 CONTINUE
      RETURN
      END
