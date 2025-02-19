      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : DIPIGRI 
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , Y     , S     
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
C  ELEMENT TYPE : SSQ       
C
    1  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       S      = EPVALU(IPSTRT+     1)
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= ( X - S )**2                             
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * ( X - S )                          
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     7
C
C  ELEMENT TYPE : SQ        
C
    3  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= X * X                                    
       ELSE
        FUVALS(IGSTRT+     1)= X + X                                    
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     7
C
C  ELEMENT TYPE : P4        
C
    2  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= X**4                                     
       ELSE
        FUVALS(IGSTRT+     1)= 4.0 * X**3                               
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=12.0 * X**2                              
        END IF
       END IF
       GO TO     7
C
C  ELEMENT TYPE : P6        
C
    4  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= X**6                                     
       ELSE
        FUVALS(IGSTRT+     1)= 6.0 * X**5                               
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=30.0 * X**4                              
        END IF
       END IF
       GO TO     7
C
C  ELEMENT TYPE : OE        
C
    5  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= 7.0 * X**2 + Y**4 - 4.0 * X * Y          
       ELSE
        FUVALS(IGSTRT+     1)= 14.0 * X - 4.0 * Y                       
        FUVALS(IGSTRT+     2)= 4.0 * Y**3 - 4.0 * X                     
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=14.0                                     
         FUVALS(IHSTRT+     2)=-4.0                                     
         FUVALS(IHSTRT+     3)=12.0 * Y**2                              
        END IF
       END IF
       GO TO     7
C
C  ELEMENT TYPE : CE        
C
    6  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= 4.0 * X**2 + Y**2 - 3.0 * X * Y          
       ELSE
        FUVALS(IGSTRT+     1)= 8.0 * X - 3.0 * Y                        
        FUVALS(IGSTRT+     2)= 2.0 * Y - 3.0 * X                        
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=8.0                                      
         FUVALS(IHSTRT+     2)=-3.0                                     
         FUVALS(IHSTRT+     3)=2.0                                      
        END IF
       END IF
    7 CONTINUE
      RETURN
      END
