      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : HAGER3  
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION XA    , XB    , XX    , UU    , U     
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
C  ELEMENT TYPE : LINSQ     
C
    1  CONTINUE
       XA     = XVALUE(IELVAR(ILSTRT+     1))
       XB     = XVALUE(IELVAR(ILSTRT+     2))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= XA * XA +  XA * XB + XB * XB             
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * XA +  XB                           
        FUVALS(IGSTRT+     2)= 2.0 * XB +  XA                           
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
         FUVALS(IHSTRT+     2)=1.0                                      
         FUVALS(IHSTRT+     3)=2.0                                      
        END IF
       END IF
       GO TO     3
C
C  ELEMENT TYPE : LINU      
C
    2  CONTINUE
       XA     = XVALUE(IELVAR(ILSTRT+     1))
       XB     = XVALUE(IELVAR(ILSTRT+     2))
       U      = XVALUE(IELVAR(ILSTRT+     3))
       XX     =   XA    
     *          + XB    
       UU     =   U     
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= XX * UU                                  
       ELSE
        FUVALS(IGSTRT+     1)= UU                                       
        FUVALS(IGSTRT+     2)= XX                                       
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=1.0                                      
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
