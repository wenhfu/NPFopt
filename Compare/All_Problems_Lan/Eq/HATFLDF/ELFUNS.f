      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : HATFLDF 
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , Y     , T     , EX    
      INTRINSIC EXP   
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  ELEMENT TYPE : XPEXP     
C
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       T      = EPVALU(IPSTRT+     1)
       EX     = EXP( T * Y )                             
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= X * EX                                   
       ELSE
        FUVALS(IGSTRT+     1)= EX                                       
        FUVALS(IGSTRT+     2)= T * X * EX                               
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=T * EX                                   
         FUVALS(IHSTRT+     3)=T * T * X * EX                           
         FUVALS(IHSTRT+     1)=0.0D+0
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
