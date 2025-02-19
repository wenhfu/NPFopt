      SUBROUTINE GROUPS( GVALUE, LGVALU, FVALUE, GPVALU, NCALCG, 
     *                   ITYPEG, ISTGPA, ICALCG, DERIVS )
      INTEGER LGVALU, NCALCG
      LOGICAL DERIVS
      INTEGER ITYPEG(*), ISTGPA(*), ICALCG(*)
      DOUBLE PRECISION GVALUE(LGVALU,3), FVALUE(*), GPVALU(*)
C
C  PROBLEM NAME : MWRIGHT 
C
      INTEGER IGRTYP, IGROUP, IPSTRT, JCALCG
      DOUBLE PRECISION GVAR  
      DO     4 JCALCG = 1, NCALCG
       IGROUP = ICALCG(JCALCG)
       IGRTYP = ITYPEG(IGROUP)
       IF ( IGRTYP .EQ. 0 ) GO TO     4
       IPSTRT = ISTGPA(IGROUP) - 1
       GO TO (    1,    2,    3
     *                                                        ), IGRTYP
C
C  GROUP TYPE : L2      
C
    1  CONTINUE 
       GVAR  = FVALUE(IGROUP)
       IF ( .NOT. DERIVS ) THEN
        GVALUE(IGROUP,1)= GVAR * GVAR                              
       ELSE
        GVALUE(IGROUP,2)= GVAR + GVAR                              
        GVALUE(IGROUP,3)= 2.0                                      
       END IF
       GO TO     4
C
C  GROUP TYPE : L3      
C
    2  CONTINUE 
       GVAR  = FVALUE(IGROUP)
       IF ( .NOT. DERIVS ) THEN
        GVALUE(IGROUP,1)= GVAR**3                                  
       ELSE
        GVALUE(IGROUP,2)= 3.0 * GVAR**2                            
        GVALUE(IGROUP,3)= 6.0 * GVAR                               
       END IF
       GO TO     4
C
C  GROUP TYPE : L4      
C
    3  CONTINUE 
       GVAR  = FVALUE(IGROUP)
       IF ( .NOT. DERIVS ) THEN
        GVALUE(IGROUP,1)= GVAR**4                                  
       ELSE
        GVALUE(IGROUP,2)= 4.0 * GVAR**3                            
        GVALUE(IGROUP,3)= 12.0 * GVAR**2                           
       END IF
    4 CONTINUE
      RETURN
      END
