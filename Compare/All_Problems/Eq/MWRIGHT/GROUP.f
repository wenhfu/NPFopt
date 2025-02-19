      SUBROUTINE GROUP ( GVALUE, LGVALU, FVALUE, GPVALU, NCALCG, 
     *                   ITYPEG, ISTGPA, ICALCG, LTYPEG, LSTGPA, 
     *                   LCALCG, LFVALU, LGPVLU, DERIVS, IGSTAT )
      INTEGER LGVALU, NCALCG, LTYPEG, LSTGPA
      INTEGER LCALCG, LFVALU, LGPVLU, IGSTAT
      LOGICAL DERIVS
      INTEGER ITYPEG(LTYPEG), ISTGPA(LSTGPA), ICALCG(LCALCG)
      DOUBLE PRECISION GVALUE(LGVALU,3), FVALUE(LFVALU), GPVALU(LGPVLU)
C
C  Problem name : MWRIGHT   
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IGRTYP, IGROUP, IPSTRT, JCALCG
      DOUBLE PRECISION GVAR  
      IGSTAT = 0
      DO     4 JCALCG = 1, NCALCG
       IGROUP = ICALCG(JCALCG)
       IGRTYP = ITYPEG(IGROUP)
       IF ( IGRTYP == 0 ) GO TO     4
       IPSTRT = ISTGPA(IGROUP) - 1
       GO TO (    1,    2,    3
     *                                                        ), IGRTYP
C
C  Group type : L2      
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
C  Group type : L3      
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
C  Group type : L4      
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
