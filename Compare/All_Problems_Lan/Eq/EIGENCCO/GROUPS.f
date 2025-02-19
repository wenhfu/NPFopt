      SUBROUTINE GROUPS( GVALUE, LGVALU, FVALUE, GPVALU, NCALCG, 
     *                   ITYPEG, ISTGPA, ICALCG, DERIVS )
      INTEGER LGVALU, NCALCG
      LOGICAL DERIVS
      INTEGER ITYPEG(*), ISTGPA(*), ICALCG(*)
      DOUBLE PRECISION GVALUE(LGVALU,3), FVALUE(*), GPVALU(*)
C
C  PROBLEM NAME : EIGENCCO
C
      INTEGER IGRTYP, IGROUP, IPSTRT, JCALCG
      DOUBLE PRECISION GVAR  
      DO     2 JCALCG = 1, NCALCG
       IGROUP = ICALCG(JCALCG)
       IGRTYP = ITYPEG(IGROUP)
       IF ( IGRTYP .EQ. 0 ) GO TO     2
       IPSTRT = ISTGPA(IGROUP) - 1
C
C  GROUP TYPE : L2      
C
       GVAR  = FVALUE(IGROUP)
       IF ( .NOT. DERIVS ) THEN
        GVALUE(IGROUP,1)= GVAR * GVAR                              
       ELSE
        GVALUE(IGROUP,2)= GVAR + GVAR                              
        GVALUE(IGROUP,3)= 2.0D+0                                   
       END IF
    2 CONTINUE
      RETURN
      END
