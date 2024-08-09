// FROM OFFICIAL REPLICATION MATERIALS

******************************************************************************************
// Big list of scenarios here, then can just loop over the globals
{ // start of scenario convenience bracket

********************************************************************************
**	SCENARIO #9: Same as scenario 5, but with stronger main effect
**				
****************************************************
#delimit ;
global scen9  = "lambdas(0.05 0.05) gammas(SHAPEHERE SHAPEHERE) 
			     cov(x1 0.65 0.65 x2 -0.65 1.5)
			     tde(x1 -0.45 0)";
#delimit cr
********************************************************************************
**	SCENARIO #10: Same as scenario 6, but with stronger main effect
**
****************************************************
#delimit ;
global scen10 = "lambdas(0.05 0.05) gammas(SHAPEHERE SHAPEHERE) 
			     cov(x1 0.65 0.65 x2 -0.65 1.5)
			     tde(x1 -0.45 -0.45)";
#delimit cr
********************************************************************************
**	SCENARIO #11: Same as scenario 7, but with stronger main effect
**
****************************************************
#delimit ;
global scen11 = "lambdas(0.05 0.05) gammas(SHAPEHERE SHAPEHERE) 
			     cov(x1 0.65 0.65 x2 -0.65 1.5)
			     tde(x1 -0.45 0.45)";
#delimit cr
********************************************************************************
**	SCENARIO #12: Same as scenario 8, but with stronger main effect
**
****************************************************
#delimit ;
global scen12 = "lambdas(0.05 0.05) gammas(SHAPEHERE SHAPEHERE) 						
			     cov(x1 -0.65 0.65 x2 -0.65 1.5)
			     tde(x1 -0.45 0.45)";			
#delimit cr
********************************************************************************
**	SCENARIO #17: Same as scenario 9, but with smaller scale param for tr2 
**				  (further reduces the size of that tr's hazard)
****************************************************
#delimit ;
global scen17 = "lambdas(0.05 0.005) gammas(SHAPEHERE SHAPEHERE) 
			     cov(x1 0.65 0.65 x2 -0.65 1.5)
			     tde(x1 -0.45 0)";
#delimit cr
********************************************************************************
**	SCENARIO #18: Same as scenario 10, but with smaller scale param for tr2 
**				  (further reduces the size of that tr's hazard)
****************************************************
#delimit ;
global scen18 = "lambdas(0.05 0.005) gammas(SHAPEHERE SHAPEHERE) 
			     cov(x1 0.65 0.65 x2 -0.65 1.5)
			     tde(x1 -0.45 -0.45)";
#delimit cr
********************************************************************************
**	SCENARIO #19: Same as scenario 11, but with smaller scale param for tr2 
**				  (further reduces the size of that tr's hazard)
****************************************************
#delimit ;
global scen19 = "lambdas(0.05 0.005) gammas(SHAPEHERE SHAPEHERE) 
			     cov(x1 0.65 0.65 x2 -0.65 1.5)
			     tde(x1 -0.45 0.45)";
#delimit cr
********************************************************************************
**	SCENARIO #20: Same as scenario 12, but with smaller scale param for tr2 
**				  (further reduces the size of that tr's hazard)
****************************************************
#delimit ;
global scen20 = "lambdas(0.05 0.005) gammas(SHAPEHERE SHAPEHERE) 						
			     cov(x1 -0.65 0.65 x2 -0.65 1.5)
			     tde(x1 -0.45 0.45)";			
#delimit cr
********************************************************************************
**	SCENARIO #25: Same as scenario 17 (which has a smaller scale param for tr2 
**				  than sc. 9), but scale param isn't as small as 17's
****************************************************
#delimit ;
global scen25 = "lambdas(0.05 0.02) gammas(SHAPEHERE SHAPEHERE) 
			     cov(x1 0.65 0.65 x2 -0.65 1.5)
			     tde(x1 -0.45 0)";
#delimit cr
********************************************************************************
**	SCENARIO #26: Same as scenario 18 (which has a smaller scale param for tr2 
**				  than sc. 10), but scale param isn't as small as 18's
****************************************************
#delimit ;
global scen26 = "lambdas(0.05 0.02) gammas(SHAPEHERE SHAPEHERE) 
			     cov(x1 0.65 0.65 x2 -0.65 1.5)
			     tde(x1 -0.45 -0.45)";
#delimit cr
********************************************************************************
**	SCENARIO #27: Same as scenario 19 (which has a smaller scale param for tr2 
**				  than sc. 11), but scale param isn't as small as 19's
****************************************************
#delimit ;
global scen27 = "lambdas(0.05 0.02) gammas(SHAPEHERE SHAPEHERE) 
			     cov(x1 0.65 0.65 x2 -0.65 1.5)
			     tde(x1 -0.45 0.45)";
#delimit cr
********************************************************************************
**	SCENARIO #28: Same as scenario 20 (which has a smaller scale param for tr2 
**				  than sc. 12), but scale param isn't as small as 20's
****************************************************
#delimit ;
global scen28 = "lambdas(0.05 0.02) gammas(SHAPEHERE SHAPEHERE) 						
			     cov(x1 -0.65 0.65 x2 -0.65 1.5)
			     tde(x1 -0.45 0.45)";			
#delimit cr
********************************************************************************
**	SCENARIO #29: Same as scenario 9, but with a *bigger* scale parameter for tr2.
**
****************************************************
#delimit ;
global scen29 = "lambdas(0.05 0.1) gammas(SHAPEHERE SHAPEHERE) 
			     cov(x1 0.65 0.65 x2 -0.65 1.5)
			     tde(x1 -0.45 0)";
#delimit cr
********************************************************************************
**	SCENARIO #30: Same as scenario 10, but with a *bigger* scale parameter for tr2.
**
****************************************************
#delimit ;
global scen30 = "lambdas(0.05 0.1) gammas(SHAPEHERE SHAPEHERE) 
			     cov(x1 0.65 0.65 x2 -0.65 1.5)
			     tde(x1 -0.45 -0.45)";
#delimit cr
********************************************************************************
**	SCENARIO #31: Same as scenario 11, but with a *bigger* scale parameter for tr2.
**
****************************************************
#delimit ;
global scen31 = "lambdas(0.05 0.1) gammas(SHAPEHERE SHAPEHERE) 
			     cov(x1 0.65 0.65 x2 -0.65 1.5)
			     tde(x1 -0.45 0.45)";
#delimit cr
********************************************************************************
**	SCENARIO #32: Same as scenario 12, but with a *bigger* scale parameter for tr2.
**
****************************************************
#delimit ;
global scen32 = "lambdas(0.05 0.1) gammas(SHAPEHERE SHAPEHERE) 						
			     cov(x1 -0.65 0.65 x2 -0.65 1.5)
			     tde(x1 -0.45 0.45)";			
#delimit cr
}	// scenario end convenience bracket

// Adding blank line at end so the file can be parsed by R easily
