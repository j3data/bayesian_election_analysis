FILENAME indata '/folders/myfolders/CDA/flight_data.csv';
DATA flights;
 INFILE indata firstobs=2 DSD;
 LENGTH FL_DATE $25;
 LENGTH DEST_CITY_NAME $25;
 LENGTH ORIGIN_CITY_NAME $25;
 LENGTH DEP_TIME_BLK $25;
 LENGTH ARR_TIME_BLK $25;
 LENGTH REASON $25;
	 input 
	 ROW_ID FL_DATE $ DAY MONTH WEEK_DAY CARRIER $ FL_NUM ORIGIN $ ORIGIN_CITY_NAME $ DEST $ DEST_CITY_NAME $
	 CRS_DEP_TIME DEP_TIME DEP_DELAY DEP_DELAY_NEW DEP_DEL15 DEP_DELAY_GROUP DEP_TIME_BLK $ DEP_TIME_ORD 
	 TAXI_OUT WHEELS_OFF WHEELS_ON TAXI_IN CRS_ARR_TIME ARR_TIME ARR_DELAY ARR_DELAY_NEW ARR_DEL15 
	 ARR_DEL60 ARR_DELAY_GROUP ARR_TIME_BLK $ ARR_TIME_ORD CRS_ELAPSED_TIME ACTUAL_ELAPSED_TIME AIR_TIME 
	 FLIGHTS DISTANCE DISTANCE_GROUP CARRIER_DELAY WEATHER_DELAY NAS_DELAY SECURITY_DELAY 
	 LATE_AIRCRAFT_DELAY FIRST_DEP_TIME TOTAL_ADD_GTIME LONGEST_ADD_GTIME REASON $;
	 n=1;  
 ;
run;

/* printing some data */
proc print data=flights (firstobs=1 obs=14);
run;

/*   With proc logistic, the stepwise option goes through the model selection procedure for the predictors listed in the model statement*/
proc logistic data=flights descending;
	class CARRIER DEST;
	model ARR_DEL15 = MONTH WEEK_DAY CARRIER DEST DEP_TIME_ORD ARR_TIME_ORD DISTANCE / selection=stepwise
		slentry=0.1 slstay=0.1;
run;

/* running logistic regression analysis on delay based on WEEK_DAY */
proc genmod data=flights descending;
class WEEK_DAY ;
model ARR_DEL15 = WEEK_DAY / dist=bin link=logit type3	;
run;

/* testing whether treating “WEEK_DAY ” as ordinal is reasonable: simplified model does not define WEEK_DAY as a class variable */
proc genmod data=flights descending;
model ARR_DEL15 = WEEK_DAY / dist=bin link=logit type3	;
run;

/* running logistic regression analysis on low birth weight based on age */
proc genmod data=flights descending;
class WEEK_DAY ARR_TIME_ORD;
model ARR_DEL15 = WEEK_DAY ARR_TIME_ORD WEEK_DAY*ARR_TIME_ORD / dist=bin link=logit type3;
run;

/* fitting the model without interaction to “test” whether interaction should be in the model */
proc genmod data=flights descending;
	model ARR_DEL15 = WEEK_DAY ARR_TIME_ORD / dist=bin link=logit;
run;

/*   With proc logistic, the stepwise option goes through the model selection procedure for the predictors listed in the model statement*/
proc logistic data=flights descending;
class WEEK_DAY ARR_TIME_ORD;
	model ARR_DEL15 = WEEK_DAY ARR_TIME_ORD WEEK_DAY*ARR_TIME_ORD / selection=stepwise
		slentry=0.1 slstay=0.1;
run;
