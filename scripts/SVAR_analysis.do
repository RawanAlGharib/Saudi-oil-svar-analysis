* Create a working directory
cd "C:\Users\user\Desktop\MA Applied Economics\MA Spring 2023\Applied Econ II\term paper\Term_paper Al Gharib Rawane"

* Create a log file
log using final_paper, replace


*load the dataset
use DATA.dta, clear

* Inform STATA that we are working with a time series
gen date=q(2005q1)+_n-1
format %tq date
tsset date

********** Summary Statistics of the Variables *********
asdoc sum  realgdp oilprice cpi 

                      ***GDP***
					  
tsline realgdp, name(realgdp, replace) ///
 lpattern(solid dash) title(Real GDP)
graph save realgdp "C:\Users\user\Desktop\MA Spring 2023\Applied Econ II\finalpaper\grapha.gph"
 
* Are the residuals serially correlated?
* LM Test 
regress D.realgdp L.realgdp 
predict ehat1, res
bgodfrey, lags(1 2 3 4 5)
* The LM test reveals evidence for serial correlation
* As a result, we include one lag
regress D.realgdp L.realgdp L.D.realgdp
predict ehat1a, res
bgodfrey, lags(1 2 3 4 5)
* The LM test reveals no evidence for serial correlation
* Thus, adding 1 lag is enough to remove serial correlation

* ADF Test
dfuller realgdp, trend lags(1)
* Note that tau observed = -3.356 > tau critical (5%) = -3.427
* => We do not reject the H0 of nonstationarity
* The variable 'real GDP' is nonstationary

*Generating the log difference				  
gen lnrgdp= ln(realgdp)
gen rgdp_growthrate = D.lnrgdp*100


*Plotting the data
tsline rgdp_growthrate, name(realgdp_growthrate, replace) ///
 lpattern(solid dash) title(Real GDP Growth Rate)
graph save  "C:\Users\user\Desktop\MA Spring 2023\Applied Econ II\finalpaper\graph1.gph"
 
*ADF test
regress D.rgdp_growthrate L.rgdp_growthrate
predict ehat1b, res
bgodfrey, lags(1 2 3 4 5)
*The LM test reveals no evidence for serial correlation


* The LM test reveals evidence for serial correlation
* As a result, we include one lag
regress D.rgdp_growthrate L.rgdp_growthrate L.D.rgdp_growthrate
predict ehat1c, res
bgodfrey, lags(1 2 3 4 5)
* The LM test reveals no evidence for serial correlation
* Thus, adding 1 lag is enough to remove serial correlation



dfuller rgdp_growthrate,  lags(1)
* Note that tau observed = -6.495 < tau critical (5%) =  -1.950    
* => We reject the H0 of nonstationarity
*rgdp_growthrate is stationary 


             *** Oil price ***
 
 tsline oilprice, name(oilprice, replace) ///
 lpattern(solid dash) title(oil price)
graph save "C:\Users\user\Desktop\MA Spring 2023\Applied Econ II\finalpaper\graphb.gph"

* Are the residuals serially correlated?
* LM Test 
regress D.oilprice L.oilprice 
predict ehat2, res
bgodfrey, lags(1 2 3 4 5)
* The LM test reveals evidence for serial correlation
* As a result, we include one lag
regress D.oilprice L.oilprice L.D.oilprice
predict ehat2a, res
bgodfrey, lags(1 2 3 4 5)
* The LM test reveals no evidence for serial correlation
* Thus, adding 1 lag is enough to remove serial correlation

* ADF Test
dfuller oilprice, noconstant lags(1)
* Note that tau observed =  -0.550  > tau critical (5%) = -1.94
* => We do not reject the H0 of nonstationarity
* The variable 'oil price' is nonstationary

*Generating the log difference				  
gen lnoilprice= ln(oilprice)
gen oilprice_growthrate = D.lnoilprice*100

*Plotting the data
tsline oilprice_growthrate, name(Oilprice_Growth, replace) ///
 lpattern(solid dash) title(Oil Price Growth rate)
graph save  "C:\Users\user\Desktop\MA Spring 2023\Applied Econ II\finalpaper\graph2.gph"
 
*the plot shows that the variable is stationary

*ADF test
regress D.oilprice_growthrate L.oilprice_growthrate
predict ehat2b, res
bgodfrey, lags(1 2 3 4 5)
*The LM test reveals no evidence for serial correlation

dfuller oilprice_growthrate, noconstant lags(0)
* Note that tau observed = -6.686  < tau critical (5%) =  -1.94    
* => We reject the H0 of nonstationarity
*dln_oilprice is stationary 

 
 
 
 
 
                  ***CPI***
tsline cpi, name(cpi, replace) ///
 lpattern(solid dash) title(cpi)
graph save  "C:\Users\user\Desktop\MA Spring 2023\Applied Econ II\finalpaper\graph3.gph"


* Are the residuals serially correlated?
* LM Test 
regress D.cpi L.cpi 
predict ehat3, res
bgodfrey, lags(1 2 3 4 5)
* The LM test reveals no evidence for serial correlation

* ADF Test
dfuller cpi, trend lags(0)
* Note that tau observed =  -1.117  > tau critical (5%) = -3.427
* => We do not reject the H0 of nonstationarity
* The variable 'cpi' is nonstationary

					 
*Generating the log difference				  
gen lnCPI= ln(cpi)
gen Inflation_rate = D.lnCPI*100

*Plotting the data
tsline Inflation_rate, name(Inflation_Growth, replace) ///
 lpattern(solid dash) title(Inflation Growth Rate)
graph save  "C:\Users\user\Desktop\MA Spring 2023\Applied Econ II\finalpaper\graphc.gph"
 
*ADF test
regress D.Inflation_rate L.Inflation_rate 
predict ehat3a, res 
bgodfrey, lags(1 2 3 4 5)
*The LM test reveals no evidence for serial correlation

dfuller Inflation_rate, trend lags(0)
* Note that tau observed = -7.36 < tau critical (5%) =  -1.940    
* => We reject the H0 of nonstationarity
*Inflation_rate is stationary 



*To combine all the plots in one figure
graph combine graph1.gph graph2.gph graph3.gph, cols(2) name(Figure1, replace)

graph combine grapha.gph graphb.gph graphc.gph, cols(2) name(Figure2, replace)

twoway (tsline realgdp) (tsline oilprice, yaxis(2))

twoway (tsline realgdp) (tsline cpi, yaxis(2))




                   *** VAR Model ***
				   
varsoc  rgdp_growthrate oilprice_growthrate Inflation_rate, maxlag(15)

* AIC is minimized at 15
quietly varbasic  rgdp_growthrate oilprice_growthrate Inflation_rate , lags(1/15) step(12) nograph
varlmar

*no serial correlation 

                    ***SVAR(15)***
					
*Note that matrix A refers to the B0 matrix
matrix A = (1,0,0\.,1,0\.,.,1)

*mainly refers to the variance covariance matrix for the structural model

matrix B = (.,0,0\0,.,0\0,0,.)

*Let us now estimate an SVAR(12)

svar rgdp_growthrate oilprice_growthrate Inflation_rate , lags(1/15) aeq(A) beq(B)
esttab 

vargranger


             *** Impulse Response Function ***

*irf
irf set "graph4"
irf create graph, step(12) replace
irf graph oirf, impulse(oilprice_growthrate) response(rgdp_growthrate) 
graph save  "C:\Users\user\Desktop\MA Spring 2023\Applied Econ II\finalpaper\graph4.gph"

irf set "Table4"
irf create Table4, step(12) replace
irf table oirf, impulse(oilprice_growthrate) response(rgdp_growthrate)
*** period 1,2,6 (positive and significant)***

*coirf 
irf set "graph6"
irf create graph6, step(12) replace
irf graph coirf, impulse(oilprice_growthrate) response(rgdp_growthrate) 
graph save  "C:\Users\user\Desktop\MA Spring 2023\Applied Econ II\finalpaper\graph6.gph"

irf table coirf, impulse(oilprice_growthrate) response(rgdp_growthrate)

*****************************************************************

*irf
irf set "graph5"
irf create graph5, step(12) replace
irf graph oirf, impulse(oilprice_growthrate) response(Inflation_rate) 
graph save  "C:\Users\user\Desktop\MA Spring 2023\Applied Econ II\finalpaper\graph5.gph"

irf set "Table5"
irf create Table5, step(12) replace
irf table oirf, impulse(oilprice_growthrate) response(Inflation_rate)
*** period 1,3 (positive and significant)***
*** periods 9,12 (negative and significant)***

*coirf 
irf set "graph7"
irf create graph7, step(12) replace
irf graph coirf, impulse(oilprice_growthrate) response(Inflation_rate) 
graph save "C:\Users\user\Desktop\MA Spring 2023\Applied Econ II\finalpaper\graph7.gph"
irf table coirf, impulse(oilprice_growthrate) response(Inflation_rate)



*To combine all the plots in one figure
graph combine graph4.gph graph5.gph ,  name(Figure3, replace)

graph combine graph6.gph graph7.gph , name(Figure4, replace)


           ***forecast error variance decomposition***


*irf table fevd
irf set "table_FEVD1"
irf create table_FEVD1, step(24) replace
irf table fevd, impulse(oilprice_growthrate) response(rgdp_growthrate)

**the effects are significant in all periods
**For 2 steps ahead forecast, a shock in oil price explains 16.7% of FEV of gdp growth.

*For the remaining steps ahead, a shock in oil price explains 30-40%  of FEV of gdp growth.


 
irf set "table_FEVD2"
irf create table_FEVD2, step(24) replace
irf table fevd, impulse(Inflation_rate) response(rgdp_growthrate)

**For 2 steps ahead forecast, a shock in inflation explains 34.6% of FEV of gdp growth. But we have less contribution in the subsequent periods.

*For the remaining steps ahead, a shock in inflation explains 25-30%  of FEV of gdp growth.



irf set "table_FEVD3"
irf create table_FEVD3, step(24) replace
irf table fevd, impulse(oilprice_growthrate) response(Inflation_rate)

**For 2 steps ahead forecast, a shock in oil price explains  19.1% of FEV of inflation.

**For the remaining steps ahead, a shock in oil price explains a larger fraction of FEV of inflation. 31.5% in 8th period and 44% in the 16th.


****NOTE before generating the log difference, let us check for cointegration
* Are they cointegrated?   

reg  realgdp oilprice cpi  , noconst
predict ehat4, res
dfuller ehat4, lags(0) noconst
* We fail to reject H0 of no cointegration=> no co-integration
*We expect no cointegration between the three variables


*Economic significance:
asdoc sum  rgdp_growthrate oilprice_growthrate Inflation_rate
*compare avg to coirf











