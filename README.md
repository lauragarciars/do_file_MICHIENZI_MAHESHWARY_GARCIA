# do_file_MICHIENZI_MAHESHWARY_GARCIA
DO-FILE EXPLORING THE DETERMINANTS OF WOMEN'S ACCESS TO EDUCATION
* Stata setup
* -----------

* Check that the -fre- command is installed.
which fre

* Allow Stata to scroll through the results.
set more off

/* ------------------------------------------ SRQM Draft 1 ----------------

 GROUP:  6 -- A. Maheshwary / A. Michienzi / L. Garcia Rodrigo

 TOPIC:  WORLD-WIDE ANALYSIS OF THE DETERMINANTS OF WOMEN'S ACCESS TO EDUCATION

 DATA:   Quality of Government (2016)

 DATE:   1st October 2021

 SUMMMARY

 In this do-file we study some variables in order to determine which of the factors studied have a real impact on Women's Access to Education (measured by Educational Attainment in years)

 HYPOTHESES

 - (H1): The presence of Political and Economic Rights have a positive influence on Women's Access to Education

 - (H2): Government 

 - (H3): Educational Attainment among women aged 24-35 is affected by the Duration of Compulsory Education. 

 QUESTIONS

 1.  What are the determinants of Women's Access to Education

 2. To what extent does Governement attitude influence Educational Attainment among Women?

----------------------------------------------------------------------------- */

* Load QOG 2016 dataset.
use data/qog2016, clear

* Summary of variables used in this research design.
d gea_ea2534f ciri_wecon ciri_wopol une_pee une_durce wbgi_pse


*-----------------------------------------------------------------
* DEPENDENT VARIABLE -- Educational Attainment (25-34 years, Female) 
*-----------------------------------------------------------------

* MEASURES: number of average years of educational attainment per capita for women between the ages of 24 and 35
* Codebook: QOG 2016, pg 295

d gea_ea2534f 

* Our DV is continuous, we summarize it to understand the distribution of our variable
su gea_ea2534f, d

* The numbers are not very indicative, we make a histogram to visualize the distribution 
hist gea_ea2534f, name(dv, replace) 

* Our DV's distribution is very skewed. We will have to fix it at some point during our analysis. 

*The name of our DV is quite complicated, we will have to rename it for ease of use. Since we won't use the other age brackets, we get rid of the numbers in the name, we know we're talking about women aged 25-34. 
ren gea_ea2534f edaf

* We check to see if the renaming operation worked. 
su edaf 

* We check for missing values within our DV
misstable pat edaf
* (with fre gea_ea2534f you already have the precise number of missing values which is equal to 3.61)
* 4% of missing values is a reasonable amount to assume that our sample is representative, we can go forward with our study. 



*-----------------------
* INDEPENDENT VARIABLES: 
*-----------------------

*IV (1)-- Women's economic rights
*--------------------------------

*MEASURES: women's economic rights include a number of internationally recognized rights like Equal pay for equal work, Free choice of profession or employment without the need to obtain a husband or male relative's consent, Non-discrimination by employers, etc
*Codebook: 	QOG 2016, 137

d ciri_wecon 

*The categories of this variable are: 
// 0. There were no economic rights for women in law and that systematic discrimination based on sex may have been built into law
// 1. Women had some economic rights under law, but these rights were not effectively enforced
// 2. Women had some economic rights under law, and the government effectively enforced these rights in practice while still allowing a low level of discrimination against women in economic matters
// 3. All or nearly all of women's economic rights were guaranteed by law and the government fully and vigorously enforces these laws in practice

*This IV is descrete (not continuous) so we do a frequency table it in order to see the distribution in numbers
fre ciri_wecon


*We rename the IV for ease of use. 
ren ciri_wecon wecon

fre wecon 
su wecon

*graphical visualisation
catplot wecon, percent name (wecon_bar, replace)


*IV (2) -- Women's political rights 
*----------------------------------

*MEASURES : includes a number of internationally recognized rights. These rights include: The right to vote, The right to run for political office, The right to hold elected and appointed government positions, The right to join political parties, The right to petition government officials

d ciri_wopol


* Categories of this variable are: 
// 0. Women's political rights were not guaranteed by law
// 1. Women's political rights were guaranteed in law, but severely prohibited in practice
// 2. Women's political rights were guaranteed in law, but were still moderately prohibited in practice 
// 3. Women's political rights were guaranteed in both law and practice

* this is a categorical variable; thus, let us check its frequencies 

fre ciri_wopol
tab ciri_wopol


*we rename the IV for ease of use
ren ciri_wopol wopol

* summary statistics
tab wopol

* graphical visualisation 
catplot wopol, percent name(wopol_bar, replace)


*IV (3) -- Government expenditure on education 
*----------------------------------

*MEASURES: Government expenditure on education as % of GDP (%).

d une_pee

* this is a continuous variable; thus, let us check its frequencies

su une_pee, d
fre une_pee

*we rename the IV for ease of use
ren une_pee pee

*summary statistics
su pee, d

*graphical visualisation
hist pee, percent normal name (qes_box, replace)


*IV (4) -- Quality of the educational system
*----------------------------------

*MEASURES: How well does the educational system in your country meet the needs of a competitive economy?
// (1) not well at all;
// (7) very well.

d wef_qes

* this is a (quasi) continuous variable; thus, let us check its frequencies

su wef_qes
fre wef_qes

*we rename the IV for ease of use
ren wef_qes qes

*summary statistics
su qes, d

*graphical visualisation
hist qes, percent normal name (qes_box, replace)



*IV (5) -- Duration of compulsory education
*-------------------------------------------

*MEASURES: Duration of compulsory education (years).

d une_durce

* this is a categorical variable; thus, let us check its frequencies
tab une_durce
fre une_durce

*we rename
ren une_durce durce

*summary statistics
su durce, d

*graphical visualisation
catplot durce, name (durce_bar, replace)


*IV (6) --- Political Stability 
*-------------------------------

*MEASURES: combines several indicators which measure perceptions of the likelihood that the government in power will be destabilized or overthrown by possibly unconstitutional and/or violent means, including domestic violence and terrorism.

d wbgi_pse 	 

*Tis is a continuous variable; thus, let us check its distribution
su wbgi_pse, d

*we rename for ease of use
ren wbgi_pse pse

*graphical visualisation
hist pse, percent normal name (pse_box, replace)



*----------------------------
* DEALING WITH MISSING VALUES 
*----------------------------

* let us check the number of observations with no missing values across all listed variables 

misstable pat edaf wecon wopol pee qes durce pse, freq 

* we can see that many observations have missing values for une_pee and wef_qes
* we check what is the percentage of observations without missing values across all the variables. 

misstable sum edaf wecon wopol pee qes durce pse
misstable pat edaf wecon wopol pee qes durce pse

* we observe that 58% of observations have no missing values, which means that 42% of observations have at least one missing value

su edaf wecon wopol pee qes durce pse
su edaf wecon wopol pee qes durce pse if !mi(pee)

* Some important changes 

su edaf wecon wopol pee qes durce pse
su edaf wecon wopol pee qes durce pse if !mi(qes)
* not impo no relevant change 

*Given the fact that the "Governement Expenditure on Education" variable alters significantly our sample with all its missing values, we have decided to drop it so as to keep as many values as possible. We also take into account that this variable was not relevant for our study, since government funding must have changed since the women whose data we are studying (aged 24-35) have left school. 

* we drop all observations with missing values 
drop if mi(edaf, wecon, wopol, qes, durce, pse)

* get final count
count 

*---------------------
* NORMALITY AND DISTRIBUTION ASSESSMENT
*---------------------

su edaf
tabstat edaf, s(n mean sd min max)
tabstat edaf, s(p25 median p75 iqr)

*visualize the distribution
hist edaf, percent 

hist edaf, kdensity

*histogram with normal distribution superimposed
hist edaf, percent normal name(hist, replace)

*Kernel density
kdensity edaf, normal legend (row(1) title ("Educational Attainment")) name(kdens, replace)


*Visual assessment
hist edaf, bin(20) normal kdensity name(histedaf, replace)

* we notice that the distribution is slightly left-skewed as demonstrated by the fact that the median is slighlty higher than the mean.

* let us verify symmetry

symplot edaf, ti("Symmetry plot") name(edaf_sym, replace)

* let us plot the quantiles of the variable against those of the normal distribution: 

qnorm edaf, ti("Normal quantile plot") name(edaf_qnorm, replace)
 
* The departures observed here are situated mostly at the tails of the distribution, which means that there is an excess of observations at these values.
* So the visual assessment tends to show that our variable does NOT follow a normal distribution.

* Formal assesment: skeweness and kurtosis
*-----------------------------------------

* let us check whether skeweness approaches 0 and kurtosis approaches 3: 

su edaf, d

* Skewness is equal to -0.5 , which is quite close to 0, which is the value that skewness takes when we are in a normal distribution situation.
* Kurtosis is equal to 2.11, which is quite close to 3, which is the value that kurtosis takes when we are in a normal distribution situation. 

* So this assessment of skewness and kurtosis tends to show that our variable is not normally distributed but it does approximate a normal distribution, since we aknowledge that deviations are natural.



* Formal assessment: intervals around the mean (+/- 1 or 2 standard deviations)
* ------------------------------------------------------------------------------

* Summary statistics.

su edaf, d

* creation of scalar mean and scalar standard deviation

sca de mean = r(mean)
sca de sd   = r(sd)
sca li

* we can make our analysis by counting the number of edaf observations that fall between (mean - 1 standard deviation) and (mean + 1 standard deviation), and then by checking if this number comes close to 68% of all observations.

count if edaf > mean - sd & edaf < mean + sd
di r(N), "observations out of", _N, "(" 100 * round(r(N) / _N, .01) "% of the sample) are within one standard deviation from the mean."

* 59% observations of the sample are within one standard deviation from the mean. This hints to the fact that our variable is not normally distributed.

* We can do the same verification with the [mean - 2 sd, mean + 2 sd] interval:
count if edaf > mean - 2 * sd & edaf < mean + 2 * sd
di r(N), "observations out of", _N, "(" 100 * round(r(N) / _N, .01) "% of the sample) are within 2 standard deviations from the mean."

* 94% observations of the sample are within 2 standard deviations from the mean. This value is quite close to 95%. 


*We save the mean and the standard deviation
su edaf, d
ret li
sca de mean = r(mean)
sca li
sca de q1  = r(p25)
sca de q3  = r(p75)
sca de iqr = q3 - q1
sca li

* outliers
*---------

* let us summarise mild (1.5 IQR) or extreme (3 IQR) outliers below Q1 and above Q3:

su edaf if edaf < q1 - 1.5 * iqr | edaf > q3 + 1.5 * iqr
*There are 0 values outside of the 1.5 IQR

su edaf if edaf < q1 - 3 * iqr   | edaf > q3 + 3 * iqr
*There are 0 values outside of the 3 IQR

*Taking into account the definition of extreme values that we adopted. We consider that this distribution does not contain any extreme values. 


* Conclusion about normality
* --------------------------
* The aforementioned results demonstrate that we cannot assume the variable edaf to be normally distributed. However, it does approximate a normal distribution.



*------------------------
*Variable Transformations
*------------------------

*Since our DV is slighlty skewed we will try to transform it and make its distribution more normal.
// gladder edaf

*It seems like a only a square transformation would work. // 
	// Since it would'nt make sense within our study to do so, we will not transform our DV (the logarithmic transformation is even more skewed than the original one). 
gen logedaf = ln(edaf)
la var logedaf "log(edaf)"

hist logedaf, percent normal name (logedaf_box, replace)
	// However, the skeweness of our dependent variable (edaf) is not significantly important, the shape of its distribution does approximate a bell curve, we conclude therefore that it does not need further transformation. 


*----------------------
*Testing of Hypotheses
*----------------------

*We want to start testing our hypothesis. 
*For Hypothesis 1 we will try and take a look at the correlation between our DV (edaf) and our two first IVs (wecon and wopol). 

*First, we make a graphic to visualize the effect of Women's Economic Rights on Educational Attainment. 
gr dot edaf, over(wecon)
*We observe that the more economic rights women in the country have, the higher the mean of our DV is. Thus, a strong presence and respect of women's economic rights seem to have a positive influence on educational attainment. 

*Then, we will also make a graphic to visualize the effect of Women's Political Rights on Educational Attainment. 
gr dot edaf, over(wopol)
*Although, less pronounced than with Economic Rights, a strong presence and implementation of women's political rights seem to have a positive influence on educational attainment.


*For Hypothesis 3 we will try and take a look at the correlation between our DV (edaf) and our IV durce. 

*We realize a graphic to visualize the correlation between Duration of Compulsory Education and Educational Attainment. 
gr dot edaf, over(durce)
*This graphic does not let us draw out clear conclusions, however, there seems to be a positive correlation between our two variables. 


*---------------------
* SIGNIFICANCE TESTS
*---------------------


*IV : Women's economic rights
*-----------------------------

su wecon 

*Generate a dummy to recode the variable into 2 categories 
recode wecon ///
	(0/1 = 0 "No rights") ///
	(2/3 = 1 "Rights") ///
	(else = .), gen(prowecon)
la var prowecon "Presence of women's economic rights'"

fre prowecon

*T-test
ttest edaf, by(prowecon)
*The null hypothesis states that women's economic rights and educational attainment among women have no relationship whatsoever. The p-value resulting of our test is close to 0, so we are confident in rejecting the null hypothesis and saying that we have a statistically significant association. 


*IV: Women's political rights 
*----------------------------

su wopol 

*Generate a dummy to recode the variable into 2 categories
recode wopol ///
	(0/1 = 0 "No rights") ///
	(2/3 = 1 "Rights") /// 
	(else = .), gen(prowopol)
la var prowopol "Presence of women's political rights'"

fre prowopol

*T-test 
ttest edaf, by(prowopol)
*The null hypothesis states that women's political rights and educational attainment among women have no relationship whatsoever. The p-value resulting of our test is quite important, quite over the standard limit of 0.05, so we cannot reject the null hypothesis, as we cannot confirm for our association to be statistically significant, this assumption could lead to a "Type I" error. However our sample can be considered small, we could be in a situation of "Type II" error.


*IV: Quality of the educational system
*--------------------------------------


su qes

*Recode into 2 groups
gen qes2:qes2 = irecode(qes, 3.5, 6)       
table qes2
la def qes2 0 "0-3.5" 1 "3.5-6"
la var qes2 "Quality of education (2 groups)"
fre qes2

*T-test 
ttest edaf, by(qes2)
*The null hypothesis states that quality of education and educational attainment among women have no relationship whatsoever. The p-value resulting of our test is close to 0, so we are confident in rejecting the null hypothesis and saying that we have a statistically significant association. 


* Study the correlation between these two variables

pwcorr edaf qes, sig star(.05)

*The correlation matrix shows a moderate-to-strong association between our DV and the quality of the educational system (r = 0.45)


*IV: Duration of compulsory education
*------------------------------------


su durce 
fre durce

*Recode into 2 groups
gen durce2:durce2 = irecode(durce, 10, 16)       
table durce2
la def durce2 0 "0-10" 1 "10-16"
la var durce2 "Duration of compulsory education (2 groups)"
fre durce2

*T-test 
ttest edaf, by(durce2)
*The null hypothesis states that duration of compulsory education and educational attainment among women have no relationship whatsoever. The p-value resulting of our test is less than 0.05, so we are confident in rejecting the null hypothesis and saying that we have a statistically significant association. 

pwcorr edaf durce, sig star(.05)
*Moderate association (r = 0.31)


*IV: Political Stability
*------------------------


su pse
fre pse

*This is a continuous variable so we will use a correlation matrix to study its statistical significance
pwcorr edaf pse, sig star(.05)

*We see a strong association between women's edcuational attainment and political stability in the country (r = 0.58)


*----------------------
*  SIMPLE REGRESSIONS
*----------------------


* Female Educational Attainment and Women's Economic Rights
*----------------------------------------------------------

*Visual fit
sc edaf wecon, $ccode ///
    name(edaf_wecon1, replace)

*Linear fit 
tw (sc edaf wecon, $ccode) (lfit edaf wecon, $ci), ///
    yti("Female Educational Attainment (per country)") ///
    name(edaf_wecon2, replace)
	
* Add 95% CI.
tw (sc edaf wecon, $ccode) (lfitci edaf wecon, $ci), ///
    yti("Female Educational Attainment (per country)") ///
    name(edaf_wecon3, replace)

*Estimate the predicted effects of women's economic rights on female educational attainment
reg edaf wecon



*** RANDOMNESS OF THE RESIDUALS
* -----------------------------

*let us check the homoskedasticity of the results 

* fitted values:

cap drop yhat
predict yhat

* residuals: 

cap drop r
predict r, resid

* let us plot the residuals against the predicted values of the DV:

sc r yhat, yline(0) $ccode name(rvfplot, replace)

* we observe that the residuals go lower in the negatives than they go higher in the positives. Moreover,they do not seem to be equally spread out on the right and on the left.
*This hints to the fact that they might not be randomly distributed.

* normality of the residuals: 

hist r, norm

* the histogram of the residuals does not seem to approximate a normal distribution. 
* We conclude that the residuals are not randomnly distributed; however, our independent variable is categorical, therefore we cannot transform it to improve our model. 


*** INTERPRETATION OF THE REGRESSION
* ----------------------------------

reg edaf i.wecon

* Given the fact that the value Prob>F is lower than 0.05, the whole model is significant.
* Thus, we reject the null hypothesis that both alpha and beta are equal to 0 with a risk of 5% of being wrong. 

* our IV is categorical, the baseline category is the first one which represents the absence of economic rights for women in law; and thus the presence of systematic discrimination based on sex into law. For this category beta is 0 and the prediction is edaf = alpha = 6.98 . The beta of the other categories are higher. Observing from the second to the fourth category the beta increase for each category. This suggests that the more women hold economic rights the more they have access to education.


* observing the p values we can notice that all betas and the alpha are significant with a risk of 5% of being wrong. Indeed, they are all lower than 0.05.


* Observing the R squared we notice that this value is moderate since it is higher than 0.1 but lower than 0.3. This means that our regression model has an acceptable explanatory power.

* The results seem to confirm our first hypothesis since they suggest that holding economic rights enables women to have more average years of educational attainment. 
* Finally, it is worth noticing that we had already observed that the results seem to confirm our first hypothesis through the t-test that we had carried out before. 




*** ===========================================================================
*                      MULTIPLE LINEAR REGRESSION
*** ===========================================================================

reg edaf i.wecon i.wopol qes2 i.durce pse

*---------------
*** DIAGNOSTICS
* --------------

* storing fitted values 

cap drop yhat
predict yhat



* 1. RANDOMNESS OF THE RESIDUALS 
***==============================

* store unstandardised residuals 

cap drop r
predict r, resid

* assess normality
*------------------

kdensity r, norm legend(off) ti("Normality of the residuals") name(norm_res, replace)

* the residuals do not seem normally distributed. 


* standardised residuals: 

cap drop rsta
predict rsta, rsta

* Let us identify the outliers beyond 2 standard deviation units:

sc rsta yhat, yline(-2 2) || sc rsta yhat if abs(rsta) > 2, ylab(-3(1)3)legend(lab(2 "Outliers")) name(norm_rsta, replace)

* Precisely counting the number of outliers:
count if abs(rsta) > 2 

* Count the total number of observations (outliers or not):
count
	
* 5.1% of observations are outliers, this value closely approximates 5%.
* However, the graphical inspection shows that the distribution is skewed.Thus, residuals might not be normally distributed.


* Homoskedasticity against y-hats
*--------------------------------

sc r yhat, yline(0) name(rvf_res, replace)

* The residuals do not seem random.
* Depending on the values taken by y-hats: 
* the residuals seem slightly more dispersed on the centre of the graph than on the extremes,
* they go lower in the negatives than they go higher in the positives. Moreover, there seems to exist a pattern in the cloud.

* lowess curve applied to standardised residuals:

lowess rsta yhat, bw(.5) yline(0) name(lowess_rsta, replace)

* the lowess curve shows a non-linear pattern in the distribution of the residuals which hints to the fact that the assumption of homoskedasticity is not met.

* Homoskedasticity against IVs
*----------------------------

* The residuals should be randomly distributed when we plot them against any of the IVs .

* - ECONOMIC RIGHTS

sc r wecon, yline(0)  legend(lab(2 "Outliers")) name(rvf_wecon, replace)

* The residuals seem not random.
* Depending on the values taken by wecon:
* the spread of the dispersion varies from the left of the graph to the right;
* they go lower in the negatives than they go higher in the positives. 

* lowess curve applied to strandardised residuals 

lowess rsta wecon, bw(.5) yline(0) name(lowess_rsta_wecon, replace)

* As we could expect from observing the dispersion the residuals are not randomnly distributed.


* - POLITICAL STABILITY

sc r pse, yline(0) legend(lab(2 "Outliers")) name(rvf_pse, replace)

* the residuals do not seem randomly. 
* Depending on the values taken by political stability: 
* the speard of the dispersion is higher on the centre than on the extremes.
* the residuals go lower in the negatives than they go higher in the positives.

* lowess curve applied to standardised residuals. 

lowess rsta pse, bw(.5) yline(0) name(lowess_rsta_pse, replace)

* the lowess curve hints to the fact that the residuals are not randomly distributed.


* the normality test of the residuals and the homokedasticity tests have failed. therefore we will try a IV transformation. 


* Trasforming the IV : PSE
*------------------------------------------------

* Let us check whether a quadratic fit is appropriate
tw (sc edaf pse, $ccode) (qfit edaf pse, $ci), name(edaf_pse_qfit, replace)
	
* The -qfit- command shows that our cloud is better summed up by a quadratic fit than it was by a linear one.

* Variable transformation.
gen sqrt_pse= sqrt(pse)
la var sqrt_pse "Political Stability (sqrt)"

* Visual inspection.
tw (sc edaf sqrt_pse, $ccode) (lfit edaf sqrt_pse, $ci), name(edaf_sqrt_pse_qfit, replace)

* Regression model with sqrt_age.
reg edaf i.wecon i.wopol qes2 i.durce sqrt_pse

* we can notice that this transformation detrimentally affects the significance of many of the beta. However, let us repeat tests to assess the randomness of the residuals 
* =============================================================


* store unstandardised residuals 

cap drop r
predict r, resid

* assess normality
*------------------

kdensity r, norm legend(off) ti("Normality of the residuals") name(norm_res, replace)

* the residuals do not seem normally distributed.They seem even worse than before the variable transformation. 


* standardised residuals: 

cap drop rsta
predict rsta, rsta

* Let us identify the outliers beyond 2 standard deviation units:

sc rsta yhat, yline(-2 2) || sc rsta yhat if abs(rsta) > 2, ylab(-3(1)3)legend(lab(2 "Outliers")) name(norm_rsta, replace)

* Precisely counting the number of outliers:
count if abs(rsta) > 2 

* Count the total number of observations (outliers or not):
count
	
* Still 52% of observations are outliers, this value is greatly higher than 5% and the situation is worsened by the varibale transformation.


* Homokedasticity against y-hats
*--------------------------------

sc r yhat, yline(0) name(rvf_res, replace)

* The residuals do not seem random.
* Depending on the values taken by y-hats:
* the residuals seem slightly more dispersed on the left of the graph than on the right,
* they go lower in the negatives than higher in the positives. 

* lowess curve applied to standardised residuals:

lowess rsta yhat, bw(.5) yline(0) name(lowess_rsta, replace)

* the lowess curve deviates from the horizontal line 0.

* Homokedasticity against IVs
*----------------------------

* The residuals should be randomly distributed when we plot them against any of the IVs .

* - PSE

sc r sqrt_pse, yline(0)  legend(lab(2 "Outliers")) name(rvf_sqrt_pse, replace)

* The residuals do not seem random.
* Depending on the values taken by age:
* the residuals' dispersion seems greater on the left than on the right;
* they go lower in the negatives than higher in the positives. 

* lowess curve applied to strandardised residuals 

lowess rsta sqrt_pse, bw(.5) yline(0) name(lowess_rsta_sqrt_pse, replace)

* As we could expect from observing dispersion within the cloud, the residuals follow
* a curve that starts from negative values on the left and then oscillates around the 0 line horizontal. 


* In the end the randomness of the residuals assumption is not met in the model when using a transformed variable. Given that this transformation does not improve our model we will disregard it and keep the original verion of pse. Given that the randomness of the residuals assumption of our regression model is not met we conclude that our final conclusions will not be very reliable. 



*** MULTICOLLINEARITY
*--------------------

reg edaf i.wecon i.wopol qes2 i.durce pse

vif

* we observe that we do not have problems of multicollinearity since all the vif values are lower than 10. 



*** ADDING INTERACTION TERMS 
*---------------------------

gen pseXqes2 = pse * qes2
la var pseXqes2 "Political Stability * Quality of education"

* Regression model.

reg edaf i.wecon i.wopol qes2 i.durce pse

* Regression model with an interaction term.

reg edaf i.wecon i.wopol qes2 i.durce pse pseXqes2

* With standardised coefficients:
reg edaf i.wecon i.wopol qes2 i.durce pse pseXqes2, b

* Adding the interaction term does not improve our model. 
* We observe that the interaction attenuates the positive relationship between the 2 IVs and the DV; indeed, its beta is negative while the IV's betas are positive. However, given the fact that the interaction term is not significative having a p-value higher than 0.05 we will not keep it in our model.



*** REMOVING INSIGNIFICANT RESULTS
* --------------------------------

reg edaf i.wecon i.wopol qes2 i.durce pse 

* Both the betas of qes and wopol are insignificant because they have a p-value that is higher than 0.05.
* The betas of categories 6 and 11 of durce are also insignificant.
* However, we will keep this variable since at least one of the betas is significant. 
reg edaf i.wecon i.durce pse 

* now all variables are significant





* ==============================================================================
*          INTERPRETING THE RESULTS OF THE FINAL REGRESSION 
* ==============================================================================

reg edaf i.wecon i.durce pse, b


*SIGNIFICANCE: 

* we can assert that the model is significant with a risk of 5% of being wrong since the value of Prob>F is equal to 0. 
* Thus, it is lower than 0.05. This means that at least one of the cofficients is different from 0 in the population, thus, this model is  better at predicting women's average years of educational attainment than random prediction.

* all the varibales' coefficients in the model are significant, with the exception of the beta coefficient for durce6, durce7, durce11, durce14 with a risk of 5% of being wrong. 


* the final regression equation is the following: 
* edaf = 4.96 + 0 wecon0 + 1.84 wecon1 + 2.89 wecon2 + 3.61 wecon3 + 3.71 durce8 + 4.1 durce9 + 3.78 durce10 + 3.96 durce12 + 6.35 durce13 + 4.83 durce15 + 1.6 pse
* This means that the number of years of educational attainment for women is estimated to be 4.96 when all the variables considered are equal to 0. 

* The betas of wecon show that there is a positive correlation between the years of educational attainment for women and the existence of economic rights; thus, they confirm our first hypothesis. Indeed, the more economic rights there exist, the more years of educational attainment are predicted. In particular, in places where all or nearly all of women's economic rights were guaranteed by law and the government fully and vigorously enforced these laws in practices (wecon3), the predicted number of years of educational attainment is higher by 3.61 years than in places where no economic rights were present (wecon0).


* The betas of durce show that the relationship between the number of years of compulary education and the average years of educational attainment is positive until a certain point and then it flattens.
* Indeed, the highest predication for the number of years of educational attainment is in places where the number of compulsory education years is 13.
* Therefore, the result seems to confirm our third hypothesis.

* The beta of pse is positive. This shows that there is a positive relation between a country's political stability and the average years of educational attainment in the country for women.

* when comparin the standardised beta we can observe that the variable affecting the average years of educational attainment for women the most is the political stability of the country.
* However, there are some remarkable standardized beta values, such as durce9, durce10 and wecon2.


* The R squared value of our model is quite high insofar as it is almost 0.5. This means that the whole model is quite effective in explaining the whole variance in the average years of educational attainment across the female population. 



*-------------
*   THE END
*-------------

*THANK YOU FOR YOUR TIME, WE HOPED YOU ENJOYED OUR WORK

*             -------------
*            - [-]    [-]  -
*            -             -   
*            -   --------  -
*            -    -----    -
*            -             -
*             -------------



*        --------
*      {  --  --  }
*      {    +     }
*      {    ---   }
*        --------
