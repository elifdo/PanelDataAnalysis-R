# PanelDataAnalysis-R

The data in this analysis is from a clinical trial of patients with epilepsy. For a baseline, patients were observed for 8 weeks, and then the number of 
seizures were recorded for each patient.The patients were then randomized to a treatment group by a drug called Progabide or to a placebo group. They 
were observed for four 2-week periods and the number of seizures were recorded at each follow-up.
The columns in the data sets corresponding to:
seizures: number of seizures 
id: identifying number of patient
treat: 1=treated, 0=not 
expind: 0=baseline period, 1=treatment period 
timeadj: weeks of period 
age: in years

For comparison reasons, I adjusted number of seizures on the first time point by dividing to 4 and saved it as "data_new". But I used original data for the 
further analyses. It seems like in both placebo and treatment groups mean number of seizures went higher than the baseline in almost all cases, only in the last
 time point in treatment group being less. It is not what we would expect, it might mean that epilepsy patients are getting worse throughout time both with the
 treatment or with no treatment. Another reason might be that data has outliers; some patients have really high seizure numbers after the first time point. 
I found these outliers, whom act very differently in all time points, which are with id number 18 and 49. And I excluded them from the exploratory data analysis
 since they have a big effect. But since these patients also have important information, it is not a good idea to exclude them totally from further analyses. 
As you can see after excluding two subjects from the data, in treatment group mean number of seizures for all time points are smaller than the baseline. 
And there is a more prominent difference between placebo and treatment group where treatment group has smaller mean number of seizures throughout. 

	Baseline	Time 1	Time 2	Time 3	Time 4

Mean for treatment 	7.911290 	8.580645	8.419355	8.129032	6.741935

	Baseline	Time 1	Time 2	Time 3	Time 4

Mean for placebo 	7.696429 	9.357143	8.285714	8.785714	8.000000



Without outliers	Baseline	Time 1	Time 2	Time 3	Time 4

Mean for treatment 	6.916667 	5.466667	6.533333	6.000000	4.866667

Without outliers	Baseline	Time 1	Time 2	Time 3	Time 4

Mean for placebo 	6.953704 	8.333333	7.518519	8.074074	7.222222

It seems like, on first time point number of seizures decreased a lot for the treatment group, then got higher but not as much as the baseline and decreased 
gradually afterwards. This instant decrease and then increase might be because of some outliers which has less number of seizures on the first time point 
but high on other timepoints. Or it might be the nature of the healing process of the drug. There needs to be further investigation done to decide which is 
true.  
 
Histogram of the number of seizures for different time points show baseline has a different distribution. This is because of the time period is 4 times higher
 for the baseline. I adjusted by dividing to 4 for comparison reasons. However, I used the original values for the further analyses.

As can be seen in the boxplots, treatment group has less variance and less mean values than the placebo group. This encourages the idea of drug has 
a meaningful effect on the number of seizures altough further investigations are needed. 
  
As can be seen in the figures, age doesn’t seem to have an effect on the number of seizures for the placebo group but has an effect on treatment group. 
It suggests that it might be a good idea to check the interaction term treatment and age.
 
Marginal models:

Firstly, if estimated correlation turned out to be negative, I discarded that model, since we don’t expect this. Also, when I look at the Wald statistics 
and it seems like most of the variables used are insignificant, there is no point of using that model. Lastly, to decide between the models in hand, 
I used QIC values, QIC is being used when there is no log-likelihoods to begin with for comparison of our models. Since marginal models doesn’t use a 
likelihood approach, to mimic AIK, Pan suggested quasi-likelihood information criteria, QIC. I used the package “QICpack” by Daniel Hocking. 
We choose the model with smaller QICu value to compare the subsets of covariates and use QIC value to compare correlation structures. The model with the 
smaller value is preferred in both cases. A table I used for comparison can be seen in the following figure. 

	Model 1	Model2	Model 3	Model4
QIC	-10801	-10829	-10814	-10807

QICu	10806	10835	10818	-10811

Log.QLik	5408	5427   	5414	5412  

I applied some models with poisson link since number of seizures are integers. But I also wanted to check if baseline values being different than 
other time points affect the fit. Therefore, I divided baseline seizure values with 4 and used gaussian link too. Then I realized, using raw data 
with timeadj variable or expind makes more sense and gives better results. Interestingly, in the final model there is no time or treatment variable.
 So as a final check I also compared mean square sum of the residuals to make sure the final result make sense. And this approach also supports the 
final model. Therefore, final model became;

 Seizures - exp(0.268  +0.007 * age + 0.028 *base +0.237* timeadj )

Positive coefficients mean as age gets bigger number of seizures also increases. As number of seizures on the baseline increases, number of seizures 
in the further time points increase. And as the other variables kept fixed, number of seizures in the baseline is higher than the number of seizures 
on the following time point. This last sentence actually suggests that, as some of the subjects use the drug, number of seizures decreases although we 
don’t end up with a final model where treatment variable is significant. So, I would conclude that there is an effect of the drug but marginal models 
couldn’t capture the nature of the data well enough to see the difference explicitly.
 
Since there is no time variable in the model, it will give the same value for every time point except the baseline. The difference between the baseline 
and other time points can be found as:

For baseline:   seizures|baseline = exp(C + 0.23684*8  )

For other time points: seizures |others = exp(C+0.23684*2  )

If we look at the ratio we get: ratio= exp(0.23684*6  )= 4.14 , which means that number of seizures at the baseline is 4.14 times the other time points. 
Estimated number of seizures for the first person at baseline is 14.9.

Transition models: 

In transition models, always independence structure has to be used. But for exploration resaons i tried some other structures too. When I use exchangeable
 correlation structure, ylag becomes insignificant, this is not what I would want from a transition model, the idea behind it is to utilize that information. 
When I used unstructured, it gave unexpected correlations. Therefore, I discarded that model too. When I used independence, treatment became insignificant, 
I built a simpler model without it and Anova results gave that simpler model should be used. I also used ar-1 structure, again treatment was insignificant, 
so I used reduced model suggested by Anova. Among these two models, I first checked their residual sum of squares, there doesn’t seem to be a significant 
difference among them. However, independence doesn’t seem to be a realistic assumption. I also applied anova and saw that p-value is small, so I chose the 
complicated model with ar1 structure. Final model became;

Seizures - exp( 0.35707 + 0.02162 * age + 0.0306 *base - 0.01212* ylag1 )
To compare two people with age difference 10 and all other variables fixed,

For age=x:         seizures|(age=x) = exp(C + 0.02162 * x)

For age=x+10 : seizures |(age=x+10) = exp(C+0.02162 * (x+10))

So, (seizures|age=x+10) /( seizures |age=x)= exp(0.02162 * 10 ) =exp(0.2162)=1.24

So, number of seizures a person with age x+10 have will be 1.24 times the number of seizures a person 10 years younger than him have, if all other variables
 are fixed.

To compare two people with baseline seizures difference 10 and all other variables fixed,

(seizures|base=x+10) /( seizures |base=x)= exp(0.0306 * 10 )= exp(0.306)= 1.36.

So, number of seizures a person with baseline x+10 have will be 1.36 times the number of seizures a person with baseline x have, if all other variables 
are fixed.

Number of seizures a person with age 0, baseline 0 and ylag1 0 will be exp(0.35707), but these values doesn’t make sense. So practically, 
coefficient 0.35707 doesn’t have a standalone interpretation. 

If the ylag1 value is 10 times higher for the same person:

For ylag1=x+10 : seizures |(ylag1=x+10) = exp(C-0.01212 * (x+10))

For ylag1=x:         seizures|(ylag1=x) = exp(C - 0.01212 * x)

So, (seizures| ylag1=x+10) /( seizures | ylag1=x)= exp(-0.01212 * 10 )= exp(-0.1212)= 0.886.

So, number of seizures a person with ylag1 x+10 have will be 0.886 times the number of seizures a person with ylag1 x have, if all other variables are fixed. 
Therefore, as number of seizures in earlier time point increases, number of seizures decreases, which doesn’t make sense. A reason would be, baseline variable 
also taking this effect somehow, so we should think the effect of base and ylag1 together instead of separately. 

We cannot use these QIC values to compare, because number of responses is different for marginal and transition models. We cannot estimate the number of 
seizures for the first person at baseline since there is no lag1 information for this point. But the estimated number of seizures for the first person 
at the second time point will be 3.68.   

When I compare sum of residual squares for both marginal and transition models, I get, 161 and 103 respectively. Therefore, I would choose the model with 
the smaller error, transition model. Also, this is a result I would expect since transition models capture the nature of panel data better than marginal models. 
 
Random effects model:

I fit random effects model with just random intercept, time seemed to be insignificant. Therefore, I built the same model without time variable. 
Anova between them resulted in a high p-value, therefore, I chose the later simpler model. Same happened with random intercept and random slope. 
Then, I applied another Anova between these two models, and found a small the p-value, which suggests picking the more complex model, which is the later one. 
So, final model is:


Seizures - exp(9.9261  -1.2110 * treat + 0.1030 *age +0.5576* base-20.8027*expind+(0+randomeffect_i *time))

Intercept being 0 in random effect part means for the baseline there is no change between individuals but as time changes individual effects happen. 
Also, variance for the random effects is high, which suggests that individual effects are important. Interpretation of the coefficient can be done only 
by keeping individual effect fixed. If the same person was 10 years younger etc. In this model, treatment has a negative coefficient which means drug makes
 a negative effect on the number of seizures. Age has a positive coefficient which means as age gets higher number of seizures are getting higher. 
Base has a positive coefficient, which means that if a person has a higher number of seizures at the baseline, he will have higher number of seizures 
at the following time points.  Expind variable has a negative coefficient, which means that on the treatment period number of seizures are less than the 
baseline period, which suggests that drug has a meaningful effect. 

If individual effects are kept fixed, comparison of a person in placebo and treatment group will be:

For placebo :          seizures |placebo= exp(C- 1.2110 *0 )

For treatment:         seizures|treatment= exp(C - 1.2110 *1)

So, (seizures|placebo) /( seizures | treatment)= exp(1.2120 )= 3.36. Number of seizures a person would have if he was in the placebo group is 3.36 times
 the number of seizures if he was in the treatment group. 

If a person was 10 years older the number of seizures he would have is exp(1.030)=2.8 times the number of seizures if he was 10 years younger, if all other
 variables are fixed. 
The coefficient -20.8027 doesn’t have a direct interpretation because we cannot compare baseline and following timepoints without changing the time 
variable too.

I would prefer random effects model since it is more compatible with our exploratory data analysis, which suggested that treatment had a meaningful effect.
Treatment variable was insignificant in the marginal model. Also, we have seen that variance is high for the random coefficients, which means individual 
effects exists. However, marginal models cannot capture this aspect of the data. 
The estimated number of seizures for the first person at the baseline is 19.3.	 
