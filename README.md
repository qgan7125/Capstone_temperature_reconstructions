# Capstone Temperature Reconstructions

## Goals
We expect to improve the basic toolbox for inferring temperature reconstructions based on the ‘clumped isotopes’ paleothermometer. Using three synthetic datasets with varying errors for temperature (independent variable) and Δ47 (response variable), we will examine the performance of alternative regression models on recovering true simulation parameters with the highest precision and smallest associated uncertainty. We will particularly focus on parametric models that are able to describe linear associations between variables under an univariate framework. Previous research in the field has examined the performance of a limited set of regression models (commonly used in the field; OLS, Deming, York regressions) in the context of parameter estimates. However, these studies have failed to examine the performance of other regression approaches. For instance, weighted and unweighted orthogonal distance regression models are still to be examined in a comparative framework. In this capstone project, we will explore additional models that could improve accuracy and precision during parameter estimation based on ‘clumped isotope’ data. 

## Data
Three simulated datasets come from the professor Cristian (Román Palacios et al., 2021)

Román Palacios, C., Carroll, H., Arnold, A., Flores, R., Petersen, S., McKinnon, K., & Tripati, A. (2021). BayClump: Bayesian Calibration and Temperature Reconstructions for Clumped Isotope Thermometry. Earth and Space Science Open Archive, 40. https://doi.org/10.1002/essoar.10507995.1 
```
Response variable: D47
Independent variable: 10^6/T^2
True slope: 0.0369
True intercept: 0.268
```

## Baseline Models (Due: February 25<sub>th</sub>)
***Ordinary least squares linear regression***：OLS is for estimating coefficients of linear regression that describe the relationship between one or more independent variables and one dependent variable. 

***Deming regression***：An errors-in-variables model which tries to find the line of best fit for a two-dimensional dataset. 

***Bayesian linear model***：Bayesian linear model estimates and updates regression parameters based on prior studies and newly generated clumped isotope data.

## New Explored Models (Due: March 15<sub>th</sub>)
***Orthogonal distance regression models*** ：Finding the maximum likelihood estimators of parameters in measurement error models in the case of normally distributed errors.

***Theil-Sen regression***：A non-parametric method which means it makes no assumption about the underlying distribution of the data. The estimation of the model is done by calculating the slopes and intercepts of a subpopulation of all possible combinations of p subsample points.

***Huber Regression***：A regression model that is robust to outliers. The idea is to use different loss functions rather than the traditional least squares.

***Elastic-net regression***: A linear regression model using combinations of two popular penalties, specifically the L1 and L2 penalty functions.

***Quantile Regression***: Estimates the median or other quantiles of y conditional on x, while ordinary least squares (OLS) estimates the conditional mean.

***Generalized Linear Regression***: a flexible generalization of ordinary linear regression. The GLM generalized linear regression by allowing the linear model to be related to the response variable via a link function and by allowing the magnitude of the variance of each measurement to be a function of its predicted value.
