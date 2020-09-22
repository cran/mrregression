modelIntercept = '
data {
int<lower=0> n;
int<lower=0> d;
matrix[n,d] X; // predictor matrix
vector[n] y; // outcome vector
}
parameters {
real alpha; // intercept
vector[d] beta; // coefficients for predictors
real<lower=0> sigma; // error scale
}
model {
y ~ normal(alpha + X * beta, sigma); // likelihood
}
'

modelNoIntercept = '
data {
int<lower=0> n;
int<lower=0> d;
matrix[n,d] X; // predictor matrix
vector[n] y; // outcome vector
}
parameters {
vector[d] beta; // coefficients for predictors
real<lower=0> sigma; // error scale
}
model {
y ~ normal(X * beta, sigma); // likelihood
}
'
