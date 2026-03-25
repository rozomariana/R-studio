library(lme4)
data(grouseticks)
summary(grouseticks)
head(grouseticks)
attach(grouseticks)
hist(TICKS, col="grey", border=NA, las=1, breaks=0:90)
plot(TICKS ~ HEIGHT, las=1)
summary(fmp <- glm(TICKS ~ HEIGHT*YEAR, family=poisson))
#In this case, our residual deviance is $3000$ for $397$ degrees of freedom. The rule of thumb is that the ratio of deviance to df should be $1$, but it is $7.6$, indicating severe overdispersion. This can be done more formally, using either package AER or DHARMa:
library(AER)
dispersiontest(fmp)
#install.packages("performance", repos = "https://easystats.r-universe.dev")
#library(performace)
#library(easystats)
check_overdispersion(fmp)
#The value here is higher than $7.5$ (remember, it was a rule of thumb!), but the result is the same: substantial overdispersion. Same thing in DHARMa (where we can additionally visualise overdispersion):
#library(devtools) # assuming you have that
#devtools::install_github(repo = "DHARMa", username = "florianhartig", subdir = "DHARMa")
library(DHARMa)
sim_fmp <- simulateResiduals(fmp, refit=T)
testDispersion(sim_fmp)
plotSimulatedResiduals(sim_fmp) #aparentemente deprecated usar plot directamente.
#DHARMa works by simulating new data from the fitted model, and then comparing the observed data to those simulated (see DHARMa’s nice vignette for an introduction to the idea).

#“Fixing” overdispersion
#Quasi-families
summary(fmqp <- glm(TICKS ~ YEAR*HEIGHT, family=quasipoisson, data=grouseticks))
#You see that $\tau$ is estimated as 11.3, a value similar to those in the overdispersion tests above (as you’d expect). The main effect is the substantially larger errors for the estimates (the point estimates do not change), and hence potentially changed significances (though not here). (You can manually compute the corrected standard errors as Poisson-standard errors $\cdot \sqrt{\tau}$.) Note that because this is no maximum likelihood method (but a quasi-likelihood method), no likelihood and hence no AIC are available. No overdispersion tests can be conducted for quasi-family objects (neither in AER nor DHARMa).
#Different distribution (here: negative binomial)
library(MASS)
summary(fmnb <- glm.nb(TICKS ~ YEAR*HEIGHT, data=grouseticks))
#Already here we see that the ratio of deviance and df is near $1$ and hence probably fine. Let’s check:
check_overdispersion(fmnb) # este dice que no hay pero....
testOverdispersion(sim_fmnb) # esta deprecated pero pezca que si hay overdispersion.
sim_fmnb <- simulateResiduals(fmnb, refit=T, n=99)
plot(sim_fmnb)

#Observation-level random effects (OLRE)
#he general idea is to allow the expectation to vary more than a Poisson distribution would suggest. To do so, we multiply the Poisson-expectation with an overdispersion parameter ( larger 1), along the lines of \(Y \sim Pois(\lambda=e^{\tau} \cdot E(Y)) = Pois(\lambda=e^{\tau} \cdot e^{aX+b}),\) where expectation $E(Y)$ is the prediction from our regression. Without overdispersion, $\tau=0$. We use $e^\tau$ to force this factor to be positive.
#You may recall that the Poisson-regression uses a log-link, so we can reformulate the above formulation to \(Y \sim Pois(\lambda=e^{\tau} \cdot e^{aX+b}) = Pois(\lambda=e^{aX+b+\tau}).\) So the overdispersion multiplier at the response-scale becomes an overdispersion summand at the log-scale.
#That means, we can add another predictor to our model, one which changes with each value of Y, and which we do not really care for: a random effect. Remember that a (Gaussian) random effect has a mean of $0$ and its standard deviation is estimated from the data. How does that work? Well, if we expected a value of, say, $2$, we add noise to this value, and hence increase the range of values realised.

set.seed(1)
hist(Y1 <- rpois(1000, 2), breaks=seq(0, 30), col="grey60", freq=F, ylim=c(0, 0.45), las=1, 
     main="", xlab="Y")
hist(Y2 <- rpois(1000, 2 * exp( rnorm(1000, mean=0, sd=1))), add=T, freq=F, breaks=seq(0, 100))
legend("right", legend=c("Poisson", "overdispersed Poisson"), pch=15, col=c("grey40", "grey80"), 
       bty="n", cex=1.5)

var(Y1); var(Y2)

#We see that with an overdispersion modelled as observation-level random effect with mean$=0$ and an innocent-looking sd$=1$, we increase the spread of the distribution substantially. In this case both more $0$s and more high values, i.e. more variance altogether.
#So, in fact modelling overdispersion as OLRE is very simple: just add a random effect which is different for each observation. In our data set, the column INDEX is just a continuously varying value from 1 to $N$, which we use as random effect.

summary(fmOLRE <- glmer(TICKS ~ YEAR*HEIGHT + (1|INDEX), family=poisson, data=grouseticks))
#Oops! What’s that? So it converged (“convergence code: 0”), but apparently the algorithm is “unhappy”. Let’s follow its suggestion and scale the numeric predictor:
height <- scale(grouseticks$HEIGHT)
summary(fmOLRE <- glmer(TICKS ~ YEAR*height + (1|INDEX), family=poisson, data=grouseticks))
#In the random-effects output, we see that the standard deviation for the random effect is around 1.06, i.e. similar to what we have simulated above. The overdispersion is thus substantial. Note that the estimates for intercept, YEAR96 and YEAR97 are substantially different (as is height, but then that has been re-scaled).

#Here’s the diagnostic plot (only DHARMa):
sim_fmOLRE <- simulateResiduals(fmOLRE, refit=T, n=250)
#plotSimulatedResiduals(sim_fmOLRE)
plot(sim_fmOLRE)
testOverdispersion(sim_fmOLRE)
testZeroInflation(sim_fmOLRE)
AIC(fmp, fmnb, fmOLRE)
check_model(fmOLRE)
model_performance(fmOLRE) # puede revisar los otros
compare_performance(fmp, fmnb, fmOLRE)
test_performance(fmp, fmnb)