library(survival)
library(rms)

data(package = "survival")
# We're gonna use the example data set for colon cancer
head(colon)

## Add survival object
lung$SurvObj <- with(lung, Surv(time, status == 2))

## Check data
head(lung)

## Kaplan-Meier estimator. The "log-log" confidence interval is preferred.
km.as.one <- survfit(SurvObj ~ 1, data = lung, conf.type = "log-log")
km.by.sex <- survfit(SurvObj ~ sex, data = lung, conf.type = "log-log")

## Show object
km.as.one

## See survival estimates at given time (lots of outputs)
## summary(km.as.one)
## summary(km.by.sex)

## Plotting without any specification
rms::survplot(SurvObj ~ sex, data = lung, conf.type = "log-log")
survdiff(SurvObj ~ group, data=survival, rho=0)
?survplot

plot(km.by.group, lty=1:3, axes=FALSE, xlim=c(0,100), ylim=c(0,1.05), xaxs="r",yaxs="r")
box(bty="L")
axis(1, labels=FALSE, tick=TRUE)
axis(2, labels=TRUE)
mtext("Proportion surviving", side=2, line=3)
text(85,1.04,"control")
text(66, 0.29, "inoculated")
legend("topleft", "a", bty="n")
x=c(0,20,40,60,80,98)
y=c(1.01,1.01,1.01,1.01,1.01,1.01)
lines(x,y,lty=3)