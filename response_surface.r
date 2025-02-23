# https://www.r-bloggers.com/2022/06/response-surface-designs-and-their-analysis-with-r/

#source('~/Documents/GitHub/R-setup/setup.r')
library(glmnet)            # LASSO and Ridge Regression
library(rsm)               # rsm wrapper on lm() regression
source('~/Documents/GitHub/R-setup/modules/plotspace.r')
source('~/Documents/GitHub/R-setup/modules/mypairs.r')
source('~/Documents/GitHub/R-setup/modules/plotfit.r')
source('~/Documents/GitHub/R-setup/modules/addfit.r')
source('~/Documents/GitHub/R-setup/modules/is.nothing.r')
source('~/Documents/GitHub/R-setup/modules/standardize.r')
r_files <- list.files('~/Documents/GitHub/response_surface/modules', pattern="*.[rR]$", full.names=TRUE)
for (f in r_files) {
  ## cat("f =",f,"\n")
  source(f)
}

# look at data
head(mtcars)
data <- mtcars[,c('cyl', 'disp', 'hp', 'wt', 'mpg')]
mypairs(data)
#ggcorplot(data)
#cor(data)

######################################################################
# scale data
df <- data
out_std <- standardize(data)
dfs <- out_std$dfs
out_std$meanvec
out_std$sdvec
mypairs(dfs)

######################################################################
## separate data into training and test sets
#use 70% of dataset as training set and 30% as test set
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]
# all data
df[ sample,'type'] <- 'train'
df[!sample,'type'] <- 'test'
# scaled
trains  <- dfs[sample, ]
tests   <- dfs[!sample, ]


######################################################################
# LASSO and Ridge Regression to get first cut at model
# https://www.statology.org/lasso-regression-in-r/
#y <- dfs$mpg
#x <- data.matrix(dfs[, c('cyl', 'disp', 'hp', 'wt')])
#y <- df$mpg
#x <- data.matrix(df[, c('cyl', 'disp', 'hp', 'wt')])

plotspace(3,2)
yvar <- 'mpg'
# regular
#train_out <- lasso(train, yvar=yvar, alpha=1, lambda='best', plot_mse=TRUE, plot_resid=TRUE)
# standardized
train_out <- lasso(trains, yvar=yvar, alpha=1, lambda='best', plot_mse=TRUE, plot_resid=TRUE)
train_out$coef
train_results <- train_out$results
b    <- train_out$coef[[1]]
coef <- train_out$coef[2:length(coef)]

used_standardized <- TRUE
if (used_standardized) {
    # need to overwrite b and coef
    bs <- b
    coefs <- coef
    out <- standardize_rev(trains, out_std$meanvec, out_std$sdvec, coef_s=train_out$coef, y=yvar)
    b <- out$b
    coef <- out$coef
}

# how do predictions look on test data?
typeloc <- which(names(df) == 'type')
xnew <- df[-typeloc]
yloc <- which(names(xnew) == yvar)
xnew <- xnew[-yloc]
LASSO <- b + as.matrix(xnew) %*% coef
results <- cbind(df, LASSO)
## plot residuals
plotspace(2,2)
train_results <- results[results$type == 'train',]
test_results  <- results[results$type == 'test',]
for (param in c(names(test[,-yloc]))) {
    #with(results, plotfit(cyl, LASSO-param, interval='none', ylimspec=range(LASSO-mpg),
    #                      equation=FALSE, main='LASSO scaled'))
    #browser()
    yrange <- range(train_results[,'LASSO'] - train_results[,yvar],
                    test_results[,'LASSO'] - test_results[,yvar])
    # plot training residuals
    plotfit(xx = train_results[,param], 
            yy = train_results[,'LASSO'] - train_results[,yvar],
            xlabel = param,
            ylabel = paste('LASSO - ', yvar, sep=''),
            ylimspec = yrange,
            interval='none', 
            equation=FALSE, 
            main='LASSO training data')
    abline(h=0)
    # plot test residuals
    plotfit(xx = test_results[,param], 
            yy = test_results[,'LASSO'] - test_results[,yvar],
            xlabel = param,
            ylabel = paste('LASSO - ', yvar, sep=''),
            ylimspec = yrange,
            interval='none', 
            equation=FALSE, 
            main='LASSO test data')
    abline(h=0)
}




    

######################################################################
# rsm package
# Use FO() in the model formula in rsm to specify a first-order response surface (i.e., a linear function)
# Use TWI() to generate two-way interactions.
# Use PQ() to generate quadratic (i.e., squared) terms.
# SO() creates all terms in FO(), TWI(), and PQ().

# fit 2nd order models for mpg = f(cyl, disp...)
model <- rsm(mpg ~ SO(cyl, disp, hp, wt), data = dfs)
# anova(model)        # does not separate parts withing FO(), TWI(), and PQ so not as useful as mysummary() with rms()
mysummary(model)                                             # p=7.458507eE-7
# if want to extract just the P value for hp
#   mysummary(model)$coef['hp',4]

# above is the same as
model <- rsm(mpg ~ FO(cyl, disp, hp, wt) + TWI(cyl, disp, hp, wt) + PQ(cyl, disp, hp, wt), data = dfs)
mysummary(model)                                             # p=7.458507eE-7

# look at P-value for the model and Pr for each parameter
# P-value is the maximum probability of getting the observed outcome by chance
# so a low P-value means the model is good (or the parameter is significant).

#--------------------------------------------------------------------
# BACKWARD SELECTION

# Remove parameter (or parameter combination) with the highest Pr and refit. 
# Keep going until all Pr <= 0.05.
# Exception: Keep a primary parameter even if PR > 0.05 if it is needed in a combination.

# drop disp
model <- rsm(mpg ~ FO(cyl, hp, wt) + TWI(cyl, hp, wt) + PQ(cyl, hp, wt), data = dfs)
mysummary(model)                                                                          # 6E-9

# drop hp^2
model <- rsm(mpg ~ FO(cyl, hp, wt) + TWI(cyl, hp, wt) + PQ(cyl, wt), data = dfs)
mysummary(model)                                                                          # 6E-9

# drop wt^2
model <- rsm(mpg ~ FO(cyl, hp, wt) + TWI(cyl, hp, wt) + PQ(cyl, hp), data = dfs)
mysummary(model)                                                                          # 1.16E-9

# drop hp^2
model <- rsm(mpg ~ FO(cyl, hp, wt) + TWI(cyl, hp, wt) + PQ(cyl), data = dfs)
mysummary(model)                                                                          # 2E-10

# drop hp:wt
model <- rsm(mpg ~ FO(cyl, hp, wt) + TWI(formula = ~cyl:(hp+wt)) + PQ(cyl), data = dfs)
model <- lm(mpg ~ cyl + hp + wt + cyl:hp + cyl:wt + I(cyl*cyl), data = dfs)
mysummary(model)                                                                          # 3.4E-11

# drop cyl^2
model <- rsm(mpg ~ FO(cyl, hp, wt) + TWI(formula = ~cyl:(hp+wt)), data = dfs)
mysummary(model)                                                                          # 6.8E-12

# drop cyl:wt
model_cyl_hp_wt_cylxhp <- rsm(mpg ~ FO(cyl, hp, wt) + cyl:hp, data = dfs)
mysummary(model_cyl_hp_wt_cylxhp)                                                       # 5.07E-12
#   mpg = b + c1 * cyl + c2 * hp + c3 * wt + c4 * cyl * hp

#--------------------------------------------------------------------
# FORWARD SELECTION (detailed steps not included)
# instead when built model from simple to complex, ended with a different model
# probably because I did not try every option each time when looking to expand by 1 more parameter.
# This highlights how reverse (i.e., above) process is more efficient.
# Results in only 2 parameters in the model which is nicer but potentially missing important other parameters.
# all of the following result in identical fits
model_hp_wt_wt2 <- rsm(mpg ~ FO(hp, wt) + I(wt*wt), data = dfs)
model_hp_wt_wt2 <- rsm(mpg ~ FO(hp, wt) + PQ(wt), data = dfs)       # p=1.309E-12 best overall
model_hp_wt_wt2 <- lm(mpg ~ hp + wt + I(wt*wt), data = dfs)
mysummary(model_hp_wt_wt2)
#   mpg = b + c1 * hp + c2 * wt + c3 * wt^2
#--------------------------------------------------------------------

######################################################################
# unscale model results









######################################################################

# send summary results to txt file
export = FALSE
if (export){
    capture.output(
        summary(model),            # Object to be exported
        file = "model_summary.txt" # File name 
    )
}

######################################################################
# plot the model response against the 4 parameters
plotspace(2,2)
model = model_cyl_hp_wt_cylxhp
contour(
    model,                  # Our model
    ~ hp + wt + cyl,        # A formula to obtain graphs of all 3 combinations 
    image = TRUE,           # If image = TRUE, apply color to each contour
)

# 3D view of the same thing
plotspace(2,2)
persp(
    model,            # Our model 
    ~ hp + wt + wt + cyl,        # A formula to obtain graphs of all 3 combinations 
    col = topo.colors(100), # Color palette
    contours = "colors"     # Include contours with the same color palette
) 

#--------------------------------------------------------------------

# best overall models

# forward model
mpg_hp_wt_wt2 <- function(hp, wt, model=model_hp_wt_wt2) {
    # coefficients
    intercept <- coef(model)[[1]]
    c1 <- coef(model)[[2]]
    c2 <- coef(model)[[3]]
    c3 <- coef(model)[[4]]
    # fit
    mpg <- intercept + c1 * hp + c2 * wt + c3 * wt * wt
    return(mpg)
}

mpg_hp_wt_wt2(100,3)
# reverse model
mpg_cyl_hp_wt_cylxhp <- function(cyl, hp, wt, model=model_cyl_hp_wt_cylxhp) {
    # coefficients
    intercept <- coef(model)[[1]]
    c1 <- coef(model)[[2]]
    c2 <- coef(model)[[3]]
    c3 <- coef(model)[[4]]
    c4 <- coef(model)[[5]]
    # fit
    mpg <- intercept + c1 * cyl + c2 * hp + c3 * wt + c4 * cyl * hp
    return(mpg)
}
mpg_cyl_hp_wt_cylxhp(6, 100,3)

#--------------------------------------------------------------------

# add redictions to data
data$pred_forward <- mpg_hp_wt_wt2(data$hp, data$wt)
data$pred_reverse <- mpg_cyl_hp_wt_cylxhp(data$cyl, data$hp, data$wt)

#--------------------------------------------------------------------

# look at residuals
plotspace(3,2)

yrange <- with(data, range(pred_forward - mpg, pred_reverse - mpg))
with(data, plotfit(cyl, pred_forward - mpg, interval='none', ylimspec=yrange,
                   equation=FALSE,
                   main='forward model: hp + wt + wt^2'))
abline(h=0)
with(data, plotfit(cyl, pred_reverse - mpg, interval='none', ylimspec=yrange,
                   equation=FALSE,
                   main='reverse model: cyl + hp + wt + cyl*hp'))
abline(h=0)

yrange <- with(data, range(pred_forward - mpg, pred_reverse - mpg))
with(data, plotfit(hp, pred_forward - mpg, interval='none', ylimspec=yrange,
                   equation=FALSE,
                   main='forward model: hp + wt + wt^2'))
abline(h=0)
with(data, plotfit(hp, pred_reverse - mpg, interval='none', ylimspec=yrange,
                   equation=FALSE,
                   main='reverse model: cyl + hp + wt + cyl*hp'))
abline(h=0)

yrange <- with(data, range(pred_forward - mpg, pred_reverse - mpg))
with(data, plotfit(wt, pred_forward - mpg, interval='none', ylimspec=yrange,
                   equation=FALSE,
                   main='forward model: hp + wt + wt^2'))
abline(h=0)
with(data, plotfit(wt, pred_reverse - mpg, interval='none', ylimspec=yrange,
                   equation=FALSE,
                   main='reverse model: cyl + hp + wt + cyl*hp'))
abline(h=0)

#--------------------------------------------------------------------

# interactive 3D plot with data for 1st model
library(rgl)
options(rgl.printRglwidget = TRUE) 
# add points
with(data, rgl::plot3d(x=hp, y=wt, z=mpg, col='red', size='5'))
# Add 3D surface plot 
x <- seq(min(data$hp), max(data$hp), length.out = 100)
y <- seq(min(data$wt), max(data$wt), length.out = 100)
z <- outer(x, y, function(x, y) mpg_hp_wt_wt2(x, y)) 
rgl::persp3d(x, y, z, col = "blue", alpha=0.5, add=TRUE) 

# the following shows the points from this model which fall exactly on the surface
# since there are only these 2 parameters in the model
rgl::plot3d(data$hp, data$wt, data$pred_forward, col='blue', size=8, add=TRUE)

# add the other model
#z <- outer(x, y, function(x, y) mpg_cyl_hp_wt_cylxhp(cyl=4, x, y)) 
#persp3d(x, y, z, col = "green", alpha=0.5, add=TRUE) 
#z <- outer(x, y, function(x, y) mpg_cyl_hp_wt_cylxhp(cyl=8, x, y)) 
#persp3d(x, y, z, col = "black", alpha=0.5, add=TRUE) 

# 2nd model looks bad because some data do not have cyl=4
# Instead plot predictions from higher order model
rgl::plot3d(data$hp, data$wt, data$pred_reverse, col='green', size=8, add=TRUE)

# add legend  <-- legend is very fuzzy
#legend3d("topright", 
#         pch = 16, cex=0.8,
#         legend = c('data', 'mpg_hp_wt_wt2', 'mpg_cyl_hp_wt_cylxhp'), 
#         col = c('red', 'blue', 'green'))

# capture snapshot
# snapshot3d(filename = '3dplot.png', fmt = 'png')

