# https://www.r-bloggers.com/2022/06/response-surface-designs-and-their-analysis-with-r/

source('/home/david/Documents/GitHub/R-setup/setup.r')

head(mtcars)

# first a couple functions
pvalue <- function (modelobject) {
    # returns pvalue of model object
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
}
mysummary <- function(modelobject) {
    # returns coefficients and pvalue of model object
    coef <- summary(modelobject)$coefficients
    pvalue <- pvalue(modelobject)
    return(list(coef=coef, pvalue=pvalue))
}

# rsm package
# Use FO() in the model formula in rsm to specify a first-order response surface (i.e., a linear function)
# Use TWI() to generate two-way interactions.
# Use PQ() to generate quadratic (i.e., squared) terms.
# SO() creates all terms in FO(), TWI(), and PQ().

# fit 2nd order models for mpg = f(cyl, disp...)
model <- rsm(mpg ~ SO(cyl, disp, hp, wt), data = mtcars)
mysummary(model)                                             # p=7.458507eE-7
# if want to extract just the P value for hp
#   mysummary(model)$coef['hp',4]

# above is the same as
model <- rsm(mpg ~ FO(cyl, disp, hp, wt) + TWI(cyl, disp, hp, wt) + PQ(cyl, disp, hp, wt), data = mtcars)
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
model <- rsm(mpg ~ FO(cyl, hp, wt) + TWI(cyl, hp, wt) + PQ(cyl, hp, wt), data = mtcars)
mysummary(model)                                                                          # 6E-9

# drop hp^2
model <- rsm(mpg ~ FO(cyl, hp, wt) + TWI(cyl, hp, wt) + PQ(cyl, wt), data = mtcars)
mysummary(model)                                                                          # 6E-9

# drop wt^2
model <- rsm(mpg ~ FO(cyl, hp, wt) + TWI(cyl, hp, wt) + PQ(cyl, hp), data = mtcars)
mysummary(model)                                                                          # 1.16E-9

# drop hp^2
model <- rsm(mpg ~ FO(cyl, hp, wt) + TWI(cyl, hp, wt) + PQ(cyl), data = mtcars)
mysummary(model)                                                                          # 2E-10

# drop hp:wt
model <- rsm(mpg ~ FO(cyl, hp, wt) + TWI(formula = ~cyl:(hp+wt)) + PQ(cyl), data = mtcars)
mysummary(model)                                                                          # 3.4E-11

# drop cyl^2
model <- rsm(mpg ~ FO(cyl, hp, wt) + TWI(formula = ~cyl:(hp+wt)), data = mtcars)
mysummary(model)                                                                          # 6.8E-12

# drop cyl:wt
model_cyl_hp_wt_cylxhp <- rsm(mpg ~ FO(cyl, hp, wt) + cyl:hp, data = mtcars)
mysummary(model_cyl_hp_wt_cylxhp)                                                       # 5.07E-12
#   mpg = b + c1 * cyl + c2 * hp + c3 * wt + c4 * cyl * hp

#--------------------------------------------------------------------
# FORWARD SELECTION (detailed steps not included)
# instead when built model from simple to complex, ended with a different model
# only 2 parameters which is nicer but possibly missing important other parameters?
model_hp_wt_wt2 <- rsm(mpg ~ FO(hp, wt, wt2), data = mymtcars)         # p=1.309E-12 best overall
mysummary(model_hp_wt_wt2)
#   mpg = b + c1 * hp + c2 * wt + c3 * wt^2
#--------------------------------------------------------------------

# send summary results to txt file
export = FALSE
if (export){
    capture.output(
        summary(model),            # Object to be exported
        file = "model_summary.txt" # File name 
    )
}

# plot the model response against the 4 parameters
plotspace(2,2)
model = model_hp_wt_wt2
contour(
    model,                  # Our model
    ~ hp + wt + wt2,        # A formula to obtain graphs of all 3 combinations 
    image = TRUE,           # If image = TRUE, apply color to each contour
)

# 3D view of the same thing
plotspace(2,2)
persp(
    model,            # Our model 
    ~ hp + wt + wt2,        # A formula to obtain graphs of all 3 combinations 
    col = topo.colors(100), # Color palette
    contours = "colors"     # Include contours with the same color palette
) 
plotspace(1,1)
persp(
    model,            # Our model 
    ~ hp + wt,        # A formula to obtain graphs of all 3 combinations 
    col = topo.colors(100), # Color palette
    contours = "colors"     # Include contours with the same color palette
) 

#--------------------------------------------------------------------

# best overall models
mpg_hp_wt_wt2 <- function(hp, wt, model=model_hp_wt_wt2) {
    # coefficients
    intercept <- coef(model)[[1]]
    c1 <- coef(model)[[2]]
    c2 <- coef(model)[[3]]
    c3 <- coef(model)[[4]]
    # fit
    mpg <- intercept + c1 * hp + c2 * wt + c3 * wt^2
}
mpg_cyl_hp_wt_cylxhp <- function(cyl, hp, wt, model=model_cyl_hp_wt_cylxhp) {
    # coefficients
    intercept <- coef(model)[[1]]
    c1 <- coef(model)[[2]]
    c2 <- coef(model)[[3]]
    c3 <- coef(model)[[4]]
    c4 <- coef(model)[[5]]
    # fit
    mpg <- intercept + c1 * cyl + c2 * hp + c3 * wt + c4 * cyl * hp
}

# interactive 3D plot with data for 1st model
library(rgl)
options(rgl.printRglwidget = TRUE) 
# add points
data <- mtcars
with(data, rgl::plot3d(x=hp, y=wt, z=mpg, col='red', size='5', main='model: hp + wt + wt^2'))
# Create a 3D surface plot 
x <- seq(min(data$hp), max(data$hp), length.out = 100)
y <- seq(min(data$wt), max(data$wt), length.out = 100)
z <- outer(x, y, function(x, y) mpg_hp_wt_wt2(x, y)) 
persp3d(x, y, z, col = "blue", alpha=0.5, add=TRUE) 

# the following shows the points from this model which fall exactly on the surface
# since there are only these 2 parameters in the model
data$pred <- mpg_hp_wt_wt2(data$hp, data$wt)
rgl::plot3d(data$hp, data$wt, data$pred, col='blue', size=8, add=TRUE)

# add the other model
#z <- outer(x, y, function(x, y) mpg_cyl_hp_wt_cylxhp(cyl=4, x, y)) 
#persp3d(x, y, z, col = "green", alpha=0.5, add=TRUE) 
#z <- outer(x, y, function(x, y) mpg_cyl_hp_wt_cylxhp(cyl=8, x, y)) 
#persp3d(x, y, z, col = "black", alpha=0.5, add=TRUE) 

# 2nd model looks bad because some data do not have cyl=4
# Instead plot predictions from higher order model
data$pred <- mpg_cyl_hp_wt_cylxhp(data$cyl, data$hp, data$wt)
rgl::plot3d(data$hp, data$wt, data$pred, col='green', size=8, add=TRUE)
