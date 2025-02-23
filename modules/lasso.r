library(glmnet)

lasso <- function(df, yvar, alpha=1, lambda='best', plot_mse=TRUE, plot_resid=TRUE) {
    ## set alpha to choose LASSO (alpha=1) or Ridge Regression (alpha=2)

    ## identify vector location with independent variable
    yloc <- which(names(df) == yvar)

    ## define x and y for regression
    y <- df[,yloc]
    x <- data.matrix(df[,-yloc])
    
    ##perform k-fold cross-validation to find optimal lambda value
    cv_model <- cv.glmnet(x, y, alpha = alpha)

    ##find optimal lambda value that minimizes test MSE
    best_lambda <- cv_model$lambda.min
    if (lambda == 'best') lambda <- best_lambda

    ##produce plot of test MSE by lambda value
    if (plot_mse) plot(cv_model)

    ##find coefficients of best model
    model <- glmnet(x, y, alpha = alpha, lambda = lambda)
    coef <- coef(model)
    #browser()
    coef <- as.vector(coef)
    names(coef) <- c('(Intercept)', names(df[,-yloc]))

    ##use fitted best model to make predictions
    yfit <- predict(model, s = lambda, newx = x)

    ##find SST and SSE
    sst <- sum((y - mean(y))^2)
    sse <- sum((yfit - y)^2)

    ##find R-Squared
    rsq <- 1 - sse/sst
    rsq

    ## add predictions to dataframe
    results <- cbind(df, yfit)
    names(results)[names(results)=='s1'] <- 'LASSO'
    
    if (plot_resid){
        ## plot residuals
        for (param in c(names(df[,-yloc]))) {
            #with(results, plotfit(cyl, LASSO-param, interval='none', ylimspec=range(LASSO-mpg),
            #                      equation=FALSE, main='LASSO scaled'))
            #browser()
            plotfit(xx = results[,param], 
                    yy = results[,'LASSO'] - results[,yvar],
                    xlabel = param,
                    ylabel = paste('LASSO - ', yvar, sep=''),
                    interval='none', 
                    equation=FALSE, 
                    main='LASSO scaled')
            abline(h=0)
        }
    }
    
    return(list(model=model, coef=coef, lambda_used=lambda, best_lambda=best_lambda,
                results=results))
}
