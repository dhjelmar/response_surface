plot_residuals <- function(xdata, ypred, ydata, yrange=range(ypred-ydata), title='') {
    plotfit(xdata, ypred - ydata, interval='none', ylimspec=yrange, equation=FALSE, main=title)
    abline(h=0)
}
