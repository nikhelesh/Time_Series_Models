# Time_Series_Models Using Rstudio
Different time series model forecast
ARIMA, Auto-Arima, SARIMA & HybridForecast

hybridModel <- function(y, models = "aefnst",
                        lambda = NULL,
                        a.args = NULL,
                        e.args = NULL,
                        n.args = NULL,
                        s.args = NULL,
                        t.args = NULL,
                        z.args = NULL,
                        weights = c("equal", "insample.errors", "cv.errors"),
                        errorMethod = c("RMSE", "MAE", "MASE"),
                        rolling = FALSE,
                        cvHorizon = frequency(y),
                        windowSize = 84,
                        horizonAverage = FALSE,
                        parallel = FALSE, num.cores = 2L,
                        verbose = TRUE)
                      
model used are AUTO.ARIMA, ETS , NNETAR, STLM , TBATS, SNAIVE
