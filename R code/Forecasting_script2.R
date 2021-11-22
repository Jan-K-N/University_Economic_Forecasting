####################################################################
#     NOTES BEFORE RUNNING THE SCRIPT:                             #
#     - 1) Please install the required packages.                   #
#      -2) Work directionary should be changed at line 15.         #
#              Can be done using the command: setwd("Path")        #
####################################################################

# Run time for the whole script: around 25 min.

## Basics: 
  rm(list = ls())
  cat("\f")

# Please set local path here:
  wd <- setwd("/Users/Jan/Desktop/Test_mappe") # <---
  
## Load liberaries: 
  library(quantmod) # Used to load/harvest data from Yahoo-Finance.
  library(xts) # Used to handle xts objects.
  library(ggplot2) # Used to plot.
  library(scales) # Used in connection with ggplot2.
  library(gtrendsR) # Used to scrape google trends data.
  library(forecast) # Used to season adjust.
  library(zoo) # Used to handle zoo objects.
  library(moments) # Used to compute moments.
  library(foreach) # Used to make for-loops.
  library(magrittr) # Used to simplify code.
  library(dplyr) # Used to produce lagged values.
  library(leaps) # Used to best subset.
  library(dynlm)
  library(ggpubr) # Used in combination with ggplot. 
  library(caret)
  library(xtable) # Used to make latex tables.
  library(gam)
  library(ForecastComb)
  library(aTSA) # ARCH test
  library(tseries) # Normality test of the errors. 
  
############################################################################
############################# Data: ########################################
############################################################################

############################# Stock data: 

# Setup: 
  #data_start  = "1900-01-01"
  #data_end    = "2021-04-01"
  #data_source = "yahoo"
  #stock_enviroment <- new.env() # Store data in enviroment in order to ease future loops etc.

# Now I use yahoo-finance to download stock data:
  #S_P500 <- getSymbols("^GSPC",
                       #src  = data_source,
                       #env  = stock_enviroment, 
                       #from = data_start,
                       #to   = data_end,
                       #periodicity = "monthly",
                       #auto.assign = FALSE) # Set auto.assign to FALSE to store results.
  
# Now store the closing data (adjusted)
  #S_P500_close <- S_P500$GSPC.Adjusted
  #S_P500_close <- as.data.frame(S_P500_close) # Convert to dataframe.

# Save the closing data in a csv-file:  
  #write.csv(S_P500_close,"/Users/Jan/Desktop/Økonomi/Kandidat/2. Semester/Forecasting/Project/Data/S&P500_close.csv") -> own path.
  #write.csv(S_P500_close, "S&P500_close.csv")
  
# Load the csv-file: 
  #S_P500_close <- read.csv("/Users/Jan/Desktop/Økonomi/Kandidat/2. Semester/Forecasting/Project/Data/S&P500_close.csv") -> own path.
  S_P500_close <- read.csv("S&P500_close.csv")
  
  colnames(S_P500_close) <- c("DATE","SP500 Closing")
############################# Google trends data: 
  
# The function below downloads the Google Trends data:
  
# Create function to download google-trends. Utilize the gtrendsR-package:
  #g_trends_search <- function(terms_to_search){
    # Prelocate some space:
    #mG_trends <- list()
    #mNames <- terms_to_search
    #for(i in 1:length(terms_to_search)){
      #mG_trends[[i]] <- gtrends(keyword = terms_to_search[i], geo = "US",time = "all",
                                #gprop = c("web"))
    #}
    # New loop:
    #n_row <- dim(mG_trends[[1]][["interest_over_time"]])[1]
    #mG_trends_2 <- data.frame(matrix(data = NA,ncol = length(terms_to_search),nrow = n_row))
    #Date <- mG_trends[[1]][["interest_over_time"]][["date"]]
    #mG_trends_2 <- cbind(Date,mG_trends_2)
    #n_col <- ncol(mG_trends_2)
    #mG_trends_2$Date <- as.Date(mG_trends_2$Date, format = "%m/%d/%Y" )
    #for(j in 1:length(mG_trends)){
      #mG_trends_2[,j+1] <- mG_trends[[j]][["interest_over_time"]][["hits"]]
    #}
    # Now name colums:
    #colnames(mG_trends_2) <- c("Date",mNames)
    
    # Specify what to return: 
    #return(mG_trends_2)
    
  #}

# Use the function to download google trend data:   
  #mGT <- g_trends_search(terms_to_search = c("GOLD PRICES","RECESSION","GOLD PRICE", "DEPRESSION",
                                             #"GREAT DEPRESSION","GOLD","ECONOMY","PRICE OF GOLD",
                                             #"THE DEPRESSION","CRISIS","FRUGAL","GDP",
                                             #"CHARITY","BANKRUPTCY","UNEMPLOYMENT","INFLATION RATE",
                                             #"BANKRUPT","THE GREAT DEPRESSION","CAR DONATE","CAPITALIZATION",
                                             #"EXPENSE","DONATION","SAVINGS","SOCIAL SECURITY CARD",
                                             #"THE CRISIS", "DEFAULT","BENEFITS","UNEMPLOYED",
                                             #"POVERTY","SOCIAL SECURITY OFFICE",
                                             #"Cash","Bubble","Return","Stocks",
                                             #"Gain","Transaction","Dividend","Revenue",
                                             #"War","Society"))
# Save the google trends data in a CSV: 
  #write.csv(mGT,"/Users/Jan/Desktop/Økonomi/Kandidat/2. Semester/Forecasting/Project/Data/Google_trends_data.csv", row.names = FALSE) -> own path.
  #write.csv(mGT, "Google_trends_data.csv")
  
# Load the csv-file: 
  #mGT <- read.csv("/Users/Jan/Desktop/Økonomi/Kandidat/2. Semester/Forecasting/Project/Data/Google_trends_data.csv") -> own path.
  mGT <- read.csv("Google_trends_data.csv")
  
# To account for the high volatility in the Google trends data, I convert it the data using the natural logaritm.
# (This is following Da, Engelberg, and Gao (2011)): 
  mGT_log <- data.frame(matrix(data = NA,ncol = dim(mGT)[2] ,nrow = dim(mGT)[1]))
  mGT_log[,1] <- mGT[,1]
  for(j in 2:ncol(mGT_log)){
    mGT_log[,j] <- log(mGT[,j])
  }
  colnames(mGT_log) <- colnames(mGT)
  mGT_log$Date <- as.Date(as.character(mGT_log$Date), format = "%Y-%m-%d") # Convert to date-format.
  
############################################################################
############################# Construct y-variable (target variable): ######
############################################################################

# Open loop to construct the monthly return:
  r_t <- data.frame(matrix(data = NA,nrow = dim(S_P500_close)[1],ncol = 2))
  r_t[,1] <- as.matrix(S_P500_close[,1])
  colnames(r_t) <- c("Date","Return_SandP500")
  for(i in 2:nrow(S_P500_close)){
    r_t[i,2] <- log(S_P500_close[i,2]/S_P500_close[i-1,2])*100 
  }
# Now drop all data until 2004:
  r_t <- r_t[-c(1:228,433:437),]
  rownames(r_t) <- seq(1:nrow(r_t)) # Adjust rownames.
# Finally, convert Date to date-format & data.frame.
  r_t <- as.data.frame(r_t)
  #r_t[,2] <- as.numeric(r_t[,2])
  r_t$Date <- as.Date(as.character(r_t$Date), format = "%Y-%m-%d")
  

############################################################################
############################# Stationarity and seasonality: ################
############################################################################

############################# Functions:
  
# The function below is an expansion of the Augmented Dickey Fuller test, were I allow for quadratic terms:
  ADF_expanded_2 <- function(x, type,k1,selectlags,...){
    # type = 1: lag no intercept 
    # type = 2: drift 
    # type = 3: linear trend
    # type = 4: quadratic trend
    # Selectlags = 1: AIC
    # Selectlags = 2: BIC
    
    k1 <- k1 + 1
    x <- as.vector(x, mode = "double")
    y <- diff(x)
    n <- length(y)
    z <- embed(y, k1)
    yt <- z[, 1]
    xt1 <- x[k1:n] # lagged version.
    tt <- k1:n
    tt2 <- tt^2
    STAT <- matrix(data = NA, nrow = 1, ncol = 2)
    STAT2 <- list()
    if(k1 > 1){
      if(selectlags == 1){
        critRes <- rep(NA,k1)
        for(i in 2:k1){
          yt1 <- z[, 2:i] # lagged version
          if(type == 1){
            res <- lm(yt ~ xt1 - 1 + yt1) # Write - 1 to exclude the intercept
          }
          if(type == 2){
            res <- lm(yt ~ xt1 +  1 + yt1) 
          }
          if(type == 3){
            res <- lm(yt ~ xt1 + 1 + tt + yt1)
          }
          if(type == 4){
            res <- lm(yt ~ xt1 + 1 + tt + tt2 + yt1)
          }
          critRes[i] <- AIC(res,k = 2)  
        }
        k1 <- which.min(critRes)
        STAT2[[3]] <- k1
      }
      # Now BIC:
      if(selectlags == 2){
        critRes <- rep(NA,k1)
        for(i in 2:k1){
          yt1 <- z[, 2:i] # lagged version
          
          if(type == 1){
            res <- lm(yt ~ xt1 - 1 + yt1) # Write - 1 to exclude the intercept
          }
          if(type == 2){
            res <- lm(yt ~ xt1 +  1 + yt1) 
          }
          if(type == 3){
            res <- lm(yt ~ xt1 + 1 + tt + yt1)
          }
          if(type == 4){
            res <- lm(yt ~ xt1 + 1 + tt + tt2 + yt1)
          }
          critRes[i] <- AIC(res,k = log(length(yt)))  
        }
        k1 <- which.min(critRes)
        STAT2[[3]] <- k1
      }
      yt1 <- z[,2:k1]
      if(type == 1){
        res1 <- lm(yt ~ xt1 - 1 + yt1) # Write - 1 to exclude the intercept
        res1_residuals <- resid(res1)
        res1_sum <- summary(res1)
        STAT[1,1] <- res1_sum$coefficients[2, 1]/res1_sum$coefficients[2,2]
        STAT[1,2] <- res1_sum$coefficients[2, 4]
        colnames(STAT) <- c("T-stat","p-value")
        STAT2[[1]] <- STAT
        STAT2[[2]] <- res1_residuals
        return(STAT2)
      }
      if(type == 2){
        res1 <- lm(yt ~ xt1 +  1 + yt1)
        res1_residuals <- resid(res1)
        res1_sum <- summary(res1)
        STAT[1,1] <- res1_sum$coefficients[2, 1]/res1_sum$coefficients[2,2]
        STAT[1,2] <- res1_sum$coefficients[2, 4]
        colnames(STAT) <- c("T-stat","p-value")
        STAT2[[1]] <- STAT
        STAT2[[2]] <- res1_residuals
        return(STAT2)
      }
      if(type == 3){
        res1 <- lm(yt ~ xt1 + 1 + tt + yt1)
        res1_residuals <- resid(res1)
        res1_sum <- summary(res1)
        STAT[1,1] <- res1_sum$coefficients[2, 1]/res1_sum$coefficients[2,2]
        STAT[1,2] <- res1_sum$coefficients[2, 4]
        colnames(STAT) <- c("T-stat","p-value")
        STAT2[[1]] <- STAT
        STAT2[[2]] <- res1_residuals
        return(STAT2)
      }
      if(type == 4){
        res1 <- lm(yt ~ xt1 + 1 + tt + tt2 + yt1)
        res1_residuals <- resid(res1)
        res1_sum <- summary(res1)
        STAT[1,1] <- res1_sum$coefficients[2, 1]/res1_sum$coefficients[2,2]
        STAT[1,2] <- res1_sum$coefficients[2, 4]
        colnames(STAT) <- c("T-stat","p-value")
        STAT2[[1]] <- STAT
        STAT2[[2]] <- res1_residuals
        return(STAT2)
      }
    }
    else {
      if(type == 1){
        res <- lm(yt ~ xt1 - 1 + tt)
        res_residuals <- resid(res)
        res_sum <- summary(res)
        STAT[1,1] <- res_sum$coefficients[2, 1]/res_sum$coefficients[2,2]
        STAT[1,2] <- res_sum$coefficients[2, 4]
        colnames(STAT) <- c("T-stat","p-value")
        STAT2[[1]] <- STAT
        STAT2[[2]] <- res_residuals
        return(STAT2)
        
      }
      if(type == 2){
        res <- lm(yt ~ xt1 +  1 )
        res_residuals <- resid(res)
        res_sum <- summary(res)
        STAT[1,1] <- res_sum$coefficients[2, 1]/res_sum$coefficients[2,2]
        STAT[1,2] <- res_sum$coefficients[2, 4]
        colnames(STAT) <- c("T-stat","p-value")
        STAT2[[1]] <- STAT
        STAT2[[2]] <- res_residuals
        return(STAT2)
      }
      if(type == 3){
        res <- lm(yt ~ xt1 + 1 + tt )
        res_residuals <- resid(res)
        res_sum <- summary(res)
        STAT[1,1] <- res_sum$coefficients[2, 1]/res_sum$coefficients[2,2]
        STAT[1,2] <- res_sum$coefficients[2, 4]
        colnames(STAT) <- c("T-stat","p-value")
        STAT2[[1]] <- STAT
        STAT2[[2]] <- res_residuals
        return(STAT2)
      }
      if(type == 4){
        res <- lm(yt ~ xt1 + 1 + tt + tt2 )
        res_residuals <- resid(res)
        res_sum <- summary(res)
        STAT[1,1] <- res_sum$coefficients[2, 1]/res_sum$coefficients[2,2]
        STAT[1,2] <- res_sum$coefficients[2, 4]
        colnames(STAT) <- c("T-stat","p-value")
        STAT2[[1]] <- STAT
        STAT2[[2]] <- res_residuals
        return(STAT2)
      }
      
    } 
    
  }

# The function below can deseaonalize data (data needs to be ts-format):
  deseason_function <- function(df){
    # Step 1: De-compose the ts. 
    # This is done using the stl function from the forecast package.
    # Step 2: Adjust the series. 
    # This is done using seasadj from the forecast package.
    
    # Get some information about the df:
    n_row <- nrow(df)
    n_col <- ncol(df)
    # Now open a loop which performs the deseaonlizing:
    for(j in 2:n_col){
      ts_stl <- stl(df[,j],"periodic") # Decompose the series.
      ts_sa <- seasadj(ts_stl) # Do the actual de-seasonalizing.
      
      # Now I am ready to insert back to the df:
      df[,j] <- ts_sa 
      
    }
    # Finally, I specify what the function should return:
    return(df)
    
  }


  stationarity_function_2 <- function(df,p, k1, selectlags){
    # Inputs: 
    # df -> data to be considered. 
    # p <- p-value to consider.
    # IC <- Informations criteria.
    
    # Information: 
    n_row <- nrow(df)
    n_col <- ncol(df)
    test <- list()
    df1 <- matrix(data = NA,ncol = n_col,nrow = n_row)
    df2 <- matrix(data = NA,ncol = n_col,nrow = n_row)
    for(j in 2:n_col){
      # Make y-variable:
      n <- n_row # Number of rows. 
      tt <- 1:n # Linear trend.
      tt_2 <- tt^2 # Quadratic trend 
      yt <- df[, j]
      
      # Run test with constant term:
      test <- ADF_expanded_2(x = df[,j],type = 2, k1 = k1,
                             selectlags = selectlags)
      # Check p-value:
      if(test[[1]][,2] > p){
        # We have failed to reject, so now run a test with a constant and a linear trend. 
        test <- ADF_expanded_2(x = df[,j],type = 3, k1 = k1,
                               selectlags = selectlags)
        if(test[[1]][,2] > p){
          # Final test: constant, linear trend and quadratic trend.
          test <- ADF_expanded_2(x = df[,j],type = 4, k1 = k1,
                                 selectlags = selectlags)
          if(test[[1]][,2] > p){
            # We have failed to reject -> take first difference:
            for(i in 2:n_row){
              df2[i,j] <- df[i,j] - df[i-1,j]
            }
          }
          else if(test[[1]][,2] < p){
            # Quadratic transformation: 
            res <- lm(yt ~ tt + tt_2)
            df2[,j] <- res$residuals

          }
        }
        else if(test[[1]][,2] < p){
          # Linear transformation:
          res <- lm(yt ~ tt)
          df2[,j] <- res$residuals

        }
      }
      else if(test[[1]][,2] < p){
        df2[,j] <- df[,j] # No transformation.
      }
      
    }
    df2 <- as.data.frame(df2)
    df2[,1] <- as.Date(time(df[,1]), format = "%m/%d/%Y")
    colnames(df2) <- colnames(df)
    return(df2)
  }
  
  demean_and_standardize <- function(df){
    # Input: 
      # df: A dataframe, with 1 colum being the date.
    n_row <- nrow(df)
    n_col <- ncol(df)
    for(j in 2:n_col){
      df[,j] <- (df[,j]-mean(df[,j],na.rm = TRUE))/sd(df[,j],na.rm = TRUE)
    }
  return(df)
  }
  
############################# Google trends data:

# First transform the data to time-series data. This is done using the ts function:  
  mGT_logts <- ts(mGT_log,start = c(2004,1,1), end = c(2021,3,1),frequency = 12)
  
# Now, I am ready to deseonalize
  mGT_logts <- deseason_function(mGT_logts)

###### Before we move on, we make a plot of how te deseason_function has worked:

# Specify what to look at: 
  series_to_look_at = 10

# Transform:
  mGT_logts_before <- ts(mGT_log,start = c(2004,1,1), end = c(2021,3,1),frequency = 12)
  mGT_logts_before <- mGT_logts_before[-c(205:207),]
  mGT_logts_before <- as.data.frame(mGT_logts_before)
  
  mGT_logts_after <- deseason_function(mGT_logts)
  mGT_logts_after <- mGT_logts_after[-c(205:207),]
  mGT_logts_after <- as.data.frame(mGT_logts_after)
  
# Bind:
  example_df <- cbind(mGT_logts_before[,1],
                      mGT_logts_before[,series_to_look_at],
                      mGT_logts_after[,series_to_look_at])
  example_df <- as.data.frame(example_df)
  colnames(example_df) <- c("Date","Before","After")
  example_df$Date <- as.Date(example_df$Date, format = "%Y-%m-%d")
  
# Plot: 
  example_df_plot <- ggplot(data = example_df, aes(x = Date, y = example_df[,2], colour = "The Depression")) +
                     geom_line() +
                     geom_line(data = example_df, aes(x = Date, y =example_df[,3], colour = "Deseasonalized" ))  + 
                     scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")) + 
                     labs(y = NULL)
  example_df_plot
  
###### Now move on.
  
  
# I now run the ADF function to do the sequential testing. To do this, I write a function: 
  mGT_log_transformed_ts <- stationarity_function_2(mGT_logts,p = 0.01,k1 = 12,selectlags = 2) # We use BIC
  mGT_log_transformed_ts <- mGT_log_transformed_ts[-c(205:207),]

# Finally, I demean and standardize all variables:
  mGT_log_transformed_ts <- demean_and_standardize(mGT_log_transformed_ts)

#############################  PLOT EXAMPLES OF GT-SERIES BEFORE AND AFTER USING THE ADF_EXPANDED_FUNCTION:
# In order to apply ggplot, the following codeblock should be run:
  mGT_logts <- mGT_logts[-c(205:207),]
  mGT_logts <- as.data.frame(mGT_logts)
  mGT_logts <- cbind(mGT_log[-c(205:207),1],mGT_logts)
  colnames(mGT_logts)[1] <- c("Date")
  mGT_logts <- mGT_logts[,-c(2)]
  
  # ECONOMY has been first differenced:
    # Before transformation:
    p1 <- ggplot(data = mGT_logts, aes(x = Date, y = mGT_logts[,8])) +
        ylim(3.5,4.5) +
        scale_x_date(labels = date_format("%Y"),breaks = date_breaks("2 years")) +
        labs(x = NULL, y = NULL) + 
        ggtitle("ECONOMY") + 
        theme(plot.title = element_text(hjust = 0.5)) +
        geom_line()
    p1
    
    # After transformation:
    p2 <- ggplot(data = mGT_log_transformed_ts, aes(x = Date, y = mGT_log_transformed_ts[,8])) +
          ylim(-5,5) + 
          scale_x_date(labels = date_format("%Y"),breaks = date_breaks("2 years")) +
          labs(x = NULL, y = NULL) +
          ggtitle("ECONOMY (first-differenced)") +
          theme(plot.title = element_text(hjust = 0.5)) +
          geom_line()
    p2
  
  # SOCIAL SECURITY CARD has been transformed using a linear trend:
    # Before transformation:
    p3 <- ggplot(data = mGT_logts, aes(x = Date, y = mGT_logts[,25])) +
          ylim(3.8,4.8) +
          scale_x_date(labels = date_format("%Y"),breaks = date_breaks("2 years")) +
          labs(x = NULL,y = NULL) + 
          geom_smooth(method = "lm", se = FALSE) +
          ggtitle("SOCIAL SECURITY CARD") + # Title to the plot.
          theme(plot.title = element_text(hjust = 0.5)) + # Adjust the title to the center of the figure.
          geom_line()
    p3
    
    # SOCIAL SECURITY CARD after transformation:
    p4 <- ggplot(data = mGT_log_transformed_ts, aes(x = Date, y = mGT_log_transformed_ts[,25])) +
          ylim(-2.5,3) + 
          scale_x_date(labels = date_format("%Y"),breaks = date_breaks("2 years")) +
          labs(x = NULL, y = NULL) +
          ggtitle("SOCIAL SECURITY CARD (detrended)") +
          theme(plot.title = element_text(hjust = 0.5)) + 
          geom_line()
    p4
  
  # CARE DONATE has been transformed using a quadratic trend:
    # Before transformation: 
    p5 <- ggplot(data = mGT_logts, aes(x = Date, y = mGT_logts[,20])) +
          ylim(2,4.5) +
          scale_x_date(labels = date_format("%Y"),breaks = date_breaks("2 years")) +
          labs(x = NULL,y = NULL) + 
          geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
          ggtitle("CAR DONATE") + # Title to the plot.
          theme(plot.title = element_text(hjust = 0.5)) + # Adjust the title to the center of the figure.
          geom_line()
    p5
    
    # After transformation:
    p6 <- ggplot(data = mGT_log_transformed_ts, aes(x = Date, y = mGT_log_transformed_ts[,20])) +
          ylim(-3,3) +
          scale_x_date(labels = date_format("%Y"),breaks = date_breaks("2 years")) +
          labs(x = NULL,y = NULL) + 
          ggtitle("CAR DONATE (detrended)") + # Title to the plot.
          theme(plot.title = element_text(hjust = 0.5)) + # Adjust the title to the center of the figure.
          geom_line()
    p6

# Combine the plots above in one single plot: 
    figure_ADF_procedure <- ggarrange(p3,p4,p5,p6,p1,p2,ncol = 2, nrow = 3)
    figure_ADF_procedure 
    

############################################################################
############################# Subset data functions: #######################
############################################################################
    
# This section includes functions for factor estimation and subset selection.    
  GetFactors <- function(X,r){
    # This function will determine the number of factors in the factor model.
    # Input: 
    # X: Matrix of regressors. 
    # r: Max number of factors. 
    # Output: 
    # factors: This is an (T*r) matrix which contains the factors.
    # loadings: This is an (N*r) matrix which contains the loadings.
    #X <- mGT_log_transformed_ts
    X <- data.matrix(X)
    X <- X[,-c(1)]
    X[is.na(X)] <- 0
    data_pca <- prcomp(X,rank. = r)
    summary_pca <- summary(data_pca)
    loadings <- data_pca$rotation
    factors  <- (X %*% loadings)
      
    list_out <- list(
      "factors" = factors,
      "loadings" = loadings,
      "summary_pca" = summary_pca
      )
    # Finally, I specify what the function should return:
    return(list_out)
      
  }
    
  EstFactors <- function(X,ic = 1, trace = FALSE, r_max = 8){
    # Inputs: 
    # X: Data matrix.
    # ic: Informations criteria: 
    # if 1: AIC.
    # if 2: BIC
      
    # Start of by excluding the date variable:
    X_est <- X[,-c(1)]
    X_est[is.na(X_est)] <- 0
      
    n_obs <- dim(X_est)[1]
    n_var <- dim(X_est)[2]
      
    r_max <- r_max
    ics <- rep(NA,r_max)
      
    for(r in 1:r_max){
      est_r <- GetFactors(X = X,r)
      F_r   <- est_r$factors
      L_r   <- est_r$loadings
        
      penalty <- r*(n_var + n_obs)/(n_var * n_obs) * log(min(n_var,n_obs))
      V_r     <- (sum(diag(crossprod(as.matrix(X_est) - tcrossprod(F_r,L_r))))/(n_obs * n_var))
      ics[r]  <- log(V_r) + penalty
        
    }
    r_opt <- which.min(ics)
    est_opt <- GetFactors(X = X, r = r_opt)
    factors_opt <- est_opt$factors
    colnames(factors_opt) <- paste("F", 1:r_opt, sep = "")
      
    df_out <- data.frame(X[,1],factors_opt)
    colnames(df_out)[1] <- c("Date")
    return(df_out)
  }

# The function below performs variable selection based on the LASSO algoritm:
  # Support function for the best subset function: 
  formula_model <- function(n_id,object, outcome){
    # Get data:
    models <- summary(object)$which[n_id,-1]
    # Now get model predictors: 
    m_predictors <- names(which(models == TRUE))
    m_predictors <- paste(m_predictors, collapse = "+")
    # Now build the formula:
    formula_print <- as.formula(paste0(outcome, "~",m_predictors))
    return(formula_print)
  }

  var_selection_best_subset <- function(y,X_reg,w_size,crit){
    # Inputs: 
      # y: Target variable.
      # X_reg: Input matrix
      # crit: 
        # if 1: Adj_R2 metric will guide model selection.
        # if 2: CP metric will guide model selection.
        # if 3: BIC metric will guide model selection.
    
    # List to return:
    list_out <- list()
    
    # Create first dataset:
    X <- X_reg[,-c(1)] # Exclude the data column.
    X <- as.matrix(X)
    X <- cbind(y,X)
    X <- X[,-c(1)]
    
    data_train <-X[(1:w_size),]
    data_test <- X[(w_size + 1):nrow(X),]
    
    # Now apply the subset algoritm:
    subset_reg <- regsubsets(data_train[,1] ~ . -Return_SandP500, data = data_train, nvmax = 40, method = "backward") # alpha = 1 for LASSO.
    subset_reg_sum <- summary(subset_reg)
    
    mD_1 <- data.frame(
        Adj_R2 = which.max(subset_reg_sum$adjr2),
        CP = which.min(subset_reg_sum$cp),
        BIC = which.min(subset_reg_sum$bic)
      )
    n_id_choosen <- mD_1[,crit]
    # Call support function:
    formula_to_print <- formula_model(n_id = n_id_choosen,subset_reg,"Return_S&P500")
    
    return(formula_to_print)
    
  }

############################################################################
############################# Forecasting functions: #######################
############################################################################

##### Expanding_window_forecasting_function

## Arima:
  expanding_forecast_arima <- function(X,w_size,h,p,q){
    T <- length(X[,2])
    n_windows <- T - w_size # This is the number of observations left to evaluate the forecast.
    
    foreach(i=1:n_windows, .combine = rbind) %do% {
      X_in <- as.ts(X[1:(w_size + i - h),2],start = c(2004,1), frequency = 12) # Select data for in and out of sample.
      # Now we estimate the model: 
      m_1 <- Arima(X_in, order = c(p,0,q))
      f1 <- forecast::forecast(m_1,h = h) # Do the h-step ahead forecast.
      
      return(c(f1)) # Return the forecast.
    }
    
  }

## Forecast function expanding window:
  expanding_f_2 <- function(y,X_reg,h, w_size,ic=1,p,d,q){
    # Call the function to estimate factors:
    F_df <- EstFactors(X_reg,ic)
    F_df2 <- F_df[,-c(1)]
    X_reg2 <- X_reg[,-c(1)]
    X <- cbind(y,F_df2,X_reg2)
    X <- X[,-c(1)]
    T <- length(X[,2])
    n_windows <- T - w_size # Definition of window size.
    
    foreach(i=1:n_windows, .combine = rbind) %do% {
      X_in <- as.ts(X[1:(w_size + i - h),],start = c(2004,1), frequency = 12)
      X_out <-  as.matrix(X[w_size + i:T,])
          #X_out <- as.matrix(X[w_size + seq(1:n_windows),])
          
      mX_in <- as.matrix(X[1:(w_size + i -h),])
      
      # Now set up the model:
      m_1 <- Arima(X_in[,1],order = c(p,d,q), xreg = mX_in[,2:4])

      f1 <- forecast::forecast(m_1,h = h, xreg = X_out[,2:4]) #newxreg
      
      return(c(f1))
      
    }

  }
  
  expanding_f_2_ml <- function(y,X_reg,h, w_size,ic=1,p,d,q){
    # Call the function to estimate factors:
    F_df <- EstFactors(X_reg,ic)
    F_df2 <- F_df[,-c(1)]
    X_reg2 <- X_reg[,-c(1)]
    X <- cbind(y,F_df2,X_reg2)
    X <- X[,-c(1)]
    T <- length(X[,2])
    n_windows <- T - w_size # Definition of window size.
    
    foreach(i=1:n_windows, .combine = rbind) %do% {
      X_in <- as.ts(X[1:(w_size + i - h),],start = c(2004,1), frequency = 12)
      X_out <-  as.matrix(X[w_size + i:T,])
      #X_out <- as.matrix(X[w_size + seq(1:n_windows),])
      
      mX_in <- as.matrix(X[1:(w_size + i -h),])
      
      # Now set up the model:
      m_1 <- Arima(X_in[,1],order = c(p,d,q), xreg = mX_in[,2:4], method = "ML")
      
      f1 <- forecast::forecast(m_1,h = h, xreg = X_out[,2:4]) #newxreg
      
      return(c(f1))
      
    }
    
  }


## Forecast function expanding window (intended for best subset):
  expanding_f_3 <- function(y,X_reg1, X_reg1_1, h, w_size,ic=1,p,d,q){
    # Input: 
      # X_reg1: Main matrix of external regressors.
      # X_reg1_1: vector of variables to include. Selected by best subset.
    X_reg2 <- X_reg1[,X_reg1_1]
    X <- cbind(y,X_reg2)
    X <- X[,-c(1)]
    T <- length(X[,2])
    n_windows <- T - w_size # Definition of window size.
    
    foreach(i=1:n_windows, .combine = rbind) %do% {
      X_in <- as.ts(X[1:(w_size + i - h),],start = c(2004,1), frequency = 12)
      X_out <-  as.matrix(X[w_size + i:T,])
      #X_out <- as.matrix(X[w_size + seq(1:n_windows),])
      
      mX_in <- as.matrix(X[1:(w_size + i -h),])
      
      # Now set up the model:
      m_1 <- Arima(X_in[,1],order = c(p,d,q), xreg = mX_in[,X_reg1_1],method = "ML")
      
      f1 <- forecast::forecast(m_1,h = h, xreg = X_out[,X_reg1_1]) #newxreg
      
      return(c(f1))
      
    }
    
  }

############################################################################
############################# Error functions: #############################
############################################################################

  for_errors <- function(y,yhat,partion = 0,length_first_period){
    # Input: 
      # y: actual series.
      # yhat: forecasts.
      # partion: 
        # dummy: 
          # If 0: no partion of data. 
          # if 1: partion of data.
    
    if(partion == 0){
      length_first_period = NULL
      # This part computes errors on the whole out-of-sample period. 
      ehat <- tail(y, n = length(yhat))  - yhat 
      rmse <- ((1/length(ehat)*(sum(ehat^2, na.rm = TRUE)))^0.5)
      mae <- (1/length(ehat)*sum(abs(ehat), na.rm = TRUE))
      
      list.out <- list(
        "ehat" = ehat ,
        "rmse" = rmse ,
        "mae" = mae
      )
      return(list.out)
      
    }
    
    else if(partion == 1){
      # This part computes errors in selected parts of the out-of-the-sample period.
      period_1 <- length_first_period
      n1 <- length(y) - length(yhat)
      n2 <- n1  + period_1
      
      # Period 1:
      ehat_period_1 <- y[(n1 + 1):n2]  - yhat[1:period_1]   # tail specifies that we want the n last obs in the vector
      rmse_period_1 <- ((1/length(ehat_period_1)*(sum(ehat_period_1^2, na.rm = TRUE)))^0.5)
      mae_period_1 <- (1/length(ehat_period_1)*sum(abs(ehat_period_1), na.rm = TRUE))
      
      # Period 2:
      ehat_period_2 <- tail(y, n = length(yhat)-period_1)  - yhat[(period_1+1):length(yhat)]   # tail specifies that we want the n last obs in the vector
      rmse_period_2 <- ((1/length(ehat_period_2)*(sum(ehat_period_2^2, na.rm = TRUE)))^0.5)
      mae_period_2 <- (1/length(ehat_period_2)*sum(abs(ehat_period_2), na.rm = TRUE))
      
      
      list.out <- list(
        "ehat_period_1" = ehat_period_1 ,
        "rmse_period_1" = rmse_period_1 ,
        "mae_period_1" = mae_period_1, 
        
        "ehat_period_2" = ehat_period_2 ,
        "rmse_period_2" = rmse_period_2 ,
        "mae_period_2" = mae_period_2 
        
      )
      return(list.out)
      
    }

  }
  
# Out of sample R^2:
  out_of_sample_R2 <- function(y, yhat, partion = 0, length_first_period = 0, mean_forecast){
    
    if(partion == 0){
      ehat_up <- tail(y, n = length(yhat)) - yhat
      ehat_down <- tail(y, n = length(mean_forecast)) - mean_forecast
      
      
      R2_OS <- 1 - sum(ehat_up,na.rm = TRUE)^2/sum(ehat_down, na.rm = TRUE)^2
      
      return(R2_OS)
    }
    
    else if(partion == 1){
      period_1 <- length_first_period
      n1 <- length(y) - length(yhat)
      n2 <- n1  + period_1
      
      # Period 1: 
      ehat_up_period1 <- y[(n1 + 1):n2] - yhat[1:period_1]
      ehat_down_period1 <- y[(n1 + 1):n2] - mean_forecast[1:period_1]
      
      
      R2_OS_period1 <- 1 - sum(ehat_up_period1 ,na.rm = TRUE)^2/sum(ehat_down_period1, na.rm = TRUE)^2
      
      # Period 2: 
      ehat_up_period2 <- tail(y, n = length(yhat) - period_1) - yhat[(period_1+1):length(yhat)]
      ehat_down_period2 <- tail(y, n = length(yhat) - period_1) - mean_forecast[(period_1+1):length(yhat)]
      
      
      R2_OS_period2 <- 1 - sum(ehat_up_period2  ,na.rm = TRUE)^2/sum(ehat_down_period2, na.rm = TRUE)^2
      
      list_out <- list(
        "R2_OS_period1" = R2_OS_period1,
        "R2_OS_period2" = R2_OS_period2
        
      )
      
      return(list_out)
      
    }

  }

############################################################################
############################# Plot y-variable & summary statistics: ########
############################################################################
  
############################# Plot: 
  r_t_plot <- ggplot(data = r_t, aes(x = Date, y = r_t[,2])) + 
    #ylim(-20,15) + 
    scale_x_date(labels = date_format("%Y"),breaks = date_breaks("2 years")) +
    ylab("log(return) of S&P500%") +
    geom_line()
  r_t_plot
  
  
############################# Summary statistics of r_t:
  
# Compute statistics:
  nobs_r_t <- dim(r_t)[1]       # Number of observations in r_t:
  mean_r_t <- mean(r_t[,2])     # Mean of r_t: 
  var_r_t <-var(r_t[,2]) # Standard deviation of r_t
  kurtosis_r_t <- kurtosis(r_t[,2]) # Kurtosis.
  skew_r_t <- skewness(r_t[,2])
  
############################# Summary statistics of Google Trends variables:
  
# Make containers: 
  summary_statistics_GT <- matrix(data = NA, nrow = ncol(mGT_log_transformed_ts) - 1, ncol = 3)
  colnames(summary_statistics_GT) <- c("Mean","Kurtosis", "Skewness")

# Now, open a loop which computes the statistics:
  for(j in 2:dim(mGT_log_transformed_ts)[2]){
    summary_statistics_GT[j-1,1] <- mean(mGT_log_transformed_ts[,j], na.rm = TRUE)
    summary_statistics_GT[j-1,2] <- kurtosis(mGT_log_transformed_ts[,j], na.rm = TRUE)
    summary_statistics_GT[j-1,3] <- skewness(mGT_log_transformed_ts[,j], na.rm = TRUE)
  }

# Save the above matrix to LaTeX:
  rownames(summary_statistics_GT) <- colnames(mGT_log_transformed_ts)[2:41]
  print(xtable(summary_statistics_GT, type = "latex"), file = "summary_statistics_GT.tex")

############################################################################
############################# Forecasting: #################################
############################################################################

###### Model overview:
  # benchmark_model: Benchmark.
    # Forecast: c0
  # Y_in_fit: AR(3)
    # Forecast: c1.
  # Y_in_fit_factor: AR with external regressors.
    # Forecast: c2.

###### Generel setup: 
  w_size <- 48
  p_y <- 10
  mAIC <- rep(NA, p_y) # p_y is max lag.
  Y_in <- as.ts(r_t[1:w_size,2], start = c(2004,1), frequency = 12)
  
  y <- r_t
  X_reg <- mGT_log_transformed_ts
  F_df <- EstFactors(X_reg,ic)
  F_df2 <- F_df[,-c(1)]
  X_reg2 <- X_reg[,-c(1)]
  X <- cbind(y,F_df2,X_reg2)
  mX_in_2 <- X[1:48,3:5]
  mX_in_2 <- as.matrix(mX_in_2)
  
  X2 <- cbind(y,X_reg2)
  mX_in_3 <- X2[1:48,c("THE.CRISIS","Cash","Bubble","Dividend","Society")]
  mX_in_3 <- as.matrix(mX_in_3)

############################# Benchmark:

####### Best model in-sample:

# Fit a arima model only with mean and noise:
  bechmark_model <- arima(Y_in, order = c(0,0,0),
              method = "ML")

####### Tests:
  
### Autocorrelation in the error terms - Ljung & Box: (H0: No autocorrelation.)
  benchmark_box4 <-  Box.test(resid(bechmark_model), lag = 4, type = "Ljung")
  benchmark_box8 <-  Box.test(resid(bechmark_model), lag = 8, type = "Ljung")
  benchmark_box12 <-  Box.test(resid(bechmark_model), lag = 12, type = "Ljung")
  benchmark_box24 <-  Box.test(resid(bechmark_model), lag = 24, type = "Ljung") # No problem. Even for high number of lags.  
  
### Homoskedasticity (H0: no heteroskedasticity): 
 benchmark_arch <- arch.test(bechmark_model) # Very high p-values -> no problem. 
  
### Test for normality (H0: Skew = 0 an excess kurtosis of 0):
  benchmark_jarque_bera <- jarque.bera.test(resid(bechmark_model)) # No problem.
  
### Make a table
  benchmark_test_matrix <- matrix(data = NA, nrow = 4, ncol = 5)
  benchmark_test_matrix[1,c(1,3)] <- 4
  benchmark_test_matrix[2,c(1,3)] <- 8
  benchmark_test_matrix[3,c(1,3)] <- 12
  benchmark_test_matrix[4,c(1,3)] <- 24
  
  colnames(benchmark_test_matrix) <- c("Lags", "P-value","Lags","P-value", "P-value")
  
# Insert values:
  benchmark_test_matrix[1,2] = benchmark_box4$p.value
  benchmark_test_matrix[2,2] = benchmark_box8$p.value
  benchmark_test_matrix[3,2] = benchmark_box12$p.value
  benchmark_test_matrix[4,2] = benchmark_box24$p.value
  
  benchmark_test_matrix[1,4] = benchmark_arch[1,5]
  benchmark_test_matrix[2,4] = benchmark_arch[2,5]
  benchmark_test_matrix[3,4] = benchmark_arch[3,5]
  benchmark_test_matrix[4,4] = benchmark_arch[6,5]
  
  benchmark_test_matrix[1,5] <- benchmark_jarque_bera$p.value
  
# Print to latex:
  print(xtable(benchmark_test_matrix, digits = 2), file = "benchmark_test_matrix.tex")

####### Forecasting:
  
### Apply function to generate forecasts:
  c0 <- data.frame(matrix(data = NA, nrow = 156, ncol = 12)) # Container for h=1 steap-a-head forecast.
  for(j in 1:12){
    expandinglist <- expanding_forecast_arima(X = r_t, w_size = 48, h=j, p=0, q=0) # NB: only 1,3,6 and 12 will be considered.
    for(i in 1:156){
      c0[i,j] <- as.data.frame(expandinglist[[468+i]][j])
    }
  }
  
### Plot the above forecasts:
  
# Bind the data with the original data:
  c0 <- cbind(c0,tail(r_t, n = 156))
  colnames(c0)[1:12] <- c("h=1","h=2","h=3","h=4","h=5","h=6",
                          "h=7","h=8","h=9","h=10", "h=11","h=12")
  colnames(c0)[14] <- c("Data")
  rownames(c0) <- rownames(r_t$Date)
  
# Plot: 
  c0_plot <- (ggplot(data = c0,aes(x = `Date`)) +
              geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
              geom_line(aes(y = `h=1`, colour = "h = 1")) +
              geom_line(aes(y = `h=3`, colour = "h = 3")) +
              geom_line(aes(y = `h=6`, colour = "h = 6")) +
              geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                
              # Adjustment of axis & titles:
              labs(title = "Forecasts of benchmark model", y = "%") +
              theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
              scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))  
  c0_plot
  
  
############################# AR:
  
####### Best model in-sample:
  
# Open loop to find the best model:
  for(i in 1:p_y){
    mfit_AR <- arima(Y_in, order = c(i,0,0),
                     method = "ML")
    mAIC[i] <- AIC(mfit_AR, k = 2)
  }
  k <- which.min(mAIC) # 3 lags should be included.

# Specify best in-sample modeL
  Y_in_fit <- arima(Y_in, order = c(k,0,0),
                    method = "ML")

####### Tests:

### Autocorrelation in the error terms - Ljung & Box:
  AR_box4 <- Box.test(resid(Y_in_fit), lag = 4, type = "Ljung") # No problem. Even for high number of lags.  
  AR_box8 <-Box.test(resid(Y_in_fit), lag = 8, type = "Ljung")
  AR_box12 <-Box.test(resid(Y_in_fit), lag = 12, type = "Ljung")
  AR_box24 <-Box.test(resid(Y_in_fit), lag = 24, type = "Ljung")
  
### Homoskedasticity: 
  AR_arch <- arch.test(Y_in_fit) # Very high p-values -> no problem. 
  
### Test for normality:
  AR_jarque_bera <- jarque.bera.test(resid(Y_in_fit)) # No problem.
  
### Make a table of the results above to illustrate the procedure:
  AR_test_matrix <- matrix(data = NA, nrow = 4, ncol = 5)
  AR_test_matrix[1,c(1,3)] <- 4
  AR_test_matrix[2,c(1,3)] <- 8
  AR_test_matrix[3,c(1,3)] <- 12
  AR_test_matrix[4,c(1,3)] <- 24

  colnames(AR_test_matrix) <- c("Lags", "P-value","Lags","P-value", "P-value")

  # Insert values:
  AR_test_matrix[1,2] = AR_box4$p.value
  AR_test_matrix[2,2] = AR_box8$p.value
  AR_test_matrix[3,2] = AR_box12$p.value
  AR_test_matrix[4,2] = AR_box24$p.value
  
  AR_test_matrix[1,4] = AR_arch[1,5]
  AR_test_matrix[2,4] = AR_arch[2,5]
  AR_test_matrix[3,4] = AR_arch[3,5]
  AR_test_matrix[4,4] = AR_arch[6,5]
  
  AR_test_matrix[1,5] <- AR_jarque_bera$p.value
  
# Print to latex:
  print(xtable(AR_test_matrix, type = "latex"), file = "AR_test_matrix.tex")
  
####### Forecasting:

### Apply function to generate forecasts:
  c1 <- data.frame(matrix(data = NA, nrow = 156, ncol = 12)) # Container for h=1 steap-a-head forecast.
  for(j in 1:12){
    expandinglist <- expanding_forecast_arima(X = r_t, w_size = 48, h=j, p=3, q=0) # NB: only 1,3,6 and 12 will be considered.
    for(i in 1:156){
      c1[i,j] <- as.data.frame(expandinglist[[468+i]][j])
    }
  }

### Plot the above forecasts:

# Bind the data with the original data:
  c1 <- cbind(c1,tail(r_t, n = 156))
  colnames(c1)[1:12] <- c("h=1","h=2","h=3","h=4","h=5","h=6",
                          "h=7","h=8","h=9","h=10", "h=11","h=12")
  colnames(c1)[14] <- c("Data")
  rownames(c1) <- rownames(r_t$Date)

# Plot: 
  c1_plot <- (ggplot(data = c1,aes(x = `Date`)) +
                geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                geom_line(aes(y = `h=1`, colour = "h = 1")) +
                geom_line(aes(y = `h=3`, colour = "h = 3")) +
                geom_line(aes(y = `h=6`, colour = "h = 6")) +
                geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                
                # Adjustment of axis & titles:
                labs(title = "Forecasts of AR(3)", y = "%") +
                theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))  
  c1_plot

### Make a table to illustrate the model selection procedure:
  matrix_mAIC <- as.matrix(t(mAIC))
  rownames(matrix_mAIC) <- c("AIC")
  colnames(matrix_mAIC) <- c("1","2","3","4","5","6","7","8","9","10")
  print(xtable(matrix_mAIC, type = "latex"), file = "AIC_selection_AR.tex")

############################# Factor model (AR with factors):

####### Best model in-sample:

# Open loop to find the best model:
  for(i in 1:p_y){
    mfit_AR_factor <- arima(Y_in,order = c(i,0,0), xreg = mX_in_2,
                       method = "ML")
    mAIC[i] <- AIC(mfit_AR_factor, k = 2)
  }
  k <- which.min(mAIC) # 3 lags should be included.
  k
  
# Specify best in-sample modeL
  Y_in_fit_factor <- arima(Y_in,order = c(k,0,0), xreg = mX_in_2,
                           method = "ML")
  
####### Tests:
  
### Autocorrelation in the error terms - Ljung & Box:
  factor_model_box4 <- Box.test(resid(Y_in_fit_factor), lag = 4, type = "Ljung") 
  factor_model_box8 <- Box.test(resid(Y_in_fit_factor), lag = 8, type = "Ljung") 
  factor_model_box12 <- Box.test(resid(Y_in_fit_factor), lag = 12, type = "Ljung") 
  factor_model_box24 <- Box.test(resid(Y_in_fit_factor), lag = 24, type = "Ljung") # No problem. Even for high number of lags.  
  
### Homoskedasticity: 
  factor_model_arch <- arch.test(Y_in_fit_factor) # Very high p-values -> no problem. 
  
### Test for normality:
  factor_model_jarque_bera <- jarque.bera.test(resid(Y_in_fit_factor)) # No problem.
  
### Make a table
  factor_model_test_matrix <- matrix(data = NA, nrow = 4, ncol = 5)
  factor_model_test_matrix[1,c(1,3)] <- 4
  factor_model_test_matrix[2,c(1,3)] <- 8
  factor_model_test_matrix[3,c(1,3)] <- 12
  factor_model_test_matrix[4,c(1,3)] <- 24
  
  colnames(factor_model_test_matrix) <- c("Lags", "P-value","Lags","P-value", "P-value")
  
# Insert values:
  factor_model_test_matrix[1,2] = factor_model_box4$p.value
  factor_model_test_matrix[2,2] = factor_model_box8$p.value
  factor_model_test_matrix[3,2] = factor_model_box12$p.value
  factor_model_test_matrix[4,2] = factor_model_box24$p.value
  
  factor_model_test_matrix[1,4] = factor_model_arch[1,5]
  factor_model_test_matrix[2,4] = factor_model_arch[2,5]
  factor_model_test_matrix[3,4] = factor_model_arch[3,5]
  factor_model_test_matrix[4,4] = factor_model_arch[6,5]
  
  factor_model_test_matrix[1,5] <- factor_model_jarque_bera$p.value
  
# Print to latex: 
  print(xtable(factor_model_test_matrix, digits = 2), file = "factor_model_test_matrix.tex")

####### Forecasting:
  
### Apply function to generate forecasts:
  c2_factor <- data.frame(matrix(data = NA, nrow = 156, ncol = 12)) # Container for h=1 steap-a-head forecast.
  for(j in 1:12){
    expandinglist <- expanding_f_2(y = r_t,X_reg = mGT_log_transformed_ts, h = j, w_size = 48, p = 3,d=0,q=0)# NB: only 1,3,6 and 12 will be considered.
    for(i in 1:156){
      c2_factor[i,j] <- as.data.frame(expandinglist[[468+i]][j])
    }
  }
  
### Plot the above forecasts:
  
# Bind the data with the original data:
  c2_factor <- cbind(c2_factor,tail(r_t, n = 156))
  colnames(c2_factor )[1:12] <- c("h=1","h=2","h=3","h=4","h=5","h=6",
                          "h=7","h=8","h=9","h=10", "h=11","h=12")
  colnames(c2_factor )[14] <- c("Data")
  rownames(c2_factor ) <- rownames(r_t$Date)
  
# Plot: 
  c2_factor_plot <- (ggplot(data = c2_factor,aes(x = `Date`)) +
                geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                geom_line(aes(y = `h=1`, colour = "h = 1")) +
                geom_line(aes(y = `h=3`, colour = "h = 3")) +
                geom_line(aes(y = `h=6`, colour = "h = 6")) +
                geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                
                # Adjustment of axis & titles:
                labs(title = "Forecasts of Factor model", y = "%") +
                theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))  
  c2_factor_plot

############################# AR model with regressors choosen by best subset algorithm.
  
####### Best model in-sample:

# Open loop to find the best model:
  for(i in 1:p_y){
    mfit_AR_best_subset <- arima(Y_in,order = c(i,0,0), xreg = mX_in_3,
                            method = "ML")
    mAIC[i] <- AIC(mfit_AR_best_subset, k = 2)
  }
  k <- which.min(mAIC) # 3 lags should be included.
  k
# Specify best in-sample modeL
  Y_in_fit_best_subset <- arima(Y_in,order = c(k,0,0), xreg = mX_in_3,
                           method = "ML")
  
  
####### Tests:
  
### Autocorrelation in the error terms - Ljung & Box:
  best_subset_box4 <- Box.test(resid(Y_in_fit_best_subset), lag = 4, type = "Ljung")
  best_subset_box8 <- Box.test(resid(Y_in_fit_best_subset), lag = 8, type = "Ljung")
  best_subset_box12 <- Box.test(resid(Y_in_fit_best_subset), lag = 12, type = "Ljung")
  best_subset_box24 <- Box.test(resid(Y_in_fit_best_subset), lag = 24, type = "Ljung") # No problem. Even for high number of lags.  
  
### Homoskedasticity: 
  best_subset_arch <- arch.test(Y_in_fit_best_subset) # Very high p-values -> no problem. 
  
### Test for normality:
  best_subset_jarque_bera <- jarque.bera.test(resid(Y_in_fit_best_subset))
  
### Make a table
  best_subset_test_matrix <- matrix(data = NA, nrow = 4, ncol = 5)
  best_subset_test_matrix[1,c(1,3)] <- 4
  best_subset_test_matrix[2,c(1,3)] <- 8
  best_subset_test_matrix[3,c(1,3)] <- 12
  best_subset_test_matrix[4,c(1,3)] <- 24
  
  colnames(factor_model_test_matrix) <- c("Lags", "P-value","Lags","P-value", "P-value")
  
# Insert values:
  best_subset_test_matrix[1,2] = best_subset_box4$p.value
  best_subset_test_matrix[2,2] = best_subset_box8$p.value
  best_subset_test_matrix[3,2] = best_subset_box12$p.value
  best_subset_test_matrix[4,2] = best_subset_box24$p.value
  
  best_subset_test_matrix[1,4] = best_subset_arch[1,5]
  best_subset_test_matrix[2,4] = best_subset_arch[2,5]
  best_subset_test_matrix[3,4] = best_subset_arch[3,5]
  best_subset_test_matrix[4,4] = best_subset_arch[6,5]
  
  best_subset_test_matrix[1,5] <- best_subset_jarque_bera $p.value

# Print to latex: 
  print(xtable(best_subset_test_matrix, digits = 2), file = "best_subset_test_matrix.tex")
  
####### Forecasting:
  
### Apply function to generate forecasts:
  c3_best_subset <- data.frame(matrix(data = NA, nrow = 156, ncol = 12)) # Container for h=1 steap-a-head forecast.
  for(j in 1:12){
    expandinglist <- expanding_f_3(y=r_t,X_reg1 = mGT_log_transformed_ts,
                                   X_reg1_1 = c("THE.CRISIS","Cash","Bubble","Dividend","Society"), # Regressors choosen by best_subset.
                                   h = j, w_size = 48, p = 10, d= 0, q = 0)
    for(i in 1:156){
      c3_best_subset[i,j] <- as.data.frame(expandinglist[[468+i]][j])
    }
  }
  
### Plot the above forecasts:
  
# Bind the data with the original data:
  c3_best_subset <- cbind(c3_best_subset,tail(r_t, n = 156))
  colnames(c3_best_subset)[1:12] <- c("h=1","h=2","h=3","h=4","h=5","h=6",
                          "h=7","h=8","h=9","h=10", "h=11","h=12")
  colnames(c3_best_subset)[14] <- c("Data")
  rownames(c3_best_subset) <- rownames(r_t$Date)
  
# Plot: 
  c3_best_subset_plot <- (ggplot(data = c3_best_subset,aes(x = `Date`)) +
                            geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                            geom_line(aes(y = `h=1`, colour = "h = 1")) +
                            geom_line(aes(y = `h=3`, colour = "h = 3")) +
                            geom_line(aes(y = `h=6`, colour = "h = 6")) +
                            geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                            
                            # Adjustment of axis & titles:
                            labs(title = "Forecasts of best subset model", y = "%") +
                            theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                            scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))  
  c3_best_subset_plot

############################# Forecast combination.
  
###### Preparation:

### Bind all the fitted values in a object:
  pred_matrix <- cbind(fitted(Y_in_fit), fitted(Y_in_fit_factor),
                       fitted(Y_in_fit_best_subset))
  pred_matrix <- as.matrix(pred_matrix)

###### Forecast: 

### h = 1
 
# Specify horizon: 
  horizon <- 1

### Bind all the forecats in a object:
  for_matrix_h1 <- cbind(c1[,1],c2_factor[,horizon],c3_best_subset[,horizon])
  colnames(for_matrix_h1) <- c("AR(3)","Factor model", "Best subset model")  
  
### Now set up the foreccomb object & do forecast:
  foreccomb_h1 <- foreccomb(observed_vector = r_t[1:48,2], prediction_matrix = pred_matrix,
                            newobs = r_t[49:204,2], newpreds = for_matrix_h1)
  
# Simple average:
  mean_comb_h1 <- comb_SA(foreccomb_h1)
  
# Bates/Granger combination:
  Bates_Granger_h1 <- comb_BG(foreccomb_h1)
  
# OLS combination:
  OLS_combination_h1 <- comb_OLS(foreccomb_h1)
  
### h = 3

# Specify horizon: 
  horizon <- 3
  
### Bind all the forecats in a object:
  for_matrix_h3 <- cbind(c1[,1],c2_factor[,horizon],c3_best_subset[,horizon])
  colnames(for_matrix_h3) <- c("AR(3)","Factor model", "Best subset model")  
  
### Now set up the foreccomb object & do forecast:
  foreccomb_h3 <- foreccomb(observed_vector = r_t[1:48,2], prediction_matrix = pred_matrix,
                            newobs = r_t[49:204,2], newpreds = for_matrix_h3)
  
# Simple average:
  mean_comb_h3 <- comb_SA(foreccomb_h3)
  
# Bates/Granger combination:
  Bates_Granger_h3 <- comb_BG(foreccomb_h3)
  
# OLS combination:
  OLS_combination_h3 <- comb_OLS(foreccomb_h3)
  
### h = 6
  
# Specify horizon: 
  horizon <- 6
  
### Bind all the forecats in a object:
  for_matrix_h6 <- cbind(c1[,1],c2_factor[,horizon],c3_best_subset[,horizon])
  colnames(for_matrix_h6) <- c("AR(3)","Factor model", "Best subset model")  
  
### Now set up the foreccomb object & do forecast:
  foreccomb_h6 <- foreccomb(observed_vector = r_t[1:48,2], prediction_matrix = pred_matrix,
                            newobs = r_t[49:204,2], newpreds = for_matrix_h6)
  
# Simple average:
  mean_comb_h6 <- comb_SA(foreccomb_h6)
  
# Bates/Granger combination:
  Bates_Granger_h6 <- comb_BG(foreccomb_h6)
  
# OLS combination:
  OLS_combination_h6 <- comb_OLS(foreccomb_h6)
  
### h = 12
  
# Specify horizon: 
  horizon <- 12
  
### Bind all the forecats in a object:
  for_matrix_h12 <- cbind(c1[,1],c2_factor[,horizon],c3_best_subset[,horizon])
  colnames(for_matrix_h12) <- c("AR(3)","Factor model", "Best subset model")  
  
### Now set up the foreccomb object & do forecast:
  foreccomb_h12 <- foreccomb(observed_vector = r_t[1:48,2], prediction_matrix = pred_matrix,
                            newobs = r_t[49:204,2], newpreds = for_matrix_h12)
  
# Simple average:
  mean_comb_h12 <- comb_SA(foreccomb_h12)
  
# Bates/Granger combination:
  Bates_Granger_h12 <- comb_BG(foreccomb_h12)
  
# OLS combination:
  OLS_combination_h12 <- comb_OLS(foreccomb_h12)

### Plot the forecast combinations:

# Simple average:  <------
  simple_average_forecast_comb <- cbind(mean_comb_h1$Forecasts_Test,
                                        mean_comb_h3$Forecasts_Test,
                                        mean_comb_h6$Forecasts_Test,
                                        mean_comb_h12$Forecasts_Test,
                                        tail(r_t, n = 156))
  
  colnames(simple_average_forecast_comb )[1:4] <- c("h=1","h=3","h=6","h=12")
  colnames(simple_average_forecast_comb)[6] <- c("Data")
  rownames(simple_average_forecast_comb) <- rownames(r_t$Date)
  
# Plot:
  simple_average_forecast_comb_plot <- (ggplot(data = simple_average_forecast_comb,aes(x = `Date`)) +
                                        geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                                        geom_line(aes(y = `h=1`, colour = "h = 1")) +
                                        geom_line(aes(y = `h=3`, colour = "h = 3")) +
                                        geom_line(aes(y = `h=6`, colour = "h = 6")) +
                                        geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                            
                                        # Adjustment of axis & titles:
                                        labs(title = "Forecast combination - Simple average", y = "%") +
                                        theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                                        scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))
  
  simple_average_forecast_comb_plot

# Bates/Granger: <------
  Bates_Granger_forecast_comb <- cbind(Bates_Granger_h1$Forecasts_Test,
                                       Bates_Granger_h3$Forecasts_Test,
                                       Bates_Granger_h6$Forecasts_Test,
                                       Bates_Granger_h12$Forecasts_Test,
                                       tail(r_t, n = 156))
  
  colnames(Bates_Granger_forecast_comb )[1:4] <- c("h=1","h=3","h=6","h=12")
  colnames(Bates_Granger_forecast_comb)[6] <- c("Data")
  rownames(Bates_Granger_forecast_comb) <- rownames(r_t$Date)
  
  # Plot:
  Bates_Granger_forecast_comb_plot <- (ggplot(data = Bates_Granger_forecast_comb,aes(x = `Date`)) +
                                          geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                                          geom_line(aes(y = `h=1`, colour = "h = 1")) +
                                          geom_line(aes(y = `h=3`, colour = "h = 3")) +
                                          geom_line(aes(y = `h=6`, colour = "h = 6")) +
                                          geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                                          
                                          # Adjustment of axis & titles:
                                          labs(title = "Forecast combination - Bates/Granger", y = "%") +
                                          theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                                          scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))
  
  Bates_Granger_forecast_comb_plot
  
# OLS combination: <------
  OLS_combination_forecast_comb <- cbind(OLS_combination_h1$Forecasts_Test,
                                         OLS_combination_h3$Forecasts_Test,
                                         OLS_combination_h6$Forecasts_Test,
                                         OLS_combination_h12$Forecasts_Test,
                                         tail(r_t, n = 156))
  
  colnames(OLS_combination_forecast_comb )[1:4] <- c("h=1","h=3","h=6","h=12")
  colnames(OLS_combination_forecast_comb)[6] <- c("Data")
  rownames(OLS_combination_forecast_comb) <- rownames(r_t$Date)
  
  # Plot:
  OLS_combination_forecast_comb_plot <- (ggplot(data = OLS_combination_forecast_comb,aes(x = `Date`)) +
                                        geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                                        geom_line(aes(y = `h=1`, colour = "h = 1")) +
                                        geom_line(aes(y = `h=3`, colour = "h = 3")) +
                                        geom_line(aes(y = `h=6`, colour = "h = 6")) +
                                        geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                                        
                                        # Adjustment of axis & titles:
                                        labs(title = "Forecast combination - OLS combination", y = "%") +
                                        theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                                        scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))
  
  OLS_combination_forecast_comb_plot
  
############################################################################
############################# Errors of forecasts: #########################
############################################################################
  
############################# Setup:
  m_Error <- matrix(data = NA, nrow = 22, ncol = 8) # Container.
  rownames(m_Error) <- c("Benchmark",
                              "RMSE",
                              "MAE",
                         "AR(3)",
                              "RMSE",
                              "MAE",
                         "Factor model",
                               "RMSE",
                               "MAE",
                         "Best Subset model",
                               "RMSE",
                               "MAE",
                         "Forecast combination",
                         "Simple average",
                               "RMSE",
                               "MAE",
                         "Bates/Granger",
                               "RMSE",
                               "MAE",
                         "OLS combination",
                               "RMSE",
                               "MAE"
                         )
  colnames(m_Error) <- c("h = 1", "h = 3", "h = 6", "h = 12","h = 1", "h = 3", "h = 6", "h = 12")
###### Benchmark:

### Period: 2008M1:2009M12

# RMSE:
  m_Error[2,1]  = for_errors(y = r_t[,2], yhat = c0[,1],
                               partion = 1, length_first_period = 12)$rmse_period_1
  m_Error[2,2]  = for_errors(y = r_t[,2], yhat = c0[,3],
                                               partion = 1, length_first_period = 12)$rmse_period_1
  m_Error[2,3]  = for_errors(y = r_t[,2], yhat = c0[,6],
                                               partion = 1, length_first_period = 12)$rmse_period_1
  m_Error[2,4]  = for_errors(y = r_t[,2], yhat = c0[,12],
                                               partion = 1, length_first_period = 12)$rmse_period_1

# MAE:
  m_Error[3,1]  = for_errors(y = r_t[,2], yhat = c0[,1],
                              partion = 1, length_first_period = 12)$mae_period_1
  m_Error[3,2]  = for_errors(y = r_t[,2], yhat = c0[,3],
                              partion = 1, length_first_period = 12)$mae_period_1
  m_Error[3,3]  = for_errors(y = r_t[,2], yhat = c0[,6],
                              partion = 1, length_first_period = 12)$mae_period_1
  m_Error[3,4]  = for_errors(y = r_t[,2], yhat = c0[,12],
                              partion = 1, length_first_period = 12)$mae_period_1

### Period: 2010:2020M12
  
# RMSE:
  m_Error[2,5]  = for_errors(y = r_t[,2], yhat = c0[,1],
                              partion = 1, length_first_period = 12)$rmse_period_2
  m_Error[2,6]  = for_errors(y = r_t[,2], yhat = c0[,3],
                              partion = 1, length_first_period = 12)$rmse_period_2
  m_Error[2,7]  = for_errors(y = r_t[,2], yhat = c0[,6],
                              partion = 1, length_first_period = 12)$rmse_period_2
  m_Error[2,8]  = for_errors(y = r_t[,2], yhat = c0[,12],
                              partion = 1, length_first_period = 12)$rmse_period_2
 
# MAE:
  m_Error[3,5]  = for_errors(y = r_t[,2], yhat = c0[,1],
                                               partion = 1, length_first_period = 12)$mae_period_2
  m_Error[3,6]  = for_errors(y = r_t[,2], yhat = c0[,3],
                                               partion = 1, length_first_period = 12)$mae_period_2
  m_Error[3,7]  = for_errors(y = r_t[,2], yhat = c0[,6],
                                               partion = 1, length_first_period = 12)$mae_period_2
  m_Error[3,8]  = for_errors(y = r_t[,2], yhat = c0[,12],
                                               partion = 1, length_first_period = 12)$mae_period_2

###### AR(3):
  
### Period: 2008M1:2009M12
  
# RMSE:
  m_Error[5,1]  = for_errors(y = r_t[,2], yhat = c1[,1],
                              partion = 1, length_first_period = 12)$rmse_period_1
  m_Error[5,2]  = for_errors(y = r_t[,2], yhat = c1[,3],
                              partion = 1, length_first_period = 12)$rmse_period_1
  m_Error[5,3]  = for_errors(y = r_t[,2], yhat = c1[,6],
                              partion = 1, length_first_period = 12)$rmse_period_1
  m_Error[5,4]  = for_errors(y = r_t[,2], yhat = c1[,12],
                              partion = 1, length_first_period = 12)$rmse_period_1

# MAE:
  m_Error[6,1]  = for_errors(y = r_t[,2], yhat = c1[,1],
                              partion = 1, length_first_period = 12)$mae_period_1
  m_Error[6,2]  = for_errors(y = r_t[,2], yhat = c1[,3],
                              partion = 1, length_first_period = 12)$mae_period_1
  m_Error[6,3]  = for_errors(y = r_t[,2], yhat = c1[,6],
                              partion = 1, length_first_period = 12)$mae_period_1
  m_Error[6,4]  = for_errors(y = r_t[,2], yhat = c1[,12],
                              partion = 1, length_first_period = 12)$mae_period_1
  
  
  
### Period: 2010:2020M12
  
# RMSE:
  m_Error[5,5]  = for_errors(y = r_t[,2], yhat = c1[,1],
                              partion = 1, length_first_period = 12)$rmse_period_2
  m_Error[5,6]  = for_errors(y = r_t[,2], yhat = c1[,3],
                              partion = 1, length_first_period = 12)$rmse_period_2
  m_Error[5,7]  = for_errors(y = r_t[,2], yhat = c1[,6],
                              partion = 1, length_first_period = 12)$rmse_period_2
  m_Error[5,8]  = for_errors(y = r_t[,2], yhat = c1[,12],
                              partion = 1, length_first_period = 12)$rmse_period_2  
  
  
# MAE:
  m_Error[6,5]  = for_errors(y = r_t[,2], yhat = c1[,1],
                              partion = 1, length_first_period = 12)$mae_period_2
  m_Error[6,6]  = for_errors(y = r_t[,2], yhat = c1[,3],
                              partion = 1, length_first_period = 12)$mae_period_2
  m_Error[6,7]  = for_errors(y = r_t[,2], yhat = c1[,6],
                              partion = 1, length_first_period = 12)$mae_period_2
  m_Error[6,8]  = for_errors(y = r_t[,2], yhat = c1[,12],
                              partion = 1, length_first_period = 12)$mae_period_2
  
###### Factor model:
  
### Period: 2008M1:2009M12
  
# RMSE:
  m_Error[8,1]  = for_errors(y = r_t[,2], yhat = c2_factor[,1],
                              partion = 1, length_first_period = 12)$rmse_period_1
  m_Error[8,2]  = for_errors(y = r_t[,2], yhat = c2_factor[,3],
                              partion = 1, length_first_period = 12)$rmse_period_1
  m_Error[8,3]  = for_errors(y = r_t[,2], yhat = c1[,6],
                              partion = 1, length_first_period = 12)$rmse_period_1
  m_Error[8,4]  = for_errors(y = r_t[,2], yhat = c1[,12],
                              partion = 1, length_first_period = 12)$rmse_period_1
  
# MAE:
  m_Error[9,1]  = for_errors(y = r_t[,2], yhat = c2_factor[,1],
                              partion = 1, length_first_period = 12)$mae_period_1
  m_Error[9,2]  = for_errors(y = r_t[,2], yhat = c2_factor[,3],
                              partion = 1, length_first_period = 12)$mae_period_1
  m_Error[9,3]  = for_errors(y = r_t[,2], yhat = c2_factor[,6],
                              partion = 1, length_first_period = 12)$mae_period_1
  m_Error[9,4]  = for_errors(y = r_t[,2], yhat = c2_factor[,12],
                              partion = 1, length_first_period = 12)$mae_period_1
  

### Period: 2010:2020M12
  
# RMSE:
  m_Error[8,5]  = for_errors(y = r_t[,2], yhat = c2_factor[,1],
                              partion = 1, length_first_period = 12)$rmse_period_2
  m_Error[8,6]  = for_errors(y = r_t[,2], yhat = c2_factor[,3],
                              partion = 1, length_first_period = 12)$rmse_period_2
  m_Error[8,7]  = for_errors(y = r_t[,2], yhat = c2_factor[,6],
                              partion = 1, length_first_period = 12)$rmse_period_2
  m_Error[8,8]  = for_errors(y = r_t[,2], yhat = c2_factor[,12],
                              partion = 1, length_first_period = 12)$rmse_period_2  
  
  
# MAE:
  m_Error[9,5]  = for_errors(y = r_t[,2], yhat = c2_factor[,1],
                              partion = 1, length_first_period = 12)$mae_period_2
  m_Error[9,6]  = for_errors(y = r_t[,2], yhat = c2_factor[,3],
                              partion = 1, length_first_period = 12)$mae_period_2
  m_Error[9,7]  = for_errors(y = r_t[,2], yhat = c2_factor[,6],
                              partion = 1, length_first_period = 12)$mae_period_2
  m_Error[9,8]  = for_errors(y = r_t[,2], yhat = c2_factor[,12],
                              partion = 1, length_first_period = 12)$mae_period_2
  
  

###### Best subset model:
  
### Period: 2008M1:2009M12
  
# RMSE:
  m_Error[11,1]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,1],
                              partion = 1, length_first_period = 12)$rmse_period_1
  m_Error[11,2]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,3],
                              partion = 1, length_first_period = 12)$rmse_period_1
  m_Error[11,3]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,6],
                              partion = 1, length_first_period = 12)$rmse_period_1
  m_Error[11,4]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,12],
                              partion = 1, length_first_period = 12)$rmse_period_1
  
  # MAE:
  m_Error[12,1]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,1],
                              partion = 1, length_first_period = 12)$mae_period_1
  m_Error[12,2]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,3],
                              partion = 1, length_first_period = 12)$mae_period_1
  m_Error[12,3]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,6],
                              partion = 1, length_first_period = 12)$mae_period_1
  m_Error[12,4]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,12],
                              partion = 1, length_first_period = 12)$mae_period_1
  
  
### Period: 2010:2020M12
  
# RMSE:
  m_Error[11,5]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,1],
                              partion = 1, length_first_period = 12)$rmse_period_2
  m_Error[11,6]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,3],
                              partion = 1, length_first_period = 12)$rmse_period_2
  m_Error[11,7]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,6],
                              partion = 1, length_first_period = 12)$rmse_period_2
  m_Error[11,8]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,12],
                              partion = 1, length_first_period = 12)$rmse_period_2  
  
  
# MAE:
  m_Error[12,5]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,1],
                              partion = 1, length_first_period = 12)$mae_period_2
  m_Error[12,6]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,3],
                              partion = 1, length_first_period = 12)$mae_period_2
  m_Error[12,7]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,6],
                              partion = 1, length_first_period = 12)$mae_period_2
  m_Error[12,8]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,12],
                              partion = 1, length_first_period = 12)$mae_period_2
  
###### Forecast combination (simple average):
  
### Period: 2008M1:2009M12
  
# RMSE:
  m_Error[15,1]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,1],
                              partion = 1, length_first_period = 12)$rmse_period_1
  m_Error[15,2]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,2],
                              partion = 1, length_first_period = 12)$rmse_period_1
  m_Error[15,3]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,3],
                              partion = 1, length_first_period = 12)$rmse_period_1
  m_Error[15,4]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,4],
                              partion = 1, length_first_period = 12)$rmse_period_1
  
# MAE:
  m_Error[16,1]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,1],
                              partion = 1, length_first_period = 12)$mae_period_1
  m_Error[16,2]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,2],
                              partion = 1, length_first_period = 12)$mae_period_1
  m_Error[16,3]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,3],
                              partion = 1, length_first_period = 12)$mae_period_1
  m_Error[16,4]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,4],
                              partion = 1, length_first_period = 12)$mae_period_1
  
  
### Period: 2010:2020M12
  
# RMSE:
  m_Error[15,5]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,1],
                              partion = 1, length_first_period = 12)$rmse_period_2
  m_Error[15,6]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,2],
                              partion = 1, length_first_period = 12)$rmse_period_2
  m_Error[15,7]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,3],
                              partion = 1, length_first_period = 12)$rmse_period_2
  m_Error[15,8]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,4],
                              partion = 1, length_first_period = 12)$rmse_period_2  
  
  
# MAE:
  m_Error[16,5]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,1],
                              partion = 1, length_first_period = 12)$mae_period_2
  m_Error[16,6]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,2],
                              partion = 1, length_first_period = 12)$mae_period_2
  m_Error[16,7]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,3],
                              partion = 1, length_first_period = 12)$mae_period_2
  m_Error[16,8]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,4],
                              partion = 1, length_first_period = 12)$mae_period_2

  
###### Forecast combination (Bates/Granger):
  
### Period: 2008M1:2009M12
  
# RMSE:
  m_Error[18,1]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,1],
                               partion = 1, length_first_period = 12)$rmse_period_1
  m_Error[18,2]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,2],
                               partion = 1, length_first_period = 12)$rmse_period_1
  m_Error[18,3]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,3],
                               partion = 1, length_first_period = 12)$rmse_period_1
  m_Error[18,4]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,4],
                               partion = 1, length_first_period = 12)$rmse_period_1
  
# MAE:
  m_Error[19,1]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,1],
                               partion = 1, length_first_period = 12)$mae_period_1
  m_Error[19,2]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,2],
                               partion = 1, length_first_period = 12)$mae_period_1
  m_Error[19,3]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,3],
                               partion = 1, length_first_period = 12)$mae_period_1
  m_Error[19,4]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,4],
                               partion = 1, length_first_period = 12)$mae_period_1

### Period: 2010:2020M12
  
# RMSE:
  m_Error[18,5]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb[,1],
                               partion = 1, length_first_period = 12)$rmse_period_2
  m_Error[18,6]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb[,2],
                               partion = 1, length_first_period = 12)$rmse_period_2
  m_Error[18,7]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb[,3],
                               partion = 1, length_first_period = 12)$rmse_period_2
  m_Error[18,8]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb[,4],
                               partion = 1, length_first_period = 12)$rmse_period_2  
  
  
# MAE:
  m_Error[19,5]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb[,1],
                               partion = 1, length_first_period = 12)$mae_period_2
  m_Error[19,6]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb[,2],
                               partion = 1, length_first_period = 12)$mae_period_2
  m_Error[19,7]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb[,3],
                               partion = 1, length_first_period = 12)$mae_period_2
  m_Error[19,8]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb[,4],
                               partion = 1, length_first_period = 12)$mae_period_2

  
###### Forecast combination (OLS-combination):
  
### Period: 2008M1:2009M12
  
# RMSE:
  m_Error[21,1]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,1],
                               partion = 1, length_first_period = 12)$rmse_period_1
  m_Error[21,2]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,2],
                               partion = 1, length_first_period = 12)$rmse_period_1
  m_Error[21,3]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,3],
                               partion = 1, length_first_period = 12)$rmse_period_1
  m_Error[21,4]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,4],
                               partion = 1, length_first_period = 12)$rmse_period_1
  
# MAE:
  m_Error[22,1]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,1],
                               partion = 1, length_first_period = 12)$mae_period_1
  m_Error[22,2]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,2],
                               partion = 1, length_first_period = 12)$mae_period_1
  m_Error[22,3]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,3],
                               partion = 1, length_first_period = 12)$mae_period_1
  m_Error[22,4]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,4],
                               partion = 1, length_first_period = 12)$mae_period_1
  
### Period: 2010:2020M12
  
# RMSE:
  m_Error[21,5]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,1],
                               partion = 1, length_first_period = 12)$rmse_period_2
  m_Error[21,6]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,2],
                               partion = 1, length_first_period = 12)$rmse_period_2
  m_Error[21,7]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,3],
                               partion = 1, length_first_period = 12)$rmse_period_2
  m_Error[21,8]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,4],
                               partion = 1, length_first_period = 12)$rmse_period_2  
  
  
# MAE:
  m_Error[22,5]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,1],
                               partion = 1, length_first_period = 12)$mae_period_2
  m_Error[22,6]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,2],
                               partion = 1, length_first_period = 12)$mae_period_2
  m_Error[22,7]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,3],
                               partion = 1, length_first_period = 12)$mae_period_2
  m_Error[22,8]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,4],
                               partion = 1, length_first_period = 12)$mae_period_2

# Print to latex: 
  print(xtable(m_Error, digits = 3), file = "m_Error.tex")
  
############################# Out-of-sample R^2:

  mError_out_of_sample <- matrix(data = NA, nrow = 6, ncol = 8) # Container.
  rownames(mError_out_of_sample) <- c("AR(3)",
                                      "Factor model",
                                      "Best subset model",
                                      "Simple average",
                                      "Bates/Granger",
                                      "OLS combination"
    
  )
  colnames(mError_out_of_sample) <- c("h = 1", "h = 3", "h = 6", "h = 12","h = 1", "h = 3", "h = 6", "h = 12")
  
###### AR(3)
  
### Period: 2008M1:2009M12
  AR3_R2OSh1_p1 <- out_of_sample_R2(r_t[,2], yhat = c1[,1], partion = 1, length_first_period = 12, mean_forecast = c0[,1])
  AR3_R2OSh3_p1 <- out_of_sample_R2(r_t[,2], yhat = c1[,3], partion = 1, length_first_period = 12, mean_forecast = c0[,3])
  AR3_R2OSh6_p1 <- out_of_sample_R2(r_t[,2], yhat = c1[,6], partion = 1, length_first_period = 12, mean_forecast = c0[,6])
  AR3_R2OSh12_p1 <- out_of_sample_R2(r_t[,2], yhat = c1[,12], partion = 1, length_first_period = 12, mean_forecast = c0[,12])
  
# Print the results: 
  mError_out_of_sample[1,1] <- AR3_R2OSh1_p1$R2_OS_period1
  mError_out_of_sample[1,2] <-  AR3_R2OSh3_p1$R2_OS_period1
  mError_out_of_sample[1,3] <-  AR3_R2OSh6_p1$R2_OS_period1
  mError_out_of_sample[1,4] <-  AR3_R2OSh12_p1$R2_OS_period1
  

### Period: 2010M1:2020M12 -> use the function results from above:
  mError_out_of_sample[1,5] <-AR3_R2OSh1_p1$R2_OS_period2 
  mError_out_of_sample[1,6] <- AR3_R2OSh3_p1$R2_OS_period2 
  mError_out_of_sample[1,7] <- AR3_R2OSh6_p1$R2_OS_period2 
  mError_out_of_sample[1,8] <-  AR3_R2OSh12_p1$R2_OS_period2 
 
###### Factor model: 
  
### Period: 2008M1:2009M12
  factor_model_R2OSh1_p1 <- out_of_sample_R2(r_t[,2], yhat = c2_factor[,1], partion = 1, length_first_period = 12, mean_forecast = c0[,1])
  factor_model_R2OSh3_p1 <- out_of_sample_R2(r_t[,2], yhat = c2_factor[,3], partion = 1, length_first_period = 12, mean_forecast = c0[,3])
  factor_model_R2OSh6_p1 <- out_of_sample_R2(r_t[,2], yhat = c2_factor[,6], partion = 1, length_first_period = 12, mean_forecast = c0[,6])
  factor_model_R2OSh12_p1 <- out_of_sample_R2(r_t[,2], yhat = c2_factor[,12], partion = 1, length_first_period = 12, mean_forecast = c0[,12])
  
# Print the results: 
  mError_out_of_sample[2,1] <-  factor_model_R2OSh1_p1$R2_OS_period1
  mError_out_of_sample[2,2] <-  factor_model_R2OSh3_p1$R2_OS_period1
  mError_out_of_sample[2,3] <-  factor_model_R2OSh6_p1$R2_OS_period1
  mError_out_of_sample[2,4] <-  factor_model_R2OSh12_p1$R2_OS_period1
  
### Period: 2010M1:2020M12 ->  use the function results from above:
  mError_out_of_sample[2,5] <- factor_model_R2OSh1_p1$R2_OS_period2 
  mError_out_of_sample[2,6] <- factor_model_R2OSh3_p1$R2_OS_period2 
  mError_out_of_sample[2,7] <-  factor_model_R2OSh6_p1$R2_OS_period2 
  mError_out_of_sample[2,8] <-  factor_model_R2OSh12_p1$R2_OS_period2 
  
###### Best subset model: 
  
### Period: 2008M1:2009M12
  c3_best_subset_R2OSh1_p1 <- out_of_sample_R2(r_t[,2], yhat = c3_best_subset[,1], partion = 1, length_first_period = 12, mean_forecast = c0[,1])
  c3_best_subset_R2OSh3_p1 <- out_of_sample_R2(r_t[,2], yhat = c3_best_subset[,3], partion = 1, length_first_period = 12, mean_forecast = c0[,3])
  c3_best_subset_R2OSh6_p1 <- out_of_sample_R2(r_t[,2], yhat = c3_best_subset[,6], partion = 1, length_first_period = 12, mean_forecast = c0[,6])
  c3_best_subset_R2OSh12_p1 <- out_of_sample_R2(r_t[,2], yhat = c3_best_subset[,12], partion = 1, length_first_period = 12, mean_forecast = c0[,12])
  
# Print the results: 
  mError_out_of_sample[3,1] <- c3_best_subset_R2OSh1_p1$R2_OS_period1
  mError_out_of_sample[3,2] <- c3_best_subset_R2OSh3_p1$R2_OS_period1
  mError_out_of_sample[3,3] <- c3_best_subset_R2OSh6_p1$R2_OS_period1
  mError_out_of_sample[3,4] <- c3_best_subset_R2OSh12_p1$R2_OS_period1
  
### Period: 2010M1:2020M12 ->  use the function results from above:
  mError_out_of_sample[3,5] <- c3_best_subset_R2OSh1_p1 $R2_OS_period2 
  mError_out_of_sample[3,6] <- c3_best_subset_R2OSh3_p1 $R2_OS_period2 
  mError_out_of_sample[3,7] <-  c3_best_subset_R2OSh6_p1 $R2_OS_period2 
  mError_out_of_sample[3,8] <- c3_best_subset_R2OSh12_p1 $R2_OS_period2 

###### Forecast combination - simple average. 
  
### Period: 2008M1:2009M12
  simple_average_forecast_comb_R2OSh1_p1 <- out_of_sample_R2(r_t[,2], yhat = simple_average_forecast_comb[,1], partion = 1, length_first_period = 12, mean_forecast = c0[,1])
  simple_average_forecast_comb_R2OSh3_p1 <- out_of_sample_R2(r_t[,2], yhat = simple_average_forecast_comb[,2], partion = 1, length_first_period = 12, mean_forecast = c0[,3])
  simple_average_forecast_comb_R2OSh6_p1 <- out_of_sample_R2(r_t[,2], yhat = simple_average_forecast_comb[,3], partion = 1, length_first_period = 12, mean_forecast = c0[,6])
  simple_average_forecast_comb_R2OSh12_p1 <- out_of_sample_R2(r_t[,2], yhat = simple_average_forecast_comb[,4], partion = 1, length_first_period = 12, mean_forecast = c0[,12])
  
# Print the results: 
  mError_out_of_sample[4,1] <- simple_average_forecast_comb_R2OSh1_p1$R2_OS_period1
  mError_out_of_sample[4,2] <- simple_average_forecast_comb_R2OSh3_p1$R2_OS_period1
  mError_out_of_sample[4,3] <- simple_average_forecast_comb_R2OSh6_p1$R2_OS_period1
  mError_out_of_sample[4,4] <- simple_average_forecast_comb_R2OSh12_p1$R2_OS_period1
  
### Period: 2010M1:2020M12 ->  use the function results from above:
  mError_out_of_sample[4,5] <- simple_average_forecast_comb_R2OSh1_p1$R2_OS_period2
  mError_out_of_sample[4,6] <- simple_average_forecast_comb_R2OSh3_p1$R2_OS_period2
  mError_out_of_sample[4,7] <-  simple_average_forecast_comb_R2OSh6_p1$R2_OS_period2
  mError_out_of_sample[4,8] <-  simple_average_forecast_comb_R2OSh12_p1$R2_OS_period2

###### Forecast combination - Bates/Granger

### Period: 2008M1:2009M12
  Bates_Granger_forecast_comb_R2OSh1_p1 <- out_of_sample_R2(r_t[,2], yhat = Bates_Granger_forecast_comb[,1], partion = 1, length_first_period = 12, mean_forecast = c0[,1])
  Bates_Granger_forecast_comb_R2OSh3_p1 <- out_of_sample_R2(r_t[,2], yhat = Bates_Granger_forecast_comb[,2], partion = 1, length_first_period = 12, mean_forecast = c0[,3])
  Bates_Granger_forecast_comb_R2OSh6_p1 <- out_of_sample_R2(r_t[,2], yhat = Bates_Granger_forecast_comb[,3], partion = 1, length_first_period = 12, mean_forecast = c0[,6])
  Bates_Granger_forecast_comb_R2OSh12_p1 <- out_of_sample_R2(r_t[,2], yhat = Bates_Granger_forecast_comb[,4], partion = 1, length_first_period = 12, mean_forecast = c0[,12])
  
# Print the results: 
  mError_out_of_sample[5,1] <-  Bates_Granger_forecast_comb_R2OSh1_p1$R2_OS_period1
  mError_out_of_sample[5,2] <-  Bates_Granger_forecast_comb_R2OSh3_p1$R2_OS_period1
  mError_out_of_sample[5,3] <-  Bates_Granger_forecast_comb_R2OSh6_p1$R2_OS_period1
  mError_out_of_sample[5,4] <-  Bates_Granger_forecast_comb_R2OSh12_p1$R2_OS_period1

### Period: 2010M1:2020M12 ->  use the function results from above:
  mError_out_of_sample[5,5] <- Bates_Granger_forecast_comb_R2OSh1_p1$R2_OS_period2
  mError_out_of_sample[5,6] <- Bates_Granger_forecast_comb_R2OSh3_p1$R2_OS_period2
  mError_out_of_sample[5,7] <- Bates_Granger_forecast_comb_R2OSh6_p1$R2_OS_period2
  mError_out_of_sample[5,8] <- Bates_Granger_forecast_comb_R2OSh12_p1$R2_OS_period2

###### Forecast combination - OLS 

### Period: 2008M1:2009M12
  OLS_combination_forecast_comb_R2OSh1_p1 <- out_of_sample_R2(r_t[,2], yhat = OLS_combination_forecast_comb[,1], partion = 1, length_first_period = 12, mean_forecast = c0[,1])
  OLS_combination_forecast_comb_R2OSh3_p1 <- out_of_sample_R2(r_t[,2], yhat = OLS_combination_forecast_comb[,2], partion = 1, length_first_period = 12, mean_forecast = c0[,3])
  OLS_combination_forecast_comb_R2OSh6_p1 <- out_of_sample_R2(r_t[,2], yhat = OLS_combination_forecast_comb[,3], partion = 1, length_first_period = 12, mean_forecast = c0[,6])
  OLS_combination_forecast_comb_R2OSh12_p1 <- out_of_sample_R2(r_t[,2], yhat = OLS_combination_forecast_comb[,4], partion = 1, length_first_period = 12, mean_forecast = c0[,12])
  
# Print: 
  mError_out_of_sample[6,1] <- OLS_combination_forecast_comb_R2OSh1_p1$R2_OS_period1
  mError_out_of_sample[6,2] <-  OLS_combination_forecast_comb_R2OSh3_p1$R2_OS_period1
  mError_out_of_sample[6,3] <-  OLS_combination_forecast_comb_R2OSh6_p1$R2_OS_period1
  mError_out_of_sample[6,4] <-  OLS_combination_forecast_comb_R2OSh12_p1$R2_OS_period1

### Period: 2010M1:2020M12 ->  use the function results from above:
  mError_out_of_sample[6,5] <-  OLS_combination_forecast_comb_R2OSh1_p1$R2_OS_period2
  mError_out_of_sample[6,6] <-  OLS_combination_forecast_comb_R2OSh3_p1$R2_OS_period2
  mError_out_of_sample[6,7] <-  OLS_combination_forecast_comb_R2OSh6_p1$R2_OS_period2
  mError_out_of_sample[6,8] <-  OLS_combination_forecast_comb_R2OSh12_p1$R2_OS_period2

# Print to latex: 
  print(xtable(mError_out_of_sample, digits = 3), file = "mError_out_of_sample.tex")


###########################################################################################################################
############################# Robustness check: ###########################################################################
###########################################################################################################################
  
  
############################# Expanding window: w_size -> 72 (first forecast point is 2010:M1)
  
  
###### Model overview:
  # benchmark_model: Benchmark.
  # Forecast: c0
  # Y_in_fit: AR(3)
  # Forecast: c1.
  # Y_in_fit_factor: AR with external regressors.
  # Forecast: c2.
  
###### Generel setup: 
  w_size <- 72
  p_y <- 10
  mAIC <- rep(NA, p_y) # p_y is max lag.
  Y_in <- as.ts(r_t[1:w_size,2], start = c(2004,1), frequency = 12)
  
  y <- r_t
  X_reg <- mGT_log_transformed_ts
  F_df <- EstFactors(X_reg,ic)
  F_df2 <- F_df[,-c(1)]
  X_reg2 <- X_reg[,-c(1)]
  X <- cbind(y,F_df2,X_reg2)
  mX_in_2 <- X[1:w_size,3:5]
  mX_in_2 <- as.matrix(mX_in_2)
  
  X2 <- cbind(y,X_reg2)
  mX_in_3 <- X2[1:w_size,c("THE.CRISIS","Cash","Bubble","Dividend","Society")]
  mX_in_3 <- as.matrix(mX_in_3)
  
############################# Benchmark:
  
####### Best model in-sample:
  
# Fit a arima model only with mean and noise:
  bechmark_model <- arima(Y_in, order = c(0,0,0),
                          method = "ML")
  
####### Tests:
  
### Autocorrelation in the error terms - Ljung & Box: (H0: No autocorrelation.)
  Box.test(resid(bechmark_model), lag = 4, type = "Ljung")
  Box.test(resid(bechmark_model), lag = 8, type = "Ljung")
  Box.test(resid(bechmark_model), lag = 12, type = "Ljung")
  Box.test(resid(bechmark_model), lag = 24, type = "Ljung") 
  
### Homoskedasticity (H0: no heteroskedasticity): 
  arch.test(bechmark_model) # Very high p-values -> no problem. 
  
### Test for normality (H0: Skew = 0 an excess kurtosis of 0):
  jarque.bera.test(resid(bechmark_model)) # No problem.
  
####### Tests:
  
### Autocorrelation in the error terms - Ljung & Box:
  benchmarkr1_box4 <- Box.test(resid(bechmark_model), lag = 4, type = "Ljung")
  benchmarkr1_box8 <- Box.test(resid(bechmark_model), lag = 8, type = "Ljung")
  benchmarkr1_box12 <- Box.test(resid(bechmark_model), lag = 12, type = "Ljung")
  benchmarkr1_box24 <- Box.test(resid(bechmark_model), lag = 24, type = "Ljung") # No problem. Even for high number of lags.  
  
### Homoskedasticity: 
  benchmarkr1_arch <- arch.test(bechmark_model) # Very high p-values -> no problem. 
  
### Test for normality:
  benchmarkr1_jarque_bera <- jarque.bera.test(resid(bechmark_model))
  
### Make a table
  benchmarkr1_test_matrix <- matrix(data = NA, nrow = 4, ncol = 5)
  benchmarkr1_test_matrix[1,c(1,3)] <- 4
  benchmarkr1_test_matrix[2,c(1,3)] <- 8
  benchmarkr1_test_matrix[3,c(1,3)] <- 12
  benchmarkr1_test_matrix[4,c(1,3)] <- 24
  
  colnames(benchmarkr1_test_matrix) <- c("Lags", "P-value","Lags","P-value", "P-value")
  
# Insert values:
  benchmarkr1_test_matrix[1,2] = benchmarkr1_box4$p.value
  benchmarkr1_test_matrix[2,2] = benchmarkr1_box8$p.value
  benchmarkr1_test_matrix[3,2] = benchmarkr1_box12$p.value
  benchmarkr1_test_matrix[4,2] = benchmarkr1_box24$p.value
  
  benchmarkr1_test_matrix[1,4] = benchmarkr1_arch [1,5]
  benchmarkr1_test_matrix[2,4] = benchmarkr1_arch [2,5]
  benchmarkr1_test_matrix[3,4] = benchmarkr1_arch [3,5]
  benchmarkr1_test_matrix[4,4] = benchmarkr1_arch [6,5]
  
  benchmarkr1_test_matrix[1,5] <- benchmarkr1_jarque_bera $p.value
  
# Print to latex: 
  print(xtable(benchmarkr1_test_matrix, digits = 2), file = "benchmarkr1_test_matrix.tex")
  
####### Forecasting:
  
### Apply function to generate forecasts:
  c0 <- data.frame(matrix(data = NA, nrow = 204-w_size, ncol = 12)) # Container for h=1 steap-a-head forecast.
  for(j in 1:12){
    expandinglist <- expanding_forecast_arima(X = r_t, w_size = w_size, h=j, p=0, q=0) # NB: only 1,3,6 and 12 will be considered.
    for(i in 1:(204-w_size)){
      c0[i,j] <- as.data.frame(expandinglist[[396+i]][j])
    }
  }
  
  expanding_counter = 396
  
### Plot the above forecasts:
  
# Bind the data with the original data:
  c0 <- cbind(c0,tail(r_t, n = 204-w_size))
  colnames(c0)[1:12] <- c("h=1","h=2","h=3","h=4","h=5","h=6",
                          "h=7","h=8","h=9","h=10", "h=11","h=12")
  colnames(c0)[14] <- c("Data")
  rownames(c0) <- rownames(r_t$Date)
  
# Plot: 
  c0_plot <- (ggplot(data = c0,aes(x = `Date`)) +
                geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                geom_line(aes(y = `h=1`, colour = "h = 1")) +
                geom_line(aes(y = `h=3`, colour = "h = 3")) +
                geom_line(aes(y = `h=6`, colour = "h = 6")) +
                geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                
                # Adjustment of axis & titles:
                labs(title = "Forecasts of benchmark model", y = "%") +
                theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))  
  c0_plot
  
############################# AR:
  
####### Best model in-sample:
  
# Open loop to find the best model:
  for(i in 1:p_y){
    mfit_AR <- arima(Y_in, order = c(i,0,0),
                     method = "ML")
    mAIC[i] <- AIC(mfit_AR, k = 2)
  }
  k <- which.min(mAIC) 
  k
  
# Specify best in-sample modeL
  Y_in_fit <- arima(Y_in, order = c(k,0,0),
                    method = "ML")
  
####### Tests:

### Autocorrelation in the error terms - Ljung & Box:
  AR_3r1_box4 <- Box.test(resid(Y_in_fit), lag = 4, type = "Ljung")
  AR_3r1_box8 <- Box.test(resid(Y_in_fit), lag = 8, type = "Ljung")
  AR_3r1_box12 <- Box.test(resid(Y_in_fit), lag = 12, type = "Ljung")
  AR_3r1_box24 <- Box.test(resid(Y_in_fit), lag = 24, type = "Ljung") 
  
### Homoskedasticity: 
  AR_3r1_arch <- arch.test(Y_in_fit) # Very high p-values -> no problem. 
  
### Test for normality:
  AR_3r1_jarque_bera <- jarque.bera.test(resid(Y_in_fit))
  
### Make a table
  AR_3r1_test_matrix <- matrix(data = NA, nrow = 4, ncol = 5)
  AR_3r1_test_matrix[1,c(1,3)] <- 4
  AR_3r1_test_matrix[2,c(1,3)] <- 8
  AR_3r1_test_matrix[3,c(1,3)] <- 12
  AR_3r1_test_matrix[4,c(1,3)] <- 24
  
  colnames(AR_3r1_test_matrix) <- c("Lags", "P-value","Lags","P-value", "P-value")
  
# Insert values:
  AR_3r1_test_matrix[1,2] = AR_3r1_box4$p.value
  AR_3r1_test_matrix[2,2] = AR_3r1_box8$p.value
  AR_3r1_test_matrix[3,2] = AR_3r1_box12$p.value
  AR_3r1_test_matrix[4,2] = AR_3r1_box24$p.value
  
  AR_3r1_test_matrix[1,4] = AR_3r1_arch  [1,5]
  AR_3r1_test_matrix[2,4] = AR_3r1_arch  [2,5]
  AR_3r1_test_matrix[3,4] = AR_3r1_arch  [3,5]
  AR_3r1_test_matrix[4,4] = AR_3r1_arch  [6,5]
  
  AR_3r1_test_matrix[1,5] <- AR_3r1_jarque_bera $p.value
  
# Print to latex: 
  print(xtable(AR_3r1_test_matrix, digits = 2), file = "AR_3r1_test_matrix.tex")

####### Forecasting:
  
### Apply function to generate forecasts:
  c1 <- data.frame(matrix(data = NA, nrow = 204-w_size, ncol = 12)) # Container for h=1 steap-a-head forecast.
  for(j in 1:12){
    expandinglist <- expanding_forecast_arima(X = r_t, w_size = w_size, h=j, p=k, q=0) # NB: only 1,3,6 and 12 will be considered.
    for(i in 1:(204-w_size)){
      c1[i,j] <- as.data.frame(expandinglist[[expanding_counter+i]][j])
    }
  }
  
### Plot the above forecasts:
  
# Bind the data with the original data:
  c1 <- cbind(c1,tail(r_t, n = (204-w_size)))
  colnames(c1)[1:12] <- c("h=1","h=2","h=3","h=4","h=5","h=6",
                          "h=7","h=8","h=9","h=10", "h=11","h=12")
  colnames(c1)[14] <- c("Data")
  rownames(c1) <- rownames(r_t$Date)
  
# Plot: 
  c1_plot <- (ggplot(data = c1,aes(x = `Date`)) +
                geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                geom_line(aes(y = `h=1`, colour = "h = 1")) +
                geom_line(aes(y = `h=3`, colour = "h = 3")) +
                geom_line(aes(y = `h=6`, colour = "h = 6")) +
                geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                
                # Adjustment of axis & titles:
                labs(title = "Forecasts of AR(3)", y = "%") +
                theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))  
  c1_plot
  

  
############################# Factor model (AR with factors):
  
####### Best model in-sample:
  
# Open loop to find the best model:
  for(i in 1:p_y){
    mfit_AR_factor <- arima(Y_in,order = c(i,0,0), xreg = mX_in_2,
                            method = "ML")
    mAIC[i] <- AIC(mfit_AR_factor, k = 2)
  }
  k <- which.min(mAIC) 
  k
  
# Specify best in-sample modeL
  Y_in_fit_factor <- arima(Y_in,order = c(k,0,0), xreg = mX_in_2,
                           method = "ML")
  
####### Tests:
  
### Autocorrelation in the error terms - Ljung & Box:
  factor_modelr1_box4 <- Box.test(resid(Y_in_fit_factor), lag = 4, type = "Ljung")
  factor_modelr1_box8 <- Box.test(resid(Y_in_fit_factor), lag = 8, type = "Ljung")
  factor_modelr1_box12 <- Box.test(resid(Y_in_fit_factor), lag = 12, type = "Ljung")
  factor_modelr1_box24 <- Box.test(resid(Y_in_fit_factor), lag = 24, type = "Ljung") 
  
### Homoskedasticity: 
  factor_modelr1_arch <- arch.test(Y_in_fit_factor) 
  
### Test for normality:
  factor_modelr1_jarque_bera <- jarque.bera.test(resid(Y_in_fit_factor))
  
### Make a table
  factor_modelr1_test_matrix <- matrix(data = NA, nrow = 4, ncol = 5)
  factor_modelr1_test_matrix[1,c(1,3)] <- 4
  factor_modelr1_test_matrix[2,c(1,3)] <- 8
  factor_modelr1_test_matrix[3,c(1,3)] <- 12
  factor_modelr1_test_matrix[4,c(1,3)] <- 24
  
  colnames(factor_modelr1_test_matrix) <- c("Lags", "P-value","Lags","P-value", "P-value")
  
# Insert values:
  factor_modelr1_test_matrix[1,2] = factor_modelr1_box4$p.value
  factor_modelr1_test_matrix[2,2] = factor_modelr1_box8$p.value
  factor_modelr1_test_matrix[3,2] = factor_modelr1_box12$p.value
  factor_modelr1_test_matrix[4,2] = factor_modelr1_box24$p.value
  
  factor_modelr1_test_matrix[1,4] = factor_modelr1_arch  [1,5]
  factor_modelr1_test_matrix[2,4] = factor_modelr1_arch [2,5]
  factor_modelr1_test_matrix[3,4] = factor_modelr1_arch  [3,5]
  factor_modelr1_test_matrix[4,4] = factor_modelr1_arch [6,5]
  
  factor_modelr1_test_matrix[1,5] <- factor_modelr1_jarque_bera $p.value
  
# Print to latex: 
  print(xtable(factor_modelr1_test_matrix, digits = 2), file = "factor_modelr1_test_matrix.tex")
  
####### Forecasting:
  
### Apply function to generate forecasts:
  c2_factor <- data.frame(matrix(data = NA, nrow = (204-w_size), ncol = 12)) # Container for h=1 steap-a-head forecast.
  for(j in 1:12){
    expandinglist <- expanding_f_2(y = r_t,X_reg = mGT_log_transformed_ts, h = j, w_size = w_size, p = k,d=0,q=0)# NB: only 1,3,6 and 12 will be considered.
    for(i in 1:(204-w_size)){
      c2_factor[i,j] <- as.data.frame(expandinglist[[expanding_counter+i]][j])
    }
  }
  
### Plot the above forecasts:
  
# Bind the data with the original data:
  c2_factor <- cbind(c2_factor,tail(r_t, n = 204-w_size))
  colnames(c2_factor )[1:12] <- c("h=1","h=2","h=3","h=4","h=5","h=6",
                                  "h=7","h=8","h=9","h=10", "h=11","h=12")
  colnames(c2_factor )[14] <- c("Data")
  rownames(c2_factor ) <- rownames(r_t$Date)
  
# Plot: 
  c2_factor_plot <- (ggplot(data = c2_factor,aes(x = `Date`)) +
                       geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                       geom_line(aes(y = `h=1`, colour = "h = 1")) +
                       geom_line(aes(y = `h=3`, colour = "h = 3")) +
                       geom_line(aes(y = `h=6`, colour = "h = 6")) +
                       geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                       
                       # Adjustment of axis & titles:
                       labs(title = "Forecasts of Factor model", y = "%") +
                       theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                       scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))  
  c2_factor_plot
  
############################# AR model with regressors choosen by best subset algorithm.
  
####### Best model in-sample:
  
# Open loop to find the best model:
  for(i in 1:p_y){
    mfit_AR_best_subset <- arima(Y_in,order = c(i,0,0), xreg = mX_in_3,
                                 method = "ML")
    mAIC[i] <- AIC(mfit_AR_best_subset, k = 2)
  }
  k <- which.min(mAIC) 
  k
# Specify best in-sample modeL
  Y_in_fit_best_subset <- arima(Y_in,order = c(k,0,0), xreg = mX_in_3,
                                method = "ML")
  
  
####### Tests:
  
### Autocorrelation in the error terms - Ljung & Box:
  best_subset_modelr1_box4 <- Box.test(resid(Y_in_fit_best_subset), lag = 4, type = "Ljung")
  best_subset_modelr1_box8 <- Box.test(resid(Y_in_fit_best_subset), lag = 8, type = "Ljung")
  best_subset_modelr1_box12 <- Box.test(resid(Y_in_fit_best_subset), lag = 12, type = "Ljung")
  best_subset_modelr1_box24 <- Box.test(resid(Y_in_fit_best_subset), lag = 24, type = "Ljung") 
  
### Homoskedasticity: 
  best_subset_modelr1_arch <- arch.test(Y_in_fit_best_subset) 
  
### Test for normality:
  best_subset_modelr1_jarque_bera <- jarque.bera.test(resid(Y_in_fit_best_subset))
  
### Make a table
  best_subset_modelr1_test_matrix <- matrix(data = NA, nrow = 4, ncol = 5)
  best_subset_modelr1_test_matrix[1,c(1,3)] <- 4
  best_subset_modelr1_test_matrix[2,c(1,3)] <- 8
  best_subset_modelr1_test_matrix[3,c(1,3)] <- 12
  best_subset_modelr1_test_matrix[4,c(1,3)] <- 24
  
  colnames(best_subset_modelr1_test_matrix) <- c("Lags", "P-value","Lags","P-value", "P-value")
  
# Insert values:
  best_subset_modelr1_test_matrix[1,2] = best_subset_modelr1_box4$p.value
  best_subset_modelr1_test_matrix[2,2] = best_subset_modelr1_box8$p.value
  best_subset_modelr1_test_matrix[3,2] = best_subset_modelr1_box12$p.value
  best_subset_modelr1_test_matrix[4,2] = best_subset_modelr1_box24$p.value
  
  best_subset_modelr1_test_matrix[1,4] = best_subset_modelr1_arch   [1,5]
  best_subset_modelr1_test_matrix[2,4] = best_subset_modelr1_arch  [2,5]
  best_subset_modelr1_test_matrix[3,4] = best_subset_modelr1_arch   [3,5]
  best_subset_modelr1_test_matrix[4,4] = best_subset_modelr1_arch [6,5]
  
  best_subset_modelr1_test_matrix[1,5] <- best_subset_modelr1_jarque_bera  $p.value
  
# Print to latex: 
  print(xtable(best_subset_modelr1_test_matrix, digits = 2), file = "best_subset_modelr1_test_matrix.tex")
  
  
####### Forecasting:
  
### Apply function to generate forecasts:
  c3_best_subset <- data.frame(matrix(data = NA, nrow = 204-w_size, ncol = 12)) # Container for h=1 steap-a-head forecast.
  for(j in 1:12){
    expandinglist <- expanding_f_3(y=r_t,X_reg1 = mGT_log_transformed_ts,
                                   X_reg1_1 = c("THE.CRISIS","Cash","Bubble","Dividend","Society"), # Regressors choosen by best_subset.
                                   h = j, w_size = w_size, p = k, d= 0, q = 0)
    for(i in 1:(204-w_size)){
      c3_best_subset[i,j] <- as.data.frame(expandinglist[[expanding_counter+i]][j])
    }
  }
  
### Plot the above forecasts:
  
# Bind the data with the original data:
  c3_best_subset <- cbind(c3_best_subset,tail(r_t, n = 204-w_size))
  colnames(c3_best_subset)[1:12] <- c("h=1","h=2","h=3","h=4","h=5","h=6",
                                      "h=7","h=8","h=9","h=10", "h=11","h=12")
  colnames(c3_best_subset)[14] <- c("Data")
  rownames(c3_best_subset) <- rownames(r_t$Date)
  
# Plot: 
  c3_best_subset_plot <- (ggplot(data = c3_best_subset,aes(x = `Date`)) +
                            geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                            geom_line(aes(y = `h=1`, colour = "h = 1")) +
                            geom_line(aes(y = `h=3`, colour = "h = 3")) +
                            geom_line(aes(y = `h=6`, colour = "h = 6")) +
                            geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                            
                            # Adjustment of axis & titles:
                            labs(title = "Forecasts of best subset model", y = "%") +
                            theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                            scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))  
  c3_best_subset_plot
  
############################# Forecast combination.
  
###### Preparation:
  
### Bind all the fitted values in a object:
  pred_matrix <- cbind(fitted(Y_in_fit), fitted(Y_in_fit_factor),
                       fitted(Y_in_fit_best_subset))
  pred_matrix <- as.matrix(pred_matrix)
  
###### Forecast: 
  
### h = 1
  
# Specify horizon: 
  horizon <- 1
  
### Bind all the forecats in a object:
  for_matrix_h1 <- cbind(c1[,1],c2_factor[,horizon],c3_best_subset[,horizon])
  colnames(for_matrix_h1) <- c("AR(3)","Factor model", "Best subset model")  
  
### Now set up the foreccomb object & do forecast:
  foreccomb_h1 <- foreccomb(observed_vector = r_t[1:w_size,2], prediction_matrix = pred_matrix,
                            newobs = r_t[(w_size + 1):204,2], newpreds = for_matrix_h1)
  
# Simple average:
  mean_comb_h1 <- comb_SA(foreccomb_h1)
  
# Bates/Granger combination:
  Bates_Granger_h1 <- comb_BG(foreccomb_h1)
  
# OLS combination:
  OLS_combination_h1 <- comb_OLS(foreccomb_h1)
  
### h = 3
  
# Specify horizon: 
  horizon <- 3
  
### Bind all the forecats in a object:
  for_matrix_h3 <- cbind(c1[,1],c2_factor[,horizon],c3_best_subset[,horizon])
  colnames(for_matrix_h3) <- c("AR(3)","Factor model", "Best subset model")  
  
### Now set up the foreccomb object & do forecast:
  foreccomb_h3 <- foreccomb(observed_vector = r_t[1:w_size,2], prediction_matrix = pred_matrix,
                            newobs = r_t[(w_size+1):204,2], newpreds = for_matrix_h3)
  
# Simple average:
  mean_comb_h3 <- comb_SA(foreccomb_h3)
  
# Bates/Granger combination:
  Bates_Granger_h3 <- comb_BG(foreccomb_h3)
  
# OLS combination:
  OLS_combination_h3 <- comb_OLS(foreccomb_h3)
  
### h = 6
  
# Specify horizon: 
  horizon <- 6
  
### Bind all the forecats in a object:
  for_matrix_h6 <- cbind(c1[,1],c2_factor[,horizon],c3_best_subset[,horizon])
  colnames(for_matrix_h6) <- c("AR(3)","Factor model", "Best subset model")  
  
### Now set up the foreccomb object & do forecast:
  foreccomb_h6 <- foreccomb(observed_vector = r_t[1:w_size,2], prediction_matrix = pred_matrix,
                            newobs = r_t[(w_size+1):204,2], newpreds = for_matrix_h6)
  
# Simple average:
  mean_comb_h6 <- comb_SA(foreccomb_h6)
  
# Bates/Granger combination:
  Bates_Granger_h6 <- comb_BG(foreccomb_h6)
  
# OLS combination:
  OLS_combination_h6 <- comb_OLS(foreccomb_h6)
  
### h = 12
  
# Specify horizon: 
  horizon <- 12
  
### Bind all the forecats in a object:
  for_matrix_h12 <- cbind(c1[,1],c2_factor[,horizon],c3_best_subset[,horizon])
  colnames(for_matrix_h12) <- c("AR(3)","Factor model", "Best subset model")  
  
### Now set up the foreccomb object & do forecast:
  foreccomb_h12 <- foreccomb(observed_vector = r_t[1:w_size,2], prediction_matrix = pred_matrix,
                             newobs = r_t[(w_size+1):204,2], newpreds = for_matrix_h12)
  
# Simple average:
  mean_comb_h12 <- comb_SA(foreccomb_h12)
  
# Bates/Granger combination:
  Bates_Granger_h12 <- comb_BG(foreccomb_h12)
  
# OLS combination:
  OLS_combination_h12 <- comb_OLS(foreccomb_h12)
  
### Plot the forecast combinations:
  
# Simple average:  <------
  simple_average_forecast_comb <- cbind(mean_comb_h1$Forecasts_Test,
                                        mean_comb_h3$Forecasts_Test,
                                        mean_comb_h6$Forecasts_Test,
                                        mean_comb_h12$Forecasts_Test,
                                        tail(r_t, n = (204-w_size)))
  
  colnames(simple_average_forecast_comb )[1:4] <- c("h=1","h=3","h=6","h=12")
  colnames(simple_average_forecast_comb)[6] <- c("Data")
  rownames(simple_average_forecast_comb) <- rownames(r_t$Date)
  
# Plot:
  simple_average_forecast_comb_plot <- (ggplot(data = simple_average_forecast_comb,aes(x = `Date`)) +
                                          geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                                          geom_line(aes(y = `h=1`, colour = "h = 1")) +
                                          geom_line(aes(y = `h=3`, colour = "h = 3")) +
                                          geom_line(aes(y = `h=6`, colour = "h = 6")) +
                                          geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                                          
                                          # Adjustment of axis & titles:
                                          labs(title = "Forecast combination - Simple average", y = "%") +
                                          theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                                          scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))
  
  simple_average_forecast_comb_plot
  
# Bates/Granger: <------
  Bates_Granger_forecast_comb <- cbind(Bates_Granger_h1$Forecasts_Test,
                                       Bates_Granger_h3$Forecasts_Test,
                                       Bates_Granger_h6$Forecasts_Test,
                                       Bates_Granger_h12$Forecasts_Test,
                                       tail(r_t, n = 204-w_size))
  
  colnames(Bates_Granger_forecast_comb )[1:4] <- c("h=1","h=3","h=6","h=12")
  colnames(Bates_Granger_forecast_comb)[6] <- c("Data")
  rownames(Bates_Granger_forecast_comb) <- rownames(r_t$Date)
  
# Plot:
  Bates_Granger_forecast_comb_plot <- (ggplot(data = Bates_Granger_forecast_comb,aes(x = `Date`)) +
                                         geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                                         geom_line(aes(y = `h=1`, colour = "h = 1")) +
                                         geom_line(aes(y = `h=3`, colour = "h = 3")) +
                                         geom_line(aes(y = `h=6`, colour = "h = 6")) +
                                         geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                                         
                                         # Adjustment of axis & titles:
                                         labs(title = "Forecast combination - Bates/Granger", y = "%") +
                                         theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                                         scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))
  
  Bates_Granger_forecast_comb_plot
  
# OLS combination: <------
  OLS_combination_forecast_comb <- cbind(OLS_combination_h1$Forecasts_Test,
                                         OLS_combination_h3$Forecasts_Test,
                                         OLS_combination_h6$Forecasts_Test,
                                         OLS_combination_h12$Forecasts_Test,
                                         tail(r_t, n = 204-w_size))
  
  colnames(OLS_combination_forecast_comb )[1:4] <- c("h=1","h=3","h=6","h=12")
  colnames(OLS_combination_forecast_comb)[6] <- c("Data")
  rownames(OLS_combination_forecast_comb) <- rownames(r_t$Date)
  
# Plot:
  OLS_combination_forecast_comb_plot <- (ggplot(data = OLS_combination_forecast_comb,aes(x = `Date`)) +
                                           geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                                           geom_line(aes(y = `h=1`, colour = "h = 1")) +
                                           geom_line(aes(y = `h=3`, colour = "h = 3")) +
                                           geom_line(aes(y = `h=6`, colour = "h = 6")) +
                                           geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                                           
                                           # Adjustment of axis & titles:
                                           labs(title = "Forecast combination - OLS combination", y = "%") +
                                           theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                                           scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))
  
  OLS_combination_forecast_comb_plot
  
  
############################################################################
############################# Errors of forecasts: #########################
############################################################################
  
############################# Setup:
  m_Error <- matrix(data = NA, nrow = 22, ncol = 4) # Container.
  rownames(m_Error) <- c("Benchmark",
                         "RMSE",
                         "MAE",
                         "AR(3)",
                         "RMSE",
                         "MAE",
                         "Factor model",
                         "RMSE",
                         "MAE",
                         "Best Subset model",
                         "RMSE",
                         "MAE",
                         "Forecast combination",
                         "Simple average",
                         "RMSE",
                         "MAE",
                         "Bates/Granger",
                         "RMSE",
                         "MAE",
                         "OLS combination",
                         "RMSE",
                         "MAE"
  )
  colnames(m_Error) <- c("h = 1", "h = 3", "h = 6", "h = 12")
###### Benchmark:

  # RMSE:
  m_Error[2,1]  = for_errors(y = r_t[,2], yhat = c0[,1],
                             partion = 0)$rmse
  m_Error[2,2]  = for_errors(y = r_t[,2], yhat = c0[,3],
                             partion = 0)$rmse
  m_Error[2,3]  = for_errors(y = r_t[,2], yhat = c0[,6],
                             partion = 0)$rmse
  m_Error[2,4]  = for_errors(y = r_t[,2], yhat = c0[,12],
                             partion = 0)$rmse
  
  # MAE:
  m_Error[3,1]  = for_errors(y = r_t[,2], yhat = c0[,1],
                             partion = 0)$mae
  m_Error[3,2]  = for_errors(y = r_t[,2], yhat = c0[,3],
                             partion = 0)$mae
  m_Error[3,3]  = for_errors(y = r_t[,2], yhat = c0[,6],
                             partion = 0)$mae
  m_Error[3,4]  = for_errors(y = r_t[,2], yhat = c0[,12],
                             partion = 0)$mae

###### AR(3):
  
### Period: 2008M1:2009M12
  
  # RMSE:
  m_Error[5,1]  = for_errors(y = r_t[,2], yhat = c1[,1],
                             partion = 0)$rmse
  m_Error[5,2]  = for_errors(y = r_t[,2], yhat = c1[,3],
                             partion = 0)$rmse
  m_Error[5,3]  = for_errors(y = r_t[,2], yhat = c1[,6],
                             partion = 0)$rmse
  m_Error[5,4]  = for_errors(y = r_t[,2], yhat = c1[,12],
                             partion = 0)$rmse
  
  # MAE:
  m_Error[6,1]  = for_errors(y = r_t[,2], yhat = c1[,1],
                             partion = 0)$mae
  m_Error[6,2]  = for_errors(y = r_t[,2], yhat = c1[,3],
                             partion = 0)$mae
  m_Error[6,3]  = for_errors(y = r_t[,2], yhat = c1[,6],
                             partion = 0)$mae
  m_Error[6,4]  = for_errors(y = r_t[,2], yhat = c1[,12],
                             partion = 0)$mae

###### Factor model:
  
  # RMSE:
  m_Error[8,1]  = for_errors(y = r_t[,2], yhat = c2_factor[,1],
                             partion = 0)$rmse
  m_Error[8,2]  = for_errors(y = r_t[,2], yhat = c2_factor[,3],
                             partion = 0)$rmse
  m_Error[8,3]  = for_errors(y = r_t[,2], yhat = c1[,6],
                             partion = 0)$rmse
  m_Error[8,4]  = for_errors(y = r_t[,2], yhat = c1[,12],
                             partion = 0)$rmse
  
  # MAE:
  m_Error[9,1]  = for_errors(y = r_t[,2], yhat = c2_factor[,1],
                             partion = 0)$mae
  m_Error[9,2]  = for_errors(y = r_t[,2], yhat = c2_factor[,3],
                             partion = 0)$mae
  m_Error[9,3]  = for_errors(y = r_t[,2], yhat = c2_factor[,6],
                             partion = 0)$mae
  m_Error[9,4]  = for_errors(y = r_t[,2], yhat = c2_factor[,12],
                             partion = 0)$mae
 
###### Best subset model:

  # RMSE:
  m_Error[11,1]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,1],
                              partion = 0)$rmse
  m_Error[11,2]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,3],
                              partion = 0)$rmse
  m_Error[11,3]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,6],
                              partion = 0)$rmse
  m_Error[11,4]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,12],
                              partion = 0)$rmse
  
  # MAE:
  m_Error[12,1]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,1],
                              partion = 0)$mae
  m_Error[12,2]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,3],
                              partion = 0)$mae
  m_Error[12,3]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,6],
                              partion = 0)$mae
  m_Error[12,4]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,12],
                              partion = 0)$mae

###### Forecast combination (simple average):

  # RMSE:
  m_Error[15,1]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,1],
                              partion = 0)$rmse
  m_Error[15,2]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,2],
                              partion = 0)$rmse
  m_Error[15,3]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,3],
                              partion = 0)$rmse
  m_Error[15,4]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,4],
                              partion = 0)$rmse
  
  # MAE:
  m_Error[16,1]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,1],
                              partion = 0)$mae
  m_Error[16,2]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,2],
                              partion = 0)$mae
  m_Error[16,3]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,3],
                              partion = 0)$mae
  m_Error[16,4]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,4],
                              partion = 0)$mae

###### Forecast combination (Bates/Granger):

  # RMSE:
  m_Error[18,1]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,1],
                              partion = 0)$rmse
  m_Error[18,2]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,2],
                              partion = 0)$rmse
  m_Error[18,3]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,3],
                              partion = 0)$rmse
  m_Error[18,4]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,4],
                              partion = 0)$rmse
  
  # MAE:
  m_Error[19,1]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,1],
                              partion = 0)$mae
  m_Error[19,2]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,2],
                              partion = 0)$mae
  m_Error[19,3]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,3],
                              partion = 0)$mae
  m_Error[19,4]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,4],
                              partion = 0)$mae
  

  
  
###### Forecast combination (OLS-combination):

  # RMSE:
  m_Error[21,1]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,1],
                              partion = 0)$rmse
  m_Error[21,2]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,2],
                              partion = 0)$rmse
  m_Error[21,3]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,3],
                              partion = 0)$rmse
  m_Error[21,4]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,4],
                              partion = 0)$rmse
  
  # MAE:
  m_Error[22,1]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,1],
                              partion = 0)$mae
  m_Error[22,2]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,2],
                              partion = 0)$mae
  m_Error[22,3]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,3],
                              partion = 0)$mae
  m_Error[22,4]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,4],
                              partion = 0)$mae
  
############################# Out-of-sample R^2:
  
###### AR(3)
  
  AR3_R2OSh1_p1 <- out_of_sample_R2(r_t[,2], yhat = c1[,1], partion = 0, mean_forecast = c0[,1])
  AR3_R2OSh3_p1 <- out_of_sample_R2(r_t[,2], yhat = c1[,3], partion = 0, mean_forecast = c0[,3])
  AR3_R2OSh6_p1 <- out_of_sample_R2(r_t[,2], yhat = c1[,6], partion = 0, mean_forecast = c0[,6])
  AR3_R2OSh12_p1 <- out_of_sample_R2(r_t[,2], yhat = c1[,12], partion = 0, mean_forecast = c0[,12])
  
  # Print the results: 
  AR3_R2OSh1_p1
  AR3_R2OSh3_p1
  AR3_R2OSh6_p1
  AR3_R2OSh12_p1

###### Factor model: 
  
  factor_model_R2OSh1_p1 <- out_of_sample_R2(r_t[,2], yhat = c2_factor[,1], partion = 0, mean_forecast = c0[,1])
  factor_model_R2OSh3_p1 <- out_of_sample_R2(r_t[,2], yhat = c2_factor[,3], partion = 0, mean_forecast = c0[,3])
  factor_model_R2OSh6_p1 <- out_of_sample_R2(r_t[,2], yhat = c2_factor[,6], partion = 0, mean_forecast = c0[,6])
  factor_model_R2OSh12_p1 <- out_of_sample_R2(r_t[,2], yhat = c2_factor[,12], partion = 0, mean_forecast = c0[,12])
  
# Print the results: 
  factor_model_R2OSh1_p1
  factor_model_R2OSh3_p1
  factor_model_R2OSh6_p1
  factor_model_R2OSh12_p1
  
###### Best subset model: 
  
  c3_best_subset_R2OSh1_p1 <- out_of_sample_R2(r_t[,2], yhat = c3_best_subset[,1], partion = 0, mean_forecast = c0[,1])
  c3_best_subset_R2OSh3_p1 <- out_of_sample_R2(r_t[,2], yhat = c3_best_subset[,3], partion = 0, mean_forecast = c0[,3])
  c3_best_subset_R2OSh6_p1 <- out_of_sample_R2(r_t[,2], yhat = c3_best_subset[,6], partion = 0, mean_forecast = c0[,6])
  c3_best_subset_R2OSh12_p1 <- out_of_sample_R2(r_t[,2], yhat = c3_best_subset[,12], partion = 0, mean_forecast = c0[,12])
  
# Print the results: 
  c3_best_subset_R2OSh1_p1
  c3_best_subset_R2OSh3_p1
  c3_best_subset_R2OSh6_p1
  c3_best_subset_R2OSh12_p1

###### Forecast combination - simple average. 

  simple_average_forecast_comb_R2OSh1_p1 <- out_of_sample_R2(r_t[,2], yhat = simple_average_forecast_comb[,1], partion = 0, mean_forecast = c0[,1])
  simple_average_forecast_comb_R2OSh3_p1 <- out_of_sample_R2(r_t[,2], yhat = simple_average_forecast_comb[,2], partion = 0, mean_forecast = c0[,3])
  simple_average_forecast_comb_R2OSh6_p1 <- out_of_sample_R2(r_t[,2], yhat = simple_average_forecast_comb[,3], partion = 0, mean_forecast = c0[,6])
  simple_average_forecast_comb_R2OSh12_p1 <- out_of_sample_R2(r_t[,2], yhat = simple_average_forecast_comb[,4], partion = 0, mean_forecast = c0[,12])
  
# Print the results: 
  simple_average_forecast_comb_R2OSh1_p1
  simple_average_forecast_comb_R2OSh3_p1
  simple_average_forecast_comb_R2OSh6_p1
  simple_average_forecast_comb_R2OSh12_p1

###### Forecast combination - Bates/Granger

  Bates_Granger_forecast_comb_R2OSh1_p1 <- out_of_sample_R2(r_t[,2], yhat = Bates_Granger_forecast_comb[,1], partion = 0, mean_forecast = c0[,1])
  Bates_Granger_forecast_comb_R2OSh3_p1 <- out_of_sample_R2(r_t[,2], yhat = Bates_Granger_forecast_comb[,2], partion = 0,  mean_forecast = c0[,3])
  Bates_Granger_forecast_comb_R2OSh6_p1 <- out_of_sample_R2(r_t[,2], yhat = Bates_Granger_forecast_comb[,3], partion = 0,  mean_forecast = c0[,6])
  Bates_Granger_forecast_comb_R2OSh12_p1 <- out_of_sample_R2(r_t[,2], yhat = Bates_Granger_forecast_comb[,4], partion = 0,  mean_forecast = c0[,12])
  
# Print the results: 
  Bates_Granger_forecast_comb_R2OSh1_p1
  Bates_Granger_forecast_comb_R2OSh3_p1
  Bates_Granger_forecast_comb_R2OSh6_p1
  Bates_Granger_forecast_comb_R2OSh12_p1

###### Forecast combination - OLS 
  
  OLS_combination_forecast_comb_R2OSh1_p1 <- out_of_sample_R2(r_t[,2], yhat = OLS_combination_forecast_comb[,1], partion = 0,  mean_forecast = c0[,1])
  OLS_combination_forecast_comb_R2OSh3_p1 <- out_of_sample_R2(r_t[,2], yhat = OLS_combination_forecast_comb[,2], partion = 0,  mean_forecast = c0[,3])
  OLS_combination_forecast_comb_R2OSh6_p1 <- out_of_sample_R2(r_t[,2], yhat = OLS_combination_forecast_comb[,3], partion = 0,  mean_forecast = c0[,6])
  OLS_combination_forecast_comb_R2OSh12_p1 <- out_of_sample_R2(r_t[,2], yhat = OLS_combination_forecast_comb[,4], partion = 0,  mean_forecast = c0[,12])
  
# Print: 
  OLS_combination_forecast_comb_R2OSh1_p1
  OLS_combination_forecast_comb_R2OSh3_p1
  OLS_combination_forecast_comb_R2OSh6_p1
  OLS_combination_forecast_comb_R2OSh12_p1

###########################################################################################################################
############################# Robustness check 2: #########################################################################
###########################################################################################################################
  
############################# Expanding window: w_size -> 132 (first forecast point is 2015:M1)
  
  
###### Model overview:
  # benchmark_model: Benchmark.
  # Forecast: c0
  # Y_in_fit: AR(3)
  # Forecast: c1.
  # Y_in_fit_factor: AR with external regressors.
  # Forecast: c2.
  
###### Generel setup: 
  w_size <- 132
  p_y <- 10
  mAIC <- rep(NA, p_y) # p_y is max lag.
  Y_in <- as.ts(r_t[1:w_size,2], start = c(2004,1), frequency = 12)
  
  y <- r_t
  X_reg <- mGT_log_transformed_ts
  F_df <- EstFactors(X_reg,ic)
  F_df2 <- F_df[,-c(1)]
  X_reg2 <- X_reg[,-c(1)]
  X <- cbind(y,F_df2,X_reg2)
  mX_in_2 <- X[1:w_size,3:5]
  mX_in_2 <- as.matrix(mX_in_2)
  
  X2 <- cbind(y,X_reg2)
  mX_in_3 <- X2[1:w_size,c("THE.CRISIS","Cash","Bubble","Dividend","Society")]
  mX_in_3 <- as.matrix(mX_in_3)
  
############################# Benchmark:
  
####### Best model in-sample:
  
# Fit a arima model only with mean and noise:
  bechmark_model <- arima(Y_in, order = c(0,0,0),
                          method = "ML")
  
####### Tests:

  ### Autocorrelation in the error terms - Ljung & Box: (H0: No autocorrelation.)
  Box.test(resid(bechmark_model), lag = 4, type = "Ljung")
  Box.test(resid(bechmark_model), lag = 8, type = "Ljung")
  Box.test(resid(bechmark_model), lag = 12, type = "Ljung")
  Box.test(resid(bechmark_model), lag = 24, type = "Ljung") 
  
  ### Homoskedasticity (H0: no heteroskedasticity): 
  arch.test(bechmark_model) # Very high p-values -> no problem. 
  
  ### Test for normality (H0: Skew = 0 an excess kurtosis of 0):
  jarque.bera.test(resid(bechmark_model)) # No problem.
  
  ####### Tests:
  
  ### Autocorrelation in the error terms - Ljung & Box:
  benchmarkr1_box4 <- Box.test(resid(bechmark_model), lag = 4, type = "Ljung")
  benchmarkr1_box8 <- Box.test(resid(bechmark_model), lag = 8, type = "Ljung")
  benchmarkr1_box12 <- Box.test(resid(bechmark_model), lag = 12, type = "Ljung")
  benchmarkr1_box24 <- Box.test(resid(bechmark_model), lag = 24, type = "Ljung") # No problem. Even for high number of lags.  
  
  ### Homoskedasticity: 
  benchmarkr1_arch <- arch.test(bechmark_model) # Very high p-values -> no problem. 
  
  ### Test for normality:
  benchmarkr1_jarque_bera <- jarque.bera.test(resid(bechmark_model))
  
  ### Make a table
  benchmarkr1_test_matrix <- matrix(data = NA, nrow = 4, ncol = 5)
  benchmarkr1_test_matrix[1,c(1,3)] <- 4
  benchmarkr1_test_matrix[2,c(1,3)] <- 8
  benchmarkr1_test_matrix[3,c(1,3)] <- 12
  benchmarkr1_test_matrix[4,c(1,3)] <- 24
  
  colnames(benchmarkr1_test_matrix) <- c("Lags", "P-value","Lags","P-value", "P-value")
  
  # Insert values:
  benchmarkr1_test_matrix[1,2] = benchmarkr1_box4$p.value
  benchmarkr1_test_matrix[2,2] = benchmarkr1_box8$p.value
  benchmarkr1_test_matrix[3,2] = benchmarkr1_box12$p.value
  benchmarkr1_test_matrix[4,2] = benchmarkr1_box24$p.value
  
  benchmarkr1_test_matrix[1,4] = benchmarkr1_arch [1,5]
  benchmarkr1_test_matrix[2,4] = benchmarkr1_arch [2,5]
  benchmarkr1_test_matrix[3,4] = benchmarkr1_arch [3,5]
  benchmarkr1_test_matrix[4,4] = benchmarkr1_arch [6,5]
  
  benchmarkr1_test_matrix[1,5] <- benchmarkr1_jarque_bera $p.value
  
  # Print to latex: 
  print(xtable(benchmarkr1_test_matrix, digits = 2), file = "benchmarkr2_test_matrix.tex")
  
  
####### Forecasting:
  
### Apply function to generate forecasts:
  expanding_counter = 216
  c0 <- data.frame(matrix(data = NA, nrow = 204-w_size, ncol = 12)) # Container for h=1 steap-a-head forecast.
  for(j in 1:12){
    expandinglist <- expanding_forecast_arima(X = r_t, w_size = w_size, h=j, p=0, q=0) # NB: only 1,3,6 and 12 will be considered.
    for(i in 1:(204-w_size)){
      c0[i,j] <- as.data.frame(expandinglist[[expanding_counter+i]][j])
    }
  }
  

  
### Plot the above forecasts:
  
# Bind the data with the original data:
  c0 <- cbind(c0,tail(r_t, n = 204-w_size))
  colnames(c0)[1:12] <- c("h=1","h=2","h=3","h=4","h=5","h=6",
                          "h=7","h=8","h=9","h=10", "h=11","h=12")
  colnames(c0)[14] <- c("Data")
  rownames(c0) <- rownames(r_t$Date)
  
# Plot: 
  c0_plot <- (ggplot(data = c0,aes(x = `Date`)) +
                geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                geom_line(aes(y = `h=1`, colour = "h = 1")) +
                geom_line(aes(y = `h=3`, colour = "h = 3")) +
                geom_line(aes(y = `h=6`, colour = "h = 6")) +
                geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                
                # Adjustment of axis & titles:
                labs(title = "Forecasts of benchmark model", y = "%") +
                theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))  
  c0_plot
  
  
############################# AR:
  
####### Best model in-sample:
  
# Open loop to find the best model:
  for(i in 1:p_y){
    mfit_AR <- arima(Y_in, order = c(i,0,0),
                     method = "ML")
    mAIC[i] <- AIC(mfit_AR, k = 2)
  }
  k <- which.min(mAIC) 
  k
  
# Specify best in-sample modeL
  Y_in_fit <- arima(Y_in, order = c(k,0,0),
                    method = "ML")
  
####### Tests:
  
  ### Autocorrelation in the error terms - Ljung & Box:
  AR_3r1_box4 <- Box.test(resid(Y_in_fit), lag = 4, type = "Ljung")
  AR_3r1_box8 <- Box.test(resid(Y_in_fit), lag = 8, type = "Ljung")
  AR_3r1_box12 <- Box.test(resid(Y_in_fit), lag = 12, type = "Ljung")
  AR_3r1_box24 <- Box.test(resid(Y_in_fit), lag = 24, type = "Ljung") 
  
  ### Homoskedasticity: 
  AR_3r1_arch <- arch.test(Y_in_fit) # Very high p-values -> no problem. 
  
  ### Test for normality:
  AR_3r1_jarque_bera <- jarque.bera.test(resid(Y_in_fit))
  
  ### Make a table
  AR_3r1_test_matrix <- matrix(data = NA, nrow = 4, ncol = 5)
  AR_3r1_test_matrix[1,c(1,3)] <- 4
  AR_3r1_test_matrix[2,c(1,3)] <- 8
  AR_3r1_test_matrix[3,c(1,3)] <- 12
  AR_3r1_test_matrix[4,c(1,3)] <- 24
  
  colnames(AR_3r1_test_matrix) <- c("Lags", "P-value","Lags","P-value", "P-value")
  
  # Insert values:
  AR_3r1_test_matrix[1,2] = AR_3r1_box4$p.value
  AR_3r1_test_matrix[2,2] = AR_3r1_box8$p.value
  AR_3r1_test_matrix[3,2] = AR_3r1_box12$p.value
  AR_3r1_test_matrix[4,2] = AR_3r1_box24$p.value
  
  AR_3r1_test_matrix[1,4] = AR_3r1_arch  [1,5]
  AR_3r1_test_matrix[2,4] = AR_3r1_arch  [2,5]
  AR_3r1_test_matrix[3,4] = AR_3r1_arch  [3,5]
  AR_3r1_test_matrix[4,4] = AR_3r1_arch  [6,5]
  
  AR_3r1_test_matrix[1,5] <- AR_3r1_jarque_bera $p.value
  
  # Print to latex: 
  print(xtable(AR_3r1_test_matrix, digits = 2), file = "AR_3r2_test_matrix.tex")
  
####### Forecasting:
  
### Apply function to generate forecasts:
  c1 <- data.frame(matrix(data = NA, nrow = 204-w_size, ncol = 12)) # Container for h=1 steap-a-head forecast.
  for(j in 1:12){
    expandinglist <- expanding_forecast_arima(X = r_t, w_size = w_size, h=j, p=k, q=0) # NB: only 1,3,6 and 12 will be considered.
    for(i in 1:(204-w_size)){
      c1[i,j] <- as.data.frame(expandinglist[[expanding_counter+i]][j])
    }
  }
  
### Plot the above forecasts:
  
# Bind the data with the original data:
  c1 <- cbind(c1,tail(r_t, n = (204-w_size)))
  colnames(c1)[1:12] <- c("h=1","h=2","h=3","h=4","h=5","h=6",
                          "h=7","h=8","h=9","h=10", "h=11","h=12")
  colnames(c1)[14] <- c("Data")
  rownames(c1) <- rownames(r_t$Date)
  
# Plot: 
  c1_plot <- (ggplot(data = c1,aes(x = `Date`)) +
                geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                geom_line(aes(y = `h=1`, colour = "h = 1")) +
                geom_line(aes(y = `h=3`, colour = "h = 3")) +
                geom_line(aes(y = `h=6`, colour = "h = 6")) +
                geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                
                # Adjustment of axis & titles:
                labs(title = "Forecasts of AR(3)", y = "%") +
                theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))  
  c1_plot
  
  
  
############################# Factor model (AR with factors):
  
####### Best model in-sample:
  
# Open loop to find the best model:
  for(i in 1:p_y){
    mfit_AR_factor <- arima(Y_in,order = c(i,0,0), xreg = mX_in_2,
                            method = "ML")
    mAIC[i] <- AIC(mfit_AR_factor, k = 2)
  }
  k <- which.min(mAIC) 
  k
  
# Specify best in-sample modeL
  Y_in_fit_factor <- arima(Y_in,order = c(k,0,0), xreg = mX_in_2,
                           method = "ML")
  
####### Tests:
  
  ### Autocorrelation in the error terms - Ljung & Box:
  factor_modelr1_box4 <- Box.test(resid(Y_in_fit_factor), lag = 4, type = "Ljung")
  factor_modelr1_box8 <- Box.test(resid(Y_in_fit_factor), lag = 8, type = "Ljung")
  factor_modelr1_box12 <- Box.test(resid(Y_in_fit_factor), lag = 12, type = "Ljung")
  factor_modelr1_box24 <- Box.test(resid(Y_in_fit_factor), lag = 24, type = "Ljung") 
  
  ### Homoskedasticity: 
  factor_modelr1_arch <- arch.test(Y_in_fit_factor) 
  
  ### Test for normality:
  factor_modelr1_jarque_bera <- jarque.bera.test(resid(Y_in_fit_factor))
  
  ### Make a table
  factor_modelr1_test_matrix <- matrix(data = NA, nrow = 4, ncol = 5)
  factor_modelr1_test_matrix[1,c(1,3)] <- 4
  factor_modelr1_test_matrix[2,c(1,3)] <- 8
  factor_modelr1_test_matrix[3,c(1,3)] <- 12
  factor_modelr1_test_matrix[4,c(1,3)] <- 24
  
  colnames(factor_modelr1_test_matrix) <- c("Lags", "P-value","Lags","P-value", "P-value")
  
  # Insert values:
  factor_modelr1_test_matrix[1,2] = factor_modelr1_box4$p.value
  factor_modelr1_test_matrix[2,2] = factor_modelr1_box8$p.value
  factor_modelr1_test_matrix[3,2] = factor_modelr1_box12$p.value
  factor_modelr1_test_matrix[4,2] = factor_modelr1_box24$p.value
  
  factor_modelr1_test_matrix[1,4] = factor_modelr1_arch  [1,5]
  factor_modelr1_test_matrix[2,4] = factor_modelr1_arch [2,5]
  factor_modelr1_test_matrix[3,4] = factor_modelr1_arch  [3,5]
  factor_modelr1_test_matrix[4,4] = factor_modelr1_arch [6,5]
  
  factor_modelr1_test_matrix[1,5] <- factor_modelr1_jarque_bera $p.value
  
  # Print to latex: 
  print(xtable(factor_modelr1_test_matrix, digits = 2), file = "factor_modelr2_test_matrix.tex")
  
####### Forecasting:
  
### Apply function to generate forecasts:
  c2_factor <- data.frame(matrix(data = NA, nrow = (204-w_size), ncol = 12)) # Container for h=1 steap-a-head forecast.
  for(j in 1:12){
    expandinglist <- expanding_f_2(y = r_t,X_reg = mGT_log_transformed_ts, h = j, w_size = w_size, p = k,d=0,q=0)# NB: only 1,3,6 and 12 will be considered.
    for(i in 1:(204-w_size)){
      c2_factor[i,j] <- as.data.frame(expandinglist[[expanding_counter+i]][j])
    }
  }
  
### Plot the above forecasts:
  
# Bind the data with the original data:
  c2_factor <- cbind(c2_factor,tail(r_t, n = 204-w_size))
  colnames(c2_factor )[1:12] <- c("h=1","h=2","h=3","h=4","h=5","h=6",
                                  "h=7","h=8","h=9","h=10", "h=11","h=12")
  colnames(c2_factor )[14] <- c("Data")
  rownames(c2_factor ) <- rownames(r_t$Date)
  
# Plot: 
  c2_factor_plot <- (ggplot(data = c2_factor,aes(x = `Date`)) +
                       geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                       geom_line(aes(y = `h=1`, colour = "h = 1")) +
                       geom_line(aes(y = `h=3`, colour = "h = 3")) +
                       geom_line(aes(y = `h=6`, colour = "h = 6")) +
                       geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                       
                       # Adjustment of axis & titles:
                       labs(title = "Forecasts of Factor model", y = "%") +
                       theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                       scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))  
  c2_factor_plot
  
############################# AR model with regressors choosen by best subset algorithm.
  
####### Best model in-sample:
  
# Open loop to find the best model:
  for(i in 1:p_y){
    mfit_AR_best_subset <- arima(Y_in,order = c(i,0,0), xreg = mX_in_3,
                                 method = "ML")
    mAIC[i] <- AIC(mfit_AR_best_subset, k = 2)
  }
  k <- which.min(mAIC) 
  k
# Specify best in-sample modeL
  Y_in_fit_best_subset <- arima(Y_in,order = c(k,0,0), xreg = mX_in_3,
                                method = "ML")
  
  
####### Tests:
  
  ### Autocorrelation in the error terms - Ljung & Box:
  best_subset_modelr1_box4 <- Box.test(resid(Y_in_fit_best_subset), lag = 4, type = "Ljung")
  best_subset_modelr1_box8 <- Box.test(resid(Y_in_fit_best_subset), lag = 8, type = "Ljung")
  best_subset_modelr1_box12 <- Box.test(resid(Y_in_fit_best_subset), lag = 12, type = "Ljung")
  best_subset_modelr1_box24 <- Box.test(resid(Y_in_fit_best_subset), lag = 24, type = "Ljung") 
  
  ### Homoskedasticity: 
  best_subset_modelr1_arch <- arch.test(Y_in_fit_best_subset) 
  
  ### Test for normality:
  best_subset_modelr1_jarque_bera <- jarque.bera.test(resid(Y_in_fit_best_subset))
  
  ### Make a table
  best_subset_modelr1_test_matrix <- matrix(data = NA, nrow = 4, ncol = 5)
  best_subset_modelr1_test_matrix[1,c(1,3)] <- 4
  best_subset_modelr1_test_matrix[2,c(1,3)] <- 8
  best_subset_modelr1_test_matrix[3,c(1,3)] <- 12
  best_subset_modelr1_test_matrix[4,c(1,3)] <- 24
  
  colnames(best_subset_modelr1_test_matrix) <- c("Lags", "P-value","Lags","P-value", "P-value")
  
  # Insert values:
  best_subset_modelr1_test_matrix[1,2] = best_subset_modelr1_box4$p.value
  best_subset_modelr1_test_matrix[2,2] = best_subset_modelr1_box8$p.value
  best_subset_modelr1_test_matrix[3,2] = best_subset_modelr1_box12$p.value
  best_subset_modelr1_test_matrix[4,2] = best_subset_modelr1_box24$p.value
  
  best_subset_modelr1_test_matrix[1,4] = best_subset_modelr1_arch   [1,5]
  best_subset_modelr1_test_matrix[2,4] = best_subset_modelr1_arch  [2,5]
  best_subset_modelr1_test_matrix[3,4] = best_subset_modelr1_arch   [3,5]
  best_subset_modelr1_test_matrix[4,4] = best_subset_modelr1_arch [6,5]
  
  best_subset_modelr1_test_matrix[1,5] <- best_subset_modelr1_jarque_bera  $p.value
  
  # Print to latex: 
  print(xtable(best_subset_modelr1_test_matrix, digits = 2), file = "best_subset_modelr2_test_matrix.tex")
  
####### Forecasting:
  
### Apply function to generate forecasts:
  c3_best_subset <- data.frame(matrix(data = NA, nrow = 204-w_size, ncol = 12)) # Container for h=1 steap-a-head forecast.
  for(j in 1:12){
    expandinglist <- expanding_f_3(y=r_t,X_reg1 = mGT_log_transformed_ts,
                                   X_reg1_1 = c("THE.CRISIS","Cash","Bubble","Dividend","Society"), # Regressors choosen by best_subset.
                                   h = j, w_size = w_size, p = k, d= 0, q = 0)
    for(i in 1:(204-w_size)){
      c3_best_subset[i,j] <- as.data.frame(expandinglist[[expanding_counter+i]][j])
    }
  }
  
### Plot the above forecasts:
  
# Bind the data with the original data:
  c3_best_subset <- cbind(c3_best_subset,tail(r_t, n = 204-w_size))
  colnames(c3_best_subset)[1:12] <- c("h=1","h=2","h=3","h=4","h=5","h=6",
                                      "h=7","h=8","h=9","h=10", "h=11","h=12")
  colnames(c3_best_subset)[14] <- c("Data")
  rownames(c3_best_subset) <- rownames(r_t$Date)
  
# Plot: 
  c3_best_subset_plot <- (ggplot(data = c3_best_subset,aes(x = `Date`)) +
                            geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                            geom_line(aes(y = `h=1`, colour = "h = 1")) +
                            geom_line(aes(y = `h=3`, colour = "h = 3")) +
                            geom_line(aes(y = `h=6`, colour = "h = 6")) +
                            geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                            
                            # Adjustment of axis & titles:
                            labs(title = "Forecasts of best subset model", y = "%") +
                            theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                            scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))  
  c3_best_subset_plot
  
############################# Forecast combination.
  
###### Preparation:
  
### Bind all the fitted values in a object:
  pred_matrix <- cbind(fitted(Y_in_fit), fitted(Y_in_fit_factor),
                       fitted(Y_in_fit_best_subset))
  pred_matrix <- as.matrix(pred_matrix)
  
###### Forecast: 
  
### h = 1
  
# Specify horizon: 
  horizon <- 1
  
### Bind all the forecats in a object:
  for_matrix_h1 <- cbind(c1[,1],c2_factor[,horizon],c3_best_subset[,horizon])
  colnames(for_matrix_h1) <- c("AR(3)","Factor model", "Best subset model")  
  
### Now set up the foreccomb object & do forecast:
  foreccomb_h1 <- foreccomb(observed_vector = r_t[1:w_size,2], prediction_matrix = pred_matrix,
                            newobs = r_t[(w_size + 1):204,2], newpreds = for_matrix_h1)
  
# Simple average:
  mean_comb_h1 <- comb_SA(foreccomb_h1)
  
# Bates/Granger combination:
  Bates_Granger_h1 <- comb_BG(foreccomb_h1)
  
# OLS combination:
  OLS_combination_h1 <- comb_OLS(foreccomb_h1)
  
### h = 3
  
# Specify horizon: 
  horizon <- 3
  
### Bind all the forecats in a object:
  for_matrix_h3 <- cbind(c1[,1],c2_factor[,horizon],c3_best_subset[,horizon])
  colnames(for_matrix_h3) <- c("AR(3)","Factor model", "Best subset model")  
  
### Now set up the foreccomb object & do forecast:
  foreccomb_h3 <- foreccomb(observed_vector = r_t[1:w_size,2], prediction_matrix = pred_matrix,
                            newobs = r_t[(w_size+1):204,2], newpreds = for_matrix_h3)
  
# Simple average:
  mean_comb_h3 <- comb_SA(foreccomb_h3)
  
# Bates/Granger combination:
  Bates_Granger_h3 <- comb_BG(foreccomb_h3)
  
# OLS combination:
  OLS_combination_h3 <- comb_OLS(foreccomb_h3)
  
### h = 6
  
# Specify horizon: 
  horizon <- 6
  
### Bind all the forecats in a object:
  for_matrix_h6 <- cbind(c1[,1],c2_factor[,horizon],c3_best_subset[,horizon])
  colnames(for_matrix_h6) <- c("AR(3)","Factor model", "Best subset model")  
  
### Now set up the foreccomb object & do forecast:
  foreccomb_h6 <- foreccomb(observed_vector = r_t[1:w_size,2], prediction_matrix = pred_matrix,
                            newobs = r_t[(w_size+1):204,2], newpreds = for_matrix_h6)
  
# Simple average:
  mean_comb_h6 <- comb_SA(foreccomb_h6)
  
# Bates/Granger combination:
  Bates_Granger_h6 <- comb_BG(foreccomb_h6)
  
# OLS combination:
  OLS_combination_h6 <- comb_OLS(foreccomb_h6)
  
### h = 12
  
# Specify horizon: 
  horizon <- 12
  
### Bind all the forecats in a object:
  for_matrix_h12 <- cbind(c1[,1],c2_factor[,horizon],c3_best_subset[,horizon])
  colnames(for_matrix_h12) <- c("AR(3)","Factor model", "Best subset model")  
  
### Now set up the foreccomb object & do forecast:
  foreccomb_h12 <- foreccomb(observed_vector = r_t[1:w_size,2], prediction_matrix = pred_matrix,
                             newobs = r_t[(w_size+1):204,2], newpreds = for_matrix_h12)
  
# Simple average:
  mean_comb_h12 <- comb_SA(foreccomb_h12)
  
# Bates/Granger combination:
  Bates_Granger_h12 <- comb_BG(foreccomb_h12)
  
# OLS combination:
  OLS_combination_h12 <- comb_OLS(foreccomb_h12)
  
### Plot the forecast combinations:
  
# Simple average:  <------
  simple_average_forecast_comb <- cbind(mean_comb_h1$Forecasts_Test,
                                        mean_comb_h3$Forecasts_Test,
                                        mean_comb_h6$Forecasts_Test,
                                        mean_comb_h12$Forecasts_Test,
                                        tail(r_t, n = (204-w_size)))
  
  colnames(simple_average_forecast_comb )[1:4] <- c("h=1","h=3","h=6","h=12")
  colnames(simple_average_forecast_comb)[6] <- c("Data")
  rownames(simple_average_forecast_comb) <- rownames(r_t$Date)
  
# Plot:
  simple_average_forecast_comb_plot <- (ggplot(data = simple_average_forecast_comb,aes(x = `Date`)) +
                                          geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                                          geom_line(aes(y = `h=1`, colour = "h = 1")) +
                                          geom_line(aes(y = `h=3`, colour = "h = 3")) +
                                          geom_line(aes(y = `h=6`, colour = "h = 6")) +
                                          geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                                          
                                          # Adjustment of axis & titles:
                                          labs(title = "Forecast combination - Simple average", y = "%") +
                                          theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                                          scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))
  
  simple_average_forecast_comb_plot
  
# Bates/Granger: <------
  Bates_Granger_forecast_comb <- cbind(Bates_Granger_h1$Forecasts_Test,
                                       Bates_Granger_h3$Forecasts_Test,
                                       Bates_Granger_h6$Forecasts_Test,
                                       Bates_Granger_h12$Forecasts_Test,
                                       tail(r_t, n = 204-w_size))
  
  colnames(Bates_Granger_forecast_comb )[1:4] <- c("h=1","h=3","h=6","h=12")
  colnames(Bates_Granger_forecast_comb)[6] <- c("Data")
  rownames(Bates_Granger_forecast_comb) <- rownames(r_t$Date)
  
# Plot:
  Bates_Granger_forecast_comb_plot <- (ggplot(data = Bates_Granger_forecast_comb,aes(x = `Date`)) +
                                         geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                                         geom_line(aes(y = `h=1`, colour = "h = 1")) +
                                         geom_line(aes(y = `h=3`, colour = "h = 3")) +
                                         geom_line(aes(y = `h=6`, colour = "h = 6")) +
                                         geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                                         
                                         # Adjustment of axis & titles:
                                         labs(title = "Forecast combination - Bates/Granger", y = "%") +
                                         theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                                         scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))
  
  Bates_Granger_forecast_comb_plot
  
# OLS combination: <------
  OLS_combination_forecast_comb <- cbind(OLS_combination_h1$Forecasts_Test,
                                         OLS_combination_h3$Forecasts_Test,
                                         OLS_combination_h6$Forecasts_Test,
                                         OLS_combination_h12$Forecasts_Test,
                                         tail(r_t, n = 204-w_size))
  
  colnames(OLS_combination_forecast_comb )[1:4] <- c("h=1","h=3","h=6","h=12")
  colnames(OLS_combination_forecast_comb)[6] <- c("Data")
  rownames(OLS_combination_forecast_comb) <- rownames(r_t$Date)
  
# Plot:
  OLS_combination_forecast_comb_plot <- (ggplot(data = OLS_combination_forecast_comb,aes(x = `Date`)) +
                                           geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                                           geom_line(aes(y = `h=1`, colour = "h = 1")) +
                                           geom_line(aes(y = `h=3`, colour = "h = 3")) +
                                           geom_line(aes(y = `h=6`, colour = "h = 6")) +
                                           geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                                           
                                           # Adjustment of axis & titles:
                                           labs(title = "Forecast combination - OLS combination", y = "%") +
                                           theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                                           scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))
  
  OLS_combination_forecast_comb_plot
  
  
############################################################################
############################# Errors of forecasts: #########################
############################################################################
  
############################# Setup:
  m_Error <- matrix(data = NA, nrow = 22, ncol = 4) # Container.
  rownames(m_Error) <- c("Benchmark",
                         "RMSE",
                         "MAE",
                         "AR(3)",
                         "RMSE",
                         "MAE",
                         "Factor model",
                         "RMSE",
                         "MAE",
                         "Best Subset model",
                         "RMSE",
                         "MAE",
                         "Forecast combination",
                         "Simple average",
                         "RMSE",
                         "MAE",
                         "Bates/Granger",
                         "RMSE",
                         "MAE",
                         "OLS combination",
                         "RMSE",
                         "MAE"
  )
  colnames(m_Error) <- c("h = 1", "h = 3", "h = 6", "h = 12")
###### Benchmark:
  
  # RMSE:
  m_Error[2,1]  = for_errors(y = r_t[,2], yhat = c0[,1],
                             partion = 0)$rmse
  m_Error[2,2]  = for_errors(y = r_t[,2], yhat = c0[,3],
                             partion = 0)$rmse
  m_Error[2,3]  = for_errors(y = r_t[,2], yhat = c0[,6],
                             partion = 0)$rmse
  m_Error[2,4]  = for_errors(y = r_t[,2], yhat = c0[,12],
                             partion = 0)$rmse
  
  # MAE:
  m_Error[3,1]  = for_errors(y = r_t[,2], yhat = c0[,1],
                             partion = 0)$mae
  m_Error[3,2]  = for_errors(y = r_t[,2], yhat = c0[,3],
                             partion = 0)$mae
  m_Error[3,3]  = for_errors(y = r_t[,2], yhat = c0[,6],
                             partion = 0)$mae
  m_Error[3,4]  = for_errors(y = r_t[,2], yhat = c0[,12],
                             partion = 0)$mae
  
###### AR(3):
  
### Period: 2008M1:2009M12
  
  # RMSE:
  m_Error[5,1]  = for_errors(y = r_t[,2], yhat = c1[,1],
                             partion = 0)$rmse
  m_Error[5,2]  = for_errors(y = r_t[,2], yhat = c1[,3],
                             partion = 0)$rmse
  m_Error[5,3]  = for_errors(y = r_t[,2], yhat = c1[,6],
                             partion = 0)$rmse
  m_Error[5,4]  = for_errors(y = r_t[,2], yhat = c1[,12],
                             partion = 0)$rmse
  
  # MAE:
  m_Error[6,1]  = for_errors(y = r_t[,2], yhat = c1[,1],
                             partion = 0)$mae
  m_Error[6,2]  = for_errors(y = r_t[,2], yhat = c1[,3],
                             partion = 0)$mae
  m_Error[6,3]  = for_errors(y = r_t[,2], yhat = c1[,6],
                             partion = 0)$mae
  m_Error[6,4]  = for_errors(y = r_t[,2], yhat = c1[,12],
                             partion = 0)$mae
  
###### Factor model:
  
  # RMSE:
  m_Error[8,1]  = for_errors(y = r_t[,2], yhat = c2_factor[,1],
                             partion = 0)$rmse
  m_Error[8,2]  = for_errors(y = r_t[,2], yhat = c2_factor[,3],
                             partion = 0)$rmse
  m_Error[8,3]  = for_errors(y = r_t[,2], yhat = c1[,6],
                             partion = 0)$rmse
  m_Error[8,4]  = for_errors(y = r_t[,2], yhat = c1[,12],
                             partion = 0)$rmse
  
  # MAE:
  m_Error[9,1]  = for_errors(y = r_t[,2], yhat = c2_factor[,1],
                             partion = 0)$mae
  m_Error[9,2]  = for_errors(y = r_t[,2], yhat = c2_factor[,3],
                             partion = 0)$mae
  m_Error[9,3]  = for_errors(y = r_t[,2], yhat = c2_factor[,6],
                             partion = 0)$mae
  m_Error[9,4]  = for_errors(y = r_t[,2], yhat = c2_factor[,12],
                             partion = 0)$mae
  
###### Best subset model:
  
  # RMSE:
  m_Error[11,1]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,1],
                              partion = 0)$rmse
  m_Error[11,2]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,3],
                              partion = 0)$rmse
  m_Error[11,3]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,6],
                              partion = 0)$rmse
  m_Error[11,4]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,12],
                              partion = 0)$rmse
  
  # MAE:
  m_Error[12,1]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,1],
                              partion = 0)$mae
  m_Error[12,2]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,3],
                              partion = 0)$mae
  m_Error[12,3]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,6],
                              partion = 0)$mae
  m_Error[12,4]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,12],
                              partion = 0)$mae
  
###### Forecast combination (simple average):
  
  # RMSE:
  m_Error[15,1]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,1],
                              partion = 0)$rmse
  m_Error[15,2]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,2],
                              partion = 0)$rmse
  m_Error[15,3]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,3],
                              partion = 0)$rmse
  m_Error[15,4]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,4],
                              partion = 0)$rmse
  
  # MAE:
  m_Error[16,1]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,1],
                              partion = 0)$mae
  m_Error[16,2]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,2],
                              partion = 0)$mae
  m_Error[16,3]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,3],
                              partion = 0)$mae
  m_Error[16,4]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,4],
                              partion = 0)$mae
  
###### Forecast combination (Bates/Granger):
  
  # RMSE:
  m_Error[18,1]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,1],
                              partion = 0)$rmse
  m_Error[18,2]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,2],
                              partion = 0)$rmse
  m_Error[18,3]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,3],
                              partion = 0)$rmse
  m_Error[18,4]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,4],
                              partion = 0)$rmse
  
  # MAE:
  m_Error[19,1]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,1],
                              partion = 0)$mae
  m_Error[19,2]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,2],
                              partion = 0)$mae
  m_Error[19,3]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,3],
                              partion = 0)$mae
  m_Error[19,4]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,4],
                              partion = 0)$mae

###### Forecast combination (OLS-combination):
  
  # RMSE:
  m_Error[21,1]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,1],
                              partion = 0)$rmse
  m_Error[21,2]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,2],
                              partion = 0)$rmse
  m_Error[21,3]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,3],
                              partion = 0)$rmse
  m_Error[21,4]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,4],
                              partion = 0)$rmse
  
  # MAE:
  m_Error[22,1]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,1],
                              partion = 0)$mae
  m_Error[22,2]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,2],
                              partion = 0)$mae
  m_Error[22,3]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,3],
                              partion = 0)$mae
  m_Error[22,4]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,4],
                              partion = 0)$mae
  
############################# Out-of-sample R^2:
  
###### AR(3)
  
  AR3_R2OSh1_p1 <- out_of_sample_R2(r_t[,2], yhat = c1[,1], partion = 0, mean_forecast = c0[,1])
  AR3_R2OSh3_p1 <- out_of_sample_R2(r_t[,2], yhat = c1[,3], partion = 0, mean_forecast = c0[,3])
  AR3_R2OSh6_p1 <- out_of_sample_R2(r_t[,2], yhat = c1[,6], partion = 0, mean_forecast = c0[,6])
  AR3_R2OSh12_p1 <- out_of_sample_R2(r_t[,2], yhat = c1[,12], partion = 0, mean_forecast = c0[,12])
  
  # Print the results: 
  AR3_R2OSh1_p1
  AR3_R2OSh3_p1
  AR3_R2OSh6_p1
  AR3_R2OSh12_p1
  
###### Factor model: 
  
  factor_model_R2OSh1_p1 <- out_of_sample_R2(r_t[,2], yhat = c2_factor[,1], partion = 0, mean_forecast = c0[,1])
  factor_model_R2OSh3_p1 <- out_of_sample_R2(r_t[,2], yhat = c2_factor[,3], partion = 0, mean_forecast = c0[,3])
  factor_model_R2OSh6_p1 <- out_of_sample_R2(r_t[,2], yhat = c2_factor[,6], partion = 0, mean_forecast = c0[,6])
  factor_model_R2OSh12_p1 <- out_of_sample_R2(r_t[,2], yhat = c2_factor[,12], partion = 0, mean_forecast = c0[,12])
  
  # Print the results: 
  factor_model_R2OSh1_p1
  factor_model_R2OSh3_p1
  factor_model_R2OSh6_p1
  factor_model_R2OSh12_p1
  
###### Best subset model: 
  
  c3_best_subset_R2OSh1_p1 <- out_of_sample_R2(r_t[,2], yhat = c3_best_subset[,1], partion = 0, mean_forecast = c0[,1])
  c3_best_subset_R2OSh3_p1 <- out_of_sample_R2(r_t[,2], yhat = c3_best_subset[,3], partion = 0, mean_forecast = c0[,3])
  c3_best_subset_R2OSh6_p1 <- out_of_sample_R2(r_t[,2], yhat = c3_best_subset[,6], partion = 0, mean_forecast = c0[,6])
  c3_best_subset_R2OSh12_p1 <- out_of_sample_R2(r_t[,2], yhat = c3_best_subset[,12], partion = 0, mean_forecast = c0[,12])
  
  # Print the results: 
  c3_best_subset_R2OSh1_p1
  c3_best_subset_R2OSh3_p1
  c3_best_subset_R2OSh6_p1
  c3_best_subset_R2OSh12_p1
  
###### Forecast combination - simple average. 
  
  simple_average_forecast_comb_R2OSh1_p1 <- out_of_sample_R2(r_t[,2], yhat = simple_average_forecast_comb[,1], partion = 0, mean_forecast = c0[,1])
  simple_average_forecast_comb_R2OSh3_p1 <- out_of_sample_R2(r_t[,2], yhat = simple_average_forecast_comb[,2], partion = 0, mean_forecast = c0[,3])
  simple_average_forecast_comb_R2OSh6_p1 <- out_of_sample_R2(r_t[,2], yhat = simple_average_forecast_comb[,3], partion = 0, mean_forecast = c0[,6])
  simple_average_forecast_comb_R2OSh12_p1 <- out_of_sample_R2(r_t[,2], yhat = simple_average_forecast_comb[,4], partion = 0, mean_forecast = c0[,12])
  
  # Print the results: 
  simple_average_forecast_comb_R2OSh1_p1
  simple_average_forecast_comb_R2OSh3_p1
  simple_average_forecast_comb_R2OSh6_p1
  simple_average_forecast_comb_R2OSh12_p1
  
###### Forecast combination - Bates/Granger
  
  Bates_Granger_forecast_comb_R2OSh1_p1 <- out_of_sample_R2(r_t[,2], yhat = Bates_Granger_forecast_comb[,1], partion = 0, mean_forecast = c0[,1])
  Bates_Granger_forecast_comb_R2OSh3_p1 <- out_of_sample_R2(r_t[,2], yhat = Bates_Granger_forecast_comb[,2], partion = 0,  mean_forecast = c0[,3])
  Bates_Granger_forecast_comb_R2OSh6_p1 <- out_of_sample_R2(r_t[,2], yhat = Bates_Granger_forecast_comb[,3], partion = 0,  mean_forecast = c0[,6])
  Bates_Granger_forecast_comb_R2OSh12_p1 <- out_of_sample_R2(r_t[,2], yhat = Bates_Granger_forecast_comb[,4], partion = 0,  mean_forecast = c0[,12])
  
  # Print the results: 
  Bates_Granger_forecast_comb_R2OSh1_p1
  Bates_Granger_forecast_comb_R2OSh3_p1
  Bates_Granger_forecast_comb_R2OSh6_p1
  Bates_Granger_forecast_comb_R2OSh12_p1
  
###### Forecast combination - OLS 
  
  ### Period: 2008M1:2009M12
  OLS_combination_forecast_comb_R2OSh1_p1 <- out_of_sample_R2(r_t[,2], yhat = OLS_combination_forecast_comb[,1], partion = 0,  mean_forecast = c0[,1])
  OLS_combination_forecast_comb_R2OSh3_p1 <- out_of_sample_R2(r_t[,2], yhat = OLS_combination_forecast_comb[,2], partion = 0,  mean_forecast = c0[,3])
  OLS_combination_forecast_comb_R2OSh6_p1 <- out_of_sample_R2(r_t[,2], yhat = OLS_combination_forecast_comb[,3], partion = 0,  mean_forecast = c0[,6])
  OLS_combination_forecast_comb_R2OSh12_p1 <- out_of_sample_R2(r_t[,2], yhat = OLS_combination_forecast_comb[,4], partion = 0,  mean_forecast = c0[,12])
  
  # Print: 
  OLS_combination_forecast_comb_R2OSh1_p1
  OLS_combination_forecast_comb_R2OSh3_p1
  OLS_combination_forecast_comb_R2OSh6_p1
  OLS_combination_forecast_comb_R2OSh12_p1
  
###########################################################################################################################
############################# Robustness check 3: #########################################################################
###########################################################################################################################
  
############################# Expanding window: w_size -> 36 (first forecast point is 2007:M1)
  
  
###### Model overview:
  # benchmark_model: Benchmark.
  # Forecast: c0
  # Y_in_fit: AR(3)
  # Forecast: c1.
  # Y_in_fit_factor: AR with external regressors.
  # Forecast: c2.
  
###### Generel setup: 
  w_size <- 36
  p_y <- 10
  mAIC <- rep(NA, p_y) # p_y is max lag.
  Y_in <- as.ts(r_t[1:w_size,2], start = c(2004,1), frequency = 12)
  
  y <- r_t
  X_reg <- mGT_log_transformed_ts
  F_df <- EstFactors(X_reg,ic)
  F_df2 <- F_df[,-c(1)]
  X_reg2 <- X_reg[,-c(1)]
  X <- cbind(y,F_df2,X_reg2)
  mX_in_2 <- X[1:w_size,3:5]
  mX_in_2 <- as.matrix(mX_in_2)
  
  X2 <- cbind(y,X_reg2)
  mX_in_3 <- X2[1:w_size,c("THE.CRISIS","Cash","Bubble","Dividend","Society")]
  mX_in_3 <- as.matrix(mX_in_3)
  
############################# Benchmark:
  
####### Best model in-sample:
  
# Fit a arima model only with mean and noise:
  bechmark_model <- arima(Y_in, order = c(0,0,0),
                          method = "ML")
  
####### Tests:
  
  ### Autocorrelation in the error terms - Ljung & Box: (H0: No autocorrelation.)
  Box.test(resid(bechmark_model), lag = 4, type = "Ljung")
  Box.test(resid(bechmark_model), lag = 8, type = "Ljung")
  Box.test(resid(bechmark_model), lag = 12, type = "Ljung")
  Box.test(resid(bechmark_model), lag = 24, type = "Ljung") 
  
  ### Homoskedasticity (H0: no heteroskedasticity): 
  arch.test(bechmark_model) # Very high p-values -> no problem. 
  
  ### Test for normality (H0: Skew = 0 an excess kurtosis of 0):
  jarque.bera.test(resid(bechmark_model)) # No problem.
  
  ####### Tests:
  
  ### Autocorrelation in the error terms - Ljung & Box:
  benchmarkr1_box4 <- Box.test(resid(bechmark_model), lag = 4, type = "Ljung")
  benchmarkr1_box8 <- Box.test(resid(bechmark_model), lag = 8, type = "Ljung")
  benchmarkr1_box12 <- Box.test(resid(bechmark_model), lag = 12, type = "Ljung")
  benchmarkr1_box24 <- Box.test(resid(bechmark_model), lag = 24, type = "Ljung") # No problem. Even for high number of lags.  
  
  ### Homoskedasticity: 
  benchmarkr1_arch <- arch.test(bechmark_model) # Very high p-values -> no problem. 
  
  ### Test for normality:
  benchmarkr1_jarque_bera <- jarque.bera.test(resid(bechmark_model))
  
  ### Make a table
  benchmarkr1_test_matrix <- matrix(data = NA, nrow = 4, ncol = 5)
  benchmarkr1_test_matrix[1,c(1,3)] <- 4
  benchmarkr1_test_matrix[2,c(1,3)] <- 8
  benchmarkr1_test_matrix[3,c(1,3)] <- 12
  benchmarkr1_test_matrix[4,c(1,3)] <- 24
  
  colnames(benchmarkr1_test_matrix) <- c("Lags", "P-value","Lags","P-value", "P-value")
  
  # Insert values:
  benchmarkr1_test_matrix[1,2] = benchmarkr1_box4$p.value
  benchmarkr1_test_matrix[2,2] = benchmarkr1_box8$p.value
  benchmarkr1_test_matrix[3,2] = benchmarkr1_box12$p.value
  benchmarkr1_test_matrix[4,2] = benchmarkr1_box24$p.value
  
  benchmarkr1_test_matrix[1,4] = benchmarkr1_arch [1,5]
  benchmarkr1_test_matrix[2,4] = benchmarkr1_arch [2,5]
  benchmarkr1_test_matrix[3,4] = benchmarkr1_arch [3,5]
  benchmarkr1_test_matrix[4,4] = benchmarkr1_arch [6,5]
  
  benchmarkr1_test_matrix[1,5] <- benchmarkr1_jarque_bera $p.value
  
  # Print to latex: 
  print(xtable(benchmarkr1_test_matrix, digits = 2), file = "benchmarkr3_test_matrix.tex")
  
  
####### Forecasting:
  
### Apply function to generate forecasts:
  expanding_counter = 504
  c0 <- data.frame(matrix(data = NA, nrow = 204-w_size, ncol = 12)) # Container for h=1 steap-a-head forecast.
  for(j in 1:12){
    expandinglist <- expanding_forecast_arima(X = r_t, w_size = w_size, h=j, p=0, q=0) # NB: only 1,3,6 and 12 will be considered.
    for(i in 1:(204-w_size)){
      c0[i,j] <- as.data.frame(expandinglist[[expanding_counter+i]][j])
    }
  }
  
  
  
### Plot the above forecasts:
  
# Bind the data with the original data:
  c0 <- cbind(c0,tail(r_t, n = 204-w_size))
  colnames(c0)[1:12] <- c("h=1","h=2","h=3","h=4","h=5","h=6",
                          "h=7","h=8","h=9","h=10", "h=11","h=12")
  colnames(c0)[14] <- c("Data")
  rownames(c0) <- rownames(r_t$Date)
  
# Plot: 
  c0_plot <- (ggplot(data = c0,aes(x = `Date`)) +
                geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                geom_line(aes(y = `h=1`, colour = "h = 1")) +
                geom_line(aes(y = `h=3`, colour = "h = 3")) +
                geom_line(aes(y = `h=6`, colour = "h = 6")) +
                geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                
                # Adjustment of axis & titles:
                labs(title = "Forecasts of benchmark model", y = "%") +
                theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))  
  c0_plot
  
  
############################# AR:
  
####### Best model in-sample:
  
# Open loop to find the best model:
  for(i in 1:p_y){
    mfit_AR <- arima(Y_in, order = c(i,0,0),
                     method = "ML")
    mAIC[i] <- AIC(mfit_AR, k = 2)
  }
  k <- which.min(mAIC) 
  k
  
# Specify best in-sample modeL
  Y_in_fit <- arima(Y_in, order = c(k,0,0),
                    method = "ML")
  
####### Tests:
  
  ### Autocorrelation in the error terms - Ljung & Box:
  AR_3r1_box4 <- Box.test(resid(Y_in_fit), lag = 4, type = "Ljung")
  AR_3r1_box8 <- Box.test(resid(Y_in_fit), lag = 8, type = "Ljung")
  AR_3r1_box12 <- Box.test(resid(Y_in_fit), lag = 12, type = "Ljung")
  AR_3r1_box24 <- Box.test(resid(Y_in_fit), lag = 24, type = "Ljung") 
  
  ### Homoskedasticity: 
  AR_3r1_arch <- arch.test(Y_in_fit) # Very high p-values -> no problem. 
  
  ### Test for normality:
  AR_3r1_jarque_bera <- jarque.bera.test(resid(Y_in_fit))
  
  ### Make a table
  AR_3r1_test_matrix <- matrix(data = NA, nrow = 4, ncol = 5)
  AR_3r1_test_matrix[1,c(1,3)] <- 4
  AR_3r1_test_matrix[2,c(1,3)] <- 8
  AR_3r1_test_matrix[3,c(1,3)] <- 12
  AR_3r1_test_matrix[4,c(1,3)] <- 24
  
  colnames(AR_3r1_test_matrix) <- c("Lags", "P-value","Lags","P-value", "P-value")
  
  # Insert values:
  AR_3r1_test_matrix[1,2] = AR_3r1_box4$p.value
  AR_3r1_test_matrix[2,2] = AR_3r1_box8$p.value
  AR_3r1_test_matrix[3,2] = AR_3r1_box12$p.value
  AR_3r1_test_matrix[4,2] = AR_3r1_box24$p.value
  
  AR_3r1_test_matrix[1,4] = AR_3r1_arch  [1,5]
  AR_3r1_test_matrix[2,4] = AR_3r1_arch  [2,5]
  AR_3r1_test_matrix[3,4] = AR_3r1_arch  [3,5]
  AR_3r1_test_matrix[4,4] = AR_3r1_arch  [6,5]
  
  AR_3r1_test_matrix[1,5] <- AR_3r1_jarque_bera $p.value
  
  # Print to latex: 
  print(xtable(AR_3r1_test_matrix, digits = 2), file = "AR_3r3_test_matrix.tex")
  
####### Forecasting:
  
### Apply function to generate forecasts:
  c1 <- data.frame(matrix(data = NA, nrow = 204-w_size, ncol = 12)) # Container for h=1 steap-a-head forecast.
  for(j in 1:12){
    expandinglist <- expanding_forecast_arima(X = r_t, w_size = w_size, h=j, p=k, q=0) # NB: only 1,3,6 and 12 will be considered.
    for(i in 1:(204-w_size)){
      c1[i,j] <- as.data.frame(expandinglist[[expanding_counter+i]][j])
    }
  }
  
### Plot the above forecasts:
  
# Bind the data with the original data:
  c1 <- cbind(c1,tail(r_t, n = (204-w_size)))
  colnames(c1)[1:12] <- c("h=1","h=2","h=3","h=4","h=5","h=6",
                          "h=7","h=8","h=9","h=10", "h=11","h=12")
  colnames(c1)[14] <- c("Data")
  rownames(c1) <- rownames(r_t$Date)
  
# Plot: 
  c1_plot <- (ggplot(data = c1,aes(x = `Date`)) +
                geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                geom_line(aes(y = `h=1`, colour = "h = 1")) +
                geom_line(aes(y = `h=3`, colour = "h = 3")) +
                geom_line(aes(y = `h=6`, colour = "h = 6")) +
                geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                
                # Adjustment of axis & titles:
                labs(title = "Forecasts of AR(3)", y = "%") +
                theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))  
  c1_plot
  
  
  
############################# Factor model (AR with factors):
  
####### Best model in-sample:
  
# Open loop to find the best model:
  for(i in 1:p_y){
    mfit_AR_factor <- arima(Y_in,order = c(i,0,0), xreg = mX_in_2,
                            method = "ML")
    mAIC[i] <- AIC(mfit_AR_factor, k = 2)
  }
  k <- which.min(mAIC) 
  k
  
# Specify best in-sample modeL
  Y_in_fit_factor <- arima(Y_in,order = c(k,0,0), xreg = mX_in_2,
                           method = "ML")
  
####### Tests:
  
  ### Autocorrelation in the error terms - Ljung & Box:
  factor_modelr1_box4 <- Box.test(resid(Y_in_fit_factor), lag = 4, type = "Ljung")
  factor_modelr1_box8 <- Box.test(resid(Y_in_fit_factor), lag = 8, type = "Ljung")
  factor_modelr1_box12 <- Box.test(resid(Y_in_fit_factor), lag = 12, type = "Ljung")
  factor_modelr1_box24 <- Box.test(resid(Y_in_fit_factor), lag = 24, type = "Ljung") 
  
  ### Homoskedasticity: 
  factor_modelr1_arch <- arch.test(Y_in_fit_factor) 
  
  ### Test for normality:
  factor_modelr1_jarque_bera <- jarque.bera.test(resid(Y_in_fit_factor))
  
  ### Make a table
  factor_modelr1_test_matrix <- matrix(data = NA, nrow = 4, ncol = 5)
  factor_modelr1_test_matrix[1,c(1,3)] <- 4
  factor_modelr1_test_matrix[2,c(1,3)] <- 8
  factor_modelr1_test_matrix[3,c(1,3)] <- 12
  factor_modelr1_test_matrix[4,c(1,3)] <- 24
  
  colnames(factor_modelr1_test_matrix) <- c("Lags", "P-value","Lags","P-value", "P-value")
  
  # Insert values:
  factor_modelr1_test_matrix[1,2] = factor_modelr1_box4$p.value
  factor_modelr1_test_matrix[2,2] = factor_modelr1_box8$p.value
  factor_modelr1_test_matrix[3,2] = factor_modelr1_box12$p.value
  factor_modelr1_test_matrix[4,2] = factor_modelr1_box24$p.value
  
  factor_modelr1_test_matrix[1,4] = factor_modelr1_arch  [1,5]
  factor_modelr1_test_matrix[2,4] = factor_modelr1_arch [2,5]
  factor_modelr1_test_matrix[3,4] = factor_modelr1_arch  [3,5]
  factor_modelr1_test_matrix[4,4] = factor_modelr1_arch [6,5]
  
  factor_modelr1_test_matrix[1,5] <- factor_modelr1_jarque_bera $p.value
  
  # Print to latex: 
  print(xtable(factor_modelr1_test_matrix, digits = 2), file = "factor_modelr3_test_matrix.tex")
  
####### Forecasting:
  
### Apply function to generate forecasts:
  c2_factor <- data.frame(matrix(data = NA, nrow = (204-w_size), ncol = 12)) # Container for h=1 steap-a-head forecast.
  for(j in 1:12){
    expandinglist <- expanding_f_2_ml(y = r_t,X_reg = mGT_log_transformed_ts, h = j, w_size = w_size, p = k,d=0,q=0)# NB: only 1,3,6 and 12 will be considered.
    for(i in 1:(204-w_size)){
      c2_factor[i,j] <- as.data.frame(expandinglist[[expanding_counter+i]][j])
    }
  }
  
### Plot the above forecasts:
  
# Bind the data with the original data:
  c2_factor <- cbind(c2_factor,tail(r_t, n = 204-w_size))
  colnames(c2_factor )[1:12] <- c("h=1","h=2","h=3","h=4","h=5","h=6",
                                  "h=7","h=8","h=9","h=10", "h=11","h=12")
  colnames(c2_factor )[14] <- c("Data")
  rownames(c2_factor ) <- rownames(r_t$Date)
  
# Plot: 
  c2_factor_plot <- (ggplot(data = c2_factor,aes(x = `Date`)) +
                       geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                       geom_line(aes(y = `h=1`, colour = "h = 1")) +
                       geom_line(aes(y = `h=3`, colour = "h = 3")) +
                       geom_line(aes(y = `h=6`, colour = "h = 6")) +
                       geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                       
                       # Adjustment of axis & titles:
                       labs(title = "Forecasts of Factor model", y = "%") +
                       theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                       scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))  
  c2_factor_plot
  
############################# AR model with regressors choosen by best subset algorithm.
  
####### Best model in-sample:
  
# Open loop to find the best model:
  for(i in 1:p_y){
    mfit_AR_best_subset <- arima(Y_in,order = c(i,0,0), xreg = mX_in_3,
                                 method = "ML")
    mAIC[i] <- AIC(mfit_AR_best_subset, k = 2)
  }
  k <- which.min(mAIC) 
  k
# Specify best in-sample modeL
  Y_in_fit_best_subset <- arima(Y_in,order = c(k,0,0), xreg = mX_in_3,
                                method = "ML")
  
  
####### Tests:
  
  ### Autocorrelation in the error terms - Ljung & Box:
  best_subset_modelr1_box4 <- Box.test(resid(Y_in_fit_best_subset), lag = 4, type = "Ljung")
  best_subset_modelr1_box8 <- Box.test(resid(Y_in_fit_best_subset), lag = 8, type = "Ljung")
  best_subset_modelr1_box12 <- Box.test(resid(Y_in_fit_best_subset), lag = 12, type = "Ljung")
  best_subset_modelr1_box24 <- Box.test(resid(Y_in_fit_best_subset), lag = 24, type = "Ljung") 
  
  ### Homoskedasticity: 
  best_subset_modelr1_arch <- arch.test(Y_in_fit_best_subset) 
  
  ### Test for normality:
  best_subset_modelr1_jarque_bera <- jarque.bera.test(resid(Y_in_fit_best_subset))
  
  ### Make a table
  best_subset_modelr1_test_matrix <- matrix(data = NA, nrow = 4, ncol = 5)
  best_subset_modelr1_test_matrix[1,c(1,3)] <- 4
  best_subset_modelr1_test_matrix[2,c(1,3)] <- 8
  best_subset_modelr1_test_matrix[3,c(1,3)] <- 12
  best_subset_modelr1_test_matrix[4,c(1,3)] <- 24
  
  colnames(best_subset_modelr1_test_matrix) <- c("Lags", "P-value","Lags","P-value", "P-value")
  
  # Insert values:
  best_subset_modelr1_test_matrix[1,2] = best_subset_modelr1_box4$p.value
  best_subset_modelr1_test_matrix[2,2] = best_subset_modelr1_box8$p.value
  best_subset_modelr1_test_matrix[3,2] = best_subset_modelr1_box12$p.value
  best_subset_modelr1_test_matrix[4,2] = best_subset_modelr1_box24$p.value
  
  best_subset_modelr1_test_matrix[1,4] = best_subset_modelr1_arch   [1,5]
  best_subset_modelr1_test_matrix[2,4] = best_subset_modelr1_arch  [2,5]
  best_subset_modelr1_test_matrix[3,4] = best_subset_modelr1_arch   [3,5]
  best_subset_modelr1_test_matrix[4,4] = best_subset_modelr1_arch [6,5]
  
  best_subset_modelr1_test_matrix[1,5] <- best_subset_modelr1_jarque_bera  $p.value
  
  # Print to latex: 
  print(xtable(best_subset_modelr1_test_matrix, digits = 2), file = "best_subset_modelr3_test_matrix.tex")
  
####### Forecasting:
  
### Apply function to generate forecasts:
  c3_best_subset <- data.frame(matrix(data = NA, nrow = 204-w_size, ncol = 12)) # Container for h=1 steap-a-head forecast.
  for(j in 1:12){
    expandinglist <- expanding_f_3(y=r_t,X_reg1 = mGT_log_transformed_ts,
                                   X_reg1_1 = c("THE.CRISIS","Cash","Bubble","Dividend","Society"), # Regressors choosen by best_subset.
                                   h = j, w_size = w_size, p = k, d= 0, q = 0)
    for(i in 1:(204-w_size)){
      c3_best_subset[i,j] <- as.data.frame(expandinglist[[expanding_counter+i]][j])
    }
  }
  
### Plot the above forecasts:
  
# Bind the data with the original data:
  c3_best_subset <- cbind(c3_best_subset,tail(r_t, n = 204-w_size))
  colnames(c3_best_subset)[1:12] <- c("h=1","h=2","h=3","h=4","h=5","h=6",
                                      "h=7","h=8","h=9","h=10", "h=11","h=12")
  colnames(c3_best_subset)[14] <- c("Data")
  rownames(c3_best_subset) <- rownames(r_t$Date)
  
# Plot: 
  c3_best_subset_plot <- (ggplot(data = c3_best_subset,aes(x = `Date`)) +
                            geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                            geom_line(aes(y = `h=1`, colour = "h = 1")) +
                            geom_line(aes(y = `h=3`, colour = "h = 3")) +
                            geom_line(aes(y = `h=6`, colour = "h = 6")) +
                            geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                            
                            # Adjustment of axis & titles:
                            labs(title = "Forecasts of best subset model", y = "%") +
                            theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                            scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))  
  c3_best_subset_plot
  
############################# Forecast combination.
  
###### Preparation:
  
### Bind all the fitted values in a object:
  pred_matrix <- cbind(fitted(Y_in_fit), fitted(Y_in_fit_factor),
                       fitted(Y_in_fit_best_subset))
  pred_matrix <- as.matrix(pred_matrix)
  
###### Forecast: 
  
### h = 1
  
# Specify horizon: 
  horizon <- 1
  
### Bind all the forecats in a object:
  for_matrix_h1 <- cbind(c1[,1],c2_factor[,horizon],c3_best_subset[,horizon])
  colnames(for_matrix_h1) <- c("AR(3)","Factor model", "Best subset model")  
  
### Now set up the foreccomb object & do forecast:
  foreccomb_h1 <- foreccomb(observed_vector = r_t[1:w_size,2], prediction_matrix = pred_matrix,
                            newobs = r_t[(w_size + 1):204,2], newpreds = for_matrix_h1)
  
# Simple average:
  mean_comb_h1 <- comb_SA(foreccomb_h1)
  
# Bates/Granger combination:
  Bates_Granger_h1 <- comb_BG(foreccomb_h1)
  
# OLS combination:
  OLS_combination_h1 <- comb_OLS(foreccomb_h1)
  
### h = 3
  
# Specify horizon: 
  horizon <- 3
  
### Bind all the forecats in a object:
  for_matrix_h3 <- cbind(c1[,1],c2_factor[,horizon],c3_best_subset[,horizon])
  colnames(for_matrix_h3) <- c("AR(3)","Factor model", "Best subset model")  
  
### Now set up the foreccomb object & do forecast:
  foreccomb_h3 <- foreccomb(observed_vector = r_t[1:w_size,2], prediction_matrix = pred_matrix,
                            newobs = r_t[(w_size+1):204,2], newpreds = for_matrix_h3)
  
# Simple average:
  mean_comb_h3 <- comb_SA(foreccomb_h3)
  
# Bates/Granger combination:
  Bates_Granger_h3 <- comb_BG(foreccomb_h3)
  
# OLS combination:
  OLS_combination_h3 <- comb_OLS(foreccomb_h3)
  
### h = 6
  
# Specify horizon: 
  horizon <- 6
  
### Bind all the forecats in a object:
  for_matrix_h6 <- cbind(c1[,1],c2_factor[,horizon],c3_best_subset[,horizon])
  colnames(for_matrix_h6) <- c("AR(3)","Factor model", "Best subset model")  
  
### Now set up the foreccomb object & do forecast:
  foreccomb_h6 <- foreccomb(observed_vector = r_t[1:w_size,2], prediction_matrix = pred_matrix,
                            newobs = r_t[(w_size+1):204,2], newpreds = for_matrix_h6)
  
# Simple average:
  mean_comb_h6 <- comb_SA(foreccomb_h6)
  
# Bates/Granger combination:
  Bates_Granger_h6 <- comb_BG(foreccomb_h6)
  
# OLS combination:
  OLS_combination_h6 <- comb_OLS(foreccomb_h6)
  
### h = 12
  
# Specify horizon: 
  horizon <- 12
  
### Bind all the forecats in a object:
  for_matrix_h12 <- cbind(c1[,1],c2_factor[,horizon],c3_best_subset[,horizon])
  colnames(for_matrix_h12) <- c("AR(3)","Factor model", "Best subset model")  
  
### Now set up the foreccomb object & do forecast:
  foreccomb_h12 <- foreccomb(observed_vector = r_t[1:w_size,2], prediction_matrix = pred_matrix,
                             newobs = r_t[(w_size+1):204,2], newpreds = for_matrix_h12)
  
# Simple average:
  mean_comb_h12 <- comb_SA(foreccomb_h12)
  
# Bates/Granger combination:
  Bates_Granger_h12 <- comb_BG(foreccomb_h12)
  
# OLS combination:
  OLS_combination_h12 <- comb_OLS(foreccomb_h12)
  
### Plot the forecast combinations:
  
# Simple average:  <------
  simple_average_forecast_comb <- cbind(mean_comb_h1$Forecasts_Test,
                                        mean_comb_h3$Forecasts_Test,
                                        mean_comb_h6$Forecasts_Test,
                                        mean_comb_h12$Forecasts_Test,
                                        tail(r_t, n = (204-w_size)))
  
  colnames(simple_average_forecast_comb )[1:4] <- c("h=1","h=3","h=6","h=12")
  colnames(simple_average_forecast_comb)[6] <- c("Data")
  rownames(simple_average_forecast_comb) <- rownames(r_t$Date)
  
# Plot:
  simple_average_forecast_comb_plot <- (ggplot(data = simple_average_forecast_comb,aes(x = `Date`)) +
                                          geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                                          geom_line(aes(y = `h=1`, colour = "h = 1")) +
                                          geom_line(aes(y = `h=3`, colour = "h = 3")) +
                                          geom_line(aes(y = `h=6`, colour = "h = 6")) +
                                          geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                                          
                                          # Adjustment of axis & titles:
                                          labs(title = "Forecast combination - Simple average", y = "%") +
                                          theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                                          scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))
  
  simple_average_forecast_comb_plot
  
# Bates/Granger: <------
  Bates_Granger_forecast_comb <- cbind(Bates_Granger_h1$Forecasts_Test,
                                       Bates_Granger_h3$Forecasts_Test,
                                       Bates_Granger_h6$Forecasts_Test,
                                       Bates_Granger_h12$Forecasts_Test,
                                       tail(r_t, n = 204-w_size))
  
  colnames(Bates_Granger_forecast_comb )[1:4] <- c("h=1","h=3","h=6","h=12")
  colnames(Bates_Granger_forecast_comb)[6] <- c("Data")
  rownames(Bates_Granger_forecast_comb) <- rownames(r_t$Date)
  
# Plot:
  Bates_Granger_forecast_comb_plot <- (ggplot(data = Bates_Granger_forecast_comb,aes(x = `Date`)) +
                                         geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                                         geom_line(aes(y = `h=1`, colour = "h = 1")) +
                                         geom_line(aes(y = `h=3`, colour = "h = 3")) +
                                         geom_line(aes(y = `h=6`, colour = "h = 6")) +
                                         geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                                         
                                         # Adjustment of axis & titles:
                                         labs(title = "Forecast combination - Bates/Granger", y = "%") +
                                         theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                                         scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))
  
  Bates_Granger_forecast_comb_plot
  
# OLS combination: <------
  OLS_combination_forecast_comb <- cbind(OLS_combination_h1$Forecasts_Test,
                                         OLS_combination_h3$Forecasts_Test,
                                         OLS_combination_h6$Forecasts_Test,
                                         OLS_combination_h12$Forecasts_Test,
                                         tail(r_t, n = 204-w_size))
  
  colnames(OLS_combination_forecast_comb )[1:4] <- c("h=1","h=3","h=6","h=12")
  colnames(OLS_combination_forecast_comb)[6] <- c("Data")
  rownames(OLS_combination_forecast_comb) <- rownames(r_t$Date)
  
# Plot:
  OLS_combination_forecast_comb_plot <- (ggplot(data = OLS_combination_forecast_comb,aes(x = `Date`)) +
                                           geom_line(data = r_t, aes(x = `Date`, y = `Return_SandP500`, colour = "Data")) +
                                           geom_line(aes(y = `h=1`, colour = "h = 1")) +
                                           geom_line(aes(y = `h=3`, colour = "h = 3")) +
                                           geom_line(aes(y = `h=6`, colour = "h = 6")) +
                                           geom_line(aes(y = `h=12`, colour = "h = 12")) + 
                                           
                                           # Adjustment of axis & titles:
                                           labs(title = "Forecast combination - OLS combination", y = "%") +
                                           theme(plot.title = element_text(hjust = 0.5)) + # Adjust title to the center of the plot. 
                                           scale_x_date(labels = date_format("%Y"),breaks = date_breaks("1 years")))
  
  OLS_combination_forecast_comb_plot
  
  
############################################################################
############################# Errors of forecasts: #########################
############################################################################
  
############################# Setup:
  m_Error <- matrix(data = NA, nrow = 22, ncol = 4) # Container.
  rownames(m_Error) <- c("Benchmark",
                         "RMSE",
                         "MAE",
                         "AR(3)",
                         "RMSE",
                         "MAE",
                         "Factor model",
                         "RMSE",
                         "MAE",
                         "Best Subset model",
                         "RMSE",
                         "MAE",
                         "Forecast combination",
                         "Simple average",
                         "RMSE",
                         "MAE",
                         "Bates/Granger",
                         "RMSE",
                         "MAE",
                         "OLS combination",
                         "RMSE",
                         "MAE"
  )
  colnames(m_Error) <- c("h = 1", "h = 3", "h = 6", "h = 12")
###### Benchmark:
  
  # RMSE:
  m_Error[2,1]  = for_errors(y = r_t[,2], yhat = c0[,1],
                             partion = 0)$rmse
  m_Error[2,2]  = for_errors(y = r_t[,2], yhat = c0[,3],
                             partion = 0)$rmse
  m_Error[2,3]  = for_errors(y = r_t[,2], yhat = c0[,6],
                             partion = 0)$rmse
  m_Error[2,4]  = for_errors(y = r_t[,2], yhat = c0[,12],
                             partion = 0)$rmse
  
  # MAE:
  m_Error[3,1]  = for_errors(y = r_t[,2], yhat = c0[,1],
                             partion = 0)$mae
  m_Error[3,2]  = for_errors(y = r_t[,2], yhat = c0[,3],
                             partion = 0)$mae
  m_Error[3,3]  = for_errors(y = r_t[,2], yhat = c0[,6],
                             partion = 0)$mae
  m_Error[3,4]  = for_errors(y = r_t[,2], yhat = c0[,12],
                             partion = 0)$mae
  
###### AR(3):
  
### Period: 2008M1:2009M12
  
  # RMSE:
  m_Error[5,1]  = for_errors(y = r_t[,2], yhat = c1[,1],
                             partion = 0)$rmse
  m_Error[5,2]  = for_errors(y = r_t[,2], yhat = c1[,3],
                             partion = 0)$rmse
  m_Error[5,3]  = for_errors(y = r_t[,2], yhat = c1[,6],
                             partion = 0)$rmse
  m_Error[5,4]  = for_errors(y = r_t[,2], yhat = c1[,12],
                             partion = 0)$rmse
  
  # MAE:
  m_Error[6,1]  = for_errors(y = r_t[,2], yhat = c1[,1],
                             partion = 0)$mae
  m_Error[6,2]  = for_errors(y = r_t[,2], yhat = c1[,3],
                             partion = 0)$mae
  m_Error[6,3]  = for_errors(y = r_t[,2], yhat = c1[,6],
                             partion = 0)$mae
  m_Error[6,4]  = for_errors(y = r_t[,2], yhat = c1[,12],
                             partion = 0)$mae
  
###### Factor model:
  
  # RMSE:
  m_Error[8,1]  = for_errors(y = r_t[,2], yhat = c2_factor[,1],
                             partion = 0)$rmse
  m_Error[8,2]  = for_errors(y = r_t[,2], yhat = c2_factor[,3],
                             partion = 0)$rmse
  m_Error[8,3]  = for_errors(y = r_t[,2], yhat = c1[,6],
                             partion = 0)$rmse
  m_Error[8,4]  = for_errors(y = r_t[,2], yhat = c1[,12],
                             partion = 0)$rmse
  
  # MAE:
  m_Error[9,1]  = for_errors(y = r_t[,2], yhat = c2_factor[,1],
                             partion = 0)$mae
  m_Error[9,2]  = for_errors(y = r_t[,2], yhat = c2_factor[,3],
                             partion = 0)$mae
  m_Error[9,3]  = for_errors(y = r_t[,2], yhat = c2_factor[,6],
                             partion = 0)$mae
  m_Error[9,4]  = for_errors(y = r_t[,2], yhat = c2_factor[,12],
                             partion = 0)$mae
  
###### Best subset model:
  
  # RMSE:
  m_Error[11,1]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,1],
                              partion = 0)$rmse
  m_Error[11,2]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,3],
                              partion = 0)$rmse
  m_Error[11,3]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,6],
                              partion = 0)$rmse
  m_Error[11,4]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,12],
                              partion = 0)$rmse
  
  # MAE:
  m_Error[12,1]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,1],
                              partion = 0)$mae
  m_Error[12,2]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,3],
                              partion = 0)$mae
  m_Error[12,3]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,6],
                              partion = 0)$mae
  m_Error[12,4]  = for_errors(y = r_t[,2], yhat = c3_best_subset[,12],
                              partion = 0)$mae
  
###### Forecast combination (simple average):
  
  # RMSE:
  m_Error[15,1]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,1],
                              partion = 0)$rmse
  m_Error[15,2]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,2],
                              partion = 0)$rmse
  m_Error[15,3]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,3],
                              partion = 0)$rmse
  m_Error[15,4]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,4],
                              partion = 0)$rmse
  
  # MAE:
  m_Error[16,1]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,1],
                              partion = 0)$mae
  m_Error[16,2]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,2],
                              partion = 0)$mae
  m_Error[16,3]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,3],
                              partion = 0)$mae
  m_Error[16,4]  = for_errors(y = r_t[,2], yhat = simple_average_forecast_comb[,4],
                              partion = 0)$mae
  
###### Forecast combination (Bates/Granger):
  
  # RMSE:
  m_Error[18,1]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,1],
                              partion = 0)$rmse
  m_Error[18,2]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,2],
                              partion = 0)$rmse
  m_Error[18,3]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,3],
                              partion = 0)$rmse
  m_Error[18,4]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,4],
                              partion = 0)$rmse
  
  # MAE:
  m_Error[19,1]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,1],
                              partion = 0)$mae
  m_Error[19,2]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,2],
                              partion = 0)$mae
  m_Error[19,3]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,3],
                              partion = 0)$mae
  m_Error[19,4]  = for_errors(y = r_t[,2], yhat = Bates_Granger_forecast_comb [,4],
                              partion = 0)$mae
  
###### Forecast combination (OLS-combination):
  
  # RMSE:
  m_Error[21,1]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,1],
                              partion = 0)$rmse
  m_Error[21,2]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,2],
                              partion = 0)$rmse
  m_Error[21,3]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,3],
                              partion = 0)$rmse
  m_Error[21,4]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,4],
                              partion = 0)$rmse
  
  # MAE:
  m_Error[22,1]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,1],
                              partion = 0)$mae
  m_Error[22,2]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,2],
                              partion = 0)$mae
  m_Error[22,3]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,3],
                              partion = 0)$mae
  m_Error[22,4]  = for_errors(y = r_t[,2], yhat = OLS_combination_forecast_comb[,4],
                              partion = 0)$mae

# Write to latex: 
  print(xtable(m_Error, digits = 3), file = "M_Error_wsize36")
  
############################# Out-of-sample R^2:
  
###### AR(3)
  
  AR3_R2OSh1_p1 <- out_of_sample_R2(r_t[,2], yhat = c1[,1], partion = 0, mean_forecast = c0[,1])
  AR3_R2OSh3_p1 <- out_of_sample_R2(r_t[,2], yhat = c1[,3], partion = 0, mean_forecast = c0[,3])
  AR3_R2OSh6_p1 <- out_of_sample_R2(r_t[,2], yhat = c1[,6], partion = 0, mean_forecast = c0[,6])
  AR3_R2OSh12_p1 <- out_of_sample_R2(r_t[,2], yhat = c1[,12], partion = 0, mean_forecast = c0[,12])
  
# Print the results: 
  AR3_R2OSh1_p1
  AR3_R2OSh3_p1
  AR3_R2OSh6_p1
  AR3_R2OSh12_p1
  
###### Factor model: 
  
  factor_model_R2OSh1_p1 <- out_of_sample_R2(r_t[,2], yhat = c2_factor[,1], partion = 0, mean_forecast = c0[,1])
  factor_model_R2OSh3_p1 <- out_of_sample_R2(r_t[,2], yhat = c2_factor[,3], partion = 0, mean_forecast = c0[,3])
  factor_model_R2OSh6_p1 <- out_of_sample_R2(r_t[,2], yhat = c2_factor[,6], partion = 0, mean_forecast = c0[,6])
  factor_model_R2OSh12_p1 <- out_of_sample_R2(r_t[,2], yhat = c2_factor[,12], partion = 0, mean_forecast = c0[,12])
  
 # Print the results: 
  factor_model_R2OSh1_p1
  factor_model_R2OSh3_p1
  factor_model_R2OSh6_p1
  factor_model_R2OSh12_p1
  
###### Best subset model: 
  
  c3_best_subset_R2OSh1_p1 <- out_of_sample_R2(r_t[,2], yhat = c3_best_subset[,1], partion = 0, mean_forecast = c0[,1])
  c3_best_subset_R2OSh3_p1 <- out_of_sample_R2(r_t[,2], yhat = c3_best_subset[,3], partion = 0, mean_forecast = c0[,3])
  c3_best_subset_R2OSh6_p1 <- out_of_sample_R2(r_t[,2], yhat = c3_best_subset[,6], partion = 0, mean_forecast = c0[,6])
  c3_best_subset_R2OSh12_p1 <- out_of_sample_R2(r_t[,2], yhat = c3_best_subset[,12], partion = 0, mean_forecast = c0[,12])
  
# Print the results: 
  c3_best_subset_R2OSh1_p1
  c3_best_subset_R2OSh3_p1
  c3_best_subset_R2OSh6_p1
  c3_best_subset_R2OSh12_p1
  
###### Forecast combination - simple average. 
  
  simple_average_forecast_comb_R2OSh1_p1 <- out_of_sample_R2(r_t[,2], yhat = simple_average_forecast_comb[,1], partion = 0, mean_forecast = c0[,1])
  simple_average_forecast_comb_R2OSh3_p1 <- out_of_sample_R2(r_t[,2], yhat = simple_average_forecast_comb[,2], partion = 0, mean_forecast = c0[,3])
  simple_average_forecast_comb_R2OSh6_p1 <- out_of_sample_R2(r_t[,2], yhat = simple_average_forecast_comb[,3], partion = 0, mean_forecast = c0[,6])
  simple_average_forecast_comb_R2OSh12_p1 <- out_of_sample_R2(r_t[,2], yhat = simple_average_forecast_comb[,4], partion = 0, mean_forecast = c0[,12])
  
# Print the results: 
  simple_average_forecast_comb_R2OSh1_p1
  simple_average_forecast_comb_R2OSh3_p1
  simple_average_forecast_comb_R2OSh6_p1
  simple_average_forecast_comb_R2OSh12_p1
  
###### Forecast combination - Bates/Granger
  
  Bates_Granger_forecast_comb_R2OSh1_p1 <- out_of_sample_R2(r_t[,2], yhat = Bates_Granger_forecast_comb[,1], partion = 0, mean_forecast = c0[,1])
  Bates_Granger_forecast_comb_R2OSh3_p1 <- out_of_sample_R2(r_t[,2], yhat = Bates_Granger_forecast_comb[,2], partion = 0,  mean_forecast = c0[,3])
  Bates_Granger_forecast_comb_R2OSh6_p1 <- out_of_sample_R2(r_t[,2], yhat = Bates_Granger_forecast_comb[,3], partion = 0,  mean_forecast = c0[,6])
  Bates_Granger_forecast_comb_R2OSh12_p1 <- out_of_sample_R2(r_t[,2], yhat = Bates_Granger_forecast_comb[,4], partion = 0,  mean_forecast = c0[,12])
  
# Print the results: 
  Bates_Granger_forecast_comb_R2OSh1_p1
  Bates_Granger_forecast_comb_R2OSh3_p1
  Bates_Granger_forecast_comb_R2OSh6_p1
  Bates_Granger_forecast_comb_R2OSh12_p1
  
###### Forecast combination - OLS 
  
  OLS_combination_forecast_comb_R2OSh1_p1 <- out_of_sample_R2(r_t[,2], yhat = OLS_combination_forecast_comb[,1], partion = 0,  mean_forecast = c0[,1])
  OLS_combination_forecast_comb_R2OSh3_p1 <- out_of_sample_R2(r_t[,2], yhat = OLS_combination_forecast_comb[,2], partion = 0,  mean_forecast = c0[,3])
  OLS_combination_forecast_comb_R2OSh6_p1 <- out_of_sample_R2(r_t[,2], yhat = OLS_combination_forecast_comb[,3], partion = 0,  mean_forecast = c0[,6])
  OLS_combination_forecast_comb_R2OSh12_p1 <- out_of_sample_R2(r_t[,2], yhat = OLS_combination_forecast_comb[,4], partion = 0,  mean_forecast = c0[,12])
  
# Print: 
  OLS_combination_forecast_comb_R2OSh1_p1
  OLS_combination_forecast_comb_R2OSh3_p1
  OLS_combination_forecast_comb_R2OSh6_p1
  OLS_combination_forecast_comb_R2OSh12_p1
  
  