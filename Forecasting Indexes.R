# Libraries that we need
if (!require("tidyquant")) install.packages("tidyquant"); library(tidyquant)
if (!require("tidyverse")) install.packages("tidyverse"); library(tidyverse)
if (!require("performance")) install.packages("performance"); library(performance)
if (!require("tidymodels")) install.packages("tidymodels"); library(tidymodels)
if (!require("see")) install.packages("see"); library(see)

# Local Variables
Ticker      <- "GC=F" # ^GSPC -> SP500 Index / ^IXIC -> Nasdaq Index / ^DJI -> Downjones Index
Ticker_Name <- "GOLD"
top_years   <- 6 # How many highly correlated years wants to use to traing your models

# Local Dataframes
db_correl      <- NULL
db_simulations <- NULL

# Deleting all the graphs and models from previous analysis
do.call(file.remove, list(list.files("Data - Model Validation/", full.names = TRUE)))
do.call(file.remove, list(list.files("Plots - Model analysis/", full.names = TRUE)))

# Getting historical data from Yahoo Finance
db_yahoo_data <- tq_get(Ticker,
       from = Sys.Date() - lubridate::years(100),
       to   = Sys.Date(),
       get  = "stock.prices", 
       complete_cases = TRUE) %>%
  dplyr::select(date, adjusted) %>%
  dplyr::mutate(Year = lubridate::year(date)) %>%
  dplyr::filter(Year > 1927) %>% # Just for the SP500
  dplyr::group_by(Year) %>%
  dplyr::mutate(Ret = TTR::ROC(adjusted, n = 1, type = "discrete")) %>%
  dplyr::ungroup() %>%
  replace(is.na(.), 0)

# Splitting the data into historical and current year
cYear <- db_yahoo_data %>%
  dplyr::filter(Year == lubridate::year(Sys.Date())) %>%
  dplyr::group_by(Year) %>%
  dplyr::mutate(Log_Ret  = log(1 + Ret),
                Acum_Ret = cumsum(Log_Ret),
                Dis_Ret  = exp(Acum_Ret) - 1,
                col_line = "green",
                num_day  = 1:n()) %>%
  dplyr::ungroup() %>%
  dplyr::select(date, Year, Dis_Ret, col_line, num_day)

hYear <- db_yahoo_data %>%
  dplyr::filter(Year != lubridate::year(Sys.Date())) %>%
  dplyr::group_by(Year) %>%
  dplyr::mutate(Log_Ret  = log(1 + Ret),
                Acum_Ret = cumsum(Log_Ret),
                Dis_Ret  = exp(Acum_Ret) - 1,
                col_line = "black",
                num_day  = 1:n()) %>%
  dplyr::mutate(nObs = n()) %>%
  dplyr::ungroup() %>%
  dplyr::filter(nObs > 200) %>% # We want to have a full year of data in the historical dataframe
  dplyr::select(-nObs) %>%
  dplyr::select(date, Year, Dis_Ret, col_line, num_day)

# Plotting all the results
hYear %>%
  bind_rows(cYear) %>%
  ggplot(aes(x = num_day, y = Dis_Ret, colour = col_line, group = Year)) +
  geom_line() + 
  scale_color_manual(values = c("gray", "blue")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title    = str_glue("Historical Performance of the {Ticker_Name} vs Current Year Performance. Ticker {Ticker}."),
       subtitle = str_glue("Pattern detection and seasonality analysis. Analysis for the {cYear$date %>% tail(n = 1)}"),
       caption  = "By: Carlos Jimenez",
       x = "Days in a Year",
       y = "Accumulated Return") +
  theme(legend.position = "none") + 
  geom_hline(yintercept = 0, 
             linetype   = "dotted", 
             color      = "darkgreen",  
             size       = 1)

# Plotting the YTD return up to the date of the analysis
hYear %>%
  bind_rows(cYear) %>%
  dplyr::group_by(Year) %>%
  dplyr::filter(num_day <= cYear$num_day %>% tail(n = 1)) %>%
  dplyr::filter(num_day == max(num_day)) %>%
  dplyr::ungroup() %>%
  dplyr::select(Year, Dis_Ret) %>%
  dplyr::mutate(Year = factor(Year, levels = Year)) %>%
  ggplot(aes(x = Year, y = Dis_Ret)) +
  geom_bar(stat = "identity") + 
  scale_color_manual(values = c("lightgray", "green")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title    = str_glue("What if Analysis: Historical Performance of the {Ticker_Name} vs Current Year Performance. Ticker {Ticker}."),
       subtitle = str_glue("Today's YTD ({cYear$date %>% tail(n = 1)}) vs Historical Same Date."),
       caption  = "By: Carlos Jimenez",
       x = "",
       y = "Accumulated Return") + 
  geom_hline(yintercept = cYear$Dis_Ret %>% tail(n = 1), 
             linetype   = "dotted", 
             color      = "blue",  
             size       = 1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Analyzing the Beta Correlation level between past years performance and current year
cYear_xts <- cYear %>% 
  dplyr::select(date, Dis_Ret) %>% 
  column_to_rownames(var = "date") %>% 
  as.xts()

for(i in seq(hYear$Year %>% first(), (hYear$Year %>% last()) - 1, 1)){ # i <- 1932
  
  # Subsetting the Historical dataset by Year and by Number of days in cYear
  hYear_xts <- hYear %>% dplyr::filter(Year == i) %>% 
    dplyr::slice(1:nrow(cYear_xts)) %>% 
    dplyr::select(date, Dis_Ret) %>% 
    dplyr::mutate(date = index(cYear_xts)) %>%
    column_to_rownames(var = "date") %>% 
    as.xts()

  # Saving the info
  db_correl <- db_correl %>%  
    bind_rows(data.frame(Year = i,
                         Corr = PerformanceAnalytics::table.Correlation(cYear_xts, hYear_xts)$Correlation))
}

# Getting the years with the most correlated performance vs current year
db_correl_top <- db_correl %>%
  dplyr::arrange(desc(Corr)) %>%
  dplyr::top_n(ifelse(top_years > 7, 7, top_years), wt = Corr)

# Plotting the most correlated years vs current years
data_chart <- hYear %>%
  dplyr::filter(Year %in% db_correl_top$Year) %>%
  bind_rows(cYear) %>%
  dplyr::mutate(Year = Year %>% as.character())

data_chart %>%
  ggplot(aes(x = num_day, y = Dis_Ret, colour = Year, group = Year)) +
  geom_line(data = filter(data_chart, Year != Sys.Date() %>% lubridate::year() %>% as.character()), size = 0.8, alpha = 0.5) + 
  geom_line(data = filter(data_chart, Year == Sys.Date() %>% lubridate::year() %>% as.character()), size = 1.1) +
  scale_y_continuous(labels = scales::percent) +
  labs(title    = str_glue("Most correlated Past Year Performance and Current Year Performance. Ticker {Ticker}."),
       subtitle = str_glue("Analysis done on the {Ticker_Name}"),
       caption  = "By: Carlos Jimenez",
       x = "Days in a Year",
       y = "Accumulated Return") +
  geom_hline(yintercept = 0, 
             linetype   = "dotted", 
             color      = "darkgreen",  
             size       = 1) +
  theme(legend.position = "bottom",
        legend.title    = element_blank()) +
  scale_colour_manual(values=c("gray", "blue", "green", "black", "orange", "purple", "steelblue", "red"))

# Training and forecasting multiple models (using multiple years as regressors)
for(i in 1:(db_correl_top$Year %>% length())){ # i <- 2
  
  # Subsetting the Years
  hist_years <- db_correl_top %>% 
    dplyr::slice(1:i) %>%
    dplyr::select(Year) %>%
    pull(1)
  
  # Training a model to forecast where the Underlying will end
  db_training <- hYear %>%
    dplyr::filter(Year %in% db_correl_top$Year) %>%
    bind_rows(cYear) %>%
    dplyr::filter(Year %in% c(hist_years, lubridate::year(Sys.Date()))) %>%
    dplyr::select(num_day, Dis_Ret, Year) %>%
    spread(Year, Dis_Ret) %>%
    na.omit() %>%
    dplyr::select(-num_day) %>%
    rename("Label" = lubridate::year(Sys.Date()) %>% as.character())

  # Building the Linear Regression Models
  MLRM_Model <- linear_reg() %>%
    set_engine("lm") %>%
    fit(Label ~ ., data = db_training)
  
    # Saving the data and the model for further analysis
    saveRDS(db_training, str_glue("Data - Model Validation/Data_Model_Features_{hist_years %>% length()}.rds"))
    saveRDS(MLRM_Model, str_glue("Data - Model Validation/LM_Model_Features_{hist_years %>% length()}.rds"))
    
  # Predicting the Test set results
  db_test <- hYear %>%
    dplyr::filter(Year %in% db_correl_top$Year) %>%
    bind_rows(cYear) %>%
    dplyr::filter(Year %in% c(hist_years, lubridate::year(Sys.Date()))) %>%
    dplyr::select(num_day, Dis_Ret, Year) %>%
    spread(Year, Dis_Ret) %>%
    dplyr::slice(nrow(db_training) +1 :n()) %>%
    column_to_rownames(var = "num_day") %>%
    dplyr::select(-c(lubridate::year(Sys.Date()) %>% as.character())) %>%
    na.omit()
  
  y_pred = predict(MLRM_Model, new_data = db_test)
  
  # Adjusting the forecast
  First_Forecast_Observation   <- y_pred %>% first()
  First_Historical_Observation <- cYear %>% dplyr::select(Dis_Ret) %>% tail(n = 1)
  Delta <- (First_Historical_Observation - First_Forecast_Observation) %>% pull(1)
  
  # Saving Simulations
  db_simulations <- db_simulations %>%
    bind_rows(data.frame(num_day  = y_pred %>% rownames() %>% as.numeric(),
                         Dis_Ret  = y_pred + Delta,
                         ID_Model = i))
}

# Validating the models (run this part manually analyzing each model)
total_models <- (list.files("Data - Model Validation/") %>% length())/2
which_model  <- 2 # Select the ID of the model (go to Data - Model Validation and see the final number of every file in with the following name: LM_Model_Features_XXX-rds)

if(which_model <= total_models){
  model <- readRDS(str_glue("Data - Model Validation/LM_Model_Features_{which_model}.rds"))  
  
  png(str_glue("Plots - Model analysis/Model_Validation_{which_model}_Features.png"), width=800, height=550)
  check_model(model)
  dev.off()
}else{
  warning("That Model ID does not exist")
}  

# Plotting the results
First_Price <- db_yahoo_data %>%
  dplyr::filter(Year == lubridate::year(Sys.Date())) %>%
  dplyr::select(adjusted) %>%
  pull(1) %>%
  first()

Last_Day_Training <- db_training %>% nrow() # Getting the Last Day in training set

db_simulations <- db_simulations %>% # Increasing in 1 the num_day column using the Last_Day_Training
  as_tibble() %>%
  dplyr::group_by(ID_Model) %>%
  dplyr::mutate(num_day = num_day + Last_Day_Training) %>%
  dplyr::ungroup()

  # Adapting historical database
  db_cYear_enhanced <- cYear %>%
    dplyr::select(num_day, Dis_Ret) %>%
    dplyr::mutate(ID_Model = 0)
  
  db_simulations_enhanced <- db_simulations %>%
    dplyr::select(c(num_day, .pred, ID_Model)) %>%
    dplyr::group_by(num_day) %>%
    dplyr::summarise(min_Ret  = min(.pred),
                     mean_Ret = mean(.pred),
                     max_Ret  = max(.pred))

db_cYear_enhanced %>%
  ggplot(aes(x = num_day, y = Dis_Ret)) +
  geom_line(colour = "darkgray", size = 0.8) +
  geom_line(data = db_simulations_enhanced, aes(x = num_day, y = min_Ret), colour = "blue", linetype  = "dashed", size = 0.5) +
  geom_line(data = db_simulations_enhanced, aes(x = num_day, y = mean_Ret), colour = "black", size = 0.8) +
  geom_line(data = db_simulations_enhanced, aes(x = num_day, y = max_Ret), colour = "blue", linetype  = "dashed", size = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(title    = str_glue("Future possible scenario of the {Ticker_Name}"),
       subtitle = "Using a Multiple Linear Regression Model.",
       caption  = "By: Carlos Jimenez",
       x = "Days in a Year",
       y = "Accumulated Return") +
  theme(legend.position = "none") + 
  geom_vline(xintercept = Last_Day_Training, 
             linetype   = "dotted", 
             color      = "black", 
             size       = 0.5) + 
  geom_hline(yintercept = 0, 
             linetype   = "dashed", 
             color      = "gray", 
             size       = 0.5) 

