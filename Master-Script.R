###############################################################################################
# Investment Strategies / WS 21 / BAB Replication
# Group: Niklas Leibinger, Elias Schreiber, Yannick Strobl, Nicolas Tschütscher
###############################################################################################

##
# github link: https://github.com/NicolasTsch/Investment-Strategies
##

##########
# Packages (to be loaded at the beginning!!)
##########

library(readxl)
library(tidyverse)
library(dplyr)
library(timetk)
library(tidyquant)
library(roll)
library(lubridate)
library(broom)
library(tibble)
library(tidyr)
library(purrr)
library(xtable)
library(rio)
library(stringi)


###############################################################################################
####################
# 1. Data
####################
# Note:
# - In the following code bits we import the raw data, prepare each dataset to make it survivorship bias free and then combine them 
# into one data set
# - In case this part shall be skipped, the final and complete data set can be loaded in line 207 (but load packages first!)

####################
# 1.1 Data Import
####################

### 3m TBill and 3m Eurodollar LIBOR (both from FRED) daily from 1990-01-01 till 2021-11-26
import1 <- read_xlsx("Daten/TB and LIBOR.xlsx", sheet="daily")
int.rates <- import1 %>% as_tibble() %>% dplyr::mutate(Date=as.Date(Date),
                                                       across(TB3M:EDL3M,~./100),                  # take the rates as decimals
                                                       TEDspr = EDL3M - TB3M) %>%                  # calc- TED spread as per definition
                                        fill(EDL3M, .direction = "down") %>%
                                        fill(TB3M, .direction = "down")


### SP500 Index daily (from Yahoo) from 1990-01-01 till 2021-11-26                                 # Yahoo can be accessed through the quantmod package (loaded with tidyquant)   
getSymbols("^GSPC", from = '1990-01-01',
           to = "2021-11-29",warnings = FALSE,
           auto.assign = TRUE)

GSPCdaily <- GSPC %>% tk_tbl(rename_index="Date") %>%
  dplyr::mutate(GSPC.ret = (GSPC.Close - lag(GSPC.Close))/GSPC.Close,                              # calculate the return of the index
                Date=as.Date(Date)) %>%
  dplyr::select(Date, GSPC.Close, GSPC.ret) %>% na.omit()


### Fama French 5 Factors loading 
install.packages("FFdownload")
library(FFdownload)                                                                                # package from Sebastian
tempf <- tempfile(fileext = ".RData")
inputlist <- c("F-F_Research_Data_5_Factors_2x3","F-F_Momentum_Factor")                            # we need the FF5 factors and the momentum factor
FFdownload(output_file = tempf, inputlist=inputlist)
load(tempf)

FF5_daily <- FFdata$`x_F-F_Research_Data_5_Factors`$daily$Temp2 %>%
  tk_tbl(rename_index="Date") %>%
  dplyr::mutate(Date=as.Date(Date)) %>%
  dplyr::filter(Date >= "1990-01-01") %>%                                                          # filter the correct date                                                  
  dplyr::mutate(across(Mkt.RF:RF,~./100))                                                          # make the factors as decimals

MOM_daily <- FFdata$`x_F-F_Momentum_Factor`$daily$Temp2 %>%
  tk_tbl(rename_index="Date") %>%
  dplyr::mutate(Date=as.Date(Date),
                Mom=Mom/100) %>%                                                                   # make the factors as decimals
  dplyr::filter(Date >= "1990-01-01")                                                              # filter the correct date  

FF_daily <- FF5_daily %>% left_join(MOM_daily, by="Date")                                          # put the datasets together

# monthly

FF5_monthly <- FFdata$`x_F-F_Research_Data_5_Factors`$monthly$Temp2 %>%
  tk_tbl(rename_index="Date") %>%
  dplyr::mutate(Date=as.Date(Date)) %>%
  dplyr::filter(Date >= "1990-01-01") %>%                                                          # filter the correct date                                                  
  dplyr::mutate(across(Mkt.RF:RF,~./100))                                                          # make the factors as decimals

MOM_monthly <- FFdata$`x_F-F_Momentum_Factor`$monthly$Temp2 %>%
  tk_tbl(rename_index="Date") %>%
  dplyr::mutate(Date=as.Date(Date),
                Mom=Mom/100) %>%                                                                   # make the factors as decimals
  dplyr::filter(Date >= "1990-01-01")                                                              # filter the correct date  

FF_monthly <- FF5_monthly %>% left_join(MOM_monthly, by="Date")                                    # put the datasets together


### Load SP500 constituents for each month between 1990-01-01 and 2021-11-01
constituents_import   <- read_xlsx("Daten/SP500_RI_data.xlsx", sheet = "Constituents")
constituents_import_2 <- constituents_import[-1,-1]

constituents_vec <- as.vector(as.matrix(constituents_import_2))
constituents     <- unique(constituents_vec)                                                        # we need the unique constituents list
constituents_tbl <- as_tibble(constituents)                                                         # Unique constituents in a tibble format
# Note we also used the unique constituents to retrieve the daily timeseries in reuters


### Total return data of each constituent between 1990-01-01 and 2021-11-01
RI_import   <- read_xlsx("Daten/SP500_RI_data.xlsx", sheet = "TS_daily")
RI_import_2 <- RI_import %>% dplyr::rename(Date = "Name")
save(RI_import, file="RI_Import.RData")

###############################################################################################

####################
# 1.2 Data Preparation
####################

### names and codes
names <- colnames(RI_import_2)[-1]                                                                  # get stock names
codes <- as.vector(RI_import_2[1,-1]) %>% as.character()                                            # get stock codes
names_code <- as.tibble(cbind(names, codes))                                                        # combine names and codes into a tibble
colnames(names_code) <- c("Instrument","Code")                                                      # rename column names

names_code$Code <- names_code$Code %>%
  stri_replace_all_fixed("(RI)", "")                                                                # replace code structure
names_code$Instrument <- names_code$Instrument %>%
  stri_replace_all_fixed(" - TOT RETURN IND", "") 



### prepare daily return dataset
RI_raw <- RI_import_2[-1,]
# Generate right date format and make all prices numeric values 
RI_raw2 <- RI_raw %>%
  mutate(Date=as.Date(as.numeric(Date), origin = "1899-12-30")) %>%                                 # generate right date format
  mutate_if(is.character, as.numeric) %>%                                                           # make all RI's numeric values
  pivot_longer(-Date, names_to = "Instrument", values_to = "RI") %>%                                # bring it into a long format
  mutate(Instrument = str_replace(Instrument, " - TOT RETURN IND", "")) %>%                         # replace instrument structure
  left_join(names_code, by="Instrument")                                                            # add Instrument codes

RI_raw3 <- RI_raw2 %>% mutate(Date_mon = as.yearmon(Date))                                          # add year-mon column
RI_raw4 <- RI_raw3[,c(1,5,2,4,3)]                                                                   # arrange column order


### prepare constituents dataset
constituents_raw <- constituents_import %>%
  mutate(Date = as_date(Date), Date_mon = as.yearmon(Date)) %>%
  dplyr::arrange(desc(row_number()))

constituents_raw2 <- constituents_raw %>%
  select(Date, Date_mon) %>%
  left_join(constituents_raw, by=c("Date", "Date_mon")) %>%
  mutate_if(is.numeric, as.character)

colnames(constituents_raw2) <- c("Date", "Date_mon",1:506)                                         # add new columns, no. of stocks in index

constituents_raw3 <- constituents_raw2 %>%
  pivot_longer(-c(Date, Date_mon), names_to = "Flag", values_to = "Code") %>%                      # flag each Instrument
  na.omit() %>%
  filter(Code != "NA")


####################
# 1.3 Data Finalisation
####################

### put all date together into one data set
SP500_raw <- RI_raw4 %>%
  left_join(constituents_raw3, by=c("Date", "Date_mon","Code")) %>%                                 # join constituents
  left_join(GSPCdaily, by = "Date") %>%                                                             # join GSPC: the SP500 index return
  left_join(int.rates %>% select(Date, TB3M, EDL3M, TEDspr), by="Date") %>%                         # join TBILL rate
  left_join(FF_daily, by ="Date") %>%                                                               # add FF5 and momentum factor
  filter(Date <= "2021-11-01")                                                                      # filter dates <= "2021-11-01"

# Bring monthly Flag to daily Flag
SP500_raw2 <- SP500_raw %>%
  group_by(Instrument) %>%
  mutate(prev_val = Flag, next_val = Flag) %>%
  fill(prev_val, .direction = "down") %>%
  fill(next_val, .direction = "up")

SP500_raw3 <- SP500_raw2 %>%
  mutate(value = ifelse(prev_val == next_val, prev_val, Flag),
         prev_val = as.numeric(prev_val),
         next_val = as.numeric(next_val),
         value = as.numeric(value))

SP500_raw4 <- SP500_raw3 %>%
  mutate(value_2 = ifelse(prev_val != "NA" & next_val != "NA", next_val, NA), Flag = value_2) %>%
  select(-prev_val, -next_val, -value, -value_2)

# Flag which is not NA defines the assets which are listed in the index
SP500_raw5 <- SP500_raw4 %>% filter(Flag != "NA")


SP500_data <- SP500_raw5 %>%
  dplyr::mutate(Return = log(RI)-lag(log(RI)),                                                      # asset return
                Inst.rf = Return-TB3M,                                                              # excess return asset
                Mkt.rf = GSPC.ret - TB3M) %>%                                                       # excess return market
  filter(Mkt.RF != "NA")                                                                            

SP500_final <- SP500_data %>% dplyr::filter (Date >= "1990-01-03")


### final save and load option for subsequent calculations 
save(SP500_final, file="SP500_final.RData")
load("SP500_final.RData")


###############################################################################################
####################
# 2. Calculations: Ex Ante Betas and Portfolios
####################
# Notes:
# - In this section we calculate the ex ante betas with a while loop for the whole sample period
# and directly assign them in the respective portfolio (deciles -> 10 portfolios)

# !!! Important note:
# - There are 4 options for the beta estimation in the while loop
# - The first option is in line with the paper, i.e., the betas are calculated as beta = correlation * (sd instrument / sd index) with
# a 5 year window for the correlation and a 2 year window for the standard devation. In option 2 and 3 the windows are unified.
# - Option 4 calculates the betas with a CAPM estimation, as suggested in the critique by Novy-Marx & Velikov (2021)
# - Before running the while-loop, the relevant code part must be uncommented (the others commented)!!
###


## data set prep
# Dataset for the first while loop (Beta calculation) 
SP500_data_w1 <- SP500_final %>% mutate(Inst.RF=Return-RF) %>% select(Date, Instrument, Mkt.RF, Inst.RF) #RF from K. French is one-month TBILL
# TBill rate for the second while loop (BAB calculation) 
TBill <- SP500_final %>% ungroup() %>% expand(Date) %>% left_join(SP500_final %>% ungroup() %>% select(Date, RF), by="Date")


### First While-loop: Betas and PF weights

## Define parameters
roll  <- 60                                                                      # rolling Window 5 Years
rebal <- 1                                                                       # rebalance each 30 days (~monthly)
ult.startp <- "1990-01-03"                                                       # first day of Portfolio Optimization
ult.endp   <- "2021-10-02"                                                       # last day of Portfolio Optimization
w <- 0.6                                                                         # weight for beta shrinkage (same value as used in the paper)
beta.xs <- 1                                                                     # assumption for cross sectional beta (not 100% correct in this case -> only SP500 and not the whole market)

startp <- as.Date(ult.startp)                                                    # insample start point
endp   <- as.Date(ult.startp) %m+% months(roll) - 1                              # insample end point


## create empty lists to store the portfolio constituents and betas at each t
beta.L.list        <- vector(mode = "list", length = 350)
beta.H.list        <- vector(mode = "list", length = 350)
weights.long.list  <- vector(mode = "list", length = 350)
weights.short.list <- vector(mode = "list", length = 350)
names.long.list    <- vector(mode = "list", length = 350)
names.short.list   <- vector(mode = "list", length = 350)

#empty portfolio lists
i <- 1
for(i in 1:10){x <- paste("names.PF",i,".list", sep=""); assign(x, vector(mode = "list", length = 350))}

#empty beta lists
i <- 1
for(i in 1:10){x <- paste("beta.ex",i,".list", sep=""); assign(x, vector(mode = "list", length = 350))}


i <- 1
while(endp <= as.Date(ult.endp) %m-% months(rebal)){
  cat("Calculating Betas",as.character(startp),"to",as.character(endp),"!\n")
  ## Define window
  
  ###
  # uncomment desired beta estimation!
  ###
  
  
  #Beta calc: option paper (differing horizons)
  # data.is <- SP500_data_w1 %>%
  #   filter(Date>=startp,Date<=endp) %>% na.omit() %>%
  #   mutate(corr     = roll_cor(Inst.RF, Mkt.RF, width=1250, min_obs = 750),    # rolling correlation, 5 year window (250 trading days per year)
  #          sd.inst  = roll_sd(Inst.RF, width = 250, min_obs = 120),            # rolling standard deviatoin, 1 year window
  #          sd.index = roll_sd(Mkt.RF, width = 250, min_obs = 120),
  #          Marker   = ifelse(!is.na(corr), T, F),                              # mark NA's
  #          beta     = corr*(sd.inst/sd.index)) %>% filter(Marker == TRUE)      # calc beta and filter NA's

  
  #Beta calc: option paper (same horizon 5 years)
  # data.is <- SP500_data_w1 %>%
  #  filter(Date>=startp,Date<=endp) %>% na.omit() %>%
  #  mutate(corr     = roll_cor(Inst.RF, Mkt.RF, width=1250, min_obs = 750),     # 5 year window
  #         sd.inst  = roll_sd(Inst.RF, width = 1250, min_obs = 750),            # 5 year window
  #         sd.index = roll_sd(Mkt.RF, width = 1250, min_obs = 750),
  #         Marker   = ifelse(!is.na(corr), T, F),
  #         beta     = corr*(sd.inst/sd.index)) %>% filter(Marker == TRUE)

  
  #Beta calc: option paper (same horizon 1 year)
  # data.is <- SP500_data_w1 %>%
  #   filter(Date>=startp,Date<=endp) %>% na.omit() %>%
  #  mutate(corr     = roll_cor(Inst.RF, Mkt.RF, width=250, min_obs = 120),     # 1 year window
  #         sd.inst  = roll_sd(Inst.RF, width = 250, min_obs = 120),            # 1 year window
  #         sd.index = roll_sd(Mkt.RF, width = 250, min_obs = 120),
  #         Marker   = ifelse(!is.na(corr), T, F),
  #         beta     = corr*(sd.inst/sd.index)) %>% filter(Marker == TRUE)
   
  
  #Beta calc: option CAPM estimation
  data.capm.w1 <- SP500_data_w1 %>%
    filter(Date>=startp,Date<=endp) %>% na.omit()
  data.is  <- data.capm.w1 %>% mutate(beta = coef(roll_lm(Mkt.RF, Inst.RF, width =250, intercept = FALSE)), # rolling regression only extract the beta coefficient 
                                      Marker = ifelse(!is.na(beta), T, F)) %>% filter(Marker == TRUE)
  
  ###
  # end of beta estimation selection
  ### 
  
  rows <- nrow(data.is); last.date <- data.is[rows,1] %>% .$Date
  
  data.beta <- data.is %>%
    filter(Date == last.date) %>%
    select(Instrument, beta)%>%
    mutate(beta.s = w*beta+(1-w)*beta.xs) %>%                                     # beta.s = shrinked beta with w = 0.6, cross sectional beta = 1
    arrange(desc(beta.s))                                                         # order the betas in descending order
  
  
  ## Calculate neccessary input parameters
  median   <- median(data.beta$beta.s)                                            # Median (cross section)
  z        <- rank(data.beta$beta.s)                                              # Ranks
  z.bar    <- mean(z)                                                             # Average rank
  diff     <- z-z.bar                                                             # difference
  abs.diff <- abs(z-z.bar)                                                        # Absolute difference between rank and average rank
  sum      <- sum(abs.diff)                                                       # Sum of absolute differences
  k        <- rep(2/sum, nrow(data.beta))                                         # Normalizing constant
  
  
  ## Calculate portfolio weights
  data.beta$z        <- z
  data.beta$diff     <- diff
  data.beta$abs.diff <- abs.diff
  data.beta$k        <- k
  
  
  ## Add two weight columns
  data.beta.2 <- data.beta %>%
    dplyr::mutate(wH = k*max(diff,0),
                  wL = k*min(diff,0),
                  wL = wL*(-1),
                  beta.weighted = ifelse(wH != 0, wH*beta, wL*beta)) %>%         # calc. beta weighted
    ungroup() %>%
    mutate(decile = ntile(beta.s, 10))
  
  
  ## Assign into long and short portfolio
  short.instruments <- data.beta.2 %>% filter(wH != 0) %>% .$Instrument
  long.instruments  <- data.beta.2 %>% filter(wL != 0) %>% .$Instrument
  
  beta.short <- data.beta.2 %>% filter(wH != 0) %>% ungroup() %>% summarise(beta = sum(beta.weighted)) %>% .$beta
  beta.long  <- data.beta.2 %>% filter(wL != 0) %>% ungroup() %>% summarise(beta = sum(beta.weighted)) %>% .$beta
  
  
  ## Calculate weight of long and short portfolios
  beta.H <- 1/beta.short                                                         # calculate the leverage
  beta.L <- 1/beta.long
  
  
  ## Store calculation results
  beta.H.list[[i]]        <- beta.H
  beta.L.list[[i]]        <- beta.L
  weights.long.list[[i]]  <- data.beta.2 %>% filter(Instrument %in% long.instruments) %>% select(Instrument, wL)
  weights.short.list[[i]] <- data.beta.2 %>% filter(Instrument %in% short.instruments) %>% select(Instrument, wH) 
  names.long.list[[i]]    <- long.instruments
  names.short.list[[i]]   <- short.instruments
  
  
  ## Sort Portfolios
  names.PF1  <- data.beta.2 %>% filter(decile == 1) %>% .$Instrument
  names.PF2  <- data.beta.2 %>% filter(decile == 2) %>% .$Instrument
  names.PF3  <- data.beta.2 %>% filter(decile == 3) %>% .$Instrument
  names.PF4  <- data.beta.2 %>% filter(decile == 4) %>% .$Instrument
  names.PF5  <- data.beta.2 %>% filter(decile == 5) %>% .$Instrument
  names.PF6  <- data.beta.2 %>% filter(decile == 6) %>% .$Instrument
  names.PF7  <- data.beta.2 %>% filter(decile == 7) %>% .$Instrument
  names.PF8  <- data.beta.2 %>% filter(decile == 8) %>% .$Instrument
  names.PF9  <- data.beta.2 %>% filter(decile == 9) %>% .$Instrument
  names.PF10 <- data.beta.2 %>% filter(decile == 10) %>% .$Instrument
  
  ## Ex-Ante Betas
  beta.ex1  <- data.beta.2 %>% filter(decile == 1) %>% summarise(beta.ex = mean(beta.s)) %>% .$beta.ex
  beta.ex2  <- data.beta.2 %>% filter(decile == 2) %>% summarise(beta.ex = mean(beta.s)) %>% .$beta.ex
  beta.ex3  <- data.beta.2 %>% filter(decile == 3) %>% summarise(beta.ex = mean(beta.s)) %>% .$beta.ex
  beta.ex4  <- data.beta.2 %>% filter(decile == 4) %>% summarise(beta.ex = mean(beta.s)) %>% .$beta.ex
  beta.ex5  <- data.beta.2 %>% filter(decile == 5) %>% summarise(beta.ex = mean(beta.s)) %>% .$beta.ex
  beta.ex6  <- data.beta.2 %>% filter(decile == 6) %>% summarise(beta.ex = mean(beta.s)) %>% .$beta.ex
  beta.ex7  <- data.beta.2 %>% filter(decile == 7) %>% summarise(beta.ex = mean(beta.s)) %>% .$beta.ex
  beta.ex8  <- data.beta.2 %>% filter(decile == 8) %>% summarise(beta.ex = mean(beta.s)) %>% .$beta.ex
  beta.ex9  <- data.beta.2 %>% filter(decile == 9) %>% summarise(beta.ex = mean(beta.s)) %>% .$beta.ex
  beta.ex10 <- data.beta.2 %>% filter(decile == 10) %>% summarise(beta.ex = mean(beta.s)) %>% .$beta.ex
  
  names.PF1.list[[i]]  <- names.PF1
  names.PF2.list[[i]]  <- names.PF2
  names.PF3.list[[i]]  <- names.PF3
  names.PF4.list[[i]]  <- names.PF4
  names.PF5.list[[i]]  <- names.PF5
  names.PF6.list[[i]]  <- names.PF6
  names.PF7.list[[i]]  <- names.PF7
  names.PF8.list[[i]]  <- names.PF8
  names.PF9.list[[i]]  <- names.PF9
  names.PF10.list[[i]] <- names.PF10
  
  beta.ex1.list[[i]]  <- beta.ex1
  beta.ex2.list[[i]]  <- beta.ex2
  beta.ex3.list[[i]]  <- beta.ex3
  beta.ex4.list[[i]]  <- beta.ex4
  beta.ex5.list[[i]]  <- beta.ex5
  beta.ex6.list[[i]]  <- beta.ex6
  beta.ex7.list[[i]]  <- beta.ex7
  beta.ex8.list[[i]]  <- beta.ex8
  beta.ex9.list[[i]]  <- beta.ex9
  beta.ex10.list[[i]] <- beta.ex10
  
  
  i <- i+1
  startp <- as.Date(startp) %m+% months(rebal)
  endp <- as.Date(startp) %m+% months(roll)-1
}




### Second while-loop: Calculate BAB Factor
## Define parameters
rebal <- 1                                                                     # Rebalance monthly
ult.startp <- "1995-01-03"                                                     # First day of Portfolio Optimization
ult.endp   <- "2021-10-02"                                                     # Last day of Portfolio Optimization

startp <- as.Date(ult.startp)
endp   <- as.Date(ult.startp) %m+% months(rebal) - 1

## Store the results of each iteration
BAB.list    <- vector(mode = "list", length = 350)

##  Data for While-loop 2
SP500_data_w2 <- SP500_final %>% select(Date, Instrument, Return) %>% complete(nesting(Instrument), Date)

j <- 1
while(endp <= as.Date(ult.endp)){
  cat("Calculating BAB of",as.character(startp),"to",as.character(endp),"!\n")
  
  # empty lists for results
  weights.long  <- weights.long.list[[j]]
  weights.short <- weights.short.list[[j]] 
  names.long    <- names.long.list[[j]]
  names.short   <- names.short.list[[j]]
  beta.L        <- beta.L.list[[j]]
  beta.H        <- beta.H.list[[j]]
  
  
  return.low <- SP500_data_w2 %>%
    filter(Date>=startp,Date<=endp, Instrument %in% names.long) %>%
    left_join(weights.long, by="Instrument") %>%
    mutate(Return.w = Return * wL)
  
  # return of long portfolio
  pf.long.ret <- return.low %>% summarise(ret = sum(Return.w)) %>% summarise(PF = sum(ret)) %>% .$PF
  
  
  return.high <- SP500_data_w2 %>%
    filter(Date>=startp,Date<=endp, Instrument %in% names.short) %>%
    left_join(weights.short, by="Instrument") %>%
    mutate(Return.w = Return * wH)
  
  # return of short portfolio
  pf.short.ret <- return.high %>% summarise(ret = sum(Return.w)) %>% summarise(PF = sum(ret)) %>% .$PF
  
  Date.rf <- return.high %>% ungroup() %>% select(Date)
  n <- nrow(Date.rf)
  Date.rf <- Date.rf[n,] %>% .$Date
  
  rf <- TBill %>% filter(Date %in% as.Date(Date.rf)) %>% select(RF) %>% .$RF
  rf <- rf[[1]]
  
  leverage.low  <- beta.L
  leverage.high <- beta.H
  
  # calculate BAB factor
  BAB <- leverage.low*(pf.long.ret-rf) - leverage.high*(pf.short.ret-rf)        # go long leveraged low beta portfolio and short deleveraged high beta portfolio
  BAB.list[[j]] <- BAB
  
  j <- j+1
  startp <- as.Date(startp) %m+% months(rebal)
  endp <- as.Date(startp) %m+% months(rebal) - 1
}




### Third while-loop: Add Dates for Performance Measurement -> this part is needed to assign a correct date vector to the calculated BAB factor
## Define parameters
rebal <- 1                            # Rebalance monthly
ult.startp <- "1995-01-03"             # First day of Portfolio Optimization
ult.endp   <- "2021-10-02"             # Last day of Portfolio Optimization

startp <- as.Date(ult.startp)
endp   <- as.Date(ult.startp) %m+% months(rebal) - 1

## Store the results of each iteration
date.list    <- vector(mode = "list", length = 350)

j <- 1
while(endp <= as.Date(ult.endp)){
  date.list[[j]] <- endp
  
  startp <- as.Date(startp) %m+% months(rebal)
  endp <- as.Date(startp) %m+% months(rebal) - 1
  
  j <- j+1
}


Dates <- as_tibble(as.vector(unlist(date.list)))
colnames(Dates) <- "Date"
Dates <- Dates %>% mutate(Date = as.Date(as.numeric(Date)))

BAB <- as.vector(unlist(BAB.list))
#sum(BAB)
Dates$BAB <- BAB 
Dates$BAB.ann <- Dates$BAB*12




### Calculate Portfolio Returns

## Define parameters
rebal <- 1                             # Rebalance monthly
ult.startp <- "1995-01-03"             # First day of Portfolio Optimization
ult.endp   <- "2021-10-02"             # Last day of Portfolio Optimization

startp <- as.Date(ult.startp)
endp   <- as.Date(ult.startp) %m+% months(rebal) - 1


## create empty lists
for (i in 1:10) {x <- paste("PF",i,".list",sep = "");assign(x,vector(mode = "list", length = 350))}

##  Data for While-loop 4
SP500_data_w4 <- SP500_final %>% mutate(Inst.RF=Return-RF) %>% select(Date, Instrument, Inst.RF, Mkt.RF) %>% complete(nesting(Instrument), Date)
SP500_data_w4 <- SP500_data_w1 %>% select(Date, Instrument, Mkt.RF, Inst.RF) %>% complete(nesting(Instrument), Date)

i <- 1
while(endp <= as.Date(ult.endp)){
  cat("Calculating Portfolio Returns from",as.character(startp),"to",as.character(endp),"!\n")
  PF.data <- SP500_data_w4 %>% filter(Date>=startp,Date<=endp) %>% ungroup() %>% na.omit()
 
  PF1.data <- PF.data %>% filter(Instrument %in% names.PF1.list[[i]])
  n.inst <- nrow(PF1.data %>% dplyr::count(Instrument))
  PF1.perf <- PF1.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.RF)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF2.data <- PF.data %>% filter(Instrument %in% names.PF2.list[[i]])
  n.inst <- nrow(PF2.data %>% dplyr::count(Instrument))
  PF2.perf <- PF2.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.RF)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF3.data <- PF.data %>% filter(Instrument %in% names.PF3.list[[i]])
  n.inst <- nrow(PF3.data %>% dplyr::count(Instrument))
  PF3.perf <- PF3.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.RF)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF4.data <- PF.data %>% filter(Instrument %in% names.PF4.list[[i]])
  n.inst <- nrow(PF4.data %>% dplyr::count(Instrument))
  PF4.perf <- PF4.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.RF)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF5.data <- PF.data %>% filter(Instrument %in% names.PF5.list[[i]])
  n.inst <- nrow(PF5.data %>% dplyr::count(Instrument))
  PF5.perf <- PF5.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.RF)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF6.data <- PF.data %>% filter(Instrument %in% names.PF6.list[[i]])
  n.inst <- nrow(PF6.data %>% dplyr::count(Instrument))
  PF6.perf <- PF6.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.RF)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF7.data <- PF.data %>% filter(Instrument %in% names.PF7.list[[i]])
  n.inst <- nrow(PF7.data %>% dplyr::count(Instrument))
  PF7.perf <- PF7.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.RF)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF8.data <- PF.data %>% filter(Instrument %in% names.PF8.list[[i]])
  n.inst <- nrow(PF8.data %>% dplyr::count(Instrument))
  PF8.perf <- PF8.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.RF)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF9.data <- PF.data %>% filter(Instrument %in% names.PF9.list[[i]])
  n.inst <- nrow(PF9.data %>% dplyr::count(Instrument))
  PF9.perf <- PF9.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.RF)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF10.data <- PF.data %>% filter(Instrument %in% names.PF10.list[[i]])
  n.inst <- nrow(PF10.data %>% dplyr::count(Instrument))
  PF10.perf <- PF10.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.RF)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  
  PF1.list[[i]]  <- PF1.perf
  PF2.list[[i]]  <- PF2.perf
  PF3.list[[i]]  <- PF3.perf
  PF4.list[[i]]  <- PF4.perf
  PF5.list[[i]]  <- PF5.perf
  PF6.list[[i]]  <- PF6.perf
  PF7.list[[i]]  <- PF7.perf
  PF8.list[[i]]  <- PF8.perf
  PF9.list[[i]]  <- PF9.perf
  PF10.list[[i]] <- PF10.perf
  
  i <- i+1
  startp <- as.Date(startp) %m+% months(rebal)
  endp <- as.Date(startp)  %m+% months(rebal)-1
}



###############################################################################################
####################
# 3. Replication Part
####################

### Table 3
## Get Portfolio Returns
PF1 <- as.vector(unlist(PF1.list))
PF2 <- as.vector(unlist(PF2.list))
PF3 <- as.vector(unlist(PF3.list))
PF4 <- as.vector(unlist(PF4.list))
PF5 <- as.vector(unlist(PF5.list))
PF6 <- as.vector(unlist(PF6.list))
PF7 <- as.vector(unlist(PF7.list))
PF8 <- as.vector(unlist(PF8.list))
PF9 <- as.vector(unlist(PF9.list))
PF10<- as.vector(unlist(PF10.list))

## Portfolio Returns Dataset
Output.raw <- Dates
Output.raw$PF1 <- PF1
Output.raw$PF2 <- PF2
Output.raw$PF3 <- PF3
Output.raw$PF4 <- PF4
Output.raw$PF5 <- PF5
Output.raw$PF6 <- PF6
Output.raw$PF7 <- PF7
Output.raw$PF8 <- PF8
Output.raw$PF9 <- PF9
Output.raw$PF10 <- PF10


## 2.3.2 Calculate mean (excess returns) and t-stats
BAB.tmp <- t.test(Output.raw$BAB*100)
BAB.ret <- as.numeric(BAB.tmp$estimate)
BAB.t   <- as.numeric(BAB.tmp$statistic)

PF1.tmp <- t.test(Output.raw$PF1*100)
PF1.ret <- as.numeric(PF1.tmp$estimate)
PF1.t   <- as.numeric(PF1.tmp$statistic)

PF2.tmp <- t.test(Output.raw$PF2*100)
PF2.ret <- as.numeric(PF2.tmp$estimate)
PF2.t   <- as.numeric(PF2.tmp$statistic)

PF3.tmp <- t.test(Output.raw$PF3*100)
PF3.ret <- as.numeric(PF3.tmp$estimate)
PF3.t   <- as.numeric(PF3.tmp$statistic)

PF4.tmp <- t.test(Output.raw$PF4*100)
PF4.ret <- as.numeric(PF4.tmp$estimate)
PF4.t   <- as.numeric(PF4.tmp$statistic)

PF5.tmp <- t.test(Output.raw$PF5*100)
PF5.ret <- as.numeric(PF5.tmp$estimate)
PF5.t   <- as.numeric(PF5.tmp$statistic)

PF6.tmp <- t.test(Output.raw$PF6*100)
PF6.ret <- as.numeric(PF6.tmp$estimate)
PF6.t   <- as.numeric(PF6.tmp$statistic)

PF7.tmp <- t.test(Output.raw$PF7*100)
PF7.ret <- as.numeric(PF7.tmp$estimate)
PF7.t   <- as.numeric(PF7.tmp$statistic)

PF8.tmp <- t.test(Output.raw$PF8*100)
PF8.ret <- as.numeric(PF8.tmp$estimate)
PF8.t   <- as.numeric(PF8.tmp$statistic)

PF9.tmp <- t.test(Output.raw$PF9*100)
PF9.ret <- as.numeric(PF9.tmp$estimate)
PF9.t   <- as.numeric(PF9.tmp$statistic)

PF10.tmp <- t.test(Output.raw$PF10*100)
PF10.ret <- as.numeric(PF10.tmp$estimate)
PF10.t   <- as.numeric(PF10.tmp$statistic)

## Mean (excess returns) and t-stats vector
exc.ret <- c(PF1.ret,PF2.ret,PF3.ret,PF4.ret,PF5.ret,PF6.ret,PF7.ret,PF8.ret,PF9.ret,PF10.ret,BAB.ret)
exc.t   <- c(PF1.t,PF2.t,PF3.t,PF4.t,PF5.t,PF6.t,PF7.t,PF8.t,PF9.t,PF10.t,BAB.t)

## Save both to new vectors so that we can later compare the results from the different beta estimations (be aware of correct assigning!)
# exc.ret.option1 <- exc.ret
# exc.t.option1 <- exc.t
# exc.ret.option2 <- exc.ret
# exc.t.option2 <- exc.t
# exc.ret.option3 <- exc.ret
# exc.t.option3 <- exc.t
# exc.ret.option4 <- exc.ret
# exc.t.option4 <- exc.t



## 2.3.3 Ex-ante Betas
beta.ex1 <- mean(as.vector(unlist(beta.ex1.list)))
beta.ex2 <- mean(as.vector(unlist(beta.ex2.list)))
beta.ex3 <- mean(as.vector(unlist(beta.ex3.list)))
beta.ex4 <- mean(as.vector(unlist(beta.ex4.list)))
beta.ex5 <- mean(as.vector(unlist(beta.ex5.list)))
beta.ex6 <- mean(as.vector(unlist(beta.ex6.list)))
beta.ex7 <- mean(as.vector(unlist(beta.ex7.list)))
beta.ex8 <- mean(as.vector(unlist(beta.ex8.list)))
beta.ex9 <- mean(as.vector(unlist(beta.ex9.list)))
beta.ex10 <- mean(as.vector(unlist(beta.ex10.list)))

## Ex-ante Beta vector
beta.ex <- c(beta.ex1,beta.ex2,beta.ex3,beta.ex4,beta.ex5,beta.ex6,beta.ex7,beta.ex8,beta.ex9,beta.ex10,0)

## Also here, save to other vector for option comparison
# beta.ex.option1 <- beta.ex
# beta.ex.option2 <- beta.ex
# beta.ex.option3 <- beta.ex
# beta.ex.option4 <- beta.ex


## Calculate volatility and standard deviation
Output.raw2 <- Output.raw %>% pivot_longer(-Date, names_to = "Portfolio", values_to = "Return")

# add factors
FF_mon_new <- FF_monthly %>% filter (Date >= "1995-02-01") #load FF_monthly in line 82 ff.
Date_new <-  Output.raw2 %>% filter(Portfolio != "BAB.ann") %>% dplyr::select(Date) %>% unique() %>% as.vector()
FF_mon_new <- cbind(FF_mon_new, Date_new)
FF_mon_new <- FF_mon_new[,-1] %>% dplyr::select(Date,Mkt.RF, SMB, HML, RMW, CMA, RF, Mom)
Output.raw3 <- Output.raw2 %>%
  filter(Portfolio != "BAB.ann") %>%
  left_join(FF_mon_new, by="Date")                                                                        #important: add monthly factors!

# option safe
# Output.raw3.option1 <- Output.raw3
# Output.raw3.option2 <- Output.raw3
# Output.raw3.option3 <- Output.raw3
# Output.raw3.option4 <- Output.raw3


vola.tmp <- Output.raw3 %>% group_by(Portfolio) %>% dplyr::summarise(vola = sd(Return))
vola.tmp2 <- vola.tmp[2,]    # rearrange a bit, felt dirty
vola.tmp3 <- vola.tmp[4:11,]
vola.tmp4 <- vola.tmp[3,]
vola.tmp5 <- vola.tmp[1,]
vola <- rbind(vola.tmp2,vola.tmp3,vola.tmp4,vola.tmp5) # now clean
vola$ex.ret <- exc.ret
vola.2 <- vola %>% mutate(vola.y = 100*vola*(12^0.5),
                          ex.ret.y= ex.ret*12,
                          SR = ex.ret.y/vola.y) # calculate sharpe ratios



## Calculate alphas
# CAPM alpha
reg.CAPM <- Output.raw3 %>% group_by(Portfolio) %>%
  nest(-Portfolio) %>%
  mutate(fit = map(data, ~lm(Return~Mkt.RF, data = .x)), # regress market return on portfolio and BAB returns
         tidied = map(fit, tidy)) %>%
  unnest(tidied)

CAPM.data.tmp <- reg.CAPM %>% filter(term =="(Intercept)") %>% select(Portfolio, estimate, statistic) # intercept to get alpha
CAPM.data.tmp2 <- CAPM.data.tmp[2:11,] # rearrange
CAPM.data.tmp3 <- CAPM.data.tmp[1,]
CAPM.data <- rbind(CAPM.data.tmp2,CAPM.data.tmp3) # rearrange

CAPM.beta.tmp <- reg.CAPM %>% filter(term =="Mkt.RF") %>% select(Portfolio, estimate, statistic) # Mkt.RF to retrieve beta coefficient
CAPM.beta.tmp2 <- CAPM.beta.tmp[2:11,] # rearrange
CAPM.beta.tmp3 <- CAPM.beta.tmp[1,]
CAPM.beta <- rbind(CAPM.beta.tmp2,CAPM.beta.tmp3) # rearrange


# Three-Factor alpha
reg.FF3 <- Output.raw3 %>%
  nest(-Portfolio) %>%
  mutate(fit = map(data, ~lm(Return~Mkt.RF+SMB+HML, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied)

FF3.data.tmp <- reg.FF3 %>% filter(term =="(Intercept)") %>% select(Portfolio, estimate, statistic)
FF3.data.tmp2 <- FF3.data.tmp[2:11,]
FF3.data.tmp3 <- FF3.data.tmp[1,]
FF3.data <- rbind(FF3.data.tmp2,FF3.data.tmp3)


# Four-Factor alpha
reg.CF4 <- Output.raw3 %>%
  nest(-Portfolio) %>%
  mutate(fit = map(data, ~lm(Return~Mkt.RF+SMB+HML+Mom, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied)

CF4.data.tmp <- reg.CF4 %>% filter(term =="(Intercept)") %>% select(Portfolio, estimate, statistic)
CF4.data.tmp2 <- CF4.data.tmp[2:11,]
CF4.data.tmp3 <- CF4.data.tmp[1,]
CF4.data <- rbind(CF4.data.tmp2,CF4.data.tmp3)


# Five-Factor alpha
reg.FF5 <- Output.raw3 %>%
  nest(-Portfolio) %>%
  mutate(fit = map(data, ~lm(Return~Mkt.RF+SMB+HML+RMW+CMA, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied)

FF5.data.tmp <- reg.FF5 %>% filter(term =="(Intercept)") %>% select(Portfolio, estimate, statistic)
FF5.data.tmp2 <- FF5.data.tmp[2:11,]
FF5.data.tmp3 <- FF5.data.tmp[1,]
FF5.data <- rbind(FF5.data.tmp2,FF5.data.tmp3)




## Output Table (Table 3)
library(xtable)
table_3 <- matrix(NA, 14, 11)
colnames(table_3) <- c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","BAB")
rownames(table_3) <- c("Excess return","t-stat","CAPM alpha", "t-stat", "Three-factor alpha", "t-stat", "Four-factor alpha", "t-stat", "Five-factor alpha", "t-stat", "Beta (ex ante)", "Beta (realized)", "Volatility", "Sharpe ratio")

table_3[1,]  <- exc.ret
table_3[2,]  <- exc.t
table_3[3,]  <- round(CAPM.data$estimate*100,2)
table_3[4,]  <- round(CAPM.data$statistic,2)
table_3[5,]  <- round(FF3.data$estimate*100,2)
table_3[6,]  <- round(FF3.data$statistic,2)
table_3[7,]  <- round(CF4.data$estimate*100,2)
table_3[8,]  <- round(CF4.data$statistic,2)
table_3[9,]  <- round(FF5.data$estimate*100,2)
table_3[10,] <- round(FF5.data$statistic,2)
table_3[11,] <- round(beta.ex,2)
table_3[12,] <- round(CAPM.beta$estimate,2)
table_3[13,] <- round(vola.2$vola.y,2)
table_3[14,] <- round(vola.2$SR,2)

# Plot Table
table_3 %>% xtable() %>% 
  kableExtra::kable(digits=3, booktabs = TRUE,
                    caption = "SP500: returns 1995-2021") %>%
  kableExtra::kable_styling(position = "center",bootstrap_options = c("striped", "hover", "condensed"),
                            latex_options = c("striped","HOLD_position","HOLD_position"), font_size = 14,
                            full_width = TRUE)

# safe option
# table3.option1 <- table_3
# table3.option2 <- table_3
# table3.option3 <- table_3
# table3.option4 <- table_3

# Export Table
export(table_3, "Table3.4.xlsx")




### TED Spread replication part
### 3m TBill and 3m Eurodollar LIBOR (both from FRED) monthly from 1990-01-01 till 2021-11-01
import2 <- read_xlsx("Daten/TB and LIBOR.xlsx", sheet="monthly")
TED.data.raw <- import2 %>% as_tibble() %>% mutate(Date = as.Date(Date)) %>% mutate((across(TB3M:EDL3M,~./100)))

TED.data.raw2 <- TED.data.raw %>%
  mutate(TED=EDL3M-TB3M,
         TED.lag = lag(TED)) %>%
  filter(Date >= "1995-02-01", Date <= "2021-10-01")

TED.data.raw2$BAB <- Output.raw$BAB

TED.data <- TED.data.raw2 %>% mutate(BAB.lag = lag(BAB), delta.TED = TED-lag(TED))

lagged.TED <- lm(BAB ~ TED.lag, data=TED.data) 
summary(lagged.TED)
change.TED <- lm(BAB ~ delta.TED, data=TED.data %>% na.omit()) 
summary(change.TED)


### optionl: plot of TED spread
TED.data %>%
  gather(key,value, EDL3M, TB3M, TED) %>%
  ggplot(aes(x=Date, y=value, colour=key)) +
  geom_line()+
  labs(title="TED-Spread", subtitle = "1995-01-01 to 2021-11-01", x="Date", y="TED Spread (LIBOR-TB3m)")+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")#+
  theme_bw()+
  My_Theme_BAB


###############################################################################################
####################
# 4. Graphics
####################

### BAB Factor
## BAB Return Plot

Output.raw3 %>% select(Date, Portfolio, Return) %>% filter(Portfolio == c("BAB","PF1","PF10")) %>% # compare portfolios PF1, PF10, BAB
    ggplot(aes(x = Date, y = Return))+ # hint: plot the series individually => comment/uncomment the lines
    geom_line(y= BAB, color = "red" , size = 0.7)+
    geom_line(y= PF1, color = "red" , size = 0.7)+
    geom_line(y= PF10, color = "blue" , size = 0.7)+
    scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
    theme_bw()+
    geom_rect(xmin = as.numeric(ymd("2000-03-01")), 
              xmax = as.numeric(ymd("2002-10-01")), 
              ymin = -0.5, ymax = 0.5, 
              fill = 'grey', alpha = 0.01)+
    geom_rect(xmin = as.numeric(ymd("2007-12-01")), 
              xmax = as.numeric(ymd("2009-06-01")), 
              ymin = -0.5, ymax = 0.5, 
              fill = 'grey', alpha = 0.01)+
    geom_rect(xmin = as.numeric(ymd("2020-02-01")), 
              xmax = as.numeric(ymd("2021-11-01")), 
              ymin = -0.5, ymax = 0.5, 
              fill = 'grey', alpha = 0.01)+
    labs(title="PF1 & 10 Return", subtitle = "1995-01-01 to 2021-11-01", y="Return")


## BAB Return Density Plot
bab_help <- Output.raw3 %>% filter(Portfolio == "BAB") %>% .$Return
ggplot(data = Output.raw3 %>% filter(Portfolio == "BAB"), aes(x = BAB))+
  geom_density(color = "black", size = 1, fill="grey")+
  theme_bw()+
  labs(title="BAB Factor Density", subtitle = "1995-01-01 to 2021-11-01", x="Return", y="Density")+
  stat_function(fun = dnorm,                                                                                    # add a normal distribution for comparison
                args = list(mean = mean(bab_help),
                            sd = sd(bab_help)),
                col = "red",
                size = 1) +
  geom_vline(xintercept=mean(bab_help), color="blue")


### Comparing Portfolios
## Cummulative BAB and PF1 to PF10 returns
cum.BAB  <- Output.raw3 %>% filter(Portfolio == "BAB") %>% select(Date, Portfolio, Return) %>% mutate(BAB = cumprod(1 + Return))
cum.PF1  <- Output.raw3 %>% filter(Portfolio == "PF1") %>% select(Date, Portfolio, Return) %>% mutate(PF1 = cumprod(1 + Return))
cum.PF2  <- Output.raw3 %>% filter(Portfolio == "PF2") %>% select(Date, Portfolio, Return) %>% mutate(PF2 = cumprod(1 + Return))
cum.PF3  <- Output.raw3 %>% filter(Portfolio == "PF3") %>% select(Date, Portfolio, Return) %>% mutate(PF3 = cumprod(1 + Return))
cum.PF4  <- Output.raw3 %>% filter(Portfolio == "PF4") %>% select(Date, Portfolio, Return) %>% mutate(PF4 = cumprod(1 + Return))
cum.PF5  <- Output.raw3 %>% filter(Portfolio == "PF5") %>% select(Date, Portfolio, Return) %>% mutate(PF5 = cumprod(1 + Return))
cum.PF6  <- Output.raw3 %>% filter(Portfolio == "PF6") %>% select(Date, Portfolio, Return) %>% mutate(PF6 = cumprod(1 + Return))
cum.PF7  <- Output.raw3 %>% filter(Portfolio == "PF7") %>% select(Date, Portfolio, Return) %>% mutate(PF7 = cumprod(1 + Return))
cum.PF8  <- Output.raw3 %>% filter(Portfolio == "PF8") %>% select(Date, Portfolio, Return) %>% mutate(PF8 = cumprod(1 + Return))
cum.PF9  <- Output.raw3 %>% filter(Portfolio == "PF9") %>% select(Date, Portfolio, Return) %>% mutate(PF9 = cumprod(1 + Return))
cum.PF10 <- Output.raw3 %>% filter(Portfolio == "PF10") %>% select(Date, Portfolio, Return) %>% mutate(PF10 = cumprod(1 + Return))

## Cummulative Fama French returns
cum.Mkt.RF <- FF_mon_new %>% filter(Date >="1995-01-01", Date <= "2021-11-01") %>% select(Date, Mkt.RF) %>% mutate(Mkt.RF = cumprod(1 + Mkt.RF/100))
cum.SMB    <- FF_mon_new %>% filter(Date >="1995-01-01", Date <= "2021-11-01") %>% select(Date, SMB) %>% mutate(SMB = cumprod(1 + SMB/100))
cum.HML    <- FF_mon_new %>% filter(Date >="1995-01-01", Date <= "2021-11-01") %>% select(Date, HML) %>% mutate(HML = cumprod(1 + HML/100))
cum.RMW    <- FF_mon_new %>% filter(Date >="1995-01-01", Date <= "2021-11-01") %>% select(Date, RMW) %>% mutate(RMW = cumprod(1 + RMW/100))
cum.CMA    <- FF_mon_new %>% filter(Date >="1995-01-01", Date <= "2021-11-01") %>% select(Date, CMA) %>% mutate(CMA = cumprod(1 + CMA/100))
cum.Mom    <- FF_mon_new %>% filter(Date >="1995-01-01", Date <= "2021-11-01") %>% select(Date, Mom) %>% mutate(Mom = cumprod(1 + Mom/100))

## Dataset for ggplot
cum.factors     <- cum.BAB
cum.factors$PF1 <- cum.PF1$PF1
cum.factors$PF2 <- cum.PF2$PF2
cum.factors$PF3 <- cum.PF3$PF3
cum.factors$PF4 <- cum.PF4$PF4
cum.factors$PF5 <- cum.PF5$PF5
cum.factors$PF6 <- cum.PF6$PF6
cum.factors$PF7 <- cum.PF7$PF7
cum.factors$PF8 <- cum.PF8$PF8
cum.factors$PF9 <- cum.PF9$PF9
cum.factors$PF10 <- cum.PF10$PF10
cum.factors$Mkt.RF <- cum.Mkt.RF$Mkt.RF
cum.factors$SMB <- cum.SMB$SMB
cum.factors$HML <- cum.HML$HML
cum.factors$RMW <- cum.RMW$RMW
cum.factors$CMA <- cum.CMA$CMA
cum.factors$Mom <- cum.Mom$Mom


## Plot cummulative Returns
cum.factors %>%
  gather(key,value, BAB, PF1, PF10) %>%                                                 # portfolios two to nine are exluded
         group_by(Portfolio) %>%
  ggplot(aes(x=Date, y=value, colour=key)) +
  geom_line()+
  labs(title="Cumulative Portfolio Returns", subtitle = "1995-01-01 to 2021-11-01", x="Date", y="Cumulative Returns")+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  scale_y_continuous(breaks = c(-1:13), limits = c(-1,13))+
  theme_bw()



## Plot Alphas
data.fig1.tmp <- as_tibble(table_3)
data.fig1.tmp$Name <- row.names(table_3)

data.fig1 <- data.fig1.tmp[,c(12,1:11)] 
colnames(data.fig1) <- c("Name", "PF1", "PF2", "PF3", "PF4", "PF5", "PF6", "PF7", "PF8", "PF9", "PF10", "BAB")


# CAPM
data.fig1 %>% filter(Name == "CAPM alpha") %>% select(-BAB) %>% pivot_longer(-Name, values_to = "alpha", names_to = "Portfolio")%>%
  arrange(alpha) %>%
  mutate(Name = factor(Portfolio, levels=c("PF1", "PF2", "PF3", "PF4", "PF5", "PF6", "PF7", "PF8", "PF9", "PF10"))) %>% #rearrange to P1 -> P10 -> BAB
  ggplot(aes(x= Name, y=alpha, fill="lightblue")) + 
  geom_col(width=0.7,color='black',fill='steelblue')+
  labs(title="Alphas of beta sorted portfolios", subtitle = "CAPM", x="Portfolio", y="Alpha")+
  theme_bw()+
  theme(legend.position="none")


# FF3
data.fig1 %>% filter(Name == "Three-factor alpha") %>% select(-BAB) %>%
  pivot_longer(-Name, values_to = "alpha", names_to = "Portfolio") %>%
  ggplot(aes(x=reorder(Portfolio, -alpha), y=alpha, fill="lightblue")) +
  geom_col(width=0.7,color='darkblue',fill='steelblue')+
  labs(title="Alphas of beta sorted Portfolios", subtitle = "Fama-French 3 Factor Model", x="Portfolio", y="Alpha")+
  theme_bw()+
  theme(legend.position="none")


# CF4
data.fig1 %>% filter(Name == "Four-factor alpha") %>% select(-BAB) %>%
  pivot_longer(-Name, values_to = "alpha", names_to = "Portfolio") %>%
  ggplot(aes(x=reorder(Portfolio, -alpha), y=alpha, fill="lightblue")) +
  geom_col(width=0.7,color='darkblue',fill='steelblue')+
  labs(title="Alphas of beta sorted Portfolios", subtitle = "Carhart-Fama-French 4 Factor Model", x="Portfolio", y="Alpha")+
  theme_bw()+
  theme(legend.position="none")


# FF5
data.fig1 %>% filter(Name == "Five-factor alpha") %>% select(-BAB) %>%
  pivot_longer(-Name, values_to = "alpha", names_to = "Portfolio") %>%
  ggplot(aes(x=reorder(Portfolio, -alpha), y=alpha, fill="lightblue")) +
  geom_col(width=0.7,color='darkblue',fill='steelblue')+
  labs(title="Alphas of beta sorted Portfolios", subtitle = "Fama-French 5 Factor Model", x="Portfolio", y="Alpha")+
  theme_bw()+
  theme(legend.position="none")


# Sharpe Ratio
data.fig1 %>% filter(Name == "Sharpe ratio") %>% select(-BAB) %>%
  pivot_longer(-Name, values_to = "SR", names_to = "Portfolio") %>%
  ggplot(aes(x=reorder(Portfolio, -SR), y=SR, fill="lightblue")) + # reorder according to SR value
  geom_col(width=0.7,color='black',fill='steelblue')+
  labs(title="Sharpe Ratios of decile portfolios", subtitle = "1995-01-01 to 2021-11-01", x="Portfolio", y="Sharpe Ratio")+
  theme_bw()+
  theme(legend.position="none")
















