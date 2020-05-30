########## Multinomial Logit Model ##########
#### Individual Unit ##########
##### Access mode choices ##### Sample size: 1042 Observations #####
# Code: 1 (Motorbike); 2 (Private car); 3 (Taxi); 4 (Airport bus); 5 (VCAT)#

#<<<First step>>>
##### Data Reading from csv file #####
  
     library(readr)
     data <- read_csv("20200423_Data5.csv")
     names(data)
     summary(data)
   # Dummies (or indicator) for Nationality

   data$Nat1   <- ifelse(data$Nationality==1,1,0) #Vientiane resident
   data$Nat2   <- ifelse(data$Nationality==2,1,0) #Non vientiane resident
   data$Nat3   <- ifelse(data$Nationality==3,1,0) #Foreign resident
   data$Lao    <- ifelse(data$Nationality<=2,1,0)  #Lao resident

  # Dummies (or indicator) for Gender
    data$Male    <- ifelse(data$Gender==1,1,0) #Male
    data$Female  <- ifelse(data$Gender==0,1,0) #Female


  # Dummies (or indicator) for data$Ag
  data$Age1  <- ifelse(data$Age==1,1,0) # <20
  data$Age2  <- ifelse(data$Age==2,1,0) # =21-30
  data$Age3  <- ifelse(data$Age==3,1,0) # =31-40
  data$Age4  <- ifelse(data$Age==4,1,0) # =41-50
  data$Age5  <- ifelse(data$Age==5,1,0) # >51


  # Dummies (or indicator) for Education
   data$Edu1    <- ifelse(data$Edu==1,1,0) # Highshool
   data$Edu2    <- ifelse(data$Edu==2,1,0) # College
   data$Edu3    <- ifelse(data$Edu==3,1,0) # Bachelor
   data$Edu4    <- ifelse(data$Edu==4,1,0) # Master

  # Dummies (or indicator) for Occupation
  data$Job1  <- ifelse(data$Job==1,1,0)  # Goverment offical
  data$Job2  <- ifelse(data$Job==2,1,0)  # Businessperson
  data$Job3  <- ifelse(data$Job==3,1,0)  # Students
  data$Job4  <- ifelse(data$Job==4,1,0)  # Others

  # Dummies (or indicator) for Monthly household income
  data$Hinc1 <- ifelse(data$Hinc==1,1,0) # Low income <4,000,000 kip
  data$Hinc2 <- ifelse(data$Hinc==2,1,0) # 4,000,000 kip< Midle income <10,000,000 kip
  data$Hinc3 <- ifelse(data$Hinc==3,1,0) # High income >10,000,000 kip

  # Dummies (or indicator) for Dependency of others

   data$Dependence1 <- ifelse(data$Dependency==1,1,0) #Car-dropped
   data$Dependence2 <- ifelse(data$Dependency==2,1,0) #Car-parked
   data$Dependence3 <- ifelse(data$Dependency==3,1,0) #Shared ride

 # Dummies (or indicator) for Household members
   data$Hmem1 <- ifelse(data$Hcar==1,1,0) # 1 member
   data$Hmem2 <- ifelse(data$Hcar==2,1,0) # 2 members
   data$Hmem3 <- ifelse(data$Hcar==3,1,0) # 3 members
   data$Hmem4 <- ifelse(data$Hcar==4,1,0) # 4 members
   data$Hmem5 <- ifelse(data$Hcar==5,1,0) # 5 members

 # Dummies (or indicator) for Number of cars in a Household
   data$Hcar0 <- ifelse(data$Hcar==0,1,0) # no car
   data$Hcar1 <- ifelse(data$Hcar==1,1,0) # 1 car
   data$Hcar2 <- ifelse(data$Hcar==2,1,0) # 2 cars
   data$Hcar3 <- ifelse(data$Hcar==3,1,0) # 3 cars
   data$Hcar4 <- ifelse(data$Hcar==4,1,0) # 4 cars


  # Dummies (or indicator) for Number of Check-in Luggage
    data$lug0 <- ifelse(data$ChLug==0,1,0) # no lug
    data$lug1 <- ifelse(data$ChLug==1,1,0) # 1 lug
    data$lug2 <- ifelse(data$ChLug==2,1,0) # 2 lug
    data$lug3 <- ifelse(data$ChLug==3,1,0) # 3 lug

 # Dummies (or indicator) for Trip purpose

    data$tpp1 <- ifelse(data$Tpp==1,1,0) # business trip
    data$tpp2 <- ifelse(data$Tpp==2,1,0) # holiday trip
    data$tpp3 <- ifelse(data$Tpp==3,1,0) # education trip
    data$tpp4 <- ifelse(data$Tpp==4,1,0) # visiting/home trip
#<<<Second step>>>
##### Defining Log-likehood function #####
LL <- function (C.M,C.T,C.B,C.V,Lao.M,Lao.T,Lao.B,Lao.V,Male.M,Male.T,Male.B,Male.V,
                Age21_30.M,Age21_30.T,Age21_30.B,Age21_30.V,Age31_40.M,Age31_40.T,Age31_40.B,Age31_40.V,Age41_50.M,Age41_50.T,Age41_50.B,Age41_50.V,
                Govt.M,Govt.T,Govt.B,Govt.V,Busines.M,Busines.T,Busines.B,Busines.V,Private.M,Private.T,Private.B,Private.V,Stud.M,Stud.T,Stud.B,Stud.V,
                LowInc.M,LowInc.T,LowInc.B,LowInc.V,MidInc.M,MidInc.T,MidInc.B,MidInc.V,Lug.M,Lug.T,Lug.B,Lug.V,Accom.M,Accom.T,Accom.B,Accom.V,BusiTrip.M,BusiTrip.T,BusiTrip.B,BusiTrip.V,Depen.M,Depen.T,Depen.B,Depen.V,Rain.M,Rain.T,Rain.B,Rain.V
                
        )
               
  
{ #Begin function

  # Utility function
  
    V2   <- 0
 V1   <- C.M   + data$Lao*Lao.M + data$Male*Male.M + data$Job1*Govt.M + data$Job2*Busines.M  + data$Job3*Stud.M + data$Age2*Age21_30.M + data$Age3*Age31_40.M + data$Age4*Age41_50.M  + data$Hinc2*MidInc.M + data$Hinc1*LowInc.M + data$ChLug*Lug.M + data$Accom*Accom.M + data$Tpp*BusiTrip.M  + data$Dependency*Depen.M + data$WeaCon*Rain.M
 V3   <- C.T   + data$Lao*Lao.T + data$Male*Male.T + data$Job1*Govt.T + data$Job2*Busines.T  + data$Job3*Stud.T + data$Age2*Age21_30.T + data$Age3*Age31_40.T + data$Age4*Age41_50.T   + data$Hinc2*MidInc.T + data$Hinc1*LowInc.T + data$ChLug*Lug.T + data$Accom*Accom.T + data$Tpp*BusiTrip.T  + data$Dependency*Depen.T + data$WeaCon*Rain.T
  V4   <- C.B   + data$Lao*Lao.B + data$Male*Male.B + data$Job1*Govt.B + data$Job2*Busines.B   + data$Job3*Stud.B + data$Age2*Age21_30.B + data$Age3*Age31_40.B + data$Age4*Age41_50.B   + data$Hinc2*MidInc.B + data$Hinc1*LowInc.B  + data$ChLug*Lug.B  + data$Accom*Accom.B + data$Tpp*BusiTrip.B   + data$Dependency*Depen.B + data$WeaCon*Rain.B
  V5   <- C.V   + data$Lao*Lao.V + data$Male*Male.V + data$Job1*Govt.V + data$Job2*Busines.V   + data$Job3*Stud.V + data$Age2*Age21_30.V + data$Age3*Age31_40.V + data$Age4*Age41_50.V   + data$Hinc2*MidInc.V + data$Hinc1*LowInc.V  + data$ChLug*Lug.V  + data$Accom*Accom.V + data$Tpp*BusiTrip.V   + data$Dependency*Depen.V + data$WeaCon*Rain.V


  # Denominator
    Sum <- exp(V1) + exp(V2) + exp(V3) + exp(V4) + exp(V5)

  # Probabilities
  P1 <- exp(V1)/Sum
  P2 <- exp(V2)/Sum
  P3 <- exp(V3)/Sum
  P4 <- exp(V4)/Sum
  P5 <- exp(V5)/Sum
  
  

  # Dummies (or indicator) for alternatives
  D1 <- ifelse(data$Choice == 1,1,0)
  D2 <- ifelse(data$Choice == 2,1,0)
  D3 <- ifelse(data$Choice == 3,1,0)
  D4 <- ifelse(data$Choice == 4,1,0)
  D5 <- ifelse(data$Choice == 5,1,0)
  
  
  
  
  
  # Log-likelihood function
  L <- -(sum(D1*log(P1) + D2*log(P2) + D3*log(P3) + D4*log(P4) + D5*log(P5)))
  return(L)
} #End function

#<<<Third step>>>
##### Model Estimation #####
  initValue <- list(C.M=0, C.T=0,C.B=0,C.V=0, Lao.M=0,Lao.T=0,Lao.B=0,Lao.V=0,Male.M=0,Male.T=0,Male.B=0,Male.V=0,Govt.M=0,Govt.T=0,Govt.B=0,Govt.V=0,Busines.M=0,Busines.T=0,Busines.B=0,Busines.V=0,Stud.M=0,Stud.T=0,Stud.B=0,Stud.V=0,                    Age21_30.M=0,Age21_30.T=0,Age21_30.B=0,Age21_30.V=0,Age31_40.M=0,Age31_40.T=0,Age31_40.B=0,Age31_40.V=0,Age41_50.M=0,Age41_50.T=0,Age41_50.B=0,Age41_50.V=0,MidInc.M=0,MidInc.T=0,MidInc.B=0,MidInc.V=0,LowInc.M=0,LowInc.T=0,LowInc.B=0,LowInc.V=0,
                    Lug.M=0,Lug.T=0,Lug.B=0,Lug.V=0,Accom.M=0,Accom.T=0,Accom.B=0,Accom.V=0,BusiTrip.M=0,BusiTrip.T=0,BusiTrip.B=0,BusiTrip.V=0,Depen.M=0,Depen.T=0,Depen.B=0,Depen.V=0,Rain.M=0,Rain.T=0,Rain.B=0,Rain.V=0
                    
                   )
  
  
  #install packages: NoOTICE: Packages is needed to install for the first time using the packages
  # install.packages("bbmle")
  library(bbmle)
  MNL <- mle2(LL, start = initValue, method = "BFGS", control = list(trace = 10000, maxit = 10000))
  out <- summary(MNL)
  out
library(dplyr)
library(tidyverse)

out_comp <- as.data.frame(out@coef) %>% round(.,2) %>%
        rownames_to_column(var = "ParName") %>%
        separate(col = "ParName", into = c("ParName", "Mode"), sep = "([\\.])") %>%
        mutate(Mode = case_when(TRUE ~ Mode,
                                Mode == "M" ~ "MotorBike",
                                Mode == "C" ~ "Car",
                                Mode == "T" ~ "Taxi",
                                Mode == "B" ~ "Bus",
                                Mode == "V" ~ "VCAT")
              ) %>%
        mutate(Sign = case_when(Estimate >  0 ~ "Positive",
                                Estimate <= 0 ~ "Negative"
                                )
               ) %>%
        mutate(ParName = case_when(ParName == "Nat" ~ "Foreigner",
                                   ParName == "Gen" ~ "Male",
                                   ParName == "Job1" ~ "Goverment offical",
                                   ParName == "Job2" ~ "Businessperson",
                                   ParName == "Job3" ~ "Students",
                                   ParName == "Job4" ~ "Professor",
                                   ParName == "Job5" ~ "Salesperson",
                                   ParName == "Job6" ~ "Housekeeper",
                                   ParName == "Job7" ~ "Private firm employee",
                                   ParName == "Job8" ~ "Self-employes person",
                                   ParName == "Job9" ~ "Retired",
                                   ParName == "Job10" ~ "Others",
                                   ParName == "Age1" ~ "Age ~20",
                                   ParName == "Age2" ~ "Age 21-30",
                                   ParName == "Age3" ~ "Age 31-40",
                                   ParName == "Age4" ~ "Age 41-50",
                                   ParName == "Age5" ~ "Age 51-60",
                                   ParName == "Age6" ~ "Age >60",
                                   ParName == "Hinc1" ~ "Income (low)",
                                   ParName == "Hinc2" ~ "Income (Medium)",
                                   ParName == "Hinc3" ~ "Income (High)",
                                   ParName == "Hcar" ~ "Number of Cars",
                                   TRUE ~ ParName
                                   )
               ) %>%
        mutate(Impact = case_when(`Pr(z)` <=0.1 ~ "Significant",
                                  TRUE ~ "Unsignificant"
                                    ))-> data_plot

 ggplot(data_plot,mapping = aes(x = Mode, y = Estimate, fill = Impact))+
         geom_col() +
         coord_flip() +
         facet_wrap(~ ParName)

 ggplot(data_plot,mapping = aes(x = ParName, y = `Pr(> t)`, fill = Impact))+
         geom_col() +
         coord_flip() +
         facet_wrap(~ Mode)


 ggplot(data_plot,mapping = aes(x = ParName, y = `t value`, fill = Impact))+
         geom_col() +
         coord_flip() +
         facet_wrap(~ Mode)


#<<<Fourth step>>>
  LL0 <- NROW(data)*log(1/5)     ## Initial log-likelihood
  LL1 <- logLik(MNL)             ## Converged log-likelihood
  ## McFadden's Rho square
  Rho     <- 1-(LL1/LL0)
  Rho.adj <- 1-((LL1-length(coef(MNL)))/LL0)
  GoF <- cbind(LL0,LL1, Rho, Rho.adj)  ## Goodness of fit
  print(GoF)

