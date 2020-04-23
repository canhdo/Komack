########## Multinomial Logit Model ##########
#### Individual Unit ##########
##### Access mode choices ##### Sample size: 1042 Observations #####
# Code: 1 (Motorbike); 2 (Private car); 3 (Taxi); 4 (Airport bus); 5 (VCAT)#

#<<<First step>>>
##### Data Reading from csv file #####
  
  
  library(readr)
  data <- read_csv("20200423_Data.csv")
  names(data)
  summary(data)
  
  
#<<<Second step>>>
##### Defining Log-likehood function #####
LL <- function (C1,C2,C3,C4,
                NatC,NatT,NatB,NatV,
                GenC,GenT,GenB,GenV,
                Age2C,Age2T,Age2B,Age2V,Age3C,Age3T,Age3B,Age3V,Age4C,Age4T,Age4B,Age4V,Age5C,Age5T,Age5B,Age5V,Age6C,Age6T,Age6B,Age6V,
                Job1C,Job1T,Job1B,Job1V,Job2C,Job2T,Job2B,Job2V,Job3C,Job3T,Job3B,Job3V,Job4C,Job4T,Job4B,Job4V,
                Hinc2C,Hinc2T,Hinc2B,Hinc2V,Hinc3C,Hinc3T,Hinc3B,Hinc3V,
                HcarC,HcarT,HcarB,HcarV,
                HinC,HinT,HinB,HinV
        )
               
  
{ #Begin function
  
  # Dummies (or indicator) for Age
   Age1  <- ifelse(data$Age==1,1,0) # <=20 
   Age2  <- ifelse(data$Age==2,1,0) # =21-30
   Age3  <- ifelse(data$Age==3,1,0) # =31-40
   Age4  <- ifelse(data$Age==4,1,0) # =41-50
   Age5  <- ifelse(data$Age==5,1,0) # =51-60
   Age6  <- ifelse(data$Age==6,1,0) # >60
  
  
  # Dummies (or indicator) for Occupation
    Job1  <- ifelse(data$Job==1,1,0)  # Goverment offical
    Job2  <- ifelse(data$Job==2,1,0)  # Businessperson
    Job3  <- ifelse(data$Job==3,1,0)  # Students
    Job4  <- ifelse(data$Job==4,1,0)  # Professor
    Job5  <- ifelse(data$Job==5,1,0)  # Salesperson
    Job6  <- ifelse(data$Job==6,1,0)  # Housekeeper
    Job7  <- ifelse(data$Job==7,1,0)  # Private firm employee
    Job8  <- ifelse(data$Job==8,1,0)  # Self-employes person
    Job9  <- ifelse(data$Job==9,1,0)  # Retired
    Job10 <- ifelse(data$Job==10,1,0) # Others
  
    
  # Dummies (or indicator) for Monthly household income
    Hinc1 <- ifelse(data$Hinc==1,1,0) # Low income <4,000,000 kip
    Hinc2 <- ifelse(data$Hinc==2,1,0) # 4,000,000 kip< Midle income <10,000,000 kip
    Hinc3 <- ifelse(data$Hinc==3,1,0) # High income >10,000,000 kip
 
    
  
    
    
    
    
  # Utility function
  
   V1   <- 0                                                            
   V2   <- C1 + data$Nationality*NatC + data$Gender*GenC  + Age2*Age2C + Age3*Age3C + Age4*Age4C + Age5*Age5C + Age6*Age6C + Job1*Job1C + Job2*Job2C  + Job3*Job3C   + Job7*Job4C    + Hinc3*Hinc3C + Hinc2*Hinc2C  + data$Hcar*HcarC              
   V3   <- C2 + data$Nationality*NatT + data$Gender*GenT  + Age2*Age2T + Age3*Age3T + Age4*Age4T + Age5*Age5T + Age6*Age6T + Job1*Job1T + Job2*Job2T  + Job3*Job3T   + Job7*Job4T    + Hinc3*Hinc3T + Hinc2*Hinc2T  + data$Hcar*HcarT      
   V4   <- C3 + data$Nationality*NatB + data$Gender*GenB  + Age2*Age2B + Age3*Age3B + Age4*Age4B + Age5*Age5B + Age6*Age6B + Job1*Job1B + Job2*Job2B  + Job3*Job3B   + Job7*Job4B    + Hinc3*Hinc3B + Hinc2*Hinc2B  + data$Hcar*HcarB        
   V5   <- C4 + data$Nationality*NatV + data$Gender*GenV  + Age2*Age2V + Age3*Age3V + Age4*Age4V + Age5*Age5V + Age6*Age6V + Job1*Job1V + Job2*Job2V  + Job3*Job3V   + Job7*Job4V    + Hinc3*Hinc3V + Hinc2*Hinc2V  + data$Hcar*HcarV         
  
  
  
  
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
  initValue <- list(C1=0,C2=0,C3=0,C4=0,NatC=0,NatT=0,NatB=0,NatV=0,GenC=0,GenT=0,GenB=0,GenV=0,Age2C=0,Age2T=0,Age2B=0,Age2V=0,Age3C=0,Age3T=0,Age3B=0,Age3V=0,Age4C=0,Age4T=0,Age4B=0,Age4V=0,Age5C=0,Age5T=0,Age5B=0,Age5V=0,Age6C=0,Age6T=0,Age6B=0,Age6V=0,
                    Job1C=0,Job1T=0,Job1B=0,Job1V=0,Job2C=0,Job2T=0,Job2B=0,Job2V=0,Job3C=0,Job3T=0,Job3B=0,Job3V=0,Job4C=0,Job4T=0,Job4B=0,Job4V=0,                  
                    Hinc3C=0,Hinc3T=0,Hinc3B=0,Hinc3V=0,Hinc2C=0,Hinc2T=0,Hinc2B=0,Hinc2V=0,HcarC=0,HcarT=0,HcarB=0,HcarV=0
                    )
  
  
  #install packages: NoOTICE: Packages is needed to install for the first time using the packages
  # install.packages("bbmle")
  library(bbmle)
  MNL <- mle2(LL, start = initValue, method = "BFGS", control = list(trace = 1, maxit = 10000))
  summary(MNL)

  
  
  
#<<<Fourth step>>>
  LL0 <- NROW(data)*log(1/5)     ## Initial log-likelihood
  LL1 <- logLik(MNL)             ## Converged log-likelihood
  ## McFadden's Rho square
  Rho     <- 1-(LL1/LL0)
  Rho.adj <- 1-((LL1-length(coef(MNL)))/LL0)
  GoF <- cbind(LL0,LL1, Rho, Rho.adj)  ## Goodness of fit
  print(GoF)

