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
  
  
#<<<Second step>>>
##### Defining Log-likehood function #####
LL <- function (C1,C2,C4,C5,
                VienM,
                VienT,
                VienB,
                VienV
                
        )
               
  
{ #Begin function
  # Dummies (or indicator) for Nationality
   
    Lao   <- ifelse(data$Nationality==1,1,0) #Lao resident
    Forg  <- ifelse(data$Nationality==0,1,0) #Foreign resident
    Vien   <- ifelse(data$Vientiane==1,1,0) #Lao resident
  # Dummies (or indicator) for Gender
    Male    <- ifelse(data$Gender==1,1,0) #Male
    Female  <- ifelse(data$Gender==0,1,0) #Female
    
  
    # Dummies (or indicator) for Age
    Age1  <- ifelse(data$Age==1,1,0) # <20      # Teenage
    Age2  <- ifelse(data$Age==2,1,0) # =20-40   # Younger adults
    Age3  <- ifelse(data$Age==3,1,0) # =40-60   # Older adults 
    Age4  <- ifelse(data$Age==4,1,0) # >60      # Elderly
  
  
  
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
  
  
  # Dummies (or indicator) for Monthly household income     
     
    Nat1   <- ifelse(data$Nationality==2,1,0) # Foreigner
    Nat2   <- ifelse(data$Nationality==1,1,0) # Vientiane resident
    Nat3   <- ifelse(data$Nationality==3,1,0) # Non-Vientiane resident
    
    
    
  # Utility function
  
  V2   <- 0                                                            
  V1   <- C1  + Vien*VienM   
  V3   <- C2  + Vien*VienT     
  V4   <- C4  + Vien*VienB     
  V5   <- C5  + Vien*VienV      
  
  
  
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
  initValue <- list(C1=0,C2=0,C4=0,C5=0,
                    VienM=0,
                    VienT=0,
                    VienB=0,
                    VienV=0
                   )
  
  
  #install packages: NoOTICE: Packages is needed to install for the first time using the packages
  # install.packages("bbmle")
  library(bbmle)
  MNL <- mle2(LL, start = initValue, method = "BFGS", control = list(trace = 10000, maxit = 10000))
  summary(MNL)

#<<<Fourth step>>>
  LL0 <- NROW(data)*log(1/5)     ## Initial log-likelihood
  LL1 <- logLik(MNL)             ## Converged log-likelihood
  ## McFadden's Rho square
  Rho     <- 1-(LL1/LL0)
  Rho.adj <- 1-((LL1-length(coef(MNL)))/LL0)
  GoF <- cbind(LL0,LL1, Rho, Rho.adj)  ## Goodness of fit
  print(GoF)

