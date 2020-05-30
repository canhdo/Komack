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
LL <- function (C1,C3,C4,C5,LaoM,LaoT,LaoB,LaoV,MaleM,MaleT,MaleB,MaleV,
                Age21_30M,Age21_30T,Age21_30B,Age21_30V,Age31_40M,Age31_40T,Age31_40B,Age31_40V,Age41_50M,Age41_50T,Age41_50B,Age41_50V,
                GovtM,GovtT,GovtB,GovtV,BusinesM,BusinesT,BusinesB,BusinesV,StudM,StudT,StudB,StudV,
                LowIncM,LowIncT,LowIncB,LowIncV,MidIncM,MidIncT,MidIncB,MidIncV,LugM,LugT,LugB,LugV,AccomM,AccomT,AccomB,AccomV,BusiTripM,BusiTripT,BusiTripB,BusiTripV,DepenM,DepenT,DepenB,DepenV,RainM,RainT,RainB,RainV,
                Cost,Vehtime,Delay,Wait,Access,Transfer,Parking 
        )
               
  
{ #Begin function
  # Dummies (or indicator) for Nationality
   
    Nat1   <- ifelse(data$Nationality==1,1,0) #Vientiane resident
    Nat2   <- ifelse(data$Nationality==2,1,0) #Non vientiane resident
    Nat3   <- ifelse(data$Nationality==3,1,0) #Foreign resident
    Lao    <- ifelse(data$Nationality<=2,1,0)  #Lao resident
  
  # Dummies (or indicator) for Gender
    Male    <- ifelse(data$Gender==1,1,0) #Male
    Female  <- ifelse(data$Gender==0,1,0) #Female
    
  
  # Dummies (or indicator) for Age
  Age1  <- ifelse(data$Age==1,1,0) # <20 
  Age2  <- ifelse(data$Age==2,1,0) # =21-30
  Age3  <- ifelse(data$Age==3,1,0) # =31-40
  Age4  <- ifelse(data$Age==4,1,0) # =41-50
  Age5  <- ifelse(data$Age==5,1,0) # >51
  
  
  # Dummies (or indicator) for Education
   Edu1    <- ifelse(data$Edu==1,1,0) # Highshool
   Edu2    <- ifelse(data$Edu==2,1,0) # College
   Edu3    <- ifelse(data$Edu==3,1,0) # Bachelor
   Edu4    <- ifelse(data$Edu==4,1,0) # Master
  
  # Dummies (or indicator) for Occupation
  Job1  <- ifelse(data$Job==1,1,0)  # Goverment employee
  Job2  <- ifelse(data$Job==2,1,0)  # Businessperson
  Job3  <- ifelse(data$Job==3,1,0)  # Private employess
  Job4  <- ifelse(data$Job==4,1,0)  # Student
  Job5  <- ifelse(data$Job==5,1,0)  # Others
  
  
  # Dummies (or indicator) for Monthly household income
  Hinc1 <- ifelse(data$Hinc==1,1,0) # Low income <4,000,000 kip
  Hinc2 <- ifelse(data$Hinc==2,1,0) # 4,000,000 kip< Midle income <10,000,000 kip
  Hinc3 <- ifelse(data$Hinc==3,1,0) # High income >10,000,000 kip
 
 

    
   
   
   
   
  # Utility function
  
    V2   <- 0    + data$TCost_Car*Cost   + data$Vehtime_Car*Vehtime    + data$Delay_Car*Delay   + data$Wtime_Car*Wait    + data$Accesstime_Car*Access   + data$NoTranfer_Car*Transfer  + data$ParkFee_Car*Parking                                      
    V1   <- C1   + data$TCost_MT*Cost    + data$Vehtime_MT*Vehtime     + data$Delay_MT*Delay    + data$Wtime_MT*Wait     + data$Accesstime_MT*Access    + data$NoTranfer_MT*Transfer   + data$ParkFee_MT*Parking    + Lao*LaoM + Male*MaleM + Job1*GovtM + Job2*BusinesM  + Job3*StudM + Age2*Age21_30M + Age3*Age31_40M + Age41_50M*Age4   + Hinc2*MidIncM + Hinc1*LowIncM  + data$ChLug*LugM  + data$Accom*AccomM + data$Tpp*BusiTripM  + data$Dependency*DepenM + data$WeaCon*RainM
    V3   <- C3   + data$TCost_Taxi*Cost  + data$Vehtime_Taxi*Vehtime   + data$Delay_Taxi*Delay  + data$Wtime_Taxi*Wait   + data$Accesstime_Taxi*Access  + data$NoTranfer_Taxi*Transfer + data$ParkFee_Taxi*Parking  + Lao*LaoT + Male*MaleT + Job1*GovtT + Job2*BusinesT  + Job3*StudT + Age2*Age21_30T + Age3*Age31_40T + Age41_50T*Age4   + Hinc2*MidIncT + Hinc1*LowIncT  + data$ChLug*LugT  + data$Accom*AccomT + data$Tpp*BusiTripT  + data$Dependency*DepenT + data$WeaCon*RainT                                                                    
    V4   <- C4   + data$TCost_Bus*Cost   + data$Vehtime_Bus*Vehtime    + data$Delay_Bus*Delay   + data$Wtime_Bus*Wait    + data$Accesstime_Bus*Access   + data$NoTranfer_Bus*Transfer  + data$ParkFee_Bus*Parking   + Lao*LaoB + Male*MaleB + Job1*GovtB + Job2*BusinesB  + Job3*StudB + Age2*Age21_30B + Age3*Age31_40B + Age41_50B*Age4   + Hinc2*MidIncB + Hinc1*LowIncB  + data$ChLug*LugB  + data$Accom*AccomB + data$Tpp*BusiTripB  + data$Dependency*DepenB + data$WeaCon*RainB                                                           
    V5   <- C5   + data$TCost_VCAT*Cost  + data$Vehtime_VCAT*Vehtime   + data$Delay_VCAT*Delay  + data$Wtime_VCAT*Wait   + data$Accesstime_VCAT*Access  + data$NoTranfer_VCAT*Transfer + data$ParkFee_VCAT*Parking  + Lao*LaoV + Male*MaleV + Job1*GovtV + Job2*BusinesV  + Job3*StudV + Age2*Age21_30V + Age3*Age31_40V + Age41_50V*Age4   + Hinc2*MidIncV + Hinc1*LowIncV  + data$ChLug*LugV  + data$Accom*AccomV + data$Tpp*BusiTripV  + data$Dependency*DepenV + data$WeaCon*RainV                                                            
    
    
    
  
  
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
  initValue <- list(C1=0,C3=0,C4=0,C5=0,LaoM=0,LaoT=0,LaoB=0,LaoV=0,MaleM=0,MaleT=0,MaleB=0,MaleV=0,GovtM=0,GovtT=0,GovtB=0,GovtV=0,BusinesM=0,BusinesT=0,BusinesB=0,BusinesV=0,StudM=0,StudT=0,StudB=0,StudV=0,
                    Age21_30M=0,Age21_30T=0,Age21_30B=0,Age21_30V=0,Age31_40M=0,Age31_40T=0,Age31_40B=0,Age31_40V=0,Age41_50M=0,Age41_50T=0,Age41_50B=0,Age41_50V=0,
                    MidIncM=0,MidIncT=0,MidIncB=0,MidIncV=0,LowIncM=0,LowIncT=0,LowIncB=0,LowIncV=0,LugM=0,LugT=0,LugB=0,LugV=0,AccomM=0,AccomT=0,AccomB=0,AccomV=0,BusiTripM=0,BusiTripT=0,BusiTripB=0,BusiTripV=0,DepenM=0,DepenT=0,DepenB=0,DepenV=0,RainM=0,RainT=0,RainB=0,RainV=0,
                    Cost=0,Vehtime=0,Delay=0,Wait=0,Access=0,Transfer=0,Parking=0
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

