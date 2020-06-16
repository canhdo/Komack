########## Multinomial Logit Model ##########
#### Individual Unit ##########
##### Access mode choices ##### Sample size: 1042 Observations #####
# Code: 1 (Motorbike); 2 (Private car); 3 (Taxi); 4 (Airport bus); 5 (VCAT)#

#<<<First step>>>
##### Data Reading from csv file #####
  
     library(readr)
     data <- read_csv("20200423_Data5.csv")
     # names(data)
     # summary(data)
  
#### Creating dummy variables ########
    # Dummies (or indicator) for Nationality
     data$Nat1   <- ifelse(data$Nationality==1,1,0) #Vientiane resident
     data$Nat2   <- ifelse(data$Nationality==2,1,0) #Non vientiane resident
     data$Nat3   <- ifelse(data$Nationality==3,1,0) #Foreign resident
     data$Lao    <- ifelse(data$Nationality<=2,1,0)  #Lao resident
     
     # Dummies (or indicator) for Gender
     Male    <- ifelse(data$Gender==1,1,0) #Male
     Female  <- ifelse(data$Gender==0,1,0) #Female
     
     
     # Dummies (or indicator) for Age
     Age1  <- ifelse(data$Age==1,1,0) # <20 
     Age2  <- ifelse(data$Age==2,1,0) # =21-30
     Age3  <- ifelse(data$Age==3,1,0) # =31-40
     Age4  <- ifelse(data$Age==4,1,0) # =41-50
     data$Age5  <- ifelse(data$Age==5,1,0) # >51
     
     
     # Dummies (or indicator) for Education
     Edu1    <- ifelse(data$Edu==1,1,0) # Highshool
     Edu2    <- ifelse(data$Edu==2,1,0) # College
     Edu3    <- ifelse(data$Edu==3,1,0) # Bachelor
     Edu4    <- ifelse(data$Edu==4,1,0) # Master
     data$HEdu <-ifelse(data$Edu >2,1,0) # High Edu
     
     # Dummies (or indicator) for Occupation
     Job1  <- ifelse(data$Job==1,1,0)  # Goverment offical + Dr/nurse + Prof.
     Job2  <- ifelse(data$Job==2,1,0)  # Businessperson +Salesperson + Self-employed person
     Job3  <- ifelse(data$Job==3,1,0)  # Students
     Job4  <- ifelse(data$Job==4,1,0)  # Others + Retired/Unemployed + Housekeeper
     data$HJob <- ifelse(data$Job <3,1,0)
     
     # Dummies (or indicator) for Monthly household income
     data$Hinc1 <- ifelse(data$Hinc==1,1,0) # Low income <4,000,000 kip
     data$Hinc2 <- ifelse(data$Hinc==2,1,0) # 4,000,000 kip< Midle income <10,000,000 kip
     data$Hinc3 <- ifelse(data$Hinc==3,1,0) # High income >10,000,000 kip
     
     # Dummies (or indicator) for Dependency of others
     
     Dependence1 <- ifelse(data$Dependency==1,1,0) #Car-dropped
     Dependence2 <- ifelse(data$Dependency==2,1,0) #Car-parked
     Dependence3 <- ifelse(data$Dependency==3,1,0) #Shared ride 
     
     # Dummies (or indicator) for Household members
     Hmem1 <- ifelse(data$Hcar==1,1,0) # 1 member
     Hmem2 <- ifelse(data$Hcar==2,1,0) # 2 members
     Hmem3 <- ifelse(data$Hcar==3,1,0) # 3 members
     Hmem4 <- ifelse(data$Hcar==4,1,0) # 4 members
     Hmem5 <- ifelse(data$Hcar==5,1,0) # 5 members
     
     # Dummies (or indicator) for Number of cars in a Household
     Hcar0 <- ifelse(data$Hcar==0,1,0) # no car
     Hcar1 <- ifelse(data$Hcar==1,1,0) # 1 car
     Hcar2 <- ifelse(data$Hcar==2,1,0) # 2 cars
     Hcar3 <- ifelse(data$Hcar==3,1,0) # 3 cars
     Hcar4 <- ifelse(data$Hcar==4,1,0) # 4 cars
     
     
     # Dummies (or indicator) for Number of Check-in Luggage 
     # lug0 <- ifelse(data$Chlug==0,1,0) # no lug
     # lug1 <- ifelse(data$Chlug==1,1,0) # 1 lug
     # lug2 <- ifelse(data$Chlug==2,1,0) # 2 lug
     # lug3 <- ifelse(data$Chlug==3,1,0) # 3 lug
     # 
     # # Dummies (or indicator) for Trip purpose  
     # 
     # tpp1 <- ifelse(data$Tpurpose==1,1,0) # business trip
     # tpp2 <- ifelse(data$Tpurpose==2,1,0) # holiday trip
     # tpp3 <- ifelse(data$Tpurpose==3,1,0) # education trip
     # tpp4 <- ifelse(data$Tpurpose==4,1,0) # visiting/home trip
     
  ### Correlation matrix ####
     data1 <- data[,c("HJob","Hinc1","Hinc2","Hinc3")]
     cor(data1)
        
     
#<<<Second step>>>
##### Defining Log-likehood function #####
LL <- function (C1,C3,C4,C5,LaoM,LaoT,LaoB,LaoV,MaleM,MaleT,MaleB,MaleV,
                Age50M,Age50T,Age50B,Age50V,
                HEduM, HEduT, HEduB, HEduV,
                GovtM,GovtT,GovtB,GovtV,BusinesM,BusinesT,BusinesB,BusinesV,
                LowIncM,LowIncT,LowIncB,LowIncV,MidIncM,MidIncT,MidIncB,MidIncV,
                LugM,LugT,LugB,LugV,AccomM,AccomT,AccomB,AccomV,
                BusiTripM,BusiTripT,BusiTripB,BusiTripV,
                DepenC,DepenM,
                RainC, RainT,RainV
        )
           
  
{ #Begin function

  # Utility function
  
    V2   <- 0    + data$Dependency*DepenC + data$WeaCon*RainC 
    
    V1   <- C1   + data$Lao*LaoM + Male*MaleM + Job1*GovtM + Job2*BusinesM   +  data$Age5*Age50M  +
                  data$HEdu*HEduM + data$Hinc2*MidIncM + data$Hinc1*LowIncM  + data$ChLug*LugM  + data$Accom*AccomM + data$Tpp*BusiTripM +   
                  data$Dependency*DepenM 
    
    V3   <- C3   + data$Lao*LaoT + Male*MaleT + Job1*GovtT + Job2*BusinesT   +  data$Age5*Age50T +
                  data$HEdu*HEduT + data$Hinc2*MidIncT + data$Hinc1*LowIncT  + data$ChLug*LugT  + data$Accom*AccomT + data$Tpp*BusiTripT +                          
                  0 + data$WeaCon*RainT   
    
    V4   <- C4   + data$Lao*LaoB + Male*MaleB + Job1*GovtB + Job2*BusinesB   +  data$Age5*Age50B +
                   data$HEdu*HEduB + data$Hinc2*MidIncB + data$Hinc1*LowIncB  + data$ChLug*LugB  + data$Accom*AccomB + data$Tpp*BusiTripB +
                  0 + 0   
                   
    V5   <- C5   + data$Lao*LaoV + Male*MaleV + Job1*GovtV + Job2*BusinesV   +  data$Age5*Age50V  +
                  data$HEdu*HEduV + data$Hinc2*MidIncV + data$Hinc1*LowIncV  + data$ChLug*LugV  + data$Accom*AccomV + data$Tpp*BusiTripV + 
                   0 + data$WeaCon*RainV                                                            
    
    
    
  
  
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
  initValue <- list(C1=0,C3=0,C4=0,C5=0,LaoM=0,LaoT=0,LaoB=0,LaoV=0,MaleM=0,MaleT=0,MaleB=0,MaleV=0,
                    GovtM=0,GovtT=0,GovtB=0,GovtV=0,BusinesM=0,BusinesT=0,BusinesB=0,BusinesV=0,
                    Age50M=0,Age50T=0,Age50B=0,Age50V=0,
                    HEduM=0, HEduT=0, HEduB=0, HEduV=0,
                    MidIncM=0,MidIncT=0,MidIncB=0,MidIncV=0,LowIncM=0,LowIncT=0,LowIncB=0,LowIncV=0,
                    LugM=0,LugT=0,LugB=0,LugV=0,AccomM=0,AccomT=0,AccomB=0,AccomV=0,BusiTripM=0,BusiTripT=0,BusiTripB=0,BusiTripV=0,
                    DepenC=0, DepenM=0,
                    RainC=0,RainT=0,RainV=0
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
