########## Multinomial Logit Model ##########
#### Individual Unit ##########
##### Access mode choices ##### Sample size: 1042 Observations #####
# Code: 1 (Motorbike); 2 (Private car); 3 (Taxi); 4 (Airport bus); 5 (VCAT)#

#<<<First step>>>
##### Data Reading from csv file #####


library(readr)
data <- read_csv("DataClean.csv")
#View(data)
# names(data)
 #summary(data)

#### Creating dummy variables ########
# Dummies (or indicator) for Nationality
data$Nat1   <- ifelse(data$Nat==1,1,0) #Vientiane resident
data$Nat2   <- ifelse(data$Nat==2,1,0) #Non vientiane resident
data$Nat3   <- ifelse(data$Nat==3,1,0) #Foreign resident
data$Lao    <- ifelse(data$Nat<2,1,0)  #Lao resident

# Dummies (or indicator) for Gender
Male    <- ifelse(data$Gender==1,1,0) #Male
Female  <- ifelse(data$Gender==2,1,0) #Female


# Dummies (or indicator) for Age
Age1  <- ifelse(data$Age==1,1,0) # <20 
Age2  <- ifelse(data$Age==2,1,0) # =21-30
Age3  <- ifelse(data$Age==3,1,0) # =31-40
Age4  <- ifelse(data$Age==4,1,0) # =41-50
Age5  <- ifelse(data$Age==5,1,0) # =51-60
Age6  <- ifelse(data$Age==6,1,0) # >61
data$Age6  <- ifelse(data$Age==6,1,0) # >61


# Dummies (or indicator) for Education
Edu1    <- ifelse(data$Edu==1,1,0)  # Highshool
Edu2    <- ifelse(data$Edu==2,1,0)  # College
Edu3    <- ifelse(data$Edu==3,1,0)  # Bachelor
Edu4    <- ifelse(data$Edu==4,1,0)  # Master
data$HEdu <-ifelse(data$Edu >=2,1,0) # High Education

# Dummies (or indicator) for Occupation
Job1  <- ifelse(data$Job==1,1,0)  # Goverment offical + Dr/nurse + Prof/teacher.
Job2  <- ifelse(data$Job==2,1,0)  # Businessperson
Job3  <- ifelse(data$Job==3,1,0)  # Salesperson
Job4  <- ifelse(data$Job==4,1,0)  # Self-employed personr
Job5  <- ifelse(data$Job==5,1,0)  # Private-employess
Job6  <- ifelse(data$Job==6,1,0)  # Students
Job7  <- ifelse(data$Job==7,1,0)  # Housekeeper
Job8  <- ifelse(data$Job==8,1,0)  # Retired/Unemployed
Job9  <- ifelse(data$Job==9,1,0)  # Others
data$HJob <- ifelse(data$Job <=5,1,0)

# Dummies (or indicator) for Monthly household income
data$Hinc1 <- ifelse(data$Hinc==1,1,0) # Low 1,299,999
data$Hinc2 <- ifelse(data$Hinc==2,1,0) # 1,300,000-1,999,999
data$Hinc3 <- ifelse(data$Hinc==3,1,0) # 2,000,000-3,999,999
data$Hinc4 <- ifelse(data$Hinc==4,1,0) # 4,000,000-6,999,999
data$Hinc5 <- ifelse(data$Hinc==5,1,0) # 7,000,000-9,999,999
data$Hinc6 <- ifelse(data$Hinc==6,1,0) # 10,000,000-14,999,999
data$Hinc7 <- ifelse(data$Hinc==7,1,0) # 15,000,000-19,999,999
data$Hinc8 <- ifelse(data$Hinc==8,1,0) # Above 20,000,000

data$Inc <- ifelse(data$Hinc >5,1,0)

#Household cars

data$Hcar <- ifelse(data$Hcar>=1,1,0) # Has aleast one car

#Weather condition

data$Rain <- ifelse(data$WCON==1,1,0) # rainy day

# Trip purpose

data$BTPP <- ifelse(data$TPP==1,1,0) # business purpose

# Accompany travelers

data$Accom <- ifelse(data$ACCM==1,1,0) # Accompany travelers

# Check-in luggage
data$LUG <-  ifelse(data$LUG==1,1,0) # Has check-in luggage


# Dependency of others to see off airport
data$Dependency <- ifelse(data$DEP==1,1,0) #Dependency of other

### Correlation matrix ####
#data1 <- data[,c("HJob","Hinc1","Hinc2","Hinc3","Hinc4","Hinc5","Hinc6","Hinc7","Hinc8")]
#cor(data1)


#<<<Second step>>>
##### Defining Log-likehood function #####
LL <- function (Cm,
                Ct,
                Cb,
                Cv,
                LaoM,
                LaoT,
                LaoB,
                LaoV,
                MaleM,
                MaleT,
                MaleB,
                MaleV,
                Age60M,
                Age60T,
                Age60B,
                Age60V,
                HEduM,
                HEduT,
                HEduB,
                HEduV,
                HJobM,
                HJobT,
                HJobB,
                HJobV,
                #HIncM,
                #HIncT,
                #HIncB,
                #HIncV,
                HcarM,
                HcarT,
                HcarB,
                HcarV,               
                LugM,
                LugT,
                LugB,
                LugV,
                AccomM,
                AccomT,
                AccomB,
                AccomV,
                BusiTripM,
                BusiTripT,
                BusiTripB,
                BusiTripV,
                DepenC,
                DepenM,
                DepenT,
                DepenV,
                RainC, 
                RainT,
                RainB,
                RainV
                
)


{ #Begin function
  
  # Utility function
  
  Vc   <- 0    + data$Dependency*DepenC + data$Rain*RainC
  
  Vm   <- Cm   + data$Lao*LaoM + Male*MaleM   + data$HJob*HJobM  +  data$Age6*Age60M  +
    data$HEdu*HEduM  + data$LUG*LugM   + data$BTPP*BusiTripM + data$Accom*AccomM +   
    data$Dependency*DepenM + data$Hcar*HcarM #+ data$Inc*HIncM  
  
  Vt   <- Ct   + data$Lao*LaoT + Male*MaleT   + data$HJob*HJobT   +  data$Age6*Age60T +
    data$HEdu*HEduT + data$LUG*LugT  + data$Accom*AccomT + data$BTPP*BusiTripT +                          
    0 + data$Hcar*HcarT  + data$Rain*RainT+ data$Dependency*DepenT  #+ data$Inc*HIncT 
  
  Vb   <- Cb   + data$Lao*LaoB + Male*MaleB  + data$HJob*HJobB   +  data$Age6*Age60B +
    data$HEdu*HEduB + data$LUG*LugB  + data$Accom*AccomB + data$BTPP*BusiTripB +
    0 + data$Hcar*HcarB  + data$Rain*RainB  #+ data$Inc*HIncB
  
  Vv   <- Cv   + data$Lao*LaoV + Male*MaleV  + data$HJob*HJobV   +  data$Age6*Age60V  +
    data$HEdu*HEduV  + data$LUG*LugV  + data$Accom*AccomV + data$BTPP*BusiTripV + 
    0 + data$Rain*RainV   + data$Hcar*HcarV + data$Dependency*DepenV   #+ data$Inc*HIncV                                                       
  
  
  
  
  
  # Denominator
  Sum <- exp(Vm) + exp(Vc) + exp(Vt) + exp(Vb) + exp(Vv)
  
  
  # Probabilities
  Pm <- exp(Vm)/Sum
  Pc <- exp(Vc)/Sum
  Pt <- exp(Vt)/Sum
  Pb <- exp(Vb)/Sum
  Pv <- exp(Vv)/Sum
  
  
  
  # Dummies (or indicator) for alternatives
  Dm <- ifelse(data$MODE == "Motorbike",1,0)
  Dc <- ifelse(data$MODE == "Private Car",1,0)
  Dt <- ifelse(data$MODE == "Taxi",1,0)
  Db <- ifelse(data$MODE == "Airport Bus",1,0)
  Dv <- ifelse(data$MODE == "Airport Limousine Bus",1,0)
  
  
  
  
  
  # Log-likelihood function
  L <- -(sum(Dm*log(Pm) + Dc*log(Pc) + Dt*log(Pt) + Db*log(Pb) + Dv*log(Pv)))
  return(L)
} #End function

#<<<Third step>>>
##### Model Estimation #####
initValue <- list(Cm=0,
                  Ct=0,
                  Cb=0,
                  Cv=0,
                  LaoM=0,
                  LaoT=0,
                  LaoB=0,
                  LaoV=0,
                  MaleM=0,
                  MaleT=0,
                  MaleB=0,
                  MaleV=0,
                  Age60M=0,
                  Age60T=0,
                  Age60B=0,
                  Age60V=0,
                  HEduM=0,
                  HEduT=0,
                  HEduB=0,
                  HEduV=0,
                  HJobM=0,
                  HJobT=0,
                  HJobB=0,
                  HJobV=0,
                  # HIncM=0,
                  ## HIncT=0,
                  # HIncB=0,
                 # HIncV=0,
                  HcarM=0,
                  HcarT=0,
                  HcarB=0,
                  HcarV=0,                
                  LugM=0,
                  LugT=0,
                  LugB=0,
                  LugV=0,
                  AccomM=0,
                  AccomT=0,
                  AccomB=0,
                  AccomV=0,
                  BusiTripM=0,
                  BusiTripT=0,
                  BusiTripB=0,
                  BusiTripV=0,
                  DepenC=0,
                  DepenM=0,
                  DepenT=0,
                  DepenV=0,
                  RainC=0,
                  RainT=0,
                  RainB=0,
                  RainV=0
                  
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

