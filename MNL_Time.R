########## Multinomial Logit Model ##########
#### Individual Unit ##########
##### Access mode choices ##### Sample size: 1042 Observations #####
# Code: 1 (Motorbike); 2 (Private car); 3 (Taxi); 4 (Airport bus); 5 (VCAT)#

#<<<First step>>>
##### Data Reading from csv file #####

library(readr)
data <- read_csv("20200528_Data.csv")


# names(data)
# summary(data)

#### Creating dummy variables ########
# Dummies (or indicator) for Nationality
data$Nat1   <- ifelse(data$Nationality==1,1,0) #Vientiane resident
data$Nat2   <- ifelse(data$Nationality==2,1,0) #Non vientiane resident
data$Nat3   <- ifelse(data$Nationality==3,1,0) #Foreign resident
data$Lao    <- ifelse(data$Nationality<=2,1,0)  #Lao resident

# Dummies (or indicator) for Gender
data$Male    <- ifelse(data$Gender==1,1,0) #Male
data$Female  <- ifelse(data$Gender==0,1,0) #Female


# Dummies (or indicator) for Age
Age1  <- ifelse(data$Age==1,1,0) # <20 
Age2  <- ifelse(data$Age==2,1,0) # =21-30
Age3  <- ifelse(data$Age==3,1,0) # =31-40
Age4  <- ifelse(data$Age==4,1,0) # =41-50
Age5  <- ifelse(data$Age==5,1,0) # =51-60
Age6  <- ifelse(data$Age==6,1,0) # >61
data$Age6  <- ifelse(data$Age<=3,1,0) # >61


# Dummies (or indicator) for Education
Edu1    <- ifelse(data$Edu==1,1,0)  # Highshool
Edu2    <- ifelse(data$Edu==2,1,0)  # College
Edu3    <- ifelse(data$Edu==3,1,0)  # Bachelor
Edu4    <- ifelse(data$Edu==4,1,0)  # Master
data$HEdu <-ifelse(data$Edu >2,1,0) # High Education

# Dummies (or indicator) for Occupation
data$Job1  <- ifelse(data$Job==1,1,0)  # Goverment offical + Dr/nurse + Prof/teacher.
data$Job2  <- ifelse(data$Job==2,1,0)  # Businessperson
data$Job3  <- ifelse(data$Job==3,1,0)  # Salesperson
data$Job4  <- ifelse(data$Job==4,1,0)  # Self-employed personr
data$Job5  <- ifelse(data$Job==5,1,0)  # Private-employess
data$Job6  <- ifelse(data$Job==6,1,0)  # Students
data$Job7  <- ifelse(data$Job==7,1,0)  # Housekeeper
data$Job8  <- ifelse(data$Job==8,1,0)  # Retired/Unemployed
data$Job9  <- ifelse(data$Job==9,1,0)  # Others
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

data$Inc <- ifelse(data$Hinc >6,1,0)

### Correlation matrix ####
#data1 <- data[,c("HJob","Hinc1","Hinc2","Hinc3","Hinc4","Hinc5","Hinc6","Hinc7","Hinc8")]
#cor(data1)

#data2 <- data[,c("HEdu","Job1","Job2","Job3","Job4","Job5","Job6","Job7","Job8","Job9")]
#cor(data2)

#data3 <- data[,c("Male","Lao","Nat3")]
#cor(data3)

#<<<Second step>>>
##### Defining Log-likehood function #####
LL <- function (Cm,
                Ct,
                Cb,
                Cv,
                #Cost,
                Vehtime,
                Waiting,
                Access,
                Delay,
                Transfer,
                #Parking,
                Safety
)
  
  
{ #Begin function
  
  # Utility function
  
 
  
  Vm   <- Cm     + data$Vehtime_MT*Vehtime + data$Safe_MT*Safety #+   #+ data$ParkFee_MT*Parking
                   #data$TCost_MT*Cost
  
  Vc   <- 0      + data$Vehtime_Car*Vehtime  + data$Safe_Car*Safety    + data$Delay_Car*Delay #+ # + data$ParkFee_Car*Parking
                   #data$TCost_Car*Cost 
  
  Vt   <- Ct      + data$Vehtime_Taxi*Vehtime    + data$Safe_Taxi*Safety     + data$Delay_Taxi*Delay  #+
                    #data$TCost_Taxi*Cost 
  
  Vb   <- Cb    + data$Vehtime_Taxi*Vehtime   + data$Safe_Bus*Safety      + data$Delay_Bus*Delay + data$Wtime_Bus*Waiting +  data$Accesstime_Bus*Access + 
                  data$NoTranfer_Bus*Transfer #+
                  #data$TCost_Bus*Cost 
                  
  Vv   <- Cv    + data$Vehtime_VCAT*Vehtime     + data$Safe_VCAT*Safety          + data$Wtime_VCAT*Waiting + data$Accesstime_VCAT*Access +
                  data$NoTranfer_VCAT*Transfer  #+                                               
                  #data$TCost_VCAT*Cost  
  
  
  
  
  # Denominator
  Sum <- exp(Vm) + exp(Vc) + exp(Vt) + exp(Vb) + exp(Vv)
  
  
  # Probabilities
  Pm <- exp(Vm)/Sum
  Pc <- exp(Vc)/Sum
  Pt <- exp(Vt)/Sum
  Pb <- exp(Vb)/Sum
  Pv <- exp(Vv)/Sum
  
  
  
  # Dummies (or indicator) for alternatives
  Dm <- ifelse(data$Choice == 1,1,0)
  Dc <- ifelse(data$Choice == 2,1,0)
  Dt <- ifelse(data$Choice == 3,1,0)
  Db <- ifelse(data$Choice == 4,1,0)
  Dv <- ifelse(data$Choice == 5,1,0)
  
  
  
  
  
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
                 #Cost=0,
                  Vehtime=0,
                  Waiting=0,
                  Access=0,
                  Delay=0,
                  Transfer=0,
                  #Parking=0,
                  Safety=0
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

