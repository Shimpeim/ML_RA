

dir.sub  <- "./Prog/sub"
ROC.func <- "functions20170410.R"
Bibtex   <- TRUE

note <- '19Aug17'
  
# Require libraries #

source(
  sprintf(
    "%s/%s",
    dir.sub,
    'require_libraries.R'
    )
  )

##== ANALYSIS DATA SET IMPORT ==##

data <- read.csv(
  '../Analysis/Data/170725/data170725.csv'
  ) %>%
  mutate(
    CRRP      = as.factor(CRRP),
    cm_bio    = as.factor(cm_bio),
    PSL       = as.factor(PSL),
    sex       = as.factor(sex),
    age       = as.numeric(age),
    MTX_use   = as.factor(MTX_use),
    dose_MTX_base = as.numeric(ifelse(!is.na(dose_MTX_base),dose_MTX_base,0)),
    MTX_1yr       = as.numeric(ifelse(!is.na(MTX_1yr),MTX_1yr,0)),
    autoantibody  = as.factor(ifelse(!is.na(autoantibody),autoantibody,3)),
    duration      = as.factor(ifelse(duration>=3,1,0)),
    duration_raw  = duration
    ) %>%
  dplyr::select(
    -id,
    -MTX_base,
    -csDMARD_change,
    -csDMARD_increase,
    -csDMARD_strength,
    -TSS.1,
    -DAS28ESR_auc.1#,
#    -ESR,
#   -CRP
    )

##== data split (train./ pred.) ==##

## caret package ##
# https://www.slideshare.net/sfchaos/ss-33703018

data_cmp <- data[complete.cases(data),]

row_in_train <- createDataPartition(
  data_cmp$CRRP, # data w/o scaling
  p=.8,
  list=FALSE
  )

train_imp  <- data_cmp[  row_in_train, ] %>% dplyr::select(-ESR,-JSN)
pred_imp   <- data_cmp[ -row_in_train, ] %>% dplyr::select(-ESR,-JSN)

##== make table 1 ==##

source('./Prog/sub/table_1.R')

##== RF ==##

source('./Prog/sub/randomforest_20170801.R')

seq_weightRF <- seq(6.0,6.5,by=0.025) 
weightRF     <- 1*10**(-seq_weightRF) 

WRF_results <- llply(weightRF,imbRF_greed,method='WRF')
BRF_results <- imbRF_greed(method='BRF')

weight_sens <- data.frame()
for(i in 1:length(seq_weightRF)){
  weight_sens[i,1] <- log(WRF_results[[i]]$weight,10)
  weight_sens[i,2] <- WRF_results[[i]]$mtry
  weight_sens[i,3] <- WRF_results[[i]]$sens.valid
  weight_sens[i,4] <- WRF_results[[i]]$sens.train
  weight_sens[i,5] <- WRF_results[[i]]$spec.valid
  weight_sens[i,6] <- WRF_results[[i]]$spec.train
}

write.csv(
  weight_sens,
  sprintf(
    fmt = './%s/%s/%s_%s',
    'Output',
    'weights',
    note,
    'weight_sens.csv')
  )


