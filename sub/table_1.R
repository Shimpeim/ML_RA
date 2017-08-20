##== Table 1 ==##

data_summ <- pred_imp %>%
  mutate(vali='validation') %>%
  rbind(train_imp %>%
          mutate(vali='training')
  ) %>%

  dplyr::select(
    vali, CRRP,
    age, DAS28ESR_auc, TSS, HAQ,
    dose_MTX_base,MTX_1yr, CRP, 
    erosion, est1yrTSS
  ) %>%
  gather(var,val,-vali,-CRRP) %>%
  group_by(var, vali,CRRP) %>%
  summarize(
    n      = n(),
    mean   = mean(val),
    std    = sd(val),
    Q2     = quantile(val, 0.25),
    median = median(val),
    Q4     = quantile(val, 0.75)
  ) %>%
  mutate(
    mean_col = ifelse(
      var %in%
        c(
          'age' ,
          'duration_raw',
          'dose_MTX_base',
          'MTX_1yr'
        ),
      format(round(mean,1),nsmall=1),
      ifelse(
        var %in% c(
          'DAS28ESR_auc',
          'HAQ'
          ),
        format(round(mean,2),nsmall=2),
        ifelse(
          var %in% c(
            'CRP',
            'TSS',
            'erosion',
            'est1yrTSS'
            ),
          format(round(median,1),nsmall=1)
          )
        )
      ) ,
    
    sd_col = ifelse(
      var %in%
        c(
          'age' ,
          'duration_raw',
          'dose_MTX_base',
          'MTX_1yr'
        ),
      format(round(std,1),nsmall=1),
      ifelse(
        var %in% c(
          'DAS28ESR_auc',
          'HAQ'
        ),
        format(round(std,2),nsmall=2),
        ifelse(
          var %in% c(
            'CRP',
            'TSS',
            'erosion',
            'est1yrTSS'
          ),
          paste(
            format(round(Q2,1),nsmall=1),
            format(round(Q4,1),nsmall=1),
            sep='-'
            )
          )
        )
      )
    )
          
data_summ_pct <- pred_imp %>%
  mutate(vali='validation') %>%
  rbind(train_imp %>%
          mutate(vali='training')
  ) %>%

  dplyr::select(
    vali, CRRP,
    cm_bio, sex, duration, autoantibody,
    MTX_use, PSL
  ) %>%
  gather(var,val,-vali,-CRRP) %>%
  filter(!(var=='autoantibody' & val=='3')) %>%
  group_by(var, vali,CRRP) %>%
  summarize(
    n      = n(),
    n_class1      = sum(as.numeric(val) ),
    prop   = sum(as.numeric(val) )/ n()
    ) %>%
  mutate(
    pct = paste(
      round(100*prop,1),
      '%',
      sep='')
    )

table_1_long <- data_summ %>%
  rbind(data_summ_pct )

write.csv(
  table_1_long, 
  sprintf(
    fmt = './%s/%s/%s_%s',
    'Output',
    'Table_1',
    note,
    'table1.csv')
  )
  
