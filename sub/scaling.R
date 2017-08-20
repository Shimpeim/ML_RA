##== Scaling
#    (not required of random forest)
#==##

data_scaled <- data %>%
  mutate(
    age = scale(age),
    DAS28ESR_auc = scale(DAS28ESR_auc),
    TSS = scale(TSS),
    HAQ = scale(HAQ),
    dose_MTX_base = scale(dose_MTX_base),
    MTX_1yr = scale(MTX_1yr),
    CRP = scale(CRP),
    ESR = scale(ESR),
    erosion = scale(erosion),
    JSN = scale(JSN),
    est1yrTSS = scale(est1yrTSS)
  )

summary(data_scaled)