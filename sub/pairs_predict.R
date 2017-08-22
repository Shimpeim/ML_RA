
quartz(width=20, height=15,type="pdf", 
       file= sprintf(
         './%s/%s/%s',
         'Output',
         'Pairs',
         'pairs._pred_red.pdf')
)
theme_set(
  theme_classic(
    base_size = 18, 
    base_family = 'YuGo')
)

pairs(
  main = 'Duration of RA <= 3 yrs at baseline',
  xlim =c(0,100),
  ylim =c(0,100),
  pred_imp_pred %>%
        mutate(
          est1yrTSS = rank(est1yrTSS) ,
          CRP = rank(CRP), 
          DAS28ESR_auc = rank(DAS28ESR_auc), 
          TSS = rank(TSS), 
          colour = ifelse(CRRP==1,as.numeric(CRRP)+1,0)
        ) %>%
        filter(duration ==0 )%>%
        dplyr::select(
          est1yrTSS, CRP, DAS28ESR_auc, TSS) ,
      #  panel = panel.smooth,
      cex = c(1.0, 1.5)[unclass(pred_imp_pred$CRRP)] ,
      pch = c(1, 25)[unclass(pred_imp_pred$pred)], 
      bg = c('red','green')[unclass(pred_imp_pred$CRRP)] ,
      cex.labels = 2, font.labels = 1
)

pairs(       
  main = 'Duration of RA > 3 yrs at baseline',
  xlim =c(0,100),
  ylim =c(0,100),
  
  pred_imp_pred %>%
        mutate(
          est1yrTSS = rank(est1yrTSS) ,
          CRP = rank(CRP), 
          DAS28ESR_auc = rank(DAS28ESR_auc), 
          TSS = rank(TSS), 
          
          colour = ifelse(CRRP==1,as.numeric(CRRP)+1,0)
        ) %>%
        filter(duration ==1 )%>%
        dplyr::select(
          est1yrTSS, CRP, DAS28ESR_auc, TSS) ,
      #  panel = panel.smooth,
      cex = c(1.0, 1.5)[unclass(pred_imp_pred$CRRP)] ,
      pch = c(1, 21)[unclass(pred_imp_pred$CRRP)], 
      bg = c('white','black')[unclass(pred_imp_pred$CRRP)] ,
      cex.labels = 2, font.labels = 1
)

pairs(
  main = 'Without concomitance of PSL at baseline',
  xlim =c(0,100),
  ylim =c(0,100),
  
  pred_imp_pred %>%
        mutate(
          est1yrTSS = rank(est1yrTSS) ,
          CRP = rank(CRP), 
          DAS28ESR_auc = rank(DAS28ESR_auc), 
          TSS = rank(TSS), 
          
          colour = ifelse(CRRP==1,as.numeric(CRRP)+1,0)
        ) %>%
        filter(PSL == 0 )%>%
        dplyr::select(
          est1yrTSS, CRP, DAS28ESR_auc, TSS) ,
      #  panel = panel.smooth,
      cex = c(1.0, 1.5)[unclass(pred_imp_pred$CRRP)] ,
      pch = c(1, 21)[unclass(pred_imp_pred$CRRP)], 
      bg = c('white','black')[unclass(pred_imp_pred$CRRP)] ,
      cex.labels = 2, font.labels = 1
)
pairs(
  main = 'With concomitance of PSL at baseline',
  xlim =c(0,100),
  ylim =c(0,100),
  
  pred_imp_pred %>%
        mutate(
          est1yrTSS = rank(est1yrTSS) ,
          CRP = rank(CRP), 
          DAS28ESR_auc = rank(DAS28ESR_auc), 
          TSS = rank(TSS), 
          
          colour = ifelse(CRRP==1,as.numeric(CRRP)+1,0)
        ) %>%
        filter(PSL == 1 )%>%
        dplyr::select(
          est1yrTSS, CRP, DAS28ESR_auc, TSS) ,
      #  panel = panel.smooth,
      cex = c(1.0, 1.5)[unclass(pred_imp_pred$CRRP)] ,
      pch = c(1, 21)[unclass(pred_imp_pred$CRRP)], 
      bg = c('white','black')[unclass(pred_imp_pred$CRRP)] ,
      cex.labels = 2, font.labels = 1
)
pairs(
  main = 'Sex = male',
  xlim =c(0,100),
  ylim =c(0,100),
  
  pred_imp_pred %>%
        mutate(
          est1yrTSS = rank(est1yrTSS) ,
          CRP = rank(CRP), 
          DAS28ESR_auc = rank(DAS28ESR_auc),
          TSS = rank(TSS), 
          
          colour = ifelse(CRRP==1,as.numeric(CRRP)+1,0)
        ) %>%
        filter(sex == 0 )%>%
        dplyr::select(
          est1yrTSS, CRP, DAS28ESR_auc, TSS) ,
      #  panel = panel.smooth,
      cex = c(1.0, 1.5)[unclass(pred_imp_pred$CRRP)] ,
      pch = c(1, 21)[unclass(pred_imp_pred$CRRP)], 
      bg = c('white','black')[unclass(pred_imp_pred$CRRP)] ,
      cex.labels = 2, font.labels = 1
)
pairs(
  main= 'Sex = Female',
  xlim =c(0,100),
  ylim =c(0,100),
  
  pred_imp_pred %>%
        mutate(
          est1yrTSS = rank(est1yrTSS) ,
          CRP = rank(CRP), 
          DAS28ESR_auc = rank(DAS28ESR_auc), 
          TSS = rank(TSS), 
          
          colour = ifelse(CRRP==1,as.numeric(CRRP)+1,0)
        ) %>%
        filter(sex == 1 )%>%
        dplyr::select(
          est1yrTSS, CRP, DAS28ESR_auc, TSS) ,
      #  panel = panel.smooth,
      cex = c(1.0, 1.5)[unclass(pred_imp_pred$CRRP)] ,
      pch = c(1, 21)[unclass(pred_imp_pred$CRRP)], 
      bg = c('white','black')[unclass(pred_imp_pred$CRRP)] ,
      cex.labels = 2, font.labels = 1
)
pairs(  
  main= 'Not started biological DMARDs within 3 months from baseline',
  xlim =c(0,100),
  ylim =c(0,100),
  
  pred_imp_pred %>%
    mutate(
          est1yrTSS = rank(est1yrTSS) ,
          CRP = rank(CRP), 
          DAS28ESR_auc = rank(DAS28ESR_auc), 
          TSS = rank(TSS), 
          
          colour = ifelse(CRRP==1,as.numeric(CRRP)+1,0)
        ) %>%
        filter(cm_bio == 0 )%>%
        dplyr::select(
          est1yrTSS, CRP, DAS28ESR_auc, ESR, TSS) ,
      #  panel = panel.smooth,
      cex = c(1.0, 1.5)[unclass(pred_imp_pred$CRRP)] ,
      pch = c(1, 21)[unclass(pred_imp_pred$CRRP)], 
      bg = c('white','black')[unclass(pred_imp_pred$CRRP)] ,
      cex.labels = 2, font.labels = 1
)
pairs(
  main= 'Started biological DMARDs within 3 months from baseline',
  xlim =c(0,100),
  ylim =c(0,100),
  
  pred_imp_pred %>%
        mutate(
          est1yrTSS = rank(est1yrTSS) ,
          CRP = rank(CRP), 
          DAS28ESR_auc = rank(DAS28ESR_auc), 
          TSS = rank(TSS), 
          
          colour = ifelse(CRRP==1,as.numeric(CRRP)+1,0)
        ) %>%
        filter(cm_bio == 1 )%>%
        dplyr::select(
          est1yrTSS, CRP, DAS28ESR_auc, TSS) ,
      #  panel = panel.smooth,
      cex = c(1.0, 1.5)[unclass(pred_imp_pred$CRRP)] ,
      pch = c(1, 21)[unclass(pred_imp_pred$CRRP)], 
      bg = c('white','black')[unclass(pred_imp_pred$CRRP)] ,
      cex.labels = 2, font.labels = 1
)
pairs(
  main= 'Without concomitance of MTX at baseline',
  xlim =c(0,100),
  ylim =c(0,100),
  
  pred_imp_pred %>%
        mutate(
          est1yrTSS = rank(est1yrTSS) ,
          CRP = rank(CRP), 
          DAS28ESR_auc = rank(DAS28ESR_auc), 
          TSS = rank(TSS), 
          
          colour = ifelse(CRRP==1,as.numeric(CRRP)+1,0)
        ) %>%
        filter(MTX_use == 0 )%>%
        dplyr::select(
          est1yrTSS, CRP, DAS28ESR_auc, TSS) ,
      #  panel = panel.smooth,
      cex = c(1.0, 1.5)[unclass(pred_imp_pred$CRRP)] ,
      pch = c(1, 21)[unclass(pred_imp_pred$CRRP)], 
      bg = c('white','black')[unclass(pred_imp_pred$CRRP)] ,
      cex.labels = 2, font.labels = 1
)
pairs(
  main= 'With concomitance of MTX at baseline',
  xlim =c(0,100),
  ylim =c(0,100),
  
  pred_imp_pred %>%
        mutate(
          est1yrTSS = rank(est1yrTSS) ,
          CRP = rank(CRP), 
          DAS28ESR_auc = rank(DAS28ESR_auc), 
          TSS = rank(TSS), 
          
          colour = ifelse(CRRP==1,as.numeric(CRRP)+1,0)
        ) %>%
        filter(MTX_use == 1 )%>%
        dplyr::select(
          est1yrTSS, CRP, DAS28ESR_auc, TSS) ,
      #  panel = panel.smooth,
      cex = c(1.0, 1.5)[unclass(pred_imp_pred$CRRP)] ,
      pch = c(1, 21)[unclass(pred_imp_pred$CRRP)], 
      bg = c('white','black')[unclass(pred_imp_pred$CRRP)] ,
      cex.labels = 2, font.labels = 1
)
dev.off()
