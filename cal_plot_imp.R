#library(patchwork)
library(stringr)
parse_stat<- function(stat){
  numeric_list <- as.numeric(str_extract_all(stat, "[0-9.]+")[[1]])
  # Output
  return(numeric_list)
  

}
cal_curve_hist <-function(data_source, ici_stat,xmax = 20,hist_max = 5000,ext = "",title = "agg full QRISK3 risk score calibration" ,mod = 1,grid = TRUE)
{
  
  
  attach(data_source)
  #title <- paste0(title,"_hist")
  
  #risk score metrics
  rs_met<- read.csv("/gpfs3/well/doherty/users/rii148/ecg_scores/bs_res.csv")

  c_stat <- parse_stat(rs_met$X0[mod])
  print("c idx")
  print(c_stat)
  c_low <- c_stat[1]
  c_index <- format(round(c_stat[2],3),nsmall = 3)
  c_high <-c_stat[3]
  print(c_low)
  c_CI <- format(round(c(c_low,c_high), 3),nsmall = 3)
  print(c_CI)
  

  #ici_stat <- parse_stat(rs_met$X1[mod])
  #ici_low <-ici_stat[1]
  #ici <- format(round(ici_stat[2],3),nsmall =3)
  #ici_high <-ici_stat[3]
  #ici_CI <- format(round(c(ici_low,ici_high), 3),nsmall =3)
  
  ici_ci <- calc_ci(ici_stat)
  ici_low <- ici[1]
  ici <- ici[2]
  ici_high <- ici[3]
  
  
  r2_stat <-parse_stat(rs_met$X2[mod])
  r2_low <- r2_stat[1]
  r_squared <- format(round(r2_stat[2],3),nsmall =3)
  r2_high <-r2_stat[3]
  r_CI <- format(round(c(r2_low,r2_high), 3),nsmall =3)
  
  nb_stat <-parse_stat(rs_met$X4[mod])
  nb_low <-nb_stat[1]
  net_benefit <- format(round(nb_stat[2],3),nsmall =3)
  nb_high <- nb_stat[3]
  nb_CI <- format(round(c(nb_low,nb_high), 3),nsmall =3)
  

  
  # This calibration plot is based on groupings, and uses the Kaplan-Meier S(t) 
  # as a basis for the observed observations and for plotting the smooth curve
  # - this curve is only an approximation.
  # Ideally we would like to have individual observed outcomes and fit to these. 
  # Due to censoring, this is not directly possible, but we can use 
  # 'pseudo' observations to overcome this - which can be generated using the 
  # prodlim command and then plotted against the predicted model risks
  
  f.val      <- prodlim(Hist(qrisk_obs_time,qrisk_10y)~1, data = data_source)
  pseudo.val <- jackknife(f.val, times = 10)
  X          <- matrix(ncol=2, nrow = length(data_source$pred_event_prob))
  X[,1]      <- data_source$pred_event_prob   ### model predictions
  print("og x1")
  print(X[,1])
  print("mean x1")
  print(mean(X[,1]))
  print(max(X[,1]))
  X[,2]      <- pseudo.val ### observed outcomes (pseudo values)
  X <- X[order(X[,1]),]
  print("ord x1")
  print(X[,1])
  # calibration plot
  png("/gpfs3/well/doherty/users/rii148/ecg_scores/cal_curves/ecg_peuso_cal.png", type="cairo")  
  par(pty="s") # make plot square
  plot(X[, 1], 1 - predict(loess(X[,2]~X[,1])), type = 'l',  col = 'grey', xlab='predicted risk', ylab='observed risk', data=data_source, xlim=c(0,1),ylim=c(0,1))
  abline(a=0,b=1)
  dev.off()
  
  # calculate deciles of predicted risk
  deciles <- quantile(X[,1], probs = seq(0, 1, by = 0.1),na.rm = TRUE)
  
  # assign each observation to a decile of predicted risk
  decile_group <- cut(X[,1], breaks = deciles, labels = FALSE, include.lowest = FALSE)
  
  # calculate the mean observed risk for each decile of predicted risk
  mean_observed_risk <- tapply(X[,2], decile_group, mean)
  
  # plot the calibration curve with risk groups
  png("ecg_risk_groups.png", type = "cairo")
  df <- data.frame(X1 = 100* X[, 1], Observed_Risk = 100* (1 - predict(loess(X[, 2] ~ X[, 1]))),decile_group)
  df$X1_count <- df$X1
  decile_df <- data.frame(deciles = 100* deciles[1:10], mean_observed_risk = 100 * (1- mean_observed_risk),dec_key = rep('Risk decile', 10))
  
  #Create a data frame with the required variables
  df_hist <- data.frame(Predicted_Risk = 100* X[, 1], Observed_Outcome = 100* (1 - predict(loess(X[, 2] ~ X[, 1]))))
  
  cols <- c("Smoothed calibaration curve "="blue","Risk decile"="red")
  
  #align limits
  dif <- xmax/5000
  print("first df")
  print(df)
  cal_plot <- ggplot(df, aes(x = X1, y = Observed_Risk,color = 'blue')) +
    
    
    geom_line(aes(color = 'blue') )+
    geom_abline(intercept = 0, slope = 1) +
    
    
    geom_point(data = decile_df, aes(x = deciles, y = mean_observed_risk, fill = "Risk decile"), pch = 21,col = "black",size = 3) +
    
    coord_cartesian(ylim = c(0, xmax),xlim = c(0,xmax))+
    scale_colour_manual(name="Legend",values=c(red="Risk decile",blue="blue"),
                        labels=c('Smoothed calibration curve','Risk decile')) +
    
    #geom_point(aes(x = deciles, y = mean_observed_risk), col = "red", pch = 16) +
    labs(x = '10 year Predicted PD Risk(%)', y = 'Observed Risk(%)',size = 4)+
    
    theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank())+
    #xlim(0, xmax)+
    #ylim(0, xmax)+
    theme(legend.position =  c(0.3, 0.9), legend.title = element_blank(),legend.key=element_blank(),legend.background=element_blank(),legend.spacing = unit(-0.7, "cm"),legend.text = element_text(size = 12),
          #axis colour
          axis.ticks.y.right = element_line(color = "sky blue"),
          axis.text.y.right = element_text(color = "sky blue"),
          axis.title.y.right = element_text(color = "sky blue"))+
    
    #axis.ticks.y.left = element_line(color = "blue"),
    #axis.text.y.left = element_text(color = "blue"),
    #axis.title.y.left = element_text(color = "blue"))+
    
    annotate("text", x = xmax, y = 6, label = expression(bold("Risk score metrics (95% CI):")), color = "black",size = 4,hjust = 1, vjust = 0) +
    
    annotate("text", x = xmax, y = 4.5, label = paste0("ICI:", ici,"(",ici_CI[1],",",ici_CI[2],")"), color = "black",size = 4 ,hjust = 1, vjust = 0) +
    
    annotate("text", x = xmax, y = 3, label = paste0("C-Index:", c_index,"(",c_CI[1],",",c_CI[2],")"), color = "black",size = 4, hjust = 1, vjust = 0) +
    
    annotate("text", x = xmax, y = 1.5, label = paste0("R-squared:", r_squared,"(",r_CI[1],",",r_CI[2],")"), color = "black" ,size = 4,hjust = 1, vjust = 0) +
    annotate("text", x = xmax, y = 0, label = paste0("Net Benefit:", net_benefit,"(",nb_CI[1],",",nb_CI[2],")"), color = "black" ,size = 4,hjust = 1, vjust = 0)
  print("geetibg hist ")
  pred_hist <- ggplot(df,aes(x = X1_count))+
  geom_histogram(fill = "sky blue", alpha = 0.2, bins = 100, color = "black", show.legend = FALSE) +
    
    xlim(0,20)+
    labs( y = 'Prediction\ncounts',size = 4)
    #ylim()
    
    if (grid){
      pred_hist <- pred_hist + theme(#panel.grid.major = element_blank(),  # Remove grid lines
          #panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())  # Remove axis ticks)  # Remove x-axis line)
    }
  else{
    pred_hist <- pred_hist + theme_void()
  }
  
  
  # Add grey grid to the plot
  plot <- plot #+ theme(panel.grid = element_line(color = "grey", linetype = "blank"))
  
  # Set x-axis and y-axis limits
  print("make my plot")
  plot <-  pred_hist +  labs(title = title) +  cal_plot +
    plot_layout(
      ncol = 1, 
      #nrow = 2, 
      heights = c(0.2,0.8)
      #respect = TRUE
      #align = "none"  # Set align parameter to "none" to align the plots horizontally
    )
  
  plot <- plot +
    theme(plot.margin = unit(rep(0, 4), "lines"))  # Remove extra padding
  
  
  file_location <- paste0('/gpfs3/well/doherty/users/rii148/ecg_scores/cal_curves/metrics/',ext,title)
  print(file_location)
  ggsave(paste0(file_location,'.png'), plot, width = 6, height = 5,dpi = 1200)
  dev.off()
  
  a <- 1- predict(loess(X[,2]~X[,1]))
  png(paste0('/gpfs3/well/doherty/users/rii148/ecg_scores/cal_curves/metrics/ecg_risk_hist.png'), type = "cairo")
  #hist(X[,1], breaks = 10, col = "skyblue", xlab = "Values", ylab = "Frequency")
  hist(X[,1], breaks = seq(min(X[,1]), max(X[,1]) + 0.05, by = 0.05), col = "skyblue", xlab = "Values", ylab = "Frequency")
  dev.off()
  
  png("/gpfs3/well/doherty/users/rii148/ecg_scores/cal_curves/metrics/ecg_obs_hist.png", type = "cairo")
  
  #hist(a, breaks = 10, col = "skyblue", xlab = "Values", ylab = "Frequency")
  hist(a, breaks = seq(min(a), max(a) + 0.05, by = 0.05), col = "skyblue", xlab = "Values", ylab = "Frequency")
  dev.off()
  print(df)
  print(X[,1])
  print(100* X[,1])
  print(mean(df$X1))
  print(max(df$X1))
 return (pred_hist) 
}
source(file.path("/well/doherty/users/rii148/plot_scripts/",'get_ci.R'), echo = T)
cal_curve <-function(data_source, mets,rows_with_na,lab = 'macce_9y',pred_event_prob = 'ukbb_full',xmin = 0,xmax = 20,hist_max = 2500,title = "agg full QRISK3 risk score calibration",store_path = "/gpfs3/well/doherty/users/rii148/ecg_scores/cal_curves/metrics/macce/minb/",met_path = '/gpfs3/well/doherty/users/rii148/ecg_scores/macce_res/minb_set/',offset = 0,event_col = 'macce.incident',time_col = 'macce_obs_time',time = 6)
{


  attach(data_source)
  
  
  #risk score metrics
  #rs_met<- read.csv(paste0(met_path,"bs_res.csv")) # /macce_res/full_set/
  print("c-index")
  c_stat <-pool_bs(mets,'c_idx',rows_with_na)
  c_low <- c_stat[1]
  c_index <- round(c_stat[2],2)#format(0.73, nsmall = 3)
  c_high <-c_stat[3]
  c_CI <- round(c(c_low,c_high), 2)
  
  print("ICI")
  ici_stat <- pool_bs(mets,'ici',rows_with_na)
  ici_low <-ici_stat[1]
  ici <- round(ici_stat[2],4)#format(0.73, nsmall = 3)
  ici_high <-ici_stat[3]
  ici_CI <- round(c(ici_low,ici_high), 4)
  

  
  print("r2")
  r2_stat <- pool_bs(mets,'r2',rows_with_na)
  r2_low <- r2_stat[1]
  r_squared <- round(r2_stat[2],2)#format(0.73, nsmall = 3)
  r2_high <-r2_stat[3]
  r_CI <- round(c(r2_low,r2_high), 2)
  
  print("nb")
  nb_stat <- pool_bs(mets,'nb_5',rows_with_na)
  nb_low <-nb_stat[1]
  net_benefit <- round(nb_stat[2],2)#format(0.73, nsmall = 3)
  nb_high <- nb_stat[3]
  nb_CI <- round(c(nb_low,nb_high), 2)
  
  # This calibration plot is based on groupings, and uses the Kaplan-Meier S(t) 
  # as a basis for the observed observations and for plotting the smooth curve
  # - this curve is only an approximation.
  # Ideally we would like to have individual observed outcomes and fit to these. 
  # Due to censoring, this is not directly possible, but we can use 
  # 'pseudo' observations to overcome this - which can be generated using the 
  # prodlim command and then plotted against the predicted model risks
  
  print('pseudo val')
  print(data_source[,pred_event_prob])
  print("pes")
  formula <- as.formula(paste("Hist(", time_col, ",", event_col, ") ~ 1"))
  f.val <- prodlim(formula, data = data_source)
  #f.val      <- prodlim(Hist(data_source[,time_col],data_source[,event_col])~1 ,data = data_source)
  pseudo.val <- jackknife(f.val, times = time)
  X          <- matrix(ncol=2, nrow = length(data_source[,pred_event_prob]))
  X[,1]      <- data_source[,pred_event_prob]/100   ### model predictions
  X[,2]      <- pseudo.val ### observed outcomes (pseudo values)
  X <- X[order(X[,1]),]
  
  print(X[,1])
  print("obs")
  print(X[,2])
  print("pred")
  # calibration plot
  #png("/gpfs3/well/doherty/users/rii148/ecg_scores/cal_curves/ecg_peuso_cal.png", type="cairo")  
  #print('got png')
  #par(pty="s") # make plot square
  #plot(X[, 1], 1 - predict(loess(X[,2]~X[,1])), type = 'l',  col = 'grey', xlab='predicted risk', ylab='observed risk', data=data_source, xlim=c(0,1),ylim=c(0,1))
  #abline(a=0,b=1)
  #dev.off()
  # we could rather plot the observed and predicted survival as before, and so 
  # add the KM groupings
  
  #png("peuso_cal_group.png", type="cairo")  
  #plot(1 - X[, 1], predict(loess(X[,2]~X[,1])), type = 'l', col = 'grey', xlab='predicted survival', ylab='observed survival', data=ecg_surv, xlim=c(0,1),ylim=c(0,1))
  #groupkm(ecg_surv$surv_10, S = Surv(qrisk_obs_time,qrisk.incident), g=10,u=5, pl=T, add=T,lty=0,cex.subtitle=FALSE)
  #abline(a=0,b=1)
  #dev.off()
  
  # calculate deciles of predicted risk
  deciles <- quantile(X[,1], probs = seq(0, 1, by = 0.1),na.rm = TRUE)
  
  # assign each observation to a decile of predicted risk
  decile_group <- cut(X[,1], breaks = deciles, labels = FALSE, include.lowest = FALSE)
  
  # calculate the mean observed risk for each decile of predicted risk
  mean_observed_risk <- tapply(X[,2], decile_group, mean)
  print('got deciles')
  # plot the calibration curve with risk groups
  #png("ecg_risk_groups.png", type = "cairo")
  # plot(X[, 1], 1 - predict(loess(X[,2]~X[,1])), type = 'l',  col = 'grey', xlab='predicted risk', ylab='observed risk', data=data_source, xlim=c(0,0.4),ylim=c(0,1))
  # abline(a=0,b=1)
  # #abline(a=0,b=1)
  # grid(col = "grey")  # Add grey grid to the plot
  # 
  # # add risk groups to the plot
  # for (i in 1:length(deciles)) {
  #   x_coord <- deciles[i]
  #   y_coord <- 1 - mean_observed_risk[i]
  #   #text(x_coord, y_coord, paste0(i*10, "%"), pos = 4)
  #   #text(x_coord, y_coord,deciles[i], pos = 4)
  #   #abline(v = x_coord, lty = 2)
  #   print(x_coord)
  #   print(y_coord)
  #   points(x_coord, y_coord, col = "red", pch = 16)  # Plot red dots instead of lines
  #}
  # Create a data frame with the required variables
  df <- data.frame(X1 = 100* X[, 1], Observed_Risk = 100* (1 - predict(loess(X[, 2] ~ X[, 1]))),decile_group)
  df$X1_count <- df$X1
  decile_df <- data.frame(deciles = 100* deciles[1:10], mean_observed_risk = 100 * (1- mean_observed_risk),dec_key = rep('Risk decile', 10))
  
  #Create a data frame with the required variables
  #df_hist <- data.frame(Predicted_Risk = 100* X[, 1], Observed_Outcome = 100* (1 - predict(loess(X[, 2] ~ X[, 1]))))
  
  cols <- c("Smoothed calibaration curve "="blue","Risk decile"="red")
  
  #align limits
  print("hist max")
  pred_hist <- ggplot(df[df$X1< xmax,],aes(x = X1_count))+
    geom_histogram( fill = "sky blue", alpha = 0.2, bins = 200, color = "black", show.legend = FALSE) 
  
  hist_data <- ggplot_build(pred_hist)$data[[1]]
  max_bin <- max(hist_data$count)
    #xlim(0,20)
  dif <- xmax/hist_max
  print("get curve")
  #png("test.png",type = 'cairo')
  df_cap <- df[df$X1<xmax,]
  print('have hist')
  #geom_histogram(fill = "sky blue", alpha = 0.2, bins = 100, color = "black", show.legend = FALSE) +
  plot <- ggplot(df_cap, aes(x = X1, y = Observed_Risk,color = 'blue')) +
    geom_histogram(data = df_cap,aes(x = X1_count, y = ..count..*dif), fill = "sky blue", alpha = 0.2, bins = 50, color = "black", show.legend = FALSE) +
    scale_y_continuous(name = "Observed risk(%)",
                       sec.axis = sec_axis(~ ./dif +2.5,
                                           name = "Count of Observed risk")) + #  inherit.aes = FALSE
    
    geom_line(aes(color = 'blue') )+
    geom_abline(intercept = 0, slope = 1) +
    
    
    geom_point(data = decile_df, aes(x = deciles, y = mean_observed_risk, fill = "Risk decile"), pch = 21,col = "black",size = 3) +
    
    coord_cartesian(ylim = c(0, xmax),xlim = c(xmin,xmax))+
    scale_colour_manual(name="Legend",values=c(red="Risk decile",blue="blue"),
                        labels=c('Smoothed calibration curve','Risk decile')) +
    
    #geom_point(aes(x = deciles, y = mean_observed_risk), col = "red", pch = 16) +
    labs(x = '6 year Predicted PD Risk(%)', y = 'Observed Risk(%)',title = title,size = 4)+
    
    theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank())+
    #xlim(0, xmax)+
    #ylim(0, xmax)+
    theme(legend.position =  c(0.3, 0.9), legend.title = element_blank(),legend.key=element_blank(),legend.background=element_blank(),legend.spacing = unit(-0.7, "cm"),legend.text = element_text(size = 12),
          #axis colour
          axis.ticks.y.right = element_line(color = "sky blue"),
          axis.text.y.right = element_text(color = "sky blue"),
          axis.title.y.right = element_text(color = "sky blue"))#+
    
    #axis.ticks.y.left = element_line(color = "blue"),
    #axis.text.y.left = element_text(color = "blue"),
    #axis.title.y.left = element_text(color = "blue"))+
    
    #annotate("text", x = xmax, y = 4.5+offset, label = expression(bold("Risk score metrics (95% CI):")), color = "black",size = 3.5,hjust = 1, vjust = 0) +
    
    #annotate("text", x = xmax, y = 3.5+offset, label = paste0("ICI:", ici,"(",ici_CI[1],",",ici_CI[2],")"), color = "black",size = 3.5 ,hjust = 1, vjust = 0) +
    
    #annotate("text", x = xmax, y = 2.5+offset, label = paste0("C-Index:", c_index,"(",c_CI[1],",",c_CI[2],")"), color = "black",size = 3.5, hjust = 1, vjust = 0) +
    
    #annotate("text", x = xmax, y = 1.5+offset, label = paste0("R-squared:", r_squared,"(",r_CI[1],",",r_CI[2],")"), color = "black" ,size = 3.5,hjust = 1, vjust = 0) +
    #annotate("text", x = xmax, y = 0.5+offset, label = paste0("Net Benefit:", net_benefit,"(",nb_CI[1],",",nb_CI[2],")"), color = "black" ,size = 3.5,hjust = 1, vjust = 0)
  
  
  # Add grey grid to the plot
  plot <- plot #+ theme(panel.grid = element_line(color = "grey", linetype = "blank"))
  
  # Set x-axis and y-axis limits
  plot <- plot# +geom_bar(data = df, aes(x= X1),'count')# geom_histogram(fill = "blue", alpha = 0.5, bins = 10, color = "black", show.legend = FALSE,position = "identity") 
  
  #file_location <- paste0('/gpfs3/well/doherty/users/rii148/ecg_scores/cal_curves/metrics/macce/minb/covid/',title,'')
  file_location <- paste0(store_path,title)
  ggsave(paste0(file_location,'.png'), plot, width = 6, height = 5,dpi = 1200)
  #dev.off()
  
  a <- 1- predict(loess(X[,2]~X[,1]))
  png(paste0('/gpfs3/well/doherty/users/rii148/ecg_scores/cal_curves/metrics/ecg_risk_hist.png'), type = "cairo")
  #hist(X[,1], breaks = 10, col = "skyblue", xlab = "Values", ylab = "Frequency")
  hist(X[,1], breaks = seq(min(X[,1]), max(X[,1]) + 0.05, by = 0.05), col = "skyblue", xlab = "Values", ylab = "Frequency")
  dev.off()
  
  png("/gpfs3/well/doherty/users/rii148/ecg_scores/cal_curves/metrics/ecg_obs_hist.png", type = "cairo")
  
  #hist(a, breaks = 10, col = "skyblue", xlab = "Values", ylab = "Frequency")
  hist(a, breaks = seq(min(a), max(a) + 0.05, by = 0.05), col = "skyblue", xlab = "Values", ylab = "Frequency")
  dev.off()
  print("max")
  print(max(hist_data$count))
  print(sum(hist_data$count))
  return(plot)
}
