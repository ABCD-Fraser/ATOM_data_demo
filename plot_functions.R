
library(tidyverse)
library("plotrix")
library("broom")


prep_data = function(data_f, test_phase){
  
  print(test_phase=='cont')
  
  
  if (test_phase == "test"){
      # Creates summary of RT scores on correct trials for both mean and median
      df_test_RT = data_f %>%
        group_by(participant) %>%
        filter(accuracy == 1) %>%
        summarise(mean_RT=mean(RT), se_RT=std.error(RT), median_RT=median(RT), IQR_RT=IQR(RT))
      
      # Creates summary of mean accuracy and merge with RT
      df_output = data_f %>%
        group_by(participant) %>%
        summarise(mean_accuracy=mean(accuracy), se_accuracy=std.error(accuracy), sd_accuracy=sd(accuracy)) %>% 
        merge(df_test_RT) %>% 
        arrange(participant)
    
    } else if(test_phase == "distance") {
      
      # Creates summary of RT scores on correct trials for both mean and median
      df_test_RT_distance = data_f %>%
        group_by(participant, distance) %>%
        filter(accuracy == 1) %>%
        summarise(mean_RT=mean(RT), se_RT=std.error(RT), median_RT=median(RT), IQR_RT=IQR(RT))
      
      # Creates summary of mean accuracy and merge with RT
      df_output = data_f %>%
        group_by(participant, distance) %>%
        summarise(mean_accuracy=mean(accuracy), se_accuracy=std.error(accuracy), sd_accuracy=sd(accuracy)) %>% 
        merge(df_test_RT_distance) %>% 
        arrange(participant)          
    
  } else if(test_phase == 'check') {
    
      # Aggregates the check data
      
      # Creates summary of RT scores on correct trials for both mean and median
      df_check_RT = data_f %>%
        group_by(participant) %>%
        filter(accuracy == 1) %>%
        summarise(mean_RT=mean(RT), se_RT=std.error(RT), median_RT=median(RT), IQR_RT=IQR(RT))
      
      # Creates summary of mean accuracy and merge with RT
      df_output = data_f %>%
        group_by(participant) %>%
        summarise(mean_accuracy=mean(accuracy), se_accuracy=std.error(accuracy), sd_accuracy=sd(accuracy)) %>% 
        merge(df_check_RT) %>% 
        arrange(participant)
    
    
    } else if(test_phase == 'learn_half') {
    
      # Aggregates the check data with the learn_half variable 
      
      # Creates summary of RT scores on correct trials for both mean and median
      df_check_RT_learn = data_f %>%
        group_by(participant, learn_half) %>%
        filter(accuracy == 1) %>%
        summarise(mean_RT=mean(RT), se_RT=std.error(RT), median_RT=median(RT), IQR_RT=IQR(RT))
      
      # Creates summary of mean accuracy and merge with RT
      df_output = data_f %>%
        group_by(participant, learn_half) %>%
        summarise(mean_accuracy=mean(accuracy), se_accuracy=std.error(accuracy), sd_accuracy=sd(accuracy)) %>% 
        merge(df_check_RT_learn) %>% 
        arrange(participant)
      
    } else if(test_phase == "cont") {
      
     
      
      PID_list = unique(data_f$participant)
      
      
      ov_trial_avg = NULL
      ov_trial_i = NULL
      ov_PID = NULL
      ov_trial_distance = NULL
      ov_block_i = NULL
      ov_trial_acc = NULL

      
      for (i in PID_list){
        
        temp_df = data_f %>% 
          filter(participant == i)
        
        
        
        trial_count = 0
        acc_total = 0
        trial_avg = NULL
        trial_i = NULL
        PID = NULL
        trial_distance = NULL
        block_i = NULL
        trial_acc = NULL
        
        for (row in 1:nrow(temp_df)) {
          
          PID = c(PID, temp_df[row, "participant"])
          trial_count = trial_count + 1
          acc_total = acc_total + temp_df[row, "accuracy"]
          trial_avg = c(trial_avg, acc_total/trial_count)
          trial_i = c(trial_i, temp_df[row, "trial"])
          trial_distance = c(trial_distance, temp_df[row, "distance"])
          block_i = c(block_i, temp_df[row, "block"] )
          trial_acc = c(trial_acc, temp_df[row, "accuracy"])
          
          
          
        }
        
        ov_PID = c(ov_PID, PID)
        ov_trial_avg = c(ov_trial_avg, trial_avg)
        ov_trial_i = c(ov_trial_i, trial_i)
        ov_trial_distance = c(ov_trial_distance, trial_distance)
        ov_block_i = c(ov_block_i, block_i)
        ov_trial_acc = c(ov_trial_acc, trial_acc)
        
      
      }
      
  
      df_output = do.call(rbind, Map(data.frame, PID=ov_PID, trial_avg=ov_trial_avg, trial_i=ov_trial_i, distance=ov_trial_distance, block_i=ov_block_i, trial_acc= ov_trial_acc))
    }
  
  return(df_output)
  
}


plot_check_acc = function(df){
  # Plots for the accuracy by learn_half
  ggplot(df, aes(y=mean_accuracy, x=fct_inorder(as.factor(learn_half)), fill=factor(learn_half)))+
    geom_bar(position='dodge', stat = "summary", fun.data = mean_se, width=0.8) +
    geom_errorbar(aes(ymin=mean_accuracy-se_accuracy, ymax=mean_accuracy+se_accuracy), 
                  stat="summary", fun.data = mean_se, width=.2, position=position_dodge(0.8)) +
    theme(aspect.ratio=4/3, axis.title.x = element_blank()) +
    labs(y="Mean accuracy", fill = "Learning Stage") +
    scale_fill_manual(values=c('grey72', 'grey87','grey38', 'grey53')) +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) 
  
}


plot_distance_acc = function(df){
  # Plot for mean accuracy by distance
  barcolsfull=cols=c('navy', 'darkmagenta', 'deeppink3', 'black')
  ggplot(df, aes(y=mean_accuracy, x=distance, fill=as.factor(distance)))  +
  geom_bar(aes(x=distance), position='dodge', stat = "summary", fun= "mean") +
  geom_errorbar(aes(ymin=mean_accuracy-se_accuracy, ymax=mean_accuracy+se_accuracy), stat="summary", fun.data=mean_se, width=.2, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Mean accuracy (test phase)", fill= "Distance") +
  scale_fill_manual(values=barcolsfull, labels=c('Close', 'Medium', 'Far', 'Overall'))
}

plot_block_acc = function(df, distance=FALSE) {
  
  
  
  if (distance==TRUE){
    plot_df = df %>% 
      group_by(block, distance) %>% 
      summarize(trial_avg = mean(accuracy), trial_se=std.error(accuracy))
    
    ggplot(data=plot_df, aes(x=block, y=trial_avg, color=as.factor(distance))) +
      geom_line() +
      geom_point() + 
      ggtitle("Average block score by distance") +
      xlab('trial n') + ylab('block avg score') +
      theme(plot.title = element_text(hjust=0.5))
    
  } else if (distance==FALSE) {
    
    plot_df = df %>% 
      group_by(block) %>% 
      summarize(trial_avg = mean(accuracy), trial_se=std.error(accuracy))
    
    ggplot(data=plot_df, aes(x=block, y=trial_avg)) +
      geom_line() +
      geom_point() +
      geom_errorbar(aes(ymin=trial_avg-trial_se, ymax=trial_avg+trial_se)) +
      ggtitle("Average block score") +
      xlab('trial n') + ylab('block avg score') +
      theme(plot.title = element_text(hjust=0.5))
    
  }
  
}

plot_trial_acc = function(df, distance=TRUE) {
  
  if (distance == TRUE){
    plot = df %>% 
      group_by(PID, block_i) %>% 
      summarize(trial_avg = mean(trial_acc), trial_se=std.error(trial_acc)) 
    
    
    ggplot(data=plot_1, aes(x=block_i, y=trial_avg, color=PID)) +
      geom_line() +
      ggtitle("Average block score for individual participants") +
      xlab('trial n') + ylab('block avg score') +
      theme(plot.title = element_text(hjust=0.5))
  }
}

test = function(data_f) {
  PID_list = unique(data_f$participant)
  
  
  ov_trial_avg = NULL
  ov_trial_i = NULL
  ov_PID = NULL
  ov_trial_distance = NULL
  ov_block_i = NULL
  ov_trial_acc = NULL
  
  
  for (i in PID_list){
    
    temp_df = data_f %>% 
      filter(participant == i)
    
    
    
    trial_count = 0
    acc_total = 0
    trial_avg = NULL
    trial_i = NULL
    PID = NULL
    trial_distance = NULL
    block_i = NULL
    trial_acc = NULL
    
    for (row in 1:nrow(temp_df)) {
      
      PID = c(PID, temp_df[row, "participant"])
      trial_count = trial_count + 1
      acc_total = acc_total + temp_df[row, "accuracy"]
      trial_avg = c(trial_avg, acc_total/trial_count)
      trial_i = c(trial_i, temp_df[row, "trial"])
      trial_distance = c(trial_distance, temp_df[row, "distance"])
      block_i = c(block_i, temp_df[row, "block"] )
      trial_acc = c(trial_acc, temp_df[row, "accuracy"])
      
      
      
    }
    
    ov_PID = c(ov_PID, PID)
    ov_trial_avg = c(ov_trial_avg, trial_avg)
    ov_trial_i = c(ov_trial_i, trial_i)
    ov_trial_distance = c(ov_trial_distance, trial_distance)
    ov_block_i = c(ov_block_i, block_i)
    ov_trial_acc = c(ov_trial_acc, trial_acc)
    
  }
  output_df = do.call(rbind, Map(data.frame, PID=ov_PID, trial_avg=ov_trial_avg, trial_i=ov_trial_i, distance=ov_trial_distance, block_i=ov_block_i, trial_acc= ov_trial_acc))
}