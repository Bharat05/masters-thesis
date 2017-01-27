library(ggplot2)
library(reshape)
df <- read.csv("C:/Stuff/Google Drive/Declines/df_pca.csv",
                      header = T, stringsAsFactors = FALSE)
df_outliers_removed <- data.frame()

#filter and storing individual production data in temp_data
for( i in unique(df$leaseNumber)){
  temp_data <- filter(df, df$leaseNumber == i )
  print(qplot( x = month_id, y = gas_norm,data = temp_data) +
          stat_smooth(aes(outfit=fit<<-..y..), n = nrow(temp_data), method = loess, span = 1, geom = "smooth") +
          labs(title = paste(temp_data$leaseNumber,"lease number", sep =" ")))
  for( j in temp_data$month_id){  
    if(abs(temp_data$gas_norm[j] - fit[j]) >= .03
       ){
      temp_data$gas_norm_corrected[j] <- fit[j]
    } else {
      temp_data$gas_norm_corrected[j] <- temp_data$gas_norm[j]
    }
  }
  temp_data$fit <- fit
  df_outliers_removed <- rbind(df_outliers_removed, temp_data)
}



df_plot <- temp_data[,c(3,4,7,9)]

df_plot <- melt(df_plot, id.vars =c('month_id', 'leaseNumber'), variable.names = 'correction')
ggplot(df_plot, aes(month_id,value, color = variable)) + geom_point()

