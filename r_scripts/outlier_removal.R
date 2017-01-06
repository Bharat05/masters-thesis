library(ggplot2)
library(reshape)

for( i in temp_data$month_id){  
  if(abs(temp_data$gas_norm[i] - fit[i]) >= .03
     ){
    temp_data$gas_norm_corrected[i] <- fit[i]
  } else {
    temp_data$gas_norm_corrected[i] <- temp_data$gas_norm[i]
  }
}
temp_data$fit <- fit

print(qplot( x =month_id, y = gas_norm,data = temp_data) +
        stat_smooth(aes(outfit=fit<<-..y..), n = nrow(temp_data), method = loess, span = 1, geom = "smooth") +
        labs(title = paste(temp_data$leaseNumber,"lease number", sep =" ")))
df_plot <- temp_data[,c(3,4,7,9)]

df_plot <- melt(df_plot, id.vars =c('month_id', 'leaseNumber'), variable.names = 'correction')
ggplot(df_plot, aes(month_id,value, color = variable)) + geom_point()

