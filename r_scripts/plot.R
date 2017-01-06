#plotting
library(ggplot2)
library(dplyr)

df <- read.csv("C:/Stuff/Google Drive/Declines/df_pca.csv", 
                 header = T, stringsAsFactors = FALSE)

#  244088 

#plotting clean data,individual plots
for( i in unique(df$leaseNumber)){
  temp_data = filter(df, df$leaseNumber == i )
  #temp_data <- filter(df_gas, df_gas$leaseNumber == i )


  ggplot( aes(month_id, gas_norm), data = temp_data) + geom_point(color = "blue") + 
        labs(title = paste(temp_data$leaseNumber,"lease number", sep =" "))
  ggsave(filename=paste("C:/Stuff/Google Drive/Declines/production_lease_number",
                        i,".jpeg",sep=" "), dpi = 200)

  # print(qual$sd)

}