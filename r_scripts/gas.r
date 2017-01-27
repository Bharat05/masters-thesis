#clear the environment
rm(list=ls())

#clear the termininal 
cat("\014")

#lubridate package for date, works like a charm
library(lubridate)
library(dplyr)
library(ggplot2)
library(psych)
library(stats)
library(FactoMineR)
library(factoextra)
library(Rlof)

#reading in the file_gas, stringAsFactors to supress column classification 
# as factor for dates in this case

#file_gas1 <- read.csv("C:/Stuff/Google Drive/Declines/Gas1MM.csv", 
# header = T, stringsAsFactors = FALSE)
file_gas2 <- read.csv("C:/Stuff/Google Drive/Declines/Gas24MM.csv", 
                      header = T, stringsAsFactors = FALSE, nrows = 1000)
#file_gas1 <- file_gas1[,1:12]
file_gas2 <- file_gas2[,1:12]

#combining the data frames
#file_gas <- rbind(file_gas1, file_gas2)
file_gas <- file_gas2

#dataframe initializing
clean_data <- data.frame()

#date format conversion using lubridate 
file_gas$productionDate <- mdy(file_gas$productionDate)

# to force the gas production column as numeric type, 
# converting to character  data type first 
file_gas$gas <-as.numeric(as.character(file_gas$gas))

# filtering the data wrt to lease number and storing in temp_data 
for( i in unique(file_gas$leaseNumber)){
  print(i)
  temp_data <- filter(file_gas, file_gas$leaseNumber == i )
  #ignoring production before max production
  # idenitfying the row with max production
  index_max_prod = which(temp_data$gas == max(temp_data$gas, na.rm = T))
  # updating the temp_data for decline after maximum produciton only
  temp_data <- temp_data[index_max_prod:nrow(temp_data),]

  
  # normalizing to maximum production
  temp_data$gas_norm <- temp_data$gas/(max(temp_data$gas))
  
  # generating plots necessary for calculating fit through stat_smooth
  # using print to see the plots in window
  print(qplot( productionDate, gas_norm,data = temp_data) +
    stat_smooth(aes(outfit=fit<<-..y..), n = nrow(temp_data), method = loess, span = .2) +
    labs(title = paste(temp_data$leaseNumber,"lease number", sep =" ")))
  
  quality_index <-  describe(fit - temp_data$gas_norm)
  
  
  # to catch the error in standard deviation and jump to next lease no
  # storing the relativey good production data in clean_data , using standard deviation of 
  tryCatch (if(quality_index$sd < .01) clean_data <- rbind(clean_data,temp_data),
    #print(qual$sd)
    finally = next )
    
  
  #print(fit)
  #scatter plot with line, pay attention to geom_smooth
  # print(temp_data %>% 
  #         ggvis( x = ~productionDate, y = ~gas) %>% 
  #         layer_points())
  # print(ggplot(temp_data, aes(temp_data$productionDate, temp_data$gas_norm))
  #       +geom_point(na.rm = TRUE)  # + stat_smooth(sifitze =1, n = 150, span = .8) 
  #       + stat_smooth(aes(outfit=fit<<- ..temp_data$gas_norm..)) +
  #         labs(title = paste(temp_data$leaseNumber,"lease number", sep =" ")) )
                                    # n = 150, level = .8))
  #geom_smooth(method = 'loess', span = ., level = .6))
  #other plotting funciton, works much faster than ggplot
  #plot(temp_data$productionDate, temp_data$gas)
  #fit
  
}
write.csv(clean_data, "C:/Stuff/Google Drive/Declines/clean_data.csv")

# plotting clean data,individual plots
# for( i in unique(clean_data$leaseNumber)){
#   temp_data= filter(clean_data, clean_data$leaseNumber == i )
#   #temp_data <- filter(file_gas, file_gas$leaseNumber == i )
#   
# 
#   print(qplot( productionDate, gas_norm,data = temp_data) +
#           stat_smooth(aes(outfit=fit<<-..y..), n = nrow(temp_data)) + 
#           labs(title = paste(temp_data$leaseNumber,"lease number", sep =" ")) )
#   
#   # print(qual$sd)
# 
# }

clean_data_long_prod <- data.frame()
#production more than 48 months
for( i in unique(clean_data$leaseNumber)){
  temp_data <- filter(clean_data, clean_data$leaseNumber == i )
  #print(c(nrow(temp_data), "no of rows"))
  if( (nrow(temp_data) >= 48)) {
    clean_data_long_prod <- rbind(clean_data_long_prod,temp_data)
    # print(c(nrow(temp_data), 'only long > 48')) 
  }
}

#extract production data for same time period, 48 months in this case
clip_data <- data.frame()
for( i in unique(clean_data_long_prod$leaseNumber)){
  temp_data <- filter(clean_data_long_prod, clean_data_long_prod$leaseNumber == i )
  temp_data <- temp_data[1:48,]
  clip_data <- rbind(clip_data, temp_data)  
}

write.csv(clip_data, "C:/Stuff/Google Drive/Declines/clip_data.csv")


#add month_id to plot

clip_data_with_month_id <-data.frame()
for( i in unique(clip_data$leaseNumber)){
  temp_data <- filter(clip_data, clip_data$leaseNumber == i )
  month_id <- 1:48
  temp_data <- cbind(temp_data,month_id)
  #print(temp_data)
  clip_data_with_month_id <- rbind(clip_data_with_month_id,temp_data)
}
write.csv(clip_data_with_month_id, "C:/Stuff/Google Drive/Declines/clip_data_with_month_id.csv")


#adding id to plot; unique for Lease Number

clip_data_with_month_id <- read.csv("C:/Stuff/Google Drive/Declines/clip_data_with_month_id.csv",  
                                      header = T, stringsAsFactors = FALSE)
df_pca <- data.frame()
j = 1
for( i in unique(clip_data_with_month_id$leaseNumber)){
  temp_data <- filter(clip_data_with_month_id, clip_data_with_month_id$leaseNumber == i )
  id <- rep(j,48)
  j <- j+1
 
  temp_data <- cbind(temp_data,id)
  count = 0
  for( k in temp_data$month_id){
  # remove production data with more than four months  zero (.01 gas norm) production
  # incorporate this for only late time month id greater than 30
    if((temp_data$gas_norm[k] -.01) < 0){
      print(temp_data$gas_norm[k])
      count <- count + 1
    }
  }
  if(count >= 4){ 
    #print(count)
    next
    
  }
  else{
    df_pca <- rbind(df_pca,temp_data)
  }
}
#remove columns which are not required
df_pca <- df_pca[,c(-1,-2)]
write.csv(df_pca, "C:/Stuff/Google Drive/Declines/df_pca.csv")


#plotting all curves see the use of colorfunction and scale_color_gradientn
# colfunc using colorRampPallette
colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))
ggplot(data = sample, aes(x = month_id, y= gas_norm, color = id)) +geom_point() + 
  stat_smooth(method = loess)

ggplot(data = df_pca, aes(x = month_id, y= gas_norm, color = id)) + geom_point(alpha = .2) + 
  scale_colour_gradientn(colors = colfunc(length(id)))

#data frame with just lease id, month_id and gas_norm
df_pca_req <- df_pca[,c(2,3,6)]

##reshape to get the wide form for PCA
df_pca_ready <- reshape(df_pca_req, timevar ='month_id', idvar ="leaseNumber" ,direction = "wide")
write.csv(df_pca_ready, "C:/Stuff/Google Drive/Declines/df_pca_ready.csv")

##Doing PCA
res.pca2 <- prcomp(df_pca_ready[,c(-1,-2,-3)])
res.pca1 <- prcomp(df_pca_ready[,c(-1,-2)])
eig <- (res.pca1$sdev)^2

variance <- eig*100/sum(eig)

cumvar <- cumsum(variance)

eig.prod.active <- data.frame(eig = eig, variance = variance, cumvariance = cumvar)

head(eig.prod.active)

pca_result_df1 <- data.frame(unclass(res.pca1$rotation[,1:3]))
pca_result_df2 <- data.frame(unclass(res.pca2$rotation[,1:3]))

ggplot(data=pca_result_df1, aes(x=1:48, y = pca_result_df1$PC1)) + geom_point()
ggplot(data=pca_result_df2, aes(x=1:47, y = pca_result_df2$PC1)) + geom_point()
var <- get_pca_var(res.pca)


## PCA Predicition
# doesnt include 1st month production
mat_pca <- pca_result_df[1:8,1:8]
## any random decline , first 4 months since 4 unknowns
b <- t(df_pca_ready[60,4:50])
## get the coefficients
c <- solve(mat_pca,b[1:8])

pca_predict <- pca_result_df$PC1*c[1] + pca_result_df$PC2*c[2]+
  pca_result_df$PC3*c[3] + pca_result_df$PC4*c[4] + pca_result_df$PC5*c[5] +
  pca_result_df$PC6*c[6] + pca_result_df$PC7*c[7] + pca_result_df$PC8*c[8]

plot(1:47, b)
plot(1:47, pca_predict)

# plot to see the correlation betwwen pca and vairable
plot(res.pca1$x[,1], df_pca_ready$gas_norm.11)
