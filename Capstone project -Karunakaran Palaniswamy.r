#-----------------------------------------------------------------------------
# Capstone project
# Title: Predict Retail CUSTOMER BEHAVIOR
# Student: Karunakaran Palaniswamy
#-----------------------------------------------------------------------------

cat("\014")
rm=list=ls(all.names = TRUE)

library(readxl)
library(ggplot2)
library(lubridate)
library(gmodels)
library(MASS)
library(dplyr)
library(plotly)
library(caret)
library(forecast)  
library(gbm)
library(ISLR)
library(MASS)
library(tree)
library(boot)
library(data.table)
library(fastDummies)
library(dummies)
library(tibble)
library(psych)
library(h2)
library(klaR)
library(factoextra)
library(NbClust)

# Input file folder - Set the Working directory
setwd("")

#-----------------------------------------------------------------------------------------------------------------
#                                  PART I - BASIC OPERATIONS
#-----------------------------------------------------------------------------------------------------------------REVENUE 

#Read the input file
Sales_Data <- read_xlsx("online_retail_II.xlsx",sheet = "Year 2010-2011")

# Change the column name from 'Customer ID' to Customer_ID
Sales_Data <- Sales_Data %>% dplyr::rename(Customer_ID = 'Customer ID')

# Display the structure of the data to identify numeric, character, 
# text fields and do the necessary conversion
str(Sales_Data)   # There are 8 variables overall 

# Display the top 5 rows
head(Sales_Data)

# Display the bottom 5 rows
tail(Sales_Data)

#--------------------------------------------------------------------------------------------
# Find the NA's in the data and omit them from the analysis
#--------------------------------------------------------------------------------------------
Sales_Data %>% summarise(Invoice_na = sum(is.na(Invoice)),
                         StockCode_na = sum(is.na(StockCode)),
                         Description_na = sum(is.na(Description)),
                         Quanitity_na = sum(is.na(Quantity)),
                         InvoiceDate_na = sum(is.na(InvoiceDate)),
                         Price_na = sum(is.na(Price)),
                         Customer_ID = sum(is.na(Customer_ID)),
                         Country = sum(is.na(Country))
)


# Remove these rows from further processing
New_Sales_Data <- na.omit(Sales_Data)
  
# Create one column YearMonth for the visualization steps
New_Sales_Data$InvoiceDate1 <- as.Date(New_Sales_Data$InvoiceDate)


# Need to create variables stating the month of each observation.
New_Sales_Data$Invoice_yearmonth <- as.Date(cut(New_Sales_Data$InvoiceDate1, breaks = "month"))

#-----------------------------------------------------------------------------------------------------------------
#                                  PART II - Retail Performance KPIs
#-----------------------------------------------------------------------------------------------------------------REVENUE 
#------------------------------------------------------------------------------------------------------
#                                    REVENUE
#------------------------------------------------------------------------------------------------------

# Calculate the Reveneue = # Active Customer Count * Order Count * Average Revenue per Order

# Calculate the revenue
New_Sales_Data$Revenue <- New_Sales_Data$Quantity*New_Sales_Data$Price


Sales_Revenue <- New_Sales_Data %>% 
  group_by(Invoice_yearmonth) %>% 
  summarise(Total_Revenue = sum(Revenue))

#-------------------------------------------------
# Revenue -  Visualization - Draw the scatter plot
#-------------------------------------------------
#Convert Invoice_yearmonth to numeric to allow scatter plot to draw lines. If it is character, 
# lines wont be drawn.

ggplot(data=Sales_Revenue, aes(x=Invoice_yearmonth,y=Total_Revenue)) +
         geom_line(aes(group=1),color="blue") +
         geom_point(shape=21, color = "black", fill = "red", size=2) +
         labs(x= "Invoice period") + labs(y = "Revenue") +
         ggtitle("Revenue for Dec 2010 to Dec 2011")
 #    main = "Revenue over months")
 #    cex=1.2, pch=18,col="blue")

# Conclusion:
# This clearly shows our revenue is growing over the period but there is a drop in Apr 2011 and also in June


#------------------------------------------------------------------------------------------------------
#                                    Monthly Revenue Growth Rate
#------------------------------------------------------------------------------------------------------
# Calculate the Revenue percent change
Sales_Revenue$Revenue_pct_change <- (Sales_Revenue$Total_Revenue/lag(Sales_Revenue$Total_Revenue, 1) - 1) * 100
#Sales_Revenue$Revenue_pct_change <- data.frame(lapply(Sales_Revenue, function(x) as.numeric(sub("%","",Revenue_pct_change)))
                                               
  ggplot(data=Sales_Revenue, aes(x=Invoice_yearmonth,y=Revenue_pct_change)) +
  geom_line(aes(group=1),color="blue") +
  geom_point(shape=21, color = "black", fill = "red", size=2) +
  labs(x= "Invoice period") + labs(y = "Revenue % Change") +
  ggtitle("Monthly Revenue Growth Rate for Dec 2010 to Dec 2011")

#Conclusion:
# we see a average to good growth between Jan and Mar'11 but need to identify what exactly happened on April. 
# Was it due to less active customers or our customers did less orders? Maybe they just started to buy 
# cheaper products? We can't say anything without doing a deep-dive analysis.

#------------------------------------------------------------------------------------------------------
#                                    Monthly Active Customers
#------------------------------------------------------------------------------------------------------
#Since UK has maximum no. of rows, filter data belongs to UK for further analysis
#Need to identify Monthly active customers and their behavior. That will tell us why there is a sales dip 
# in April
#------------------------------------------------------------------------------------------------------
Sales_UK_Data <-  New_Sales_Data %>%subset(Country == "United Kingdom") 
                                 
Sales_UK_Active_Customers <-  Sales_UK_Data %>% 
                              dplyr::select(Country, Invoice_yearmonth, Customer_ID) %>%
                              group_by(Invoice_yearmonth)  %>%
                             summarise(Active_Customers = n()) 

ggplot(data=Sales_UK_Active_Customers, aes(x=Invoice_yearmonth,y=Active_Customers)) +
  geom_bar(stat = "identity", color="blue") +
 labs(x= "Invoice period") + labs(y = "Active Customers") +
  ggtitle("Monthly Active Customers for Dec 2010 to Dec 2011")
# View(Sales_UK_Active_Customers)
# Conclusion
# in Apr 2011, no. of customers fell 13% [24,587 to 21,358]

#------------------------------------------------------------------------------------------------------
#                                 Monthly Orders (or Purchase Quantity)  
#------------------------------------------------------------------------------------------------------
# Use Quantity data to calculate monthly orders using United Kingdom data
Sales_monthly <- Sales_UK_Data %>%
                 select(Country, Invoice_yearmonth, Quantity) %>%
                 group_by(Invoice_yearmonth) %>%
  
                 summarise(M_Quantity = sum(Quantity))
ggplot(data=Sales_monthly, aes(x=Invoice_yearmonth,y=M_Quantity)) +
  geom_bar(stat = "identity", color="green") +
  labs(x= "Invoice period") + labs(y = "Order Quantity") +
  ggtitle("Monthly orders(Quantity) for Dec 2010 to Dec 2011")

#Conclusion
#Between March and April 2011, the total quantity has come down from 272,305 to 247,915, that is almost 9%
#This might be the impact of decrease in Active Customer Count 

#------------------------------------------------------------------------------------------------------
#                                Average Revenue per Order 
#------------------------------------------------------------------------------------------------------
# Calculate the average revenue for every order (i.e Basket size)
Sales_average_Revenue <- Sales_UK_Data %>%
  dplyr::select(Country, Invoice_yearmonth, Revenue) %>%
  group_by(Invoice_yearmonth) %>%
  summarise(M_Revenue = mean(Revenue))

ggplot(data=Sales_average_Revenue, aes(x=Invoice_yearmonth,y=M_Revenue)) +
  geom_bar(stat = "identity", color="green") +
  labs(x= "Invoice period") + labs(y = "Average Revenue") +
  ggtitle("Average Revenue per Order")

# Conclusion
# in the above, the average revenue has come down (from 19.26 to 18.58) due to the reduction in 
# purchase quantity and active customers. Here we are seeing slowdown in the overall retail. 

#------------------------------------------------------------------------------------------------------
#                                 New/Existing Customer Ratio
#------------------------------------------------------------------------------------------------------
# New customer is whoever did his/her first purchase in the time window we define

# Create a dataframe with the Customer ID and first Purchase date
Sales_First_Purchase <- Sales_UK_Data %>%
                      select(Customer_ID, InvoiceDate)  %>%
                      group_by(Customer_ID) %>% summarise(First_PurchaseDt = min(InvoiceDate))
                
# Convert to date format                      
Sales_First_Purchase$First_Purchase_Yearmon <- as.Date(cut(as.Date(Sales_First_Purchase$First_PurchaseDt), breaks = "month"))

# Merge the above dataframe with Sales_UK_Data using Customer ID as KEY
Sales_UK_Data <- merge(Sales_UK_Data, Sales_First_Purchase, by = "Customer_ID")

# Create a column User Type and assign the below 
#     - "Existing" - if the first purchase YearMonth is before the selected Invoice Year Month
#     - Otherwise "New"
Sales_UK_Data$UserType <- 'New'
Sales_UK_Data$UserType[Sales_UK_Data$Invoice_yearmonth > Sales_UK_Data$First_Purchase_Yearmon] <- "Existing"

# Calculate the Revenue per month for each User Type
UserType_Revenue <- Sales_UK_Data %>% 
                   select(Invoice_yearmonth, UserType, Revenue) %>%
                   group_by(Invoice_yearmonth, UserType) %>% 
                  summarise(UserType_Revenue1 = sum(Revenue))

# Remove data which doesn't belong to one single year. 
# In our case, remove 2010 data as we have only Dec'10 data. Keep only 2011 data
UserType_Revenue <- subset(UserType_Revenue, Invoice_yearmonth != '2010-12-01')

ggplot(data=UserType_Revenue, aes(x=Invoice_yearmonth, y = UserType_Revenue1)) +
            geom_line(aes(group = UserType_Revenue$UserType, color = UserType_Revenue$UserType), stat = "identity") +
            geom_point() +
            scale_color_manual(labels = c("New", "Existing"), values = c("purple", "orange")) +
            labs(title = "New-Existing Customer Ratio for 2011", x= "Invoice period", y = "Revenue", color = " \n") 
# Conclusion:
# Both New and Existing customers slight negative trend in Apr 2011           

#------------------------------------------------------------------------------------------------------
#                                 Monthly Retention Rate
#------------------------------------------------------------------------------------------------------
#   - Retention rate indicates how good is the service and how well the product fits the market
#   - Need to calculate how many customers retained from previous month 
#Monthly Retention Rate = Retained Customers From Prev. Month/Active Customers Total
#
# Identify which customer are active by looking at their revenue per month
Monthly_Revenue_customer = Sales_UK_Data %>%
                           select(Customer_ID, Invoice_yearmonth, Revenue) %>%
                           group_by(Customer_ID, Invoice_yearmonth) %>%
                          summarise(Monthly_Revenue_customerID = sum(Revenue))


# Retention table shows us which customers are active on each month (1 stands for active).
Revenue_Crosstab <- xtabs(~ Customer_ID+Invoice_yearmonth, data = Monthly_Revenue_customer)

#create an array of dictionary which keeps Retained & Total User count for each month
#retention_array <- array()



#*****************************************************************************************************************
#                                  PART III - CUSTOMER SEGMENTATION 
#*****************************************************************************************************************
#-----------------------------------------------------------------------------------------------------------------
#                                  RECENCY, FREQUENCY, and MONETARY 
#-----------------------------------------------------------------------------------------------------------------
# Why Segmentation is necessary?
#       can't treat every customer the same way with the same content, same channel, same importance. 
#       They will find another option which understands them better. The actions should cater to their needs.
#        To increase the customer retention, we can do the segmentation based on churn probability and take actions
#         One of the methods is Recency Frequency Monetary (RFM)
#----------------------------------------------------------------------------------------------------------------
# Recency Frequency Monetary (RFM) - the segments will be:
#        1) Low Value: Customers who are less active than others, not very frequent buyer/visitor and generates 
#           very low - zero - maybe negative revenue.
#        2) Mid Value: In the middle of everything. Often using our platform (but not as much as our High Values), 
#           fairly frequent and generates moderate revenue.
#        3) High Value: The group we don't want to lose. High Revenue, Frequency and low Inactivity.
#----------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
#                                  RECENCY
#-----------------------------------------------------------------------------------------------------------------
# Recency - To calculate recency, we need to find out most recent purchase date of each customer and see how many 
# days they are inactive for. After having no. of inactive days for each customer, we will apply K-means* clustering 
# to assign customers a recency score.
#----------------------------------------------------------------------------------------------------------------
# Create a generic user dataframe to keep CustomerID and new segmentation scores
Segment_Customer = Sales_UK_Data %>%  distinct(Customer_ID) 

# Get the max purchase date for each customer and create a dataframe with it
Segment_last_PurchaseDt = Sales_UK_Data %>% 
                        select(Customer_ID, InvoiceDate1) %>%
                        group_by(Customer_ID)  %>%
                        summarise(Last_PurchaseDt = max(InvoiceDate1))
Segment_last_PurchaseDt$Recency = as.numeric(max(Segment_last_PurchaseDt$Last_PurchaseDt) - Segment_last_PurchaseDt$Last_PurchaseDt)

#Merge both data
Segment_Customer <- merge(Segment_Customer, Segment_last_PurchaseDt, by = "Customer_ID")

# Plot the recency diagram
ggplot(Segment_Customer,aes(Recency)) +   
#       geom_histogram(breaks = seq(0,400, by=2), fill="green",col="red") + 
     geom_histogram(fill="green",col="red") + 
       labs(title="Recency", x="Recency", y="Count") 
  
# Call fviz_nbclust with different methods to get the best cluster number
Segment_recency_k <- as.data.frame(Segment_Customer[,c("Recency")])

# Determine number of clusters using Elbow, Silhouette, Gap Statistic methods  
# Suggesting 4 clusters
fviz_nbclust(Segment_recency_k, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method - Suggesting 2 clusters
fviz_nbclust(Segment_recency_k, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic - nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for the analysis.
# Use verbose = FALSE to hide computing progression.
# Suggesting 2 clusters
set.seed(123)
fviz_nbclust(Segment_recency_k, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")


# Use No. of clusters: 4 as per Elbow method's recommendation
pc_cluster <-kmeans(Segment_Customer[,c("Recency")], 4)
Segment_Customer$Rec_cluster <- as.numeric(pc_cluster$cluster)
      

# Ordering  the cluster numbers based on Recency Mean     
#K-means assigns clusters as numbers but not in an ordered way. We can't say cluster 0 is the worst
#and cluster 4 is the best. order_cluster() method does this for us and our new dataframe looks much neater:
  df_recency <- Segment_Customer %>% 
                 select(Customer_ID, Recency,Rec_cluster) %>%
                 group_by(Rec_cluster)  %>% 
               summarise(Recency_mean = mean(Recency)) %>% ungroup()
  df_recency <- df_recency %>%
                 arrange(desc(Recency_mean)) 
  
   df_recency$New_index <- rownames(df_recency)
   Segment_Customer <- merge(Segment_Customer, df_recency, by = "Rec_cluster")
#  Drop the original Recency_cluster   
   Segment_Customer <- subset(Segment_Customer, select = -c(Rec_cluster))
#  Change the column name from 'New_Index' to Recency_Cluster
   Segment_Customer <- Segment_Customer %>% dplyr::rename(Rec_cluster = 'New_index') 
   
   Segment_Customer$Rec_cluster <- as.numeric(Segment_Customer$Rec_cluster)
   
# Display the summary for Recency   
 Segment_Customer %>% group_by(Rec_cluster) %>% summary(Recency)
 

#--------------------------------------------------------------------------------------------------------
#                                   F R E Q U E N C Y
#--------------------------------------------------------------------------------------------------------        
# To create frequency clusters, we need to find total number orders for each customer. 
#--------------------------------------------------------------------------------------------------------

# Get the max purchase date for each customer and create a dataframe with it
Segment_frequency = Sales_UK_Data %>% 
                   select(Customer_ID, InvoiceDate1) %>%
                   group_by(Customer_ID)  %>% 
                   summarise(Frequency = n())
        
#Merge both data
Segment_Customer <- merge(Segment_Customer, Segment_frequency, by = "Customer_ID")
        
# Plot the Frequency diagram
ggplot(subset(Segment_Customer, Frequency < 1000),aes(Frequency)) +   
             geom_histogram(fill="green",col="red",binwidth = 20) + 
             labs(title="Frequency", x="Frequency", y="Count")
        
        

# Use No. of clusters: 4 as per Elbow method's recommendation
pc_cluster <-kmeans(Segment_Customer[,c("Frequency")], 4)
Segment_Customer$Frequency_cluster <- as.numeric(pc_cluster$cluster)
        
        
df_frequency <- Segment_Customer %>% 
              select(Frequency,Frequency_cluster) %>%
             group_by(Frequency_cluster)  %>% 
             summarise(Freq_mean = mean(Frequency)) %>% ungroup()
        
df_frequency <- df_frequency %>%
              arrange(desc(Freq_mean)) 
             df_frequency$New_index <- rownames(df_frequency)
        
Segment_Customer <- merge(Segment_Customer, df_frequency, by = "Frequency_cluster")
#  Drop the original Frequency_cluster   
Segment_Customer <- subset(Segment_Customer, select = -c(Frequency_cluster))
#  Change the column name from 'New_Index' to Recency_Cluster
Segment_Customer <- Segment_Customer %>% dplyr::rename(Frequency_cluster = 'New_index')
Segment_Customer$Frequency_cluster <- as.numeric(Segment_Customer$Frequency_cluster)

Segment_Customer %>% group_by(Frequency_cluster) %>%  describe(Segment_Customer$Frequency)

 #--------------------------------------------------------------------------------------------------------
 #                                   Monetary (Revenue)
 #--------------------------------------------------------------------------------------------------------        
 # To create monetary clusters, calculate revenue for each customer, plot a histogram and apply the same 
 #      clustering method 
 #--------------------------------------------------------------------------------------------------------
 # Calculate the revenue for each customer
 Sales_UK_Data$Revenue <- Sales_UK_Data$Quantity*Sales_UK_Data$Price
 # Segment_Customer <- subset(Segment_Customer, select = -c(Rev_cluster, Rev_mean.y))
 Segment_monetary <- Sales_UK_Data %>%
                     select(Customer_ID, Revenue) %>%
                     group_by(Customer_ID) %>%
                     summarise(Revenue = sum(Revenue))
        
 #Merge both data
 Segment_Customer <- merge(Segment_Customer, Segment_monetary, by = "Customer_ID")
        
 # Plot the recency diagram
 ggplot(subset(Segment_Customer, Revenue > 0 & Revenue <=  10000 ),aes(Revenue)) +   
           geom_histogram(fill="green",col="red",binwidth = 20) + 
           labs(title="Revenue", x="Revenue", y="Count")
        

   
#From the graph, you can see the optimal k is 3, where the curve is starting to have a diminishing 
#return. Once you have our optimal k, you re-run the algorithm with k equals to 4 and evaluate the clusters.
pc_cluster <-kmeans(Segment_monetary, 4)
Segment_Customer$Rev_cluster <- as.numeric(pc_cluster$cluster)


df_monetary <- Segment_Customer %>% 
               select(Revenue,Rev_cluster) %>%
               group_by(Rev_cluster)  %>% 
               summarise(Rev_mean = mean(Revenue)) %>% ungroup()
df_monetary <- df_monetary %>%
               arrange(desc(Rev_mean)) 

df_monetary$New_index <- rownames(df_monetary)

Segment_Customer <- merge(Segment_Customer, df_monetary, by = "Rev_cluster")

#  Drop the original Recency_cluster   
Segment_Customer <- subset(Segment_Customer, select = -c(Rev_cluster))
#  Change the column name from 'New_Index' to Rev_Cluster
Segment_Customer <- Segment_Customer %>% dplyr::rename(Rev_cluster = 'New_index') 
Segment_Customer$Rev_cluster <- as.numeric(Segment_Customer$Rev_cluster)
Segment_Customer %>% group_by(Rev_cluster) %>% summary(Revenue)


#------------------------------------------------------------------------------------------------------------------
#                   Recency, frequency & revenue.
# Let's create an overall score out of them:
#------------------------------------------------------------------------------------------------------------------
Segment_Customer$Overall_score <- Segment_Customer$Rec_cluster + 
                              Segment_Customer$Frequency_cluster +
                              Segment_Customer$Rev_cluster
set.seed(123)
# Get the mean for Recency, Frequency and Revenue

Segment_Customer %>% 
  arrange(desc(Recency)) %>%
  select_at(vars(Overall_score, Recency, Frequency, Revenue)) %>%
  group_by(Overall_score) %>%
  summarise_all(c("mean")) 
#-------------------------------------------------------------------------------------------
# Couldn't find a better R statement to display the below in Overall score and Recency order.
# EVery time it is giving different results
#-----------------------------------------------------------------------------------------
#Overall_score Recency Frequency Revenue
#
#1       6     4        5128    57121.
#2       7    1.33      3282.   54201.
#3       8    286.        65.5   3629.
#4       9    214.        86.7   2037.
#5      10   97.9       98.4     1630.
#6      11   38.1       90.6     1354.
#7      12   20.3       66.0     1093.  

# The scoring above clearly shows us that customers with score 8 & 9 are our best customers 
# whereas 6, 7 and 12are the worst.To keep things simple, better we name these scores:

# To keep things simple, better we name these scores:
#       6 & 7       : Low Value
#       8, 9, and 10: High Value
#       11 & 12     : Mid Value
Segment_Customer$Segment <- 'Low Value'
Segment_Customer$Segment[between(Segment_Customer$Overall_score,11,12)] <- 'Mid-Value' 
Segment_Customer$Segment[between(Segment_Customer$Overall_score,8,10)] <- 'High-Value' 

#*****************************************************************************************************************
#                                  PART IV - LIFETIME VALUE PREDICTION (LTV)
#*****************************************************************************************************************
#-----------------------------------------------------------------------------------------------------------------
# Lifetime Value Prediction (LTV)
#-------------------------------------------------------------------------------------------------------- 
# Define an appropriate time frame for Customer Lifetime Value calculation

# To implement it correctly, we need to split our dataset. 
#       We will take 3 months (Mar-Apr-May) of data, calculate RFM and use it for predicting next 6 (Jun-Nov) months.
#       So we need to create two dataframes first and append RFM scores to them.

# Use UK dataset for the split
Sales_UK_3Mon <- Sales_UK_Data %>% subset(InvoiceDate1 >= "2011-03-01" & InvoiceDate1 < "2011-06-01" )
Sales_UK_6Mon <- Sales_UK_Data %>% subset(InvoiceDate1 >= "2011-06-01" & InvoiceDate1 < "2011-12-01" )

Sales_UK_3Mon_Cluster <- Sales_UK_3Mon %>% distinct(Customer_ID)


#--------------------------------------------------------------------------------------------------

# Calculate the Recency using 3 months data
# Get the max purchase date for each customer and create a dataframe with it
Sales_UK_3Mon_summary = Sales_UK_3Mon %>% 
                        select(Customer_ID, InvoiceDate1) %>%
                        group_by(Customer_ID)  %>%
                        summarise(Last_PurchaseDt = max(InvoiceDate1))
Sales_UK_3Mon_summary$Recency = as.numeric(max(Sales_UK_3Mon_summary$Last_PurchaseDt) -  +
                                             Sales_UK_3Mon_summary$Last_PurchaseDt)

#Merge both data
Sales_UK_3Mon <- merge(Sales_UK_3Mon, Sales_UK_3Mon_summary, by = "Customer_ID")

# Plot the recency diagram
ggplot(Sales_UK_3Mon_summary,aes(Recency)) +   
           geom_histogram(fill="green",col="red") + 
          labs(title="Recency data for Mar-Jun'11 - United Kingdom", x="Recency", y="Count") 

# Kmeans clustering
pc_cluster <-kmeans(Sales_UK_3Mon_summary[,c("Recency")], 4)
Sales_UK_3Mon_summary$Rec_cluster <- as.numeric(pc_cluster$cluster)

# Call the Order clustering function
#Sales_UK_3Mon_summary <-  Order_Cluster("Recency_cluster", "Recency", Sales_UK_3Mon_summary)
df_new <- Sales_UK_3Mon_summary %>% 
          select(Recency,Rec_cluster) %>%
          group_by(Rec_cluster)  %>% 
          summarise(Rec_mean = mean(Recency)) %>% ungroup()
df_new <- df_new %>%
          arrange(desc(Rec_mean)) 

df_new$New_index <- rownames(df_new)

Sales_UK_3Mon_summary <- merge(Sales_UK_3Mon_summary, df_new, by = "Rec_cluster")

#  Drop the original Recency_cluster   
Sales_UK_3Mon_summary <- subset(Sales_UK_3Mon_summary, select = -c(Rec_cluster))
#  Change the column name from 'New_Index' to Rec_Cluster
Sales_UK_3Mon_summary <- Sales_UK_3Mon_summary %>% dplyr::rename(Rec_cluster = 'New_index') 
Sales_UK_3Mon_summary$Rec_cluster <- as.numeric(Sales_UK_3Mon_summary$Rec_cluster)

 Sales_UK_3Mon_summary %>% group_by(Rec_cluster) %>% summary(Recency)
remove(df_new)

#-----------------------------------------------------------------------------------------------
# Frequency
#-----------------------------------------------------------------------------------------------
# Get the max purchase date for each customer and create a dataframe with it
Segment_frequency = Sales_UK_3Mon %>% 
                    select(Customer_ID, InvoiceDate1) %>%
                    group_by(Customer_ID)  %>% 
                    summarise(Frequency = n())

#Merge both data
Sales_UK_3Mon_summary <- merge(Sales_UK_3Mon_summary, Segment_frequency, by = "Customer_ID")

# Plot the recency diagram
ggplot(subset(Sales_UK_3Mon_summary, Frequency < 1000),aes(Frequency)) +   
        geom_histogram(fill="green",col="red",binwidth = 20) + 
         labs(title="Frequency", x="Frequency", y="Count")


#From the graph, you can see the optimal k is 3, where the curve is starting to have a diminishing 
#return. Once you have our optimal k, you re-run the algorithm with k equals to 4 and evaluate the clusters.
pc_cluster <-kmeans(Sales_UK_3Mon_summary[,c("Frequency")], 4)
Sales_UK_3Mon_summary$Freq_cluster <- as.numeric(pc_cluster$cluster)


# Call the Order clustering function

df_new <- Sales_UK_3Mon_summary %>% 
          select(Frequency,Freq_cluster) %>%
          group_by(Freq_cluster)  %>% 
          summarise(Freq_mean = mean(Frequency)) %>% ungroup()
df_new <- df_new %>%
          arrange(desc(Freq_mean)) 

df_new$New_index <- rownames(df_new)

Sales_UK_3Mon_summary <- merge(Sales_UK_3Mon_summary, df_new, by = "Freq_cluster")

#  Drop the original Frequency_cluster   
Sales_UK_3Mon_summary <- subset(Sales_UK_3Mon_summary, select = -c(Freq_cluster))
#  Change the column name from 'New_Index' to Freq_Cluster
Sales_UK_3Mon_summary <- Sales_UK_3Mon_summary %>% dplyr::rename(Freq_cluster = 'New_index') 
Sales_UK_3Mon_summary$Freq_cluster <- as.numeric(Sales_UK_3Mon_summary$Freq_cluster)

Sales_UK_3Mon_summary %>% group_by(Freq_cluster) %>% summary(Frequency)
remove(df_new)


#-----------------------------------------------------------------------------------------------
# Revenue
#-----------------------------------------------------------------------------------------------
# Calculate the revenue for each customer
Sales_UK_3Mon$Revenue <- Sales_UK_3Mon$Quantity*Sales_UK_3Mon$Price
Segment_Revenue<- Sales_UK_3Mon %>%
                  select(Customer_ID, Revenue) %>%
                  group_by(Customer_ID) %>%
                  summarise(Revenue = sum(Revenue))

#Merge both data
Sales_UK_3Mon_summary <- merge(Sales_UK_3Mon_summary, Segment_Revenue, by = "Customer_ID")

# Plot the recency diagram
ggplot(subset(Sales_UK_3Mon_summary, Revenue > 0 & Revenue <=  10000 ),aes(Revenue)) +   
              geom_histogram(fill="green",col="red",binwidth = 20) + 
              labs(title="Revenue", x="Revenue", y="Count")

#From the graph, you can see the optimal k is 3, where the curve is starting to have a diminishing 
#return. Once you have our optimal k, you re-run the algorithm with k equals to 4 and evaluate the clusters.
pc_cluster <-kmeans(Sales_UK_3Mon_summary[,c("Revenue")], 4)
Sales_UK_3Mon_summary$Rev_cluster <- as.numeric(pc_cluster$cluster)

df_new <- Sales_UK_3Mon_summary %>% 
          select(Revenue,Rev_cluster) %>%
          group_by(Rev_cluster)  %>% 
          summarise(Rev_mean = mean(Revenue)) %>% ungroup()
df_new <- df_new %>%
          arrange(desc(Rev_mean)) 

df_new$New_index <- rownames(df_new)

Sales_UK_3Mon_summary <- merge(Sales_UK_3Mon_summary, df_new, by = "Rev_cluster")

#  Drop the original Rev_cluster   
Sales_UK_3Mon_summary <- subset(Sales_UK_3Mon_summary, select = -c(Rev_cluster))
#  Change the column name from 'New_Index' to Rev_Cluster
Sales_UK_3Mon_summary <- Sales_UK_3Mon_summary %>% dplyr::rename(Rev_cluster = 'New_index') 
Sales_UK_3Mon_summary$Rev_cluster <- as.numeric(Sales_UK_3Mon_summary$Rev_cluster)

Sales_UK_3Mon_summary %>% group_by(Rev_cluster) %>% summary(Revenue)

remove(df_new)

Sales_UK_3Mon_summary$Overall_score <- Sales_UK_3Mon_summary$Rec_cluster + 
                                       Sales_UK_3Mon_summary$Freq_cluster +
                                       Sales_UK_3Mon_summary$Rev_cluster
# Display summary of mean
set.seed(123)
Sales_UK_3Mon_summary %>%  select_at(vars(Overall_score, Recency, Frequency, Revenue)) %>%
                     group_by(Overall_score) %>%
                     summarise_all(c("mean")) 
#---------------------------------------------------------
#    Overall_score Recency Frequency Revenue
#
#1         5         27        433    14985.
#2         6        44.5      344     8795.
#3         7        53.8      219.    3960.
#4         8        52.9      103.    2175.
#5         9        60.4      41.8    698.
#6        10        35.9      36.1    683.
#7        11        19.7      26.4    526.
#8        12        9.29      18.6    370
#---------------------------------------------------------
Sales_UK_3Mon_summary$Segment <- 'Low-Value'
Sales_UK_3Mon_summary$Segment[between(Sales_UK_3Mon_summary$Overall_score,6,9)] <- 'High-Value'
Sales_UK_3Mon_summary$Segment[Sales_UK_3Mon_summary$Overall_score == 10 | +
                                Sales_UK_3Mon_summary$Overall_score == 5] <- 'Mid-Value'
#
#-----------------------------------------------------------------------------------------------
# Calculate the Revenue pattern for the next 6 months using past 6 months data
#-----------------------------------------------------------------------------------------------
# Calculate the revenue for each customer
Sales_UK_6Mon$Revenue <- Sales_UK_6Mon$Quantity*Sales_UK_6Mon$Price
Sales_UK_6Mon_summary<- Sales_UK_6Mon %>%
                        select(Customer_ID, Revenue) %>%
                        group_by(Customer_ID) %>%
                        summarise(M6_Revenue = sum(Revenue))

# Plot the Revenue diagram. The plot shows outliers (Revenue < 0) that have to be removed.
ggplot(subset(Sales_UK_6Mon_summary, M6_Revenue <  10000 ),aes(M6_Revenue)) +   
   geom_histogram(fill="green",col="red",binwidth = 20) + 
  labs(title="6 Months Revenue", x="Revenue", y="Count")

# merge our 3 months and 6 months dataframes to see correlations between LTV and the feature set 
# Left sort - Inlcude only rows from the 2nd data frame that matches with the first
Sales_UK_merge <- merge(Sales_UK_3Mon_summary, Sales_UK_6Mon_summary, by = "Customer_ID", all.x = TRUE)

# Replace NA with zeros
Sales_UK_merge[is.na(Sales_UK_merge)] = 0

# Take only data with M6_Revenue < 30000 for plotting
Sales_UK_plot <- subset(Sales_UK_merge, M6_Revenue < 30000)

# Scatter plot pending
#Draw the scattterplot (ref: https://plotly.com/r/filled-area-plots/#basic-filled-area-plot)


# Before building the machine learning model, we need to identify what is the type of this machine 
#learning problem. LTV itself is a regression problem. A machine learning model can predict the $ value
#of the LTV. But here, we want LTV segments. Because it makes it more actionable and easy to communicate
#with other people. By applying K-means clustering, we can identify our existing LTV groups and build 
#segments on top of it.
# Apply clustering and have 3 segments - Low LTV, Mid LTV and High LTV

# Remove outliers
Sales_UK_merge <- subset(Sales_UK_merge,!(Sales_UK_merge$M6_Revenue > quantile(Sales_UK_merge$M6_Revenue, probs = c(.01, .99))[2] | +
                                            Sales_UK_merge$M6_Revenue < quantile(Sales_UK_merge$M6_Revenue, probs = c(.01, .99))[1]))

# Kmeans clustering on M6_Revenue
pc_cluster <-kmeans(Sales_UK_merge[,c("M6_Revenue")],3)
Sales_UK_merge$LTV_cluster <- as.numeric(pc_cluster$cluster)

df_new <- Sales_UK_merge %>% 
          select(M6_Revenue,LTV_cluster) %>%
          group_by(LTV_cluster)  %>% 
          summarise(LTV_mean = mean(M6_Revenue)) %>% ungroup()
df_new <- df_new %>%
          arrange(desc(LTV_mean)) 

df_new$New_index <- rownames(df_new)

Sales_UK_merge <- merge(Sales_UK_merge, df_new, by = "LTV_cluster")

#  Drop the original LTV_cluster   
Sales_UK_merge <- subset(Sales_UK_merge, select = -c(LTV_cluster))
#  Change the column name from 'New_Index' to Rev_Cluster
Sales_UK_merge <- Sales_UK_merge %>% dplyr::rename(LTV_cluster = 'New_index') 
Sales_UK_merge$LTV_cluster <- as.numeric(Sales_UK_merge$LTV_cluster)

Sales_UK_merge %>% group_by(LTV_cluster) %>% summary(M6_Revenue)
remove(df_new)

Sales_UK_merge1 <- Sales_UK_merge

# Drop the date field which is column 2
Sales_UK_merge1 <- Sales_UK_merge1 %>% select(-c("Last_PurchaseDt"))
Sales_UK_merge1 %>% group_by(LTV_cluster) %>% summary(M6_Revenue)

#----------------------------------------------------------------------------------------------------
# Machine Learning
#----------------------------------------------------------------------------------------------------
# 1. Need to do some feature engineering. We should convert categorical columns to numerical columns   
# 2. check the correlation of features against our label, LTV clusters
#    - 
# 3. split our feature set and label (LTV) as X and y. We use X to predict y
# 4. Will create Training and Test dataset. Training set will be used for building the machine 
#    learning model. We will apply our model to Test set to see its real performance.
#----------------------------------------------------------------------------------------------------

#  Convert the Segment column to numeric variables

  Sales_UK_merge1 <- dummy.data.frame(Sales_UK_merge1, names=c('Segment'), sep="_")

#----------------------------------------------------------------------------------------------------
#  CORRELATION
#----------------------------------------------------------------------------------------------------

# check the correlation of features against our label, LTV clusters
corr_matrix <- cor(Sales_UK_merge1)
# Sort the LTV Cluster column in the correlation matrix, descending order
   corr_matrix[order(-corr_matrix[,"LTV_cluster"]),]
   
pairs.panels(Sales_UK_merge1[,-c(1,3:4,6:7,9:10,12:14,16)],
                method = "pearson" # correlation method: "kendall"
                #             pch = 19, cex.labels = 1, cex = 0.8, cex.axis = 1,
                ##             number.cex = 2,
                #             lm=TRUE,
                #             scale = TRUE,
                ##             cor = TRUE,
                #             hist.col = "#00AFBB",
                #             density = TRUE,  # show density plots
                #             ellipses = TRUE # show correlation ellipses
   )
#----------------------------------------------------------------------------------------------------
#  GRADIENT BOOSTING
#----------------------------------------------------------------------------------------------------
   
# Create a training and testing data set
  # define an 70%/30% train/test split of the dataset
  
  data_train = sample(1:nrow(Sales_UK_merge1), nrow(Sales_UK_merge1)*0.7)
   
  boost.Sales_UK = gbm(LTV_cluster~., data = Sales_UK_merge1[data_train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
  summary(boost.Sales_UK,  cBars = 10, 
          method = relative.influence,
          las = 2.5)
  par(mfrow=c(1,2))
  plot(boost.Sales_UK,i="Revenue")
  plot(boost.Sales_UK,i="Recency")  

  yhat.boost = predict(boost.Sales_UK, newdata = Sales_UK_merge1[-data_train,],n.trees = 5000, interaction.depth=4)
  Sales_UK_merge1.test = Sales_UK_merge1[-data_train, "LTV_cluster"]
  #graphics.off()
  plot(yhat.boost, Sales_UK_merge1.test, xlab = "Predicted", ylab = "Actual")
  abline(0,1)  
  
#----------------------------------------------------------------------------------------------------------------
# RESAMPLING Techniques
#---------------------------------------------------------------------------------------------------------------
 par(mfrow=c(1,1))
   plot(Sales_UK_merge1$LTV_cluster, Sales_UK_merge1$Recency, xlab = "LTV Cluster", ylab = "Recency")
  
#Fit Linear + Polynomial regression with R
#Blue line indicates linear regression model
#Red line - polynomial - degree 2
#Green line - polynomial - degree 3
  
#-------------------------------------------------------------------------------------------------
#  LINEAR REGRESSION - Polynomial - 3 degrees
#-------------------------------------------------------------------------------------------------
  ggplot(Sales_UK_merge1, aes(LTV_cluster, Recency)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, linetype = 2, color = "red") +
    geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, linetype = 3, color = "green") +
    scale_color_manual(values = c("red","green","blue"))

   # obtain the test error, that is mean squared error
   # Take the square root of the output from mean to get the true errror value
   
### Iteration 1 ######
  lm.fit = lm(LTV_cluster~Recency, data=Sales_UK_merge1, subset=data_train)
  attach(Sales_UK_merge1)
  mean((LTV_cluster-predict(lm.fit,Sales_UK_merge1))[-data_train]^2)
  
  # Polynomial degree 1: 0.243
  
### Iteration 2 ######  
  # Fit the model of polynomial regression (degree 2) - 
  lm.fit2 = lm(LTV_cluster~poly(Recency,2),data=Sales_UK_merge1,subset = data_train)
  mean((LTV_cluster-predict(lm.fit2,Sales_UK_merge1))[-data_train]^2)
  
  # Polynomial degree 2: 0.240

### Iteration 3 ######
  # Fit the model of polynomial regression (degree 3) -
  lm.fit3 = lm(LTV_cluster~poly(Recency,3),data=Sales_UK_merge1,subset = data_train)
  mean((LTV_cluster-predict(lm.fit3,Sales_UK_merge1))[-data_train]^2)
  
  # Polynomial degree 3: 0.240
  
### Polynomial degree 1 with 0.243 is looking better.

#-------------------------------------------------------------------------------------------------
#  NAIVE BAYES
#-------------------------------------------------------------------------------------------------

  split = 0.70
  trainIndex <- createDataPartition(Sales_UK_merge1$LTV_cluster, p = split, list = FALSE)    
 NB_data_train <- Sales_UK_merge1[trainIndex,-c(1,3:4,6:7,9:10,12:14,16)]
 NB_data_test  <- Sales_UK_merge1[-trainIndex,-c(1,3:4,6:7,9:10,12:14,16)]
  
 NB_data_train$LTV_cluster <- as.factor(NB_data_train$LTV_cluster)
 NB_data_test$LTV_cluster <- as.factor(NB_data_test$LTV_cluster)
 
 # train a naive bayes model
  model <- NaiveBayes(LTV_cluster~., data=NB_data_train)
  
# make predictions
  x_test <- NB_data_test[,1:5]
  y_test <- NB_data_test[,6]
  predictions <- predict(model, x_test)
  #summarize results
  confusionMatrix(predictions$class, y_test)
#-------------------------- End of NAIVE BAYES ----------------------------------------------------
 
  #-------------------------------------------------------------------------------------------------
  # LOOCV -  CROSS VALIDATION
  #-------------------------------------------------------------------------------------------------
  # Leave One Out Cross Validation  - LOOCV
  # uses a single observation from the original sample as the validation data, and the 
  # remaining observations as the training data.  
  
  # 10 fold CV
  # A vector for collecting the errors
  loocv.error10 = rep(0,10)
  
  # The polynomial degree
  degree = 1:10
  
  # A fit for each degree
  for (d in degree){
    glm.fit <- glm(LTV_cluster~poly(Recency,d), data = Sales_UK_merge1)
    loocv.error10[d] = cv.glm(Sales_UK_merge1, glm.fit, K=10)$delta[1]
  }
  lines(degree, loocv.error10, type = "b", col="red")
  
  summary(glm.fit)
  loocv.error10
#-------------------------- End of LOOCV  --------------------------------------------------------  
  


#-------------------------------------------------------------------------------------------------
#  BOOTSTRAP
#------------------------------------------------------------------------------------------------- 
# To account for the deviation
  
  statistic <- function(Sales_UK_merge1, index) {
    lm.fit <- lm(LTV_cluster ~ Recency, data = Sales_UK_merge1, subset = index)
    coef(lm.fit)
  }
  
  set.seed(123)
  summary(lm(LTV_cluster ~ Recency, data = Sales_UK_merge1))
  
  statistic(Auto, 1:392)
  set.seed(123)
  #Bootstrap with 1000 replicas
  boot(Sales_UK_merge1, statistic, 1000)
  
  quad.statistic <- function(Sales_UK_merge1, index) {
    lm.fit <- lm(LTV_cluster ~ poly(Recency, 2), data = Sales_UK_merge1, subset = index)
    coef(lm.fit)
  }
  
  set.seed(1)
  
  #Bootstrap with 1000 replicas
  boot(Sales_UK_merge1, statistic, 1000)
#-------------------------- BOOTSTRAP Model----------------------------------------------------------  

#*****************************************************************************************************************
#                                  PART V - PREDICT the NEXT PURCHASE DAY
#*****************************************************************************************************************
# Use one 6 months data to identify the purchase pattern 
# Use last 3 months data to predict
Sales_UK_6Mon_1 <- Sales_UK_Data %>% subset(InvoiceDate1 >= "2011-03-01" & InvoiceDate1 < "2011-09-01" )
Sales_UK_Next <- Sales_UK_Data %>% subset(InvoiceDate1 >= "2011-09-01" & InvoiceDate1 < "2011-12-01" )

  Sales_UK_User <- Sales_UK_6Mon_1 %>% distinct(Customer_ID)
  
  #create a dataframe with customer id and first purchase date in Sales_UK_Next
  Next_First_PurchaseDt = Sales_UK_Next %>% 
    select(Customer_ID, InvoiceDate1) %>%
    group_by(Customer_ID)  %>%
    summarise(Next_PurchaseDt = min(InvoiceDate1))

  #create a dataframe with customer id and last purchase date in Sales_UK_6Mon_1
  Mon6_last_PurchaseDt = Sales_UK_6Mon_1 %>% 
    select(Customer_ID, InvoiceDate1) %>%
    group_by(Customer_ID)  %>%
    summarise(Last_PurchaseDt = max(InvoiceDate1))

  # Merge 2 dataframes
  Sales_purchase_dates <- merge(Mon6_last_PurchaseDt,Next_First_PurchaseDt, by = "Customer_ID", all.x = TRUE)
  
  #calculate the time difference in days:
  Sales_purchase_dates$Next_Purch_Day = as.numeric(Sales_purchase_dates$Next_PurchaseDt - Sales_purchase_dates$Last_PurchaseDt)
  # Merge only the Next Purchase day column 
 
  Sales_UK_User <- merge(x=Sales_UK_User,y=Sales_purchase_dates[,c("Customer_ID","Next_Purch_Day")], by = "Customer_ID", all.x = TRUE )
 # Sales_UK_User1 <- Sales_UK_User1 %>% dplyr::rename(Next_Purch_Day = "Recency")
  Sales_UK_User[is.na(Sales_UK_User)] = 999

#-------------------------------------------------------------------------------------------------
#                               RECENCY
#-------------------------------------------------------------------------------------------------   
# We have customer ids and corresponding labels in a dataframe. Let's enrich it with our feature 
# set to build our machine learning model.  
#   Feature Engineering
#   RFM scores & clusters
#   Calculate the Recency using 6 months data
# Get the max purchase date for each customer and create a dataframe with it
  
  Sales_UK_6Mon_1_summary = Sales_UK_6Mon_1 %>% 
                            select(Customer_ID, InvoiceDate1) %>%
                            group_by(Customer_ID)  %>%
                            summarise(Last_PurchaseDt = max(InvoiceDate1))
  
  Sales_UK_6Mon_1_summary$Recency = as.numeric(max(Sales_UK_6Mon_1_summary$Last_PurchaseDt) -  +
                                                 Sales_UK_6Mon_1_summary$Last_PurchaseDt)
  
  
  #Merge both data
  Sales_UK_6Mon_1 <- merge(Sales_UK_6Mon_1, Sales_UK_6Mon_1_summary, by = "Customer_ID")
  
  Sales_UK_User <- merge(Sales_UK_User, Sales_UK_6Mon_1_summary, by = "Customer_ID")
  
  # Plot the recency diagram
  ggplot(Sales_UK_User,aes(Recency)) +   
    #       geom_histogram(breaks = seq(0,400, by=2), fill="green",col="red") + 
    geom_histogram(fill="green",col="red") + 
    labs(title="Recency", x="Recency", y="Count") 
  #         xlim(c(0,400)) + 
  #         ylim(c(0,300))
  
  # Kmeans clustering
  pc_cluster <-kmeans(Sales_UK_User[,c("Recency")], 4)
  Sales_UK_User$Rec_cluster <- as.numeric(pc_cluster$cluster)
  
  Sales_UK_6Mon_1_summary <- Sales_UK_User
  
  # Call the Order clustering function
  #Sales_UK_6Mon_1_summary <-  Order_Cluster("Recency_cluster", "Recency", Sales_UK_6Mon_1_summary)
  df_new <- Sales_UK_6Mon_1_summary %>% 
    select(Recency,Rec_cluster) %>%
    group_by(Rec_cluster)  %>% 
    summarise(Rec_mean = mean(Recency)) %>% ungroup()
  df_new <- df_new %>%
    arrange(desc(Rec_mean)) 
  
  df_new$New_index <- rownames(df_new)
  
  Sales_UK_6Mon_1_summary <- merge(Sales_UK_6Mon_1_summary, df_new, by = "Rec_cluster")
  
  #  Drop the original Recency_cluster   
  Sales_UK_6Mon_1_summary <- subset(Sales_UK_6Mon_1_summary, select = -c(Rec_cluster))
  #  Change the column name from 'New_Index' to Rec_Cluster
  Sales_UK_6Mon_1_summary <- Sales_UK_6Mon_1_summary %>% dplyr::rename(Rec_cluster = 'New_index')
  
  Sales_UK_6Mon_1_summary$Rec_cluster <- as.numeric(Sales_UK_6Mon_1_summary$Rec_cluster)
  Sales_UK_6Mon_1_summary %>% group_by(Rec_cluster) %>% summary(Recency)
  remove(df_new)
  
#-----------------------------------------------------------------------------------------------
#                               FREQUENCY
#-----------------------------------------------------------------------------------------------
# Get the max purchase date for each customer and create a dataframe with it
  Segment_frequency = Sales_UK_6Mon_1 %>% 
                      select(Customer_ID, InvoiceDate1) %>%
                      group_by(Customer_ID)  %>% 
                      summarise(Frequency = n())
  
  #Merge both data
 Sales_UK_6Mon_1_summary <- merge(Sales_UK_6Mon_1_summary, Segment_frequency, by = "Customer_ID")
  
  # Plot the recency diagram
  ggplot(subset(Sales_UK_6Mon_1_summary, Frequency < 1000),aes(Frequency)) +   
    #       geom_histogram(breaks = seq(0,400, by=2), fill="green",col="red") + 
    geom_histogram(fill="green",col="red",binwidth = 20) + 
    labs(title="Frequency", x="Frequency", y="Count")
  
  
  #From the graph, you can see the optimal k is 3, where the curve is starting to have a diminishing 
  #return. Once you have our optimal k, you re-run the algorithm with k equals to 4 and evaluate the clusters.
  pc_cluster <-kmeans(Sales_UK_6Mon_1_summary[,c("Frequency")], 4)
  Sales_UK_6Mon_1_summary$Freq_cluster <- as.numeric(pc_cluster$cluster)
  
  
  # Call the Order clustering function
  #Sales_UK_6Mon_1_summary <-  Order_Cluster("Frequency_cluster", "Frequency", Sales_UK_6Mon_1_summary)
  df_new <- Sales_UK_6Mon_1_summary %>% 
    select(Frequency,Freq_cluster) %>%
    group_by(Freq_cluster)  %>% 
    summarise(Freq_mean = mean(Frequency)) %>% ungroup()
  df_new <- df_new %>%
    arrange(desc(Freq_mean)) 
  
  df_new$New_index <- rownames(df_new)
  
  Sales_UK_6Mon_1_summary <- merge(Sales_UK_6Mon_1_summary, df_new, by = "Freq_cluster")
  
  #  Drop the original Frequency_cluster   
  Sales_UK_6Mon_1_summary <- subset(Sales_UK_6Mon_1_summary, select = -c(Freq_cluster))
  #  Change the column name from 'New_Index' to Freq_Cluster
  Sales_UK_6Mon_1_summary <- Sales_UK_6Mon_1_summary %>% dplyr::rename(Freq_cluster = 'New_index') 
  
  Sales_UK_6Mon_1_summary$Freq_cluster <- as.numeric(Sales_UK_6Mon_1_summary$Freq_cluster)
  remove(df_new)
  
#-----------------------------------------------------------------------------------------------
#                                  REVENUE
#-----------------------------------------------------------------------------------------------
# Calculate the revenue for each customer
  Sales_UK_6Mon_1$Revenue <- Sales_UK_6Mon_1$Quantity*Sales_UK_6Mon_1$Price
  Segment_Revenue<- Sales_UK_6Mon_1 %>%
                    select(Customer_ID, Revenue) %>%
                    group_by(Customer_ID) %>%
                    summarise(Revenue = sum(Revenue))
  
  #Merge both data
  Sales_UK_6Mon_1_summary <- merge(Sales_UK_6Mon_1_summary, Segment_Revenue, by = "Customer_ID")
  
  # Plot the recency diagram
  ggplot(subset(Sales_UK_6Mon_1_summary, Revenue > 0 & Revenue <=  10000 ),aes(Revenue)) +   
    #       geom_histogram(breaks = seq(0,400, by=2), fill="green",col="red") + 
    geom_histogram(fill="green",col="red",binwidth = 20) + 
    labs(title="Revenue", x="Revenue", y="Count")
  
  #From the graph, you can see the optimal k is 3, where the curve is starting to have a diminishing 
  #return. Once you have our optimal k, you re-run the algorithm with k equals to 4 and evaluate the clusters.
  pc_cluster <-kmeans(Sales_UK_6Mon_1_summary[,c("Revenue")], 4)
  Sales_UK_6Mon_1_summary$Rev_cluster <- as.numeric(pc_cluster$cluster)
  
  df_new <- Sales_UK_6Mon_1_summary %>% 
    select(Revenue,Rev_cluster) %>%
    group_by(Rev_cluster)  %>% 
    summarise(Rev_mean = mean(Revenue)) %>% ungroup()
  df_new <- df_new %>%
    arrange(desc(Rev_mean)) 
  
  df_new$New_index <- rownames(df_new)
  
  Sales_UK_6Mon_1_summary <- merge(Sales_UK_6Mon_1_summary, df_new, by = "Rev_cluster")
  
  #  Drop the original Rev_cluster   
  Sales_UK_6Mon_1_summary <- subset(Sales_UK_6Mon_1_summary, select = -c(Rev_cluster))
  #  Change the column name from 'New_Index' to Rev_Cluster
  Sales_UK_6Mon_1_summary <- Sales_UK_6Mon_1_summary %>% dplyr::rename(Rev_cluster = 'New_index') 
  
  Sales_UK_6Mon_1_summary$Rev_cluster <- as.numeric(Sales_UK_6Mon_1_summary$Rev_cluster)
  Sales_UK_6Mon_1_summary %>% group_by(Rev_cluster) %>% summary(Revenue)
  
  
# Overall score
  Sales_UK_6Mon_1_summary$Overall_score <- Sales_UK_6Mon_1_summary$Rec_cluster + 
                                           Sales_UK_6Mon_1_summary$Freq_cluster +
                                          Sales_UK_6Mon_1_summary$Rev_cluster
  # Display summary of mean
  Sales_UK_6Mon_1_summary %>%  select_at(vars(Overall_score, Recency, Frequency, Revenue)) %>%
    group_by(Overall_score) %>%
    summarise_all(c("mean")) 
#-------------------------------------------------------------  
#  Overall_score Recency Frequency Revenue
# 
# 1             7    52.3    1583.   16657.
# 2             8    67.5     335.   17535.
# 3             9   141.       50.0   1102.
# 4            10    78.8      65.8   1184.
# 5            11    46.5      52.2    835.
# 6            12    16.8      38.8    689.
#-------------------------------------------------------------    
  Sales_UK_6Mon_1_summary$Segment <- 'Low-Value'
  Sales_UK_6Mon_1_summary$Segment[between(Sales_UK_6Mon_1_summary$Overall_score, 7,8)] <- 'Mid-Value'
  Sales_UK_6Mon_1_summary$Segment[between(Sales_UK_6Mon_1_summary$Overall_score, 10,11)] <- 'Mid-Value'
  Sales_UK_6Mon_1_summary$Segment[Sales_UK_6Mon_1_summary$Overall_score == 9] <- 'High-Value'  

  # Stacked scatterplot pending
  
  #we create a dataframe with Customer ID and Invoice Date (date format)
 UK_6Mon_1_day_ordr <- Sales_UK_6Mon_1 %>% select(Customer_ID, InvoiceDate1)
 
 # Remove duplicates and keep unique rows for the Customer Id and Invoice Date combination
 UK_6Mon_1_day_ordr <- UK_6Mon_1_day_ordr  %>% 
                        arrange(Customer_ID, InvoiceDate1) %>% 
                       distinct(Customer_ID, InvoiceDate1)
 
 
 # by using shift, create new columns with the dates of last 3 purchases
 # and see how the dataframe looks like:
 # shifting last 3 purchase dates
 # Note: R expects the Data Table to apply shift function
UK_6Mon_1_day_ordr1 <- as.data.table(UK_6Mon_1_day_ordr)
UK_6Mon_1_day_ordr1[, Prev_idate1 := shift(InvoiceDate1,1L), by=Customer_ID]
UK_6Mon_1_day_ordr1[, Prev_idate2 := shift(InvoiceDate1,2L), by=Customer_ID] 
UK_6Mon_1_day_ordr1[, Prev_idate3 := shift(InvoiceDate1,3L), by=Customer_ID]
remove(UK_6Mon_1_day_ordr)

UK_6Mon_1_day_ordr <- as.data.frame(UK_6Mon_1_day_ordr1)
remove(UK_6Mon_1_day_ordr1)

# calculating the difference in days for each invoice date:
 
UK_6Mon_1_day_ordr$DayDiff1 <- as.numeric(UK_6Mon_1_day_ordr$InvoiceDate1 - UK_6Mon_1_day_ordr$Prev_idate1)
UK_6Mon_1_day_ordr$DayDiff2 <- as.numeric(UK_6Mon_1_day_ordr$InvoiceDate1 - UK_6Mon_1_day_ordr$Prev_idate2)
UK_6Mon_1_day_ordr$DayDiff3 <- as.numeric(UK_6Mon_1_day_ordr$InvoiceDate1 - UK_6Mon_1_day_ordr$Prev_idate3)

# For each customer ID, we utilize .agg() method to find out the mean and standard deviation of the 
# difference between purchases in days
remove(temp_6Mon_data,UK_6Mon_1_day_ordr_DayDiff_mean, UK_6Mon_1_day_ordr_DayDiff_SD)

temp_6Mon_data <- subset(UK_6Mon_1_day_ordr, select = c("Customer_ID","DayDiff1"))
# Aggregate works only on the Data Table and ordered by the key
temp_6Mon_data <- as.data.table(temp_6Mon_data)

temp_6Mon_data <- temp_6Mon_data[order(temp_6Mon_data$Customer_ID),]
#   find the mean
UK_6Mon_1_day_ordr_DayDiff_mean <- aggregate(DayDiff1 ~ Customer_ID,
                                        data = temp_6Mon_data,
                                        FUN = mean, 
                                        na.rm=TRUE
                                        )

UK_6Mon_1_day_ordr_DayDiff_mean <- UK_6Mon_1_day_ordr_DayDiff_mean %>% dplyr::rename(DayDiff_mean = 'DayDiff1')

# Find the SD
UK_6Mon_1_day_ordr_DayDiff_SD <- aggregate(DayDiff1 ~ Customer_ID,
                                        data = temp_6Mon_data,
                                        FUN = sd, 
                                        na.rm=TRUE)
UK_6Mon_1_day_ordr_DayDiff_SD <- UK_6Mon_1_day_ordr_DayDiff_SD %>% dplyr::rename(DayDiff_SD = 'DayDiff1')

UK_6Mon_1_day_ordr_DayDiff_mean <- merge(UK_6Mon_1_day_ordr_DayDiff_mean, UK_6Mon_1_day_ordr_DayDiff_SD, by = "Customer_ID")
                             
# keep customers who have > 3 purchases i.e Prev_idate1, Prev_idate2, and Prev_idate3 are no "NA"

UK_6Mon_1_day_ordr_Last <- UK_6Mon_1_day_ordr %>%
                           group_by(Customer_ID) %>%
                           arrange(InvoiceDate1) %>%
                           filter(row_number()== n())

#Drop rows having NA
UK_6Mon_1_day_ordr_Last <- na.omit(UK_6Mon_1_day_ordr_Last)

UK_6Mon_1_day_ordr_Last <- merge(UK_6Mon_1_day_ordr_Last, UK_6Mon_1_day_ordr_DayDiff_mean, by = "Customer_ID" )
Sales_UK_6Mon_1_summary_bkup <- Sales_UK_6Mon_1_summary
Sales_UK_6Mon_1_summary <- merge(Sales_UK_6Mon_1_summary, +
        UK_6Mon_1_day_ordr_Last[,c("Customer_ID","DayDiff1","DayDiff2","DayDiff3","DayDiff_mean","DayDiff_SD")], by = "Customer_ID")

# Take a backup to proceed with data cleansing
Sales_UK_6Mon_1_summary_1 <- Sales_UK_6Mon_1_summary

#  Convert the Segment column to numeric variables
Sales_UK_6Mon_1_summary_1 <- dummy.data.frame(Sales_UK_6Mon_1_summary_1, names=c('Segment'), sep="_")

# identify the classes in our label
summary(Sales_UK_6Mon_1_summary_1$Next_Purch_Day)

# Categorize the next purchase day into three to take action and communicate. These boundaries can be 
# modified as per business needs
#-----------------------------------------------------------------------------------------------
#  Class name: 1 => Customers will purchase in the next 0-20 days
#  Class name: 2 => Customers will purchase in the next 21-49 days
#  Class name: 0 => Customers will purchase in the next >= 50 days
#-----------------------------------------------------------------------------------------------
Sales_UK_6Mon_1_summary_1$Next_Purch_DayRange <- 0
Sales_UK_6Mon_1_summary_1$Next_Purch_DayRange[between(Sales_UK_6Mon_1_summary_1$Next_Purch_Day,21,49)] <- 2
Sales_UK_6Mon_1_summary_1$Next_Purch_DayRange[between(Sales_UK_6Mon_1_summary_1$Next_Purch_Day,0,20)] <- 1

# check the correlation of features against our label, LTV clusters
# Drop the date column and make others as numeric
Sales_UK_6Mon_1_summary_1 <- subset(Sales_UK_6Mon_1_summary_1, select=-c(Last_PurchaseDt))

#-------------------------------------------------------------------------------------------------
#  CORRELATION
#-------------------------------------------------------------------------------------------------
corr_matrix <- cor(Sales_UK_6Mon_1_summary_1[,-c(1,4:5,7:8,10:11,13:18)])
corr_matrix[order(-corr_matrix[,"Next_Purch_Day"]),]

graphics.off()

dev.off()
pairs.panels(Sales_UK_6Mon_1_summary_1[,-c(1,4:5,7:8,10:11,13:18)],
             method = "pearson" # correlation method: "kendall"
#             pch = 19, cex.labels = 1, cex = 0.8, cex.axis = 1,
##             number.cex = 2,
#             lm=TRUE,
#             scale = TRUE,
##             cor = TRUE,
#             hist.col = "#00AFBB",
#             density = TRUE,  # show density plots
#             ellipses = TRUE # show correlation ellipses
)
# Using Pearson and Kendall methods, the Recency and Overall score are more correlated  
# followed by Revenue

#------------------------------------------------------------------------------------------------
# Apply machine learning algorithms
#-------------------------------------------------------------------------------------------------
#  GRADIENT BOOSTING
#-------------------------------------------------------------------------------------------------

set.seed(123)
data_y = sample(1:nrow(Sales_UK_6Mon_1_summary_1), nrow(Sales_UK_6Mon_1_summary_1)*0.7)
gbm.Sales_UK_Next = gbm(Next_Purch_Day~., data = Sales_UK_6Mon_1_summary_1[data_y,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(gbm.Sales_UK_Next,
        cBars = 10, 
        method = relative.influence,
        las = 2
        )
par(mfrow=c(2,1))
plot(gbm.Sales_UK_Next,i="Recency")
plot(gbm.Sales_UK_Next,i="Revenue")  

yhat.boost = predict(gbm.Sales_UK_Next, newdata = Sales_UK_6Mon_1_summary_1[-data_y,],n.trees = 5000, interaction.depth=4)
Sales_UK_6Mon_1_summary_1.test = Sales_UK_6Mon_1_summary_1[-data_y, "Next_Purch_Day"]
#graphics.off()
plot(yhat.boost, Sales_UK_6Mon_1_summary_1.test, xlab = "Predicted", ylab = "Actual")
abline(0,1)  
graphics.off()

#-------------------------------------------------------------------------------------------------
#  LINEAR REGRESSION
#-------------------------------------------------------------------------------------------------
#Fit Linear + Polynomial regression with R
#Blue line indicates linear regression model
#Red line - polynomial - degree 2
#Green line - polynomial - degree 3
Next_Purch_Day_log <- log(Sales_UK_6Mon_1_summary_1$Next_Purch_Day)
ggplot(Sales_UK_6Mon_1_summary_1, aes(Revenue, Next_Purch_Day_log)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, linetype = 2, color = "red") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, linetype = 3, color = "green") +
  scale_color_manual(values = c("red","green","blue"))

#### Iteration 1 #############
# obtain the test error, that is mean squared error
# Take the square root of the output from mean to get the true errror value

set.seed(123)
train_next = sample(1:nrow(Sales_UK_6Mon_1_summary_1),nrow(Sales_UK_6Mon_1_summary_1)*0.70)
lm.fit = lm(Next_Purch_Day~., data=Sales_UK_6Mon_1_summary_1, subset=train_next)
summary(lm.fit)
attach(Sales_UK_6Mon_1_summary_1)

mean((Next_Purch_Day-predict(lm.fit,Sales_UK_6Mon_1_summary_1))[-train_next]^2)


#### Iteration 1 #############
# Fit the model of polynomial regression (degree 2) - 

lm.fit2 = lm(Next_Purch_Day~poly(Revenue,2),data=Sales_UK_6Mon_1_summary_1,subset = train_next)
mean((Next_Purch_Day-predict(lm.fit2,Sales_UK_6Mon_1_summary_1))[-train_next]^2)

# Polynomial degree 2 should considerably reduce the error 

# Fit the model of polynomial regression (degree 3) - 

lm.fit3 = lm(Next_Purch_Day~poly(Revenue,3),data=Sales_UK_6Mon_1_summary_1,subset = train_next)
mean((Next_Purch_Day-predict(lm.fit3,Sales_UK_6Mon_1_summary_1))[-train_next]^2)


#-------------------------------------------------------------------------------------------------
#  NAIVE BAYES
#-------------------------------------------------------------------------------------------------
# define an 70%/30% train/test split of the dataset
split = 0.70
trainIndex <- createDataPartition(Sales_UK_6Mon_1_summary_1$Next_Purch_Day, p = split, list = FALSE)
data_train <- Sales_UK_6Mon_1_summary_1[trainIndex,-c(1,4:5,7:8,10:11,13:20)]
data_test  <- Sales_UK_6Mon_1_summary_1[-trainIndex,-c(1,4:5,7:8,10:11,13:20)]

# We need to use a factor in the NB algorithm. Hencing converting Next_Purch_dayRange as a factor.
data_train$Next_Purch_DayRange <- as.factor(data_train$Next_Purch_DayRange)
data_test$Next_Purch_DayRange <- as.factor(data_test$Next_Purch_DayRange)
# train a naive bayes model
model <- NaiveBayes(Next_Purch_DayRange~., data=data_train)

# make predictions
x_test <- data_test[,1:5]
y_test <- data_test[,6]
predictions <- predict(model, x_test)
#summarize results
confusionMatrix(predictions$class, y_test)
#-------------------------- End of NAIVE BAYES ----------------------------------------------------

#-------------------------------------------------------------------------------------------------
# LOOCV -  CROSS VALIDATION
#-------------------------------------------------------------------------------------------------
# Leave One Out Cross Validation  - LOOCV
# uses a single observation from the original sample as the validation data, and the 
# remaining observations as the training data.  

# 10 fold CV
# A vector for collecting the errors
cv.error10 = rep(0,10)

# The polynomial degree
degree = 1:10

# A fit for each degree
for (d in degree){
  glm.fit <- glm(Next_Purch_Day~poly(Revenue,d), data = Sales_UK_6Mon_1_summary_1)
  cv.error10[d] = cv.glm(Sales_UK_6Mon_1_summary_1, glm.fit, K=10)$delta[1]
}
lines(degree, cv.error10, type = "b", col="red")

summary(glm.fit)
cv.error10
#-------------------------- End of LOOCV  --------------------------------------------------------

#-------------------------------------------------------------------------------------------------
#  BOOTSTRAP
#-------------------------------------------------------------------------------------------------
statistic <- function(Sales_UK_6Mon_1_summary_1, index) {
  lm.fit <- lm(Next_Purch_Day ~ Revenue, data = Sales_UK_6Mon_1_summary_1, subset = index)
  coef(lm.fit)
}

set.seed(123)
summary(lm(Next_Purch_Day ~ Revenue, data = Sales_UK_6Mon_1_summary_1))

statistic(Auto, 1:392)

set.seed(123)

#Bootstrap with 1000 replicas
boot(Sales_UK_6Mon_1_summary_1, statistic, 1000)

quad.statistic <- function(Sales_UK_6Mon_1_summary_1, index) {
  lm.fit <- lm(Next_Purch_Day ~ poly(Revenue, 2), data = Sales_UK_6Mon_1_summary_1, subset = index)
  coef(lm.fit)
}

set.seed(1)

#Bootstrap with 1000 replicas
boot(Sales_UK_6Mon_1_summary_1, statistic, 1000)

#-------------------------------------- THE END -----------------------------------------------------
