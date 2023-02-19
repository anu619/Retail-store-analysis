library(readr)
library(data.table)
library(dplyr)
library(skimr)
library(ggplot2)
library(tidyverse)
library(naniar)
library(stringr)

install.packages("hablar")
library(hablar)
library(dplyr)
library(sqldf)
library(odbc)
library(DBI)


#Load data

country <- read.table("C:/Users/LabStudent-55-706949/Documents/ADMP_Reassesment/OrionStarData/OrionStarData_txt/country.txt",sep=',',header = TRUE)
county <- read.table("C:/Users/LabStudent-55-706949/Documents/ADMP_Reassesment/OrionStarData/OrionStarData_txt/county.txt",sep=',',header = TRUE)
customer <- read.table("C:/Users/LabStudent-55-706949/Documents/ADMP_Reassesment/OrionStarData/OrionStarData_txt/customer.txt",sep=',',header = TRUE, encoding='latin1')
customer_type <- read.table("C:/Users/LabStudent-55-706949/Documents/ADMP_Reassesment/OrionStarData/OrionStarData_txt/customer_type.txt",sep=',',header = TRUE)
discount <- read.table("C:/Users/LabStudent-55-706949/Documents/ADMP_Reassesment/OrionStarData/OrionStarData_txt/discount.txt",sep=',',header = TRUE)
order <- read.table("C:/Users/LabStudent-55-706949/Documents/ADMP_Reassesment/OrionStarData/OrionStarData_txt/order.txt",sep=',',header = TRUE)
order_item <- read.table("C:/Users/LabStudent-55-706949/Documents/ADMP_Reassesment/OrionStarData/OrionStarData_txt/order_item.txt",sep=',',header = TRUE)
price_list <- read.table("C:/Users/LabStudent-55-706949/Documents/ADMP_Reassesment/OrionStarData/OrionStarData_txt/price_list.txt",sep=',',header = TRUE)
product <- read.table("C:/Users/LabStudent-55-706949/Documents/ADMP_Reassesment/OrionStarData/OrionStarData_txt/product.txt",sep=',',header = TRUE)
product_level <- read.table("C:/Users/LabStudent-55-706949/Documents/ADMP_Reassesment/OrionStarData/OrionStarData_txt/product_level.txt",sep=',',header = TRUE)
state <- read.table("C:/Users/LabStudent-55-706949/Documents/ADMP_Reassesment/OrionStarData/OrionStarData_txt/state.txt",sep=',',header = TRUE)
staff <- read.table("C:/Users/LabStudent-55-706949/Documents/ADMP_Reassesment/OrionStarData/OrionStarData_txt/OrionStarData/staff.txt",sep=',',header = TRUE)
postal_code <- read.table("C:/Users/LabStudent-55-706949/Documents/ADMP_Reassesment/OrionStarData/OrionStarData_txt/OrionStarData/postal_code.txt",sep=',',header = TRUE)
street_code <- read.table("C:/Users/LabStudent-55-706949/Documents/ADMP_Reassesment/OrionStarData/OrionStarData_txt/street_code.txt",sep=',',header = TRUE)
organization <- read.table("C:/Users/LabStudent-55-706949/Documents/ADMP_Reassesment/OrionStarData/OrionStarData_txt/organization.txt",sep=',',header = TRUE,encoding='latin1')
supplier <- read.table("C:/Users/LabStudent-55-706949/Documents/ADMP_Reassesment/OrionStarData/OrionStarData_txt/supplier.txt",sep=',',header = TRUE,encoding='latin1')




#Select Appropriate column
country_1 <- country[c(1,2,4,5)]
county_1 <- county[c(1,8)]
customer_1 <- customer[c(1,2,5,12)] 
customer_type_1 <- customer_type
discount_1 <- discount
order_1 <- order[c(1,2,4,5,6)]
product_1 <- product[c(1,2,3,4)]
product_level_1 <- product_level
order_item_1 <- order_item[c(1,2,3,4,5)]
price_list_1 <- price_list[c(1,2,3,4,5)]


#Checkpoint1:
#check data types
sapply(country_1, class)
sapply(county_1, class)
sapply(customer_1, class)
sapply(customer_type_1, class)
sapply(discount_1, class)
sapply(order_1, class)
sapply(product_1, class)
sapply(product_level_1, class)
sapply(order_item_1, class)
sapply(price_list_1, class)
sapply(organization, class)
sapply(supplier, class)

# Remove special characters from customer name
# Replace Registered symbol to R
customer_1$Customer_Name <- str_replace_all(customer_1$Customer_Name, "\\u00AE", "R")

# Replace Copyright symbol to C
customer_1$Customer_Name <- str_replace_all(customer_1$Customer_Name, "\\u00a9", "C")

#Remove all special characters
customer_1$Customer_Name <- gsub("[^\x20-\x7F]","",customer_1$Customer_Name)



#update date format
##dates in 'discount', 'order', 'price_list' are characters.. needs to be updated
order_1$Order_Date <- as.Date(order_1$Order_Date, format='%d%b%Y')
order_1$Delivery_Date <- as.Date(order_1$Delivery_Date, format='%d%b%Y')
discount_1$Start_Date <- as.Date(discount$Start_Date, format='%d%b%Y')
discount_1$End_Date <- as.Date(discount$End_Date, format='%d%b%Y')
price_list_1$Start_Date <- as.Date(price_list_1$Start_Date, format='%d%b%Y')
price_list_1$End_Date <- as.Date(price_list_1$End_Date, format='%d%b%Y')
organization$Start_Date <- as.Date(organization$Start_Date, format='%d%b%Y')
organization$End_Date <- as.Date(organization$End_Date, format='%d%b%Y')
staff$Start_Date <- as.Date(staff$Start_Date, format='%d%b%Y')
staff$End_Date <- as.Date(staff$End_Date, format='%d%b%Y')
staff$Birth_Date <- as.Date(staff$Birth_Date, format='%d%b%Y')
staff$Emp_Hire_Date <- as.Date(staff$Emp_Hire_Date, format='%d%b%Y')

##prices in 'price_list', 'order_item' are characters.. needs to be converted to numbers

#numbers greater than 1000 includes ',' and when it gets converted from char to float NAs introduced
#To avoid introducing null values when data type conversion, need to remove ,
#remove $ symbol
price_list_1$Unit_Cost_Price <- gsub("\\$|\\,", "", price_list_1$Unit_Cost_Price)
price_list_1$Unit_Sales_Price <- gsub("\\$|\\,", "", price_list_1$Unit_Sales_Price)

#remove $ and % symbol
discount_1$Unit_Sales_Price <- gsub("\\$|\\,", "", discount_1$Unit_Sales_Price)
discount_1$Discount <- gsub("\\%", "", discount_1$Discount)

#Converting char to numbers
price_list_1$Unit_Cost_Price <- as.double(price_list_1$Unit_Cost_Price)
price_list_1$Unit_Sales_Price <- as.double(price_list_1$Unit_Sales_Price)

discount_1$Unit_Sales_Price <- as.double(discount_1$Unit_Sales_Price)
discount_1$Discount <- as.numeric(discount_1$Discount)
discount_1$Discount <- discount_1$Discount*0.01

#Above steps can be encapsulated together like below
order_item_1$Total_Retail_Price <- as.double(gsub("\\$|\\,", "", order_item_1$Total_Retail_Price))

staff$Salary <- as.double(gsub("\\$|\\,", "", staff$Salary))


#check date range
min(order_1$Order_Date)
max(order_1$Order_Date)

min(price_list_1$Start_Date)
max(price_list_1$End_Date)


#above step reveals incorrect data year '9999'
#incorrect data
#Handling the issue by replacing it with 31-12-2002
#Incorrect date in price list
price_list_1$temp <- c(diff(price_list_1$Product_ID),1)
price_list_1[which(price_list_1[,6]>0, arr.ind=TRUE), 3] <- as.Date('2002-12-31')

invalid_products <- product_1 %>% filter(Supplier_ID=='.')

product_2 <- product_1 %>% filter(Supplier_ID!='.')
product_2 <- product_1 %>% filter(!str_detect('.',Supplier_ID))

nrow(product_1)
nrow(invalid_products)
nrow(product_2)

product_group <- invalid_products %>% filter(Product_Level==2)
product_category <- invalid_products %>% filter(Product_Level==3)
product_line <- invalid_products %>% filter(Product_Level==4)

product_2$Supplier_ID <- as.numeric(product_2$Supplier_ID)

#Product ID 230100700001 remove from discount table
invalid_discount <- discount_1 %>% filter(Product_ID=='230100700001')

discount_2 <- discount_1 %>% filter(Product_ID!='230100700001')

##Data Validation
n_distinct(order_1$Customer_ID)
n_distinct(order_fact$Customer_ID)

n_distint(order_1$Order_ID)
n_distint(order_fact$Order_ID)

n_distint(order_item_1$Product_ID)
n_distint(order_fact$Product_ID)

#check for out-of-range data (for numeric variables)
hist(price_list_1$Unit_Cost_Price)
hist(order_item_1$Total_Retail_Price)
hist(order_item_1$Quantity,main = "Histogram of Quantity", xlab = "Quantity")
boxplot(order_item_1$Total_Retail_Price,main="Total Retail Price distribution",xlab="Total Retail price",ylab="amount")
hist(price_list_1$Unit_Sales_Price,main = "Histogram of Unit Sales price", xlab = "Unit Sales price (Doller)")
hist(price_list_1$Unit_Cost_Price,main="Histogram of Unit Cost price", xlab="Unit Cost Price (Doller)")
hist(discount_1$Discount,main="Histogram of Discount", xlab="Discount")
ggplot(customer_1,aes(x=factor(Customer_Type_ID)))+geom_bar()
ggplot(discount,aes(x=factor(Discount)))+geom_dotplot()

#Check NA value
#Change NA value to country name in country table
colSums(is.na(country_1))
colSums(is.na(county_1))
colSums(is.na(customer_1))
country_1[is.na(country_1)]<- 'NA' # Namibia stands for NA
colSums(is.na(customer_type_1))
colSums(is.na(discount))
colSums(is.na(price_list_1))
colSums(is.na(organization))
colSums(is.na(supplier))
colSums(is.na(product_2))

#check any unwanted value in the column
grepl(".", customer_1$Street_ID)
sum(str_detect(customer_1$Street_ID,"."))

#Check the duplicate value
sum(duplicated(country_1))
sum(duplicated(county_1$State_ID))
sum(duplicated(customer_1))
customer_1 %>% duplicated()
dup_data <- duplicated(customer_1)

#Data frame overview - Summary
skim(customer_1)
skim(discount)
skim(price_list_1)
skim(country_1)
skim(order_1)
skim(order_item_1)
skim(organization_1)
skim(supplier)
skim(customer_type)

#Data filtering
customer_1 %>% distinct(customer_1$Street_ID) %>% summarize(n=n())
which(duplicated(customer_1))



#Missing value
gg_miss_var(customer_1)
gg_miss_var(order_1)
gg_miss_var(price_list)       
gg_miss_var(product_1)

gg_miss_upset(order_1)


#String contains in country

sum(str_detect(organization$Org_Name, 'Orion Austalia')) > 0

#Replace with correct country name
organization_1 <- str_replace(organization$Org_Name, "Orion Austalia", "Orion Australia")

#Replace with correct country name
organization_1 <- organization %>%  mutate(Org_Name = str_replace(Org_Name, "Orion Austalia", "Orion Australia"))

sum(str_detect(organization_1$Org_Name, 'Orion Austalia')) > 0

##consistency check: make sure currency in all tables are in same unit ($)
sum(grepl("$",price_list_1$Unit_Cost_Price))
sum(grepl("$",price_list_1$Unit_Sales_Price))
nrow(price_list_1)
sum(grepl("$",discount_1$Unit_Sales_Price))
nrow(discount_1)



#Create Dimension tables
Dimcustomers <- sqldf('SELECT Customer.Customer_ID, Customer.Customer_Name, Customer_type.Customer_Type_ID, Customer_type.Customer_Type INTO Dimcustomer FROM Customer_type INNER JOIN Customer ON Customer_type.Customer_Type_ID = Customer.Customer_Type_ID GROUP BY Customer.Customer_ID, Customer.Customer_Name, Customer_type.Customer_Type_ID, Customer_type.Customer_Type;')
DimProduct <- sqldf('SELECT Product.Product_ID, Product.Product_Name, Supplier.Supplier_ID, Product.Line, Product.Category, Product.Group INTO DimProduct FROM Supplier INNER JOIN Product ON Supplier.Supplier_ID = Product.Supplier_ID GROUP BY Product.Product_ID, Product.Product_Name, Supplier.Supplier_ID, Product.Line, Product.Category, Product.Group;')
Dimlocation <- sqldf('SELECT Country.Country, Country.Country_Name, Continent.Continent_ID, Continent.Continent_Name INTO Dimlocation FROM Continent INNER JOIN Country ON Continent.Continent_ID = Country.Continent_ID GROUP BY Country.Country, Country.Country_Name, Continent.Continent_ID, Continent.Continent_Name; ')
TempDimTime <- sqldf('SELECT (Month([Delivery_Date]) & (Year([Delivery_Date]))) AS TimeID, Year([Delivery_Date]) AS [Year], Month([Delivery_Date]) AS [Month], Order.Delivery_Date AS [Date] INTO Tempdimtime FROM [Order] GROUP BY (Month([Delivery_Date]) & (Year([Delivery_Date]))), Year([Delivery_Date]), Month([Delivery_Date]), Order.Delivery_Date ORDER BY Year([Delivery_Date]), Month([Delivery_Date])')
DimTime <- sqldf('SELECT * FROM TempDimTime')



#Create Fact tables
sale_sfact <- sqldf('SELECT Tempdimtime.TimeID, Country.Country, Order.Delivery_Date, Order.Order_ID, Order.Customer_ID, Order.Employee_ID, Order_item1.Product_ID, Order.Order_Type, Sum(Order_item1.Quantity) AS Quantity, Sum(Order_item1.Unit_Retail_Price) AS Unit_Retail_Price, Avg(Price_list.Unit_Cost_Price) AS Unit_Cost_Price, Avg(Discount.Discount) AS Discount INTO sales_fact FROM ((((Country INNER JOIN Organization ON Country.Country = Organization.Country) INNER JOIN Supplier ON Country.Country = Supplier.Country) INNER JOIN (Customer INNER JOIN ([Order] INNER JOIN Tempdimtime ON Order.Delivery_Date = Tempdimtime.Date) ON Customer.Customer_ID = Order.Customer_ID) ON Organization.Employee_ID = Order.Employee_ID) INNER JOIN ((Product INNER JOIN Discount ON Product.Product_ID = Discount.Product_ID) INNER JOIN Price_list ON Product.Product_ID = Price_list.Product_ID) ON Supplier.Supplier_ID = Product.Supplier_ID) INNER JOIN Order_item1 ON (Product.Product_ID = Order_item1.Product_ID) AND (Order.Order_ID = Order_item1.Order_ID)
GROUP BY Tempdimtime.TimeID, Country.Country, Order.Delivery_Date, Order.Order_ID, Order.Customer_ID, Order.Employee_ID, Order_item1.Product_ID, Order.Order_Type
ORDER BY Order.Delivery_Date;')

#Establish a connection with Hive
con <- dbConnect(odbc::odbc(), .connection_string = "Driver={Cloudera ODBC Driver for Apache Hive};", 
                 timeout = 10,
                 uid="hive", 
                 Host = "sandbox-hdp.hortonworks.com",Port =10000)

#Creating tables in Hive
dbGetQuery(con, "CREATE DATABASE IF NOT EXISTS Orionsales")
dbGetQuery(con,"use Orionsales")
dbGetQuery(con,"CREATE TABLE Dimcustomers(Customer_ID INT, Customer_Name STRING, Customer_Type_ID INT, Customer_Type STRING)")
dbGetQuery(con,"CREATE TABLE DimProduct(Product_ID INT, Product_Name STRING, Supplier_ID INT, Line STRING, Category STRING, Group STRING)")
dbGetQuery(con,"CREATE TABLE Dimlocation(Country STRING, Country_Name STRING, Continent_ID INT,Continent_Name STRING)")
dbGetQuery(con,"CREATE TABLE TempDimTime(TimeID INT, Year INT, Month INT)")
dbGetQuery(con,"CREATE TABLE DimTime(TimeID INT, Year INT, Month INT)")


#Loading the data in R to hive 
DBI::dbWriteTable(conn = con,"Dimcustomers", Dimcustomers,append = TRUE)
DBI::dbWriteTable(conn = con,"DimProduct", DimProduct, append = TRUE)
DBI::dbWriteTable(conn = con,"Dimlocation", Dimlocation, append = TRUE)
DBI::dbWriteTable(conn = con,"TempDimTime", TempDimTime, append = TRUE)
DBI::dbWriteTable(conn = con,"DimTime", DimTime, append = TRUE)
DBI::dbWriteTable(conn = con,"sales_fact", sales_fact, append = TRUE)

#Schema Creation
dbGetQuery(con,'CREATE TABLE Star1 as (SELECT DimProduct.Product_ID, DimProduct.Product_Name, DimProduct.Group, Dimlocation.Country, Dimlocation.Country_Name, Dimlocation.Continent_Name, DimCustomer.Customer_ID, DimCustomer.Customer_Name, DimCustomer.Customer_Type, DimTime.TimeID, DimTime.Year, DimTime.Month, sales_fact.Order_Type, sales_fact.Total_Retail_Price, sales_fact.Quantity, sales_fact.Unit_Cost_Price, sales_fact.Discount INTO Schema1
FROM DimTime INNER JOIN (DimProduct INNER JOIN (Dimlocation INNER JOIN (DimCustomer INNER JOIN sales_fact ON DimCustomer.Customer_ID = sales_fact.Customer_ID) ON Dimlocation.Country = sales_fact.Country) ON DimProduct.Product_ID = sales_fact.Product_ID) ON DimTime.TimeID = sales_fact.TimeID
GROUP BY DimProduct.Product_ID, DimProduct.Product_Name, DimProduct.Group, Dimlocation.Country, Dimlocation.Country_Name, Dimlocation.Continent_Name, DimCustomer.Customer_ID, DimCustomer.Customer_Name, DimCustomer.Customer_Type, DimTime.TimeID, DimTime.Year, DimTime.Month, sales_fact.Order_Type, sales_fact.Total_Retail_Price, sales_fact.Quantity, sales_fact.Unit_Cost_Price, sales_fact.Discount')