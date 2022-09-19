## import packages
library(dplyr)
library(DBI)
library(RMySQL)
library(odbc)
library(tidyquant)
# 没有daatabase，所以没有dname
host = 'database-mysql.c67oqqqukqcy.ap-southeast-2.rds.amazonaws.com'
port=3306
dbname = "food"
user='admin'
password = 'Qwe1106626229'

mydb <- dbConnect(MySQL(), dbname = dbname,user = user,password = password ,host = host, port = port)

# insert tble，创建database
#dbSendQuery(mydb,"create database food")

# view all the table
#dbListTables(mydb)

# view all the elements in the table,用fetch函数去抓取
result = dbSendQuery(mydb, "select * from new_nuitiion")
dbFetch(result, n = 5)
dbClearResult(result) # must

# select the name_descip for milk
creat_query <- function(column_name, food_name){
 food_name = paste("'", food_name, "'", sep="")
 answer = paste('select',column_name,'from new_nuitiion where Name =',food_name,seq='')
 
 return (answer)
}

food_name = paste("'", 'Beer', "'", sep="")
answer = paste('select * from new_nuitiion where Name =',food_name,seq='')

#answer = creat_query('Protein','Beer')
result = dbSendQuery(mydb, answer)
dbFetch(result)
dbClearResult(result) # must



#break 链接
dbDisconnect(mydb)
