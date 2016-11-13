# Read the companies and rounds2 files into two different dataframes

setwd("F:/Data Analytics/Investment case study 1")

#Load the Companies database in to R for analysis.

companies <- read.delim("companies.txt",header = TRUE, sep = "\t",stringsAsFactors = F)

#Load the Rounds database in to R for analysis.
rounds2 <- read.csv("rounds2.csv",header = TRUE,stringsAsFactors = F)

# counts of unique companies in companies and rounds2 dataframes

companies$permalink <- tolower(companies$permalink)

rounds2$company_permalink <- tolower(rounds2$company_permalink)

count_companies_rounds2 <- length(unique(rounds2$company_permalink))

count_companies <- length(unique(companies$permalink))

# Are there any companies in the rounds2 file which are not present in companies?

permalink_match <- subset(rounds2,!(rounds2$company_permalink %in% companies$permalink))

# we are interested in companies that are not closed, subset the active companies into 
# a data frame

#Since our goal is to throw insight on potential companies, 
#so it is necessary to eliminate the companies which are not operational anymore
active_companies <- subset(companies,companies$status != "closed")


# Normalize the permalink columns in active_companies and rounds2 dataframes
# for merge

active_companies$permalink <- trimws(active_companies$permalink)

rounds2$company_permalink <- trimws(rounds2$company_permalink)

# Merge active_companies and rounds2 dataframes into a master dataframe

master_frame <- merge(rounds2,active_companies,by.x="company_permalink", by.y = "permalink")

# Find total number of NAs in raised_amount_usd in master_frame and replace them with mean   

na_master_frame <- length(which((is.na(master_frame$raised_amount_usd) == "TRUE")))

#All the NA rows from the master_frame have been replaced with Value 1, the reason behind not to eliminate/replace with
#with Mean, if we eliminate we might loose the potential investment sector , if we replace with Mean/Median/Mode
#we might end up investing in wrong sector.

master_frame$raised_amount_usd[which(is.na(master_frame$raised_amount_usd)== T)] <- 1

# Subset master_frame based on Investment type

venture <- subset(master_frame,funding_round_type == "venture")
angel <- subset(master_frame,funding_round_type == "angel")
seed <- subset(master_frame,funding_round_type == "seed")
private_equity <- subset(master_frame,funding_round_type == "private_equity")

# Calculate avearge funding amount for each Investment type

venture_avg_amount <- mean(venture$raised_amount_usd)
angel_avg_amount <- mean(angel$raised_amount_usd)
seed_avg_amount <- mean(seed$raised_amount_usd)
private_equity_avg_amount <- mean(private_equity$raised_amount_usd)

# Top9 countries with chosen Investment type

temp2 <- data.frame(aggregate(venture$raised_amount_usd, by=list(country_code=venture$country_code), FUN=sum))
temp3 <- temp2[order(temp2$x, decreasing = T),]
skip_space_country <-  subset(temp3,temp3$country_code != '')
top9<-skip_space_country[1:9,]
top9_countries <- head(skip_space_country,9)

#Top Country1
top_country1<-top9$country_code[[1]]
#Top Country2
top_country2<-top9$country_code[[3]]
#Top Country3
top_country3<-top9$country_code[[4]]  

# Create a new column "primary_sector" in master_frame and extract first sector from catogery_list

master_frame["primary_sector"] <- NA
master_frame$primary_sector <- sub("\\|.*","",as.character(master_frame$category_list))

# Read the mapping file and merge it with master_frame to get the main sectors

mapping <- read.csv("mapping_file.csv",header = T)
new_master_frame <- merge(master_frame,mapping,by.x = c("primary_sector"),by.y = c("category_list"),all.x = TRUE)

# Remove Blanks in the main_sector column.
#It is important to remove blanks because if we add to any existing sector like "OTHERS" which increases the 
#favourability of investments towards to avoid such misleading numbers, we need to remove them from our data frame.

new_master_frame <- subset(new_master_frame,main_sector != "Blanks")

# Subset the new_master_frame into D1, D2 and D3 based on top countries and chosen Investment type
# and investment range between 5M and 15M

tempA <- subset(new_master_frame,funding_round_type == "venture")
tempB <- subset(tempA,country_code == "USA")
tempC <- subset(tempB,raised_amount_usd >= 5000000)
D1 <- subset(tempC,raised_amount_usd <= 15000000)

tempD <- subset(tempA,country_code == "GBR")
tempE <- subset(tempD,raised_amount_usd >= 5000000)
D2 <- subset(tempE,raised_amount_usd <= 15000000)

tempF <- subset(tempA,country_code == "IND")
tempG <- subset(tempF,raised_amount_usd >= 5000000)
D3 <- subset(tempG,raised_amount_usd <= 15000000)

Count_investments_D1 <- nrow(D1)
Count_investments_D2 <- nrow(D2)
Count_investments_D3 <- nrow(D3)


# Add two new columns in each of the dataframes representing count and total amount raised 
# for each main sector

abc <- data.frame(aggregate(D1$company_permalink, by=list(main_sector = D1$main_sector), FUN= length))
abc <- abc[order(abc$x,decreasing = T ),]
abc_D1_top3 <- head(abc,3)
abc_D1_top3["country_code"] <- "USA"
D1 <- merge(D1,abc,by = "main_sector")
names(D1)[names(D1) == 'x'] <- 'main_sector_count'


xyz <- data.frame(aggregate(D1$raised_amount_usd, by=list(main_sector = D1$main_sector), FUN= sum))
D1 <- merge(D1,xyz,by = "main_sector")
names(D1)[names(D1) == 'x'] <- 'main_sector_raised_usd'

abc_D2 <- data.frame(aggregate(D2$company_permalink, by=list(main_sector = D2$main_sector), FUN= length))
abc_D2 <- abc_D2[order(abc_D2$x,decreasing = T ),]
abc_D2_top3 <- head(abc_D2,3)
abc_D2_top3["country_code"] <- "GBR"
D2 <- merge(D2,abc_D2,by = "main_sector")
names(D2)[names(D2) == 'x'] <- 'main_sector_count'

xyz_D2 <- data.frame(aggregate(D2$raised_amount_usd, by=list(main_sector = D2$main_sector), FUN= sum))
D2 <- merge(D2,xyz_D2,by = "main_sector")
names(D2)[names(D2) == 'x'] <- 'main_sector_raised_usd'

abc_D3 <- data.frame(aggregate(D3$company_permalink, by=list(main_sector = D3$main_sector), FUN= length))
abc_D3 <- abc_D3[order(abc_D3$x,decreasing = T ),]
abc_D3_top3 <- head(abc_D3,3)
abc_D3_top3["country_code"] <- "IND"
D3 <- merge(D3,abc_D3,by = "main_sector")
names(D3)[names(D3) == 'x'] <- 'main_sector_count'

xyz_D3 <- data.frame(aggregate(D3$raised_amount_usd, by=list(main_sector = D3$main_sector), FUN= sum))
D3 <- merge(D3,xyz_D3,by = "main_sector")
names(D3)[names(D3) == 'x'] <- 'main_sector_raised_usd'

# Total funding amount in top 3 countries

D1_total_investment <- sum(D1$raised_amount_usd)
D2_total_investment <- sum(D2$raised_amount_usd)
D3_total_investment <- sum(D3$raised_amount_usd)

# Company that received highest funding amount in Country 1 and main sector 1

temp_D1 <- subset(D1,main_sector == "Others")
aggregate_temp_D1 <- aggregate(temp_D1$raised_amount_usd,by = list(temp_D1$company_permalink),FUN = sum)
aggregate_temp_D1 <- aggregate_temp_D1[order(aggregate_temp_D1$x,decreasing = T),]
top_company_permalink <- head(aggregate_temp_D1$Group.1,1)
top_company_name <- unique(temp_D1$name[which(temp_D1$company_permalink == top_company_permalink)])

# Company that received highest funding amount in Country 1 and main sector 2

temp2_D1 <- subset(D1,main_sector == "Social, Finance, Analytics, Advertising")
aggregate_temp2_D1 <- aggregate(temp2_D1$raised_amount_usd,by = list(temp2_D1$company_permalink),FUN = sum)
aggregate_temp2_D1 <- aggregate_temp2_D1[order(aggregate_temp2_D1$x,decreasing = T),]
top_company_permalink2 <- head(aggregate_temp2_D1$Group.1,1)
top_company_name2 <- unique(temp2_D1$name[which(temp2_D1$company_permalink == top_company_permalink2)])


# Company that received highest funding amount in Country 2 and main sector 1

temp_D2 <- subset(D2,main_sector == "Others")
aggregate_temp_D2 <- aggregate(temp_D2$raised_amount_usd,by = list(temp_D2$company_permalink),FUN = sum)
aggregate_temp_D2 <- aggregate_temp_D2[order(aggregate_temp_D2$x,decreasing = T),]
top_company_permalink3 <- head(aggregate_temp_D2$Group.1,1)
top_company_name3 <- unique(temp_D2$name[which(temp_D2$company_permalink == top_company_permalink3)])


# Company that received highest funding amount in Country 2 and main sector 2

temp2_D2 <- subset(D2,main_sector == "Social, Finance, Analytics, Advertising")
aggregate_temp2_D2 <- aggregate(temp2_D2$raised_amount_usd,by = list(temp2_D2$company_permalink),FUN = sum)
aggregate_temp2_D2 <- aggregate_temp2_D2[order(aggregate_temp2_D2$x,decreasing = T),]
top_company_permalink4 <- head(aggregate_temp2_D2$Group.1,1)
top_company_name4 <- unique(temp2_D2$name[which(temp2_D2$company_permalink == top_company_permalink4)])

# Company that received highest funding amount in Country 3 and main sector 1

temp_D3 <- subset(D3,main_sector == "Others")
aggregate_temp_D3 <- aggregate(temp_D3$raised_amount_usd,by = list(temp_D3$company_permalink),FUN = sum)
aggregate_temp_D3 <- aggregate_temp_D3[order(aggregate_temp_D3$x,decreasing = T),]
top_company_permalink5 <- head(aggregate_temp_D3$Group.1,1)
top_company_name5 <- unique(temp_D3$name[which(temp_D3$company_permalink == top_company_permalink5)])

# Company that received highest funding amount in Country 3 and main sector 2

temp2_D3 <- subset(D3,main_sector == "Social, Finance, Analytics, Advertising")
aggregate_temp2_D3 <- aggregate(temp2_D3$raised_amount_usd,by = list(temp2_D3$company_permalink),FUN = sum)
aggregate_temp2_D3 <- aggregate_temp2_D3[order(aggregate_temp2_D3$x,decreasing = T),]
top_company_permalink6 <- head(aggregate_temp2_D3$Group.1,1)
top_company_name6 <- unique(temp2_D3$name[which(temp2_D3$company_permalink == top_company_permalink6)])


# Pie chart representing the fraction of total investments (globally) in venture, 
#seed and private equity and the average amount of investment in each funding type.

install.packages("plotly")
library(plotly)

investment_type <- c("Venture","Seed","Private Equity")

avg_Investment <- c("$10.78M","$0.57M","$600M")

count_investment <- c(nrow(venture),nrow(seed),nrow(private_equity))

percent_count_investment <- round(100*count_investment/sum(count_investment),1) 

Investment_type_analysis <- data.frame(investment_type,avg_Investment,count_investment)

labels1 <- paste(investment_type,"(","Avg_amount =",avg_Investment,")")
investment_df <- data.frame(labels = investment_type, values = count_investment, labels1)
plot1 <-plot_ly(investment_df, labels = labels1,values = values, type = "pie") %>% layout(title = "Investment type Vs Total Number of Investments")


# Bar chart to represent best countries for Investment

countries_investment <- ggplot(top9_countries,aes(x = top9_countries$country_code,y = top9_countries$x, fill = top9_countries$country_code))

plot2 <- countries_investment +geom_bar(stat = "identity") + xlab("Country code") + ylab("Total Amount of Investments") + guides(fill=guide_legend(title="Country code"))


# Bar chart for top3 countries and sectors

countries_sectors <- rbind(abc_D1_top3,abc_D2_top3,abc_D3_top3)
xyz_d3 <- ggplot(countries_sectors,aes(x = countries_sectors$country_code,y = countries_sectors$x,fill = countries_sectors$main_sector))
plot3 <- xyz_d3 + geom_bar(stat = "identity",position = "dodge") + xlab("Country code") + ylab("Investments Count") + guides(fill=guide_legend(title="Main Sector")) 




