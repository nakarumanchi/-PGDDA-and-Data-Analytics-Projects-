
#------------------------Investment case study solution------------------------
# Load packages 

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)


## Checkpoint 1: Data Cleaning 1------------------------------------------------

# Loading both files "rounds" and "companies".  

rounds2 <- read.csv("rounds2.csv", stringsAsFactors = F)

companies <- read.delim("companies.txt", stringsAsFactors = F)

#-------------------------------------------------------------------------------

## Check Granularity-------------------------------------------------

#-------------------------------------------------------------------------------

# Convert all permalinks of both the files into lowercase/uppercase.
# Permalink is a unique id for each company
# calculate unique permalinks in rounds2 and companies files.

rounds2$company_permalink <- tolower(rounds2$company_permalink)

no_of_com_permalinks_rounds <- unique(rounds2$company_permalink)
length(no_of_com_permalinks_rounds)# 66368

# now check the number of unique companies in the companies file
companies$permalink <- tolower(companies$permalink)
no_of_company_permalinks <- unique(companies$permalink)
length(no_of_company_permalinks)
# also 66368, seems like all companies in rounds 2 are matching those in companies, 
# but it is possible that the unique 'number of values' are matching but 
# the names may be different

#-------------------------------------------------------------------------------

# check if all values in permalink vector of rounds are present in companies
permalinks_match <- subset(companies, 
                           companies$permalink %in% rounds2$company_permalink)
# thus, all 66368 are matching

# Another way to do the same thing - find records that do not match, it should be 0
permalinks_dont_match <- subset(companies, 
                                !companies$permalink %in% rounds2$company_permalink)

#-------------------------------------------------------------------------------

# merging the two data frames companies and rounds2
# Change the column name in rounds file  
colnames(rounds2)[1] <- "permalink"

# merging the two frames
master_frame <- merge(x = companies, y = rounds2, by = "permalink")

#-------------------------------------------------------------------------------

#Total number of NA values present in column raised_amount_usd
sum(is.na(master_frame$raised_amount_usd))
sum(is.na(master_frame$raised_amount_usd))/nrow(master_frame)
# 17% missing values

# Checkpoint -2 Data Cleaning-2

# Replacing all NA values in raised amount column to 0
master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)==T] <- 0

### or you can use this code for the same
na_indices <- which(is.na(master_frame$raised_amount_usd == T))

# replace NA values with 0
master_frame$raised_amount_usd[na_indices] <- 0
sum(is.na(master_frame$raised_amount_usd))

#-------------------------------------------------------------------------------

## Checkpoint 3: Funding Type Analysis

# look at types of funding

summary(factor(master_frame$funding_round_type))

# Average of venture type funding
venture <- subset(master_frame, funding_round_type=="venture")
mean(venture$raised_amount_usd)

#-------------------------------------------------------------------------------

# Average of angel type funding
angel <- subset(master_frame,funding_round_type=="angel")
mean(angel$raised_amount_usd)

#-------------------------------------------------------------------------------

# Average of seed type funding
seed <- subset(master_frame,funding_round_type=="seed")
mean(seed$raised_amount_usd)

#--------------------------------------------------------------------  
# Average of private_equity type funding
private_equity <- subset(master_frame,funding_round_type=="private_equity")
mean(private_equity$raised_amount_usd)

#-------------------------------------------------------------------------------

# Alternative
fund_types <- filter(master_frame, 
                     funding_round_type == "venture" |
                       funding_round_type == "angel" |
                       funding_round_type == "seed" |
                       funding_round_type == "private_equity")

fund_types %>% 
  group_by(funding_round_type) %>% 
  summarise(avg_fund_by_type = mean(raised_amount_usd)) %>%
  arrange(desc(avg_fund_by_type))


#--------------------------------------------------------------------------------
## Checkpoint 4: Country Analysis

# checking the summary of country_code variable
summary(factor(master_frame$country_code))

# Some of the country codes are blanks:

length(which(master_frame$country_code == ""))
length(which(master_frame$country_code == ""))/nrow(master_frame)
# 7.5%

#-------------------------------------------------------------------------------

# Let's replace the blank cells by "missing" 
# (or any other descriptive value easy to understand)

# Note: This step is very subjective. You can either replace black cells with some 
# arbitrary character name or you could let it be as it is.

master_frame$country_code[which(master_frame$country_code=="")]="missing"

#--------------------------------------------------------------------------------

# Let's aggreate the raised amount by country
highest_funding_countries <- aggregate(raised_amount_usd~country_code, 
                                       venture, sum)

# sorting the above frame
highest_funding_countries_sorted <- highest_funding_countries[order(highest_funding_countries$raised_amount_usd,
                                                                    decreasing = T), ]

#-------------------------------------------------------------------------------

# alternative
venture %>% 
  group_by(country_code) %>% 
  summarise(avg_fund_by_country = sum(raised_amount_usd)) %>%
  arrange(desc(avg_fund_by_country))

#-------------------------------------------------------------------------------

# Extracting top 9 countries (Considering # missing as a country name, 
# doesn't matter if you have not considered missing as a differet country.)

top9 <- head(highest_funding_countries_sorted, 9)

# Check the top 3 english speaking countries from top9 dataframe
#View(top9)

#------------------------------------------------------------------------------- 

### Checkpoint 5: Sector Analysis1----------------------------------------------

# Extracting word before "|" for each sector in the new frame

# creating a vector with primary sector names
master_frame$category_list <- gsub("\\|.*", "", master_frame$category_list)

# alternative 
s11 = str_split_fixed(master_frame$category_list, "[|]", 2)

# checking unique sectors in "master_frame"
#levels(factor(master_frame$category_list))

## Using mapping file "mapping_file.csv" to map priority sector
#  to one of the 8 main sectors

#####################################################################################################

# import mapping file
mapping_file <- read.csv("mapping.csv", header=T, 
                         stringsAsFactors = F,check.names = F)

# Let's glance the data and find out the data quality issues

View(mapping_file) 

# before going ahead, let's change "category_list"column to lowercase. 
mapping_file$category_list <- tolower(mapping_file$category_list)

# Also, changing "category_list" column of master_frame to lowercase 
master_frame$category_list <- tolower(master_frame$category_list)

# You will observe that some of "category_list" strings 
# are spelled wrong such as "0notechnology" [index- 472]should be "nanotechnology" 

#or "0tural Language Processing"[index- 473] should be 
# "natural language processing". 

# But incorrect spelling follows a consistent pattern. You can inspect that the "na" is replaced with "0" in both 
# the above examples and also where the na comes along. 

mapping_file$zeros <- str_detect(mapping_file$category_list, "0")
sum(mapping_file$zeros)

# But detect only detects whether 0 occurs, does not count the number of 0s
mapping_file$zeros <- str_count(mapping_file$category_list, "0")
sum(mapping_file$zeros)


# Let's treat this: 
mapping_file$category_list <- str_replace(mapping_file$category_list, "[0]", "na")
mapping_file$zeros <- str_count(mapping_file$category_list, "0")
sum(mapping_file$zeros)
# Because str_replace replaces only one occurrence, not multiple

# Hence we use str_replace_all
mapping_file$category_list <- str_replace_all(mapping_file$category_list, 
                                              "[0]", "na")
mapping_file$zeros <- str_count(mapping_file$category_list, "0")
sum(mapping_file$zeros)

# However, this may also end up replacing 0 in strings with na, 
# even when we don't want it to.


# "enterprise 2.0" could change to "enterprise 2.na". 
# Let's treat this as well:

mapping_file$category_list[which(mapping_file$category_list=="enterprise 2.na")] <- "enterprise 2.0"

# Converting mapping_file to long format: 

mapping_file <- gather(mapping_file,main_sector,my_val,-category_list)
mapping_file <- mapping_file[which(mapping_file$my_val==1),]
mapping_file$my_val <- NULL
mapping_file <- mapping_file[order(mapping_file$category_list),]

# Before merging, Let's also check the sectors which are not 
# present in the master_frame

# creating new column "check" in master_frame which stores TRUE, if the sector matches with mapping sector list. 
master_frame$check = master_frame$category_list %in% mapping_file$category_list

sum(!master_frame$check)
# 79
# So, 79 observations do not match with the sectors in mapping file. 
# which means 79/114949 i.e 0.068% of rows do not have sectors. 

#-------------------------------------------------------------------------------

# Checking unmapped sectors, i.e. which ones are not present in the mapping file
#levels(factor(master_frame$category_list[which(master_frame$check=="FALSE")]))

# 41 sectors do not match with "master_frame" dataset. 

# merging sector_analysis_frame with mapping_file
sector_analysis_merged <- merge(x = master_frame, y = mapping_file,
                                by = "category_list", all.x = T)

# Let's see the 79 observations in sector_analysis_merged file. 
Nas <- subset(sector_analysis_merged,is.na(sector_analysis_merged$main_sector))

# Best is to assign these NA's with "blanks" 
# since we don't have the business domain expertise necessary to replace them.   
sector_analysis_merged$main_sector[which(is.na(sector_analysis_merged$main_sector))] <- "Blanks"

# Check the levels of main_sector column
#levels(factor(sector_analysis_merged$main_sector))

# Excluding "blanks" sector, there are total 8 different sectors for the analysis
#------------------------------------------------------------------------------ 

##Checkpoint 6: Sector Analysis 2----------------------------------------------

# US, UK and IND analysis (venture type; investments between 5 - 15 million USD)-

# Those who have not considered "Others" as in top list: So the D1, D2 and D3
# are as follows: 
# D1 <- USA
# D2 <- GBR
# D3 <- IND

# But for those who have considered "Others" as in top list: Thus, 

# D1 <- USA
# D2 <- missing
# D3 <- GBR

# The total number (or count) of investments and sum of investment for each 
# main sector in a separate columns

############################################################################## 
#  sector wise - USA Count & USA total amount of investment distribution

USA <- subset(sector_analysis_merged , country_code == "USA" 
              & funding_round_type == "venture" &
                raised_amount_usd >=5e+06 & raised_amount_usd <=15e+06 )

# summarise the count and amount of investment in each sector.
USA_summary <- 
  USA %>% group_by(main_sector) %>% 
  summarise(frequency=n(),investment_by_sector=sum(raised_amount_usd))%>%
  arrange(desc(frequency))


###############################################################################
### Using dplyr package, you can do the both above steps in a single command

USA_1 <- sector_analysis_merged %>% filter(country_code == "USA", 
                                           funding_round_type == "venture", 
                                           raised_amount_usd >= 5e+06, 
                                           raised_amount_usd <= 15e+06) %>% group_by(main_sector) %>% summarise(frequency=n(),investment_by_sector=sum(raised_amount_usd))

###############################################################################

# Let's sort the USA dataframe
USA_summary <- USA_summary[order(USA_summary$frequency, decreasing = T), ]
# or use arrange()

# Total amount invested in USA
sum(USA_summary$investment_by_sector)

# Also, checking company name received the highest investment in top 
# sector i.e "Social, Finance, Analytics, Advertising"

USA__company_summary_1 <- 
  USA %>% 
  filter(main_sector=="Social, Finance, Analytics, Advertising")%>% 
  group_by(name) %>% 
  summarise(frequency=n(),investment_by_company=sum(raised_amount_usd))%>%
  arrange(desc(investment_by_company))

# Also, checking company name received the highest investment in 
# 2nd top sector "Cleantech / Semiconductors" sector

USA__company_summary_2 <- 
  USA %>% 
  filter(main_sector=="Cleantech / Semiconductors")%>% 
  group_by(name) %>% 
  summarise(frequency=n(),investment_by_company=sum(raised_amount_usd))%>% 
  arrange(desc(investment_by_company))

########################################################################
#  sector wise - GBR Count & GBR total amount of investment distribution
#######################################################################

GBR <- subset(sector_analysis_merged , country_code == "GBR" 
              & funding_round_type == "venture" &
                raised_amount_usd >= 5e+06 & raised_amount_usd <= 15e+06 )

# summarise the count and amount of investment in each sector.
GBR_summary <- GBR %>% 
  group_by(main_sector) %>% 
  summarise(frequency=n(),investment_by_sector=sum(raised_amount_usd))%>% 
  arrange(desc(frequency))


###############################################################################
### using dplyr package, you can do the both above steps in a single command

GBR_1 <- sector_analysis_merged %>% 
  filter(country_code == "GBR", funding_round_type == "venture", 
         raised_amount_usd >= 5e+06, 
         raised_amount_usd <= 15e+06) %>% group_by(main_sector) %>% 
  summarise(frequency=n(),investment_by_sector=sum(raised_amount_usd))%>% 
  arrange(desc(frequency))

###############################################################################

# Total amount invested in GBR
sum(GBR_summary$investment_by_sector)

# Also, checking company name received the highest investment in  top 
# sector i.e "Social, Finance, Analytics, Advertising" sector

GBR__company_summary_1 <- 
  GBR %>% 
  filter(main_sector=="Social, Finance, Analytics, Advertising")%>% 
  group_by(name) %>% 
  summarise(frequency=n(),investment_by_company=sum(raised_amount_usd))%>% 
  arrange(desc(investment_by_company))


# Also, checking company name received the highest investment in 
# second sector i.e "Cleantech / Semiconductors" sector

GBR__company_summary_2 <- 
  GBR %>% 
  filter(main_sector=="Cleantech / Semiconductors")%>% 
  group_by(name) %>% 
  summarise(frequency=n(),investment_by_company=sum(raised_amount_usd))%>% 
  arrange(desc(investment_by_company))


#####################################################################
# Sector wise - IND Count & IND total amount of investment distribution
#####################################################################

IND <- subset(sector_analysis_merged , country_code == "IND" 
              & funding_round_type == "venture" &
                raised_amount_usd >= 5e+06 & raised_amount_usd <= 15e+06 )

# summarise the count and amount of investment in each sector.
IND_summary <- IND %>%
  group_by(main_sector) %>% 
  summarise(frequency=n(),investment_by_sector=sum(raised_amount_usd))%>%
  arrange(desc(frequency))

###############################################################################
### using dplyr package, you can do the both above steps in a single command

IND_1 <- sector_analysis_merged %>%
  filter(country_code == "IND", funding_round_type == "venture", raised_amount_usd >= 5e+06, raised_amount_usd <= 15e+06) %>%
  group_by(main_sector) %>% 
  summarise(frequency=n(),investment_by_sector=sum(raised_amount_usd))%>%
  arrange(desc(frequency))


###############################################################################

# Let's sort the USA dataframe
IND_summary <- IND_summary[order(IND_summary$frequency, decreasing = T), ]

# Total amount invested in USA
sum(IND_summary$investment_by_sector)


# Also, checking company name received the highest investment in top sector i.e "Social, Finance, Analytics, Advertising" sector

IND__company_summary_1 <- 
  IND %>% 
  filter(main_sector=="Social, Finance, Analytics, Advertising")%>% 
  group_by(name) %>% 
  summarise(frequency=n(),investment_by_company=sum(raised_amount_usd))%>% 
  arrange(desc(investment_by_company))


# Also, checking company name received the highest investment in 2nd top sector 
# "News, Search and Messaging" sector

IND__company_summary_2 <- 
  IND %>% 
  filter(main_sector=="News, Search and Messaging")%>% 
  group_by(name) %>% 
  summarise(frequency=n(),investment_by_company=sum(raised_amount_usd))%>% 
  arrange(desc(investment_by_company))


#################### END ####################################################

