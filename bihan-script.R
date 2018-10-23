library(data.table)
library(stringr)

# load datasets
census <- fread("Census2010_long.txt")
voters <- fread("voter_stats_20161108.txt")

# data manipulation: Make all matching variables same
## Gender
census$Gender[census$Gender == "Female"] = "F"
census$Gender[census$Gender == "Male"] = "M"
## Age
voters$age = str_replace_all(substr(voters$age, 5, nchar(voters$age)), " ", "")
voters$age = str_replace_all(voters$age, "Over66", "66+")
## Race
census$Race[census$Race == "WhiteAlone"]  = "W"
census$Race[census$Race == "BlackAlone"]  = "B"
census$Race[census$Race == "AmericanIndianOrAlaskaNativeAlone"]  = "I"
census$Race[census$Race == "NativeHawaiianOrOtherPacificIslanderAlone"]  = "O"
census$Race[census$Race == "AsianAlone"]  = "A"
census$Race[census$Race == "SomeOtherRaceAlone"]  = "O"
census$Race[census$Race == "TwoOrMoreRaces"]  = "M"
## Ethnicity
census$Hispanic[census$Hispanic == "Hispanic"] = "HL"
census$Hispanic[census$Hispanic == "NotHispanic"] = "NL"

# drop columns that we don't need and rename columns
colnames(voters)[1] <- "county"
voters$election_date <- NULL
voters$stats_type <- NULL
voters$precinct_abbrv <- NULL
voters$vtd_abbrv <- NULL
colnames(voters)[2] <- "party"
colnames(voters)[3] <- "race"
colnames(voters)[4] <- "hisp"
colnames(voters)[5] <- "gender"
colnames(census) <- c("county", "age", "gender", "hisp", "race", "freq", "total_county_population")

# merge datasets
## aggregate both datasets by variables to make each combination unique
voters <- voters %>% 
  group_by(county, party, race, hisp, gender, age) %>% 
  summarise(total_voters = sum(total_voters))
census <- census %>% 
  group_by(county, race, hisp, gender, age) %>% 
  summarise(freq = sum(freq))
## then merge the two datasets 
merged <- left_join(x = voters, y = census, 
                by = c("county", "age", "gender", "race", "hisp"))





