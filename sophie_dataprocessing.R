library(corrplot)
library(car)
library(GGally)

money_formatter <- function(x) {
  x <- gsub("â‚¬", "", x)
  x <- gsub("€", "", x)
  x <- case_when(
    grepl("K", x, fixed=TRUE) ~ as.numeric(gsub("K", "", x))*1e3,
    grepl("M", x, fixed=TRUE) ~  as.numeric(gsub("M", "", x))*1e6,
    TRUE ~ as.numeric(x)
  )
  return(x)
}
weight_formatter <- function(x) {
  x <- case_when(
    grepl("kg", x, fixed=TRUE) ~ as.numeric(gsub("kg", "", x)),
    grepl("cm", x, fixed=TRUE) ~  as.numeric(gsub("cm", "", x)),
    TRUE ~ as.numeric(x)
  )
  return(x)
}
isMajor <- function(x, major) {
  x <- case_when(
    x %in% major ~ 1,
    TRUE ~ 0
  )
  return(x)
}

data <- read.csv('data/external/FIFA22_official_data.csv')
dim(data)
glimpse(data)
# analysis only involves goal-keepers
df <- (filter(data, Best.Position == "GK")) #1546 rows
# remove irrelevant string columns
df <- select(df, 
             -ID, -Name, -Photo, 
             -Flag, -Club.Logo, -Jersey.Number, 
             -Position, -Real.Face)

# No NAs in Value, reformatting into number
df$Value <- money_formatter(df$Value)
# No NAs in Wage, reformatting into number
df$Wage <- money_formatter(df$Wage)
# No NAs in Height, reformatting into number
df$Height <- weight_formatter(df$Height)
# No NAs in Weight, reformatting into number
df$Weight <- weight_formatter(df$Weight)
# reformat Release.Clause into number
df$Release.Clause <- money_formatter(df$Release.Clause)
# convert Preferred.Foot into binary variable Preferred.Foot.Right
df$Preferred.Foot[df$Preferred.Foot == "Right"] <- 1
df$Preferred.Foot[df$Preferred.Foot == "Left"] <- 0
df <- rename(df, Preferred.Foot.Right = Preferred.Foot)
df$Preferred.Foot.Right <- as.numeric(df$Preferred.Foot.Right)
# convert contract valid date
df$Contract.Valid.Until <- as.numeric(format(as.Date(df$Contract.Valid.Until, tryFormats=c("%B %d, %Y", "%Y")),"%Y"))
df$Contract.Years <- 2022 - df$Contract.Valid.Until

# NATIONALITY
by_nationality <- tally(df %>% group_by(Nationality))
major_nationality <- by_nationality[by_nationality$n >= 20,]
df$isMajorNationality <- isMajor(df$Nationality, major_nationality$Nationality)

#CLUB
byClub <- tally(df %>% group_by(Club))
major_club <- byClub[byClub$n >= 5,]
dim(major_club)
df$isMajorClub <- isMajor(df$Club, major_club$Club)
table(df$isMajorClub)

df <- select(df,
             Value,
             Age, Overall, Potential,
             isMajorNationality,
             Nationality,
             Club,
             isMajorClub,
             International.Reputation,
             Preferred.Foot.Right,
             Weak.Foot,
             Curve,
             FKAccuracy,
             LongPassing,
             BallControl,
             Acceleration,
             SprintSpeed,
             Agility,
             Reactions,
             Balance,
             ShotPower,
             Jumping,
             Stamina,
             Strength,
             LongShots,
             Aggression,
             Interceptions,
             Positioning,
             Vision,
             Penalties,
             Composure,
             # Marking,
             StandingTackle,
             SlidingTackle,
             GKDiving,
             GKHandling,
             GKKicking,
             GKPositioning,
             GKReflexes,
             DefensiveAwareness,
             Wage,
             Special,
             Height,
             Weight, 
             Contract.Years,
             Release.Clause)
# Release.Clause has 137 NAs
df <- na.omit(df)
# remove 38 rows of value == 0, which are outliers
df <- df[df$Value != 0,]

dim(df)
# save to data/interim folder
write.csv(df, file="data/interim/fifa22_interim_sophie.csv")
# original: 16710 rows
# reduced to 1370