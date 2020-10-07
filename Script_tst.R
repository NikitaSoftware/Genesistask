#Genesis tst
#Task 1
#readsng data
booster <- read.csv('task1_data.csv')
summary(booster)
#Removing duplicates
cat("The number of non-duplicate observations within the data set is",
    nrow(unique(booster)), "out of", "\n",
    nrow(booster),
    "indicating that there are",
    nrow(booster)-nrow(unique(booster)),
    "duplicates within the data set.","\n",
    "booster2 is our new, duplicate observation free data frame.")

booster2 <- unique(booster)
# Is NA value
cat("There are",
    nrow(booster2[!complete.cases(booster2),]),
    "out of",
    nrow(booster2),
    "total observations that feature at least one NA value.","\n",
    "The CustomerId column alone features all of the",
    nrow(booster2[!complete.cases(booster2$user_id),]),
    "of the total NA values. These", "\n", "observations will be excluded from the new data frame, booster3,
    giving us a new total of",
    nrow(booster2)-nrow(booster2[!complete.cases(booster2),]),
    "observations to work with.")


booster2$purchase_date <- as.Date(booster2$purchase_date, format = "%m/%d/%Y")

# Setting up a column for the year data to make it easier to focus on 2011
booster2$Day <- as.numeric(format(booster2$purchase_date, '%D'))

# Seperating the 2010 observations from the 2011
# observations to focus strictly on the 2011 cohorts
cohorts01 <- booster2[(booster2$Day - 7) < 0]

# Dumping the unneeded variables
cohorts01 <- cohorts01[,c("user_id","purchase_date","Day")]


str(cohorts01)

#LTV


boosters <- read.csv('task1_data.csv')
    filter(between(month_lt, 1, 2) == TRUE & example == 'case03')

activeCust <- c(boosters$product_id)
lostCust <- c(boosters$refunded)

opt <- optim(c(1, 1), MLL)
retention_pred <- round(c(survivalBG(alpha = opt$par[1], beta = opt$par[2], c(3:24))), 3)

df_pred <- data.frame(month_lt = c(3:24),
                      retention_pred = retention_pred)

boosters <- read.csv('task1_data.csv') 
    filter(between(month_lt, 0, 2) == TRUE & example == 'case03') 
    select(month_lt, retention_rate) 
    bind_rows(., df_pred)
    mutate(retention_rate_calc = ifelse(is.na(retention_rate), retention_pred, retention_rate),
           ltv_monthly = retention_rate_calc * 1,
           ltv_cum = round(cumsum(ltv_monthly), 2))
    
    
#CAC
    data(boosters)
    cac <- CAC(boosters$product_id,boosters$trial)
    plot(cac$CACscores,cac$size)#plot scores against Centroid size
    cor.test(cac$CACscores,cac$size)#check for correlation