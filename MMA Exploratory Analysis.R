install.packages("pacman")
library(pacman)

pacman::p_load(readr, here, nortest, corrplot, kernlab, fastDummies, magrittr, forcats, ggfortify, kableExtra, janitor, dplyr, stringr, ggplot2, RColorBrewer, knitr, AssocTests, vcd, tidyverse, DT)
mma = read_csv("mma_data_filtered.csv")
mma_master_df = as.data.frame(mma)

######################## Cleaning and preparing data ######################################

######################## Trimming down data to specified columns ######################################
colnames(mma_master_df)
cols_to_keep = c('date', 'result', 'fighter', 'opponent', 'division', 'method', 
                 'sig_strikes_landed', 'sig_strikes_attempts', 'sig_strikes_absorbed',
                 'total_strikes_landed', 'total_strikes_attempts', 'total_strikes_absorbed',
                 'days_since_last_knockout', 'age', 'total_comp_time', 'round', 'time', 'fight_url')

mma_df = mma_master_df[, cols_to_keep]
colnames(mma_df)

####################### Adding column to define whether fighters are male or female ######################################
mma_df <- mma_df %>%
  mutate(sex = ifelse(grepl("Women's", division), "Female", "Male"))

####################### Adding column corresponding to max weight in division #######################
mma_df = mma_df %>% 
  mutate(weightclass = case_when(
    
    division == "Women's Strawweight" ~ 115,
    division == "Women's Flyweight" ~ 125,
    division == "Women's Bantamweight" ~ 135,
    division == "Women's Featherweight" ~ 145,
    division == "Flyweight" ~ 125,
    division == "Bantamweight" ~ 135,
    division == "Featherweight" ~ 145,
    division == "Lightweight" ~ 155,
    division == "Welterweight" ~ 170,
    division == "Middleweight" ~ 185,
    division == "Light Heavyweight" ~ 205,
    division == "Heavyweight" ~ 265,
    TRUE ~ 0
  ))

######################## combining similar values of 'method' into 'method_new' #######################
# unique(mma_df$method) returns all different outcomes of a fight: ("KO/TKO" "SUB"    "DRAW"   "U-DEC"  "S-DEC"  "M-DEC"  "DQ")
# combining all three types of decisions into one "DEC" category using IFELSE
mma_df$method_new = ifelse(mma_df$method %in% c("U-DEC", "S-DEC", "M-DEC"), "DEC", mma_df$method)

################ Look at all data at high level ##################################
# plot different fight outcomes by date and group into 12-month bins

ggplot(data = mma_df) +
  geom_bar(mapping = aes(x = cut(date, "12 months"), fill=method_new)) +
  scale_x_discrete(labels = function(x) format(as.Date(x), "%Y")) +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_y_continuous(limits = c(0, 1220)) +
  annotate("text", x = 7, y = 340, label = "Multiple future stars debut:\nAnderson Silva (2000)\nB.J. Penn (2001)\nGeorges St-Pierre & Nate Diaz (2004)", color = "black", size = 3, angle = 0,  hjust = 0.5, vjust = 0.5) +
  geom_segment(aes(x = 7.2, y = 260, xend = 7, yend = 130), color = "black", arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  geom_segment(aes(x = 7.2, y = 250, xend = 8, yend = 110), color = "black", arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  geom_segment(aes(x = 7.2, y = 250, xend = 10.85, yend = 110), color = "black", arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  annotate("text", x = 10, y = 560, label = "TUF 1 Finale (2005):\nForrest Griffin  	vs. Stephan Bonnar", color = "black", size = 3, angle = 0,  hjust = 0.5, vjust = 0.5) +
  geom_segment(aes(x = 10, y = 510, xend = 12, yend = 200), color = "black", arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  annotate("text", x = 15, y = 810, label = "Conor McGregor &\n Jon Jones debut (2008)", color = "black", size = 3, angle = 0,  hjust = 0.5, vjust = 0.5) +
  geom_segment(aes(x = 15, y = 760, xend = 15, yend = 480), color = "black", arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  annotate("text", x = 27, y = 1220, label = "COVID-19 Pandemic\nBegins", color = "black", size = 3, angle = 0,  hjust = 0.5, vjust = 0.5) +
  geom_segment(aes(x = 27, y = 1180, xend = 27, yend = 1000), color = "black", arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  labs(title = "All MMA Fights by Year and Outcome", x = "Year", y = "Number of Fights", fill = "Fight Outcome")

# explore female MMA fights by year and outcome
# plot different fight outcomes by date and group into 12-month bins

mma_df %>%
  filter(sex == "Female") %>% 
  ggplot() +
    geom_bar(mapping = aes(x = cut(date, "12 months"), fill=method_new)) +
    scale_x_discrete(labels = function(x) format(as.Date(x), "%Y")) +
    theme(axis.text.x = element_text(angle = 45)) +
    scale_y_continuous(limits = c(0, 230)) +
    annotate("text", x = 1.5, y = 135, label = "Ronda Rousey 3-Year\nChampionship Reign Begins", color = "black", size = 3, angle = 0,  hjust = 0.5, vjust = 0.5) +
    geom_segment(aes(x = 1.5, y = 125, xend = 1, yend = 45), color = "black", arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
    annotate("text", x = 8, y = 210, label = "COVID-19 Pandemic\nBegins", color = "black", size = 3, angle = 0,  hjust = 0.5, vjust = 0.5) +
    geom_segment(aes(x = 8, y = 200, xend = 8, yend = 170), color = "black", arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
    labs(title = "Female MMA Fights by Year and Outcome", x = "Year", y = "Number of Fights", fill = "Fight Outcome")

# we can see the Women's MMA started in 2013

############################## MEN'S ANALYSIS ################################################

#################### looking at male fighter data only #######################

mma_df %>%
  filter(sex == "Male") %>% 
  ggplot() +
    geom_bar(mapping = aes(x = cut(date, "12 months"), fill=method_new)) +
    scale_x_discrete(labels = function(x) format(as.Date(x), "%Y")) +
    theme(axis.text.x = element_text(angle = 45)) +
    labs(title = "Male MMA Fights by Year and Outcome", x = "Year", y = "Number of Fights", fill = "Fight Outcome")

# addition of female divisions can explain some increase in fights since 2013, in addition to growing popularity
######################## look at different results by weightclass (division) #######################
# A ***'catch weight'*** is used when two fighters agree to fight at a nonstandard weight 
# for a variety of reasons, such as one fighter being unable to make weight or 
# both fighters agreeing to meet at a mutually agreed-upon weight that falls
# between two established weight classes. This happens in < 0.74% of Male MMA data.
# Since the early 2000s, the ***Super Heavyweight*** and ***Open Weight*** divisions
# have not been recognized by most of the professional MMA organizations. 
# In an Open Weight fight, fighters can have completely different weights, confusing our data. 
# We'll filter this out from our visual analysis. Since the Super Heavyweight division is 
# similarly no longer active and only represents a mere 0.02% of our Male MMA data,
# we'll also exclude this rarity for this next visual analysis. 

---
Women's Strawweight (up to 115 lbs)
Women's Flyweight (up to 125 lbs)
Women's Bantamweight (up to 135 lbs)
Women's Featherweight (up to 145 lbs)
--
Flyweight (up to 125 lbs)
Bantamweight (up to 135 lbs)
Featherweight (up to 145 lbs)
Lightweight (up to 155 lbs)
Welterweight (up to 170 lbs)
Middleweight (up to 185 lbs)
Light Heavyweight (up to 205 lbs)
Heavyweight (up to 265 lbs)
---
# Super Heavyweight, Catch Weight, and Open Weight divisions all represent small portion of the male MMA fights
  
####################### Simple table of division proportions #######################
mma_male_df = mma_df %>% filter(sex == "Male")
division_df = data.frame(Proportion = c(round(prop.table(table(mma_male_df$division)) * 100, 2)))
kable(division_df, "html") %>%
  kable_styling(full_width = FALSE, font_size = 14)
  
# Arranging the men's divisions by ascending order, with weight increasing
mma_male_df$division = factor(mma_male_df$division,
                              levels = c("Flyweight", "Bantamweight", "Featherweight",
                                         "Lightweight", "Welterweight", "Middleweight",
                                         "Light Heavyweight", "Heavyweight"))

# Plot of Men's MMA Fights by Year and Outcome, excluding three (3) aforementioned divisions

ggplot(data = na.omit(mma_male_df)) +
  geom_bar(mapping = aes(x = cut(date, "12 months"), fill=method_new)) +
  facet_wrap(~division, ncol = 4, drop = T) +
  scale_x_discrete(labels = function(x) format(as.Date(x), "%Y")) +
  theme(axis.text.x = element_blank()) +
  labs(title = "Men's MMA Fights by Year and Outcome", x = "Year", y = "Number of Fights", fill = "Fight Outcome")

# Look at the average KO/TKO result in each division, plotted by ascending weightclass

mma_df %>%  
  filter(sex == "Male") %>% 
  filter(!division %in% c("Open Weight", "Super Heavyweight", "Catch Weight")) %>% 
  select(division, method_new, weightclass) %>% 
  group_by(division, weightclass) %>% 
  summarize(pct_ko_tko = (sum(method_new == 'KO/TKO'))/n()) %>% 
  arrange(pct_ko_tko) %>% 
  ggplot(aes(x = reorder(division, weightclass), y = pct_ko_tko)) +
  geom_bar(stat = "identity", fill = "turquoise3") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  labs(title = "Percentage of Male MMA Fights Won by KO/TKO by Weight Division", x = "", y = "% of KO/TKO Outcomes")

################################ Women's Analysis #######################################

#################### Fight Outcomes and Their Relationship with Divisions ##################### 
# looking at female fighter data

mma_df %>%
  filter(sex == "Female") %>% 
  ggplot() +
  geom_bar(mapping = aes(x = cut(date, "12 months"), fill=method_new)) +
  scale_x_discrete(labels = function(x) format(as.Date(x), "%Y")) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Female MMA Fights by Year and Outcome", x = "Year", y = "Number of Fights", fill = "Fight Outcome")

# Create a data frame with division names and their corresponding weights
weightclass_df = data.frame(division = c("Women's Strawweight", "Women's Flyweight", "Women's Bantamweight", "Women's Featherweight", "Flyweight", "Bantamweight", "Featherweight", "Lightweight", "Welterweight", "Middleweight", "Light Heavyweight", "Heavyweight"), sex = c("Female", "Female", "Female", "Female", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male"), weight = c(115, 125, 135, 145, 125, 135, 145, 155, 170, 185, 205, 265))
weightclass_df$division = factor(weightclass_df$division, levels = weightclass_df$division[order(weightclass_df$weight)])

ggplot(weightclass_df, aes(x = weight, y = division, color = sex, fill = sex)) +
  geom_point(stat = "identity", size = 3, shape = 21) +
  geom_segment(x = weightclass_df$weight, xend = weightclass_df$weight, y = weightclass_df$division, yend = weightclass_df$division, size = 1.5) +
  scale_x_continuous(limits = c(100, 300), breaks = seq(100, 300, by = 15)) +
  labs(x = "Weight (lbs)", y = "", title = "Weight Limits of MMA Divisions") +
  theme_minimal() +
  theme(panel.spacing.y = unit(0.1, "lines"))

# Arranging the women's divisions by ascending order, with weight increasing
mma_female_df = mma_df %>%  filter(sex == "Female")
mma_female_df$division = factor(mma_female_df$division,
                                levels = c("Women's Strawweight", "Women's Flyweight",
                                           "Women's Bantamweight", "Women's Featherweight"))

#################### Plot of Women's MMA Fights by Year and Outcome #################### 

ggplot(data = na.omit(mma_female_df)) +
  geom_bar(mapping = aes(x = cut(date, "12 months"), fill=method_new)) +
  facet_wrap(~division, ncol = 2, drop = T) +
  scale_x_discrete(labels = function(x) format(as.Date(x), "%Y")) +
  theme(axis.text.x = element_blank()) +
  labs(title = "Women's MMA Fights by Year and Outcome", x = "Year", y = "Number of Fights", fill = "Fight Outcome")

#### Looking at average KO/TKO result in each women's division, plotted by ascending weightclass 

mma_df %>%  
  filter(sex == "Female") %>% 
  select(division, method_new, weightclass) %>% 
  group_by(division, weightclass) %>% 
  summarize(pct_ko_tko = (sum(method_new == 'KO/TKO'))/n()) %>% 
  arrange(pct_ko_tko) %>% 
  ggplot(aes(x = reorder(division, weightclass), y = pct_ko_tko)) +
  geom_bar(stat = "identity", fill = "salmon") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  labs(title = "Percentage of Female MMA Fights Won by KO/TKO by Weight Division", x = "", y = "% of KO/TKO Outcomes")

######################### Male and Female comparisons #############################

### Looking at correlation between weight of the fighters and likelihood of a knockout

mma_df %>%
  filter(weightclass != 0) %>%
  group_by(weightclass) %>%
  summarize(pct_ko_tko = mean(method_new == "KO/TKO")) %>% 
  ggplot() +
  geom_point(mapping = aes(x = weightclass, y = pct_ko_tko), size = 3) +
  scale_x_continuous(limits = c(100, 300), breaks = seq(100, 300, by = 15)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  labs(title = "Weight of Fighters vs. KO/TKO Probability", x = "Weight of Fighters", y = "% of KO/TKO Outcomes")

#### assume the chances of a fight outcome being a KO/TKO is ???? ######################### 
# compute Cramer's V correlation coefficient

mma_filtered_df = mma_df %>%  filter(weightclass != 0)
assocstats(table(mma_filtered_df$division, mma_filtered_df$method_new))

# comparing male and female fight outcomes side-by-side

mma_df_prop = mma_df %>%
  group_by(sex, method_new) %>%
  summarize(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(method_new = reorder(method_new, -n)) # reorder the method_new variable based on frequency

ggplot(mma_df_prop, aes(x = method_new, y = prop, fill = sex)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Comparing Male and Female Fight Outcomes", x = "Fight Outcome", y = "% of Total Fights", fill = "Gender")

# Only plotting ("Women's Flyweight", "Women's Bantamweight" , "Women's Featherweight", "Flyweight", "Bantamweight" , "Featherweight")

mma_df_prop <- mma_df %>%
  filter(division %in% c("Women's Flyweight", "Women's Bantamweight" , "Women's Featherweight", "Flyweight", "Bantamweight" , "Featherweight")) %>% 
  group_by(sex, method_new) %>%
  summarize(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(method_new = reorder(method_new, -n)) # reorder the method_new variable based on frequency

ggplot(mma_df_prop, aes(x = method_new, y = prop, fill = sex)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Male and Female Fight Outcomes of Same-weight Divisions", x = "Fight Outcome", y = "% of Total Fights", fill = "Gender")

#################### Days since last knockout #####################################

####### what am I doing down below? Can I reasonably filter the outliers in HW and LHW?

########## engineer fields 'days_since_last_fight' and 'ko_tko_losses' ##############
mma_df = mma_df %>%
  arrange(fighter, date) %>%
  group_by(fighter) %>%
  mutate(days_since_last_fight = as.numeric(date - lag(date))) %>%
  mutate(ko_tko_losses = cumsum(ifelse(method_new == "KO/TKO" & result == 0, 1, 0))) %>%
  ungroup()

############### Scatter plot of ko_tko_losses vs. days_since_last_knockout ###########

mma_df %>%
  filter(method_new == "KO/TKO", result == 0, ko_tko_losses > 0, !division %in% c("Super Heavyweight", "Catch Weight", "Open Weight")) %>%
  select(division, days_since_last_knockout, days_since_last_fight, sex, fighter, ko_tko_losses) %>%
  arrange(ko_tko_losses) %>%
  group_by(fighter) %>%
  ggplot(aes(x = ko_tko_losses, y = days_since_last_knockout, fill = ko_tko_losses)) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~ division, ncol = 2) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  labs(x = "KO/TKO Losses", y = "Days Since Last Knockout", fill = "ko_tko_losses") +
  coord_cartesian(ylim = c(0, max(mma_df$days_since_last_knockout, na.rm = TRUE))) +
  scale_x_continuous(breaks = seq(1, 8, 1))

####################### Correlation analysis to see what cols to drop ###############

library(corrplot)

num_cols = sapply(mma_df, is.numeric)
mma_numeric = mma_df[, num_cols]
other_cols = sapply(mma_df, function(x) !is.numeric(x))
other_cols = names(mma_df)[other_cols]
num_cols = colnames(mma_numeric)

mma_df_subset = mma_df[c(num_cols, other_cols)]

correlations = cor(mma_numeric)
# corrplot(correlations, type="full", method="number", tl.cex=0.7) #hiding initial correlation plot

num_cols_drop = c("sig_strikes_attempts", "total_strikes_attempts", "total_strikes_absorbed", "total_strikes_landed", "round")
mma_numeric = mma_numeric %>% select(-one_of(num_cols_drop))
new_corr = cor(mma_numeric)
corrplot(new_corr, type="full", method="number", tl.cex=0.8)

######################## Anderson-Darling test for normality #####################################

ad_results = NULL
for (col in names(mma_numeric)) {
  ad_results = cbind(ad_results, ad.test(mma_numeric[[col]])$p.value)
}
colnames(ad_results) = names(mma_numeric)

# View the p-values
datatable(ad_results)
kable(ad_results) %>% 
kable_styling(full_width = FALSE, font_size = 12)
# p-values are all less than significance values, thus we reject null hypothesis that data is normally distributed

# !!!!!!!!!!!!!!!!! data is NOT normally distributed !!!!!!!!!!!!!!!!!!!!!!!!!!

######################## Test for normality #####################################

num_cols = colnames(mma_numeric)
# Create a 3 by 3 grid of plots
par(mfrow = c(3, 3))

# Loop through each numeric column and create a histogram
for (col in num_cols) {
  hist(mma_numeric[[col]], main = col, xlab = col)
}

########################### Test for Linearity ##################################

par(mfrow = c(6, 6), mar=c(2,2,2,2))
colors <- rainbow(length(num_cols)) # generate a vector of colors

for (i in 1:length(num_cols)) {
  for (j in 1:length(num_cols)) {
    if (i != j) {
      col <- colors[i] # assign color to the variable
      plot(mma_df_subset[[num_cols[i]]], mma_df_subset[[num_cols[j]]], 
           main = paste(num_cols[i], "vs", num_cols[j]), 
           xlab = num_cols[i], ylab = num_cols[j],
           col = col) # use the color for the markers
    }
  }
}


# !!!!!!!!!!!!!!!!! data is NOT normally distributed !!!!!!!!!!!!!!!!!!!!!!!!!!


##################### Nonlinear PCA ###########################################

drop_these_cols = c("result", "fight_url", "result", "fighter", "method", "date", "opponent", "round", "time", "weightclass")

mma_df_subset = mma_df_subset %>% 
  filter(!division %in% c("Open Weight", "Super Heavyweight", "Catch Weight"))

mma_df_test = mma_df_subset %>% select(-one_of(drop_these_cols))

# select categorical columns for one-hot encoding
cat_cols = c("division", "sex", "method_new")

# one-hot encode categorical columns
mma_df_test = fastDummies::dummy_cols(mma_df_test, select_columns = cat_cols)

# apply kernel PCA on numeric columns
mma_numeric = mma_df_test %>% 
  select_if(is.numeric) %>% 
  scale()

#set.seed(123)
#kpca_fit = kernlab::kpca(mma_numeric, kernel = "rbfdot", kpar = list(sigma = 0.1), features = 3)
#kpca_scores = as.data.frame(predict(kpca_fit, mma_numeric))

# Visualize Kernal PCA scores with plot of principal components against each other
#pairs(kpca_scores, main = "Kernel PCA")

##################### Exploring age of fighters and their success rate? ##################### 

##################### Exploring total strikes absorbed over time ##################### 
# create variable for cumulative strikes absorbed per fighter


# group by fighter, show win % vs. cum_strikes_absorbed


# calculate win pct. col

##################### Do fighters have a maximum number of strikes they can take ##################### 


#################################  NOT USED ########################################
############# Create association plots for male and female divisions ###############

# Men's 

mma_filtered_df <- mma_df %>% filter(weightclass != 0, sex == 'Male')
mma_filtered_df$division <- reorder(mma_filtered_df$division, mma_filtered_df$weightclass, FUN = function(x) min(x))
par(las = 2)
assocplot(table(mma_filtered_df$division, mma_filtered_df$method_new), main = "Association Plot of Men's Division vs. Fight Outcome", col= c('black', 'yellow'))

mma_filtered_df <- mma_df %>% filter(weightclass != 0, sex == 'Female')
mma_filtered_df$division <- reorder(mma_filtered_df$division, mma_filtered_df$weightclass, FUN = function(x) min(x))
par(las = 2)
assocplot(table(mma_filtered_df$division, mma_filtered_df$method_new), main = "Association Plot of Women's Division vs. Fight Outcome", col= c('black', 'yellow'))


ct <- table(mma_filtered_df$division, mma_filtered_df$method_new)
model <- loglin(ct, margin = c(1,2))
converged(model)


