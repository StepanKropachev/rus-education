# List of required packages
required_packages <- c("readxl", "dplyr", "stringr", "ggplot2", "tidyverse",
                       "tidyr", "scales", "gridExtra", "stringi", "naniar", 
                       "patchwork", "corrplot", "car", "caret", "pROC", 
                       "randomForest")

# Check which packages are not installed and install them
not_installed <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if(length(not_installed) > 0) {
  cat("Installing missing packages:", paste(not_installed, collapse=", "), "\n")
  install.packages(not_installed)
} else {
  cat("All required packages are already installed!\n")
}

# Load the packages
lapply(required_packages, library, character.only = TRUE)

#===============================
# 0. Upload and transform the data
#===============================

# 1. Read the file
df <- read_excel("Rus_Census_Labour_force_grouped_by_status_and_education.xlsx", 
                 skip = 9)

# 2. Convert character columns to UTF-8
df[] <- lapply(df, function(x) {
  if(is.character(x)) {
    return(stri_encode(x, "", "UTF-8"))
  }
  return(x)
})

# 2. Add headers
colnames(df) <- c("region",
                  "all_pop",
                  "ind_edu",
                  "phd_edu",
                  "hig_edu",
                  "mast_edu",
                  "spec_edu",
                  "bac_edu",
                  "not_compl_edu",
                  "prof_edu",
                  "prof_edu_mid_spec",
                  "prof_edu_work",
                  "mid_edu",
                  "mid_edu_gen",
                  "mid_edu_prim",
                  "non_edu",
                  "not_ind_edu")

# 3. Remove rows containing "в том числе:" (= "including:")
df <- df[!grepl("в том числе:", df$region), ]

# 4. Add columns for the settlement type and labour status
df$set_type <- NA
df$status <- NA

# 5. Create mapping dictionaries

# Settlement type
set_type_mapping <- c(
  "Городское население" = "city",
  "Сельское население" = "village",
  "Городское и сельское население" = "total"
)

# Labour status
status_mapping <- c(
  "указавшие статус участия в составе рабочей силы" = "indicated status",
  "рабочая сила" = "labour force",
  "занятые" = "employed",
  "безработные" = "unemployed",
  "не входящие в состав рабочей силы" = "not in labour",
  "из них потенциальная рабочая сила" = "potential labour",
  "не указавшие статус участия в составе рабочей силы" = "not indicated status"
)

# Regions
regions_dict <- c(
  "Центральный федеральный округ" = "Central federal district",
  "Белгородская область" = "Belgorod Oblast",
  "Брянская область" = "Bryansk Oblast",
  "Владимирская область" = "Vladimir Oblast",
  "Воронежская область" = "Voronezh Oblast",
  "Ивановская область" = "Ivanovo Oblast",
  "Калужская область" = "Kaluga Oblast",
  "Костромская область" = "Kostroma Oblast",
  "Курская область" = "Kursk Oblast",
  "Липецкая область" = "Lipetsk Oblast",
  "Московская область" = "Moscow Oblast",
  "Орловская область" = "Oryol Oblast",
  "Рязанская область" = "Ryazan Oblast",
  "Смоленская область" = "Smolensk Oblast",
  "Тамбовская область" = "Tambov Oblast",
  "Тверская область" = "Tver Oblast",
  "Тульская область" = "Tula Oblast",
  "Ярославская область" = "Yaroslavl Oblast",
  "г. Москва" = "Moscow City",
  "Северо-Западный федеральный округ" = "Northwestern federal district",
  "Республика Карелия" = "Republic of Karelia",
  "Республика Коми" = "Komi Republic",
  "Архангельская область" = "Arkhangelsk Oblast",
  "Ненецкий автономный округ" = "Nenets Autonomous Okrug",
  "Вологодская область" = "Vologda Oblast",
  "Калининградская область" = "Kaliningrad Oblast",
  "Ленинградская область" = "Leningrad Oblast",
  "Мурманская область" = "Murmansk Oblast",
  "Новгородская область" = "Novgorod Oblast",
  "Архангельская область без автономного округа" = "Archangelskaya Oblast",
  "Псковская область" = "Pskov Oblast",
  "г. Санкт-Петербург" = "Saint Petersburg City",
  "Южный федеральный округ" = "Southern federal district",
  "Республика Адыгея" = "Republic of Adygea",
  "Республика Калмыкия" = "Republic of Kalmykia",
  "Республика Крым" = "Republic of Crimea",
  "Краснодарский край" = "Krasnodar Krai",
  "Астраханская область" = "Astrakhan Oblast",
  "Волгоградская область" = "Volgograd Oblast",
  "Ростовская область" = "Rostov Oblast",
  "г. Севастополь" = "Sevastopol City",
  "Северо-Кавказский федеральный округ" = "North Caucasian federal district",
  "Республика Дагестан" = "Republic of Dagestan",
  "Республика Ингушетия" = "Republic of Ingushetia",
  "Кабардино-Балкарская Республика" = "Kabardino-Balkarian Republic",
  "Карачаево-Черкесская Республика" = "Karachay-Cherkess Republic",
  "Республика Северная Осетия – Алания" = "Republic of North Ossetia-Alania",
  "Чеченская Республика" = "Chechen Republic",
  "Ставропольский край" = "Stavropol Krai",
  "Приволжский федеральный округ" = "Volga federal district",
  "Республика Башкортостан" = "Republic of Bashkortostan",
  "Республика Марий Эл" = "Mari El Republic",
  "Республика Мордовия" = "Republic of Mordovia",
  "Республика Татарстан" = "Republic of Tatarstan",
  "Удмуртская Республика" = "Udmurt Republic",
  "Чувашская Республика" = "Chuvash Republic",
  "Пермский край" = "Perm Krai",
  "Кировская область" = "Kirov Oblast",
  "Нижегородская область" = "Nizhny Novgorod Oblast",
  "Оренбургская область" = "Orenburg Oblast",
  "Пензенская область" = "Penza Oblast",
  "Самарская область" = "Samara Oblast",
  "Саратовская область" = "Saratov Oblast",
  "Ульяновская область" = "Ulyanovsk Oblast",
  "Уральский федеральный округ" = "Ural federal district",
  "Курганская область" = "Kurgan Oblast",
  "Свердловская область" = "Sverdlovsk Oblast",
  "Тюменская область" = "Tyumen Oblast",
  "Тюменская область без автономных округов" = "Tyumen Oblast without Okrugs",
  "Ханты-Мансийский автономный округ – Югра" = "Khanty-Mansi Autonomous Okrug",
  "Ямало-Ненецкий автономный округ" = "Yamalo-Nenets Autonomous Okrug",
  "Челябинская область" = "Chelyabinsk Oblast",
  "Сибирский федеральный округ" = "Siberian federal district",
  "Республика Алтай" = "Republic of Altai",
  "Республика Тыва" = "Republic of Tyva",
  "Республика Хакасия" = "Republic of Khakassia",
  "Алтайский край" = "Altai Krai",
  "Красноярский край" = "Krasnoyarsk Krai",
  "Иркутская область" = "Irkutsk Oblast",
  "Кемеровская область – Кузбасс" = "Kemerovo Oblast",
  "Новосибирская область" = "Novosibirsk Oblast",
  "Омская область" = "Omsk Oblast",
  "Томская область" = "Tomsk Oblast",
  "Дальневосточный федеральный округ" = "Far Eastern federal district",
  "Республика Бурятия" = "Republic of Buryatia",
  "Республика Саха (Якутия)" = "Republic of Sakha",
  "Забайкальский край" = "Zabaykalsky Krai",
  "Камчатский край" = "Kamchatka Krai",
  "Приморский край" = "Primorsky Krai",
  "Хабаровский край" = "Khabarovsk Krai",
  "Амурская область" = "Amur Oblast",
  "Магаданская область" = "Magadan Oblast",
  "Сахалинская область" = "Sakhalin Oblast",
  "Еврейская автономная область" = "Jewish Autonomous Oblast",
  "Чукотский автономный округ" = "Chukotka Autonomous Okrug"
)

# 6. Function to process rows and extract information
process_rows <- function(df) {
  region_clean <- character(nrow(df))
  set_type <- character(nrow(df))
  status <- character(nrow(df))
  
  current_region <- NA
  current_set_type <- NA
  
  for(i in 1:nrow(df)) {
    row_text <- df$region[i]
    # Check for city names first
    if(grepl("^г\\. ", row_text)) {
      current_region <- row_text
    }
    
    # Check region types
    else if(grepl("федеральный округ|область|край|Республика|автономный округ", 
                  row_text)) {
      current_region <- row_text
    }
    
    # Update set_type
    for(pattern in names(set_type_mapping)) {
      if(grepl(pattern, row_text)) {
        current_set_type <- set_type_mapping[pattern]
        break
      }
    }
    
    # Determine status
    current_status <- NA
    for(pattern in names(status_mapping)) {
      if(grepl(pattern, row_text)) {
        current_status <- status_mapping[pattern]
        break
      }
    }
    
    # Store values
    region_clean[i] <- current_region
    set_type[i] <- current_set_type
    status[i] <- current_status
  }
  
  # Update the dataframe
  df$region <- region_clean
  df$set_type <- set_type
  df$status <- status
  
  return(df)
}

# 7. Process the dataframe
df <- process_rows(df)

# 8. Translate regions
df$region <- sapply(df$region, function(x) {
  translated <- regions_dict[x]
  if(is.na(translated)) x else translated
})

# 9. Remove rows where all three columns (region, set_type, status) are NA
df <- df[!(is.na(df$region) & is.na(df$set_type) & is.na(df$status)), ]

# 10. Convert numeric columns to appropriate type
numeric_cols <- colnames(df)[2:17]
df[numeric_cols] <- lapply(df[numeric_cols], as.numeric)

# 11. Reorder columns
df <- df %>%
  select(region, set_type, status, everything())

# 12. fix some issues with regions:
df$region[is.na(df$region)] <- "Central federal district" # idk why, but gives NAs
df <- df[!df$region == "Tyumen Oblast", ] # we've already have it 'without okrugs'

# Print the first few rows to verify the transformation
head(df)

#===============================
# 1. Initial Data Exploration
#===============================

# 1. The data structure:
str(df)

# 2. The data summary:
summary(df)

# We see something strange in NA values - almost all the columns have the same 
# number of NA values. We should look closer;

# 3. Look at the NA values:
vis_miss(df)

# We should delete rows that are fully NAs - those rows are technical
df <- df %>%
  filter(!if_all(
    -c(region, set_type), 
    is.na
  ))

# We also see that in 'status' column there're some NAs - those are 
# summarizations and we can drop those too:
colSums(is.na(df))
df <- df[!is.na(df$status), ]
colSums(is.na(df))

# Check the rest of the rows that contain NA values:
df[!complete.cases(df), ]

# There're still 66 rows that contain NA data. It wouldn't be right 
# to drop those. The best thing we can do is to replace them with median value,
# with regards to the region's total population.
#
# For NA values, we will:
#
# a. Identify regions with non-NA data for the given metric;
# b. Calculate this metric as a percentage of each region's total population;
# c. Find the median of these percentages across regions;
# d. Fill NA values by applying this median percentage to each region's population;

impute_na_by_population_proportion <- function(df, value_column, 
                                               population_column = "all_pop") {
  # a. Calculate proportion for non-NA regions
  proportions <- df[!is.na(df[[value_column]]), ] %>%
    mutate(proportion = .data[[value_column]] / .data[[population_column]]) %>%
    pull(proportion)
  
  # b. Get median proportion
  median_proportion <- median(proportions, na.rm = TRUE)
  
  # c. Impute NA values using the median proportion
  df[[value_column]] <- ifelse(
    is.na(df[[value_column]]),
    round(df[[population_column]] * median_proportion, 0),
    df[[value_column]]
  )
  
  return(df)
}

# Apply to each column with NAs
columns_with_na <- names(df)[colSums(is.na(df)) > 0]

for(col in columns_with_na) {
  df <- impute_na_by_population_proportion(df, col)
}

summary(df)

#===============================
# 2. Regional Analysis
#===============================

# 1. Put aside the 'federal districts' from the regions
df_dist <- df[grepl("federal district", df$region, ignore.case = TRUE), ]
df <- df[!grepl("federal district", df$region, ignore.case = TRUE), ]

# 2. Total population indicated their labour status by regions
df_status <- df %>%
  filter((grepl("City$", region) & set_type == "city") | 
           (!grepl("City$", region) & set_type == "total"), 
         status == "indicated status") %>%
  arrange(desc(all_pop)) %>%
  mutate(region = reorder(region, all_pop),
         is_city = ifelse(grepl("City$", region), "City", "Region"),
         pop_millions = round(all_pop/1000000, 1))

ggplot(df_status, aes(x = all_pop/1000000, y = region, fill = is_city)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = pop_millions), hjust = -0.2, size = 3) +
  scale_fill_manual(values = c("City" = "#FF9999", "Region" = "#4682B4")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Total Population Indicated Their Labour Status by Region",
       subtitle = "Federal cities shown in red, regions in blue",
       x = "Population (millions)",
       y = NULL) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray50"),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

# 3. Investigate the education levels by region
# Calculate education distribution
edu_cols <- c("phd_edu", "hig_edu", "mast_edu", "spec_edu", "bac_edu", 
              "prof_edu", "mid_edu", "non_edu", "not_ind_edu")

df_edu <- df %>%
  filter((grepl("City$", region) & set_type == "city") | 
           (!grepl("City$", region) & set_type == "total"), 
         status == "indicated status") %>%
  mutate(across(all_of(edu_cols), ~ . / all_pop * 100)) %>%
  select(region, all_of(edu_cols)) %>%
  pivot_longer(cols = edu_cols, 
               names_to = "education",
               values_to = "percentage") %>%
  mutate(education = factor(education, 
                            levels = edu_cols,
                            labels = c("PhD", "Higher", "Master", "Specialist", 
                                       "Bachelor", "Professional", "Middle",
                                       "No Education", "Not Indicated")))

# Create a heatmap
ggplot(df_edu, aes(x = education, y = reorder(region, -percentage), fill = percentage)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Education Level Distribution by Region",
       x = "Education Level",
       y = NULL,
       fill = "Percentage") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(face = "bold", size = 14),
    panel.grid = element_blank()
  )

# 3. Top-10 for highest educated regions and the lowest
df_edu_summary <- df %>%
  filter((grepl("City$", region) & set_type == "city") | 
           (!grepl("City$", region) & set_type == "total"), 
         status == "indicated status") %>%
  mutate(
    higher_edu_percent = (phd_edu + mast_edu + spec_edu + bac_edu) / all_pop * 100,
    is_city = ifelse(grepl("City$", region), "City", "Region")
  )

# Top 10 plot
top_10 <- df_edu_summary %>% 
  top_n(10, higher_edu_percent)

p1 <- ggplot(top_10, aes(x = reorder(region, higher_edu_percent), 
                         y = higher_edu_percent, 
                         fill = is_city)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", higher_edu_percent)), 
            hjust = -0.2, 
            size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("City" = "#FF9999", "Region" = "#4682B4")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Top 10 Regions by Higher Education",
       x = NULL,
       y = "Percentage with Higher Education") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

# Bottom 10 plot
bottom_10 <- df_edu_summary %>% 
  top_n(-10, higher_edu_percent)

p2 <- ggplot(bottom_10, aes(x = reorder(region, -higher_edu_percent), 
                            y = higher_edu_percent, 
                            fill = is_city)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", higher_edu_percent)), 
            hjust = -0.2, 
            size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("City" = "#FF9999", "Region" = "#4682B4")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Bottom 10 Regions by Higher Education",
       x = NULL,
       y = "Percentage with Higher Education") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

gridExtra::grid.arrange(p1, p2, ncol = 2)

#===============================
# 3. Urban vs Rural Analysis
#===============================

# 1. Education Urban vs Rural
education_data <- df %>%
  filter(set_type %in% c("city", "village")) %>%
  group_by(set_type) %>%
  summarise(
    phd = sum(phd_edu) / sum(all_pop) * 100,
    specialist = sum(spec_edu) / sum(all_pop) * 100,
    masters = sum(mast_edu) / sum(all_pop) * 100,
    bachelor = sum(bac_edu) / sum(all_pop) * 100,
    professional = sum(prof_edu) / sum(all_pop) * 100,
    middle = sum(mid_edu) / sum(all_pop) * 100,
    no_education = sum(non_edu) / sum(all_pop) * 100
  ) %>%
  tidyr::pivot_longer(-set_type, names_to = "education_level", 
                      values_to = "percentage")

ggplot(education_data, aes(x = education_level, y = percentage, 
                           fill = set_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3) +
  theme_minimal() +
  labs(
    title = "Education Levels: Urban vs Rural Population",
    x = "Education Level",
    y = "Percentage of Population",
    fill = "Territory Type"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2") +
  ylim(0, max(education_data$percentage) * 1.1)

# 2. Employment villages and cities

# employment
employment_data <- df %>%
  filter(set_type %in% c("city", "village"),
         status == "employed") %>%
  group_by(set_type) %>%
  summarise(total_employed = sum(all_pop))

ggplot(employment_data, aes(x = set_type, y = total_employed/1000000, 
                            fill = set_type)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(title = "Employment Comparison: City vs Village",
       x = "Area Type",
       y = "Number of Employed (Millions)",
       fill = "Area Type") +
  scale_fill_manual(values = c("city" = "#3498db", "village" = "#2ecc71")) +
  theme_minimal() +
  geom_text(aes(label = sprintf("%.1f M", total_employed/1000000)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5)

# unemployment
unemployment_data <- df %>%
  filter(set_type %in% c("city", "village"),
         status == "unemployed") %>%
  group_by(set_type) %>%
  summarise(total_unemployed = sum(all_pop))


ggplot(unemployment_data, aes(x = set_type, y = total_unemployed/1000000, 
                              fill = set_type)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(title = "Unemployment Comparison: City vs Village",
       x = "Area Type",
       y = "Number of Unmployed (Millions)",
       fill = "Area Type") +
  scale_fill_manual(values = c("city" = "#3498db", "village" = "#2ecc71")) +
  theme_minimal() +
  geom_text(aes(label = sprintf("%.1f M", total_unemployed/1000000)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5)

#===============================
# 4. Hypothesis testing
#===============================

# 1. H0: There's no correlation between education and labour status
education_cols <- c("phd_edu", "hig_edu", "mast_edu", "spec_edu", 
                    "bac_edu", "prof_edu", "mid_edu")

analyze_education_status <- function(df) {
  df_status <- df[df$status %in% c("employed", "unemployed", "not in labour"), ]
  
  # one-way ANOVA for each education type
  results <- list()
  
  for(col in education_cols) {
    formula <- as.formula(paste(col, "~ status"))
    aov_result <- aov(formula, data = df_status)
    results[[col]] <- summary(aov_result)
  }
  
  df_long <- df_status %>%
    select(status, all_of(education_cols)) %>%
    pivot_longer(cols = all_of(education_cols),
                 names_to = "education_type",
                 values_to = "count")
  
    p <- ggplot(df_long, aes(x = status, y = count, fill = status)) +
    geom_boxplot() +
    facet_wrap(~education_type, scales = "free_y") +
    theme_minimal() +
    labs(title = "Education Levels by Employment Status",
         y = "Count",
         x = "Status") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(list(anova_results = results, plot = p))
}

results <- analyze_education_status(df)
print(results$anova_results)
print(results$plot)


# 2. H0: Education levels have no significant effect on employment rates
df_total <- df[df$set_type == "total" & df$status == "indicated status", ]
df_employed <- df[df$set_type == "total" & df$status == "employed", ]

df_total$emp_rate <- as.numeric(df_employed$all_pop) / as.numeric(df_total$all_pop)

df_total$phd_pct <- as.numeric(df_total$phd_edu) / as.numeric(df_total$all_pop) * 100
df_total$higher_pct <- as.numeric(df_total$hig_edu) / as.numeric(df_total$all_pop) * 100
df_total$masters_pct <- as.numeric(df_total$mast_edu) / as.numeric(df_total$all_pop) * 100
df_total$bachelors_pct <- as.numeric(df_total$bac_edu) / as.numeric(df_total$all_pop) * 100
df_total$professional_pct <- as.numeric(df_total$prof_edu) / as.numeric(df_total$all_pop) * 100

model <- lm(emp_rate ~ phd_pct + higher_pct + masters_pct + bachelors_pct + professional_pct, 
            data = df_total)

summary(model)

par(mfrow=c(2,2))
plot(model)

vif(model) # Check for multicollinearity

# 3. H0: There is no association between region type (urban/rural) and education levels

# Filter for city and village rows
df_filtered <- df %>%
  filter(set_type %in% c("city", "village")) %>%
  group_by(set_type) %>%
  summarize(
    phd_edu = sum(phd_edu),
    hig_edu = sum(hig_edu),
    mast_edu = sum(mast_edu),
    spec_edu = sum(spec_edu),
    bac_edu = sum(bac_edu),
    prof_edu = sum(prof_edu),
    mid_edu = sum(mid_edu),
    non_edu = sum(non_edu)
  )

# Create contingency table
cont_table <- as.table(as.matrix(df_filtered[, -1]))
rownames(cont_table) <- c("City", "Village")

chi_test <- chisq.test(cont_table)
print(chi_test)

prop_table <- prop.table(cont_table, margin = 1) * 100
print(round(prop_table, 2))

#==========================================
# 5. Supervised prediction of employment based on education
#==========================================

# 1. Prepare the dataset for modeling
prepare_model_data <- function(df) {
  # Filter only employed and unemployed status
  model_df <- df %>%
    filter(status %in% c("employed", "unemployed")) %>%
    filter(set_type == "total")
  
  # Create features
  model_df <- model_df %>%
    group_by(region) %>%
    mutate(
      # Normalization
      phd_ratio = phd_edu / all_pop,
      higher_ratio = hig_edu / all_pop,
      master_ratio = mast_edu / all_pop,
      spec_ratio = spec_edu / all_pop,
      bach_ratio = bac_edu / all_pop,
      prof_ratio = prof_edu / all_pop,
      mid_ratio = mid_edu / all_pop,
      
      # Binary encoding for employment
      is_employed = ifelse(status == "employed", 1, 0)
    ) %>%
    ungroup()
  
  # Select features for modeling
  model_df <- model_df %>%
    select(is_employed, phd_ratio, higher_ratio, master_ratio, 
           spec_ratio, bach_ratio, prof_ratio, mid_ratio)
  
  return(model_df)
}

# Split data and train models
train_models <- function(model_df) {
  
  set.seed(123)
  
  # Create training/test split
  trainIndex <- createDataPartition(model_df$is_employed, p = .8, list = FALSE)
  train_data <- model_df[trainIndex,]
  test_data <- model_df[-trainIndex,]
  
  # Add cross-validation
  ctrl <- trainControl(
    method = "cv",
    number = 10,
    classProbs = TRUE,
    savePredictions = TRUE
  )
  
  # Logistic regression
  logit_model <- glm(is_employed ~ ., 
                     data = train_data, 
                     family = "binomial",
                     control = list(maxit = 10))
  
  # Random Forest
  rf_model <- randomForest(
    as.factor(is_employed) ~ ., 
    data = train_data,
    ntree = 10,
    mtry = sqrt(ncol(train_data)),
    max_depth = 5
  )
  
  # Make predictions
  logit_pred <- predict(logit_model, test_data, type = "response")
  rf_pred <- predict(rf_model, test_data, type = "prob")[,2]
  
  # Calculate ROC curves
  logit_roc <- roc(test_data$is_employed, logit_pred)
  rf_roc <- roc(test_data$is_employed, rf_pred)
  
  # Plot ROC curves
  plot(logit_roc, main = "ROC Curves for Employment Prediction", col = "blue")
  lines(rf_roc, col = "red")
  legend("bottomright", legend = c("Logistic Regression", "Random Forest"),
         col = c("blue", "red"), lwd = 2)
  
  # Feature importance for random forest
  importance_df <- as.data.frame(importance(rf_model))
  importance_df$feature <- rownames(importance_df)
  
  # Plot feature importance
  importance_plot <- ggplot(importance_df, aes(x = reorder(feature, MeanDecreaseGini), 
                            y = MeanDecreaseGini)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    theme_minimal() +
    labs(title = "Feature Importance in Random Forest Model",
         x = "Features",
         y = "Mean Decrease in Gini")
  
  print(importance_plot)
  
  
  return(list(
    logit_model = logit_model,
    rf_model = rf_model,
    logit_auc = auc(logit_roc),
    rf_auc = auc(rf_roc),
    importance_plot = importance_plot
  ))
}

model_df <- prepare_model_data(df)
results <- train_models(model_df)

# Models performance
cat("Logistic Regression AUC:", results$logit_auc, "\n")
cat("Random Forest AUC:", results$rf_auc, "\n")

summary(results$logit_model)

