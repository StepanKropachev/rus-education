# List of required packages
required_packages <- c("readxl", "dplyr", "stringr", "ggplot2", "tidyverse",
                       "tidyr", "scales", "gridExtra", "stringi", "naniar")

# Check which packages are not installed
not_installed <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

# Install missing packages
if(length(not_installed) > 0) {
  cat("Installing missing packages:", paste(not_installed, collapse=", "), "\n")
  install.packages(not_installed)
} else {
  cat("All required packages are already installed!\n")
}

# Load all required packages
lapply(required_packages, library, character.only = TRUE)

#===============================
# 0. Upload and transform the data
#===============================

# 1. Read the Excel file
df <- read_excel("Rus_Census_Labour_force_grouped_by_status_and_education.xlsx", 
                 skip = 9)  # Skip first 9 rows

# Convert character columns to UTF-8
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

# 3. Remove rows containing "в том числе:"
df <- df[!grepl("в том числе:", df$region), ]

# 4. Add new columns
df$set_type <- NA
df$status <- NA

# 5. Create mapping dictionaries
set_type_mapping <- c(
  "Городское население" = "city",
  "Сельское население" = "village",
  "Городское и сельское население" = "total"
)

status_mapping <- c(
  "указавшие статус участия в составе рабочей силы" = "indicated status",
  "рабочая сила" = "labour force",
  "занятые" = "employed",
  "безработные" = "unemployed",
  "не входящие в состав рабочей силы" = "not in labour",
  "из них потенциальная рабочая сила" = "potential labour",
  "не указавшие статус участия в составе рабочей силы" = "not indicated status"
)

# Create regions dictionary
regions_dict <- c(
  "Центральный федеральный округ" = "Central federal district", # Idk why, but the conversion doesn't work with this region
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
  # Create empty vectors to store results
  region_clean <- character(nrow(df))
  set_type <- character(nrow(df))
  status <- character(nrow(df))
  
  current_region <- NA
  current_set_type <- NA
  
  for(i in 1:nrow(df)) {
    row_text <- df$region[i]
    
    # Update region if a new one is found
    if(grepl("федеральный округ|область|край|Республика|автономный округ|город", row_text)) {
      current_region <- row_text
    }
    
    # Update set_type if found
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

# Reorder columns
df <- df %>%
  select(region, set_type, status, everything())

# If it's stupid, but it works - it's genius
df$region[is.na(df$region)] <- "Central federal district"

# Print the first few rows to verify the transformation
head(df)

#===============================
# 1. Initial Data Exploration
#===============================

# 1. Look at the data structure
str(df)

# 2. Look at the data summary
summary(df)

# We see something strange in NA values - almost all the columns have the same 
# number of NA values. We should look closer;

# 3. Look at the NA values
vis_miss(df)

# We should delete rows that are fully NAs - those rows are technical
df <- df %>%
  filter(!if_all(
    -c(region, set_type), 
    is.na
  ))

# We also see that in 'status' column there are some NAs - those are 
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

# Create a function to handle this imputation
impute_na_by_population_proportion <- function(df, value_column, population_column = "all_pop") {
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

# 1. Split the dataset to 'federal districts' and it's contains
df_dist <- df[grepl("federal district", df$region, ignore.case = TRUE), ]
df_reg <- df[!grepl("federal district", df$region, ignore.case = TRUE), ]


