Data Clean and Transform
================

``` r
library(tidyverse) #φορτώνω το tidyverse
```

    ## Warning: package 'tidyverse' was built under R version 4.5.2

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.1     ✔ stringr   1.5.2
    ## ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
load("TEST12_RESUMED_4200.RData") #κάνω load τα δεδομένα του Crawler


rm(list = ls()[!sapply(ls(), function(x) is.data.frame(get(x)) || is.function(get(x)))])
rm(ai_themes,basic_info,combined_row,existing_data,wide_ratings) #αφαιρώ ότι δεν είναι απαραίτητο και κρατώ μόνο το final dataframe 

gc() # Καθαρισμός μνήμης (Garbage Collection)
```

    ##           used (Mb) gc trigger  (Mb) max used  (Mb)
    ## Ncells 1207287 64.5    2434435 130.1  2130486 113.8
    ## Vcells 2210621 16.9    8388608  64.0  3683263  28.2

``` r
# Συνάρτηση αυτόματου διαχωρισμού (auto_separate) είναι η ίδια με εκεινή της 3ης εργασίας και θα χρησιμοποιηθεί αργότερα
auto_separate <- function(data, col_to_split, prefix, limit = NULL) {
  col_vec <- data %>% pull({{ col_to_split }})
  max_items <- max(str_count(col_vec, ","), na.rm = TRUE) + 1
  if (is.infinite(max_items) || is.na(max_items)) max_items <- 1
  count_to_use <- if (!is.null(limit)) min(max_items, limit) else max_items
  
  new_names <- paste0(prefix, "_", 1:count_to_use)
  
  data %>%
    separate(
      col = {{ col_to_split }},
      into = new_names,
      sep = ",",
      fill = "right",    
      extra = "drop",    
      remove = TRUE      
    )
}

# Συνάρτηση καθαρισμού Mixed Genres 
#Σκανάρει το Genres για να βρεί mix genres που ταιριάζουν με τα simple Genre 1-3 αλλά έχουν έξτρα info 
#(π.χ. κρατάει το 'Psychological Drama' αν έχουμε ήδη το 'Drama')

clean_mixed_genres <- function(df) {
  df %>%
    rowwise() %>%
    mutate(
      simple_list = list(na.omit(c(Genre_1, Genre_2, Genre_3))),
      split_genres = list(str_trim(str_split(Genres, ",")[[1]])),
      pattern = paste(simple_list, collapse = "|"),
      matches = list(split_genres[str_detect(split_genres, regex(pattern, ignore_case = TRUE))]),
      final_list = list(matches[!(matches %in% simple_list)]),
      Filtered_Genres = paste(final_list, collapse = ", "),
      Filtered_Genres = na_if(Filtered_Genres, "")
    ) %>%
    ungroup() %>%
    select(-simple_list, -split_genres, -pattern, -matches, -final_list)
}



final_data1 <- final_data %>%
  
  filter(!is.na(Rating), !is.na(NumVotes), !is.na(Runtime), !is.na(Simple_Genres)) %>% #κρατώ τα δεδομένα εκείνα που δεν τους λείπει rating,numvotes,runtime,simple_genres καθώς είναι οι κύριες μεταβλητές μου
  replace_na(list(Popularity_Trend = 3, User_Reviews = 0, Critic_Reviews = 0)) %>% # αντικαθιστώ τα NA με κάποιες default τιμές
  
  
  mutate(
    Year_Clean = str_replace_all(Year, "–", "-"),
    Start_Year = as.numeric(str_extract(Year_Clean, "^\\d+")),
    End_Year = as.numeric(case_when(
      str_detect(Year_Clean, "-$") ~ NA_character_,
      str_detect(Year_Clean, "-") ~ str_remove(Year_Clean, ".*-"),
      TRUE ~ as.character(Start_Year))), # καθαρίζω την στήλη Year με σκοπό να έχω στήλες start year και end year 
    
   
    hours_final = coalesce(as.numeric(str_extract(Runtime, "\\d+(?=h)")), 0),
    mins_final  = coalesce(as.numeric(str_extract(Runtime, "\\d+(?=m)")), 0),
    runtimeMinutes = (hours_final * 60) + mins_final, # μετατρέπω την μεταβλητή runtime σε minutes 
    
    
    IMDb_ID = str_extract(URL, "tt\\d+"), # εξάγω το id Κάθε τίτλου από το URL με σκοπό να φτίαξω την μεταβλητή IMDb id με την βοήθεια relative expression
    
   
    Rating = as.numeric(Rating), # το μετατρέπω σε Numeric
    across(starts_with("Percentage"), ~ as.numeric(str_remove(., "%")))) %>% #αφαιρώ το % στην στήλη percentage και κρατώ μόνο τον αριθμό
  
  #Καθαρισμός βοηθητικών στηλών
  select(-Year_Clean, -Year, -hours_final, -mins_final, -Runtime) %>%
  
  #χρήση της συνάρτησης αποό την εργασία 3 για να διαχωρίσω την στήλη Simple Genres
  auto_separate(Simple_Genres, "Genre") %>%
  
  #χρήση της συνάρτησης 2 για να βρώ τα mix
  clean_mixed_genres() %>%
  
  #χρήση της συνάρτησης αποό την εργασία 3 πάλι για να διαχωρίσω την στήλη Genres
  auto_separate(Filtered_Genres, "Mix_Genre") %>%
  
  #Τελική Τακτοποίηση
  select(-Genres) %>%
  relocate(IMDb_ID, .before = Title) %>%
  relocate(URL, Start_Year, End_Year, runtimeMinutes, .after = Title) %>%
  relocate(starts_with("Mix_Genre"), .after = Genre_3)

glimpse(final_data1) #ρίχνω μια ματία στα δεδομένα μου 
```

    ## Rows: 4,078
    ## Columns: 23
    ## $ IMDb_ID          <chr> "tt0773262", "tt14164730", "tt33043892", "tt32252772"…
    ## $ Title            <chr> "Ντέξτερ", "Dexter: New Blood", "Dexter: Resurrection…
    ## $ URL              <chr> "https://www.imdb.com/title/tt0773262/", "https://www…
    ## $ Start_Year       <dbl> 2006, 2021, 2025, 2024, 2008, 2010, 2015, 2019, 2005,…
    ## $ End_Year         <dbl> 2013, 2022, NA, 2025, 2013, 2022, 2022, 2026, 2017, 2…
    ## $ runtimeMinutes   <dbl> 60, 60, 50, 60, 48, 45, 45, 60, 44, 60, 60, 55, 23, 4…
    ## $ Rating           <dbl> 8.6, 8.0, 9.0, 8.2, 9.5, 8.1, 9.0, 8.6, 8.3, 9.2, 8.7…
    ## $ Genre_1          <chr> "Crime", "Crime", "Crime", "Crime", "Crime", "Drama",…
    ## $ Genre_2          <chr> " Drama", " Drama", " Drama", " Drama", " Drama", " H…
    ## $ Genre_3          <chr> " Mystery", " Mystery", " Thriller", " Mystery", " Th…
    ## $ Mix_Genre_1      <chr> "Psychological Drama", "Suspense Mystery", "Psycholog…
    ## $ Mix_Genre_2      <chr> NA, NA, " Psychological Thriller", NA, " Psychologica…
    ## $ Mix_Genre_3      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ Popularity_Trend <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,…
    ## $ User_Reviews     <dbl> 1300, 1500, 886, 362, 5700, 2800, 1900, 3100, 1300, 6…
    ## $ Critic_Reviews   <dbl> 244, 21, 9, 15, 176, 360, 143, 189, 75, 369, 89, 108,…
    ## $ Themes_Positive  <dbl> 6, 0, 5, 5, 7, 6, 10, 4, 5, 9, 4, 9, 2, 4, 1, 9, 5, 7…
    ## $ Themes_Negative  <dbl> 2, 0, 2, 0, 0, 4, 0, 0, 2, 0, 0, 0, 3, 3, 8, 0, 0, 0,…
    ## $ Themes_Neutral   <dbl> 1, 0, 2, 4, 3, 0, 0, 4, 0, 0, 4, 0, 5, 1, 1, 0, 3, 2,…
    ## $ NumVotes         <dbl> 906800, 147388, 124471, 60394, 2512700, 1176900, 7963…
    ## $ Percentage_10    <dbl> 38.3, 25.1, 59.7, 26.8, 67.0, 32.6, 46.2, 29.6, 35.2,…
    ## $ Percentage_5     <dbl> 1.3, 2.6, 0.6, 1.6, 0.6, 2.8, 0.7, 1.1, 1.8, 0.9, 1.0…
    ## $ Percentage_1     <dbl> 1.5, 2.9, 1.6, 1.3, 2.0, 2.3, 0.8, 1.4, 1.2, 2.9, 0.9…

``` r
final_data1<- final_data1 %>%
  mutate(
    count_mix=rowSums(!is.na(select(.,starts_with("Mix_Genre")))),# φτιάχνω μια μεταβλητή που μετρά πόσα mix genre βρέθηκαν
    Era = cut(Start_Year,
             breaks = c(1919, 1979, 1999, 2010, 2019, 2026),
              labels = c("Classic", "Retro", "Millennium", "Digital", "Streaming"),
              include.lowest = TRUE ), # αντίστοιχα φτίαχνω μια μεταβλητή η οποία ανάλογα με το start year κάθε τίτλου του αποδίδει ένα label πχ Retro
    Runtime_Cat = cut(runtimeMinutes,
                      breaks = c(0, 60, 120, 180, Inf),
                      labels = c("Short", "Standard", "Epic", "Marathon"),
                      include.lowest = TRUE)) # κατά τον ίδιο τρόπο φτίαχνω μια μεταβήτή που κάνει κάτι αντιστόχιο αλλά με το runtimeMinutes



final_data2 <- final_data1 %>%
  select(1:10, Popularity_Trend, Critic_Reviews, User_Reviews, NumVotes, count_mix, 
         Themes_Positive, Themes_Negative, Themes_Neutral, Era, Runtime_Cat) %>% # επιλέγω τις μεταβλητές που θέλω να περιέχει το final dataset
  mutate(
    Popularity_Trend = case_when(
      Popularity_Trend == 0 ~ "Down",
      Popularity_Trend == 1 ~ "Up",
      Popularity_Trend == 2 ~ "Neutral",
      TRUE ~ "Unknown"),
    Popularity_Trend = factor(Popularity_Trend, levels = c("Neutral", "Down", "Up", "Unknown")), #μετατρέπω τα 0-2,ΝΑ σε down,up κλπ και έπειτα τα μετατρέπω σε factor
    
    Votes_Per_Year = round(NumVotes / (2026 - Start_Year + 1), 0), # βρίσκω τα votes per year
    engagement_score = User_Reviews / NumVotes,
    net_sentiment_ratio = (Themes_Positive - Themes_Negative) / (Themes_Positive + Themes_Negative + Themes_Neutral + 1), # βρίσκω το δείκτη συναισθηματικής ποιότητας
    
    Genre_2 = as.character(Genre_2),
    Genre_3 = as.character(Genre_3),
    Genre_2 = replace_na(Genre_2, "None"),
    Genre_3 = replace_na(Genre_3, "None"), # μετατρέπω το 2,3 genre σε character και κάνω replace ta ΝΑ με none  
    
    Is_Series = case_when(
      is.na(End_Year) ~ 1,
      Start_Year != End_Year ~ 1,
      Genre_1 %in% c("Reality-TV", "Talk-Show", "Game-Show", "News") ~ 1,
      (Start_Year == End_Year) & (runtimeMinutes <= 60) ~ 1,
      TRUE ~ 0
    )
  ) %>% # φτίαχνω την μεταβλητη is series 1 αμα ειναι σειρα 0 αμα ειναι ταινια
  mutate(
    across(c(Genre_1, Genre_2, Genre_3, Is_Series), as.factor)) # μετατρέπω τα genre1-3 + το is series σε factor




final_data2 <- final_data2 %>%
  select(
    IMDb_ID, Title, Rating,
    Is_Series, Start_Year, End_Year, Era, 
    runtimeMinutes, Runtime_Cat,
    Genre_1, Genre_2, Genre_3, 
    count_mix, 
    NumVotes, Votes_Per_Year,
    Popularity_Trend, engagement_score,
    User_Reviews, Critic_Reviews,
    net_sentiment_ratio, 
    Themes_Positive, Themes_Negative, Themes_Neutral,
    URL) # κάνω μια αναδιάταξη με σκοπό να φαίνεται πιο όμορφο


##################   Basic EDA  #######################


# 1) Κατανομή της Βαθμολογίας
ggplot(final_data2, aes(x = Rating)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "black") +
  theme_minimal() +
  labs(title = "Κατανομή Βαθμολογίας (Rating Distribution)",
       x = "Rating", y = "Πλήθος Τίτλων")
```

![](1_Data-clean-and-transform--2-_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
#Η κατανομή παρουσιάζει ξεκάθαρη αριστερή ασυμμετρία 
#με τον κύριο όγκο των παραγωγών να συγκεντρώνεται σε υψηλές βαθμολογίες.


# 2) Η Εξέλιξη της Βαθμολογίας ανά Εποχή
ggplot(final_data2, aes(x = Era, y = Rating, fill = Era)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Η Εξέλιξη της Βαθμολογίας ανά Εποχή",
       x = "Εποχή", y = "Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
```

![](1_Data-clean-and-transform--2-_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
#Το Streaming είναι η μόνη εποχή που έπεσε κάτω από το 7 σπάζοντας τη σταθερότητα των προηγούμενων 
#Οι πολλές τελείες κάτω από το box δείχνουν ότι πλέον παράγεται μαζικά κακό υλικό που ρίχνει τον μέσο όρο.

# 3) Πλήθος Ταινιών ανά Genre 1
ggplot(final_data2, aes(x = Genre_1)) +
  geom_bar(fill = "red", color = "black") +
  coord_flip() + 
  theme_minimal() +
  labs(title = "Πλήθος Τίτλων ανά Genre 1",
       x = "Είδος", y = "Πλήθος")
```

![](1_Data-clean-and-transform--2-_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->

``` r
# Τα είδη Drama, Action και Comedy κυριαρχούν



# 4) Movies vs Series 10x10 grid 
# !! (την συγκεκριμένη ίδεα την είδα από ένα github repo και ήθελα να την δοκιμάσω σαν extra)
waffle_data <- final_data2 %>%
  count(Is_Series) %>%
  mutate(percent = n / sum(n) * 100,             # Βρίσκουμε το ποσοστό
         Type = ifelse(Is_Series == "0", "Movie", "Series")) # Άμα η τιμή είναι 0 τότε => Movies αλλίως Series

# Φτίαχνω το πλέγμα Grid
#  Φτίαχνω ένα dataframe με 100 θέσεις
grid_data <- expand.grid(x = 1:10, y = 1:10) %>%
  arrange(y, x) %>%
  mutate(Type = rep(waffle_data$Type, times = round(waffle_data$percent))) 


# Χρησιμοποιούμε το grid_data που φτιάξαμε στο προηγούμενο βήμα
#κάθε Grid αποτελεί και 1% του συνόλου

ggplot(grid_data, aes(x = x, y = y, fill = Type)) +
  geom_tile(color = "black", size = 0.8) +  
  coord_equal() +
  
  scale_fill_manual(values = c("Movie" = "#FFD97D", "Series" = "#AAF683")) + 
  
  
  theme_void() + 
    theme(
   
    plot.title = element_text(hjust = 0.1, face = "bold", size = 16, margin = margin(b = 5)),
    legend.title = element_blank(),     
    legend.text = element_text(size = 12), 
    legend.position = "bottom"         
  ) +
  labs(title = "Movies vs Series")
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](1_Data-clean-and-transform--2-_files/figure-gfm/unnamed-chunk-1-4.png)<!-- -->

``` r
#Μας βοηθά να καταλάβουμε την κατανομή της μεταβλητή Is_Series που είναι factor.Στην συγκεκριμένη περίπτωση οι ταινίες είναι περισσότερες
```
