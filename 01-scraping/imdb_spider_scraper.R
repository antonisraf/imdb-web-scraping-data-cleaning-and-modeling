library(tidyverse)
library(rvest)
library(stringr)
library(httr) 
library(jsonlite)

# ==============================================================================
# 1. ΒΟΗΘΗΤΙΚΕΣ ΣΥΝΑΡΤΗΣΕΙΣ 
# ==============================================================================

get_title_opt <- function(page) {
  tryCatch(page %>% 
             html_element(".hero__primary-text") %>%  # σημείο που βρίσκεται ο κύριος τίτλος
             html_text() %>% #μετατρέπεται σε text 
             str_extract("^[^,]+"), error = function(e) NA) # relative expression από το GEMINI
}

get_year_opt <- function(page) {
  tryCatch(page %>% 
             html_elements(".ipc-inline-list__item") %>% # σημείο που βρίσκεται το έτος
             html_text() %>% #μετατρέπεται σε text 
             str_subset("^\\d{4}") %>% # relative expression που κρατά μόνο το έτος 
             head(1), error = function(e) NA)
}

get_runtime_opt <- function(page) {
  tryCatch({
    page %>%
      html_element(".ipc-inline-list.ipc-inline-list--show-dividers.sc-b41e510f-3.ggypaO.baseAlt.baseAlt") %>% # σημείο που βρίσκεται τo runtime
      html_elements("li") %>% # κρατάω όλα τα list elements
      html_text() %>% #μετατρέπεται σε text 
      str_subset("h|m") %>% #κρατα ότι έχει h(hour) η m(minutes)
      str_extract("\\d+h( \\d+m)?|\\d+m") %>% # relative expressions για να κρατήσω οτιδήποτε της μορφής "2h 15m" (πλήρες), "2h" (σκέτο ώρα) ή "45m" (σκέτο λεπτά)
      head(1)
  }, error = function(e) NA)
}

get_rating_opt <- function(page) {
  tryCatch(page %>%
             html_element(".sc-4dc495c1-0.fUqjJu") %>%  # σημείο που βρίσκεται το rating
             html_text() %>%  #μετατρέπεται σε text 
             str_extract("^[^/]+"), error = function(e) NA) # relative expression για να κρατήσει το rating μόνο του χωρις ότι το ακολουθει πχ "9.0/10124K"
}


get_descriptive_genre_opt <- function(page) {
  tryCatch({
    page %>%
      html_element(".ipc-chip-list__scroller") %>% #σημείο που βρίσκονται τα περιγραφικά genres
      html_elements("a") %>% #κρατά τα anchors
      html_text() %>% #μετατρέπεται σε text 
      head(3) %>% # κρατώ τα πρώτα 3 για να είναι διαχειρίσιμο
      paste(collapse = ", ") # τα διαχωρίζω μεταξύ τους με κόμμα
  }, error = function(e) NA)
}

#Για την συγκεκριμένη συνάρτηση χρησιμοποιήσα αποκλειστικά την βοήθεια του GEMINI καθώς το συγκεκριμένο σημείο δυσκολεύτηκα πολύ να το κάνω extract
get_simple_genre_opt <- function(page) {
  tryCatch({
    # ΣΤΡΑΤΗΓΙΚΗ: JSON-LD (Metadata)
    # Ψάχνουμε το κρυφό script που περιέχει την ταυτότητα της ταινίας/σειράς.
    # Είναι ο μόνος τρόπος να μην μπερδέψεις Genres με Actors/Directors στο static HTML.
    
    json_txt <- page %>% 
      html_element("script[type='application/ld+json']") %>% 
      html_text()
    
    if(is.na(json_txt)) return(NA)
    
    # Parsing του JSON με το πακέτο jsonlite
    meta_data <- jsonlite::fromJSON(json_txt)
    
    # Εξαγωγή του πεδίου 'genre'
    genres <- meta_data$genre
    
    if(is.null(genres)) return(NA)
    
    # Ενώνουμε τα αποτελέσματα (αν είναι πολλά) με κόμμα
    return(paste(genres, collapse = ", "))
    
  }, error = function(e) {
    # Fallback: Αν αποτύχει το JSON, επιστρέφει NA
    return(NA)
  })
}

get_user_reviews_opt <- function(page) {
  
  tryCatch({
    u_rating <- page %>% 
      html_element(".less-than-three-Elements") %>% #σημείο που βρίσκονται τα user reviews
      html_text() #μετατρέπεται σε text 
    
    if(is.na(u_rating)) return(NA) # επιστρέψω ΝΑ άμα δεν υπάρχουν
    
    number <- as.numeric(str_extract(u_rating, "[0-9.]+")) # κρατώ τον σκέτο αριθμό (οχι το user reviews από διπλα) στην μεταβλητή Number
    # Μετατροπή των συντομογραφιών K (χιλιάδες) και M (εκατομμύρια) σε νούμερα
    case_when(
      str_detect(u_rating, "K") ~ number * 1000,
      str_detect(u_rating, "M") ~ number * 1000000,
      TRUE ~ number
    )
  }, error = function(e) NA)
}

get_critic_reviews_opt <- function(page) {
  tryCatch({
    c_rating <- page %>% 
      html_elements(".ipc-inline-list__item.sc-41359969-1.ohASI") %>% #σημείο που βρίσκονται τα critic reviews
      html_elements("a") %>% #κρατά τα anchors
      html_text() %>% #μετατρέπεται σε text 
      str_subset("Critic") #βάζω φίλτρο να κρατήσει μόνο τα critic reviews
    if(length(c_rating) == 0) return(NA) # επιστρέψω ΝΑ άμα δεν υπάρχουν
    
    number <- as.numeric(str_extract(c_rating, "[0-9.]+"))  # κρατώ τον σκέτο αριθμό (οχι το critic reviews από διπλα) στην μεταβλητή Number
    # Μετατροπή των συντομογραφιών K (χιλιάδες) και M (εκατομμύρια) σε νούμερα
    case_when(
      str_detect(c_rating, "K") ~ number * 1000,
      str_detect(c_rating, "M") ~ number * 1000000,
      TRUE ~ number
    )
  }, error = function(e) NA)
}

get_popularity_trend_opt <- function(page) {
  tryCatch({
    trend_val <- NA
    if (!is.na(page %>%
               html_element(".ipc-icon--popularity-neutral"))) {
      trend_val <- 2
    } else if (!is.na(page %>%
                      html_element(".ipc-icon--popularity-up"))) {
      trend_val <- 1
    } else if (!is.na(page %>% 
                      html_element(".ipc-icon--popularity-down"))) {
      trend_val <- 0
    }
    return(trend_val)
  }, error = function(e) NA)
} #ανάλογα με το ποια θέση από τις 3 βρεί αποδίδει μια τιμή 2 άμα το popularity είναι neutral 1 άμα ειναι up 0 άμα είναι down 


get_similar_links_from_page <- function(page) {
  tryCatch({
    hrefs <- page %>%
      html_elements("div.ipc-poster-card__poster a") %>% #σημείο που βρίσκονται τα More like this 
      html_attr("href") %>%
      str_extract("tt[0-9]+") %>% #κρατώ τα tt_____ (Νούμερα)
      unique() # μόνο τα μοναδικά
    
    
    if(length(hrefs) == 0) {
      hrefs <- page %>%
        html_elements(".ipc-shoveler__arrow--next") %>% #Ψάχνουμε σε sliders/shovellers
        html_xpath("//a[contains(@href, '/title/tt')]") %>% #Βρες οποιοδήποτε link περιέχει "/title/tt"
        html_attr("href") %>%
        str_extract("tt[0-9]+") %>% #κρατώ τα tt_____ (Νούμερα)
        unique() # μόνο τα μοναδικά
    }
    
    #ουρά crawler
    hrefs <- hrefs[!is.na(hrefs)]
    if(length(hrefs) > 0) {
      return(paste0("https://www.imdb.com/title/", hrefs, "/"))
    } else {
      return(character(0))
    }
  }, error = function(e) character(0))
}


get_wide_ratings <- function(base_url) {
  clean_url <- str_remove(base_url, "/$")
  ratings_url <- paste0(clean_url, "/ratings/") # Καθαρισμός URL και δημιουργία του link για τη σελίδα των αναλυτικών βαθμολογιών tt___/ratings
  
  
  page <- tryCatch(read_html_live(ratings_url), error = function(e) NULL)
  if(is.null(page)) return(tibble(URL = base_url))
  
  Sys.sleep(1.5) 
  
  ratings_table <- page %>% 
    html_element(".sc-c55fd2d-9.dUHzci") %>% #σημείο που βρίσκεται το γράφημα
    html_elements("text") %>% #κρατώ τα text elements
    html_text() %>% #μετατρέπεται σε text 
    as_tibble() #αποθηκεύω σαν tibble
  
  try(page$session$stop(), silent = TRUE) #Κλείνουμε τον browser για να μην γεμίσει η RAM
  
  if(nrow(ratings_table) < 20) return(tibble(URL = base_url)) #Αν δεν βρήκε αρκετά δεδομένα (π.χ. ταινία χωρίς ψήφους), επιστρέφει κενό
  
  ratings_wide <- ratings_table %>%
    slice(11:20) %>% #Κρατάμε τις γραμμές 11-20 που αντιστοιχούν στα  αστέρια
    mutate(
      Rating = 10:1,
      Percentage = str_extract(value, "[0-9.]+%"),
      Votes = str_extract(value, "(?<=\\().+?(?=\\))") # relative expresion Που βρίσκει κείμενο ανάμεσα στις παρενθέσεις
    ) %>%
    select(Rating, Percentage, Votes) %>%
    pivot_wider(names_from = Rating, values_from = c(Percentage, Votes), names_glue = "{.value}_{Rating}") %>% # γαι να έχω μια εγγραφή ναά ταινία
    mutate(across(starts_with("Votes_"), function(x) {
      num <- as.numeric(str_extract(x, "[0-9.]+"))
      case_when(str_detect(x, "M") ~ num * 1000000, str_detect(x, "K") ~ num * 1000, TRUE ~ num)
    })) %>% # φτιάχνω τις στήλες votes10,9....και percentage10,9....κλπ + Μετατροπή των "K" και "M" σε νούμερα για όλες τις στήλες Votes
    mutate(NumVotes = rowSums(select(., starts_with("Votes_")), na.rm = TRUE)) %>% # Υπολογισμός συνολικών ψήφων
    select(NumVotes, Percentage_10, Percentage_5, Percentage_1) %>% #κρατώ τα numvote,percentage10,5,1 για να είανι διαχειρίσιμο
    mutate(URL = base_url) # επιστρέφω το url στην αρχική του μορφή
  
  return(ratings_wide)
}


get_ai_themes_opt <- function(page) {
  tryCatch({
    
    theme_icons <- page %>% 
      html_elements("[class*='AIThemesChipList_theme_preicon']") %>% #στοχεύω στο σημείο που βρίσκονται τα εικονίδια 
      html_attr("class")
    
    #Αν η ταινία δεν έχει AI Storyline επιστρέφουμε μηδενικά
    if(length(theme_icons) == 0) {
      return(tibble(Themes_Positive = 0, Themes_Negative = 0, Themes_Neutral = 0))
    }
    
    # Χρησιμοποιούμε str_detect για να δούμε πόσες φορές εμφανίζεται η λέξη positive/negative/neutral
    pos_count <- sum(str_detect(theme_icons, "theme-positive"))
    neg_count <- sum(str_detect(theme_icons, "theme-negative"))
    neu_count <- sum(str_detect(theme_icons, "theme-neutral"))
    
    return(tibble(
      Themes_Positive = pos_count,
      Themes_Negative = neg_count,
      Themes_Neutral = neu_count
    )) # επιστρέφω σαν Tibble
    
  }, error = function(e) {
    message("Error finding themes")
    return(tibble(Themes_Positive = 0, Themes_Negative = 0, Themes_Neutral = 0)) #Σε περίπτωση λάθους επιστρέφουμε μηδενικά για να μην κρασάρει ο crawler.
  })
}

# ==============================================================================
# 2. ΣΥΝΑΡΤΗΣΗ CRAWLER
# ==============================================================================
run_spider_crawler_fast <- function(start_url, max_movies = 50) {
  
  queue_urls <- c(start_url)
  visited_urls <- c()
  final_data <- tibble()
  counter <- 0 
  
  # User-Agent
  ua <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36")
  
  while(length(queue_urls) > 0 && counter < max_movies) {
    
   
    # Αφαιρώ μαζικά οτι έχουμε ήδη επισκεφτεί
    queue_urls <- setdiff(queue_urls, visited_urls)
    
    # Αν αδειάσει η ουρά μετά τον καθαρισμό, βάλε backup
    if(length(queue_urls) == 0) {
      message("--> Queue Empty after cleanup! Injecting Backup Link...")
      queue_urls <- c("https://www.imdb.com/title/tt2741602/") # Backup link
    }
    
    current_url <- queue_urls[1]
    queue_urls <- queue_urls[-1] # Αφαιρούμε το πρώτο
    
    # Delay 
    if (counter > 0) {
      sleep_time <- runif(1, 2.5, 3.5) 
      Sys.sleep(sleep_time)
    }
    
    counter <- counter + 1
  
    message(paste("[", counter, "/", max_movies, "] Connecting to:", current_url, "..."))
    
    # Timeout στα 15 δευτερόλεπτα μήπως το site αργεί να απαντήσει
    resp <- tryCatch(GET(current_url, ua, timeout(15)), error = function(e) NULL)
    
    if(!is.null(resp) && status_code(resp) == 200) {
      
      message("   --> Downloaded. Parsing HTML...") 
      page <- read_html(resp)
      
      # --- ΕΚΤΕΛΕΣΗ ΒΑΣΙΚΩΝ ΣΥΝΑΡΤΗΣΕΩΝ ---
      title      <- get_title_opt(page)
      
      if(is.na(title)) { 
        message("   --> Skipped (No title found)")
        visited_urls <- c(visited_urls, current_url) # Το μαρκάρουμε ως visited για να μην ξαναπροσπαθήσει
        rm(page)
        next 
      }
      
      year      <- get_year_opt(page)
      runtime   <- get_runtime_opt(page)
      rating    <- get_rating_opt(page)
      
      descriptive_genre <- get_descriptive_genre_opt(page)
      simple_genre      <- get_simple_genre_opt(page)
      
      u_rev     <- get_user_reviews_opt(page)
      c_rev     <- get_critic_reviews_opt(page)
      pop_trend <- get_popularity_trend_opt(page)
      ai_themes <- get_ai_themes_opt(page) 
      
      # Ενημέρωση ουράς (Μόνο μοναδικά links)
      new_links <- get_similar_links_from_page(page)
      # Κρατάμε μόνο αυτά που δεν είναι visited και δεν είναι ήδη στην ουρά
      new_unique_links <- setdiff(new_links, visited_urls)
      new_unique_links <- setdiff(new_unique_links, queue_urls)
      
      # Προσθέτουμε τα νέα links
      queue_urls <- c(queue_urls, new_unique_links)
      
      wide_ratings <- tryCatch(get_wide_ratings(current_url), error = function(e) tibble(URL = current_url))
      
      basic_info <- tibble(
        Title = title, 
        Year = year, 
        Runtime = runtime, 
        Rating = rating, 
        Genres = descriptive_genre,       
        Simple_Genres = simple_genre,     
        Popularity_Trend = pop_trend, 
        User_Reviews = u_rev, 
        Critic_Reviews = c_rev, 
        URL = current_url
      )
      
      basic_info <- bind_cols(basic_info, ai_themes)
      combined_row <- left_join(basic_info, wide_ratings, by = "URL")
      
     
      combined_row$Genres <- as.character(combined_row$Genres)
      combined_row$Simple_Genres <- as.character(combined_row$Simple_Genres)
      combined_row$Year <- as.character(combined_row$Year)
      combined_row$Runtime <- as.character(combined_row$Runtime)
      
      final_data <- bind_rows(final_data, combined_row)
      visited_urls <- c(visited_urls, current_url)
      
      rm(page) 
      
      # ---ΚΑΘΕ 50 ΕΓΓΡΑΦΕΣ ΚΑΝΕ HUMAN BREAK & GC ---
      if(counter %% 50 == 0) {
        gc(verbose = FALSE) 
        hb_time <- runif(1, 3, 5) 
        message(paste("--- HUMAN BREAK: Pausing for", round(hb_time, 2), "s (RAM Cleaned) ---"))
        Sys.sleep(hb_time)
      }
      
      # --- SAVE CHECKPOINT ---
      # ΣΩΖΩ IMAGE ANA 200 ΕΓΓΡΑΦΕΣ
      if(counter %% 200 == 0) {
        filename <- paste0("~/TEST12_", counter, ".RData")
        message(paste("--- SAFETY CHECKPOINT: Saving to", filename, "---"))
        gc(verbose = FALSE)
        save(list = ls(all.names = TRUE), file = filename, envir = environment())
      }
      
      message(paste("   --> Done. Queue Size:", length(queue_urls)))
      
    } else {
      message("   --> Failed to download (Status not 200 or Error).")
      visited_urls <- c(visited_urls, current_url) # Μαρκάρισμα ως visited
    }
  }
  
  return(final_data)
}

# ==============================================================================
# ΣΥΝΑΡΤΗΣΗ ΓΙΑ ΣΥΝΕΧΙΣΗ (RESUME) ΑΠΟ RDATA
# ==============================================================================
run_spider_resume <- function(existing_data, existing_queue, existing_visited, movies_to_add = 500) {
  
  # Φορτώνουμε τα υπάρχοντα δεδομένα στις τοπικές μεταβλητές
  final_data <- existing_data
  queue_urls <- existing_queue
  visited_urls <- existing_visited
  
  # Υπολογίζουμε πού είμαστε και πού θέλουμε να φτάσουμε
  current_count <- nrow(final_data)
  target_count <- current_count + movies_to_add
  
  message(paste("--> RESUMING CRAWLER..."))
  message(paste("--> Already have:", current_count, "movies."))
  message(paste("--> Target:", target_count, "movies."))
  message(paste("--> Queue size:", length(queue_urls)))
  
  # User-Agent
  ua <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36")
  
  # Το Loop τρέχει μέχρι να φτάσουμε τον ΣΤΟΧΟ (όχι από το 0)
  while(length(queue_urls) > 0 && nrow(final_data) < target_count) {
    
    # 1. ΚΑΘΑΡΙΣΜΟΣ ΟΥΡΑΣ
    queue_urls <- setdiff(queue_urls, visited_urls)
    
    if(length(queue_urls) == 0) {
      message("--> Queue Empty! Injecting Backup Link...")
      queue_urls <- c("https://www.imdb.com/title/tt2741602/") 
    }
    
    current_url <- queue_urls[1]
    queue_urls <- queue_urls[-1] 
    
    # Delay
    sleep_time <- runif(1, 2.5, 3.5) 
    Sys.sleep(sleep_time)
    
  
    counter <- nrow(final_data) + 1
    message(paste("[", counter, "/", target_count, "] Scraping:", current_url))
    
    resp <- tryCatch(GET(current_url, ua, timeout(15)), error = function(e) NULL)
    
    if(!is.null(resp) && status_code(resp) == 200) {
      
      page <- read_html(resp)
      
      # --- ΕΚΤΕΛΕΣΗ ΒΑΣΙΚΩΝ ΣΥΝΑΡΤΗΣΕΩΝ ---
      title <- get_title_opt(page)
      
      if(is.na(title)) { 
        message("   --> Skipped (No title)")
        visited_urls <- c(visited_urls, current_url)
        rm(page)
        next 
      }
      
      # Λήψη πεδίων
      year      <- get_year_opt(page)
      runtime   <- get_runtime_opt(page)
      rating    <- get_rating_opt(page)
      descriptive_genre <- get_descriptive_genre_opt(page)
      simple_genre      <- get_simple_genre_opt(page)
      u_rev     <- get_user_reviews_opt(page)
      c_rev     <- get_critic_reviews_opt(page)
      pop_trend <- get_popularity_trend_opt(page)
      ai_themes <- get_ai_themes_opt(page) 
      
      # Ενημέρωση ουράς
      new_links <- get_similar_links_from_page(page)
      new_unique_links <- setdiff(new_links, visited_urls)
      new_unique_links <- setdiff(new_unique_links, queue_urls)
      queue_urls <- c(queue_urls, new_unique_links)
      
      wide_ratings <- tryCatch(get_wide_ratings(current_url), error = function(e) tibble(URL = current_url))
      
      # Δημιουργία γραμμής
      basic_info <- tibble(
        Title = title, 
        Year = year, 
        Runtime = runtime, 
        Rating = rating, 
        Genres = descriptive_genre,       
        Simple_Genres = simple_genre,     
        Popularity_Trend = pop_trend, 
        User_Reviews = u_rev, 
        Critic_Reviews = c_rev, 
        URL = current_url
      )
      
      basic_info <- bind_cols(basic_info, ai_themes)
      combined_row <- left_join(basic_info, wide_ratings, by = "URL")
      
      
      combined_row$Genres <- as.character(combined_row$Genres)
      combined_row$Simple_Genres <- as.character(combined_row$Simple_Genres)
      combined_row$Year <- as.character(combined_row$Year)
      combined_row$Runtime <- as.character(combined_row$Runtime)
      
      final_data <- bind_rows(final_data, combined_row)
      visited_urls <- c(visited_urls, current_url)
      
      rm(page) 
      
      # ---ΚΑΘΕ 50 ΕΓΓΡΑΦΕΣ ΚΑΝΕ HUMAN BREAK & GC ---
      if(counter %% 50 == 0) {
        gc(verbose = FALSE) 
        hb_time <- runif(1, 3, 5) 
        message(paste("--- HUMAN BREAK: Pausing for", round(hb_time, 2), "s ---"))
        Sys.sleep(hb_time)
      }
      
      # --- SAVE CHECKPOINT ---
      # ΣΩΖΩ IMAGE ANA 200 ΕΓΓΡΑΦΕΣ
      if(counter %% 200 == 0) {
        filename <- paste0("~/TEST12_RESUMED_", counter, ".RData")
        message(paste("--- SAFETY CHECKPOINT: Saving to", filename, "---"))
        gc(verbose = FALSE)
        # Σώζουμε τα πάντα (και τα παλιά και τα νέα)
        save(list = ls(all.names = TRUE), file = filename, envir = environment())
      }
      
    } else {
      message("   --> Failed to download.")
      visited_urls <- c(visited_urls, current_url)
    }
  }
  
  return(final_data)
}

# ==============================================================================
# 3. ΕΚΤΕΛΕΣΗ
# ==============================================================================

start_link <- "https://www.imdb.com/title/tt0773262/" 
page <- read_html_live(start_link)   

df_dataset <- run_spider_crawler_fast(start_link, max_movies = 3) # αρχική συνάρτηση 



updated_data <- run_spider_resume(final_data, queue_urls, visited_urls, movies_to_add = 4) # συνάρτηση σε περιπτώση που κολλήσει και έχω image για να μην ξεκινήσει από την αρχή
