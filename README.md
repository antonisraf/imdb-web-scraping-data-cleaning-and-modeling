# IMDb Data Analysis & Statistical Modeling

This project is a study on the factors that influence IMDb ratings. It covers the entire process from data collection to statistical analysis using R.

## 📊 Process Overview

### 1. Data Collection ([Script](./01-scraping/imdb_spider_scraper.R))
[cite_start]I built a custom web scraper using `rvest` and `httr` to collect data from IMDb[cite: 4, 5]. 
* [cite_start]**Method**: The crawler started from the series *Dexter* and navigated through "More like this" links to gather over 4,000 titles[cite: 64, 55].
* [cite_start]**Stability**: To prevent IP bans, I implemented User-Agent spoofing and randomized delays[cite: 47, 49].
* [cite_start]**Reliability**: The script saves progress every 200 entries, allowing it to resume if the connection drops[cite: 54, 57].

### 2. Data Preparation & EDA ([Report](./02-cleaning-eda/data_cleaning_eda.md))
[cite_start]Using `tidyverse`, I cleaned the raw data and created new variables to better understand the dataset[cite: 70, 85]:
* [cite_start]**Feature Engineering**: Grouped titles into **Eras** (e.g., Classic, Retro, Streaming) and **Runtime Categories**[cite: 85, 86].
* [cite_start]**Observation**: The data showed a noticeable decline in median ratings during the "Streaming" era compared to previous decades[cite: 101].
* [cite_start]**Format**: Movies and series were nearly balanced, with a slight majority of movies in the final dataset[cite: 112, 113].



### 3. Statistical Modeling ([Results](./03-modeling/rating_prediction.md))
[cite_start]The goal was to model how different variables (genres, duration, engagement) affect the final rating[cite: 118]. I compared four different approaches:

* [cite_start]**GLM**: Established that TV Series have a +1.14 rating advantage over movies[cite: 124].
* [cite_start]**Elastic Net**: Used for feature selection, identifying that certain genres acted more as "noise" than significant predictors[cite: 135].
* [cite_start]**MARS**: Found non-linear tipping points in user engagement through hinge functions[cite: 142, 143].
* [cite_start]**Random Forest**: Achieved the best fit with an **$R^2$ of 47.6%**[cite: 149].

## 📈 Key Findings
* [cite_start]**Engagement vs. Genre**: Behavioral metrics (how much users interact with a title) proved to be more significant predictors of a rating than the genre itself[cite: 150, 159].
* [cite_start]**Duration**: Longer formats (Epic/Marathon) tend to correlate with higher ratings[cite: 125].
* [cite_start]**Format**: TV series consistently hold a statistical advantage in ratings within this dataset[cite: 124, 145].

## 🛠 Tools Used
* [cite_start]**Language**: R [cite: 1]
* **Key Packages**: `tidyverse`, `rvest`, `caret`, `glmnet`, `earth`, `ranger`.
