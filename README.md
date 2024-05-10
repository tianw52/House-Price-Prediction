# House-Price-Prediction
* Predicting house price using random forest model and smoothing spline model.
* Both models are built using `dtrain` and tested using `dtest`

# Results:
The comparison of the two models is outlined in this document. Detailed pre-processing and model-building processes can be found in the corresponding folders.
## Smoothing Spline

## Random Forest

# Note:
* `R` is the primary language
* The same pre-processing is applied for both models since the datasets have the same variables (differ in observation).However, some new variables are only used in random forest (interation 1-6) but not in smoothing spline. Specific pre-processing can be found in the markdown/pdf in corresponding folders.


# Data Description
## Variable Description (after pre-processing)
**13 Factors:**
* heat: type of heat used in the house
* ac: whether the house has air conditioning or not
* style: describes the number of stories and/or structure of the house
* grade: overall rating of the house
* cndtn: condition of the house
* extwall: material used for exterior wall
* roof: type of roof
* intwall: material used for interior wall
* nbhd: ID of the neighborhood the house belongs to
* ward: ID of the ward the house belongs to
* quadrant: quadrant the house belongs to
* if_rmdl: whether the house has been re-modeled ever
* buy_first: indicator variable that has value of 1 if the house was bought before build

**6 Integers:**
* rooms: total number of rooms
* bathrm: number of full bathrooms (shower + toilet)
* bedrm: number of bedrooms
* eyb: the year an improvement was built
* kitchens: number of kitchens
* fireplaces: number of fireplaces

**19 Numerical:**
* ayb: the earliest time the main portion of the building was built
* stories: number of stories in the primary dwelling
* saledate: date of sale as numerical values
* price (response): price of the house
* gba: gross building area in square feet
* landarea: land area of property in square feet
* latitude: latitude of the house
* longitude: longitude of the house
* saleyear: year the house sold
* rmdl_diff: the difference between the sale year and the re-model year, if re-model is done after sale,
then the value is 0
* avg_room_size: average size of the room in sqre feet
* build_age: how long the house has been built
* total_bath: total number of full bathrooms and half bathrooms
* inter1: interaction between latitude and saledate (used in random forest only)
* inter2: interaction between longitude and saledate (used in random forest only)
* inter3: interaction between gba and saledate (used in random forest only)
* inter4: interaction between landarea and longitude (used in random forest only)
* inter5: interaction between eyb and ayb (used in random forest only)
* inter6: interaction between latitude and build_age (used in random forest only)

## Numerical Variables
```
excluded_vars <- c("inter1", "inter2", "inter3", "inter4",
                   "inter5", "inter6", "fold")

plot_histograms <- function(df, exclude_vars = NULL,
                            bin_count = 30) {
  if (!is.null(exclude_vars)) {
    df <- select(df, -all_of(exclude_vars))
  }
  numeric_df <- df[sapply(df, is.numeric)]

  long_df <- pivot_longer(numeric_df, cols = everything(),
                          names_to = "Column", values_to = "Value")

  p <- ggplot(long_df, aes(x = Value)) +
    geom_histogram(bins = bin_count, fill = "orange", color = "black") +
    facet_wrap(~ Column, scales = "free") +
    theme_minimal() +
    theme(plot.title = element_text(size = 10, face = "bold"),
          axis.text = element_text(size = 6),
          axis.title = element_text(size = 6)) +
    labs(title = "Histograms for Numeric variables", x = "Value", y = "Count")

  return(p)
}

plot_histograms(dat_full, exclude_vars = excluded_vars)
```

<img width="842" alt="Screenshot 2024-05-10 at 14 36 59" src="https://github.com/tianw52/House-Price-Prediction/assets/129543727/3ce2fc2a-0ac3-4c20-8ef4-e73e98643434">

```
plot_numeric <- function(data, target_var, exclude_vars) {
  numeric_vars <- sapply(data, is.numeric)
  numeric_vars[exclude_vars] <- FALSE
  plots <- list()  
  for (var in names(numeric_vars)[numeric_vars]) {
    if (var != target_var) {  
      p <- ggplot(data, aes_string(x = var, y = target_var)) +
        geom_point(alpha = 0.5, col = "steelblue") +  
        geom_smooth(method = "lm", color = "orange") +
        labs(title = paste( target_var, "vs", var),
             x = var,
             y = target_var) +
        theme(plot.title = element_text(size = 10),
              axis.text = element_text(size = 6),
              axis.title = element_text(size = 6))

      plots[[var]] <- p  
    }
  }

  plot_layout <- Reduce(`+`, plots) + 
                 plot_layout(guides = 'collect')
  print(plot_layout)
}

excluded_vars <- c("inter1", "inter2", "inter3", "inter4", "inter5", "inter6", "fold")
plot_numeric(dat_full, "price", excluded_vars)
```
<img width="842" alt="Screenshot 2024-05-10 at 14 37 41" src="https://github.com/tianw52/House-Price-Prediction/assets/129543727/bf72c34a-8096-4441-a1c9-42324ba70521">

All the numerical variables other than `longtitude` have a positive relationship with price.

## Geographic

```
ggplot(dat_ori, aes(x = longitude, y = latitude, color = price, size = price)) +
  geom_point(alpha = 0.5, shape = 15) + 
  scale_color_gradient(low = "lightblue", high = "firebrick") +  
  ggtitle("Geospatial Distribution of Price") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_minimal() 
```
<img width="842" alt="Screenshot 2024-05-10 at 14 41 18" src="https://github.com/tianw52/House-Price-Prediction/assets/129543727/ea28d0c8-572c-4b9c-ab27-ec3e89670f5c">


Using latitude and longitude values, we see area around longitude = -74.2 and latitude = 40.725 has higher price, 
which indicates location is an important factor in house price.

## Categorical Variables
```
plot_cate <- function(data, target_var) {
  factor_vars <- sapply(data, is.factor)
  plots <- list()
  
  for (var in names(factor_vars)[factor_vars]) {
      p <- ggplot(data, aes_string(x = var, y = target_var)) +
          geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +
          labs(title = paste(target_var, "vs", var),
               x = var, y = target_var) +
          theme(plot.title = element_text(size = 10),
                axis.text = element_text(size = 6),
                axis.title = element_text(size = 6),
                axis.text.x = element_text(angle = 45, hjust = 1))
      plots[[var]] <- p
  }
  
  group1 <- plots[1:min(7, length(plots))]  
  group2 <- if (length(plots) > 5) plots[6:length(plots)] else NULL 

  if (!is.null(group1)) {
    plot_group1 <- wrap_plots(group1)
    print(plot_group1)
  }
  if (!is.null(group2)) {
    plot_group2 <- wrap_plots(group2)
    print(plot_group2)
  }
}

plot_cate(data = dat_full, target_var = "price")
```
<img width="842" alt="Screenshot 2024-05-10 at 14 42 44" src="https://github.com/tianw52/House-Price-Prediction/assets/129543727/14d29748-28ba-4d29-bfb6-a734b4167706">
<img width="842" alt="Screenshot 2024-05-10 at 14 42 55" src="https://github.com/tianw52/House-Price-Prediction/assets/129543727/6528f126-a041-4cf4-963c-ff2ba5742441">

It can be seen that some variables has obvious different impact on prices based on their levels. Such variables are 

* `cndtn`: the better the condition, the higher the price.
* `grade`: the better the rating, the higher the price.
* `ward`: houses located in ward 2 and 3 have higher prices while house located in ward 7 and 8 have the lowest prices.
* `quadrant`: houses located in northwest tend to have higher prices.




