library(explore)
library(stringr)
library(dplyr)

obs <- 10000
data <- create_data_empty(obs = obs) |>
  add_var_random_cat(name = "var1", cat = 1:2) |>
  add_var_random_cat(name = "var2", cat = 1:2) |>
  add_var_random_cat(name = "var3", cat = 1:2) |>
  add_var_random_cat(name = "var4", cat = 1:2) |>
  add_var_random_cat(name = "var5", cat = 1:2) |>
  add_var_random_cat(name = "var6", cat = 1:2) |>
  add_var_random_cat(name = "var7", cat = 1:2) |>
  add_var_random_cat(name = "var8", cat = 1:2) |>
  add_var_random_cat(name = "var9", cat = 1:2) 
  
cat_pull <- function(data, cat9, pos = 1, from_left = TRUE) {
  if (!is.data.frame(data)) { stop("data must be a data.frame") }
  if (!cat9 %in% names(data)) { stop("cat9 variable not found in data") }
  num_str <- sprintf("%09d", data[[cat9]])
  if (!from_left) {
    as.integer(substr(num_str, nchar(num_str)-pos, nchar(num_str)-pos))
  } else {
    as.integer(substr(num_str, pos, pos))
  }
}

cat_init <- function(data, cat9 = "cat9var") {
  if (!is.data.frame(data)) { stop("data must be a data.frame") }
  data[[cat9]] <- rep(999999999, nrow(data))
  data[[cat9]] <- as.integer(data[[cat9]])
  data
}

cat_shrink <- function(data, name, cat9, pos) {
  if (!is.data.frame(data)) { stop("data must be a data.frame") }
  if (!name %in% names(data)) { stop("variable not found in data") }
  if (!cat9 %in% names(data)) { stop("cat9 variable not found in data") }
  if (!is.numeric(data[[name]])) { stop("name must be numeric") }
  if (!is.numeric(data[[cat9]])) { stop("cat9 must be numeric") }
  
  vec <- data[[name]]
  str <- sprintf("%09d", data[[cat9]])
  
  str_new <- paste0(substr(str, 1, pos - 1), 
                    as.character(data[[name]]),
                    substr(str, pos + 1, nchar(str)) )
  data[[cat9]] <- as.integer(str_new)
  
  data
}

cat_expand <- function(data, def, as_label = TRUE) {
  if (!is.data.frame(data)) { stop("data must be a data.frame") }
  if (!def$cat9 %in% names(data)) { stop("cat9 variable not found in data") }
  
  vec <- cat_pull(data = data, cat9 = def$cat9, pos = def$pos)

  if (as_label)  {
    data[[def$name]] <- cat_label(vec, def)
  } else {
    data[[def$name]] <- vec
  }

  data
  
}

data <- data |>
  cat_init("cat9var") |>
  cat_shrink("var1", "cat9var", 1) |>
  cat_shrink("var2", "cat9var", 2) |>
  cat_shrink("var3", "cat9var", 3) |>
  cat_shrink("var4", "cat9var", 4) |>
  cat_shrink("var5", "cat9var", 5) |>
  cat_shrink("var6", "cat9var", 6) |>
  cat_shrink("var7", "cat9var", 7) |>
  cat_shrink("var8", "cat9var", 8) |>
  cat_shrink("var9", "cat9var", 9) |>
  select(cat9var) 
  
data_new <- data.frame(
  var1 = cat_pull(data, "cat9var", pos = 1),
  var2 = cat_pull(data, "cat9var", pos = 2),
  var3 = cat_pull(data, "cat9var", pos = 3),
  var4 = cat_pull(data, "cat9var", pos = 4),
  var5 = cat_pull(data, "cat9var", pos = 5),
  var6 = cat_pull(data, "cat9var", pos = 6),
  var7 = cat_pull(data, "cat9var", pos = 7),
  var8 = cat_pull(data, "cat9var", pos = 8),
  var9 = cat_pull(data, "cat9var", pos = 9)
)  
  
object.size(data)
object.size(data_new)

data |> cat_expand(cat9 = "cat9var", pos = 1, name = "var1") |> head()

cat_def <- function(name, value, label, cat9, pos) {
  def <- list(
    name = name,
    value = value,
    label = label,
    cat9 = cat9,
    pos = pos
  )
  def
}

def_seg <- cat_def(
  name = "seg_cd",
  value = c(1,2,3),
  label = c("RES", "SBS", "AM")
)

cat_label <- function(value, def, add_value = FALSE, sep = "|") {
  
  df1 <- data.frame(id = seq_len(length(value)), value = value)
  df2 <- data.frame(value = def$value, label = def$label)
  dfm <- merge(df1, df2, by = "value", all.x = TRUE)
  dfm <- dfm[order(dfm$id), ]
  if (add_value) {
    result <- paste0(dfm$value, sep, dfm$label)
  } else {
    result <- dfm$label
  }  
  result
}

cat_demo <- function(obs = 200) {

  def_rating_room <- cat_def(
    name = "rating_room",
    value = 1:5,
    label = c("*", "**", "***", "****", "*****"),
    cat9 = "cat9var1",
    pos = 1)
  
  def_rating_service <- cat_def(
    name = "rating_service",
    value = 1:5,
    label = c("*", "**", "***", "****", "*****"),
    cat9 = "cat9var1",
    pos = 2)
  
  def_rating_internet <- cat_def(
    name = "rating_internet",
    value = 1:5,
    label = c("*", "**", "***", "****", "*****"),
    cat9 = "cat9var1",
    pos = 3)
  
  def_rating_restaurant <- cat_def(
    name = "rating_restaurant",
    value = 1:5,
    label = c("*", "**", "***", "****", "*****"),
    cat9 = "cat9var1",
    pos = 4)
  
  def_rating_pool <- cat_def(
    name = "rating_pool",
    value = 1:5,
    label = c("*", "**", "***", "****", "*****"),
    cat9 = "cat9var1",
    pos = 5)
  
  def_room <- cat_def(
    name = "room",
    value = 1:3,
    label = c("Single", "Double", "Family"),
    cat9 = "cat9var1",
    pos = 6
  )
  
  def_language <- cat_def(
    name = "language",
    value = 1:5,
    label = c("English", "Spanish", "French", "German", "Other"),
    cat9 = "cat9var1",
    pos = 7)
  
  def_gender <- cat_def(
    name = "gender",
    value = 1:4,
    label = c("Female", "Male", "Other", "Prefere not to answer"),
    cat9 = "cat9var1",
    pos = 8)
  
  def_duration <- cat_def(
    name = "duration",
    value = 1:3,
    label = c("1-3 days", "4-7 days", "8+ days"),
    cat9 = "cat9var1",
    pos = 9)
  
  hotel_value <- data.frame(
    rating_room = sample(def_rating_room$value, size = obs, prob = c(0.1,0.1,0.2,0.3,0.3), replace = TRUE),
    rating_service = sample(def_rating_service$value, size = obs, prob = c(0.1,0.1,0.2,0.3,0.3), replace = TRUE),
    rating_internet = sample(def_rating_internet$value, size = obs, prob = c(0.1,0.1,0.2,0.3,0.3), replace = TRUE),
    rating_restaurant = sample(def_rating_restaurant$value, size = obs, prob = c(0.1,0.1,0.2,0.3,0.3), replace = TRUE),
    rating_pool = sample(def_rating_pool$value, size = obs, prob = c(0.1,0.1,0.2,0.3,0.3), replace = TRUE),
    room = sample(def_room$value, size = obs, prob = c(0.2,0.6,0.2), replace = TRUE),
    language = sample(def_language$value, size = obs, prob = c(0.2,0.2,0.2,0.2,0.2), replace = TRUE),
    gender = sample(def_gender$value, size = obs, prob = c(0.4,0.4,0.1,0.1), replace = TRUE),
    duration = sample(def_duration$value, size = obs, prob = c(0.6,0.3,0.1), replace = TRUE)
  )
  
  hotel_label <- data.frame(
    rating_room = cat_label(hotel_value$rating_room, def_rating_room, add_value = FALSE),
    rating_service = cat_label(hotel_value$rating_service, def_rating_service, add_value = FALSE),
    rating_internet = cat_label(hotel_value$rating_internet, def_rating_internet, add_value = FALSE),
    rating_restaurant = cat_label(hotel_value$rating_restaurant, def_rating_restaurant, add_value = FALSE),
    rating_pool = cat_label(hotel_value$rating_pool, def_rating_pool, add_value = FALSE),
    room = cat_label(hotel_value$room, def_room, add_value = FALSE),
    language = cat_label(hotel_value$language, def_language, add_value = FALSE),
    gender = cat_label(hotel_value$gender, def_gender, add_value = FALSE),
    duration = cat_label(hotel_value$duration, def_duration, add_value = FALSE)
  )
  
  hotel_cat9 <- hotel_value |>
    cat_init(cat9 = "cat9var1") |>
    cat_shrink("rating_room", "cat9var1", 1) |>
    cat_shrink("rating_service", "cat9var1", 2) |>
    cat_shrink("rating_internet", "cat9var1", 3) |>
    cat_shrink("rating_restaurant", "cat9var1", 4) |>
    cat_shrink("rating_pool", "cat9var1", 5) |>
    cat_shrink("room", "cat9var1", 6) |>
    cat_shrink("language", "cat9var1", 7) |>
    cat_shrink("gender", "cat9var1", 8) |>
    cat_shrink("duration", "cat9var1", 9) 
  
  hotel_cat9 <- data.frame(cat9var = hotel_cat9$cat9var)
  
  hotel_factor <- hotel_label |>
    mutate(rating_room = as.factor(rating_room)) |> 
    mutate(rating_service = as.factor(rating_service)) |> 
    mutate(rating_internet = as.factor(rating_internet)) |> 
    mutate(rating_restaurant = as.factor(rating_restaurant)) |> 
    mutate(rating_pool = as.factor(rating_pool)) |> 
    mutate(room = as.factor(room)) |> 
    mutate(language = as.factor(language)) |> 
    mutate(gender = as.factor(gender)) |> 
    mutate(duration = as.factor(duration)) 
    
  size_label <- format(object.size(hotel_label), units = "Kb")
  size_factor <- format(object.size(hotel_factor), units = "Kb")
  size_value <- format(object.size(hotel_value), units = "Kb")
  size_cat9 <- format(object.size(hotel_cat9), units = "Kb")
  
  list(
    data = list(
      label = hotel_label,
      factor = hotel_factor,
      value = hotel_value,
      cat9 = hotel_cat9
    ),
    def = list(
      def_rating_room = def_rating_room,
      def_rating_service = def_rating_service,
      def_rating_internet = def_rating_internet,
      def_rating_restaurant = def_rating_restaurant,
      def_rating_pool = def_rating_pool,
      def_room = def_room,
      def_language = def_language,
      def_gender = def_gender,
      def_duration = def_duration
    ),
    size = c(
      label = size_label,
      factor = size_factor,
      value = size_value,
      cat9 = size_cat9
    )
  )
  
}



cat_plot <- function(data, def, name) {

#data <- hotel_cat9
#def <- hotel$def$def_rating_pool

if (!missing(def)) {
  values <- cat_pull(data, def$cat9, def$pos)
  labels <- cat_label(values, def, add_value = FALSE)
  if (is.character(labels)) {
    labels <- ifelse(nchar(labels) > 25, paste0(substr(labels, 1, 25), "..."), labels)
  }  
  labels <- paste0("[", values, "]", "\n", labels)
  mytitle <- def$name
  mysubtitle <- paste0("cat9 = ", def$cat9, ", pos = ", def$pos, ", levels = ", length(def$label))
} else {
  labels <- data[[name]]
  if (is.character(labels)) {
    labels <- ifelse(nchar(labels) > 25, paste0(substr(labels, 1, 25), "..."), labels)
  }  
  mytitle <- name
  mysubtitle <- paste0("levels = ", length(table(labels)))
}
  
cat_n <- table(labels)
cat_pct <- prop.table(table(labels)) * 100
left_margin <- 2 + max(nchar(names(cat_pct)))/3.5
par(mar=c(3, left_margin, 3, 1))
barplot(height = cat_pct, 
        horiz = TRUE, las = 1,
        col = "#C1D5E6", border = NA,
        cex.axis=0.8, cex.names=0.8,
        xlim = c(0, max(cat_pct) * 1.2),
        xlab = "percent"
       )
mtext(side=3, line=1.5, at=0, adj=0, cex=1, mytitle)
mtext(side=3, line=0.5, at=0, adj=0, cex=0.7, mysubtitle)

if (length(cat_n) <= 15) {
  text(x = cat_pct, y = 0.68 + ((seq_along(cat_n)-1)*1.2), 
       labels = paste0(
         round(cat_pct,1), "%"),
         #format(cat_n, big.mark = " "), ")"), 
       pos = 4, cex = 0.7)

  }
}


hotel_cat9 |> cat_plot(hotel$def$def_gender)
hotel_label |> cat_plot(name = "gender")
