library(tidyverse)
library(data.table)
library(xgboost)
library(caret)
library(magrittr)

numerise_data <- function(data, numeric_columns){
	features <- colnames(data)

	data[is.na(data)] <- 0

	for(f in features) {
		if ((class(data[[f]])=="factor") || (class(data[[f]])=="character")) {
			levels <- unique(data[[f]]) %>% sort()
			data[[f]] <- (factor(data[[f]], levels=levels)) %>% as.numeric()
		}else{
			data[[f]] <- data[[f]] %>% as.numeric()
		}
	}
	data[is.na(data)] <- 0
	return(data)
}

get_folds <- function(data, group, v = 5) {
	group_folds <- group_vfold_cv(data[group], group, v = v)
	list.zip(tr = tr_ind <- map(group_folds$splits, ~ .x$in_id),
					 val = val_ind <- map(group_folds$splits, ~ setdiff(1:nrow(data), .x$in_id)))
}

train_meta <- fread("data/training_set_metadata.csv")
train      <- fread("data/training_set.csv")

test_meta  <- fread("data/test_set_metadata.csv")
test       <- fread("data/test_set.csv")



# Preprocessing -----------------------------------------------------------

train_meta %<>%
	mutate(
		target = as.factor(target),
		ddf    = as.factor(ddf)
	)

test_meta %<>%
	mutate(ddf    = as.factor(ddf))

train %<>%
	mutate(
		passband = as.factor(passband),
		detected = as.factor(detected),
		date = as.Date(as.POSIXct('1858-11-17') + (mjd*24*60*60)),
		mjd = NULL
	)

test %<>%
	mutate(
		passband = as.factor(passband),
		detected = as.factor(detected),
		date = as.Date(as.POSIXct('1858-11-17') + (mjd*24*60*60)),
		mjd = NULL
	)

tr <- train_meta %>%
	select(object_id, ra, decl, ddf, hostgal_specz, hostgal_photoz, hostgal_photoz_err, distmod, mwebv, target) %>%
	right_join(train, by = "object_id")

te <- test_meta %>%
	select(object_id, ra, decl, ddf, hostgal_specz, hostgal_photoz, hostgal_photoz_err, distmod, mwebv) %>%
	right_join(test, by = "object_id")

# Save target variable for later
y <- tr$target
tr$target <- NULL
tri <- 1:nrow(tr)

tr_val_ind <- get_folds(train, "object_id", 5)

tr_te <- bind_rows(tr,te) %>%
	numerise_data()

# Preprocessing -----------------------------------------------------------

fn <- funs(mean, median, var, min, max, sum, n_distinct, .args = list(na.rm = TRUE))
agg_tr_te <- tr_te %>%
	group_by(object_id) %>%
	summarise_all(fn)
