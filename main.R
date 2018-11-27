library(tidyverse)
library(data.table)
library(xgboost)
library(caret)
library(magrittr)

library(rlist)
library(rsample)


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
test       <- fread("data/small_test.csv")



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


# XGB ---------------------------------------------------------------------
dtest <- xgb.DMatrix(data = data.matrix(tr_te[-tri, ]))
pred_tr <- rep(0, length(tri))
pred_te <- 0

train_tune <- tr_te[tri,]
train_tune$target <- y_class
tuning_scores <- tune_xgb(train_data = train_tune,
													target_label = "target",
													ntrees = 100,
													objective = "multi:softmax",
													eval_metric = "mlogloss",
													fast = TRUE)
rm(train_tune);gc()
ts.plot(tuning_scores$scores)

m <- which.max(tuning_scores$scores)
currentSubsampleRate <- tuning_scores[["subsample"]][[m]]
currentColsampleRate <- tuning_scores[["colsample_bytree"]][[m]]
lr <- tuning_scores[["lr"]][[m]]
mtd <- tuning_scores[["mtd"]][[m]]
mcw <- tuning_scores[["mcw"]][[m]]

ntrees <- 1e3

p <- list(objective = "multi:softmax",
					booster = "gbtree",
					eval_metric = "mlogloss",
					nthread = 4,
					eta = lr/ntrees,
					max_depth = mtd,
					min_child_weight = 30,
					gamma = 0,
					subsample = currentSubsampleRate,
					colsample_bytree = currentColsampleRate,
					colsample_bylevel = 0.632,
					alpha = 0,
					lambda = 0,
					nrounds = ntrees)

for (i in seq_along(tr_val_ind)) {
	cat("Group fold:", i, "\n")

	tr_ind <- tr_val_ind[[i]]$tr
	val_ind <- tr_val_ind[[i]]$val

	dtrain <- xgb.DMatrix(data = data.matrix(tr_te[tr_ind, ]), label = y_class[tr_ind])
	dval <- xgb.DMatrix(data = data.matrix(tr_te[val_ind, ]), label = y_class[val_ind])

	set.seed(0)
	cv <- xgb.train(p, dtrain, p$nrounds, list(val = dval), print_every_n = 50, early_stopping_rounds = 300)

	pred_tr[val_ind] <- (predict(cv, dval, type = "prob"))
	pred_te <- pred_te + (predict(cv, dtest, type = "prob"))

	rm(dtrain, dval, tr_ind, val_ind)
	gc()
}

pred_te <- ifelse(pred_te < 0, 0, pred_te / length(tr_val_ind))

cols <- colnames(tr_te)
imp <- xgb.importance(cols, model = cv) %>%
	xgb.plot.importance(top_n = 25)

rm(dtest, cv); gc()
