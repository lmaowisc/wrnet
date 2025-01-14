
# Get minimum of a vector, return Inf if vector is null or all elements are NA
# Handles edge cases where the input vector is null or entirely NA.
min_na_inf <- function(x){
  if(all(is.na(x)) | is.null(x)){
    # Return Inf if input is null or all elements are NA
    return(Inf)
  } else {
    # Otherwise, return the minimum value, ignoring NA
    return(min(x, na.rm = TRUE))
  }
}


# Extract first time where status == k by time t
# Outputs Inf if no such time exists
# Arguments:
# - time: Vector of time points
# - status: Vector of statuses corresponding to time points
# - k: The specific status of interest
# - t: Upper limit of time (default Inf)
extact_time_with_status <- function(time, status, k, t = Inf){
  # Filter times where status equals k and time is within the limit, then find the minimum
  time[status == k & time <= t] |> min_na_inf()
}


# Transform outcomes data by standard win function (Pocock's setting)
# Groups data by subject and converts long format to wide format
# Then computes pairwise wins and losses
poc_win <- function(df_y, n){
  df_y_wide <- df_y |>
    group_by(id) |>
    reframe(
      results = long_to_wide_death_nf(time, status)
    ) |>
    mutate(
      names = rep(c("death_time", "nonfatal_time", "fu_time"), n)
    ) |>
    pivot_wider(
      names_from = names,
      values_from = results
    )

  # Generate all pairwise combinations
  pairwise_tibble <- crossing(i = df_y_wide, j = df_y_wide) %>%
    filter(i |> row_number() < j |> row_number()) |>
    unnest(c(i, j), names_sep = "_")


  # Compute pairwise wins and losses
  pw_win <- pairwise_tibble |>
    mutate(
      win_death = case_when(
        j_death_time < pmin(i_death_time, i_fu_time) ~ 1,
        i_death_time < pmin(j_death_time, j_fu_time) ~ -1,
        TRUE ~ 0
      ),
      win_nonfatal = if_else(win_death == 0, case_when(
        j_nonfatal_time < pmin(i_nonfatal_time, i_fu_time) ~ 1,
        i_nonfatal_time < pmin(j_nonfatal_time, j_fu_time) ~ -1,
        TRUE ~ 0
      ), 0),
      win = win_death + win_nonfatal,
      component = case_when(
        win_death != 0 ~ "death",
        win_nonfatal != 0 ~ "nonfatal",
        TRUE ~ "tie"
      )
    ) |>
    select(
      id_i = i_id,
      id_j = j_id,
      win,
      component
    ) |>
    filter(win != 0) # Remove pairs with win == 0

  # Remove pairs with win == 0 in pairwise tibble as well
  # by left joining with
  pairwise_tibble <- pw_win |> select(id_i, id_j) |>
    left_join(pairwise_tibble, by = c("id_i" = "i_id", "id_j" = "j_id"))

  list(pairwise_tibble = pairwise_tibble, pw_win = pw_win)
}



# Reorganize long outcomes (death, nonfatal, censor) to wide format
# Returns a vector with death time, nonfatal event time, and follow-up time
long_to_wide_death_nf <- function(time, status, deathcode = 1, nfcode = 2, t = Inf){

  fu_time <- min(max(time), t)
  death_time <- extact_time_with_status(time, status, deathcode, fu_time)
  nonfatal_time <- extact_time_with_status(time, status, nfcode, fu_time)

  obj <- list(
    death_time = death_time,
    nonfatal_time = nonfatal_time,
    fu_time = fu_time
  )
  # return(obj)
  return(c(death_time, nonfatal_time, fu_time))

}

# Transform data from subject-wise long format to pairwise win-loss format
# Includes information on the deciding component (death or nonfatal events)
# Arguments:
# - df: Data frame with (id, time, status) and potentially other covariates
pair_win_loss_transform <- function(df) {
  # Separate outcomes with baseline covariates
  # Covariates
  Zn <- df |>
    group_by(id) |>
    slice(1) |>
    select(-c(time, status))
  # Number of covariates
  p <- ncol(Zn) - 1


  # Flatten outcome data and self-pair --------------------------------------
  # Specific to win function wfun
  # df_y -> pairwise_tibble, pw_win

  df_y <- df |>
    select(id, time, status)


  n <- nrow(Zn)

  poc_win_obj <- poc_win(df_y, n)

  # Extract pairwise data
  pairwise_tibble <- poc_win_obj$pairwise_tibble
  pw_win <- poc_win_obj$pw_win
  # Self-pair covariates




  pw_Z <- crossing(i = Zn, j = Zn) %>%
    filter(i |> row_number() < j |> row_number()) |>
    unnest(c(i, j), names_sep = "_") |>
    # Right joint with pw_win to remove pairs with win ==0
    right_join(pw_win |> select(id_i, id_j),
               by = c("i_id" = "id_i", "j_id" = "id_j"))



  # Compute difference in covariates
  pw_Zd <- pw_Z |>
    select(id_i = i_id, id_j = j_id) |>
    bind_cols(
      # Z_i - Z_j
      pw_Z |> select(starts_with("i_")) |> rename_with(~ str_remove(., "i_")) |> select(-id) -
        pw_Z |> select(starts_with("j_")) |> rename_with(~ str_remove(., "j_")) |> select(-id)
    )

  # Merge Zd with pw_win by id_i and id_j
  pw_df <- pw_win |>
    left_join(pw_Zd, by = c("id_i", "id_j")) |>
    mutate(
      win = win |> as.factor(),
      component = component |> as.factor()
    )

  pw_df
}

# Split data into training and test sets
# Arguments:
# - df: Data frame with (id, time, status) and potentially other covariates
# - prop: Proportion of data to be used for training
# - strata: Whether to stratify the split based on event types
# Returns a list with training and test data frames
wr_split <- function(df, prop = 0.8, strata = TRUE){
  if (strata){
    out_id_train <- df |>
      group_by(id) |>
      summarize(
        out = case_when(
          any(status == 1) & any(status == 2) ~ 1,
          any(status == 1) ~ 2,
          any(status == 2) ~ 3,
          TRUE ~ 4
        )
      ) |>
      group_by(out) |>
      mutate(
        split = sample(c("train", "test"), n(), replace = TRUE, prob = c(prop, 1 - prop))
      ) |>
      ungroup() |>
      select(-out)
  } else {
    out_id_train <- df |>
      distinct(id) |>
      mutate(split = sample(c("train", "test"), n(), replace = TRUE, prob = c(prop, 1 - prop)))
  }

  df_split <- df |>
    inner_join(out_id_train, by = "id")

  list(df_train = df_split |> filter(split == "train") |> select(-split),
       df_test = df_split |> filter(split == "test") |> select(-split))


}


# Main function to fit a penalized win ratio regression with pathwise solution
# Arguments (long format):
# - id, time, status: Subject identifiers, event times, and event statuses
# - Z: Matrix of covariates
# - ...: Additional arguments passed to glmnet (lambda, alpha, etc.)
wrnet <- function(id, time, status, Z, ...){

  df <- tibble(id, time, status, Z)
  # Transform data to pairwise win-loss format
  pw_df <- pair_win_loss_transform(df)

  y <- pw_df$win
  x <- pw_df |> select(-c(id_i, id_j, win, component))

  # Use glmnet for logistic regression

  obj <- glmnet(
    x = x,
    y = y,
    family = "binomial",
    # Constrain intercept = 0
    intercept = FALSE,
    ...
  )

  obj$Zn <-  df |> group_by(id) |> slice(1) |> ungroup() |>  select(-c(id, time, status)) |> as.matrix()

  class(obj) <- "wrnet"
  obj
}

# Create default lambda sequence for regularization
# Arguments:
# - id, time, status: Subject identifiers, event times, and statuses
# - Z: Covariate matrix
create_default_lambda <- function(id, time, status, Z){
  Z <- as.matrix(Z)
  df <- tibble(id, time, status, Z)
  n <- length(unique(id))
  p <- ncol(Z)
  # Transform data to pairwise win-loss format
  pw_df <- pair_win_loss_transform(df)
  y <- pw_df$win
  x <- pw_df |> select(-c(id_i, id_j, win, component))
  x_scaled <- scale(x)
  N <- nrow(x)

  lambda.max <- max(abs(1 / N * t(x_scaled) %*% (1/N - (y == unique(y)[1]))), na.rm = TRUE)
  lambda_min_ratio <- ifelse(n <= p, 0.01, 0.0001)
  lambda <- lambda.max * lambda_min_ratio^seq(0, 1, length.out = 100)


  return(lambda)

}

# Cross-validation for wrnet
# Arguments:
# - id, time, status: Subject identifiers, event times, and statuses
# - Z: Covariate matrix
# - k: Number of folds
# - strata: Whether to stratify folds based on event types
cv_wrnet <- function(id, time, status, Z, k = 10, strata = TRUE, lambda = NULL, ...){

  # Generate default lambda sequence if not provided
  if (is.null(lambda)){
    lambda <- create_default_lambda(id, time, status, Z)
  }

  df_train <- tibble(id, time, status, Z)

  # Generate folds
  if (strata){
    out_id_train_cv <- df_train |>
      group_by(id) |>
      summarize(
        out = case_when(
          any(status == 1) & any(status == 2) ~ 1,
          any(status == 1) ~ 2,
          any(status == 2) ~ 3,
          TRUE ~ 4
        )
      ) |>
      group_by(out) |>
      mutate(
        fold = sample(1:k, n(), replace = TRUE)
      ) |>
      ungroup() |>
      select(-out)
  } else {
    out_id_train_cv <- df_train |>
      distinct(id) |>
      mutate(fold = sample(1:k, n(), replace = TRUE))
  }

  # Transform each fold to pairwise format
  df_train_cv <- df_train |>
    inner_join(out_id_train_cv, by = "id") |>
    select(fold, everything()) |>
    group_by(fold) |>
    nest() |>
    mutate(
      pw_data = map(data, ~ .x |> pair_win_loss_transform())
    ) |>
    select(-data)

  fit_pw_fold <- function(k, lambda, ...){
    df_train_cv_k <- df_train_cv |> filter(fold != k) |> unnest(pw_data) |> ungroup()
    # Fit regularized logistic regression model
    pw_fit <- glmnet(
      x = df_train_cv_k |> select(-(1:5)) |> as.matrix(),
      y = df_train_cv_k$win,
      family = "binomial",
      # constrain intercept = 0
      intercept = FALSE,
      lambda= lambda,
      ...
    )

    Beta <- as.matrix(coef(pw_fit))[-1, ]

    df_val_cv_k <- df_train_cv |> filter(fold == k) |> unnest(pw_data)|> ungroup()


    pw_win_test_score <- df_val_cv_k |>
      select((1:5)) |>
      bind_cols(
        as_tibble(df_val_cv_k |> select(-(1:5)) |> as.matrix() %*% Beta)
      )

    # Compute concordance index
    cind_lambda <- pw_win_test_score |>
      mutate(
        win = case_when(
          win == "1" ~ 1,
          win == "-1" ~ -1
        ),
        across(-c(fold, id_i,id_j,win,component), \(x) if_else(x == 0, 0.5, sign(x) * win == 1))
      ) |>
      summarize(
        across(-c(fold, id_i,id_j,win,component), mean)
      ) |>
      pivot_longer(
        everything(),
        names_to = "lambda_name",
        values_to = "concordance"
      ) |>
      mutate(
        lambda = pw_fit$lambda
      )

    cind_lambda
  }


  fit_pw_fold_param <- function(k) fit_pw_fold(k, lambda, ...)


  cv_results <- tibble(
    fold = 1:k,
    cind_lambda = map(fold, ~ fit_pw_fold_param(.x))
  )


  cv_results_expanded <- cv_results |>
    unnest(cind_lambda) |>
    group_by(lambda) |>
    summarize(
      concordance = mean(concordance)
    )

  cv_results_expanded

}

# Test the fitted wrnet model on test data
# Arguments:
# - fit: Fitted wrnet object
# - df_test: Test dataset with (id, time, status, covariates)
test_wrnet <- function(fit, df_test){


  # fit <- final_fit

  # Extract beta
  beta <- as.matrix(fit$beta)

  Zn_test <- df_test |> group_by(id) |> slice(1) |> select(-c(time, status)) |> ungroup()
  score_test <- tibble(
    id = Zn_test$id,
    win_score = Zn_test |> select(-id) |> as.matrix() %*% beta |> as.vector(),
    risk_score = - win_score
  )

  pw_score_test <- crossing(i = score_test, j = score_test) %>%
    filter(i |> row_number() < j |> row_number()) |>
    unnest(c(i, j), names_sep = "_") |>
    select(
      id_i = i_id,
      id_j = j_id,
      win_score_i = i_win_score,
      win_score_j = j_win_score
    )




  df_y_test <- df_test |>
    select(id, time, status)
  n_test <- nrow(Zn_test)

  poc_win_obj <- poc_win(df_y_test, n_test)

  # Extract pairwise data
  pw_win_test <- poc_win_obj$pw_win

  # Merge win-loss outcomes with risk scores
  pw_win_score_test <- pw_win_test |>
    left_join(pw_score_test, by = c("id_i" = "id_i", "id_j" = "id_j"))

  # Overall and component-wise concordance
  concordance <- pw_win_score_test |>
    bind_rows(
      pw_win_score_test |> mutate(component = "overall")
    ) |>
    mutate(
      score_diff = win_score_i - win_score_j,
      concord = if_else(score_diff == 0, 0.5, sign(score_diff)  == win)
    ) |>
    group_by(component) |>
    summarize(
      concordance = mean(concord, na.rm = TRUE)
    )

  list(concordance = concordance, score_test = score_test, pw_win_score_test = pw_win_score_test)
}

# Computes variable importance based on standardized coefficients
# Arguments:
# - fit: Fitted wrnet object
vi_wrnet <- function(fit){

  beta <- fit$beta
  Zn <- fit$Zn

  sds <- apply(Zn, 2, sd)
  importance <- abs(beta * sds)

  obj <- tibble(
    vars = rownames(beta),
    importance = as.vector(importance)
  ) |>
  arrange(
    desc(importance)
  ) |>
    filter(
      importance > 0
    )

  class(obj) <- c("vi_wrnet", class(obj))

  obj
}



# Define the generic
vip <- function(x, ...) {
  UseMethod("vip")
}
# Plot top k variables based on variable importance
# Arguments:
# - x: Variable importance object (vi_wrnet)
# - k: Number of top variables to display (default is top 10 or all if fewer than 20)
vip.vi_wrnet <- function(x, k = NULL){


  vip_tbl <- tibble(
      vars = x$vars,
      importance = x$importance
    )

  m <- nrow(vip_tbl)

  if (is.null(k)){
    k <- ifelse(m <= 20, m, 10)
  } else(
    k = min(k, m)
  )

  vip_tbl |>
    head(k) |>
    ggplot(
      aes(y = reorder(vars, importance), x = importance)
    ) +
    geom_col() +
    labs(
      x = "Importance",
      y = NULL
    ) +
    theme_minimal()


}

logit <- function(x) {

  x <- ifelse(x == 1,
              1 - 1e-12,
              x)
  x <- ifelse(x == 0,
              1e-12,
              x)
  # return results
  log(x / (1 - x))
}



# A streamlined function to automate the process
# Data partitioning -> CV > Final Model -> Test
wrnet_pipeline <- function(df,  k = 10, prop = 0.8, lambda = NULL, cox = FALSE, ...){

  # Split data into training and test sets
  split <- wr_split(df, prop = prop)
  df_train <- split$df_train
  df_test <- split$df_test

  # Cross-validation
  cv_results <- cv_wrnet(df_train$id, df_train$time, df_train$status, df_train |> select(-c(id, time, status)),
                         k = k, lambda = lambda, ...)

  # Fit final model
  lambda_final <- cv_results |>
    filter(concordance == max(concordance)) |>
    pull(lambda)

  fit <- wrnet(df_train$id, df_train$time, df_train$status, df_train |> select(-c(id, time, status)),
               lambda = lambda_final, ...)

  beta <- as.vector(fit$beta)
  varnames <- rownames(fit$beta)
  # Test the model
  test_results <- test_wrnet(fit, df_test)
  concordance <- test_results$concordance

  result <- list(varnames = varnames, beta = beta, concordance = concordance)

  if (cox) {
    # Fit regularized Cox model by glmnet
    # Subset to first events
    df_train_tfe <- df_train |>
      group_by(id) |>
      arrange(time) |>
      slice(1) |>
      mutate(
        status = status > 0,
        time = if_else(time <= 0, 0.0001, time) # fix 0 times
      ) |>
      ungroup()
    # Fit regularized cox regression model
    # 10-fold CV
    cox_cvfit <- cv.glmnet(
      x = df_train_tfe |> select(-c(id, time, status)) |> as.matrix(),
      y = df_train_tfe |> select(time, status) |> as.matrix(),
      family = "cox",
      type.measure = "C", # C-index for CV
      ...
    )

    cox_beta <- as.vector(coef(cox_cvfit, s = "lambda.min"))
    # Trick test_wrnet() to get risk score for Cox model
    cox_cvfit$beta <-  - cox_beta
    # Test the final model on test set
    cox_test_result <- cox_cvfit |> test_wrnet(df_test)
    # Test C-index
    cox_concordance <- cox_test_result$concordance

    result$cox_beta<- cox_beta
    result$cox_concordance <- cox_concordance

  }

  result
}



