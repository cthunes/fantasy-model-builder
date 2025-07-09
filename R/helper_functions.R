# Returns a one-row data table containing results for a specific model's f-stat,
# aic, sigma, r-squared, adjusted r-squared, correlation, min-max, and MAPE
fit_results <- function(model, test, pred) {
  dist_pred <- predict(model, test)
  actuals_preds <- data.frame(cbind(actuals=test[,..pred], predicteds = dist_pred))
  summ <- summary(model)
  aic <- AIC(model)  # lower is better
  cor <- cor(actuals_preds)[[2]] # higher is better
  min_max <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) # higher is better
  mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals) # lower is better
  data.table(fstatistic = summ$fstatistic[1], aic = aic, sigma = summ$sigma, rs = summ$r.squared,
             adjrs = summ$adj.r.squared, cor = cor, min_max = min_max, mape = mape)
}


# Returns a list of median values from a given number of runs to fit_results()
m <- function(form, dat, pred, num = 5000) {
  model_scores <- rbindlist(lapply(1:num, function(x) {
    trainIndex <- sample(1:nrow(dat), .8*nrow(dat))
    fit_results(lm(form, data = dat[trainIndex,]), dat[-trainIndex,], pred)
  }))
  list(f = median(model_scores$fstatistic),
       aic = median(model_scores$aic),
       sig = median(model_scores$sigma),
       rs = median(model_scores$rs),
       ars = median(model_scores$adjrs),
       cor = median(model_scores$cor),
       mm = median(model_scores$min_max),
       mape = median(model_scores$mape))
}



step_upwards <- function(dat, pred, max_terms = 16, num = 5000) {
  cols <- colnames(dat)[!grepl("Player|TEAM|POS|_act|proj_|INJCOR|year", colnames(dat))]
  terms <- c()
  i = 1
  for(x in cols) {
    terms <- c(terms, x)
    for(y in cols[i:length(cols)]) {
      if (x!=y) terms <- c(terms, paste0(x, ":", y))
    }
    i = i + 1
  }
  sel_terms <- c()
  rsq_tbl <- data.table(term = c(), rsq = c())
  form <- paste0(pred, " ~ ")
  # only create number of terms up to max allowed based on number of observations
  cap <- min(floor(nrow(dat) / 25) - 1, max_terms) # no more than 16
  while(length(sel_terms) < cap) {
    cat("Selecting term #", length(sel_terms) + 1, "of", cap, "\n")
    rmax <- 0
    max_t <- NULL
    sel_str <- paste(sel_terms, collapse = " + ")
    if (length(sel_terms) > 0) {
      sel_str <- paste(sel_str,"+")
    }
    for (t in terms) {
      r <- summary(lm(formula(paste(form, sel_str, t)), dat))$r.squared
      rsq_tbl = rbind(rsq_tbl, list(term = paste(sel_str, t), rsq = r))
      if (r > rmax) {
        rmax = r
        max_t = t
      }
    }
    sel_terms <- c(sel_terms, max_t)
  }
  sel_str <- paste(sel_terms, collapse = " + ")
  cat("Stepping all possible models...\n")
  ols_opt <- as.data.table(ols_step_all_possible(lm(formula(paste(form, sel_str)), dat))[[1]][1:9])
  ols_opt$adjCP <- ols_opt$cp - ols_opt$n - 1
  ols_opt <- ols_opt[!duplicated(n)]
  cat("Calculating scores")
  scores <- rbindlist(lapply(1:nrow(ols_opt), function(i) {
    cat(".")
    form <- paste0(form, stringr::str_replace_all(ols_opt[i,]$predictors, " ", " + "))
    l <- m(formula(form), dat, pred, num)
    l$n <- ols_opt[i,]$n
    l$form <- form
    l
  }), fill = TRUE)
  cat(" Done.\n")
  ols_opt <- left_join(ols_opt[,c("n", "cp", "adjCP")], scores, by = "n")
  rank_cols <- c("adjCP", "f", "aic", "sig", "ars", "cor", "mm", "mape")
  ascending_cols <- c("adjCP", "aic", "sig", "mape")
  weights <- c(
    adjCP = 0.8,  # adjusted Cp (lower is better)
    f     = 0.5,  # F-statistic (higher is better, but inflated by complexity)
    aic   = 1.5,  # AIC (lower is better)
    sig   = 1.0,  # Residual std error (lower is better)
    ars   = 1.2,  # Adjusted R-squared (higher is better)
    cor   = 0.5,  # Correlation (higher is better)
    mm    = 0.5,  # Min-max accuracy (higher is better)
    mape  = 1.5   # Mean Absolute Percentage Error (lower is better)
  )
  cat("Ranking each column...\n")
  # Rank each column appropriately
  norm_dt <- as.data.table(Map(function(x, colname) {
    x_max <- max(x, na.rm = TRUE)
    x_min <- min(x, na.rm = TRUE)
    denom <- x_max - x_min
    if (denom == 0) return(rep(0, length(x)))  # handle constant columns
    if (colname %in% ascending_cols) {
      (x - x_min) / denom
    } else {
      (x_max - x) / denom
    }
  }, ols_opt[, ..rank_cols], rank_cols))
  # Multiply each normalized column by its corresponding weight
  weighted_cols <- Map(function(col, w) col * w, norm_dt, weights[rank_cols])
  # Sum weighted values row-wise and divide by total weight
  ols_opt[, avg_score := rowSums(as.data.table(weighted_cols)) / sum(weights[rank_cols])]
  # sort from best (lowest score) to worst
  setorder(ols_opt, avg_score)
  ols_opt
}



generate_python_formula <- function(lm_obj, output_var = "proj_y", df_name = "df") {
  coefs <- coef(lm_obj)
  terms <- names(coefs)

  cat(df_name, "[\"", output_var, "\"] = (\n", sep = "")

  for (i in seq_along(coefs)) {
    coef_val <- coefs[i]
    term <- terms[i]

    if (term == "(Intercept)") {
      cat(sprintf("    %.5f\n", coef_val))
    } else {
      # Replace . with /
      term_cleaned <- gsub("\\.", "/", term)

      # Handle interaction terms
      if (grepl(":", term_cleaned)) {
        parts <- strsplit(term_cleaned, ":")[[1]]
        term_expr <- paste0(df_name, "[\"", parts[1], "\"]")
        for (j in 2:length(parts)) {
          term_expr <- paste0(term_expr, " * ", df_name, "[\"", parts[j], "\"]")
        }
      } else {
        term_expr <- paste0(df_name, "[\"", term_cleaned, "\"]")
      }

      sign_prefix <- if (coef_val >= 0) "    + " else "    - "
      cat(sign_prefix, sprintf("%.5f * %s\n", abs(coef_val), term_expr))
    }
  }

  cat(")\n")
}
