# EDUR7126 - Stage 1 Project Proposal
# HSLS:09: Preliminary data feasibility analysis

# 1. Load the original HSLS:09 public-use RData file.
# Select the downloaded RData file when prompted.
load("/Users/lyg/Desktop/Phd Study/7126 Advanced Regression and Generalized Linear Models/project/HSLS_2009-16_PETS_Pear_v1_0_R_Datasets/hsls09_16_student_pets_pear_v1_0.rdata")
raw <- hsls09_16_student_pets_pear_v1_0
names(raw) <- toupper(names(raw))

# 2. Select variables.
vars <- c("X4EVRATNDCLG", "X1LOCALE", "X1SES", "X1SEX",
          "X1RACE", "X3TGPATOT", "X1TXMTSCOR", "X4HSCOMPSTAT")
stopifnot(all(vars %in% names(raw)))
d <- raw[, vars, drop = FALSE]
cat("Original sample:", nrow(d), "\n")
print(table(d$X4EVRATNDCLG, useNA = "ifany"))
print(table(d$X4HSCOMPSTAT, useNA = "ifany"))
print(table(d$X1LOCALE, useNA = "ifany"))

# 3. Reproduce preliminary missing-value recoding.
# SES can legitimately be negative; do not remove all negative SES values.
# Confirm these provisional special codes against the variable codebook
# before the final analysis.
for (v in setdiff(vars, "X1SES")) {
  d[[v]][d[[v]] %in% (-9:-1)] <- NA
}
d$X1SES[d$X1SES %in% (-9:-5)] <- NA
cat("Valid binary enrollment records:",
    sum(d$X4EVRATNDCLG %in% c(0, 1)), "\n")
print(round(colMeans(is.na(d)) * 100, 2))

# 4. Restrict the sample to regular high school diploma recipients.
grad_idx <- which(d$X4HSCOMPSTAT == 1)
graduates <- d[grad_idx, , drop = FALSE]
cat("Regular high school diploma recipients:", nrow(graduates), "\n")
print(round(colMeans(is.na(graduates)) * 100, 2))

# 5. Recode school locale: 1 = rural; 0 = non-rural.
stopifnot(all(na.omit(graduates$X1LOCALE) %in% 1:4))
graduates$rural <- ifelse(graduates$X1LOCALE == 4, 1L, 0L)
print(table(graduates$rural, useNA = "ifany"))

# 6. Construct the preliminary complete-case sample.
model_vars <- c("X4EVRATNDCLG", "rural", "X1SES", "X1SEX",
                "X1RACE", "X3TGPATOT", "X1TXMTSCOR")
cc <- complete.cases(graduates[, model_vars, drop = FALSE])
analysis_data <- graduates[cc, , drop = FALSE]
keep <- grad_idx[cc]
stopifnot(all(analysis_data$X4EVRATNDCLG %in% c(0, 1)))
cat("Complete cases:", nrow(analysis_data), "\n")
cat("Excluded graduates:", sum(!cc), "\n")
cat("Retention percentage:",
    round(100 * mean(cc), 2), "\n")
print(table(analysis_data$rural))
print(round(100 * prop.table(table(analysis_data$rural)), 2))

# 7. Calculate unweighted postsecondary enrollment rates.
tab <- with(analysis_data, table(rural, X4EVRATNDCLG))
print(tab)
print(round(100 * prop.table(tab, margin = 1), 2))
rates <- 100 * prop.table(tab, margin = 1)[, "1"]
cat("Non-rural enrollment (%):", round(rates["0"], 2), "\n")
cat("Rural enrollment (%):", round(rates["1"], 2), "\n")
cat("Non-rural minus rural gap (percentage points):",
    round(rates["0"] - rates["1"], 2), "\n")
print(summary(analysis_data[, c("X1SES", "X3TGPATOT", "X1TXMTSCOR")]))

# 8. Check candidate longitudinal survey weight and replicate weight names.
stopifnot("W4W1STU" %in% names(raw))
w <- raw$W4W1STU[keep]
print(summary(w))
valid_w <- is.finite(w) & w > 0
print(table(valid_w, useNA = "always"))
cat("Valid positive main weights:", sum(valid_w), "\n")
rep_names <- sprintf("W4W1STU%03d", 1:200)
cat("Replicate weight variables available:",
    length(intersect(rep_names, names(raw))), "out of 200\n")


