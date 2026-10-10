
set.seed(1998)

M <- 10000
n <- 100
B <- 2000
true_mean <- 1

results <- data.frame(
  bootstrap_covers = logical(M),
  t_covers = logical(M),
  bootstrap_width = numeric(M),
  t_width = numeric(M)
)

for (m in seq_len(M)) {
  x <- rexp(n, rate = 1)

  boot_ci <- bootmean::bootstrap_mean_ci(x, B = B)
  t_ci <- as.numeric(t.test(x, conf.level = 0.95)$conf.int)

  results$bootstrap_covers[m] <-
    boot_ci$lower <= true_mean && true_mean <= boot_ci$upper

  results$t_covers[m] <-
    t_ci[1] <= true_mean && true_mean <= t_ci[2]

  results$bootstrap_width[m] <- boot_ci$upper - boot_ci$lower
  results$t_width[m] <- t_ci[2] - t_ci[1]

  if (m %% 1000 == 0) {
    cat("Completed", m, "of", M, "simulations\n")
  }
}

coverage <- c(
  mean(results$bootstrap_covers),
  mean(results$t_covers)
)
mc_se <- sqrt(coverage * (1 - coverage) / M)

summary_results <- data.frame(
  Method = c("Bootstrap", "Standard t"),
  Coverage = coverage,
  Average_width = c(
    mean(results$bootstrap_width),
    mean(results$t_width)
  ),
  MC_SE = mc_se,
  MC_lower = coverage - 1.96 * mc_se,
  MC_upper = coverage + 1.96 * mc_se
)

difference <- as.integer(results$t_covers) -
  as.integer(results$bootstrap_covers)
paired_se <- sd(difference) / sqrt(M)

paired_results <- data.frame(
  Coverage_difference = mean(difference),
  Paired_MC_SE = paired_se,
  MC_lower = mean(difference) - 1.96 * paired_se,
  MC_upper = mean(difference) + 1.96 * paired_se
)

# Run this script from the experiments folder.
dir.create("results", showWarnings = FALSE)

write.csv(results, "results/validation_exp100_raw.csv",
          row.names = FALSE)
write.csv(summary_results, "results/validation_exp100_summary.csv",
          row.names = FALSE)
write.csv(paired_results, "results/validation_exp100_paired.csv",
          row.names = FALSE)

capture.output(sessionInfo(),
               file = "results/validation_exp100_sessionInfo.txt")
capture.output(RNGkind(),
               file = "results/validation_exp100_RNGkind.txt")

print(summary_results)
print(paired_results)

