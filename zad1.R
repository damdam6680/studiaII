set.seed(123)

m <- 20

mu <- 1
sigma <- 0.5

X <- rnorm(m, mean = mu, sd = sigma)
print("Wygenerowany rozkład normalny:")
print(X)

k <- 100

A <- matrix(0, k, 6)

for (s in 1:k) {
  
  
  boot20 <- numeric(m)
  for (i in 1:m) boot20[i] <- mean(sample(X, m, replace = TRUE))
  mb <- mean(boot20)
  obmb <- abs(mean(X) - mb)
  bsboot <- sd(boot20) / sqrt(m)
  
  jack <- numeric(m - 1)
  pseudo <- numeric(m)
  for (i in 1:m) {
    for (j in 1:m) {
      if (j < i) jack[j] <- X[j] 
      else if (j > i) jack[j - 1] <- X[j]
    }
    pseudo[i] <- m * mean(sample(X, replace = TRUE)) - (m - 1) * mean(sample(jack, replace = TRUE))
  }
  mj <- mean(pseudo)
  obmj <- (mean(X) - mj) * (m - 1)
  bsmj <- sqrt((m - 1) * var(pseudo))
  
  A[s, ] <- c(round(mb, 4), round(obmb, 4), round(bsboot, 4), round(mj, 4), round(obmj, 4), round(bsmj, 4))
}

WYNIK <- as.data.frame(A)
colnames(WYNIK) <- c("bootmean", "obc.boot", "sdboot", "jackmean", "obc.jack", "sdjack")
print("Wyniki są zaokrąglone do 4 miejsc po przecinku:")
print(WYNIK)

print("Sprawdzenie, czy obie metody dają porównywalne wyniki:")
T <- t.test(WYNIK$bootmean, WYNIK$jackmean, alternative = "two.sided", var.equal = FALSE)
W <- wilcox.test(WYNIK$bootmean, WYNIK$jackmean, paired = TRUE)
print(T)
print(ifelse(T$p.value < 0.05, "Istotne różnice w wynikach", "Brak istotnych różnic"))
print(W)
print(ifelse(W$p.value < 0.05, "Istotne różnice w wynikach", "Brak istotnych różnic"))

summary_stats <- function(data, name) {
  print(paste("Statystyki dla", name))
  print(summary(data))
  print("Odchylenie standardowe:")
  print(sd(data))
  print("Współczynnik zmienności:")
  print(sd(data) / mean(data))
  error <- qnorm(0.975) * sd(data) / sqrt(nrow(WYNIK))
  print("95% przedział ufności dla średniej:")
  print(c(mean(data) - error, mean(data) + error))
}

summary_stats(WYNIK$bootmean, "bootstrap")
summary_stats(WYNIK$jackmean, "jackknife")

# Zastosowanie dla innego rozkładu - Chi-kwadrat
nu <- 3  
X_chi2 <- rchisq(m, df = nu)
print("Wygenerowany rozkład chi-kwadrat:")
print(X_chi2)

A_chi2 <- matrix(0, k, 6)
for (s in 1:k) {
  boot20 <- numeric(m)
  for (i in 1:m) boot20[i] <- mean(sample(X_chi2, m, replace = TRUE))
  mb <- mean(boot20)
  obmb <- abs(mean(X_chi2) - mb)
  bsboot <- sd(boot20) / sqrt(m)
  
  jack <- numeric(m - 1)
  pseudo <- numeric(m)
  for (i in 1:m) {
    for (j in 1:m) {
      if (j < i) jack[j] <- X_chi2[j] 
      else if (j > i) jack[j - 1] <- X_chi2[j]
    }
    pseudo[i] <- m * mean(sample(X_chi2, replace = TRUE)) - (m - 1) * mean(sample(jack, replace = TRUE))
  }
  mj <- mean(pseudo)
  obmj <- (mean(X_chi2) - mj) * (m - 1)
  bsmj <- sqrt((m - 1) * var(pseudo))
  
  A_chi2[s, ] <- c(round(mb, 4), round(obmb, 4), round(bsboot, 4), round(mj, 4), round(obmj, 4), round(bsmj, 4))
}

WYNIK_chi2 <- as.data.frame(A_chi2)
colnames(WYNIK_chi2) <- c("bootmean", "obc.boot", "sdboot", "jackmean", "obc.jack", "sdjack")
print("Wyniki dla rozkładu chi-kwadrat:")
print(WYNIK_chi2)

summary_stats(WYNIK_chi2$bootmean, "bootstrap (chi^2)")
summary_stats(WYNIK_chi2$jackmean, "jackknife (chi^2)")


write.table(WYNIK, file="raport.csv", append=TRUE, sep=";", row.names=FALSE, col.names=TRUE, dec=",")
write.table(WYNIK_chi2, file="raport_chi2.csv", append=TRUE, sep=";", row.names=FALSE, col.names=TRUE, dec=",")
