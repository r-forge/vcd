vcd.demo <- function(chapter = 2)
{
  par(ask = TRUE)
  
if(chapter == 2) {
  data(HorseKicks)
  barplot(HorseKicks$Freq, names = HorseKicks$nDeaths, col = 2,
          xlab = "Number of Deaths", ylab = "Number of Corps-Years",
          main = "Deaths by Horse Kicks")

  data(Federalist)
  barplot(Federalist$Freq, names = Federalist$nMay, col = 2,
          xlab = "Occurences of 'may'", ylab = "Number of Blocks of Text",
          main = "'may' in Federalist papers")

  data(WomenQueue)
  barplot(WomenQueue$Freq, names = WomenQueue$nWomen, col = 2,
          xlab = "Number of women", ylab = "Number of queues",
          main = "Women in queues of length 10")

  data(WeldonDice)
  barplot(WeldonDice$Freq, names = c(WeldonDice$n56[-11], "10+"), col = 2,
          xlab = "Number of 5s and 6s", ylab = "Frequency",
	  main = "Weldon's dice data")

  data(Butterfly)
  barplot(Butterfly$nSpecies, names = Butterfly$nTokens, col = 2,
          xlab = "Number of individuals", ylab = "Number of Species",
  	main = "Butterfly species im Malaya")


  ############################
  ## Binomial distributions ##
  ############################

  par(mfrow = c(1,2))
  barplot(dbinom(0:10, p = 0.15, size = 10), names = 0:10, col = grey(0.7),
          main = "p = 0.15", ylim = c(0,0.35))
  barplot(dbinom(0:10, p = 0.35, size = 10), names = 0:10, col = grey(0.7),
          main = "p = 0.35", ylim = c(0,0.35))
  par(mfrow = c(1,1))
  mtext("Binomial distributions", line = 2, cex = 1.5)

  plot(0:10, dbinom(0:10, p = 0.15, size = 10), type = "b", ylab = "Density",
       ylim = c(0, 0.4), main = "Binomial distributions, N = 10", pch = 19)
  lines(0:10, dbinom(0:10, p = 0.35, size = 10), type = "b", col = 2, pch = 19)
  lines(0:10, dbinom(0:10, p = 0.55, size = 10), type = "b", col = 4, pch = 19)
  lines(0:10, dbinom(0:10, p = 0.75, size = 10), type = "b", col = 3, pch = 19)
  legend(3, 0.4, c("p", "0.15", "0.35", "0.55", "0.75"), lty = rep(1,5), col =
         c(0,1,2,4,3), bty = "n")

  ###########################
  ## Poisson distributions ##
  ###########################

  par(mfrow = c(1,2))
  dummy <- barplot(dpois(0:12, 2), names = 0:12, col = grey(0.7), ylim = c(0,0.3),
        main = expression(lambda == 2))
  abline(v = dummy[3], col = 2)
  diff <- (dummy[3] - dummy[2]) * sqrt(2)/2
  lines(c(dummy[3] - diff, dummy[3] + diff), c(0.3, 0.3), col = 2)
  dummy <- barplot(dpois(0:12, 5), names = 0:12, col = grey(0.7), ylim = c(0,0.3),
          main = expression(lambda == 5))
  abline(v = dummy[6], col = 2)
  diff <- (dummy[6] - dummy[5]) * sqrt(5)/2
  lines(c(dummy[6] - diff, dummy[6] + diff), c(0.3, 0.3), col = 2)
  par(mfrow = c(1,1))
  mtext("Poisson distributions", line = 2, cex = 1.5)

  #####################################
  ## Negative binomial distributions ##
  #####################################

  nbplot <- function(p = 0.2, size = 2, ylim = c(0, 0.2))
  {
    plot(0:20, dnbinom(0:20, p = p, size = size), type = "h", col = grey(0.7),
         xlab = "Number of failures (k)", ylab = "Density", ylim = ylim,
         yaxs = "i", bty = "L")
    nb.mean <- size * (1-p)/p
    nb.sd <- sqrt(nb.mean/p)
    abline(v = nb.mean, col = 2)
    lines(nb.mean + c(-nb.sd, nb.sd), c(0.01, 0.01), col = 2)
    legend(14, 0.2, c(paste("p = ", p), paste("n = ", size)), bty = "n")
  }
  par(mfrow = c(3,2))
  nbplot()
  nbplot(size = 4)
  nbplot(p = 0.3)
  nbplot(p = 0.3, size = 4)
  nbplot(p = 0.4, size = 2)
  nbplot(p = 0.4, size = 4)
  par(mfrow = c(1,1))
  mtext("Negative binomial distributions for the number of trials to observe n = 2 or n = 4 successes", line = 3)

  #####################
  ## Goodness of fit ##
  #####################

  attach(HorseKicks)
  p <- weighted.mean(nDeaths, Freq)
  p.hat <- dpois(0:4, p)
  expected <- sum(Freq) * p.hat
  chi2 <- sum((Freq - expected)^2/expected)
  pchisq(chi2, df = 3, lower = FALSE)
  ## or:
  HK.fit <- goodfit(Freq, nDeaths)
  summary(HK.fit)
  detach(HorseKicks)

  attach(WeldonDice)
  ## Are the dice fair?
  p.hyp <- 1/3
  p.hat <- dbinom(0:12, prob = p.hyp, size = 12)
  expected <- sum(Freq) * p.hat
  expected <- c(expected[1:10], sum(expected[11:13]))
  chi2 <- sum((Freq - expected)^2/expected)
  G2 <- 2*sum(Freq*log(Freq/expected))
  pchisq(chi2, df = 10, lower = FALSE)
  ## Are the data from a binomial distribution?
  p <- weighted.mean(n56/12, Freq)
  p.hat <- dbinom(0:12, prob = p, size = 12)
  expected <- sum(Freq) * p.hat
  expected <- c(expected[1:10], sum(expected[11:13]))
  chi2 <- sum((Freq - expected)^2/expected)
  G2 <- 2*sum(Freq*log(Freq/expected))
  pchisq(chi2, df = 9, lower = FALSE)
  ## or:
  WD.fit1 <- goodfit(Freq, n56, type = "binomial", par = 1/3, size = 12)
  WD.fit2 <- goodfit(Freq, n56, type = "binomial", size = 12)
  WD.fit1$fitted[11] <- sum(dbinom(10:12, prob = WD.fit1$estimate, size = 12))*sum(Freq)
  WD.fit2$fitted[11] <- sum(dbinom(10:12, prob = WD.fit2$estimate, size = 12))*sum(Freq)
  summary(WD.fit1)
  summary(WD.fit2)
  detach(WeldonDice)

  attach(Federalist)
  F.fit1 <- goodfit(Freq, nMay)
  F.fit2 <- goodfit(Freq, nMay, type = "nbinomial")
  summary(F.fit1)
  par(mfrow = c(2,2))
  plot(F.fit1, scale = "raw", type = "standing")
  plot(F.fit1, type = "standing")
  plot(F.fit1)
  plot(F.fit1, type = "deviation")
  par(mfrow = c(1,1))
  plot(F.fit2, type = "deviation")
  summary(F.fit2)
  detach(Federalist)

  data(Saxony)
  attach(Saxony)
  S.fit <- goodfit(Freq, nMales, type = "binomial", size = 12)
  summary(S.fit)
  plot(S.fit)
  detach(Saxony)

  ###############
  ## Ord plots ##
  ###############

  par(mfrow = c(2,2))
  Ord.plot(HorseKicks$Freq, HorseKicks$nDeaths, main = "Death by horse kicks")
  Ord.plot(Federalist$Freq, Federalist$nMay, main = "Instances of 'may' in Federalist papers")
  Ord.plot(Butterfly$nSpecies, Butterfly$nTokens, main = "Butterfly species collected in Malaya")
  Ord.plot(WomenQueue$Freq, WomenQueue$nWomen, main = "Women in queues of length 10")
  par(mfrow = c(1,1))

  ###############
  ## Distplots ##
  ###############

  distplot(HorseKicks$Freq, HorseKicks$nDeaths, type = "poisson")
  distplot(HorseKicks$Freq, HorseKicks$nDeaths, type = "poisson", lambda = 0.61)
  distplot(Federalist$Freq, Federalist$nMay, type = "poisson")
  distplot(Federalist$Freq, Federalist$nMay, type = "nbinomial")
  distplot(Saxony$Freq, Saxony$nMales, type = "binomial", size = 12)

}

if (chapter == 3) {
  #####################
  ## Fourfold tables ##
  #####################
  
  ### Berkeley Admission Data ###
  ###############################
  data(UCBAdmissions)

  ## unstratified
  ### no margin is standardized
  x <- apply(UCBAdmissions, c(2,1), sum)
  fourfoldplot(x, std = "i")
  ### std. for gender
  fourfoldplot(x, margin = 1)
  ### std. for both
  fourfoldplot(x)
  
  ## stratified
  fourfoldplot(UCBAdmissions, col = c("red","blue"))

  ### Coal Miners Lung Data ###
  #############################
  data(CoalMiners)
  
  ## Fourfold display, both margins equated
  fourfoldplot(CoalMiners, col = c("red","blue"), mfcol = c(2,4))

  ## Log Odds Ratio Plot
  l <- logoddsratio(CoalMiners)
  g <- seq(25, 60, by = 5)
  plot(l,
       xlab = "Age Group",
       main = "Breathelessness and Wheeze in Coal Miners")
  m <- lm(l ~ g + I(g^2))
  lines(fitted(m), col = "red")
  
  ## Fourfold display, strata equated
  fourfoldplot(CoalMiners, col = c("red","blue"), std = "ind.max", mfcol = c(2,4))

  ####################
  ## Sieve Diagrams ##
  ####################

  ### Hair Eye Color ###
  ######################
  data(HairEyeColor)

  ## aggregate over `sex':
  (tab <- apply(HairEyeColor, 1:2, sum))

  ## plot expected values:
  sieveplot(t(tab), type = "expected", values = "both")

  ## plot sieve diagram:
  sieveplot(t(tab))

  ### Visual Acuity ###
  #####################
  data(VisualAcuity)
  sieveplot(xtabs(Freq ~ right + left,
                  data = VisualAcuity, subset = gender == "female"),
            reverse.y = FALSE,
            main = "Unaided distant vision data",
            xlab = "Left Eye Grade",
            ylab = "Right Eye Grade")
  

  ### Berkeley Admission ###
  ##########################

  ## -> Larger tables: Cross factors
  ### Cross Gender and Admission
  data(UCBAdmissions)
  tab <- xtabs(Freq ~ Dept + I(Gender : Admit), data = UCBAdmissions)
  sieveplot(tab, reverse.y = FALSE,
            xlab = "Gender:Admission",
            ylab = "Department",
            main = "Berkeley Admissions Data"
            )

  ######################
  ## Association Plot ##
  ######################
  
  ### Hair Eye Color ###
  ######################
  data(HairEyeColor)
  assocplot(apply(HairEyeColor, c(1,2), sum),
            col = c("blue","red"),
            xlab = "Hair Color",
            ylab = "Eye Color",
            main = "Association Plot")

  ####################
  ## Agreement Plot ##
  ####################

  ### Sexual Fun ###
  ##################
  data(SexualFun)

  ## Kappa statistics
  Kappa(SexualFun)

  ## Agreement Chart
  agreementplot(t(SexualFun), weights = 1)
  ## Partial Agreement Chart and B-Statistics
  (agreementplot(t(SexualFun),
                 xlab = "Husband's Rating",
                 ylab = "Wife's Rating",
                 main = "Husband's and Wife's Sexual Fun")
   )
  
  ### MS Diagnosis data ###
  #########################
  data(MSpatients)
  plot.new()
  X11(width = 12)
  par(mfrow = c(1,2), ask = TRUE)
  agreementplot(t(MSpatients[,,1]), main = "Winnipeg Patients")
  agreementplot(t(MSpatients[,,2]), main = "New Orleans Patients")
  plot.new()
  dev.off()

  ##################
  ## Ternary Plot ##
  ##################

  ### sample data ###
  ###################
  (x <- rbind(c(A=10,B=10,C=80),
              c(40,30,30),
              c(20,60,20)
              )
   )
  ternaryplot(x,
              cex = 2,
              col = c("black", "blue", "red"),
              coordinates = TRUE
              )

  ### Arthritis Treatment Data ###
  ################################
  data(Arthritis)
  
  ## Build table by crossing Treatment and Sex
  (tab <- as.table(xtabs(~ I(Sex:Treatment) + Improved, data = Arthritis)))
  
  ## Mark groups
  col <- c("red", "red", "blue", "blue")
  pch <- c(1, 19, 1, 19)
  
  ## plot
  ternaryplot(
              tab,
              col = col,
              pch = pch,
              cex = 2,
              bg = "lightgray",
              grid.color = "white",
              labels.color = "white",
              main = "Arthritits Treatment Data"
              )
  ## legend
  legend(0.7, 0.8,
         c("GROUP", rownames(tab)),
         pch = c(NA, pch),
         col = c(NA, col)
         )

  ### Baseball Hitters Data ###
  #############################
  data(Hitters)
  attach(Hitters)
  colors <- c("black","red","green","blue","red","black","blue")
  pch <- substr(levels(Positions), 1, 1)
  ternaryplot(
              Hitters[,2:4],
              pch = as.character(Positions),
              col = colors[codes(Positions)],
              main = "Baseball Hitters Data"
              )
  legend(
         0.7, 0.85,
         legend = c("POSITION(S)", levels(Positions)),
         pch = c("", pch),
         col = c(NA, colors)
         )

  ### Lifeboats on Titanic ###
  ############################
  data(Lifeboats)
  attach(Lifeboats)
  ternaryplot(
              Lifeboats[,4:6],
              pch = ifelse(side=="Port", 1, 19),
              col = ifelse(side=="Port", "red", "blue"),
              id  = ifelse(men/total > 0.1, as.character(boat), NA),
              dimnames.position = "edge",
              dimnames = c("Men of Crew", "Men passengers", "Women and Children"),
              main = "Lifeboats on Titanic"
              )
  legend(
         0.7, 0.8,
         legend = c("SIDE", "Port", "Starboard"),
         pch = c(NA, 1, 19),
         col = c("black", "red", "blue"),
         )

  ## Load against time for Port/Starboard boats
  plot(launch, total,
       pch = ifelse(side == "Port", 1, 19),
       col = ifelse(side == "Port", "red", "darkblue"),
       xlab = "Launch Time",
       ylab = "Total loaded",
       main = "Lifeboats on Titanic"
       )
  legend(as.POSIXct("1912-04-15 01:48:00"), 70,
         legend = c("SIDE","Port","Starboard"),
         pch = c(NA, 1, 19),
         col = c(NA, "red", "darkblue")
         )
  text(as.POSIXct(launch),
       total,
       labels = as.character(boat),
       pos = 3,
       offset = 0.3
       )
  abline(lm(total ~ as.POSIXct(launch),
            subset = side == "Port"),
         col = "red")     
  abline(lm(total ~ as.POSIXct(launch),
            subset = side == "Starboard"),
         col = "darkblue")     
}
  
  par(ask = FALSE)
}


