#####################
## Mosaic Displays ##
#####################

#########################
## Hair Eye Color Data ##
#########################

data(HairEyeColor)

## Basic Mosaic Display ##

HairEye <- margin.table(HairEyeColor, c(1,2))

grid.mosaicplot(HairEye, main = "Basic Mosaic Display of Hair Eye Color data")

## Hair Mosaic Display with Pearson residuals ##
Hair <- margin.table(HairEyeColor,1)
Hair
mHair <- as.table(rep(mean(margin.table(HairEyeColor, 1)), 4))
names(mHair) <- names(Hair)
mHair

## Pearson residuals from Equiprobability model ##

resid <- (Hair - mHair) / sqrt(mHair)
resid

## First Step in a Mosaic Display ##

grid.mosaicplot(Hair, residuals = resid, main = "Hair Color Proportions")

## Hair Eye Mosais Display with Pearson residuals ##

grid.mosaicplot(HairEye, main = " Hair Eye Color with Pearson residuals")

## Show Pearson Residuals ##

(HairEye - loglin(HairEye, c(1, 2), fit = TRUE)$fit) /
  sqrt(loglin(HairEye, c(1, 2), fit = TRUE)$fit)

###################
## UKSoccer Data ##
###################

data(UKSoccer)

## UKSoccer Mosaic Display ##

grid.mosaicplot(UKSoccer, main = "UK Soccer Scores")

###############################
## Repeat Victimization Data ##
###############################

data(RepVict)

grid.mosaicplot(RepVict[-c(4, 7), -c(4, 7)], main = "Repeat Victimization Data")


##################
## 3-Way Tables ##
##################

## Hair Eye Sex Mosais Display with Pearson residuals ##
grid.mosaicplot(HairEyeColor, main = "Hair Eye Color Sex" )

grid.mosaicplot(HairEyeColor, margin = ~ Hair * Eye + Sex,
                main = "Model: (Hair Eye) (Sex)" )

grid.mosaicplot(HairEyeColor, margin = ~ Hair * Sex + Eye*Sex,
                main = "Model: (Hair Sex) (Eye Sex)")


####################
## Premarital Sex ##
####################

data(PreSex)

## Mosaic display for Gender and Premarital Sexual Expirience ##

## (Gender Pre) ##
grid.mosaicplot(margin.table(PreSex, c(3, 4)), legend = FALSE,
                main = "Gender and Premarital Sex")

## (Gender Pre)(Extra) ##
grid.mosaicplot(margin.table(PreSex,c(2,3,4)), legend = FALSE,
                margin = ~ Gender * PremaritalSex + ExtramaritalSex ,
                main = "(PreMaritalSex Gender) (Sex)")

## (Gender Pre Extra)(Marital) ##
grid.mosaicplot(PreSex,
                margin = ~ Gender * PremaritalSex * ExtramaritalSex + MaritalStatus,
                legend = FALSE,
                main = "(PreMarital ExtraMarital) (MaritalStatus)")

## (GPE)(PEM) ##
grid.mosaicplot(PreSex,
                margin = ~ Gender * PremaritalSex * ExtramaritalSex
                         + MaritalStatus * PremaritalSex * ExtramaritalSex,
                legend = FALSE,
                main = "(G P E) (P E M)")

############################
## Employment Status Data ##
############################

data(Employment)

## Employment Status ##
grid.mosaicplot(Employment,
                margin = ~ LayoffCause * EmploymentLength + EmploymentStatus,
                main = "(Layoff Employment) + (EmployStatus)")


grid.mosaicplot(Employment,
                margin = ~ LayoffCause * EmploymentLength +
                           LayoffCause * EmploymentStatus,
                main = "(Layoff EmpL) (Layoff EmplS)")

## Closure ##
grid.mosaicplot(Employment[,,1], main = "Layoff : Closure")

## Replaced ##
grid.mosaicplot(Employment[,,2], main = "Layoff : Replaced")


#####################
## Mosaic Matrices ##
#####################

data(UCBAdmissions)

grid.mosaicpairs(PreSex)

grid.mosaicpairs(UCBAdmissions)

grid.mosaicpairs(UCBAdmissions, type="conditional")

grid.mosaicpairs(UCBAdmissions, type="pairwise", gp = gp.max)



