# Pre-analysis 

library(MatchIt)
library(dplyr)
library(ggplot2)

setwd("/home/ruser_usapkota/GitHub/ecls")
ecls <- read.csv("data-processed/ecls.csv")

#####

ecls %>%
    group_by(catholic) %>%
    summarise(n_students = n(),
              mean_math = mean(c5r2mtsc_std),
              std_error = sd(c5r2mtsc_std) / sqrt(n_students))

#####

with(ecls, t.test(c5r2mtsc_std ~ catholic))

##
ecls_cov <- c('race_white', 'p5hmage', 'w3income', 'p5numpla', 'w3momed_hsb')

ecls %>%
    group_by(catholic) %>%
    select(one_of(ecls_cov)) %>%
    summarise_all(funs(mean(.,na.rm=T)))

##

lapply(ecls_cov, function(v){
    t.test(ecls[,v] ~ ecls[,'catholic'])
})

##

ecls <- ecls %>%
    mutate (w3income_1k = w3income /1000)

##
m_ps <- glm(catholic ~ race_white + w3income_1k + p5hmage + p5numpla + w3momed_hsb,
            family = binomial(), data = ecls)

summary(m_ps)

##
prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                     catholic = m_ps$model$catholic)

head(prs_df)


########  Examining the region of common support ##############

labs <- paste("Actual school type attended:", c("Catholic", "Public"))
prs_df %>%
    mutate(catholic = ifelse(catholic == 1, labs[1], labs[2])) %>%
    ggplot(aes(x = pr_score)) + 
    geom_histogram(color = "white") + 
    facet_wrap(~catholic) + 
    xlab("Probability of going to Catholic school") + 
    theme_bw()


######## Executing a matching algorithm #########

ecls_nomiss <- ecls %>%  # MatchIt does not allow missing values
    select(c5r2mtsc_std, catholic, one_of(ecls_cov)) %>%
    na.omit()

mod_match <- matchit(catholic ~ race_white + w3income + p5hmage + p5numpla + w3momed_hsb,
                       method = "nearest", data = ecls_nomiss)

summary(mod_match)
plot(mod_match)

dta_m <- match.data(mod_match)


######### Visual Inspection ###############
fn_bal <- function(dta, variable) {
    dta$variable <- dta[, variable]
    if (variable == 'w3income') dta$variable <- dta$variable / 10^3
    dta$catholic <- as.factor(dta$catholic)
    support <- c(min(dta$variable), max(dta$variable))
    ggplot(dta, aes(x = distance, y = variable, color = catholic)) +
        geom_point(alpha = 0.2, size = 1.3) +
        geom_smooth(method = "loess", se = F) +
        xlab("Propensity score") +
        ylab(variable) +
        theme_bw() +
        ylim(support)
}

library(gridExtra)
grid.arrange(
    fn_bal(dta_m, "w3income"),
    fn_bal(dta_m, "p5numpla") + theme(legend.position = "none"),
    fn_bal(dta_m, "p5hmage"),
    fn_bal(dta_m, "w3momed_hsb") + theme(legend.position = "none"),
    fn_bal(dta_m, "race_white"),
    nrow = 3, widths = c(1, 0.8)
)

####### Difference in means #############


dta_m %>%
    group_by(catholic) %>%
    select(one_of(ecls_cov)) %>%
    summarise_all(funs(mean))

lapply(ecls_cov, function(v) {
    t.test(dta_m[,v] ~ dta_m$catholic)
})

###### Average absolute standardized difference ##########



