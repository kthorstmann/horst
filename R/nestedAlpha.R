




# function -----------------------------------------------------------

# this function estmates the reliability of scales used in MLM models, following the article from Nezlek, 2016.

# sample data frame

# 10 persons
person <- sort(rep(letters[1:10], 40))
table(person)
# 10 days, 4 items per day:
day <- rep(sort(rep(seq(1:10), 4)), 10)
table(day)
# 4 items per day
variable <- rnorm(400, 50, 10)

data <- data.frame(person, day, variable, stringsAsFactors = FALSE)
head(data)


# input to the function:

data = data
item.level.1 = "variable"
level.2      = "day"
level.3      = "person"

library(stringr)


# 3 level model in lme4
library(lme4)



## random effect variance
# level 1 = residual: 9.318e+01
# level 2 = day: 0.000e+00

# lvl1 <- attr(VarCorr(mod), "sc")^2
# lvl2 <- attr(VarCorr(mod)[[2]], "stddev")["(Intercept)"]^2
# icc <- lvl2/(lvl1+lvl2)


nestedAlpha <- function(){
  model.string <- stringr::str_c("lme4::lmer(", item.level.1,
                                 " ~ 1 + (1 | ", level.3,
                                 ") + (1 | ", level.2,
                                 "), data = data)")

  model <- eval(parse(text = model.string))

  # model <- lmer(variable ~ 1 + (1 | person) + (1 | day), data = data)
  # summary(model)

  # extract variance:

  # item level reliability:
  # (level 1 variance)
  var_item_lvl1 <- attr(VarCorr(model), "sc")^2
  # (level 2 variance)
  var_occasion_lvl2 <- attr(VarCorr(model)[[2]], "stddev")["(Intercept)"]^2

  # length of scale
  # determine length of scale:
  p <- max(table(data$day, data$person))


  # final reliability
  item_alpha <- var_occasion_lvl2/(var_occasion_lvl2 + (var_item_lvl1/p))
  attr(item_alpha, "names") <- "alpha"

  item_alpha


}
