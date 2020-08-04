library(tidyverse)

#Read in project dataframe
df <- read.csv("projectdf.csv")

#SPY chisquare test
SPYChiSquareTest <- df %>% 
  mutate(SPY = if_else(SPY_TomorrowMedian > SPY_CurrentMedian, "Good", "Bad"), #Generate Success Metric features
         VIX = if_else(VIX_TomorrowMedian > VIX_CurrentMedian, "Bad", "Good"),
         sentiment = if_else(sentiment > 0, "Positive", "Negative")) %>% 
  select(SPY, VIX, sentiment) %>% 
  gather(key = "market", value = "value", -sentiment) %>% #Count Positive Negative sentiment for each Good and Bad financial outcome
  group_by(sentiment, market) %>% 
  count(value) %>% 
  spread(key = "value", value = "n") %>% #Formats the data so it can be used in a chi-square test
  ungroup() %>% 
  filter(market == "SPY") %>% 
  select(-market, -sentiment) %>% 
  as.data.frame() %>% 
  chisq.test()

#p-value of .07335, We do not reject the null hypothesis, there is no interaction
SPYChiSquareTest

#SPY chisquare data table 
#Sanity check to make sure chisquare test is receiving correct data
df %>% 
  mutate(SPY = if_else(SPY_TomorrowMedian > SPY_CurrentMedian, "Good", "Bad"), 
         VIX = if_else(VIX_TomorrowMedian > VIX_CurrentMedian, "Bad", "Good"),
         sentiment = if_else(sentiment > 0, "Positive", "Negative")) %>% 
  select(SPY, VIX, sentiment) %>% 
  gather(key = "market", value = "value", -sentiment) %>% 
  group_by(sentiment, market) %>% 
  count(value) %>% 
  spread(key = "value", value = "n") %>% 
  ungroup() %>% 
  filter(market == "SPY") %>% 
  select(-market)


#VIX chisquare test
#Same code from SPY chisqure test
VIXChiSquareTest <-  df %>% 
  mutate(SPY = if_else(SPY_TomorrowMedian > SPY_CurrentMedian, "Good", "Bad"),
         VIX = if_else(VIX_TomorrowMedian > VIX_CurrentMedian, "Bad", "Good"),
         sentiment = if_else(sentiment > 0, "Positive", "Negative")) %>% 
  select(SPY, VIX, sentiment) %>% 
  gather(key = "market", value = "value", -sentiment) %>% 
  group_by(sentiment, market) %>% 
  count(value) %>% 
  spread(key = "value", value = "n") %>% 
  ungroup() %>% 
  filter(market == "VIX") %>% 
  select(-market, -sentiment) %>% 
  as.data.frame() %>% 
  chisq.test()

#P-value is .1645, we do not reject the null hypothesis
VIXChiSquareTest

#VIX chisqure table
df %>% 
  mutate(SPY = if_else(SPY_TomorrowMedian > SPY_CurrentMedian, "Good", "Bad"),
         VIX = if_else(VIX_TomorrowMedian > VIX_CurrentMedian, "Bad", "Good"),
         sentiment = if_else(sentiment > 0, "Positive", "Negative")) %>% 
  select(SPY, VIX, sentiment) %>% 
  gather(key = "market", value = "value", -sentiment) %>% 
  group_by(sentiment, market) %>% 
  count(value) %>% 
  spread(key = "value", value = "n") %>% 
  ungroup() %>% 
  filter(market == "VIX") %>% 
  select(-market)

#VIX SPY pos/neg sentiment and pos/neg performance comparison 
#When plotting count of positive and negative sentiment for each market success metric 
#We can see each metricshares a relatively similar amound of sentiment
#SPY negative performance and #SPY positive peerformance have similar counts of Positive and Negative sentiment
#This confirms the result fo the chisqure tests from above 
df %>% 
  mutate(PosNegSPY = if_else(SPY_TomorrowMedian > SPY_CurrentMedian, "PositivePerformance", "NegativePerformance"), #Generates market success metrics
         PosNegVIX = if_else(VIX_TomorrowMedian > VIX_CurrentMedian, "NegativePerformance", "PositivePerformance"),
         Sentiment = if_else(sentiment > 0, "PositiveSentiment", "NegativeSentiment")) %>% 
  select(PosNegSPY, PosNegVIX, Sentiment) %>% 
  mutate(PosNegSPY = str_replace(PosNegSPY, "Performance", " Performance")) %>% #Cleans text for easier interpretation
  mutate(PosNegVIX = str_replace(PosNegVIX, "Performance", " Performance")) %>% 
  mutate(Sentiment = str_replace(Sentiment, "Sentiment", "")) %>% 
  gather(key = "market", value = "performance", -Sentiment) %>% 
  group_by(Sentiment, market) %>% 
  count(performance) %>% 
  ungroup() %>% 
  mutate(market = str_replace(market, "PosNeg", "")) %>% 
  arrange(market, performance, Sentiment) %>% 
  ggplot(aes(x = Sentiment, y = n, fill = Sentiment)) +
  geom_col(color = "white") + 
  facet_wrap(~market + performance) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 20))


#SPY, VIX, and Sentiment Timeseries Chart
#Sentiment and VIX share a similar trend 
#Also SPY and VIX appear to have an inverse relationship 
df %>% 
  select(SPY_CurrentMedian, VIX_CurrentMedian, sentiment, `date`) %>%
  rename(., "SPY Median Price" = SPY_CurrentMedian, "VIX Median Price" = VIX_CurrentMedian) %>% 
  mutate(date = as.Date(date)) %>% 
  gather(key = "type", value = "value", -`date`) %>% 
  ggplot(aes(x = `date`, y= value, color = type, group = type)) + 
  geom_smooth(se = FALSE) + 
  geom_point(size = 5) +
  facet_wrap(~type, scales = "free") + 
  scale_x_date(date_breaks = "1 month") + 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.position = "none",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 20))



#Facet chart for exploratory analysis and inspiration
df %>% 
  mutate(PosNegSPY = if_else(SPY_TomorrowMedian > SPY_CurrentMedian, "PositivePerformance", "NegativePerformance"),
         PosNegVIX = if_else(VIX_TomorrowMedian > VIX_CurrentMedian, "NegativePerformance", "PositivePerformance"),
         SentimentCat = if_else(sentiment > 0, "PositiveSentiment", "NegativeSentiment")) %>%
  select(daily_fav_per_tweet, daily_rt_per_tweet, sentiment, PosNegSPY, PosNegVIX, SentimentCat, SPY_CurrentMedian,VIX_CurrentMedian) %>% GGally::ggpairs(aes(color = SentimentCat))



#Comparison of Market volatility and Market value 
#SPY and VIX have a negative linear relationship
#Sentiment appears to be randomly distributed
df %>% 
  select(SPY_CurrentMedian, VIX_CurrentMedian, sentiment) %>% 
  mutate(sentiment = if_else(sentiment > 0, "Positive", "Negative")) %>% 
  ggplot(aes(x = VIX_CurrentMedian, y = SPY_CurrentMedian, color = sentiment)) +
  geom_smooth(se= FALSE, method = "lm", color = "black", linetype = "dotted") +
  geom_point(size = 5) + 
  annotate("label", x = 20, y = 297.5, size = 10, label = "Blue is positive sentiment, Red is negative sentiment \n There is a negative linear relationship between SPY and VIX \n As market volatility increases, market value decreaes") +
  xlab("VIX (Market Volatility)") + 
  ylab("SPY (Market Value)") + 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.position = "none") 
  

#Distribution of SPY and VIX Daily Change by Sentiment
#Distributions appear to be relatively similar
df %>%
  select(sentiment, SPY_CurrentMedian, SPY_TomorrowMedian, VIX_CurrentMedian, VIX_TomorrowMedian) %>% 
  mutate(SPYDailyChange = (SPY_TomorrowMedian - SPY_CurrentMedian)/SPY_CurrentMedian,
         VIXDailyChange = (VIX_TomorrowMedian - VIX_CurrentMedian)/ VIX_CurrentMedian) %>% 
  mutate(score = sentiment) %>% 
  mutate(sentiment = if_else(sentiment > 0, "Positive", "Negative")) %>% 
  select(sentiment, VIXDailyChange, SPYDailyChange) %>% 
  gather(key = "market", value = "value", -sentiment) %>% 
  ggplot(aes(x = value, fill = sentiment)) + 
  geom_density(alpha = .5, color = "white") + 
  facet_wrap(~market, scales = "free") +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.position = "none",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 20))

#BoxPlot of SPY and VIX Daily Change by Sentiment
#Boxplots show similar medians however, positive sentiment has a wider variance
df %>%
  select(sentiment, SPY_CurrentMedian, SPY_TomorrowMedian, VIX_CurrentMedian, VIX_TomorrowMedian) %>% 
  mutate(SPYDailyChange = (SPY_TomorrowMedian - SPY_CurrentMedian)/SPY_CurrentMedian,
         VIXDailyChange = (VIX_TomorrowMedian - VIX_CurrentMedian)/ VIX_CurrentMedian) %>% 
  mutate(score = sentiment) %>% 
  mutate(sentiment = if_else(sentiment > 0, "Positive", "Negative")) %>% 
  select(sentiment, VIXDailyChange, SPYDailyChange) %>% 
  gather(key = "market", value = "value", -sentiment) %>% 
  ggplot(aes(x = sentiment, y = value, color = sentiment)) + 
  geom_boxplot(color = "black") + 
  geom_jitter(size = 6, width = .1) +
  facet_wrap(~market, scales = "free") +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.position = "none",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 20))


#Correlation Between SPY and VIX
#User Spearman correlation (nonparametric)
#Highly Correlated -.9553556
df %>% 
  select(SPY_CurrentMedian, VIX_CurrentMedian) %>% 
  cor(method= c("pearson"))


#Simple Linear Regression with VIX Current Median price as a predictor for Tomorrow SPY median price
#Coeffcient is significant
#-1.6916 indicates a decreae in value of $1.69 in SPY as VIX increases by $1 or 1% annual volatility 
linearmodel <- lm(SPY_TomorrowMedian~VIX_CurrentMedian, df)
linearmodel %>% summary()
linearmodel$residuals %>% 
  as.data.frame() %>% 
  ggplot(aes(sample = .)) + 
  geom_qq() + 
  geom_qq_line() + 
  ylab("Standardized Residuals")