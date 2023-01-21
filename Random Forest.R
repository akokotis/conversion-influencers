# Search Relevancy Weights _ dataset 1

#Load Database -- needs to be done in 3.2.3
library(nzr)
nzConnect(machine='', database='', user='', password='xxxx')

data=nzQuery('(SELECT * FROM AK_RESEARCH_DATASET_v2 where PURCHASE_FLAG=0 order by RANDOM()) union all (SELECT * FROM AK_RESEARCH_DATASET_v2 where PURCHASE_FLAG=1 order by RANDOM())')

# Disconnect
nzDisconnect()

#Format data
data$GENDER[which(is.na(data$GENDER))]="*NA"
data_full=na.omit(data)

### Random Forest Setup

#new vars based on keywords
require("dplyr")
require("data.table")
data_full_v2=data_full %>%
  mutate(act_ALL=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'ALL', 1, 0))) %>%
  mutate(act_BACKCOUNTRY=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'BACKCOUNTRY', 1, 0))) %>%
  mutate(act_BACKPACKING=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'BACKPACKING', 1, 0))) %>%
  mutate(act_BIKE=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'BIKE', 1, 0))) %>%
  mutate(act_BIKING=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'BIKING', 1, 0))) %>%
  mutate(act_CAMP=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'CAMP', 1, 0))) %>%
  mutate(act_CAMPING=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'CAMPING', 1, 0))) %>%
  mutate(act_CANOEING=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'CANOEING', 1, 0))) %>%
  mutate(act_CANYONEERING=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'CANYONEERING', 1, 0))) %>%
  mutate(act_CAR=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'CAR', 1, 0))) %>%
  mutate(act_CLIMB=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'CLIMB', 1, 0))) %>%
  mutate(act_CLIMBING=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'CLIMBING', 1, 0))) %>%
  mutate(act_COUNTRY=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'COUNTRY', 1, 0))) %>%
  mutate(act_CROSS=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'CROSS', 1, 0))) %>%
  mutate(act_CYCLE=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'CYCLE', 1, 0))) %>%
  mutate(act_CYCLING=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'CYCLING', 1, 0))) %>%
  mutate(act_DAY=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'DAY', 1, 0))) %>%
  mutate(act_DISPLACEMENT=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'DISPLACEMENT', 1, 0))) %>%
  mutate(act_DOWNHILL=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'DOWNHILL', 1, 0))) %>%
  mutate(act_E=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'E', 1, 0))) %>%
  mutate(act_EDGED=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'EDGED', 1, 0))) %>%
  mutate(act_ENDURO=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'ENDURO', 1, 0))) %>%
  mutate(act_FISHING=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'FISHING', 1, 0))) %>%
  mutate(act_FIT=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'FIT', 1, 0))) %>%
  mutate(act_FITNESS=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'FITNESS', 1, 0))) %>%
  mutate(act_FREERIDE=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'FREERIDE', 1, 0))) %>%
  mutate(act_GYM=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'GYM', 1, 0))) %>%
  mutate(act_HIKING=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'HIKING', 1, 0))) %>%
  mutate(act_HYBRID=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'HYBRID', 1, 0))) %>%
  mutate(act_ICE=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'ICE', 1, 0))) %>%
  mutate(act_INFLATABLE=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'INFLATABLE', 1, 0))) %>%
  mutate(act_KAYAK=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'KAYAK', 1, 0))) %>%
  mutate(act_KAYAKING=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'KAYAKING', 1, 0))) %>%
  mutate(act_LEISURE=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'LEISURE', 1, 0))) %>%
  mutate(act_METAL=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'METAL', 1, 0))) %>%
  mutate(act_MOUNTAIN=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'MOUNTAIN', 1, 0))) %>%
  mutate(act_MOUNTAINEERING=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'MOUNTAINEERING', 1, 0))) %>%
  mutate(act_MULTI=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'MULTI', 1, 0))) %>%
  mutate(act_OVERNIGHT=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'OVERNIGHT', 1, 0))) %>%
  mutate(act_PERFORMANCE=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'PERFORMANCE', 1, 0))) %>%
  mutate(act_PLANING=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'PLANING', 1, 0))) %>%
  mutate(act_PLAY=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'PLAY', 1, 0))) %>%
  mutate(act_POWDER=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'POWDER', 1, 0))) %>%
  mutate(act_RAPPELLING=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'RAPPELLING', 1, 0))) %>%
  mutate(act_REC=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'REC', 1, 0))) %>%
  mutate(act_RECREATIONAL=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'RECREATIONAL', 1, 0))) %>%
  mutate(act_ROAD=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'ROAD', 1, 0))) %>%
  mutate(act_RUNNING=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'RUNNING', 1, 0))) %>%
  mutate(act_SAND=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'SAND', 1, 0))) %>%
  mutate(act_SKATE=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'SKATE', 1, 0))) %>%
  mutate(act_SKIING=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'SKIING', 1, 0))) %>%
  mutate(act_SNORKELING=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'SNORKELING', 1, 0))) %>%
  mutate(act_SNOW=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'SNOW', 1, 0))) %>%
  mutate(act_SNOWBOARDING=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'SNOWBOARDING', 1, 0))) %>%
  mutate(act_SNOWSHOEING=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'SNOWSHOEING', 1, 0))) %>%
  mutate(act_SPLITBOARDING=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'SPLITBOARDING', 1, 0))) %>%
  mutate(act_SPORT=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'SPORT', 1, 0))) %>%
  mutate(act_SPORTS=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'SPORTS', 1, 0))) %>%
  mutate(act_SUN=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'SUN', 1, 0))) %>%
  mutate(act_SUPING=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'SUPING', 1, 0))) %>%
  mutate(act_SWIMMING=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'SWIMMING', 1, 0))) %>%
  mutate(act_THRU=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'THRU', 1, 0))) %>%
  mutate(act_TOP=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'TOP', 1, 0))) %>%
  mutate(act_TOUR=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'TOUR', 1, 0))) %>%
  mutate(act_TOURING=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'TOURING', 1, 0))) %>%
  mutate(act_TRAD=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'TRAD', 1, 0))) %>%
  mutate(act_TRAIL=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'TRAIL', 1, 0))) %>%
  mutate(act_TRANSIT=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'TRANSIT', 1, 0))) %>%
  mutate(act_ULTRALIGHT=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'ULTRALIGHT', 1, 0))) %>%
  mutate(act_VERTICAL=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'VERTICAL', 1, 0))) %>%
  mutate(act_WATER=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'WATER', 1, 0))) %>%
  mutate(act_WATERSPORTS=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'WATERSPORTS', 1, 0))) %>%
  mutate(act_WHITEWATER=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'WHITEWATER', 1, 0))) %>%
  mutate(act_WIDE=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'WIDE', 1, 0))) %>%
  mutate(act_XC=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'XC', 1, 0))) %>%
  mutate(act_YOGA=as.factor(ifelse(PRIMARY_ACTIVITY_DESC %like% 'YOGA', 1, 0)))

data_full_v2$PURCHASE_FLAG=as.factor(data_full_v2$PURCHASE_FLAG)
data_full_v2$PRODUCT_PAGE_SIZE_COUNT=as.integer(data_full_v2$PRODUCT_PAGE_SIZE_COUNT)
data_full_v2$PRODUCT_PAGE_COLOR_COUNT=as.integer(data_full_v2$PRODUCT_PAGE_COLOR_COUNT)
data_full_v2$AVG_SKU_PRICE=as.numeric(data_full_v2$AVG_SKU_PRICE)
data_full_v2$MIN_SKU_PRICE=as.numeric(data_full_v2$MIN_SKU_PRICE)
data_full_v2$MAX_SKU_PRICE=as.numeric(data_full_v2$MAX_SKU_PRICE)
data_full_v2$AVAILABLE_TO_SELL_QTY=as.numeric(data_full_v2$AVAILABLE_TO_SELL_QTY)
data_full_v2$TOTAL_4_WEEK_SALES_QTY=as.numeric(data_full_v2$TOTAL_4_WEEK_SALES_QTY)
data_full_v2$PRODUCT_AGE_DAYS=as.numeric(data_full_v2$PRODUCT_AGE_DAYS)
data_full_v2$REVIEW_COUNT=as.integer(data_full_v2$REVIEW_COUNT)
data_full_v2$REVIEW_SCORE=as.numeric(data_full_v2$REVIEW_SCORE)
data_full_v2$EVENT_FLAG=as.factor(data_full_v2$EVENT_FLAG)
data_full_v2$USA_FLAG=as.factor(data_full_v2$USA_FLAG)
data_full_v2$CHINA_FLAG=as.factor(data_full_v2$CHINA_FLAG)

data_full_v2$ FISCAL_DAY_NAME =as.factor(data_full_v2$ FISCAL_DAY_NAME)
data_full_v2$ STATE_NAME    =as.factor(data_full_v2$ STATE_NAME)
data_full_v2$ DISTRICT_NAME  =as.factor(data_full_v2$ DISTRICT_NAME)
data_full_v2$ CBSA_NAME  =as.factor(data_full_v2$ CBSA_NAME)

data_full_v2$ DEVICE_TYPE   =as.factor(data_full_v2$ DEVICE_TYPE)
data_full_v2$ GENDER     =as.factor(data_full_v2$ GENDER)
data_full_v2$ SHOP_DESC    =as.factor(data_full_v2$ SHOP_DESC)

data_full_v2$ DEPARTMENT_DESC    =as.factor(data_full_v2$ DEPARTMENT_DESC)
data_full_v2$ CLASS_DESC    =as.factor(data_full_v2$ CLASS_DESC)
data_full_v2$ SUBCLASS_DESC    =as.factor(data_full_v2$ SUBCLASS_DESC)

data_full_v2$ SKILL_LEVEL_DESC  =as.factor(data_full_v2$ SKILL_LEVEL_DESC)
data_full_v2$ SILHOUETTE_DESC   =as.factor(data_full_v2$ SILHOUETTE_DESC)
data_full_v2$ VALUE_TIER_DESC   =as.factor(data_full_v2$ VALUE_TIER_DESC)

data_full_v2$ AVG_4_WEEK_SALES_QTY=as.numeric(data_full_v2$ AVG_4_WEEK_SALES_QTY)
data_full_v2$ TOTAL_28_DAY_DEMAND_QTY=as.numeric(data_full_v2$ TOTAL_28_DAY_DEMAND_QTY)
data_full_v2$ AVG_28_DAY_DEMAND_QTY=as.numeric(data_full_v2$ AVG_28_DAY_DEMAND_QTY)
data_full_v2$ WEEKS_OF_SUPPLY_VIA_SALE=as.numeric(data_full_v2$ WEEKS_OF_SUPPLY_VIA_SALE)
data_full_v2$ WEEKS_OF_SUPPLY_VIA_DEMAND=as.numeric(data_full_v2$ WEEKS_OF_SUPPLY_VIA_DEMAND)

###
New Vars
    #bucket continuous
    #Fiscal Day - Monday, Fri+Sat+Sun, Tu+We+Thu
    #Bucket Product Age
    #Sizes available seems important
-weekday_bucket
-age_category


#######

data_full_v2=data_full_v2 %>%
    mutate(PRODUCT_PAGE_SIZE_COUNT_log=as.numeric(log(PRODUCT_PAGE_SIZE_COUNT))) %>%
  mutate(PRODUCT_PAGE_COLOR_COUNT_log=as.numeric(log(PRODUCT_PAGE_COLOR_COUNT))) %>%
  mutate(AVG_SKU_PRICE_log=as.numeric(log(AVG_SKU_PRICE))) %>%
  mutate(MIN_SKU_PRICE_log=as.numeric(log(MIN_SKU_PRICE))) %>%
  mutate(MAX_SKU_PRICE_log=as.numeric(log(MAX_SKU_PRICE))) %>%
  mutate(AVAILABLE_TO_SELL_QTY_log=as.numeric(log(AVAILABLE_TO_SELL_QTY))) %>%
  mutate(TOTAL_4_WEEK_SALES_QTY_log=as.numeric(log(TOTAL_4_WEEK_SALES_QTY))) %>%
  
  mutate(AVG_4_WEEK_SALES_QTY_log=as.numeric(log(AVG_4_WEEK_SALES_QTY))) %>%
  mutate(TOTAL_28_DAY_DEMAND_QTY_log=as.numeric(log(TOTAL_28_DAY_DEMAND_QTY))) %>%
  mutate(AVG_28_DAY_DEMAND_QTY_log=as.numeric(log(AVG_28_DAY_DEMAND_QTY))) %>%
  mutate(WEEKS_OF_SUPPLY_VIA_SALE_log=as.numeric(log(WEEKS_OF_SUPPLY_VIA_SALE))) %>%
  mutate(WEEKS_OF_SUPPLY_VIA_DEMAND_log=as.numeric(log(WEEKS_OF_SUPPLY_VIA_DEMAND))) %>%

  mutate(PRODUCT_AGE_DAYS_log=as.numeric(log(PRODUCT_AGE_DAYS))) %>%
  mutate(REVIEW_COUNT_log=as.numeric(log(REVIEW_COUNT))) %>%
  mutate(REVIEW_SCORE_log=as.numeric(log(REVIEW_SCORE)))

str(data_full_v2, list.len=ncol(data_full_v2))
data_orig_naomit2=na.omit(data_full_v2)
nrow(data_orig_naomit2)
setwd("~/Research")
save(data_orig_naomit2, file="data_orig_clean.RData")
 
### Variables for the model 
colnames(data_orig_naomit2)
vars= c("PURCHASE_FLAG",
        "FISCAL_DAY_NAME",
        "EVENT_FLAG",
        "STATE_NAME",
        "DISTRICT_NAME",
        "DEVICE_TYPE",
        "GENDER",
        "USA_FLAG",
        "CHINA_FLAG",
        "SHOP_DESC",
        "DEPARTMENT_DESC",
        "SILHOUETTE_DESC",
        "VALUE_TIER_DESC",
        "SKILL_LEVEL_DESC",
        "act_ALL",
        "act_BACKCOUNTRY",
        "act_BACKPACKING",                "act_BIKE",                       "act_BIKING",                     "act_CAMP",
        "act_CAMPING",                    "act_CANOEING",                   "act_CANYONEERING", "act_CAR",
        "act_CLIMB",                      "act_CLIMBING",                   "act_COUNTRY", "act_CROSS",
        "act_CYCLE",                      "act_CYCLING",                    "act_DAY", "act_DISPLACEMENT",
        "act_DOWNHILL",                   "act_E",                          "act_EDGED","act_ENDURO",
        "act_FISHING",                    "act_FIT",                        "act_FITNESS","act_FREERIDE",
        "act_GYM",                        "act_HIKING",                     "act_HYBRID", "act_ICE",
        "act_INFLATABLE",                 "act_KAYAK",                      "act_KAYAKING", "act_LEISURE",
        "act_METAL",                      "act_MOUNTAIN",                   "act_MOUNTAINEERING",             "act_MULTI",
        "act_OVERNIGHT",                  "act_PERFORMANCE",                "act_PLANING",                    "act_PLAY",
        "act_POWDER",                     "act_RAPPELLING",                 "act_REC",                        "act_RECREATIONAL",
        "act_ROAD",                       "act_RUNNING",                    "act_SAND",                       "act_SKATE",
        "act_SKIING",                     "act_SNORKELING",                 "act_SNOW",                       "act_SNOWBOARDING",
        "act_SNOWSHOEING",                "act_SPLITBOARDING",              "act_SPORT",                      "act_SPORTS",
        "act_SUN",                        "act_SUPING",                     "act_SWIMMING",                   "act_THRU",
        "act_TOP",                        "act_TOUR",                       "act_TOURING",                    "act_TRAD",
        "act_TRAIL",                      "act_TRANSIT",                    "act_ULTRALIGHT",                 "act_VERTICAL",
        "act_WATER",                      "act_WATERSPORTS",                "act_WHITEWATER",                 "act_WIDE",
        "act_XC",                         "act_YOGA",                       "PRODUCT_PAGE_SIZE_COUNT_log",
        "PRODUCT_PAGE_COLOR_COUNT_log",   "AVG_SKU_PRICE_log",              "MIN_SKU_PRICE_log",              "MAX_SKU_PRICE_log",
        "AVAILABLE_TO_SELL_QTY_log",      "TOTAL_4_WEEK_SALES_QTY_log",     "AVG_4_WEEK_SALES_QTY_log",      
        "PRODUCT_AGE_DAYS_log",            "REVIEW_COUNT_log",               "REVIEW_SCORE_log","TOTAL_28_DAY_DEMAND_QTY_log",
        "AVG_28_DAY_DEMAND_QTY_log","WEEKS_OF_SUPPLY_VIA_DEMAND_log")
        
vars2= c("PURCHASE_FLAG",
        "FISCAL_DAY_NAME",
        "EVENT_FLAG",
        "STATE_NAME",
        "DISTRICT_NAME",
        "DEVICE_TYPE",
        "GENDER",
        "SHOP_DESC",
        "DEPARTMENT_DESC",
        "PRODUCT_PAGE_SIZE_COUNT_log",
        "PRODUCT_PAGE_COLOR_COUNT_log",   "AVG_SKU_PRICE_log",              "MIN_SKU_PRICE_log",              "MAX_SKU_PRICE_log",
        "AVAILABLE_TO_SELL_QTY_log",      "TOTAL_4_WEEK_SALES_QTY_log",     "AVG_4_WEEK_SALES_QTY_log",      
        "PRODUCT_AGE_DAYS_log",            "REVIEW_COUNT_log",               "REVIEW_SCORE_log")        

data_rf=data_orig_naomit2[,vars]
data_rf2=data_orig_naomit2[,vars2]

str(data_rf)
setwd("~/Research")
save(data_rf,file= "data_rf.Rdata")
save(data_rf2,file= "data_rf2.Rdata")

#install.packages("randomForest")
#install.packages('ranger') >3.1
#install.packages('h2o')
require("randomForest")
require('ranger')
require("ggplot2")
require('h2o')




#model 1: Method 1
rf2=randomForest(PURCHASE_FLAG~.,data_rf, importance=T)
save(rf,file = "rf_1.RData")
imp=importance(rf)
imp2=imp[,ncol(imp)-1]
rf.vars=names(imp2)[order(imp2, decreasing=T)]

#model 1: Method 2
rf_ranger=ranger(PURCHASE_FLAG~.,data = data_rf, write.forest = TRUE, importance = "impurity") #Gini impurity
save(rf_ranger,file = "rf_ranger_1_v2.RData")
imp_ranger=importance(rf_ranger)
  #Variables with high importance are drivers of the outcome and their values have a significant impact on the outcome values.
  #The importance measure is based on the decrease of Gini impurity when a variable is chosen to split a node.
  #which attribute gives us the most information about which class a new data point belongs to
  #Mean Decrease in Gini is the average (mean) of a variable’s total decrease in node impurity, weighted by the proportion of samples reaching that node in each individual decision tree in the random forest. This is effectively a measure of how important a variable is for estimating the value of the target variable across all of the trees that make up the forest. A higher Mean Decrease in Gini indicates higher variable importance.
imp_ranger_frame=data.frame(imp_ranger)[order(imp_ranger, decreasing=T),,drop = FALSE]

ggplot(imp_ranger_frame[1:20,,drop = FALSE], aes(x=reorder(rownames(imp_ranger_frame[1:20,,drop = FALSE]),imp_ranger[1:20,drop = FALSE]), y=imp_ranger[1:20,drop = FALSE],fill=imp_ranger))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("Variables")+
  ggtitle("Gini Impurity: Mean Decrease")+
  scale_fill_gradient(low="red", high="blue")


imp_p_ranger=importance_pvalues(rf_ranger, method = c("altmann"), formula = PURCHASE_FLAG~., data = data_rf)
imp_p_ranger_frame=data.frame(imp_p_ranger)[order(imp_p_ranger_frame$pvalue, decreasing=F),,drop = FALSE]
write.csv(imp_p_ranger_frame,"Importance-pvalues.csv")


#model 1: Method 3
  #https://cran.r-project.org/web/packages/h2o/h2o.pdf
  #http://docs.h2o.ai/h2o/latest-stable/h2o-docs/performance-and-prediction.html#logloss
  #localH2O = h2o.init()
#training_frame' must be a valid H2OFrame

X=2:105
y=1
  
data_rf_h2o=as.h2o(data_rf)
data_rf2_h2o=as.h2o(data_rf2)

splits=h2o.splitFrame(
  data_rf_h2o,           ##  splitting the H2O frame we read above
  c(0.6,0.2),   ##  create splits of 60% and 20%; 
  ##  H2O will create one more split of 1-(sum of these parameters)
  ##  so we will get 0.6 / 0.2 / 1 - (0.6+0.2) = 0.6/0.2/0.2
  seed=1234)

h2o_data=list()
h2o_data$train=h2o.assign(splits[[1]], "train.hex")   
h2o_data$valid=h2o.assign(splits[[2]], "valid.hex")   ## R valid, H2O valid.hex
h2o_data$test=h2o.assign(splits[[3]], "test.hex")

rf_h2o=h2o.randomForest(training_frame = h2o_data$train, validation_frame = h2o_data$valid, x=2:105,y=1,model_id = "rf_h2o_v1")
save(rf_h2o,file = "rf_h2o_1.RData")

rf.VI = h2o.varimp(rf_h2o)
imp_h2o_frame=data.frame(rf.VI)[order(rf.VI$relative_importance, decreasing=T),,drop = FALSE]
write.csv(imp_h2o_frame,"Importance-H2o-1.csv")

h2o.varimp_plot(rf_h2o, num_of_features = 20)

      # define random grid search parameters
      ntrees_opts = seq(100, 500, 50)
      max_depth_opts = seq(2, 20, 2)
      sample_rate_opts = seq(0.1, 0.9, 0.1)
      col_sample_rate_opts = seq(0.1, 0.9, 0.1)
      
      hyper_params = list(ntrees = ntrees_opts,
                          max_depth = max_depth_opts,
                          sample_rate = sample_rate_opts,
                          col_sample_rate = col_sample_rate_opts)
      
      # search a random subset of these hyper-parmameters
      # max runtime and max models are enforced
      # and the search will stop after not improving much over the best 5 random models
      search_criteria = list(strategy = "RandomDiscrete", 
                             max_runtime_secs = 300, 
                             max_models = 10, 
                             stopping_metric = "AUC", 
                             stopping_tolerance = 0.0001, 
                             stopping_rounds = 5, 
                             seed = 12345)
      
      # execute training w/ grid search
      X=2:105
      y=1
      
      loan_gbm <- h2o.grid("xgboost", 
                           grid_id = "gbm_grid",
                           x = X, 
                           y = y, 
                           training_frame = h2o_data$train,
                           validation_frame = h2o_data$valid,
                           
                           # per model stopping criteria 
                           stopping_rounds = 2,
                           stopping_tolerance = 1e-3,
                           stopping_metric = "AUC",
                           
                           # how often to score (affects early stopping, training speed)
                           score_tree_interval = 5, 
                           
                           # seed to control sampling 
                           seed = 12345,
                           # grid serach options
                           hyper_params = hyper_params,
                           search_criteria = search_criteria)
      
      # view detailed results at http://ip:port/flow/index.html
      
      # show grid results
      sorted_grid <- h2o.getGrid(grid_id = "gbm_grid")
      print(sorted_grid)
      
      # select best model
      best_model <- h2o.getModel(sorted_grid@model_ids[[1]])
      summary(best_model)
      # A perfect classifier has AUC = 1 and a completely random classifier has AUC = 0.5 (evaluate how well a binary classification model is able to distinguish between true positives and false positives)
      # A Gini index of zero expresses perfect equality (or a totally useless classifier)
      # Logloss can be any value greater than or equal to 0, with 0 meaning that the model correctly assigns a probability of 0% or 100%.
      # MSE (average of the squares of the errors or deviations) - the lower the MSE the higher the accuracy of prediction
      # RMSE sensitive to outliers
      
      # use variable importance to get insight into important relationships
      h2o.varimp(best_model)
      h2o.varimp_plot(best_model, num_of_features = 20)
      
      # use partial dependence plots to get insight into important relationships
      h2o.partialPlot(best_model, h2o_data$valid, "REVIEW_SCORE_log")
      h2o.partialPlot(best_model, h2o_data$valid, "PRODUCT_PAGE_COLOR_COUNT_log")
      h2o.partialPlot(best_model, h2o_data$valid, "PRODUCT_PAGE_SIZE_COUNT_log")
      h2o.partialPlot(best_model, h2o_data$valid, "DEVICE_TYPE.DESKTOP")
      h2o.partialPlot(best_model, h2o_data$valid, "TOTAL_28_DAY_DEMAND_QTY_log")
      h2o.partialPlot(best_model, h2o_data$valid, "MAX_SKU_PRICE_log")
      h2o.partialPlot(best_model, h2o_data$valid, "REVIEW_COUNT_log")
      h2o.partialPlot(best_model, h2o_data$valid, "EVENT_FLAG.0")
      
      h2o.shutdown(prompt = FALSE)


#model 1: Method 4
gbm_h2o = h2o.gbm(
  training_frame = h2o_data$train,        ## the H2O frame for training
  validation_frame = h2o_data$valid,      ## the H2O frame for validation (not required)
  x=2:105,                        ## the predictor columns, by column index
  y=1,                          ## the target index (what we are predicting)
  model_id = "gbm_v1",     ## name the model in H2O
  seed = 2000000)                ## Set the random seed for reproducability

summary(gbm_h2o)


#model 1: Method 5
  #https://cran.r-project.org/web/packages/xgboost/xgboost.pdf
  #XGBoost
#install.packages("xgboost")
require("xgboost")
library(readr)
library(stringr)
library(caret)

sparse.matrix.train= sparse.model.matrix(PURCHASE_FLAG~., data = data_rf)

xgb_data=list()
xgb_data$data=as.numeric(data.matrix(data_rf[,-1]))
xgb_data$label=data.matrix(data_rf$PURCHASE_FLAG)

xgb=xgboost(data = xgb_data$data, 
               label = xgb_data$label,
               eval_metric = "merror",
               objective = "multi:softprob"
               )
