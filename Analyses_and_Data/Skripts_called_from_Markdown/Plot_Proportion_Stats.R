#Written by Alexander E. Hausmann (hausmann@bio.lmu.de) in 2019 - 2021

##### Plot Courtship Proportion data with estimators and significance for
##### comparison of different groups (e.g. species) and/or subgroups.
##### Or: Perform Bayesian statistics
##### Or: Don't plot, but only perform GLMM/GLM/binomial/Bayesian stats (whatever you choose).


#You can pass your data to the function either as a R data.frame or as
#a csv sheet. The table must have column names.

#If you actively want to "unactivate" certain parameters, you can
#do this by setting certain parameters to NA, c() (or NULL) or to "". Those parameters
#where this is possible are indicated in the explanations below.


#Explanation for each PARAMETER: 
####################

#ALWAYS RELEVANT:
#Statistical and Data Parameters 
####################

#input_data
#Data sheet name
#Name here the name of the data file
#Can be either an R data.frame object or the file directory of your csv 
#file. If it is a file directory, include the ".csv" in the end and
#pass it with quotation marks. If it is an R object, don't pass it with quotation marks.

#verbose
#Muting printed messages
#If set to F, no messages will be printed. If set to T, they appear.
#Default is T

#column_delimiter
#If data provided as csv file, this indicates how columns are separated.
#If you pass data as an R object, this parameter is not of relevance.
#Default is ","

#type_of_stats
#Type of statistics to be performed.
#freq_binom = frequentist binomial statistics
#freq_glm_glmer = frequentist modelling approach
#bayes_glm_glmer = Bayesian modelling approach based on glmer with arm simulations
#bayes_own_brm = Bayesian modelling approach based on own brms model
#If this is set to 'freq_binom', all parameters relevant for GLMM/GLM modelling
#become irrelevant.
#If set to 'bayes_glm_glmer', simulations will be performed using the sim function
#from arm, using flat priors. If set to bayes_own_brm, you can pass your own model under the parameter brm_model.
#Default is 'freq_glm_glmer'

#brm_model
#Pass own brm model(s). Mind that you still have to CORRECTLY set almost all other 
#modelling related parameters! The only parameters that lose their meaning are:
#dont_make_factor_glmm,nsim_bayes,bayes_seedi
#brm_model always has to be passed as a list and has to have at least one entry.
#If you don't set a sub2_col, or if you did but also set sub1_2_in_one_model to TRUE,
#brm_model is a list of length 1.
#Otherwise (multiple subsets), brm_model is of length 1+length(sub1_states), with
#the first entry standing for the sub1_col and the other for the second subset models,
#in order of appearance in sub1_states. If a sub1_state has not two sub2_states,
#simply pass it as an NA.
#We highly recommend you ran your model by previously setting following dummy coding in R:
# options(contrasts = c("contr.sum", "contr.poly"))
#Otherwise, estimators may not reliably be calculated in case you
#have more than one factor in your model (not just the respective sub_col).
#Default is NA.

#add_ID_column
#Whether to add a column named "ID"
#Choose this if your table doesn't have an ID identifier, but you want
#to use it e.g. as a random effect in the GLMM.
#In case this is set to T and there is already a column "ID", it gets
#replaced by the new column during the GLMM step.
#Mind that the function cannot "understand", which table rows
#come from which individual. Therefore this only really makes sense
#if in your table, one row shows the total responses of one individual.
#Default is F.

#sum_by_col_plot
#Summarize the data by unique identifiers in one (or multiple) column(s)
#Pass the vector of column names here by which you want to summarize the data
#for the plot. If not contradicting with other_effects_sub1 (or other_effects_sub2) 
#or in case you choose to perform a glm or binomial stats, this will also be
#applied to the data before passed to the stats function, since this decreases runtime.
#If you don't want to sum by a column, type "" or NA or c() (or don't set it at all). 
#If you want to sum for example by the columns ID and trial, type c("ID","trial"). This
#will produce unique identifiers for each combination of ID and trial.
#But concerning this example: It does not make sense to sum by ID if you set
#add_ID_coumn to TRUE.
#Default is c()

#response_col
#Proportion columns
#Indicate relevant column(s) with the title of each column.
#These columns will be considered for the calculation of the proportions.
#There is two ways of passing the response column. 
#a) you have a simple 0-1 binomial column ("RAW" data with one row repesenting
#exactly one behavioural interaction). As an example: an individual shows 10 responses to 
#two types of butterfly models, let's say 6 responses to model A and 4 to model B. In this type of table
#These 10 interactions would be appearing in 10 rows, and response_col would always take either the value
#of 1 or 0 (1 e.g. for attraction to model A, 0 for attraction to model B). If you set 
#sum_by_col_plot in a way that the data of an individual gets summed up, then a preference
#score will be calculated for this individual, by sum(response_col)/length(response_col), i.e. 6/10=0.6.
#b) you have multiple columns as reponse_col
#This is a more difficult case to explain. You pass these columns as a list of length 2 to the function.
#In the example before, the columns in the first element of the list would correspond to attraction to model A
#and the columns in the second element would correspond to responses to model B. Not that here, each
#element of the list can be multiple columns, e.g. one column could be "type1" responses to model A and the 
#other column "type2" responses to model A. The function will sum all columns in one element of the list
#together. Thse columns can be either 0-1 columns or also contain values bigger than 1.
#A proportion is calculated as x/(x+y)
#Example: response_col=list(c(x1,x2),c(y1,y2))
#In the above example: the first vector of the list indicates the columns relevant for Model A (this vector can be of any length),
#the second vector indicates those relevant for Model B. Each vector has two columns, which will then be
#added up by the function to create a response vector. Then, the proportion of interactions with model A
#amongst all interactions will be calculated (preference score). Another example would be 
#response_col=list(c(x1),c(y1)), where x1 will directly be the response
#vector for Model A and y1 the response vector for Model B (without adding anything up).

#sub1_col
#First subset column
#Indicate name of column relevant for first subset. This has to be 
#a character vector of size 1, e.g. sub1_col="Species".
#With one exception! If you set this to NA, c() or '', then the function
#will plot all your data together without grouping. All sub1_states will be removed.
#The function will use your first sub1_label as x-axis label and if you don't set sub1_labels, 
#the x axis label will be "ALL_DATA".
#Default is NA.

#sub2_col
#Second subset column
#Same as for first subset column. If you dont want to do a second subset
#put this as NA, c() (or NULL) or "" (or don't set it at all).
#But there is one major difference to sub1_col! If you have multiple sub2_cols,
#you can fill the sub2_col vector with the same number of entries as sub1_states
#has and indicate for each sub1_state a different sub2_col for subsetting.
#If you don't want to subset one of the sub1_states (but you want to subset others,
#indicate this with an NA). Example: You have 3 sub1_states, c(x1,x2,x3). Now you want
#to subset x1 by column y1, x2 by column y2 and you don't want to subset x3, then
#set sub2_col=c(y1,y2,NA).
#If you want to subset all sub1_states by the same column, then just set
#sub2_col to one single column name.
#Default is NA.

#sub1_states
#First subset states
#Indicate in a vector the states that this subset column should contain.
#If sub1_states has more than 2 elements, the p values (if frequentist approach chosen) plotted in the graph
#will just show if all sub1_states are overall different.
#You only get a pairwise comparison, if you select
#two specific states to perform the stats with, setting two_sub1_cols_for_stats.
#The elements of the vector will be sorted from left to right in the graph.
#If you didn't set a sub1_col, this parameter will be ignored. To avoid a notification message,
#you can set sub1_states to NA, c() (or NULL) or '' as well (or not set it at all), to be clear 
#that you are aware that this parameter is of no relevance.
#If you set this parameter to c(), "" or NA, the program will set sub1_states
#to all unique values of sub1_col (if sub1_col was not set, this gets ignored).

#sub2_states
#Second subset states
#If sub2_col was set, please indicate in this list which are the 
#second subset states for each of the first subset classes.
#For example, if you have 3 sub1_states, you may only want to subset the first and second state,
#but not the third state. Additionally, you want to subset the first and second
#state by two different sub2_cols (see explanation above for sub2_col). 
#The list could look like this then:
#list(c("A","B"),c("C","D"),NA). The NA at the third list entry shows no subset for state 3.
#Basically, it is irrelevant in this case what you put at this position though, since your
#setting of sub2_col (as in the example for sub2_col, c(y1,y2,NA)) 
#already tells the program that the third sub1_state should
#not be subset anymore. In another case where you set sub2_col to one column alone
#(which means the same sub2_col counts for all sub1_states), setting an NA at those positions
#that should not receive a second subset will be curcial.
#In general, the first entry in the subset vectors will always appear on the left side in the graph
#(in the above example, 'A' and 'C' will be left in their category, 'B' and 'D'
#will be right).
#If you didn't set a sub2_col, this parameter will be ignored. To avoid a notification message,
#you can set sub2_states to NA, c() (or NULL) or '' as well (or just not set it at all), 
#to be clear that you are aware that this parameter is of no relevance.
#Default is NA.

#stats_for_1sub
#First subset information
#This parameter only matters if a first subset was set.
#Decision whether statistical analyses will be performed on first subset. 
#Also, if plotting is wished and if this is set 
#to TRUE, other information will be provided about the first subset 
#in the plot (estimators, boxplots and significance). 
#Type T for yes and F for no.
#Default is T.

#stats_for_2sub
#Second subset information
#This parameter only matters if a second subset was set.
#Decision whether statistical analyses will be performed on second subset separately
#as well. Each of the classes of subset 1 will be tested separately for their sub2_states.
#Also, if plotting is wished and if this is set 
#to TRUE, other information will be provided about the second subset 
#in the plot (estimators, boxplots and significance). 
#Type T for yes and F for no.
#Default is T.

#sub1_2_in_one_model
#This parameter only matters if a second subset was set, if stats_for_1sub
#and stats_for_2sub are TRUE and if type_of_stats!="freq_binom".
#Set this parameters to TRUE if you want to implement your first and your
#second subset in the same model within an interaction term (but 
#at least sub1_col always has to be a separate fixed effect as well).
#The code will then ontly consider your other_effects_sub1 as model term.
#Ideally, other_effects_sub1 would start off with a '*sub2_col', which then
#get translated into a model which explains courtship by sub1_col,
#sub2_col and their interaction. Mind that the effect of sub2_col alone
#will never make it into your plot or the CI/CrI output. In the plot/CI/CrI output,
#you will see estimators for the effect of sub1_col alone as well as for
#the interaction between sub1_col and sub2_col.
#Default is set to FALSE

#two_sub1_cols_for_stats
#If, on the first subset, you only want to perform stats on two specific 
#sub1_states, define these columns here.
#If you don't want to apply this parameter, type NA, c() (or NULL) or "",
#or simply don't set it at all.
#E.g. if your sub1_col contains several different species, but
#you just want to do stats on two of these species (but still plot
#the rest of the sub1_states), then define those sub1_states here
#which you want to do the stats on. This is important because if you perform non-Bayesian
#stats on more than 2 groups, you won't get any pairwise-comparison p values 
#(which doesn't tell much). Those you will only get 
#if you specify two columns that you want to compare here. The other
#sub1_states will then be omitted for calculation of the p value in
#the stats output.
#If you didn't set a sub1_col or set stats_for_1sub to FALSE, 
#this parameter will be ignored. To avoid a notification message,
#you can set two_sub1_cols_for_stats to NA, c() (or NULL) or '' as well (or not set it at all), 
#to be clear that you are aware that this parameter is of no relevance.
#Default is NA

#dont_make_factor_glmm
#A vector of columns that have to necessarily NOT be a factor in the (potential) glmm.
#Matters only if type_of_stats!=freq_binom and type_of_stats!=bayes_own_brm
#If you don't want to apply this parameter, type NA, c() (or NULL) or "" (or not set it at all).
#The function always first "un-factorizes" all columns (i.e., columns that are e.g. 
#characters will be brought into character format.)
#For the GLMM however, all columns other than those
#listed in response_col and dont_make_factor_glmm will become factors again by default. 
#Provide here a vector of columns that should not become factors
#in the GLMM (don't include the response_cols!).
#If you set type_of_stats to "freq_binom", or other stats_for_1sub and stats_for_2sub
#to FALSE, this parameter will be ignored. To avoid a notification message,
#you can set dont_make_factor_glmm to NA, c() (or NULL) or '' as well (or not set it at all), 
#to be clear that you are aware that this parameter is of no relevance.
#Default is set to c() (all columns other than response_col become factors).

#other_effects_sub1
#Other effects for the GLMM on subset 1
#Matters only if type_of_stats!="freq_binom".
#If you don't want to apply this parameter (no other effects), 
#type NA, c() (or NULL) or "".
#If you want to add other effects, type here the expression you 
#wish to add as ONE character string.
#E.g. write: "+(1|ID)+(1|trial/day)", or "+age+(1|trial)".
#The relevant subset column is already set as fixed effect by default.
#If your other_effects_sub1 is empty or only contains fixed effects, a GLM (not a GLMM!) 
#will be performed on the data! 
#If you set type_of_stats to "freq_binom" or stats_for_1sub to FALSE, 
#this parameter will be ignored. To avoid a notification message,
#you can set other_effects_sub1 to NA, c() (or NULL) or '' as well, to be clear that you are aware that
#this parameter is of no relevance.
#If sub1_2_in_one_model is set to TRUE, you have to include an interaction
#term with sub2_col in this model term. You could start the term off with
#'*sub2_col', for example.
#Default is set to "".

#other_effects_sub2
#Same as for other_effects_sub1, just that this parameter is of relevance only for subset 2.
#If you set type_of_stats to "freq_binom" and/or did not set a sub2_col or set stats_for_2sub TO FALSE
#and/or set sub1_2_in_one_model to TRUE, this parameter will be ignored. 
#To avoid a notification message, you can set other_effects_sub2 to 
#NA, c() (or NULL) or '' as well, to be clear that you are aware that
#this parameter is of no relevance.

#simplify_for_model
#For type_of_stats==freq_glm_glmer or type_of_stats==bayes_glm_glmer
#Indicates whether the table should be simplified for the model, as it
#is simplified for the plot. This will only be done if the grouping
#variable in the plot always has the same state among the other effects
#of the GLM/GLMM.
#Consider the following example: if your model looks like this: ~ species + (1|individual_ID),
#and in each individual_ID is constantly the same species throughout the table
#(which logically seems to make sense). And let's assume you have multiple
#observations per individual in your table. Then, if simplify_for_model is TRUE, the program
#will sum all the rows up by individual ID and run the model on this dataset.
#Usually, you'd expect roughly the same outcome if simplify_for_model is TRUE or FALSE,
#but results may slightly differ. However, simplify_for_model will require less runtime
#for running the GLM/GLMM, which may be of relevance if your dataset is large.
#Default is FALSE

#test_type
#The type of significance test performed if type_of_stats is 'freq_glm_glmer'
#Can be either "LRT" or "typeIII_anova".
#Under LRT, the full model is compared to a model reduced by 
#sub1_col/sub2_col, respectively. 
#Under typeIII_anova, the emmeans package is used to perform a type III Anova.
#If sub1_2_in_one_model is TRUE or if either your sub1_col or your sub2_col
#is involved in an interaction, test_type can only be "typeIII_anova".
#Default is "LRT"

#scale_center
#Decision whether to scale and center all relevant numeric 
#columns involved as fixed and/or random effects in models.
#if your variables are not centered and scaled, this is highly
#recommended since estimation of p values can be wrong if the
#numeric columns are not centered. Also for Bayesian stats, this is desirable.
#If your relevant columns are already scaled and centered,
#put this to FALSE.
#Only of relevance if type_of_stats!="freq_binom" and 
#if either stats_for_1sub & other_effects_sub1 set or if 
#stats_for_2sub==T & other_effects_sub2 set
#Default is FALSE.

#nsim_bayes
#Number of simulations for Bayesian approach
#Only of relevance if type_of_stats=="bayes_glm_glmer"
#Default is 5000

#bayes_seedi
#Setting a seed before Bayesian simulations
#Seed will be set if type_of_stats=="bayes_glm_glmer"
#If you set a seed, the function will after it is done always set a new random seed
#(such that running this function does not cause pseudo-replication in other R scripts of yours).
#If you don't want to set a seed, set this to NA, c() or ""
#Default is set to "random" (time of the day) -> bayes_seedi=as.numeric(Sys.time()). 


#This decides, whether you also want to plot
########

#only_stats
#Whether to plot & output statistical results OR whether to only output the statistical results.
#Type T for only stats and F for Plot+stats.
#Default is F.

#sub1_panel_weights
#Size extents for each sub1_state panel.
#Vector of same length as sub1_states
#Example sub1_states=c("A","B","C"); sub1_panel_weights=c(1,2,1)
#The panel for "B" would be twice as big as the panels for "A" and "C".
#If set to c(), '' or NA, a panels will have same size.
#Unfortunately, the error check for this parameter is not being skipped if only_stats==T,
#because it is too much involved in the code with plotting-irrelevant steps.
#If you don't want to plot, just leave this parameter at default.
#Default c()

########
#From here on, the parameters are only relevant to set if you also want
#graphical output, i.e. if only_stats was set to F


#output_dir
#Output file directory
#Directory for output file.
#If set to NA, c() (or NULL) or "" (or not set it at all), no file will be produced, but plot presented in R.
#Default is set to ""

#plot_dim
#PNG dimensions in number of pixels.
#First value stands for pixels in width, second for pixels in height.
#If set to c(), '' or NA, a default fitting will be performed which
#sets width and height to an aesthetically pleasant setting.
#If you set it to any other value, the values you chose will be used.
#Only important if plotting to png.
#Default c()

#par_mar
#margin dimensions of plot, as in par(mar=c(xbottom,yleft,xtop,yright)).
#If set to c(), '' or NA, a default fitting will be performed which
#sets margins to an aesthetically pleasant setting.
#Default c()

#show_estimators
#Whether to show estimators (+CI) resulting from the statistical test in the plot (TRUE) or whether not (FALSE)
#Has to be a boolean vector of size 2. First entry stands for decision concerning
#first subset, second entry for second subset. No matter which other settings you did,
#this parameter has to be boolean and of length 2. E.g. if you didn't set a sub2_col
#or e.g. set stats_for_1sub to F or e.g. have only one sub1_state and therefore
#no GLM/GLMM results, this vector still has to be of length 2! 
#In such cases as just named (where no estimator would be plotted for one subset type anyway),
#the setting for the respective subset will not matter in the end,
#therefore you can set it randomly to T or F and it won't change anything in the end.
#It still has to be set.
#Default is c(T,T)

#suppress_est1_if2
#Whether sub1_col estimators will be suppressed if sub2_col estimators are present.
#Default is FALSE

#show_p_values_plot
#Whether to show p-values resulting from the statistical test in the plot (TRUE) or whether not (FALSE)
#Has to be a boolean vector of size 2. First entry stands for decision concerning
#first subset, second entry for second subset. No matter which other settings you did,
#this parameter has to be boolean and of length 2. E.g. if you didn't set a sub2_col
#or e.g. set stats_for_1sub to F or e.g. have only one sub1_state and therefore
#no GLM/GLMM results, this vector still has to be of length 2! 
#In such cases as just named (where no p-value would be plotted for one subset type anyway),
#the setting for the respective subset will not matter in the end,
#therefore you can set it randomly to T or F and it won't change anything in the end.
#It still has to be set.
#Also, this setting is irrelevant if type_of_stats="bayes_glm_glmer" or type_of_stats=="bayes_own_brm".
#Default is c(T,T)

#sub1_labels
#First subset axis labels
#Vector of x-axis labels for first subset levels.
#If you didn't set a sub1_col, only the first string in this vector will be used as 
#x axis label. If you didn't set a sub1_col and don't set sub1_labels (i.e. if you
#don't include it in the function call), it will be set to the label "ALL_DATA".
#In all other cases when you don't define it in the function call, this is set to sub1_states

#italics_sub1
#TRUE or FALSE. Use italics for labels of subset 1
#Default=T

#sub2_labels
#Second subset labels
#This is only of relevance if a sub2_col was set. If you don't set sub2_col and have this parameter
#set to something else than NA, c() (or NULL) or '', you will get a notification that your parameter setting was omitted
#(you can also simply not set sub2_states to avoid this).
#Give as list, following same logic as sub2_states.
#If your sub2_col is set and you do a second subset, but you don't want any sub2_labels in your plot, 
#just set sub2_labels to NA, c() (or NULL) or '' (or don't set it at all).
#As default, this is set to sub2_states

#cex_sub1_labels
#cex parameter for sub1_labels
#Default is 1

#cex_sub1_counts
#cex parameter for counts of sub1_states
#Default is 0.85

#cex_sub2_labels
#cex parameter for sub2_labels
#This is only of relevance if a sub2_col was set.
#Default is 0.9

#show_N
#TRUE or FALSE. Whether to show sample size for each group on x-axis
#Default is TRUE

#label_count_col
#Column which determines where to count unique entries from for the sample size calculation
#for each sub1_state and sub2_state.
#Matters only if show_N==T
#If put to NA, '' or c(), program just counts the number of rows for each subset.
#Default is c()

#yaxs_label
#Y axis label
#String of y axis label.
#You can write certain words in italics by adapting this default command:
#substitute(paste("Proportion interactions towards ",italic(' Spec 2')," female"))

#add_y_labels
#Whether to add y labels at 0, 0.25, 0.5, 0.75 and 1
#Default is TRUE

#y_tck
#Tick size on y-axis at positions 0, 0.25, 0.5, 0.75 and 1
#Default is -0.015

#horiz_0.5
#Dashed horizontal line at y=0.5 (boolean)
#Default is TRUE

#panel_dividers
#Vertical lines dividing panels
#Default is TRUE

#dot_size
#Dot size parameter
#A parameter without clear numeric meaning. This determines the size of the dots
#in your graph. The higher this value, the bigger the dots.
#A too high setting may cause the function to crash. Values
#have to be >=0. 
#If set to 0, no dots will appear in the graph.
#If the function crashes because of this parameter, read the recommendations
#in the ERROR message! It will tell you what the maximum setting is that you can use.
#For observations of around 100 per dot, a setting between 0.05 and 0.3
#seems sensible (depending on how many unique dots you have and some other parameter settings).
#Default is 0.35

#dot_lwd
#Line width of dot outline
#This parameter gives the width of the black line surrounding each dot.
#A numeric vector of size 1. Unlike usual plotting functions, it cannot be set to NA.
#Default is 0.5

#legend_dots
#Legend dots
#A parameter that determines the legend dots. It has to be a vector of 
#length of minimum 0 and maximum 5 entries. 
#If you set this to NA, c() (or NULL) or "", no dot legend will be shown.
#The dots will be presented on the right side of the graph, starting 
#(coming from top) with the first value you provide here.
#Sometimes it may look cooler to start with the smallest and sometimes it may be
#prettier to start with the biggest! Just play around. To find reasonable intervals,
#check the notification on the right side of your first try plot which says "Max = ...".
#This shows you the maximum number of observations in the plot.
#Default is c(1,2,3,4,5)

#legend_omi
#Whether to consider outer margins for legend positioning
#Default is FALSE

#legend_info
#Whether info about sample sizes is shown at legend
#Default is TRUE

#dot_colours
#Colour of dots
#You can provide dot colour in three ways:
#a) As just one character string, e.g. "black". All dots will get black then.
#b) As a column name of your table (if you have a column determining the colour of each datapoint)
#c) As a list (see detailed explanation below).
#If you provide the parameter as a list, it should basically look like a combination 
#of sub1_states and sub2_states. Assume e.g. you have 3 sub1_states, and the second state 
#is subsetted again in sub2_states into two categories. E.g. sub1_states=c(x,y,z); 
#sub2_states=list(NA,c(y1,y2),NA).
#Then your dot_colours could for example be: list("yellow",c("orange","darkorange"),"red")
#This will make all x dots yellow and all z dots red. Since y is subsetted, we give separate
#colours to each subset, i.e. orange to the dots on left (y1) and darkorange (y2) to dots on right.
#If you didn't set a sub1_col and dot_colours is set to a list, only the first element of the list will be used as 
#x axis label. 
#Default is "black"

#dot_colour_legend
#Legend dot colour (just a vector of size 1, with 1 colour).
#If you didn't set legend_dots to appear, this parameter will be ignored. To avoid a notification message,
#you can set dot_colour_legend to NA, c() (or NULL) or '' as well, to be clear 
#that you are aware that this parameter is of no relevance.
#Default is "black"

#squeeze_legend
#Factor to multiply legend positions with. Has to be a positive, numeric 
#value. The default gives a pleasantly looking legend when the width
#of the graph is higher than the height. Once the height is higher,
#you may want to consider making this value smaller than 1. If the value 
#is bigger than 1, the legend gets stretched.
#Default is 1.

#transparency
#Transparency of dots
#0=completely transparent, 1=no transparency
#Default is 0.75

#width_mean_sub1
#Width of the mean bar of the sub1_col estimators. One section for a sub1_state in the plot is usually 1 wide (unless you set sub1_panel_weights).
#Matters only if either stats_for_1sub or stats_for_2sub ==T
#Default 0.3

#width_error_sub1
#Width of the error bars of the sub1_col estimators. One section for a sub1_state in the plot is 1 wide.
#Matters only if either stats_for_1sub or stats_for_2sub ==T
#Default is 0.08

#width_mean_sub2
#Width of the mean bar of the sub2_col estimator. 
#Same logic as width_mean_sub1,
#Default 0.15

#width_error_sub2
#Width of the error bars of the sub2_col estimators. 
#Same logic as width_error_sub1,
#Default is 0.04

#sub2_est_perc_cent
#Proportion of maximum distance from center to sub2_col estimators / the center of the boxplots
#A value of 0 will position the estimator/box in the center, a value of 1 on the very 
#ends left and right of the panel.
#Default is 0.8

#thickness_est_CI
#Line thickness of estimator and CI/CrI
#Default is 5

#sub_box_yn
#Define for each sub1_state and sub2_state whether a boxplot will be added or not
#This parameters is either defined by a boolean vector of size 1 (either =T or =F)
#or by a list. If this parameter is simply set to T or F, it is applied to
#all sub categories. If this parameter is set as a list, we indicate for each boxplot
#from "left to right" in the graph whether it will be added or whether not.
#In the simple case of having only a sub1_col, but no sub2_col, this list
#is filled with single T or F entries (telling each of the sub1_states whether
#a boxplot gets added or not). Mind that a sub1_col will always exist,
#even if you don't define it, since with no definition of a sub1_col,
#all data points fall in category #1 of the first subset.
#If we have a sub1_col and a sub2_col set, the logic of this parameter becomes
#more difficult to understand. Imagine we have 3 sub1_states, and the second of
#those is divided into 2 sub2_classes. This means there is potentially 5 boxes
#to be drawn (in case stats_for_1sub and stats_for_2sub are TRUE). 
#Each sub1_state will get a vector entry in the list.
#While for the first and the third sub1_state, we just enter again a T or a F,
#we have to make 3 decisions for the second sub1_state. Therefore we enter a
#vector of size 3. As explained before, these Ts and Fs belong to those boxplots
#which belong to the corresponding data when we go from left to right through
#the graph. In the second sub1_state, there will be one boxplots left and one right
#for the 2 sub2_states and one boxplot in the middle for the whole second sub1_state.
#Therefore, our first element of the vector will correspond to the left sub2_state,
#our second for the whole sub1_state and our third for the second sub2_state.
#Example: list(T,c(T,F,T),T) (this removes the boxplot for the combined second
#sub1_state, but produces one boxplot for each sub2_state at the second position).
#To not make this parameter even more complicated, I decided that the number
#of T and F decisions always has to be typed in INDEPENDENTLY of the settings for
#stats_for_1sub and stats_for_2sub. This means that e.g. if you set stats_for_2sub to F,
#you still have to pretend as if all possible boxes would potentially be drawn. 
#Coming back to the previous example: You'd still have to fill a list of length 3,
#with the second entry comprising of a vector of length 3, even though the two boxes
#for the second subset would not appear. Whether you type T or F at those positions will therefore
#not matter, but you have to either type T or F. In the previous example, you could type:
#list(T,c(RANDOM_either_T_or_F,F,RANDOM_either_T_or_F),T) (RANDOM_either_T_or_F of course
#has to be either T or F). Since stats_for_2sub=F, the only setting that will matter
#is the "F" in the middle, which means there will be no box for the second sub1_state.
#Default is set to T (all boxes appear)

#box_type
#Boxplot type
#Type 1 for boxplot without weights, 2 for boxplot
#with weights by number of observations and 3 for weights by extent of
#binomial confidence interval.
#Matters only if either stats_for_1sub or stats_for_2sub ==T and if boxplot gets added
#Default is 3

#boxwex_set
#boxwex setting for Boxplot (controls width of box)
#Only of relevance if boxplot gets added and if either stats_for_1sub or stats_for_2sub ==T
#Size of second subset boxes cannot be changed.
#Default is 0.7

#box_colour
#Colour setting for Boxplot
#Only of relevance if boxplot gets added and if either stats_for_1sub or stats_for_2sub ==T
#Default is NA

#box_whiskers
#Whether to add whiskers to boxplots (boolean)
#Only of relevance if boxplot gets added and if either stats_for_1sub or stats_for_2sub ==T
#Default is T

#thin_line_sub
#Sets thin line at main category axis. Especially makes sense if second subset chosen.
#Set T for having a line and F for not having it.
#Default is F.

#numb_iterations
#Number of iterations done during local jittering (the more, the tighter)
#Default set to 100.

#border_space
#Space from center given on right and left for points to be jittered.
#Value must be bigger than 0 (if set bigger than 0.5*max(sub1_panel_weights/min(sub1_panel_weights)),
#some dots may not stay in their category). The default (0.35) avoids pretty well that dots jitter
#into boxplots of the second subset, if no specific sub1_panel_weights were set.
#Default is 0.35

#center_space_cat
#Space given in center to dots if they are jittered by second subset category.
#Only of relevance if sub2_col was set.
#Default is 0.01

#reaction_norm_col
#Column name which determines which second subset dots should be connected
#with reaction norm lines.
#E.g. if dots from the second subset come from the same individual, you can
#connect them with those lines. Assume one dot in your plot comes from one
#individual. Assume now e.g. your sub1_col splits the 
#data into different species. If the second subset is now e.g. a column
#with indicators for different behaviours, you will get dots on the left and
#the right side in one sub1_state which may result from the same set of 
#individuals. Those indiviuals you may want to connect to visualize how 
#the two behaviours correlate within individuals.
#Where data for both subset2 categories misses, no line will be drawn.
#If you don't want to draw such reaction norm lines, type NA, '' or c().
#Default is set to c().

#reaction_norm_max_trans
#Maximum transparency value a reaction norm line can take (the smaller the value,
#the more transparent the line is). Values between 0 and 1 are possible.
#If reaction_norm_weights is set to 2 or 3, the line with the highest weight will
#be set to reaction_norm_max_trans and all other transparencies scaled to it (all
#will be smaller or equal to this value).

#reaction_norm_weights
#Sets what determines line transparency of reaction norm.
#Similar to box_type.
#The maximum value for transparency is always reaction_norm_max_trans. This is used as scaling measure.
#Type 1 for lines without weights, 2 for lines
#with weights by number of observations of the two dots 
#and 3 for weights by mean extent of
#binomial confidence interval of the two dots.
#Matters only if reaction_norm_col was set and if a sub2_col was set.
#Default is 3

#seedi
#Setting a seed
#Since the dot jittering is based on randomness, the same plot command
#on the same data set will give differently looking dot assemblies if you don't set a seed.
#If you always want the dot assembly to look the same when you re-run, 
#keep this at a chosen integer number. 
#Seed will be set if only_stats==F.
#If you set a seed, the function will after it is done always set a new random seed
#(such that running this function does not cause pseudo-replication in other R scripts of yours).
#If you don't want to set a seed, set this to NA, c() or ""
#Default is set to "random" (time of the day) -> seedi=as.numeric(Sys.time()). 


#######################################################
#Install necessary libraries:

package_check<-c(!"ENmisc"%in% rownames(installed.packages()),!"binom"%in% rownames(installed.packages()),!"lme4"%in% rownames(installed.packages()),!"truncnorm"%in%rownames(installed.packages()))
if(sum(package_check)>0){
  print(paste0("Installing necessary packages ",paste(c("ENmisc","binom","lme4","truncnorm")[package_check],collapse=","),"."))
  
  #weighted boxplots
  if(!"ENmisc"%in% rownames(installed.packages())){
    install.packages("ENmisc")
    if(!"ENmisc"%in% rownames(installed.packages())){
      stop("Library ENmisc could not be installed. Check your internet connection and/or compatibility between the ENmisc package and your R version.")
    }
  } 
  #binomial stats
  if(!"binom"%in% rownames(installed.packages())){
    install.packages("binom")
    if(!"binom"%in% rownames(installed.packages())){
      stop("Library binom could not be installed. Check your internet connection and/or compatibility between the binom package and your R version.")
    }
  }
  #glm/glmer
  if(!"lme4"%in% rownames(installed.packages())){
    install.packages("lme4")
    if(!"lme4"%in% rownames(installed.packages())){
      stop("Library lme4 could not be installed. Check your internet connection and/or compatibility between the lme4 package and your R version.")
    }
  }
  #confidence intervals for estimators and p values
  if(!"emmeans"%in% rownames(installed.packages())){
    install.packages("emmeans")
    if(!"emmeans"%in% rownames(installed.packages())){
      stop("Library emmeans could not be installed. Check your internet connection and/or compatibility between the emmeans package and your R version.")
    }
  }
  #For Bayesian approach
  if(!"arm"%in% rownames(installed.packages())){
    install.packages("arm")
    if(!"arm"%in% rownames(installed.packages())){
      stop("Library arm could not be installed. Check your internet connection and/or compatibility between the arm package and your R version.")
    }
  }
  #For Bayesian approach based on own model
  if(!"brms"%in% rownames(installed.packages())){
    install.packages("brms")
    if(!"brms"%in% rownames(installed.packages())){
      stop("Library brms could not be installed. Check your internet connection and/or compatibility between the brms package and your R version.")
    }
  }
  #Sampling from truncated normal distribution
  if(!"truncnorm"%in%rownames(installed.packages())){
    install.packages("truncnorm")
    if(!"truncnorm"%in% rownames(installed.packages())){
      stop("Library truncnorm could not be installed. Check your internet connection and/or compatibility between the truncnorm package and your R version.")
    }
  }
  
  print("Installation of external packages completed.")
  
}

##########################################
#FUNCTION

plot_proportion_stats<-function(input_data,
                                verbose=T,
                                column_delimiter=",", ## matters only if data comes from csv
                                type_of_stats="freq_glm_glmer",
                                brm_model=NA, ## matters only if type_of_stats=="bayes_own_brm"
                                add_ID_column=F, 
                                sum_by_col_plot=c(),
                                response_col,
                                sub1_col=NA,
                                sub2_col=NA,
                                sub1_states=c(),  ##matters only if sub1_col was set
                                sub2_states=c(),  ## matters only if sub2_col was set
                                stats_for_1sub=T, ## matters only if sub1_col was set
                                stats_for_2sub=T, ## matters only if sub2_col was set
                                sub1_2_in_one_model=F, ## matters only if a second subset was set, if stats_for_1sub and stats_for_2sub are TRUE and if type_of_stats!="freq_binom"
                                two_sub1_cols_for_stats=NA,  ##matters only if sub1_col was set and stats_for_1sub==F
                                dont_make_factor_glmm=c(), ## matters only if type_of_stats!="freq_binom" and type_of_stats!="bayes_own_brm" and either stats_for_1sub or stats_for_2sub ==T
                                other_effects_sub1="", ## matters only if type_of_stats!="freq_binom" and sub1_col set and stats_for_1sub==F
                                other_effects_sub2="", ## matters only if sub2_col was set and if type_of_stats!="freq_binom" and stats_for_2sub==F
                                simplify_for_model=F, ## matters only if type_of_stats=="freq_glm_glmer" or == "bayes_glm_glmer" and either stats_for_1sub or stats_for_2sub ==T
                                test_type="LRT", ## matters only if type_of_stats=="freq_glm_glmer" and either stats_for_1sub or stats_for_2sub ==T
                                scale_center=F, ## matters only if type_of_stats!="freq_binom" and either stats_for_1sub & other_effects_sub1 set or if stats_for_2sub ==T and other_effects_sub2 set
                                nsim_bayes=5000, ## matters only if type_of_stats=="bayes_glm_glmer"
                                bayes_seedi=as.numeric(Sys.time()), ## matters only if type_of_stats=="bayes_glm_glmer"
                                ####
                                #Decide whether to plot
                                only_stats=F,
                                sub1_panel_weights=c(), ##matters only if we have more than one sub1_state
                                ####ALL FOLLOWING ONES ONLY MATTER IF only_stats==F
                                output_dir="",
                                plot_dim=c(), ##matters only if output_dir was set
                                par_mar=c(),
                                show_estimators=c(T,T), ##matters only if either stats_for_1sub or stats_for_1sub ==T
                                show_p_values_plot=c(T,T), ##matters only if type_of_stats!="bayes_glm_glmer" and type_of_stats!="bayes_own_brm" and either stats_for_1sub or stats_for_2sub ==T
                                suppress_est1_if2=F, ##matters only if type_of_stats!="bayes_glm_glmer" and type_of_stats!="bayes_own_brm" and stats_for_1sub and stats_for_2sub ==T and sub1_col and sub2_col are set
                                sub1_labels,
                                italics_sub1=T,
                                sub2_labels, ## only matters if sub2_col was set
                                cex_sub1_labels=1,
                                cex_sub1_counts=0.85,
                                cex_sub2_labels=0.9, ## only matters if sub2_col was set
                                show_N=T,
                                label_count_col=c(), ## only matters if show_N==T
                                yaxs_label=substitute(paste("Proportion interactions towards ",italic(' Spec 2')," female")),
                                add_y_labels=T,
                                y_tck=-0.015,
                                horiz_0.5=T,
                                panel_dividers=T,
                                dot_size=0.35,
                                dot_lwd=0.5,
                                legend_dots=c(1,2,3,4,5),
                                legend_omi=F, ## only matters if legend should be plotted
                                legend_info=T, ## only matters if legend should be plotted
                                dot_colours="black", ## only matters if legend should be plotted
                                dot_colour_legend="black", ## only matters if legend should be plotted
                                squeeze_legend=1, ## only matters if legend should be plotted
                                transparency=0.75,
                                width_mean_sub1=0.35, ##matters only if stats_for_1sub ==T
                                width_error_sub1=0.08, ##matters only if stats_for_1sub ==T
                                width_mean_sub2=0.133, ##matters only if stats_for_2sub ==T
                                width_error_sub2=0.04, ##matters only if stats_for_2sub ==T
                                sub2_est_perc_cent=0.8, ##matters only if stats_for_2sub ==T
                                thickness_est_CI=7, ##matters only if either stats_for_1sub or stats_for_2sub ==T
                                sub_box_yn=T, ##matters only if either stats_for_1sub or stats_for_2sub ==T
                                box_type=3, ##matters only if either stats_for_1sub or stats_for_2sub ==T and if sub_box_yn at least contains one T
                                boxwex_set=0.7, ##matters only if either stats_for_1sub or stats_for_2sub ==T and if sub_box_yn at least contains one T
                                box_colour=NA, ##matters only if either stats_for_1sub or stats_for_2sub ==T and if sub_box_yn at least contains one T
                                box_whiskers=T, ##matters only if either stats_for_1sub or stats_for_2sub ==T and if sub_box_yn at least contains one T
                                thin_line_sub=F,
                                numb_iterations=100,
                                border_space=0.35,
                                center_space_cat=0.01, ##matters only if sub2_col was set
                                reaction_norm_col=c(), ##matters only if a sub2_col was set
                                reaction_norm_max_trans=0.5, ##matters only if a sub2_col was set and a reaction_norm_col was set
                                reaction_norm_weights=3, ##matters only if a sub2_col was set and a reaction_norm_col was set
                                seedi=as.numeric(Sys.time())){
  
  
  #######################################################
  #Get data and do some error checks
  
  if(exists("input_data")){
    #If it is already an R object.
    tfinal<-input_data
    #Test if it is really a data frame
    if(!is.data.frame(tfinal)){
      stop("input_data has to be of data.frame type.")
    }
    #transform everything to character that is factor
    for(i in 1:length(tfinal[1,])){
      if(is.factor(tfinal[,i])){
        tfinal[,i]<-levels(tfinal[,i])[tfinal[,i]]
      }
    }
  } else{
    #Read in results table, if passed as file path
    if(file_test("-f", input_data)){
      if(!is.vector(column_delimiter)|is.list(column_delimiter)|!is.character(column_delimiter)|length(column_delimiter)!=1){
        stop("column_delimiter for csv file has to be one single character string.")
      }
      tfinal<-read.csv(input_data,header=T,stringsAsFactors=F,sep=column_delimiter)
    } else{
      stop("input_data is neither an R object nor an existing file path.")
    }
  }
  
  # if(dim(tfinal)[2]<=1){
  #   stop("Less than two columns (minimum dimension of the table) found for input table. Maybe column_delimiter is set wrong, if you read it in from csv.")
  # }
  if(length(names(tfinal))==0){
    stop("Column names are missing.")
  }
  if(nrow(tfinal)==0){
    stop("Table you provided has 0 rows.")
  }
  
  if(verbose){
    print("Data read in.")
    print("--------")
  }
  
  #######################################################
  #ERROR checks (hundreds of lines)
  
  #Before we start, set these two variables to F
  seed_set_plot<-F
  seed_set<-F
  
  #verbose
  
  if(length(verbose)!=1){
    stop("verbose has to be of length 1 and boolean (either TRUE or FALSE).")
  }
  if(!is.vector(verbose)|is.list(verbose)){
    stop("verbose has to be a boolean vector of length 1 (either TRUE or FALSE).")
  }
  if(!verbose%in%0:1){
    stop("verbose has to be boolean (T or F).")
  }
  
  if(verbose){
    print("Conduct most relevant error checks:")
  }
  
  #type_of_stats
  
  if(length(type_of_stats)!=1){
    stop("type_of_stats has to be of length one and a character (either 'freq_binom', 'freq_glm_glmer', 'bayes_glm_glmer' or 'bayes_own_brm').")
  }
  if(!is.vector(type_of_stats)|is.list(type_of_stats)){
    stop("type_of_stats has to be of length one and a character (either 'freq_binom', 'freq_glm_glmer', 'bayes_glm_glmer' or 'bayes_own_brm').")
  }
  if(!is.character(type_of_stats)){
    stop("type_of_stats has to be of length one and a character (either 'freq_binom', 'freq_glm_glmer', 'bayes_glm_glmer' or 'bayes_own_brm').")
  }
  if(!type_of_stats%in%c("freq_binom","freq_glm_glmer","bayes_glm_glmer","bayes_own_brm")){
    stop("type_of_stats has to be of length one and of type character (either 'freq_binom', 'freq_glm_glmer', 'bayes_glm_glmer' or 'bayes_own_brm').")
  }
  
  #brm_model
  perform_GLM_GLMM<-T
  if(type_of_stats=="bayes_own_brm"){
    if(!is.list(brm_model)&is.vector(brm_model)){
      stop("Your brm_model parameter has to be passed as a list.")
    }
    if(!is.list(brm_model)){
      stop("Your brm_model parameter has to be passed as a list.")
    }
    if(length(brm_model)==0){
      stop("brm_model has to be at least of length 1.")
    }
    if(length(brm_model)>1){
      if(sum(sapply(1:length(brm_model),function(x) sum(is.na(brm_model[[x]]))!=length(is.na(brm_model[[x]]))&class(brm_model[[x]])!="brmsfit"))>0){
        stop("Not all brm_model entries you passed are of class 'brmsfit'. If at a certain position, you don't want to pass a model, then add an NA.")
      }
    } else{
      if(!is.na(brm_model)&class(brm_model[[1]])!="brmsfit"){
        stop("You set bayes_own_brm as type_of_stats. Your brm model is not of class 'brmsfit'!")
      }
    }
    perform_GLM_GLMM<-F
  } else{
    warn_brms<-F
    if(length(brm_model)==1){
      if(!sum(is.na(brm_model))==1){
        if(is.character(brm_model)){
          if(!gsub(" ","",brm_model)==""){
            warn_brms<-T
          } 
        } else{
          warn_brms<-T
        }
      }
    } else{
      warn_brms<-T
    }
    if(warn_brms){
      print("Your setting for brm_model will be ignored, since you didn't set 'bayes_own_brm' as type_of_stats.")
    }
    brm_model<-eval(parse(text=paste0("list(",paste0(rep(NA,length(sub1_states)*2),collapse=","),")")))
  }
  
  #add_ID_column
  
  if(length(add_ID_column)!=1){
    stop("add_ID_column has to be of length one and boolean (either TRUE or FALSE).")
  }
  if(!is.vector(add_ID_column)|is.list(add_ID_column)){
    stop("add_ID_column has to be a boolean vector of length 1 (either TRUE or FALSE).")
  }
  if(!add_ID_column%in%0:1){
    stop("add_ID_column has to be boolean (T or F).")
  }
  
  #If ID column is to be added, do it here.
  if(add_ID_column){
    #Delete old ID column
    if(sum(names(tfinal)%in%c("id","ID"))>0){
      warning("Because you selected add_ID_column, your column(s) ",paste(names(tfinal)[names(tfinal)%in%c("id","ID")],collapse=",")," is/are getting removed (since otherwise column names would overlap).")
      tfinal<-tfinal[,!names(tfinal)%in%c("id","ID")]
    }
    tfinal<-cbind(tfinal,ID=1:nrow(tfinal))
  }
  
  #sum_by_col_plot
  
  if(length(sum_by_col_plot)>0){
    if(!is.vector(sum_by_col_plot)|is.list(sum_by_col_plot)){
      stop("sum_by_col_plot has to be a vector.")
    }
    check_sum_cols<-F
    if(length(sum_by_col_plot)==1){
      if(!sum(is.na(sum_by_col_plot))==1){
        if(is.character(sum_by_col_plot)){
          if(!gsub(" ","",sum_by_col_plot)==""){
            check_sum_cols<-T
          } else{
            sum_by_col_plot<-c()
          }
        } else{
          check_sum_cols<-T
        }
      } else{
        sum_by_col_plot<-c()
      }
    } else{
      check_sum_cols<-T
    }
    if(check_sum_cols){
      if(sum(is.na(sum_by_col_plot))!=0){
        stop("sum_by_col_plot has to be a vector of character strings indicating the columns to be summed by, or otherwise set to c(), NA or ''.")
      }
      if(!is.character(sum_by_col_plot)){
        stop("sum_by_col_plot has to be a vector of character strings indicating the columns to be summed by, or otherwise set to c(), NA or ''.")
      }
      if("ID"%in%sum_by_col_plot&add_ID_column){
        warning("One of the columns in sum_by_col_plot is the column 'ID', which you added to the data set by setting add_ID_column to TRUE. It is true that the ID column now actually exists in the data set, but it makes NO sense to sum the table by such added ID column (since adding happens without any knowledge on which data point belongs to which ID)!")
      }
      if(sum(sum_by_col_plot%in%names(tfinal))!=length(sum_by_col_plot)){
        stop("Columns named in sum_by_col_plot cannot all be found in the table's column names.")
      }
    }
  } else{
    sum_by_col_plot<-c()
  }
  
  #response_col
  
  if(!length(response_col)%in%1:2){
    stop("response_col has to contain those column name(s) relevant to calculate the proportions and it has to be either a list of length 2 or a vector of length 1.")
  }
  if(is.list(response_col)&length(response_col)==1){
    response_col<-response_col[[1]]
  }
  if(!is.list(response_col)&!is.vector(response_col)){
    stop("response_col has to be a list of length 2 or a vector of length 1.")
  }
  if(!is.list(response_col)){
    if(length(response_col)==1){
      if(!is.character(response_col)){
        stop("response_col has to be of type character and contain those column name(s) relevant to calculate the proportions.")
      }
      if(is.na(response_col)){
        stop("response_col has to contain those column name(s) relevant to calculate the proportions and it has to be either a list of length 2 or a vector of length 1.")
      } else{
        if(gsub(" ","",response_col)==""){
          stop("response_col has to contain those column name(s) relevant to calculate the proportions and it has to be either a list of length 2 or a vector of length 1.")
        } else{
          if(!response_col%in%names(tfinal)){
            stop("The column named in response_col cannot be found in the table's column names.")
          }
          if(!is.numeric(tfinal[,response_col])){
            stop("The column named in response_col is not numeric. Check for character elements!")
          }
          if(sum(tfinal[,response_col][!is.na(tfinal[,response_col])]%in%0:1)!=sum(!is.na(tfinal[,response_col]))){
            stop("The column named in response_col has values different to 0 and 1. If you pass the response as one column, it has to contain only 0s and 1s.")
          }
          tfinal$resp1_temp_col<-sapply(1:length(tfinal[,response_col]),function(x) ifelse(tfinal[x,response_col]==1,1,0))
          tfinal$resp2_temp_col<-sapply(1:length(tfinal[,response_col]),function(x) ifelse(tfinal[x,response_col]==0,1,0))
          response_col<-list("resp1_temp_col","resp2_temp_col")
        }
      }
    } else{
      stop("Since you defined response_col as a vector, it has to be of length 1 and has to contain the column name relevant to calculate the proportions.")
    }
  }
  
  if(length(response_col[[1]])==0|length(response_col[[2]])==0){
    stop("You defined response_col as a list: each vector of the response_col list has to contain at least one column.")
  }
  if(!is.character(unlist(response_col))){
    stop("You defined response_col as a list: response_col has to be a list of character strings.")
  }
  if(sum(unlist(response_col)%in%names(tfinal))!=length(unlist(response_col))){
    stop("Not all columns named in response_col can be found in the table's column names.")
  }
  num_check<-sapply(1:length(unlist(response_col)),function(x) is.numeric(tfinal[,unlist(response_col)[x]]))
  if(sum(num_check)!=length(unlist(response_col))){
    stop(paste0("The response_cols ",paste(unlist(response_col)[!num_check],collapse=",")," are not numeric. Check for character elements!"))
  }
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  int_check<-sapply(1:length(unlist(response_col)),function(x) sum(is.wholenumber(tfinal[,unlist(response_col)[x]]))==length(tfinal[,unlist(response_col)[x]]))
  if(sum(int_check)!=length(unlist(response_col))){
    warning(paste0("The response_cols ",paste(unlist(response_col)[!int_check],collapse=",")," contain non-integer values."))
  }
  
  #sub1_col
  
  create_dummy_col<-F
  if(length(sub1_col)==0){
    create_dummy_col<-T
  } else{
    if(is.list(sub1_col)&length(sub1_col)==1){
      sub1_col<-sub1_col[[1]]
    }
    if(is.vector(sub1_col)&!is.list(sub1_col)){
      if(length(sub1_col)==1){
        if(is.na(sub1_col)){
          create_dummy_col<-T
        } else{
          if(gsub(" ","",sub1_col)==""){
            create_dummy_col<-T
          }
        }
      }
    }
  }
  if(create_dummy_col){
    tfinal$THIS_IS_A_DUMMY_COL<-rep("ALL_DATA",nrow(tfinal))
    sub1_col<-"THIS_IS_A_DUMMY_COL"
    sub1_states_old<-sub1_states
    sub1_states<-"ALL_DATA"
  }
  if(length(sub1_col)!=1){
    stop("sub1_col has to be a character vector of size one or set to NA, c() (or NULL) or '' (if no subsetting is wished).")
  }
  if(!is.vector(sub1_col)|is.list(sub1_col)){
    stop("sub1_col has to be a vector, if not set to or set to NA, c() (or NULL) or '' (if no subsetting is wished).")
  }
  if(sum(is.na(sub1_col))!=0){
    stop("sub1_col has to be a character string, or set to NA, c() (or NULL) or '' (if no subsetting is wished).")
  }
  if(!is.character(sub1_col)){
    stop("sub1_col has to be a character string, or set to NA, c() (or NULL) or '' (if no subsetting is wished).")
  }
  if(sum(sub1_col%in%names(tfinal))!=1){
    stop("Column named in sub1_col cannot be found in the table's column names.")
  }
  if(length(sum_by_col_plot)>0){
    if(sub1_col%in%sum_by_col_plot&length(sum_by_col_plot)==1){
      warning("sum_by_col_plot contains sub1_col alone, which doesn't really make sense.")
    }
  }
  
  #sub2_col
  
  check_sub2_states<-F
  if(length(sub2_col)!=0){
    if(!is.vector(sub2_col)|is.list(sub2_col)){
      stop("If you don't want to have sub2_col, set to c(), NA or ''. Otherwise set to a character string vector of length 1 or (if you want to apply a differen sub2_col to your different sub1_states), set it to a vector equal in length as the sub1_states vector.")
    }
    if(!(sum(is.na(sub2_col))==length(sub2_col))){
      # if(length(sub2_col)!=1){
      #   stop("If you don't want to have sub2_col, set to c(), NA or ''. Otherwise set to a character string vector of length 1 or (if you want to apply a differen sub2_col to your different sub1_states), set it to a vector equal in length as the sub1_states vector.")
      # }
      if(!is.character(sub2_col[!is.na(sub2_col)])){
        stop("If you don't want to have sub2_col, set to c(), NA or ''. Otherwise set to a character string vector of length 1 or (if you want to apply a differen sub2_col to your different sub1_states), set it to a vector equal in length as the sub1_states vector.")
      }
      #If all are equal, just make it one vector
      if(sum(is.na(sub2_col))==0&length(unique(sub2_col))==1){
        sub2_col<-sub2_col[1]
      }
      if(sum(gsub(" ","",sub2_col[!is.na(sub2_col)])=="")==0){
        if(sum(sub2_col[!is.na(sub2_col)]%in%names(tfinal))!=sum(!is.na(sub2_col))){
          stop("Not all columns named in sub2_col can be found in the table's column names.")
        } 
        if(length(sum_by_col_plot)>0){
          if(sum(sub2_col%in%sum_by_col_plot)>0&length(sum_by_col_plot)==1){
            warning("sum_by_col_plot contains one sub2_col alone, and it doesn't make much sense to sum the table by a subset column alone.")
          }
        }
        check_sub2_states<-T
      } else{
        if(sum(gsub(" ","",sub2_col[!is.na(sub2_col)])=="")!=length(sub2_col)){
          stop("At least one of the positions of your sub2_col is an empty character vector, while others are not. You can only set sub2_col to an empty character vector if this is the only entry to the vector (which then tells the program to not do a second subsetting). Otherwise, use column names and NAs to indicate which sub1_states should or should not be subset.")
        }
        sub2_col<-c()
      }
    } else{
      sub2_col<-c()
    }
  } else{
    sub2_col<-c()
  }
  
  #sub1_states
  
  #For later:
  sub1_states_automatic<-F
  #If sub1_col was set to ""
  if(create_dummy_col){
    give_sub1_error<-T
    if(length(sub1_states_old)==0){
      give_sub1_error<-F
    } else{
      if(is.vector(sub1_states_old)&!is.list(sub1_states_old)){
        if(sum(is.na(sub1_states_old))>0){
          give_sub1_error<-F
        } else{
          if(is.character(sub1_states_old)){
            if(sum(gsub(" ","",sub1_states_old)=="")>0){
              give_sub1_error<-F
            }
          }
        }
      }
      if(give_sub1_error){
        if(verbose){
          print("You did set sub1_states, but you did not set sub1_col. Your setting for sub1_states will be omitted.")
        }
      }
    }
  } else{
    #Unify to c() if not set
    if(length(sub1_states)==0){
      sub1_states<-c()
    } else{
      if(length(sub1_states)==1){
        if(is.vector(sub1_states)&!is.list(sub1_states)){
          if(sum(is.na(sub1_states))>0){
            sub1_states<-c()
          } else{
            if(is.character(sub1_states)){
              if(sum(gsub(" ","",sub1_states)=="")>0){
                sub1_states<-c()
              }
            }
          }
        }
      }
    }
    #Do the next two checks only if sub1_col was not set to c(), "" or NA
    if(length(sub1_states)==0){
      sub1_states<-sort(unique(tfinal[,sub1_col]))
      sub1_states_automatic<-T
      if(verbose){
        print("Since you didn't set sub1_states, it was set to the unique states in sub1_col, in alphabetical order.")
      }
    }
    if(!is.vector(sub1_states)|is.list(sub1_states)){
      stop("sub1_states has to be a vector.")
    }
    if(sum(is.na(sub1_states))>0){
      stop("No NAs is sub1_states!")
    }
  }
  if(length(sub2_col)>1){
    if(length(sub2_col)!=length(sub1_states)){
      stop("The length of sub2_col has to be either 0 (no second subset), 1 (all sub1_states will be subset by the same column) or equal to the number of sub1_states.")
    }
  }
  
  #sub2_states
  
  #For later:
  sub2_states_automatic<-F
  if(!check_sub2_states){
    give_sub2_error<-T
    if(length(sub2_states)==0){
      give_sub2_error<-F
      sub2_states<-c()
    } else{
      if(is.vector(sub2_states)&!is.list(sub2_states)){
        if(length(sub2_states)==1){
          if(is.na(sub2_states)){
            give_sub2_error<-F
            sub2_states<-c()
          } else{
            if(is.character(sub2_states)){
              if(gsub(" ","",sub2_states)==""){
                give_sub2_error<-F
                sub2_states<-c()
              }
            }
          }
        } else{
          if(sum(is.na(sub2_states))==length(sub2_states)){
            give_sub2_error<-F
            sub2_states<-c()
          }
        }
      } else{
        if(is.list(sub2_states)){
          if(sum(is.na(sub2_states))==length(unlist(sub2_states))){
            give_sub2_error<-F
            sub2_states<-c()
          }
        }
      }
      if(give_sub2_error){
        if(verbose){
          print("You did set sub2_states, but you did not set sub2_col. Your setting for sub2_states will be omitted.")
        }
      }
    }
  } else{
    if(length(sub2_states)==0){
      sub2_states<-c()
    } else{
      if(is.vector(sub2_states)&!is.list(sub2_states)){
        if(length(sub2_states)==1){
          if(is.na(sub2_states)){
            sub2_states<-c()
          } else{
            if(is.character(sub2_states)){
              if(gsub(" ","",sub2_states)==""){
                sub2_states<-c()
              }
            }
          }
        } else{
          if(sum(is.na(sub2_states))==length(sub2_states)){
            sub2_states<-c()
          }
        }
      } else{
        if(is.list(sub2_states)){
          if(sum(is.na(unlist(sub2_states)))==length(unlist(sub2_states))){
            sub2_states<-c()
          }
        }
      }
    }
    
    if(length(sub2_states)==0){
      sub2_states<-list()
      for(s2_filler in 1:length(sub1_states)){
        if(length(sub2_col)==1){
          curr_col<-sub2_col
        } else{
          curr_col<-sub2_col[s2_filler]
        }
        if(!is.na(curr_col)){
          if(length(unique(tfinal[tfinal[,sub1_col]==sub1_states[s2_filler],curr_col]))>2){
            stop(paste0("Automatic filling of sub2_states resulted in more than two unique states for sub1_state ",sub1_states[s2_filler],". sub2_states cannot have more than 2 entries for any sub1_state."))
          }
          sub2_states[[length(sub2_states)+1]]<-sort(unique(tfinal[tfinal[,sub1_col]==sub1_states[s2_filler],curr_col]))
        } else{
          sub2_states[[length(sub2_states)+1]]<-NA
        }
      }
      sub2_states_automatic<-T
      if(verbose){
        print("Since you didn't set sub2_states, it was filled automatically by the unique states of sub2_col for each sub1_state, in alphabetical order.")
      }
    }
    
    if(!is.list(sub2_states)){
      stop("sub2_states has to be a list.")
    }
    if(length(sub2_states)!=length(sub1_states)){
      stop("Length of sub2_states has to be equal to length of sub1_states.")
    }
    sub_lengths<-sapply(1:length(sub2_states),function(x) length(sub2_states[[x]]))
    if(!sub2_states_automatic){
      if(sum(sub_lengths>1)==0){
        stop("You defined a sub2_col but didn't provide any double sub2_states for any sub1_state to subset this column.")
      }
    }
    if(sum(sub_lengths==0)>0|sum(sub_lengths>2)>0){
      stop("Every element of the list of sub2_states has to be a vector of one or two elements. If you don't want a subset for the respective sub1_state, put NA at that position.")
    }
    if(sum(sub_lengths==1)>0){
      id_sub_l1<-(1:length(sub_lengths))[sub_lengths==1]
      if(sum(sapply(1:sum(sub_lengths==1),function(x) is.na(sub2_states[[id_sub_l1[x]]])))!=sum(sub_lengths==1)){
        if(sub2_states_automatic){
          if(verbose){
            print("sub2_states was set automatically, but for at least one of the sub1_states, not two sub2_states could be found. All positions in sub2_states with only one element (i.e. those positions that are indicating no second subset) will be ignored later on.")
          }
        } else{
          warning("All positions in sub2_states with only one element (i.e. those positions that are indicating no second subset) will be ignored later on. You should indicate those positions with an NA for clarity (which you didn't in some cases).")
        }
      }
    }
    for(na_checker in 1:length(sub_lengths)){
      if(sub_lengths[na_checker]>1&sum(is.na(sub2_states[[na_checker]]))>0){
        stop("Don't put NAs is the element number ",na_checker," of the sub2_states list! If you don't want a second subset at this particular position of the list, just add one NA and not a vector containing NAs.")
      }
    }
  }
  
  #stats_for_1sub
  
  if(!"THIS_IS_A_DUMMY_COL"%in%names(tfinal)){
    if(length(stats_for_1sub)!=1){
      stop("stats_for_1sub has to be of length 1 and boolean (either TRUE or FALSE).")
    }
    if(!is.vector(stats_for_1sub)|is.list(stats_for_1sub)){
      stop("stats_for_1sub has to be a boolean vector of length 1 (either TRUE or FALSE).")
    }
    if(!stats_for_1sub%in%0:1){
      stop("stats_for_1sub has to be boolean (T or F).")
    }
  } else{
    #Don't remember why I did this here...
    #stats_for_1sub<-F
  }
  
  #stats_for_2sub
  
  if(check_sub2_states){
    if(length(stats_for_2sub)!=1){
      stop("stats_for_2sub has to be of length one and boolean (either TRUE or FALSE).")
    }
    if(!is.vector(stats_for_2sub)|is.list(stats_for_2sub)){
      stop("stats_for_2sub has to be a boolean vector of length 1 (either TRUE or FALSE).")
    }
    if(!stats_for_2sub%in%0:1){
      stop("stats_for_2sub has to be boolean (T or F).")
    }
  } else{
    stats_for_2sub<-F
  }
  
  #sub1_2_in_one_model
  
  if(length(sub1_2_in_one_model)!=1){
    stop("sub1_2_in_one_model has to be of length one and boolean (either TRUE or FALSE).")
  }
  if(!is.vector(sub1_2_in_one_model)|is.list(sub1_2_in_one_model)){
    stop("sub1_2_in_one_model has to be a boolean vector of length 1 (either TRUE or FALSE).")
  }
  if(!sub1_2_in_one_model%in%0:1){
    stop("sub1_2_in_one_model has to be boolean (T or F).")
  }
  if(sub1_2_in_one_model&!(check_sub2_states&stats_for_1sub&stats_for_2sub&type_of_stats!="freq_binom")){
    warning("Some of your settings contradict sub1_2_in_one_model being TRUE, so it is set automatically to FALSE.")
    sub1_2_in_one_model<-FALSE
  } else{
    if(sub1_2_in_one_model){
      if(length(sub2_states)>0){
        if(length(sub2_col)>1){
          stop("sub1_2_in_one_model==T is unfortunately not compatible with multiple elements of sub2_col. To make it work, you have to select one sub2_col and it will count for all the sub1_states.")
        }
        if(length(unique(sapply(1:length(sub2_states),function(x) length(sub2_states[[x]]))))!=1){
          stop("sub2_states has to have the exact same values for each sub1_state if you set sub1_2_in_one_model to TRUE. Otherwise it doesn't really make sense to run the model under sub1_2_in_one_model=TRUE.")
        }
        if(sum(sapply(1:length(sub2_states),function(x) length(sub2_states[[x]]))==2)!=length(sub2_states)){
          stop("sub2_states has to have exactly two values for each sub1_state if you set sub1_2_in_one_model to TRUE.")
        }
        if(sum(is.na(unlist(sub2_states)))>0){
          stop("sub2_states cannot contain NA values if you set sub1_2_in_one_model to TRUE. Under the setting of sub1_2_in_one_model, one single model will be run for the interaction between sub1_col and sub2_col. So each sub1_state has to have a valid sub2_state, for this type of analysis to make sense.")
        }
        uni_states<-matrix(ncol=2,nrow=0)
        for(uni_checko in 1:length(sub2_states)){
          uni_states<-rbind(uni_states,unique(sub2_states[[uni_checko]]))
        }
        if(length(unique(uni_states[,1]))!=1|length(unique(uni_states[,2]))!=1){
          stop("sub2_states has to have the exact same values for each sub1_state if you set sub1_2_in_one_model to TRUE. Otherwise it doesn't really make sense to run the model under sub1_2_in_one_model=TRUE.")
        }
      }
    }
  }
  
  if(!perform_GLM_GLMM){
    if(check_sub2_states&!sub1_2_in_one_model){
      if(length(brm_model)<(1+length(sub1_states))){
        stop("Since you run separate models on sub1_col and sub2_col, brm_model has to be of same length as 1 + length of sub1_states. Even if a sub1_state does not have sub2_states, this has to be put to at least an NA.")
      }
    } else{
      if(length(brm_model)!=1){
        stop("You set one subset, but brm_model has not one entry to its list.")
      }
    }
  } 
  
  #two_sub1_cols_for_stats
  
  skip_error<-F
  
  if(create_dummy_col|stats_for_1sub==F){
    give_two_col_error<-T
    if(length(two_sub1_cols_for_stats)==0){
      give_two_col_error<-F
    } else{
      if(is.vector(two_sub1_cols_for_stats)&!is.list(two_sub1_cols_for_stats)){
        if(sum(is.na(two_sub1_cols_for_stats))==length(two_sub1_cols_for_stats)){
          give_two_col_error<-F
        } else{
          if(is.character(two_sub1_cols_for_stats)){
            if(sum(gsub(" ","",two_sub1_cols_for_stats)=="")==length(two_sub1_cols_for_stats)){
              give_two_col_error<-F
            }
          }
        }
      }
      if(give_two_col_error){
        if(verbose){
          if(create_dummy_col){
            print("You did set two_sub1_cols_for_stats, but you did not set sub1_col. Your setting for two_sub1_cols_for_stats will be omitted.")
          } else{
            print("You did set two_sub1_cols_for_stats, but you set stats_for_1sub to FALSE. Your setting for two_sub1_cols_for_stats will be omitted.")
          }
        }
      }
    }
  } else{
    
    skip_error<-F
    if(length(two_sub1_cols_for_stats)==0){
      skip_error<-T
    } else{
      if(!is.vector(two_sub1_cols_for_stats)|is.list(two_sub1_cols_for_stats)){
        stop("two_sub1_cols_for_stats has to be of type vector, if not set to NA, c() (or NULL) or ''.")
      }
      if(length(two_sub1_cols_for_stats)==1){
        if(!is.na(two_sub1_cols_for_stats)){
          if(is.character(two_sub1_cols_for_stats)){
            if(gsub(" ","",two_sub1_cols_for_stats)!=""){
              stop("If you don't wish to set two_sub1_cols_for_stats, you have to either set it to NA, c() (or NULL) or '', or not set it at all; if you want to set it, it has to be set to two character strings that can be found in the sub1_states.")
            }
          } else{
            stop("If you don't wish to set two_sub1_cols_for_stats, you have to either set it to NA, c() (or NULL) or '', or not set it at all; if you want to set it, it has to be set to two character strings that can be found in the sub1_states.")
          }
        }
        skip_error<-T
      } else{
        if(sum(is.na(two_sub1_cols_for_stats))>0){
          stop("If you don't wish to set two_sub1_cols_for_stats, you have to either set it to NA, c() (or NULL) or '', or not set it at all; if you want to set it, it has to be set to two character strings that can be found in the sub1_states.")
        } 
        if(!is.character(two_sub1_cols_for_stats)){
          stop("Since it seems that you want to set two_sub1_cols_for_stats, you have to set it as a vector of 2 character strings.")
        }
      }
    }
    if(!skip_error){
      if(length(two_sub1_cols_for_stats)!=2|sum(two_sub1_cols_for_stats%in%sub1_states)!=2){
        stop("two_sub1_cols_for_stats has to be a vector of length 2 and all its elements have to appear in sub1_states.")
      }
    }
    #if user didn't define two_sub1_cols_for_stats, overwrite with sub1_states
    if(skip_error){    
      two_sub1_cols_for_stats<-sub1_states
    }
  }
  
  #dont_make_factor_glmm
  
  if(!type_of_stats%in%c("freq_binom","bayes_own_brm")&!(stats_for_1sub==F&stats_for_2sub==F)){
    if(length(dont_make_factor_glmm)>0){
      if(!is.vector(dont_make_factor_glmm)|is.list(dont_make_factor_glmm)){
        stop("If you don't want to set any columns in dont_make_factor_glmm, either use c(), NA or '' (or don't set the parameter at all). If you want to name columns here, they have to be named in a character vector.")
      }
      if(sum(is.na(dont_make_factor_glmm))==0){
        if(!is.character(dont_make_factor_glmm)){
          stop("If you don't want to set any columns in dont_make_factor_glmm, either use c(), NA or '' (or don't set the parameter at all). If you want to name columns here, they have to be named in a character vector.")
        }
        if(sum(gsub(" ","",dont_make_factor_glmm)=="")==0){
          if(sum(dont_make_factor_glmm%in%names(tfinal))!=length(dont_make_factor_glmm)){
            stop("Not all columns named in dont_make_factor_glmm can be found in the table's column names.")
          }
          if(sum(dont_make_factor_glmm%in%unlist(response_col))>0){
            stop("dont_make_factor_glmm cannot contain any element of response_col (since these have to stay numeric).")
          }
        } else{
          dont_make_factor_glmm<-c()
        }
      } else{
        if(length(dont_make_factor_glmm)>1){
          stop("If you don't want to set any columns in dont_make_factor_glmm, either use c(), NA or '' (or don't set the parameter at all). If you want to name columns here, they have to be named in a character vector.")
        }
        dont_make_factor_glmm<-c()
      }
    } else{
      dont_make_factor_glmm<-c()
    }
  } else{
    notify_fact<-T
    if(length(dont_make_factor_glmm)==0){
      notify_fact<-T
    }
    if(length(dont_make_factor_glmm)==1){
      if(is.vector(dont_make_factor_glmm)&!is.list(dont_make_factor_glmm)){
        if(sum(is.na(dont_make_factor_glmm))==1){
          notify_fact<-T
        } else{
          if(is.character(dont_make_factor_glmm)){
            if(gsub(" ","",dont_make_factor_glmm)==""){
              notify_fact<-T
            }
          }
        }
      }
    }
    if(!notify_fact){
      if(verbose){
        if(type_of_stats=="freq_binom"){
          print("Since type_of_stats=='freq_binom', your setting for dont_make_factor_glmm is being omitted.")
        } else{
          print("Since no GLM/GLMM can be performed under your current settings, your setting for dont_make_factor_glmm is being omitted.")
        }
      }
    }
  }
  
  #other_effects_sub1
  
  already_warned<-F
  if(type_of_stats!="freq_binom"&stats_for_1sub){
    if(length(other_effects_sub1)==0){
      other_effects_sub1<-""
    }
    if(!is.vector(other_effects_sub1)|is.list(other_effects_sub1)){
      stop("other_effects_sub1 has to be a vector or empty vector.")
    }
    if(sum(is.na(other_effects_sub1))==0){
      if(!is.character(other_effects_sub1)){
        stop("other_effects_sub1 has to be of type character. If you don't want other effects of sub1, type NA, c() or ''.")
      }
      if(length(other_effects_sub1)>1){
        stop("other_effects_sub1 has to be ONE character string. E.g. '+(1|ID)+(1|trial)' or '+age+(1|trial)'.")
      } 
      other_effects_sub1<-gsub(" ","",other_effects_sub1)
      if(other_effects_sub1!=""){
        # if(!(grepl("^[A-Za-z]+$",substr(other_effects_sub1,1,1), perl = T)|substr(other_effects_sub1,1,1)=="+"|substr(other_effects_sub1,1,1)=="-")){
        #   stop("Your rand")
        # }
        
        #This error check is not very perfect
        
        #interact_error<-!grepl(paste0("+",sub1_col),other_effects_sub1)
        #if(!interact_error)
        if(substr(other_effects_sub1,1,1)==":"){
          stop(paste0("You started your other_effects_sub1 off with a ':'. sub1_col cannot be involved in an interaction term alone, and the way you phrased the other_effects_sub1 expression, it seems that this is what's happening. If you want to include an additional interaction term, please specify this interaction term inside other_effects_sub1, including your sub1_col. OR: start your other_effects_sub1 off with a '*'!"))
        }
        if(substr(other_effects_sub1,1,1)!="+"&!substr(other_effects_sub1,1,1)%in%c("-",":","*","/")){
          other_effects_sub1<-paste0("+",other_effects_sub1)
          if(verbose){
            print("A plus sign was added at the beginning of other_effects_sub1, since you started without any sign.")
          }
        }
        other_effects1_s<-strsplit(other_effects_sub1,"\\+")[[1]]
        #reunite if other effects were split apart, e.g. hypothetical case of "(1+x|1+ID)"
        if(length(other_effects1_s)>1){
          corrector_update<-0
          for(corrector in 1:length(other_effects1_s)){
            if(corrector+corrector_update<length(other_effects1_s)){
              if(grepl("\\(",other_effects1_s[corrector+corrector_update])&!grepl("\\|",other_effects1_s[corrector+corrector_update])&!grepl("\\(",other_effects1_s[corrector+corrector_update+1])&grepl("\\|",other_effects1_s[corrector+corrector_update+1])){
                other_effects1_s[corrector+corrector_update]<-paste0(other_effects1_s[corrector+corrector_update],"+",other_effects1_s[corrector+corrector_update+1])
                other_effects1_s<-other_effects1_s[-(corrector+corrector_update+1)]
                corrector_update<-corrector_update-1
              }
            }
            if(corrector+corrector_update>1){
              if(grepl("\\|",other_effects1_s[corrector+corrector_update-1])&!grepl("\\)",other_effects1_s[corrector+corrector_update-1])&!grepl("\\|",other_effects1_s[corrector+corrector_update])&grepl("\\)",other_effects1_s[corrector+corrector_update])){
                other_effects1_s[corrector+corrector_update]<-paste0(other_effects1_s[corrector+corrector_update-1],"+",other_effects1_s[corrector+corrector_update])
                other_effects1_s<-other_effects1_s[-(corrector+corrector_update-1)]
                corrector_update<-corrector_update-1
              }
            }
          }
        }
        
        if(length(other_effects1_s)>0){
          for(testr in 1:length(other_effects1_s)){
            if(other_effects1_s[testr][[1]][1]!=""){
              if(grepl("\\(",other_effects1_s[testr])[[1]][1]){
                brack1<-gregexpr(pattern="\\(",other_effects1_s[testr])[[1]][1]
                brack2<-gregexpr(pattern="\\)",other_effects1_s[testr])[[1]][1]
                plus<-gregexpr(pattern="\\|",other_effects1_s[testr])[[1]][1]
                if(length(brack1)==0|length(brack2)==0|length(plus)==0){
                  stop("Not a valid input for other effects in other_effects_sub1. A valid expression would e.g. be '+(1|ID)+(1|trial)' or '+age+(1|trial)'. Please contact me in case the error is not justified or go yourself through the source code.")
                } else{
                  if(sum(order(c(brack1,plus,brack2))==c(1,2,3))!=3){
                    stop("Not a valid input for other effects in other_effects_sub1. A valid expression would e.g. be '+(1|ID)+(1|trial)' or '+age+(1|trial)'. Please contact me in case the error is not justified or go yourself through the source code.")
                  }
                }
              } 
            }
          }
        }
        if(sub1_2_in_one_model&check_sub2_states&stats_for_1sub&stats_for_2sub&type_of_stats!="freq_binom"){
          if(nchar(other_effects_sub1)>=(1+nchar(sub2_col))){
            if(!(grepl(paste0(sub1_col,":",sub2_col),other_effects_sub1)|grepl(paste0(sub2_col,":",sub1_col),other_effects_sub1)|substr(other_effects_sub1,1,1+nchar(sub2_col))==paste0("*",sub2_col))){
              stop("Since you set sub1_2_in_one_model to TRUE, your model term has to include an interaction term between sub1_col and sub2_col. Ideally you would set your other_effects_sub1 to start with '*' and after that put your sub2_col.")
            }
          }
        }
      }
    } else{
      other_effects_sub1<-""
    }
  } else{
    check_rand1<-F
    if(length(other_effects_sub1)==0){
      check_rand1<-T
    }
    if(length(other_effects_sub1)==1){
      if(is.vector(other_effects_sub1)&!is.list(other_effects_sub1)){
        if(sum(is.na(other_effects_sub1))==1){
          check_rand1<-T
        } else{
          if(is.character(other_effects_sub1)){
            if(gsub(" ","",other_effects_sub1)==""){
              check_rand1<-T
            }
          }
        }
      }
    }
    if(!check_rand1){
      if(verbose){
        if(type_of_stats=="freq_binom"){
          print("Since type_of_stats=='freq_binom', your setting for other_effects_sub1 is being omitted.")
        } else{
          print("Since stats_for_1sub=F, your setting for other_effects_sub1 is being omitted.")
        }
      }
    } else{
      other_effects_sub1<-""
    }
  }
  
  #other_effects_sub2
  
  rand2_omitted<-F
  if(type_of_stats!="freq_binom"){
    if(check_sub2_states&stats_for_2sub){
      if(length(other_effects_sub2)==0){
        other_effects_sub2<-""
      }
      if(!is.vector(other_effects_sub2)|is.list(other_effects_sub2)){
        stop("other_effects_sub2 has to be a vector or empty vector.")
      }
      if(sum(is.na(other_effects_sub2))==0){
        if(!is.character(other_effects_sub2)){
          stop("other_effects_sub2 has to be of type character. If you don't want other effects of sub2, type NA, c() or ''.")
        }
        if(length(other_effects_sub2)>1){
          stop("other_effects_sub2 has to be ONE character string. E.g. '+(1|ID)+(1|trial)' or '+age+(1|trial)'.")
        } 
        other_effects_sub2<-gsub(" ","",other_effects_sub2)
        if(other_effects_sub2!=""){
          # if(!(grepl("^[A-Za-z]+$",substr(other_effects_sub2,1,1), perl = T)|substr(other_effects_sub2,1,1)=="+"|substr(other_effects_sub2,1,1)=="-")){
          #   stop("Your rand")
          # }
          
          #This error check is not very perfect
          
          #interact_error<-!grepl(paste0("+",sub2_col),other_effects_sub2)
          #if(!interact_error)
          if(substr(other_effects_sub2,1,1)==":"){
            stop(paste0("You started your other_effects_sub2 off with a ':'. sub2_col cannot be involved in an interaction term alone, and the way you phrased the other_effects_sub2 expression, it seems that this is what's happening. If you want to include an additional interaction term, please specify this interaction term inside other_effects_sub2, including your sub2_col."))
          }
          if(substr(other_effects_sub2,1,1)!="+"&!substr(other_effects_sub2,1,1)%in%c("-",":","*","/")){
            other_effects_sub2<-paste0("+",other_effects_sub2)
            if(verbose){
              print("A plus sign was added at the beginning of other_effects_sub2, since you started without any sign.")
            }
          }
          other_effects1_s<-strsplit(other_effects_sub2,"\\+")[[1]]
          #reunite if other effects were split apart, e.g. hypothetical case of "(1+x|1+ID)"
          if(length(other_effects1_s)>1){
            corrector_update<-0
            for(corrector in 1:length(other_effects1_s)){
              if(corrector+corrector_update<length(other_effects1_s)){
                if(grepl("\\(",other_effects1_s[corrector+corrector_update])&!grepl("\\|",other_effects1_s[corrector+corrector_update])&!grepl("\\(",other_effects1_s[corrector+corrector_update+1])&grepl("\\|",other_effects1_s[corrector+corrector_update+1])){
                  other_effects1_s[corrector+corrector_update]<-paste0(other_effects1_s[corrector+corrector_update],"+",other_effects1_s[corrector+corrector_update+1])
                  other_effects1_s<-other_effects1_s[-(corrector+corrector_update+1)]
                  corrector_update<-corrector_update-1
                }
              }
              if(corrector+corrector_update>1){
                if(grepl("\\|",other_effects1_s[corrector+corrector_update-1])&!grepl("\\)",other_effects1_s[corrector+corrector_update-1])&!grepl("\\|",other_effects1_s[corrector+corrector_update])&grepl("\\)",other_effects1_s[corrector+corrector_update])){
                  other_effects1_s[corrector+corrector_update]<-paste0(other_effects1_s[corrector+corrector_update-1],"+",other_effects1_s[corrector+corrector_update])
                  other_effects1_s<-other_effects1_s[-(corrector+corrector_update-1)]
                  corrector_update<-corrector_update-1
                }
              }
            }
          }
          
          if(length(other_effects1_s)>0){
            for(testr in 1:length(other_effects1_s)){
              if(other_effects1_s[testr][[1]][1]!=""){
                if(grepl("\\(",other_effects1_s[testr])[[1]][1]){
                  brack1<-gregexpr(pattern="\\(",other_effects1_s[testr])[[1]][1]
                  brack2<-gregexpr(pattern="\\)",other_effects1_s[testr])[[1]][1]
                  plus<-gregexpr(pattern="\\|",other_effects1_s[testr])[[1]][1]
                  if(length(brack1)==0|length(brack2)==0|length(plus)==0){
                    stop("Not a valid input for other effects in other_effects_sub2. A valid expression would e.g. be '+(1|ID)+(1|trial)' or '+age+(1|trial)'. Please contact me in case the error is not justified or go yourself through the source code.")
                  } else{
                    if(sum(order(c(brack1,plus,brack2))==c(1,2,3))!=3){
                      stop("Not a valid input for other effects in other_effects_sub2. A valid expression would e.g. be '+(1|ID)+(1|trial)' or '+age+(1|trial)'. Please contact me in case the error is not justified or go yourself through the source code.")
                    }
                  }
                } 
              }
            }
          }
        }
      } else{
        other_effects_sub2<-""
      }
    }else{
      rand2_omitted<-T
    }
  }else{
    rand2_omitted<-T
  } 
  #The omission check
  if(rand2_omitted){
    check_rand2<-F
    if(length(other_effects_sub2)==0){
      check_rand2<-T
    }
    if(length(other_effects_sub2)==1){
      if(is.vector(other_effects_sub2)&!is.list(other_effects_sub2)){
        if(sum(is.na(other_effects_sub2))==1){
          check_rand2<-T
        } else{
          if(is.character(other_effects_sub2)){
            if(gsub(" ","",other_effects_sub2)==""){
              check_rand2<-T
            }
          }
        }
      }
    }
    if(!check_rand2){
      if(verbose){
        if(type_of_stats=="freq_binom"){
          print("Since type_of_stats=='freq_binom', your setting for other_effects_sub2 is being omitted.")
        } else{
          if(!check_sub2_states){
            print("Since no sub2_col was set, your setting for other_effects_sub2 is being omitted.")
          } else{
            print("Since stats_for_2sub=F, your setting for other_effects_sub2 is being omitted.")
          }
        }
      }
    } else{
      other_effects_sub2<-""
    }
  }
  
  if(type_of_stats!="freq_binom"){
    is.random<-F
    other_eff_split<-""
    if(stats_for_1sub){
      if(gsub(" ","",other_effects_sub1)!=""){
        other_eff_split<-unlist(strsplit(unlist(strsplit(strsplit(other_effects_sub1, "\\+|\\(|\\)|\\:|\\=|\\}|\\{|\\[|\\]|\\/|\\-|\\&|\\*|\\~")[[1]],"\\|")),"[\\\\]|[^[:print:]]",fixed=F))
        other_eff_split<-trimws(unique(other_eff_split[suppressWarnings(is.na(as.numeric(other_eff_split)))]))
        is.random<-T
      }
    }
    if(check_sub2_states&stats_for_2sub){      
      if(gsub(" ","",other_effects_sub2)!=""){
        other_eff_split2<-unlist(strsplit(unlist(strsplit(strsplit(other_effects_sub2, "\\+|\\(|\\)|\\:|\\=|\\}|\\{|\\[|\\]|\\/|\\-|\\&|\\*|\\~")[[1]],"\\|")),"[\\\\]|[^[:print:]]",fixed=F))
        other_eff_split2<-trimws(unique(other_eff_split2[suppressWarnings(is.na(as.numeric(other_eff_split2)))]))
        if(gsub(" ","",other_effects_sub1)!=""){
          other_eff_split<-unique(c(other_eff_split,other_eff_split2))
        } else{
          other_eff_split<-unique(other_eff_split2)
        }
        is.random<-T
      }
    }
  }
  if(type_of_stats!="freq_binom"){
    if(is.random){
      if(sum(other_eff_split=="")!=length(other_eff_split)){
        coln_split<-unlist(strsplit(unlist(strsplit(unlist(strsplit(names(tfinal), "\\+|\\(|\\)|\\:|\\=|\\}|\\{|\\[|\\]|\\/|\\-|\\&|\\*|\\~")),"\\|")),"[\\\\]|[^[:print:]]",fixed=F))
        if(length(coln_split)!=length(names(tfinal))){
          warning("One or more of your column names contain unusual characters, which may cause the function to be slightly slower (or even do wrong things in a very unlikely scenario). Please try not to include the following characters in your column names: +():=}{[]/-&*~|")
          already_warned<-T
        }
        if(sum(other_eff_split[other_eff_split!=""]%in%trimws(names(tfinal))|other_eff_split[other_eff_split!=""]%in%c(0,1))!=sum(other_eff_split!="")){
          stop(paste0("The other effect(s) ",paste0("'",other_eff_split[other_eff_split!=""][!(other_eff_split[other_eff_split!=""]%in%trimws(names(tfinal))|other_eff_split[other_eff_split!=""]%in%c(0,1))],"'",collapse=",")," is/are not valid column name(s). If you see the here mentioned other effects not as you wanted the function to recognize the columns you meant, it may likely be a mistake of this error check. Please contact me in that case or go yourself through the source code."))
        }
      }
    }
  }
  
  
  #simplify_for_model
  if(stats_for_1sub|stats_for_2sub){
    if(type_of_stats%in%c("freq_glm_glmer","bayes_glm_glmer")){
      if(length(simplify_for_model)!=1){
        stop("simplify_for_model has to be a boolean vector of length 1 (either TRUE or FALSE).")
      }
      if(!is.vector(simplify_for_model)|is.list(simplify_for_model)){
        stop("simplify_for_model has to be a boolean vector of length 1 (either TRUE or FALSE).")
      }
      if(!simplify_for_model%in%0:1){
        stop("simplify_for_model has to be boolean (T or F).")
      }
    }
  }
  
  #test_type
  if(stats_for_1sub|stats_for_2sub){
    if(type_of_stats=="freq_glm_glmer"){
      if(length(test_type)!=1){
        stop("test_type has to be a character vector of length one.")
      }
      if(!is.vector(test_type)|is.list(test_type)){
        stop("test_type has to be a character vector of length one.")
      }
      if(!is.character(test_type)){
        stop("test_type has to be a character vector of length one.")
      }
      if(!test_type%in%c("LRT","typeIII_anova")){
        stop("test_type has to be either LRT or typeIII_anova.")
      }
      if(sub1_2_in_one_model&test_type=="LRT"){
        stop("test_type can only be typeIII_anova, as you set sub1_2_in_one_model to TRUE.")
      }
      if(stats_for_1sub){
        if(grepl(sub1_col,other_effects_sub1)&test_type=="LRT"){
          stop("test_type can only be typeIII_anova, as your sub1_col appears in other_effects_sub1.")
        } else{
          if((substr(trimws(other_effects_sub1),1,1)=="*")&test_type=="LRT"){
            stop("test_type can only be typeIII_anova, as other_effects_sub1 indicates that sub1_col is involved in an interaction.")
          }
        }
      }
      
      if(check_sub2_states&stats_for_2sub){
        if(sum(grepl(sub2_col[!is.na(sub2_col)],other_effects_sub2))>0){
          stop("test_type can only be typeIII_anova, as your sub2_col appears in other_effects_sub2.")
        } else{
          if(substr(trimws(other_effects_sub2),1,1)=="*"){
            stop("test_type can only be typeIII_anova, as other_effects_sub2 indicates that sub2_col is involved in an interaction.")
          }
        }
      }
    }
  }
  
  #scale_center
  if(type_of_stats!="freq_binom"&((stats_for_1sub&gsub(" ","",other_effects_sub1)!="")|(stats_for_2sub&gsub(" ","",other_effects_sub2)!=""))){
    if(length(scale_center)!=1){
      stop("scale_center has to be a boolean vector of length 1 (either TRUE or FALSE).")
    }
    if(!is.vector(scale_center)|is.list(scale_center)){
      stop("scale_center has to be a boolean vector of length 1 (either TRUE or FALSE).")
    }
    if(!scale_center%in%0:1){
      stop("scale_center has to be boolean (T or F).")
    }
  }
  
  #nsim_bayes
  
  if(type_of_stats=="bayes_glm_glmer"){
    if(length(nsim_bayes)!=1){
      stop("nsim_bayes has to be a numeric vector of size 1.")
    }
    if(!is.vector(nsim_bayes)|is.list(nsim_bayes)){
      stop("nsim_bayes has to be a numeric vector of size 1.")
    }
    if(!is.numeric(nsim_bayes)){
      stop("nsim_bayes has to be a numeric vector of size 1.")
    }
  }
  
  #only_stats
  
  if(length(only_stats)!=1){
    stop("only_stats has to be a boolean vector of length 1 (either TRUE or FALSE).")
  }
  if(!is.vector(only_stats)|is.list(only_stats)){
    stop("only_stats has to be a boolean vector of length 1 (either TRUE or FALSE).")
  }
  if(!only_stats%in%0:1){
    stop("only_stats has to be boolean (T or F).")
  }
  
  if(only_stats&stats_for_1sub==F&stats_for_2sub==F){
    stop("You chose to only perform stats and not plot, but at the same time you chose to not perform stats on the different subsets. There is no point then running the function.")
  }
  
  #sub1_panel_weights
  
  if(length(sub1_states)>1){
    skip_sub1_panel_weights<-F
    if(length(sub1_panel_weights)>0){
      if(is.vector(sub1_panel_weights)&!is.list(sub1_panel_weights)){
        if(length(sub1_panel_weights)==1){
          if(is.na(sub1_panel_weights)){
            sub1_panel_weights<-c()
            skip_sub1_panel_weights<-T
          } else{
            if(gsub(" ","",sub1_panel_weights)==""){
              sub1_panel_weights<-c()
              skip_sub1_panel_weights<-T
            }
          }
        }
      }
    } else{
      sub1_panel_weights<-c()
      skip_sub1_panel_weights<-T
    }
    
    if(!skip_sub1_panel_weights){
      
      if(length(sub1_panel_weights)!=length(sub1_states)){
        stop("sub1_panel_weights has to be a numeric vector of same length as length sub1_states.")
      }
      if(!is.vector(sub1_panel_weights)|is.list(sub1_panel_weights)){
        stop("sub1_panel_weights has to be a numeric vector of same length as length sub1_states.")
      }
      if(!is.numeric(sub1_panel_weights)){
        stop("sub1_panel_weights has to be numeric.")
      }
      if(sum(sub1_panel_weights<=0)>0){
        stop("If you chose to set sub1_panel_weights, all its values have to be bigger than 0.")
      }
      #Scale minimum to 1.
      sub1_panel_weights<-sub1_panel_weights/min(sub1_panel_weights)
    } else{
      sub1_panel_weights<-rep(1,length(sub1_states))
    }
  } else{
    skip_sub1_panel_weights<-F
    sub1_panel_weights<-rep(1,length(sub1_states))
  }
  sub1_pos_in_plot<-sapply(1:length(sub1_panel_weights),function(x) sum(sub1_panel_weights[1:x])-0.5*sub1_panel_weights[x])
  
  
  #####PLOT ERROR CHECK
  
  if(!only_stats){
    
    #output_dir
    
    if(is.list(output_dir)){
      stop("output_dir cannot be a list.")
    }
    output_as_file<-F
    if(length(output_dir)>0){
      if(length(output_dir)>1){
        stop("output_dir has to be set to either c(), NA or '' for no output file for the plot or to a valid file path as character string.")
      }
      if(!is.na(output_dir)){
        if(gsub(" ","",output_dir)!=""){
          output_as_file<-T
        }
      }
    }
    
    #plot_dim
    
    if(output_as_file){
      skip_plot_dim<-F
      if(length(plot_dim)>0){
        if(is.vector(plot_dim)&!is.list(plot_dim)){
          if(length(plot_dim)==1){
            if(is.na(plot_dim)){
              plot_dim<-c()
              skip_plot_dim<-T
            } else{
              if(gsub(" ","",plot_dim)==""){
                plot_dim<-c()
                skip_plot_dim<-T
              }
            }
          }
        }
      } else{
        plot_dim<-c()
        skip_plot_dim<-T
      }
      
      if(!skip_plot_dim){
        
        if(length(plot_dim)!=2){
          stop("plot_dim has to be a numeric vector of length 2.")
        }
        if(!is.vector(plot_dim)|is.list(plot_dim)){
          stop("plot_dim has to be a numeric vector of length 2.")
        }
        if(!is.numeric(plot_dim)){
          stop("plot_dim has to be numeric.")
        }
        if(sum(plot_dim<=0)>0){
          stop("If you chose to set plot_dim, both its values have to be bigger than 0.")
        }
      }
      
    }
    
    #par_mar
    
    skip_par_mar<-F
    if(length(par_mar)>0){
      if(is.vector(par_mar)&!is.list(par_mar)){
        if(length(par_mar)==1){
          if(is.na(par_mar)){
            par_mar<-c()
            skip_par_mar<-T
          } else{
            if(gsub(" ","",par_mar)==""){
              par_mar<-c()
              skip_par_mar<-T
            }
          }
        }
      }
    } else{
      par_mar<-c()
      skip_par_mar<-T
    }
    
    if(!skip_par_mar){
      
      if(length(par_mar)!=4){
        stop("par_mar has to be a numeric vector of length 4.")
      }
      if(!is.vector(par_mar)|is.list(par_mar)){
        stop("par_mar has to be a numeric vector of length 4.")
      }
      if(!is.numeric(par_mar)){
        stop("par_mar has to be numeric.")
      }
      if(sum(par_mar<0)>0){
        stop("If you chose to set par_mar, all its values have to be 0 or bigger than 0.")
      }
    }
    
    #show_estimators
    
    if(stats_for_1sub|stats_for_2sub){
      if(length(show_estimators)!=2){
        stop("show_estimators has to be a boolean vector of length 2.")
      }
      if(!is.vector(show_estimators)|is.list(show_estimators)){
        stop("show_estimators has to be a boolean vector of length 2.")
      }
      if(sum(!show_estimators%in%0:1)>0){
        stop("show_estimators has to be boolean (T or F).")
      }
      
      #show_p_values_plot
      
      if(!type_of_stats%in%c("bayes_own_brm","bayes_glm_glmer")){
        if(length(show_p_values_plot)!=2){
          stop("show_p_values_plot has to be a boolean vector of length 2.")
        }
        if(!is.vector(show_p_values_plot)|is.list(show_p_values_plot)){
          stop("show_p_values_plot has to be a boolean vector of length 2.")
        }
        if(sum(!show_p_values_plot%in%0:1)>0){
          stop("show_p_values_plot has to be boolean (T or F).")
        }
      } else{
        show_p_values_plot<-c(F,F)
      }
    }
    
    #sub1_labels
    
    #if labels are missing
    if(missing(sub1_labels)){
      if(verbose){
        if(create_dummy_col){
          print("Since sub1_labels was not set and no sub1_col was set, sub1_labels is set to 'ALL_DATA'.")
        } else{
          print("Since sub1_labels was not set, sub1_states is used as labels.")
        }
      }
      sub1_labels<-sub1_states
    } else{
      if(length(sub1_states)==0){
        stop("sub1_labels has to be set as a character vector of same size as sub1_states.")
      }
      if(!is.vector(sub1_labels)|is.list(sub1_labels)){
        stop("sub1_labels has to be set as a character vector of same size as sub1_states.")
      }
      if(sum(is.na(sub1_labels))>0){
        stop("No NAs in labels.")
      }
      if(!is.character(sub1_labels)){
        stop("sub1_labels has to be set as a character vector of same size as sub1_states.")
      }
      if(create_dummy_col){
        if(length(sub1_labels)>1){
          if(verbose){
            print("Since sub1_labels was not set, all elements in sub1_labels are removed except the first one.")
          }
          sub1_labels<-sub1_labels[1]
        }
      } else{
        if(length(sub1_labels)!=length(sub1_states)){
          stop("sub1_labels has to be set as a character vector of same size as sub1_states.")
        }
      }
    }
    
    #italics_sub1
    
    if(length(italics_sub1)!=1){
      stop("italics_sub1 has to be of length one and boolean (either TRUE or FALSE).")
    }
    if(!is.vector(italics_sub1)|is.list(italics_sub1)){
      stop("italics_sub1 has to be a boolean vector of length 1 (either TRUE or FALSE).")
    }
    if(!italics_sub1%in%0:1){
      stop("italics_sub1 has to be boolean (T or F).")
    }
    
    #sub2_labels
    
    if(check_sub2_states){
      #If labels are missing
      if(missing(sub2_labels)){
        if(verbose){
          print("Since sub2_labels was not set, sub2_states is used as labels.")
        }
        sub2_labels<-sub2_states
      } else{
        check_labels2<-T
        if(length(sub2_labels)==0){
          check_labels2<-F
          sub2_labels<-NA
        } else{
          if(length(sub2_labels)==1){
            if(is.vector(sub2_labels)&!is.list(sub2_labels)){
              if(is.na(sub2_labels)){
                check_labels2<-F
              }
              if(gsub(" ","",sub2_labels)==""){
                check_labels2<-F
                sub2_labels<-NA
              }
            }
          }
        }
        if(check_labels2){
          if(length(sub2_labels)!=length(sub2_states)){
            stop("The list of sub2_labels has to be of same length as sub2_states. If you don't want sub2_labels, put sub2_labels to NA, c() or ''.")
          }
          if(!is.list(sub2_labels)){
            stop("sub2_labels has to be a list or if you don't want sub2_labels, put sub2_labels to NA, c() or ''.")
          }
          sub2_labels_length<-sapply(1:length(sub2_labels),function(x) length(sub2_labels[[x]]))
          if(sum(sub2_labels_length==1)>0){
            if(sum(is.na(sapply(1:sum(sub2_labels_length==1),function(x) sub2_labels[[(1:length(sub2_labels))[sub2_labels_length==1][x]]])))!=sum(sub2_labels_length==1)){
              sub2_labels_length[length(sub2_labels_length)==1]<-NA
              if(!sub2_states_automatic){
                warning("In the sub2_labels list, those positions where only one sub1_state exists were filled automatically with an NA. To avoid this message, fill with NA yourself.")
              }
            }
          }
          if(sum(sub2_labels_length>2)>0){
            stop("No element of sub2_labels can be of more than size 2.")
          }
          if(sum(sub_lengths==sub2_labels_length)!=length(sub2_labels)){
            stop("The lengths of the single elements of sub2_labels does not match the length of the single elements of sub2_states.")
          }
          if(sum(sub2_labels_length>1)>0){
            if(sum(sapply(1:sum(sub2_labels_length>1),function(x) is.character(sub2_labels[[(1:length(sub2_labels))[sub2_labels_length>1][x]]])))!=sum(sub2_labels_length>1)){
              stop("Not all sub2_labels at those positions respective to where sub2_states exist are character strings.")
            }
          }
        }
      }
    } else{
      if(missing(sub2_labels)){
        sub2_labels<-NA
      }
      give_sub2_error<-T
      if(length(sub2_labels)==0){
        give_sub2_error<-F
      } else{
        if(is.vector(sub2_labels)&!is.list(sub2_labels)){
          if(length(sub2_labels)==1){
            if(is.na(sub2_labels)){
              give_sub2_error<-F
            } else{
              if(is.character(sub2_labels)){
                if(gsub(" ","",sub2_labels)==""){
                  give_sub2_error<-F
                }
              }
            }
          } else{
            if(sum(is.na(sub2_labels))==length(sub2_labels)){
              give_sub2_error<-F
            }
          }
        } else{
          if(is.list(sub2_labels)){
            if(sum(is.na(unlist(sub2_labels)))==length(unlist(sub2_labels))){
              give_sub2_error<-F
            }
          }
        }
        if(give_sub2_error&verbose){
          print("You did set sub2_labels, but you did not set sub2_col. Your setting for sub2_labels will be omitted.")
        }
      }
    }
    
    #cex_sub1_labels
    if(length(cex_sub1_labels)!=1){
      stop("cex_sub1_labels has to be a numeric vector of size 1.")
    }
    if(!is.vector(cex_sub1_labels)|is.list(cex_sub1_labels)){
      stop("cex_sub1_labels has to be a numeric vector of size 1.")
    }
    if(!is.numeric(cex_sub1_labels)){
      stop("cex_sub1_labels has to be a numeric vector of size 1.")
    }
    
    #cex_sub1_counts
    if(length(cex_sub1_counts)!=1){
      stop("cex_sub1_counts has to be a numeric vector of size 1.")
    }
    if(!is.vector(cex_sub1_counts)|is.list(cex_sub1_counts)){
      stop("cex_sub1_counts has to be a numeric vector of size 1.")
    }
    if(!is.numeric(cex_sub1_counts)){
      stop("cex_sub1_counts has to be a numeric vector of size 1.")
    }
    
    #cex_sub2_labels
    if(check_sub2_states){
      if(length(cex_sub2_labels)!=1){
        stop("cex_sub2_labels has to be a numeric vector of size 1.")
      }
      if(!is.vector(cex_sub2_labels)|is.list(cex_sub2_labels)){
        stop("cex_sub2_labels has to be a numeric vector of size 1.")
      }
      if(!is.numeric(cex_sub2_labels)){
        stop("cex_sub2_labels has to be a numeric vector of size 1.")
      }
    }
    
    #show_N
    if(length(show_N)!=1){
      stop("show_N has to be of length one and boolean (either TRUE or FALSE).")
    }
    if(!is.vector(show_N)|is.list(show_N)){
      stop("show_N has to be a boolean vector of length 1 (either TRUE or FALSE).")
    }
    if(!show_N%in%0:1){
      stop("show_N has to be boolean (T or F).")
    }
    
    #label_count_col
    
    if(show_N){
      skip_count_col<-F
      if(length(label_count_col)>0){
        if(is.vector(label_count_col)&!is.list(label_count_col)){
          if(length(label_count_col)==1){
            if(is.na(label_count_col)){
              label_count_col<-c()
              skip_count_col<-T
            } else{
              if(gsub(" ","",label_count_col)==""){
                label_count_col<-c()
                skip_count_col<-T
              }
            }
          }
        }
      } else{
        label_count_col<-c()
        skip_count_col<-T
      }
      
      if(!skip_count_col){
        if(length(label_count_col)!=1){
          stop("label_count_col has to be a character vector of size one or set to NA, c() (or NULL) or '' (if no summary by specific column for label numbers wished).")
        }
        if(!is.vector(label_count_col)|is.list(label_count_col)){
          stop("label_count_col has to be a character vector of size one or set to NA, c() (or NULL) or '' (if no summary by specific column for label numbers wished).")
        }
        if(sum(is.na(label_count_col))!=0){
          stop("label_count_col has to be a character vector of size one or set to NA, c() (or NULL) or '' (if no summary by specific column for label numbers wished).")
        }
        if(!is.character(label_count_col)){
          stop("label_count_col has to be a character vector of size one or set to NA, c() (or NULL) or '' (if no summary by specific column for label numbers wished).")
        }
        if(sum(label_count_col%in%names(tfinal))!=1){
          stop("Column named in label_count_col cannot be found in the table's column names.")
        }
      }
    }
    
    #yaxs_label
    
    if(is.list(yaxs_label)){
      stop("yaxs_label cannot be of type list.")
    }
    if(!is.call(yaxs_label)){
      if(length(yaxs_label)!=1){
        stop("yaxs_label either has to be a substitute call or a vector of length 1.")
      }
    }
    if(!is.vector(yaxs_label)&!is.call(yaxs_label)){
      stop("yaxs_label either has to be a substitute call or a vector of length 1.")
    }
    
    #add_y_labels
    
    if(length(add_y_labels)!=1){
      stop("add_y_labels has to be of length one and boolean (either TRUE or FALSE).")
    }
    if(!is.vector(add_y_labels)|is.list(add_y_labels)){
      stop("add_y_labels has to be a boolean vector of length 1 (either TRUE or FALSE).")
    }
    if(!add_y_labels%in%0:1){
      stop("add_y_labels has to be boolean (T or F).")
    }
    
    #y_tck
    
    if(length(y_tck)!=1){
      stop("y_tck has to be a numeric vector of size 1.")
    }
    if(!is.vector(y_tck)|is.list(y_tck)){
      stop("y_tck has to be a numeric vector of size 1.")
    }
    if(!is.numeric(y_tck)){
      stop("y_tck has to be a numeric vector of size 1.")
    }
    
    #horiz_0.5
    
    if(length(horiz_0.5)!=1){
      stop("horiz_0.5 has to be of length one and boolean (either TRUE or FALSE).")
    }
    if(!is.vector(horiz_0.5)|is.list(horiz_0.5)){
      stop("horiz_0.5 has to be a boolean vector of length 1 (either TRUE or FALSE).")
    }
    if(!horiz_0.5%in%0:1){
      stop("horiz_0.5 has to be boolean (T or F).")
    }
    
    #panel_dividers
    
    if(length(panel_dividers)!=1){
      stop("panel_dividers has to be of length one and boolean (either TRUE or FALSE).")
    }
    if(!is.vector(panel_dividers)|is.list(panel_dividers)){
      stop("panel_dividers has to be a boolean vector of length 1 (either TRUE or FALSE).")
    }
    if(!panel_dividers%in%0:1){
      stop("panel_dividers has to be boolean (T or F).")
    }
    
    #dot_size
    
    if(length(dot_size)!=1){
      stop("dot_size has to be a numeric vector of size 1.")
    }
    if(!is.vector(dot_size)|is.list(dot_size)){
      stop("dot_size has to be a numeric vector of size 1.")
    }
    if(!is.numeric(dot_size)){
      stop("dot_size has to be a numeric vector of size 1.")
    }
    
    #dot_lwd
    
    if(length(dot_lwd)!=1){
      stop("dot_lwd has to be a numeric vector of size 1.")
    }
    if(!is.vector(dot_lwd)|is.list(dot_lwd)){
      stop("dot_lwd has to be a numeric vector of size 1.")
    }
    if(!is.numeric(dot_lwd)){
      stop("dot_lwd has to be a numeric vector of size 1.")
    }
    
    #legend_dots
    
    no_legend_check<-F
    if(length(legend_dots)==0){
      no_legend_check<-T
    } else{
      if(length(legend_dots)==1){
        if(is.vector(legend_dots)&!is.list(legend_dots)){
          if(is.na(legend_dots)){
            no_legend_check<-T
          } else{
            if(gsub(" ","",legend_dots)==""){
              no_legend_check<-T
            }
          }
        }
      }
    }
    if(!no_legend_check){
      if(!length(legend_dots)%in%1:5){
        stop("legend_dots has to be a numeric vector of a length between 1 and 5 or has to be set to NA, c() (or NULL) or '' (for no legend).")
      }
      if(!is.vector(legend_dots)|is.list(legend_dots)){
        stop("legend_dots has to be a numeric vector of a length between 1 and 5 or has to be set to NA, c() (or NULL) or '' (for no legend).")
      }
      if(!is.numeric(legend_dots)){
        stop("legend_dots has to be a numeric vector of a length between 1 and 5 or has to be set to NA, c() (or NULL) or '' (for no legend).")
      }
    }
    
    #legend_omi
    if(!no_legend_check){
      if(length(legend_omi)!=1){
        stop("legend_omi has to be of length one and boolean (either TRUE or FALSE).")
      }
      if(!is.vector(legend_omi)|is.list(legend_omi)){
        stop("legend_omi has to be a boolean vector of length 1 (either TRUE or FALSE).")
      }
      if(!legend_omi%in%0:1){
        stop("legend_omi has to be boolean (T or F).")
      }
    }
    
    #legend_info
    if(!no_legend_check){
      if(length(legend_info)!=1){
        stop("legend_info has to be of length one and boolean (either TRUE or FALSE).")
      }
      if(!is.vector(legend_info)|is.list(legend_info)){
        stop("legend_info has to be a boolean vector of length 1 (either TRUE or FALSE).")
      }
      if(!legend_info%in%0:1){
        stop("legend_info has to be boolean (T or F).")
      }
    }
    
    #COLOURS
    
    areColors <- function(x) {
      sapply(x, function(X) {
        tryCatch(is.matrix(col2rgb(X)), 
                 error = function(e) FALSE)
      })
    }
    
    #dot_colours
    
    if(length(dot_colours)==0){
      stop("dot_colours is empty.")
    }
    dont_do_later_col_checks<-F
    colour_as_columns<-F
    if(!is.list(dot_colours)){
      if(!is.vector(dot_colours)){
        stop("dot_colours has to either be provided as a list or as a vector of size 1.")
      }
      dont_do_later_col_checks<-T
      if(length(dot_colours)==1){
        if(!dot_colours%in%names(tfinal)){
          if(!areColors(dot_colours)){
            stop("If you provide dot_colours as a vector of just size 1, the one element has to be a valid colour or a column name of your table.")
          }
        } else{
          if(areColors(dot_colours)){
            stop(paste0("The column name '",dot_colours,"' in your table carries the same name as a valid R colour. Your setting for dot_colours is ambiguous, since you could have either meant this as a single R colour or as a pointer to your colour column."))
          }
          if(sum(areColors(tfinal[,dot_colours]))!=nrow(tfinal)){
            stop("You provided dot_colours as a column name of your table, but not all its elements are valid colours.")
          }
          colour_as_columns<-T
        }
      } else{
        stop("If you provide dot_colours as a vector, it has to be of size 1.")
      }
      #If the list is though one containing one vector of length 1,
      #unlist it.
    } else{
      if(length(dot_colours)==1){
        if(length(dot_colours[[1]])==1){
          dot_colours<-dot_colours[[1]]
          if(!areColors(dot_colours)&!dot_colours%in%names(tfinal)){
            stop("You provided dot_colours as list with size 1, which is a vector of size 1. The list was removed and changed into a vector of size 1, but the colour it contains is neither a valid colour nor a column name.")
          }
          dont_do_later_col_checks<-T
        }
      }
    }
    if(!dont_do_later_col_checks){
      if(length(dot_colours)!=length(sub1_states)){
        if(create_dummy_col){
          if(verbose){
            print("Since you didn't set a sub1_col and the dot_colours list is not of length 1, its first element gets used.")
          }
        } else{
          stop("Since you provide dot_colours as a list of colours, it has to be of same length as sub1_states.")
        }
      }
      dot_colours_length<-sapply(1:length(dot_colours),function(x) length(dot_colours[[x]]))
      if(sum(dot_colours_length>2)>0){
        stop("No element of dot_colours can be of more than size 2.")
      }
      if(!check_sub2_states&sum(dot_colours_length>1)>0){
        #stop("There is no second subsetting performed, but dot_colours has some elements of more than just one colour.")
      }
      if(check_sub2_states){
        if(sum(sub_lengths==dot_colours_length)!=length(dot_colours_length)){
          warning("Some sub2_states don't have different colours, because you didn't provide them in the dot_colours list.")
        }
      }
      if(sum(areColors(unlist(dot_colours)))!=length(unlist(dot_colours))){
        stop("dot_colours has some non-colour elements.")
      }
    }
    
    #dot_colour_legend
    
    if(!no_legend_check){
      if(!is.vector(dot_colour_legend)|is.list(dot_colour_legend)|length(dot_colour_legend)!=1){
        stop("dot_colour_legend has to be provided as a vector of length 1, indicating one colour.")
      }
      if(sum(areColors(dot_colour_legend))!=length(dot_colour_legend)){
        stop("dot_colour_legend is a non-colour element.")
      }
    } else{
      dont_warn_leg_col<-F
      if(length(dot_colour_legend)==0){
        dont_warn_leg_col<-T
      } else{
        if(is.vector(dot_colour_legend)&!is.list(dot_colour_legend)&length(dot_colour_legend)==1){
          if(is.na(dot_colour_legend)){
            dont_warn_leg_col<-T
          } else{
            if(gsub(" ","",dot_colour_legend)==""){
              dont_warn_leg_col<-T
            }
          }
        }
      }
      if(!dont_warn_leg_col&verbose){
        print("You did set dot_colour_legend, but you decided to not have a legend. Your setting for dot_colour_legend will be omitted.")
      }
    }
    
    if(!no_legend_check){
      if(length(squeeze_legend)!=1){
        stop("squeeze_legend has to be a numeric vector of size 1.")
      }
      if(!is.vector(squeeze_legend)|is.list(squeeze_legend)){
        stop("squeeze_legend has to be a numeric vector of size 1.")
      }
      if(!is.numeric(squeeze_legend)){
        stop("squeeze_legend has to be a numeric vector of size 1.")
      }
      if(squeeze_legend<=0){
        stop("squeeze_legend has to be bigger than 0.")
      }
    }
    
    
    #transparency
    
    if(length(transparency)!=1){
      stop("transparency has to be a numeric vector of size 1.")
    }
    if(!is.vector(transparency)|is.list(transparency)){
      stop("transparency has to be a numeric vector of size 1.")
    }
    if(!is.numeric(transparency)){
      stop("transparency has to be a numeric vector of size 1.")
    }
    if(transparency>1|transparency<0){
      stop("transparency has to be a value between 0 and 1.")
    }
    
    if(stats_for_1sub){
      
      #width_mean_sub1
      
      if(length(width_mean_sub1)!=1){
        stop("width_mean_sub1 has to be a numeric vector of size 1.")
      }
      if(!is.vector(width_mean_sub1)|is.list(width_mean_sub1)){
        stop("width_mean_sub1 has to be a numeric vector of size 1.")
      }
      if(!is.numeric(width_mean_sub1)){
        stop("width_mean_sub1 has to be a numeric vector of size 1.")
      }
      
      #width_error_sub1
      
      if(length(width_error_sub1)!=1){
        stop("width_error_sub1 has to be a numeric vector of size 1.")
      }
      if(!is.vector(width_error_sub1)|is.list(width_error_sub1)){
        stop("width_error_sub1 has to be a numeric vector of size 1.")
      }
      if(!is.numeric(width_error_sub1)){
        stop("width_error_sub1 has to be a numeric vector of size 1.")
      }
      if(width_error_sub1<0){
        stop("width_error_sub1 cannot be smaller than 0.")
      }
      
    }
    
    if(stats_for_2sub){
      
      #width_mean_sub2
      
      if(length(width_mean_sub2)!=1){
        stop("width_mean_sub2 has to be a numeric vector of size 1.")
      }
      if(!is.vector(width_mean_sub2)|is.list(width_mean_sub2)){
        stop("width_mean_sub2 has to be a numeric vector of size 1.")
      }
      if(!is.numeric(width_mean_sub2)){
        stop("width_mean_sub2 has to be a numeric vector of size 1.")
      }
      
      #width_error_sub2
      
      if(length(width_error_sub2)!=1){
        stop("width_error_sub2 has to be a numeric vector of size 1.")
      }
      if(!is.vector(width_error_sub2)|is.list(width_error_sub2)){
        stop("width_error_sub2 has to be a numeric vector of size 1.")
      }
      if(!is.numeric(width_error_sub2)){
        stop("width_error_sub2 has to be a numeric vector of size 1.")
      }
      if(width_error_sub2<0){
        stop("width_error_sub2 cannot be smaller than 0.")
      }
      
      #sub2_est_perc_cent
      
      if(length(sub2_est_perc_cent)!=1){
        stop("sub2_est_perc_cent has to be a numeric vector of size 1.")
      }
      if(!is.vector(sub2_est_perc_cent)|is.list(sub2_est_perc_cent)){
        stop("sub2_est_perc_cent has to be a numeric vector of size 1.")
      }
      if(!is.numeric(sub2_est_perc_cent)){
        stop("sub2_est_perc_cent has to be a numeric vector of size 1.")
      }
      if(sub2_est_perc_cent<0|sub2_est_perc_cent>1){
        stop("sub2_est_perc_cent cannot be smaller than 0 or bigger than 1.")
      }
      
    }
    
    if(stats_for_1sub|stats_for_2sub){
      
      #thickness_est_CI
      
      if(length(thickness_est_CI)!=1){
        stop("thickness_est_CI has to be a numeric vector of size 1.")
      }
      if(!is.vector(thickness_est_CI)|is.list(thickness_est_CI)){
        stop("thickness_est_CI has to be a numeric vector of size 1.")
      }
      if(!is.numeric(thickness_est_CI)){
        stop("thickness_est_CI has to be a numeric vector of size 1.")
      }
      if(thickness_est_CI<0){
        stop("thickness_est_CI cannot be smaller than 0.")
      }
      
      #sub_box_yn
      
      if(length(sub_box_yn)==0){
        stop("sub_box_yn has to be at least of size 1.")
      }
      if(!is.list(sub_box_yn)&!is.vector(sub_box_yn)){
        stop("sub_box_yn has to either be provided as a list or as a vector of size 1.")
      }
      if(!is.logical(unlist(sub_box_yn))){
        stop("sub_box_yn has to be boolean.")
      }
      if(!is.list(sub_box_yn)){
        if(length(sub_box_yn)>1){
          stop("sub_box_yn has to be of length 1 if passed as vector.")
        }
      }
      if(is.list(sub_box_yn)){
        if(length(unlist(sub_box_yn))==1){
          sub_box_yn<-unlist(sub_box_yn)
        } else{
          if(check_sub2_states){
            if(length(sub_box_yn)!=length(sub2_states)){
              stop("The length of the list sub_box_yn has to be the same as the length of sub2_states. Don't get confused with what is meant by 'length of a list'. E.g. length(list(1,c(2,3,4)))=2.")
            }
            sub_lengths_box<-sapply(1:length(sub_box_yn),function(x) length(sub_box_yn[[x]]))
            if(sum(sub_lengths>1)>0){
              if(sum(sub_lengths_box[sub_lengths>1]!=3)>0){
                stop("At those sub1_states where two sub2_states are prominent, sub_box_yn has to have three entries at these positions ('left' sub2 category, all dots together, 'right' sub2 category).")
              }
            }
            if(sum(sub_lengths==1)>0){
              if(sum(sub_lengths_box[sub_lengths==1]!=1)>0){
                stop("At those sub1_states where no sub2_states are prominent (no second subset), sub_box_yn has to have only one entry at these positions.")
              }
            }
          } else{
            if(length(sub_box_yn)!=length(sub1_states)){
              stop("The length of the list sub_box_yn has to be the same as the length of the vector sub1_states.")
            }
          }
        }
      }
      
      #box_type
      
      if(sum(unlist(sub_box_yn))>0){
        
        if(length(box_type)!=1){
          stop("box_type has to be a numeric vector of size 1 and can be set to 1, 2 or 3.")
        }
        if(!is.vector(box_type)|is.list(box_type)){
          stop("box_type has to be a numeric vector of size 1 and can be set to 1, 2 or 3.")
        }
        if(!is.numeric(box_type)){
          stop("box_type has to be a numeric vector of size 1 and can be set to 1, 2 or 3.")
        }
        if(!box_type%in%1:3){
          stop("box_type has to be a numeric vector of size 1 and can be set to 1, 2 or 3.")
        }
      }
      
      #boxwex_set
      
      if(sum(unlist(sub_box_yn))>0){
        
        if(length(boxwex_set)!=1){
          stop("boxwex_set has to be a numeric vector of size 1.")
        }
        if(!is.vector(boxwex_set)|is.list(boxwex_set)){
          stop("boxwex_set has to be a numeric vector of size 1.")
        }
        if(!is.numeric(boxwex_set)){
          stop("boxwex_set has to be a numeric vector of size 1.")
        }
      }
      
      #box_colour
      
      if(sum(unlist(sub_box_yn))>0){
        
        if(!is.vector(box_colour)|is.list(box_colour)|length(box_colour)!=1){
          stop("box_colour has to be provided as a vector of length 1, indicating one colour.")
        }
        if(sum(areColors(box_colour))!=length(box_colour)){
          stop("box_colour is a non-colour element.")
        }
      }
      
      #box_whiskers
      
      if(sum(unlist(sub_box_yn))>0){
        
        if(length(box_whiskers)!=1){
          stop("box_whiskers has to be a boolean vector of length 1 (either TRUE or FALSE).")
        }
        if(!is.vector(box_whiskers)|is.list(box_whiskers)){
          stop("box_whiskers has to be a boolean vector of length 1 (either TRUE or FALSE).")
        }
        if(!box_whiskers%in%0:1){
          stop("box_whiskers has to be boolean (T or F).")
        }
      }
      
    }
    
    #thin_line_sub
    
    if(length(thin_line_sub)!=1){
      stop("thin_line_sub has to be of length one and boolean (either TRUE or FALSE).")
    }
    if(!is.vector(thin_line_sub)|is.list(thin_line_sub)){
      stop("thin_line_sub has to be a boolean vector of length 1 (either TRUE or FALSE).")
    }
    if(!thin_line_sub%in%0:1){
      stop("thin_line_sub has to be boolean (T or F).")
    }
    if((stats_for_1sub|stats_for_2sub)&sum(unlist(sub_box_yn))>0){
      if(box_whiskers&thin_line_sub&verbose){
        print("Your boxplot whiskers may not be very visible, since thin_line_sub=T.")
      }
    }
    
    #numb_iterations
    
    if(length(numb_iterations)!=1){
      stop("numb_iterations has to be a numeric vector of size 1.")
    }
    if(!is.vector(numb_iterations)|is.list(numb_iterations)){
      stop("numb_iterations has to be a numeric vector of size 1.")
    }
    if(!is.numeric(numb_iterations)){
      stop("numb_iterations has to be a numeric vector of size 1.")
    }
    
    #border_space
    
    if(length(border_space)!=1){
      stop("border_space has to be a numeric vector of size 1.")
    }
    if(!is.vector(border_space)|is.list(border_space)){
      stop("border_space has to be a numeric vector of size 1.")
    }
    if(!is.numeric(border_space)){
      stop("border_space has to be a numeric vector of size 1.")
    }
    if(border_space<=0){
      stop("border_space has to bigger than 0.")
    }
    
    #center_space_cat
    
    if(check_sub2_states){
      if(length(center_space_cat)!=1){
        stop("center_space_cat has to be a numeric vector of size 1.")
      }
      if(!is.vector(center_space_cat)|is.list(center_space_cat)){
        stop("center_space_cat has to be a numeric vector of size 1.")
      }
      if(!is.numeric(center_space_cat)){
        stop("center_space_cat has to be a numeric vector of size 1.")
      }
      if(center_space_cat<0|center_space_cat>=0.5){
        stop("center_space_cat cannot be smaller than 0 or bigger than 0.5")
      }
    }
    
    #reaction_norm_col
    
    if(check_sub2_states){
      skip_react_norm<-F
      if(length(reaction_norm_col)>0){
        if(is.vector(reaction_norm_col)&!is.list(reaction_norm_col)){
          if(length(reaction_norm_col)==1){
            if(is.na(reaction_norm_col)){
              reaction_norm_col<-c()
              skip_react_norm<-T
            } else{
              if(gsub(" ","",reaction_norm_col)==""){
                reaction_norm_col<-c()
                skip_react_norm<-T
              }
            }
          }
        }
      } else{
        reaction_norm_col<-c()
        skip_react_norm<-T
      }
      
      if(!skip_react_norm){
        if(length(reaction_norm_col)!=1){
          stop("reaction_norm_col has to be a character vector of size one or set to NA, c() (or NULL) or '' (if no reaction norm is wished).")
        }
        if(!is.vector(reaction_norm_col)|is.list(reaction_norm_col)){
          stop("reaction_norm_col has to be a vector, if not set to or set to NA, c() (or NULL) or '' (if no reaction norm is wished).")
        }
        if(sum(is.na(reaction_norm_col))!=0){
          stop("reaction_norm_col has to be a character string, or set to NA, c() (or NULL) or '' (if no reaction norm is wished).")
        }
        if(!is.character(reaction_norm_col)){
          stop("reaction_norm_col has to be a character string, or set to NA, c() (or NULL) or '' (if no reaction norm is wished).")
        }
        if(sum(reaction_norm_col%in%names(tfinal))!=1){
          stop("Column named in reaction_norm_col cannot be found in the table's column names.")
        }
        if(length(sum_by_col_plot)>0){
          if(reaction_norm_col%in%sum_by_col_plot&length(sum_by_col_plot)==1){
            warning("sum_by_col_plot contains reaction_norm_col alone, which doesn't really make sense.")
          }
        }
      }
    } else{
      warn_unused_react_norm<-T
      if(length(reaction_norm_col)==0){
        reaction_norm_col<-c()
        warn_unused_react_norm<-F
      } else{
        if(is.list(reaction_norm_col)&length(reaction_norm_col)==1){
          reaction_norm_col<-reaction_norm_col[[1]]
        }
        if(is.vector(reaction_norm_col)&!is.list(reaction_norm_col)){
          if(length(reaction_norm_col)==1){
            if(is.na(reaction_norm_col)){
              reaction_norm_col<-c()
              warn_unused_react_norm<-F
            } else{
              if(gsub(" ","",reaction_norm_col)==""){
                reaction_norm_col<-c()
                warn_unused_react_norm<-F
              }
            }
          }
        }
      }
      if(warn_unused_react_norm&verbose){
        print("Since you didn't set a sub2_col, your setting for reaction_norm_col is being omitted.")
      }
    }
    #reaction_norm_max_trans
    
    if(check_sub2_states){
      if(length(reaction_norm_col)>0){
        if(length(reaction_norm_max_trans)!=1){
          stop("reaction_norm_max_trans has to be a numeric vector of size 1.")
        }
        if(!is.vector(reaction_norm_max_trans)|is.list(reaction_norm_max_trans)){
          stop("reaction_norm_max_trans has to be a numeric vector of size 1.")
        }
        if(!is.numeric(reaction_norm_max_trans)){
          stop("reaction_norm_max_trans has to be a numeric vector of size 1.")
        }
        if(reaction_norm_max_trans<0|reaction_norm_max_trans>1){
          stop("reaction_norm_max_trans cannot be smaller than 0 or bigger than 1.")
        }
        if(reaction_norm_max_trans>0){
          if(sum(unlist(sub_box_yn))>0|thin_line_sub|show_estimators[1]){
            if(verbose){
              print(paste0("Some of your settings for ",paste(c("sub_box_yn","thin_line_sub","show_estimators")[c(sum(unlist(sub_box_yn))>0,thin_line_sub,show_estimators[1])],collapse=",")," may lead to an aesthetic clash with the reaction norm and your graph may look confusing."))
            }
          }
          if(center_space_cat<0.1){
            center_space_cat<-0.1
            if(verbose){
              print("center_space_cat was set to 0.1 in order for the reaction norm to look well.")
            }
          }
        }
      }
    }
    
    #reaction_norm_weights
    
    if(check_sub2_states){
      if(length(reaction_norm_col)>0&reaction_norm_max_trans>0){
        if(length(reaction_norm_weights)!=1){
          stop("reaction_norm_weights has to be a numeric vector of size 1 and can be set to 1, 2 or 3.")
        }
        if(!is.vector(reaction_norm_weights)|is.list(reaction_norm_weights)){
          stop("reaction_norm_weights has to be a numeric vector of size 1 and can be set to 1, 2 or 3.")
        }
        if(!is.numeric(reaction_norm_weights)){
          stop("reaction_norm_weights has to be a numeric vector of size 1 and can be set to 1, 2 or 3.")
        }
        if(!reaction_norm_weights%in%1:3){
          stop("reaction_norm_weights has to be a numeric vector of size 1 and can be set to 1, 2 or 3.")
        }
      }
    }
    
  }  
  
  if(verbose){
    print("--------")
  }
  
  
  #Throw out rows where there is an NA in an important column.
  #List all important columns.
  na_colos<-sub1_col
  if(check_sub2_states){
    na_colos<-c(na_colos,unlist(sub2_col))
  }
  if(length(sum_by_col_plot)>0){
    na_colos<-c(na_colos,sum_by_col_plot)
  }
  na_colos<-c(na_colos,unlist(response_col))
  if(only_stats==F){
    na_colos<-c(na_colos,reaction_norm_col)
  }
  na_colos<-na_colos[!is.na(na_colos)]
  na_colos<-na_colos[gsub(" ","",na_colos)!=""]
  
  rows_with_NA<-(1:nrow(tfinal))[rowSums(is.na(tfinal[,na_colos]))>0]
  
  if(length(rows_with_NA)>0){
    if(verbose){
      print(paste0("The following rows where removed because of at least one NA entry in an essential column: ",paste(rows_with_NA,collapse=",")))
    }
    tfinal<-tfinal[-rows_with_NA,,drop=F]
  }
  
  
  
  #######################################################
  #Load Libraries
  
  #weighted boxplots
  if("ENmisc"%in% rownames(installed.packages())){
    suppressWarnings(suppressMessages(library(ENmisc)))
  } else{
    stop("Library ENmisc could not be found. It should have been automatically installed by this function, but maybe you were not connected to the internet or use an outdated version of R (this function was created under 3.6.1).")
  }
  #binomial stats
  if("binom"%in% rownames(installed.packages())){
    suppressWarnings(suppressMessages(library(binom)))
  } else{
    stop("Library binom could not be found. It should have been automatically installed by this function, but maybe you were not connected to the internet or use an outdated version of R (this function was created under 3.6.1).")
  }
  #glm/glmer
  if("lme4"%in% rownames(installed.packages())){
    suppressWarnings(suppressMessages(library(lme4)))
  } else{
    stop("Library lme4 could not be found. It should have been automatically installed by this function, but maybe you were not connected to the internet or use an outdated version of R (this function was created under 3.6.1).")
  }
  #Confidence intervals for estimators and p values
  if("emmeans"%in% rownames(installed.packages())){
    suppressWarnings(suppressMessages(library(emmeans)))
  } else{
    stop("Library emmeans could not be found. It should have been automatically installed by this function, but maybe you were not connected to the internet or use an outdated version of R (this function was created under 3.6.1).")
  }
  #For Bayesian approach
  if("arm"%in% rownames(installed.packages())){
    suppressWarnings(suppressMessages(library(arm)))
  } else{
    stop("Library arm could not be found. It should have been automatically installed by this function, but maybe you were not connected to the internet or use an outdated version of R (this function was created under 3.6.1).")
  }
  #For Bayesian approach with own brms model
  if("brms"%in% rownames(installed.packages())){
    suppressWarnings(suppressMessages(library(brms)))
  } else{
    stop("Library brms could not be found. It should have been automatically installed by this function, but maybe you were not connected to the internet or use an outdated version of R (this function was created under 3.6.1).")
  }
  #Sampling from truncated normal distribution
  if("truncnorm"%in%rownames(installed.packages())){
    suppressWarnings(suppressMessages(library(truncnorm)))
  } else{
    stop("Library truncnorm could not be found. It should have been automatically installed by this function, but maybe you were not connected to the internet or use an outdated version of R (this function was created under 3.6.1).")
  }
  
  
  
  #######################################################
  #FUNCTION DECLARATIONS
  
  
  #For GLMM/glm testing
  glm_glmer<-function(data1=NULL,
                      court_cols_spec1=NULL,
                      court_cols_spec2=NULL,
                      subset_col=NULL,
                      subset_vec=NULL,
                      other_effects_expr=NULL,
                      dont_make_factor_glmm1=c(),
                      warning_col_glmm1=c(),
                      for_error_sub="First",
                      if_sub2_sub1_cat=c(),
                      verbose=T,
                      scale_center1=scale_center,
                      sub1_2_in_one_model1=F,
                      sub2_col_for_both=NA,
                      perform_GLM_GLMM=T,
                      brm_model1=NA,
                      test_type="LRT"){
    
    if(perform_GLM_GLMM){
      
      #Reset the contrasts (more stable if interacting factors)
      old_contr<-options("contrasts")
      options(contrasts = c("contr.sum", "contr.poly"))
      
      #Create response column. If multiple columns, add them together
      #temp_court_spec1 stands for "successes"
      #temp_court_spec2 stands for "failures
      if(length(court_cols_spec1)>1){
        data1$temp_court_spec1<-rowSums(data1[,court_cols_spec1])
      } else{
        data1$temp_court_spec1<-data1[,court_cols_spec1]
      }
      if(length(court_cols_spec2)>1){
        data1$temp_court_spec2<-rowSums(data1[,court_cols_spec2])
      } else{
        data1$temp_court_spec2<-data1[,court_cols_spec2]
      }
      
      #Total appearance for each row
      appearo<-as.numeric(rowSums(data1[,c("temp_court_spec1","temp_court_spec2")]))
      
      #Only take non-0 datapoints
      data1<-data1[appearo>0,]
      
      #Throw out all data points that don't belong to the subset. Take care of factors
      if(is.factor(data1[,subset_col])){
        data_sub<-data1[levels(data1[,subset_col])[data1[,subset_col]]%in%unlist(subset_vec),]
        #subset_colcol<-levels(data_sub$subset_col)[data_sub$subset_col]
      } else{
        data_sub<-data1[data1[,subset_col]%in%unlist(subset_vec),]
        #subset_colcol<-data_sub$subset_col
      }
      
      #Drop unused levels
      data_sub<-droplevels(data_sub)
      
      
      #Cannot remember why this was necessary. It gives a
      #neutral name to each level of the subset column. I took it out
      #for now.
      # for(facrep in 1:length(subset_vec)){
      #   subset_colcol[subset_colcol%in%subset_vec[[facrep]]]<-paste0("subset_col",facrep)
      # }
      #Make the subset column a factor.
      #data_sub$subset_col<-as.factor(subset_colcol)
      
      #Check which columns we have in the other_effects term
      other_eff_split<-""
      if(gsub(" ","",other_effects_expr)!=""){
        other_effects_expr<-gsub(" ","",other_effects_expr)
        other_eff_split<-unlist(strsplit(unlist(strsplit(strsplit(other_effects_expr, "\\+|\\(|\\)|\\:|\\=|\\}|\\{|\\[|\\]|\\/|\\-|\\&|\\*|\\~")[[1]],"\\|")),"[\\\\]|[^[:print:]]",fixed=F))
        other_eff_split<-trimws(unique(other_eff_split[suppressWarnings(is.na(as.numeric(other_eff_split)))]))
      }
      
      #Give warning if numeric column becomes factor.
      problem_col<-warning_col_glmm1
      if(sum(gsub(" ","",other_eff_split)=="")==0){
        effects_not_covered<-other_eff_split[!other_eff_split%in%dont_make_factor_glmm1]
        if(length(effects_not_covered)>0){
          for(check_numerico in 1:length(effects_not_covered)){
            numeric_check<-is.numeric(data_sub[,effects_not_covered[check_numerico]])
            if(numeric_check){
              if(!effects_not_covered[check_numerico]%in%problem_col){
                problem_col<-c(problem_col,effects_not_covered[check_numerico])
                warning(paste0("The column '",effects_not_covered[check_numerico], "' from one of your other_effects terms can be understood as numeric in your dataset, was turned by default into a factor for modelling. If you intend this column to be numeric, having it as factor may massively slow down your model, especially if it is set as a fixed effect (and give wrong results). If you want a column to be kept numeric for modelling, define it in dont_make_factor_glmm1 (note that the response_col will never be made a factor by the function, so you don't need to define it in dont_make_factor_glmm1)."))
              }
            }
          }
        }
      }
      
      
      #Make all columns a factor other than the response_col and
      #those specified in dont_make_factor_glmm1 (if given).
      if(length(dont_make_factor_glmm1)>0){
        col_IDs<-(1:dim(data_sub)[2])[!names(data_sub)%in%c(unlist(response_col),dont_make_factor_glmm1,"temp_court_spec1","temp_court_spec2")]
      } else{
        col_IDs<-(1:dim(data_sub)[2])[!names(data_sub)%in%c(unlist(response_col),"temp_court_spec1","temp_court_spec2")]
      }
      for(fact_col in col_IDs){
        if(!is.factor(data_sub[,fact_col])){
          data_sub[,fact_col]<-as.factor(data_sub[,fact_col])
        }
      }
      
      #Only do the following if there is at least 2 groups with data
      if(length(unique(data_sub[,subset_col]))>1){
        
        #cbind together the courtship to the two types
        courtships<-cbind(data_sub$temp_court_spec1, data_sub$temp_court_spec2)
        #This only works for later if we run the model exactly like this.
        #Otherwise the model.matrix function makes problems
        
        #First, get the command the user wants by pasting in other effects
        if(grepl("\\|",other_effects_expr)){
          function_call_FULL<-paste0("glmer(courtships~",subset_col,other_effects_expr,",family=binomial,data=data_sub)")
        } else{
          #If no other effects, perform GLM
          function_call_FULL<-paste0("glm(courtships~",subset_col,other_effects_expr,",family=binomial,data=data_sub)")
        }  
        
        
        #THE NULL MODEL. OLD TRY
        # if(grepl("\\|",other_effects_expr)){
        #   #Null model will depend whether we have an *-interaction term directly
        #   #after the subset column
        #   if(substr(gsub(" ","",other_effects_expr),1,1)=="*"){
        #     other_eff_split<-other_eff_split[gsub(" ","",other_eff_split)!=""]
        #     if(nchar(other_eff_split[1])+1==nchar(other_effects_expr)){
        #       function_call_null<-paste0("glmer(courtships~1+",other_eff_split[1],"+subset_col:",other_eff_split[1],",family=binomial,data=data_sub)")
        #     } else{
        #       function_call_null<-paste0("glmer(courtships~1+",other_eff_split[1],"+subset_col:",other_eff_split[1],substr(other_effects_expr,nchar(other_eff_split[1])+2,nchar(other_effects_expr)),",family=binomial,data=data_sub)")
        #     }
        #   } else{
        #     function_call_null<-paste0("glmer(courtships~1",other_effects_expr,",family=binomial,data=data_sub)")
        #   }
        # } else{
        #   #Null model will depend whether we have an *-interaction term directly
        #   #after the subset column
        #   if(substr(gsub(" ","",other_effects_expr),1,1)=="*"){
        #     other_eff_split<-other_eff_split[gsub(" ","",other_eff_split)!=""]
        #     if(nchar(other_eff_split[1])+1==nchar(other_effects_expr)){
        #       function_call_null<-paste0("glm(courtships~1+",other_eff_split[1],"+subset_col:",other_eff_split[1],",family=binomial,data=data_sub)")
        #     } else{
        #       function_call_null<-paste0("glm(courtships~1+",other_eff_split[1],"+subset_col:",other_eff_split[1],substr(other_effects_expr,nchar(other_eff_split[1])+2,nchar(other_effects_expr)),",family=binomial,data=data_sub)")
        #     }
        #   } else{
        #     function_call_null<-paste0("glm(courtships~1",other_effects_expr,",family=binomial,data=data_sub)")
        #   }
        # }  
        
        #Some other tries...
        #function_call_FULL<-paste0("glmer(cbind(temp_court_spec1,temp_court_spec2)~subset_col",other_effects_expr,",family=binomial,data=data_sub)")
        #function_call_null<-paste0("glmer(cbind(temp_court_spec1,temp_court_spec2)~1",other_effects_expr,",family=binomial,data=data_sub)")
        #function_call_FULL<-paste0("glmer(cbind(data_sub$temp_court_spec1,data_sub$temp_court_spec2)~data_sub$subset_col+(1|data_sub$ID),family=binomial)")
        #function_call_null<-paste0("glmer(cbind(data_sub$temp_court_spec1,data_sub$temp_court_spec2)~1+(1|data_sub$ID),family=binomial)")
        #may also be helpful to do this later:
        #mm<-matrix(c(1,1,0,1),byrow=F,nrow=2)
        
        #run the models. Define evaluation function first.
        #The special thing about this function is that it still returns
        #if a warning pops up and we are still able to catch this warning.
        eval_function <- function(function_call_HERE,subset_type,sub_col_name,sub1_category,null_or_full) {
          model_outcome <- 
            tryCatch(
              withCallingHandlers(
                {
                  error_text <- "No error."
                  list(model_out = eval(parse(text=function_call_HERE)), 
                       error_text = error_text)
                }, 
                warning = function(e) {
                  error_text <<- paste0("WARNING for ",null_or_full," model on ",subset_type," '",sub_col_name,"'",c(paste0(" under the sub1_state: '",if_sub2_sub1_cat,"': "),": ")[c(subset_type==c("sub2_col","sub1_col"))],e)
                  invokeRestart("muffleWarning")
                }
              ), 
              error = function(e) {
                return(list(model_out = NA, error_text = paste0("WARNING for ",null_or_full," model on ",subset_type," '",sub_col_name,"'",c(paste0(" under the sub1_state: '",if_sub2_sub1_cat,"': "),": ")[c(subset_type==c("sub2_col","sub1_col"))],null_or_full," model could not be run, therefore no statistics in this category were performed! Most likely, you have too few (non-NA) observations in one of the relevant columns.")))
              }, 
              finally = {
              }
            )
          
          return(model_outcome)
        }
        
        #scale and center all numeric columns
        if(scale_center1==T&sum(gsub(" ","",other_eff_split)=="")!=length(other_eff_split)){
          centero<-c()
          for(numero in 1:length(other_eff_split)){
            if(is.numeric(data_sub[,other_eff_split[numero]])){
              if(sum(is.na(data_sub[,other_eff_split[numero]]))!=length(data_sub[,other_eff_split[numero]])){
                data_sub[,other_eff_split[numero]]<-scale(data_sub[,other_eff_split[numero]],center=T)
                centero<-c(centero,numero)
              }
            }
          }
          if(verbose&length(centero)>0){
            if(length(centero)>1){
              print(paste0("Columns: ",paste(other_eff_split[centero],collapse=",")," were scaled and centered for significance analysis."))
            } else{
              print(paste0("Column: ",paste(other_eff_split[centero],collapse=",")," was scaled and centered for significance analysis."))
            }
          }
        }
        
        
        full_MODEL<-list(model_out = NA, error_text = "Not run.")
        if(verbose){
          if(for_error_sub=="Second"){
            print(paste0("Run FULL model for sub2_col '",subset_col,"' on data from sub1_state '",if_sub2_sub1_cat,"'."))
          } else{
            print(paste0("Run FULL model for sub1_col '",subset_col,"'."))
          }
        }
        
        full_MODEL<-eval_function(function_call_HERE=function_call_FULL,
                                  subset_type=c("sub1_col","sub2_col")[for_error_sub==c("First","Second")],
                                  sub_col_name=subset_col,
                                  sub1_category=if_sub2_sub1_cat,
                                  null_or_full="FULL")
        
        if(full_MODEL$error_text!="No error."){
          warning(full_MODEL$error_text)
        }
        
        # null_MODEL<-list(model_out = NA, error_text = "Not run.")
        # if(!grepl("model could not be run, therefore",full_MODEL$error_text)){
        #   if(verbose){
        #     if(for_error_sub=="Second"){
        #       print(paste0("Run NULL model for sub2_col '",subset_col,"' on data from sub1_state '",if_sub2_sub1_cat,"'."))
        #     } else{
        #       print(paste0("Run NULL model for sub1_col '",subset_col,"'."))
        #     }
        #   }
        #   null_MODEL<-eval_function(function_call_HERE=function_call_null,
        #                             subset_type=c("sub1_col","sub2_col")[for_error_sub==c("First","Second")],
        #                             sub_col_name=subset_col,
        #                             sub1_category=if_sub2_sub1_cat,
        #                             null_or_full="NULL")
        # }
        
        #If the model ran, do all the following
        if(!grepl("model could not be run, therefore",full_MODEL$error_text)){
          
          #Get rid of warning messages.
          full_MODEL<-full_MODEL$model_out
          
          if(type_of_stats!="bayes_glm_glmer"){
            
            #Depending on whether a glmm or a glm were run.
            #Calculate p and overdispersal
            
            
            #Here, I thought it'd be smart to do contr.sum coding within
            #the GLMM, but it seems this is actually way more unstable
            #Instead, contrasts are now recoded globally (and then changed back again)
            
            # #Find the fixed effects
            # other_eff_split_fac_test<-""
            # if(gsub(" ","",other_effects_expr)!=""){
            #   other_effects_expr<-gsub(" ","",other_effects_expr)
            #   other_eff_split_fac_test<-unlist(strsplit(unlist(strsplit(other_effects_expr, "\\+|\\:|\\=|\\}|\\{|\\[|\\]|\\/|\\-|\\&|\\*|\\~")[[1]]),"[\\\\]|[^[:print:]]",fixed=F))
            #   other_eff_split_fac_test<-trimws(unique(other_eff_split_fac_test[suppressWarnings(is.na(as.numeric(other_eff_split_fac_test)))]))
            # }
            # 
            # dropout<-c()
            # for(unito in 1:length(other_eff_split_fac_test)){
            #   if(grepl("\\|",other_eff_split_fac_test[unito])){
            #     dropout<-c(dropout,unito)
            #   } else{
            #     if((!grepl("\\(",other_eff_split_fac_test[unito])&grepl("\\)",other_eff_split_fac_test[unito]))|(grepl("\\(",other_eff_split_fac_test[unito])&!grepl("\\)",other_eff_split_fac_test[unito]))){
            #       dropout<-c(dropout,unito)
            #     }
            #   }
            # }
            # 
            # other_eff_split_fac_test<-other_eff_split_fac_test[-dropout]
            # 
            # other_eff_split_fac_test<-c(subset_col,other_eff_split_fac_test[!(grepl("\\(",other_eff_split_fac_test)&grepl("\\)",other_eff_split_fac_test)&grepl("\\|",other_eff_split_fac_test))])
            # other_eff_split_fac_test<-gsub("\\(","",other_eff_split_fac_test)
            # other_eff_split_fac_test<-gsub("\\)","",other_eff_split_fac_test)
            # other_eff_split_fac_test<-gsub("\\|","",other_eff_split_fac_test)
            # other_eff_split_fac_test<-other_eff_split_fac_test[other_eff_split_fac_test!=""]
            # 
            # #Find factors
            # find_fact<-sapply(1:length(other_eff_split_fac_test),function(x) is.factor(data_sub[,other_eff_split_fac_test[x]]))
            
            # if(sum(find_fact)>0){
            #   #Let the model run again, but with new contrasts (important for getting the right p value from type III anova)
            #   add_contr<-paste0(",contrasts=list(",paste0(other_eff_split_fac_test[find_fact],"=contr.sum",collapse=","),")")
            #   
            #   if(grepl("\\|",other_effects_expr)){
            #     function_call_FULL<-paste0("glmer(courtships~",subset_col,other_effects_expr,",family=binomial,data=data_sub",add_contr,")")
            #   } else{
            #     #If no other effects, perform GLM
            #     function_call_FULL<-paste0("glm(courtships~",subset_col,other_effects_expr,",family=binomial,data=data_sub",add_contr,")")
            #   }  
            #   full_MODEL_aov<-eval(parse(text=function_call_FULL))
            #   
            # } else{
            #   full_MODEL_aov<-full_MODEL
            # }
            
            #get the the p value for the subset column (same for both)
            #In case we let both subset columns run in the same model
            #we extract the p value for sub1_col as well as for the interaction terms
            if(sub1_2_in_one_model1){
              #This is not needed if simple LRT test is to be performed
              anova.res<-joint_tests(full_MODEL)
              p_val_glmer1<-anova.res$p.value[anova.res$`model term`==subset_col]
              emmeans_interact<-summary(eval(parse(text=paste0("emmeans(full_MODEL,pairwise ~ ",sub2_col_for_both," | ",subset_col,")"))))
              p_val_glmer2<-cbind(emmeans_interact$contrasts[subset_col],p.value=emmeans_interact$contrasts$p.value)
              p_val_glmer<-list(p_val_glmer1,p_val_glmer2)
            } else{
              if(test_type=="LRT"){
                null_MODEL<-eval_function(function_call_HERE=gsub(subset_col,"1",function_call_FULL),
                                          subset_type=c("sub1_col","sub2_col")[for_error_sub==c("First","Second")],
                                          sub_col_name=subset_col,
                                          sub1_category=if_sub2_sub1_cat,
                                          null_or_full="FULL")
                
                if(null_MODEL$error_text!="No error."){
                  warning(null_MODEL$error_text)
                  p_val_glmer<-NA
                } else{
                  p_val_glmer<-anova(full_MODEL,null_MODEL$model_out,test="LRT")$`Pr(>Chisq)`[2]
                }
                
              } else{
                #This is not needed if simple LRT test is to be performed
                anova.res<-joint_tests(full_MODEL)
                p_val_glmer<-anova.res$p.value[anova.res$`model term`==subset_col]
              }
            }
          } else{
            if(sub1_2_in_one_model1){
              p_val_glmer<-eval(parse(text=paste0("list(NA,data.frame(",subset_col,"=unique(levels(data_sub[,subset_col])[data_sub[,subset_col]]),p.value=rep(NA,length(unique(data_sub[,subset_col])))))")))
            } else{
              p_val_glmer<-NA
            }
          }
          
          if(grepl("\\|",other_effects_expr)){
            # if(!grepl("model could not be run, therefore",null_MODEL$error_text)){
            #   null_MODEL<-null_MODEL$model_out
            #   glmres<-anova(full_MODEL,null_MODEL)
            #   p_val_glmer<-glmres$`Pr(>Chisq)`[length(glmres$`Pr(>Chisq)`)]
            # } else{
            #   null_MODEL<-null_MODEL$model_out
            #   p_val_glmer<-NA
            # }
            overdisp_glmer<-sqrt(sum(c(resid(full_MODEL),full_MODEL@u)^2)/length(resid(full_MODEL)))
          } else{
            # if(!grepl("model could not be run, therefore",null_MODEL$error_text)){
            #   null_MODEL<-null_MODEL$model_out
            #   glmres<-anova(full_MODEL,null_MODEL,test="Chisq")
            #   p_val_glmer<-glmres$`Pr(>Chi)`[length(glmres$`Pr(>Chi)`)]
            # } else{
            #   null_MODEL<-null_MODEL$model_out
            #   p_val_glmer<-NA
            # }
            summo<-summary(full_MODEL)
            overdisp_glmer<-summo$deviance/summo$df.residual
          }
          
          
          # The following task of calculating CIs for the estimators can be done in different ways.
          # a) Fit the model with a coefficient for each combination and use confint.
          # b) predictInterval
          # c) library(broom.mixed); tidy(full_MODEL,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
          # d) se <- sqrt(diag(vcov(full_MODEL)));tab <- cbind(Est = fixef(full_MODEL), LL = fixef(full_MODEL) - 1.96 * se, UL = fixef(full_MODEL) + 1.96 * se); print(exp(tab), digits=3)
          # e) many other possibilities here: https://stackoverflow.com/questions/11072544/how-to-get-coefficients-and-their-confidence-intervals-in-mixed-effects-models
          # f) test = function(fit) fixef(fit); bb <- bootMer(full_MODEL, nsim=5, FUN=test); library("boot"); boot.ci(bb,type="perc")
          # g) super, super nice answer from Ben Bolker https://stats.stackexchange.com/questions/117641/how-trustworthy-are-the-confidence-intervals-for-lmer-objects-through-effects-pa
          # h) similar to f, I think: library(boot); mixed <- glmer(incidence/size ~ period + (1|herd),weights=size, data=cbpp, family=binomial); FUN <- function(fit) { return(fixef(fit)) }; result <- bootMer(mixed, FUN, nsim = 3)
          
          # i) This is lengthy. It is a solution coming from Justin Touchon.
          #Code similar to here: https://www.r-bloggers.com/confidence-intervals-for-prediction-in-glmms/
          #Does not allow multiple fixed effects
          # #Create a blank dataset to fill in with the predictors
          # newdat<-expand.grid(subset_col=levels(data_sub[,subset_col]), courtships=0)
          # #The code below is modified from stuff on the GLMM Wikidot page
          # #The plo and phi objects are your lower and upper confidence intervals
          # mm <- model.matrix(terms(full_MODEL),newdat)
          # if(grepl("\\|",other_effects_expr)){
          #   newdat$courtships <- mm %*% fixef(full_MODEL)
          # } else{
          #   newdat$courtships <- mm %*% full_MODEL$coefficients
          # }
          # pvar1 <- diag(mm %*% tcrossprod(vcov(full_MODEL),mm))
          # newdat <- data.frame(
          #   newdat
          #   , plo = newdat$courtships-1.96*sqrt(pvar1)
          #   , phi = newdat$courtships+1.96*sqrt(pvar1)
          # )
          # CI_est<-newdat
          # CI_est[,2:4] <- mapply(plogis, CI_est[,2:4])
          
          #### The problem with all those is that I didn't figure out an efficient/possible
          #### way to get confidence intervals for different factors.  
          #### THEREFORE....
          
          
          if(type_of_stats!="bayes_glm_glmer"){
            #We use emmeans here, because it is a fast and easy way to get the CIs for the
            #different levels.
            if(verbose){
              print("Estimators and CIs are being calculated.")
            }
            sum_emmeans<-eval(parse(text=paste0("summary(emmeans(full_MODEL,spec='",subset_col,"'))")))
            
            #Get the table
            CI_est<-data.frame(with(sum_emmeans,cbind(emmean,asymp.LCL,asymp.UCL)))
            #Add subset_col categories
            CI_est<-eval(parse(text=paste0("cbind(",subset_col,"=levels(sum_emmeans[,subset_col])[sum_emmeans[,subset_col]],exp(CI_est)/(1+exp(CI_est)))")))
            
            for(i in 1:length(CI_est[1,])){
              if(is.factor(CI_est[,i])){
                CI_est[,i]<-levels(CI_est[,i])[CI_est[,i]]
              }
            }
            
            #Rename, because the current codeworks with the below names
            names(CI_est)<-c(subset_col,"estimator","lower0.025","upper0.975")
            
            #If some selected levels were not found in the data
            #add them with NAs.
            if(dim(CI_est)[1]!=length(subset_vec)){
              not_there<-subset_vec[!subset_vec%in%CI_est[,subset_col]]
              for(put_miss in 1:length(not_there)){
                CI_est<-rbind(CI_est,c(not_there[put_miss],NA,NA,NA))
              }
            }
            CI_est[,2]<-as.numeric(CI_est[,2])
            CI_est[,3]<-as.numeric(CI_est[,3])
            CI_est[,4]<-as.numeric(CI_est[,4])
            
            if(sub1_2_in_one_model1){
              CI_est2<-cbind(data.frame(with(emmeans_interact$emmeans,cbind(emmean,asymp.LCL,asymp.UCL))))
              int1<-eval(parse(text=paste0("levels(emmeans_interact$emmeans$",subset_col,")[emmeans_interact$emmeans$",subset_col,"]")))
              int2<-eval(parse(text=paste0("levels(emmeans_interact$emmeans$",sub2_col_for_both,")[emmeans_interact$emmeans$",sub2_col_for_both,"]")))
              CI_est2<-eval(parse(text=paste0("cbind(",subset_col,"=int1,",sub2_col_for_both,"=int2,CI_est2)")))
              for(i in 1:length(CI_est2[1,])){
                if(is.factor(CI_est2[,i])){
                  CI_est2[,i]<-levels(CI_est2[,i])[CI_est2[,i]]
                }
              }
              
              #Rename, because the current codeworks with the below names
              names(CI_est2)[3:5]<-c("estimator","lower0.025","upper0.975")
              
              #If some selected levels were not found in the data
              #add them with NAs.
              if(dim(CI_est2)[1]!=length(subset_vec)*2){
                not_there<-subset_vec[!subset_vec%in%CI_est2[,subset_col]]
                for(put_miss in 1:length(not_there)){
                  CI_est2<-rbind(CI_est2,c(not_there[put_miss],unique(CI_est2[,2])[1],NA,NA,NA))
                  CI_est2<-rbind(CI_est2,c(not_there[put_miss],unique(CI_est2[,2])[2],NA,NA,NA))
                }
              }
              CI_est2[,3]<-plogis(as.numeric(CI_est2[,3]))
              CI_est2[,4]<-plogis(as.numeric(CI_est2[,4]))
              CI_est2[,5]<-plogis(as.numeric(CI_est2[,5]))
              
              CI_est<-list(sub1_col_alone=CI_est,interact_sub1_sub2_col=CI_est2) 
            }
            
          } else{
            
            #We use sim here, to simulate with flat priors.
            if(verbose){
              print("Estimators and CrIs are being calculated.")
            }
            #Get posterior
            bayes_sim<-sim(full_MODEL,n.sim=nsim_bayes)
            
            #depending on whether we ran a glm or glmer, the synthax of some commands changes...
            if(grepl("\\|",other_effects_expr)){
              colnames(bayes_sim@fixef) <- names(fixef(full_MODEL))
              newdat <- eval(parse(text=paste0("expand.grid(",subset_col,"=levels(data_sub[,subset_col]))")))
              X <- eval(parse(text=paste0("model.matrix(~",subset_col,", data=newdat)")))
              #Get estimators from model. Just take the subset_col estimator
              #and ignore all others.
              newdat$fit <- X%*%fixef(full_MODEL)[1:length(levels(data_sub[,subset_col]))]
              fitmat <- matrix(ncol=nsim_bayes, nrow=nrow(newdat))
              rownames(fitmat)<-levels(newdat[,1])[newdat[,1]]
              #Fill in posterior simulations. Same here. Ignore all other
              #simulated values and just focus on the subset_col estimator.
              for(i in 1:nsim_bayes) fitmat[,i] <- X%*%bayes_sim@fixef[i,1:length(levels(data_sub[,subset_col]))]
              newdat$lower <- apply(fitmat, 1, quantile, prob=0.025)
              newdat$upper <- apply(fitmat, 1, quantile, prob=0.975)
              newdat$fit<-plogis(newdat$fit)
              newdat$lower<-plogis(newdat$lower)
              newdat$upper<-plogis(newdat$upper)
            } else{
              colnames(bayes_sim@coef) <- names(coef(full_MODEL))
              newdat <- eval(parse(text=paste0("expand.grid(",subset_col,"=levels(data_sub[,subset_col]))")))
              X <- eval(parse(text=paste0("model.matrix(~",subset_col,", data=newdat)")))
              #Get estimators from model. Just take the subset_col estimator
              #and ignore all others.
              newdat$fit <- X%*%coef(full_MODEL)[1:length(levels(data_sub[,subset_col]))]
              fitmat <- matrix(ncol=nsim_bayes, nrow=nrow(newdat))
              rownames(fitmat)<-levels(newdat[,1])[newdat[,1]]
              #Fill in posterior simulations. Same here. Ignore all other
              #simulated values and just focus on the subset_col estimator.
              for(i in 1:nsim_bayes) fitmat[,i] <- X%*%bayes_sim@coef[i,1:length(levels(data_sub[,subset_col]))]
              newdat$lower <- apply(fitmat, 1, quantile, prob=0.025)
              newdat$upper <- apply(fitmat, 1, quantile, prob=0.975)
              newdat$fit<-plogis(newdat$fit)
              newdat$lower<-plogis(newdat$lower)
              newdat$upper<-plogis(newdat$upper)
            }
            parameter_post<-fitmat
            rownames(parameter_post)<-levels(newdat[,1])[newdat[,1]]
            CI_est<-data.frame(newdat,stringsAsFactors=F)
            names(CI_est)<-c(subset_col,"estimator","lower0.025","upper0.975")
            
            #If all was ran in one model, we have to get the estimates and CrIs for the interaction terms here
            if(sub1_2_in_one_model1){
              #depending on whether we ran a glm or glmer, the synthax of some commands changes...
              if(grepl("\\|",other_effects_expr)){
                newdat <- eval(parse(text=paste0("expand.grid(",subset_col,"=levels(data_sub[,subset_col]),",sub2_col_for_both,"=levels(data_sub[,sub2_col_for_both]))")))
                X <- eval(parse(text=paste0("model.matrix(~",subset_col,"*",sub2_col_for_both,", data=newdat)")))
                #Get estimators from model. Just take the estimators involved in the interaction
                #and ignore all others.
                newdat$fit <- X%*%fixef(full_MODEL)[sapply(1:ncol(X),function(x) (1:length(fixef(full_MODEL)))[names(fixef(full_MODEL))==colnames(X)[x]])]
                fitmat <- matrix(ncol=nsim_bayes, nrow=nrow(newdat))
                rownames(fitmat)<-paste0(levels(newdat[,1])[newdat[,1]],":::",levels(newdat[,2])[newdat[,2]])
                #Fill in posterior simulations. Same here. Ignore all other
                #simulated values and just take the estimators involved in the interaction.
                for(i in 1:nsim_bayes) fitmat[,i] <- X%*%bayes_sim@fixef[i,sapply(1:ncol(X),function(x) (1:length(fixef(full_MODEL)))[names(fixef(full_MODEL))==colnames(X)[x]])]
                newdat$lower <- apply(fitmat, 1, quantile, prob=0.025)
                newdat$upper <- apply(fitmat, 1, quantile, prob=0.975)
                newdat$fit<-plogis(newdat$fit)
                newdat$lower<-plogis(newdat$lower)
                newdat$upper<-plogis(newdat$upper)
              } else{
                newdat <- eval(parse(text=paste0("expand.grid(",subset_col,"=levels(data_sub[,subset_col]),",sub2_col_for_both,"=levels(data_sub[,sub2_col_for_both]))")))
                X <- eval(parse(text=paste0("model.matrix(~",subset_col,"*",sub2_col_for_both,", data=newdat)")))
                #Get estimators from model. Just take the estimators involved in the interaction
                #and ignore all others.
                newdat$fit <- X%*%coef(full_MODEL)[sapply(1:ncol(X),function(x) (1:length(coef(full_MODEL)))[names(coef(full_MODEL))==colnames(X)[x]])]
                fitmat <- matrix(ncol=nsim_bayes, nrow=nrow(newdat))
                rownames(fitmat)<-paste0(levels(newdat[,1])[newdat[,1]],":::",levels(newdat[,2])[newdat[,2]])
                #Fill in posterior simulations. Same here. Ignore all other
                #simulated values and just take the estimators involved in the interaction.
                for(i in 1:nsim_bayes) fitmat[,i] <- X%*%bayes_sim@coef[i,sapply(1:ncol(X),function(x) (1:length(coef(full_MODEL)))[names(coef(full_MODEL))==colnames(X)[x]])]
                newdat$lower <- apply(fitmat, 1, quantile, prob=0.025)
                newdat$upper <- apply(fitmat, 1, quantile, prob=0.975)
                newdat$fit<-plogis(newdat$fit)
                newdat$lower<-plogis(newdat$lower)
                newdat$upper<-plogis(newdat$upper)
              }
              CI_est2<-data.frame(newdat,stringsAsFactors=F)
              names(CI_est2)[3:5]<-c("estimator","lower0.025","upper0.975")
              parameter_post2<-fitmat
              rownames(parameter_post2)<-paste0(levels(newdat[,1])[newdat[,1]],":::",levels(newdat[,2])[newdat[,2]])
              parameter_post<-list(parameter_post_sub1_col=parameter_post,parameter_post_interact_sub1_sub2_col=parameter_post2)
              CI_est<-list(sub1_col_alone=CI_est,interact_sub1_sub2_col=CI_est2) 
            }
          }
          
          #Reset old contrasts
          options(contrasts = old_contr$contrasts)
          
          
          if(verbose){
            print("--------")
          }
          if(type_of_stats!="bayes_glm_glmer"){
            return(list(FULL_MODEL=full_MODEL,P_VALUE=p_val_glmer,OVERDISP=overdisp_glmer,CI_EST=CI_est,
                        problem_col=problem_col))
          } else{
            return(list(FULL_MODEL=full_MODEL,P_VALUE=p_val_glmer,OVERDISP=overdisp_glmer,CrI_EST=CI_est,
                        problem_col=problem_col,posterior=bayes_sim,parameter_post=parameter_post))
          }
        } else{
          
          #Reset old contrasts
          options(contrasts = old_contr$contrasts)
          
          matNA<-data.frame(Type=subset_vec,estimator=c(NA),lower0.025=c(NA),upper0.975=c(NA))
          if(type_of_stats!="bayes_glm_glmer"){
            return(list(FULL_MODEL=NA,P_VALUE=NA,OVERDISP=NA,CI_EST=matNA,
                        problem_col=problem_col))
          } else{
            return(list(FULL_MODEL=NA,P_VALUE=NA,OVERDISP=NA,CrI_EST=matNA,
                        problem_col=problem_col,posterior=NA,parameter_post=NA))
          }
        }
      } else{
        
        #Reset old contrasts
        options(contrasts = old_contr$contrasts)
        
        matNA<-data.frame(Type=subset_vec,estimator=c(NA),lower0.025=c(NA),upper0.975=c(NA))
        if(type_of_stats!="bayes_glm_glmer"){
          return(list(FULL_MODEL=NA,P_VALUE=NA,OVERDISP=NA,CI_EST=matNA,
                      problem_col=problem_col))
        } else{
          return(list(FULL_MODEL=NA,P_VALUE=NA,OVERDISP=NA,CrI_EST=matNA,
                      problem_col=problem_col,posterior=NA,parameter_post=NA))
        }
      }
      
      #Reset old contrasts
      options(contrasts = old_contr$contrasts)
      
    } else{
      
      
      factor_checko<-sapply(1:ncol(brm_model1$data),function(x) is.factor(brm_model1$data[,x])|is.character(brm_model1$data[,x]))
      
      #Find the fixed effects
      other_eff_split_fac_test<-""
      other_effects_expr<-gsub(" ","",strsplit(as.character(brm_model1$formula[1]),"\\~")[[1]][2])
      if(gsub(" ","",other_effects_expr)!=""){
        other_effects_expr<-gsub(" ","",other_effects_expr)
        other_eff_split_fac_test<-unlist(strsplit(unlist(strsplit(other_effects_expr, "\\+|\\:|\\=|\\}|\\{|\\[|\\]|\\/|\\-|\\&|\\*|\\~")[[1]]),"[\\\\]|[^[:print:]]",fixed=F))
        other_eff_split_fac_test<-trimws(unique(other_eff_split_fac_test[suppressWarnings(is.na(as.numeric(other_eff_split_fac_test)))]))
      }
      
      dropout<-c()
      for(unito in 1:length(other_eff_split_fac_test)){
        if(grepl("\\|",other_eff_split_fac_test[unito])){
          dropout<-c(dropout,unito)
        } else{
          if((!grepl("\\(",other_eff_split_fac_test[unito])&grepl("\\)",other_eff_split_fac_test[unito]))|(grepl("\\(",other_eff_split_fac_test[unito])&!grepl("\\)",other_eff_split_fac_test[unito]))){
            dropout<-c(dropout,unito)
          }
        }
      }
      
      other_eff_split_fac_test<-other_eff_split_fac_test[-dropout]
      
      other_eff_split_fac_test<-c(subset_col,other_eff_split_fac_test[!(grepl("\\(",other_eff_split_fac_test)&grepl("\\)",other_eff_split_fac_test)&grepl("\\|",other_eff_split_fac_test))])
      other_eff_split_fac_test<-gsub("\\(","",other_eff_split_fac_test)
      other_eff_split_fac_test<-gsub("\\)","",other_eff_split_fac_test)
      other_eff_split_fac_test<-gsub("\\|","",other_eff_split_fac_test)
      other_eff_split_fac_test<-other_eff_split_fac_test[other_eff_split_fac_test!=""]
      
      fixed_factors<-other_eff_split_fac_test[factor_checko[1:length(other_eff_split_fac_test)]]
      
      if(length(fixed_factors[fixed_factors!=subset_col])>0){
        test_dummy<-sapply(1:length(fixed_factors[fixed_factors!=subset_col]),function(x) sum(grepl(paste0(fixed_factors[fixed_factors!=subset_col][x],"1"),rownames(fixef(brm_model1))))==0)
        if(sum(test_dummy)>0){
          warning("It seems as if you used contr.treatment() (R default) as dummy coding while you produced your brms model. Since you have more than one fixed effect interpreted as factor, the produced estimators will NOT be leveled over the grand mean of the other factorial fixed effects (which is not what we want). Run the brms model again with e.g. contr.sum() as dummy coding.")
        }
      }
      
      to_take_care<-fixed_factors[fixed_factors!=subset_col]
      
      if(verbose){
        print("Estimators and CIs are being calculated.")
      }
      
      if(length(to_take_care)>0){
        NAset<-eval(parse(text=paste0("data.frame(",paste0(to_take_care,"=NA",collapse=","),")")))
        condi_eff<-conditional_effects(brm_model1,subset_col,
                                       conditions = NAset)[[1]]
      } else{
        condi_eff<-conditional_effects(brm_model1,subset_col)[[1]]
      }
      CI_est<-data.frame(condi_eff$effect1__,condi_eff$estimate__,condi_eff$lower__,condi_eff$upper__,stringsAsFactors = F)
      names(CI_est)<-c(subset_col,"estimator","lower0.025","upper0.975")
      
      if(sub1_2_in_one_model1){
        to_take_care<-fixed_factors[!fixed_factors%in%c(subset_col,sub2_col_for_both)]
        if(length(to_take_care)>0){
          NAset<-eval(parse(text=paste0("data.frame(",paste0(fixed_factors,"=NA",collapse=","),")")))
          condi_eff<-conditional_effects(brm_model1,paste0(subset_col,":",sub2_col_for_both),
                                         conditions = NAset)[[1]]
        } else{
          condi_eff<-conditional_effects(brm_model1,paste0(subset_col,":",sub2_col_for_both))[[1]]
        }
        CI_est2<-data.frame(condi_eff$effect1__,condi_eff$effect2__,condi_eff$estimate__,condi_eff$lower__,condi_eff$upper__,stringsAsFactors = F)
        names(CI_est2)<-c(subset_col,sub2_col_for_both,"estimator","lower0.025","upper0.975")
        CI_est<-list(sub1_col_alone=CI_est,interact_sub1_sub2_col=CI_est2) 
      }
      
      if(sub1_2_in_one_model){
        return(list(FULL_MODEL=brm_model1,
                    P_VALUE=eval(parse(text=paste0("list(NA,data.frame(",subset_col,"=unique(levels(CI_est$interact_sub1_sub2_col$type)[CI_est$interact_sub1_sub2_col$type]),p.value=rep(NA,length(unique(levels(CI_est$interact_sub1_sub2_col$type)[CI_est$interact_sub1_sub2_col$type])))))"))),
                    OVERDISP=NA,CrI_EST=CI_est,
                    problem_col=NA,posterior=posterior_samples(brm_model1),
                    parameter_post="Please produce these yourself for your brmsfit model (brms has good predict-functions that can do this)."))
      } else{
        return(list(FULL_MODEL=brm_model1,P_VALUE=NA,OVERDISP=NA,CrI_EST=CI_est,
                    problem_col=NA,posterior=posterior_samples(brm_model1),
                    parameter_post="Please produce these yourself for your brmsfit model (brms has good predict-functions that can do this)."))
      }
      
    }
    
  }
  
  
  #For performing binomial statistics, and producing the same
  #output structure as above function.
  binom_return<-function(sub_states,sub_col,input_tfinal){
    cont.table<-as.data.frame(matrix(rep(NA,2*length(sub_states)),nrow=2))
    row.names(cont.table)<-c("Court_spec_1","Court_spec_2")
    colnames(cont.table)<-sub_states
    for(bin_num in 1:length(sub_states)){
      if(length(response_col[[1]])>1){
        cont.table[1,bin_num]<-sum(rowSums(input_tfinal[,response_col[[1]]])[input_tfinal[,sub_col]==sub_states[bin_num]])
        cont.table[2,bin_num]<-sum(rowSums(input_tfinal[,response_col[[2]]])[input_tfinal[,sub_col]==sub_states[bin_num]])
      } else{
        cont.table[1,bin_num]<-sum((input_tfinal[,response_col[[1]]])[input_tfinal[,sub_col]==sub_states[bin_num]])
        cont.table[2,bin_num]<-sum((input_tfinal[,response_col[[2]]])[input_tfinal[,sub_col]==sub_states[bin_num]])
      }
    }
    
    CI_est<-as.data.frame(matrix(rep(NA,4*length(sub_states)),nrow=length(sub_states)))
    CI_est[,1]<-sub_states
    colnames(CI_est)<-c("sub1_type","Mean","CI_neg","CI_pos")
    
    for(bin_num in 1:length(sub_states)){
      if(sum(cont.table[,bin_num])==0){
        CI_est[bin_num,2:4]<-c(NA,NA,NA)
      } else{
        CI_est[bin_num,2:4]<-as.numeric(binom.confint(cont.table[1,bin_num],sum(cont.table[,bin_num]),
                                                      method="exact")[1,4:6])
      }
    }
    returner<-list()
    if(sum(colSums(cont.table)>0)==1){
      returner[[1]]<-("No Statistics could be performed, since only 1 group in subset 1.")
      returner[[2]]<-(cont.table)
      stat.test<-NA
    } else{
      if(sum(colSums(cont.table)>0)==2){
        returner[[1]]<-("Simple prop.test performed. CIs are calculated with binom.confint, method=exact.")
        returner[[2]]<-(cont.table)
        stat.test<-prop.test(c(cont.table[,colSums(cont.table)>0][1,1],
                               cont.table[,colSums(cont.table)>0][1,2]),
                             c(colSums(cont.table[,colSums(cont.table)>0][,1:2])))$p.value
      } else{
        stat.test<-chisq.test(cont.table[,colSums(cont.table)>0])$p.value
        returner[[1]]<-"Simple chisq.test performed on contingency table. CIs are calculated with binom.confint, method=exact."
        returner[[2]]<-cont.table
      }
    }
    return(list(FULL_MODEL=returner,
                P_VALUE=stat.test,OVERDISP=NA,CI_EST=CI_est))
  }
  
  
  
  #For significance codes and predictor/CI bars
  sign_coder<-function(values=0,
                       lino=0,ticki=0,where=0,
                       bars_where=0,
                       split_axis=F,split_ID=2,
                       conf=T,
                       dist_center=0.18,
                       width_mean_bar=0.5,
                       width_error_sub1=0.2,
                       thickness_est_CI1=5){
    
    if(!sum(is.na(values[[4]][,3]))==nrow(values[[4]])){
      
      #If there is only two groups in first subset, plot significance, otherwise don't
      ftest<-values[[2]][1]
      # axis(3,at=mean(where)-dist_center,lwd=0,
      #      line=lino-c(0.8,1.2,1.2,1.2)[c(ftest>0.05,ftest<=0.05&ftest>0.01,
      #                                     ftest<=0.01&ftest>0.001,
      #                                     ftest<=0.001)],
      #      c("ns","*","**","***")[c(ftest>0.05,ftest<=0.05&ftest>0.01,
      #                               ftest<=0.01&ftest>0.001,
      #                               ftest<=0.001)],
      #      font=c(1,2,2,2)[c(ftest>0.05,ftest<=0.05&ftest>0.01,
      #                        ftest<=0.01&ftest>0.001,
      #                        ftest<=0.001)],
      #      cex.axis=c(0.8,1.2,1.2,1.2)[c(ftest>0.05,ftest<=0.05&ftest>0.01,
      #                                    ftest<=0.01&ftest>0.001,
      #                                    ftest<=0.001)])
      # if(round(ftest,3)!=0){
      # axis(3,at=mean(where)+dist_center,lwd=0,
      #      line=lino-0.8,
      #      paste0("(p = ",round(ftest,3),")"),
      #      font=1,
      #      cex.axis=0.8)
      # } else{
      #   axis(3,at=mean(where)+dist_center,lwd=0,
      #        line=lino-0.8,
      #        paste0("(p < 0.001)"),
      #        font=1,
      #        cex.axis=0.8)
      #   
      # }
      
      if(round(ftest,3)!=0){
        axis(3,at=mean(where),lwd=0,
             line=lino-0.8,
             paste0(c("ns","*","**","***")[c(ftest>0.05,ftest<=0.05&ftest>0.01,
                                             ftest<=0.01&ftest>0.001,
                                             ftest<=0.001)],
                    "   ","(p = ",round(ftest,3),")"),
             font=1,
             cex.axis=0.8)
      } else{
        axis(3,at=mean(where),lwd=0,
             line=lino-0.8,
             paste0(c("ns","*","**","***")[c(ftest>0.05,ftest<=0.05&ftest>0.01,
                                             ftest<=0.01&ftest>0.001,
                                             ftest<=0.001)],
                    "   ","(p < 0.001)"),
             font=1,
             cex.axis=0.8)
        
      }
      
      if(split_axis){
        axis(3,at=where,labels=F,line=lino,
             tck=0.5*ticki,
             lwd=2)
        axis(3,at=where[3-split_ID],labels=F,line=lino,
             tck=ticki,
             lwd=2)
        axis(3,at=c(where[split_ID]-0.3,where[split_ID]+0.3),labels=F,line=lino-0.3,
             tck=0.5*ticki,
             lwd=2)
      } else{
        axis(3,at=where,labels=F,line=lino,tck=ticki,lwd=2)
      }
      
      
      #add estimator+CI if conf is TRUE
      if(conf){
        segments(bars_where-(width_mean_bar/2),
                 as.numeric(values[[4]][,2]),
                 bars_where+(width_mean_bar/2),
                 as.numeric(values[[4]][,2]),
                 col="white",lwd=thickness_est_CI1,lend=3)
        arrows(bars_where,
               as.numeric(values[[4]][,3]),
               bars_where,
               as.numeric(values[[4]][,4]),
               col="white",lwd=0.71*thickness_est_CI1,length=width_error_sub1,code=3,angle=90,lend=3)
        
        
        segments(bars_where-(width_mean_bar/2),
                 as.numeric(values[[4]][,2]),
                 bars_where+(width_mean_bar/2),
                 as.numeric(values[[4]][,2]),
                 col="black",lwd=0.57*thickness_est_CI1,lend=3)
        
        arrows(bars_where,
               as.numeric(values[[4]][,3]),
               bars_where,
               as.numeric(values[[4]][,4]),
               col="black",lwd=0.29*thickness_est_CI1,length=width_error_sub1,code=3,angle=90,lend=3)
        
      }
    }
  }
  
  
  #local_jitter: jittering of points only where necessary to avoid overlap. 
  #Creates jittered stripcharts that resemble violin plots (you see the density of points)
  #(may also remind a bit of "stack" method in stripchart).
  
  #Arguments:
  #fact_coord:
  #coordinates on factoriasl axis (i.e. this is the axis the should get jittered).
  #gradual_coord: 
  #This is the axis that does not get jittered.
  #factorial_axis:
  #Type 1 if the x-axis is the factorial axis and 2 if the y-axis is.
  #categories
  #This is a vector of two different types, e.g. a vector of male/female or species1/species2.
  #Once this argument is defined, the points will be split in each category and plotted on the two different sides 
  #of the categorial position. The first state that appears in the vector will become the
  #bottom (if y factorial) or the left (if x factorial) distribution.
  #sizes:
  #The size of your points in cex. YOU HAVE TO USE PCH=1 or PCH=21 in your plot/points
  #execution OR modify your sizes argument (see above).
  #center_space_rel:
  #Only of relevance if categories is provided. Space left empty on each side in axis units.
  #buffer:
  #distance away from that center that can maximally be jittered (one-sided distance)
  #distance_fac:
  #cex units of distance between two dots.
  #sd_p:
  #Units of buffer that are used as sd from which new coordinates for factorial axis are drawn. High buffer
  #can have positive and negative effects. It may allow points to not get stuck in local optima, but more iterations
  #may in total be needed and distribution may by chance not be equally distributed over both sides of center anymore.
  #Gets multiplied by 0.5 if categories are given.
  #iterations:
  #How many times we run the simulation.
  #start_seed:
  #The seed the function uses for randomization.
  #priority_space:
  #1 or 2. If set to 1, avoiding overlap has always priority over avoiding distance from center.
  #Setting this to 1 is better in situations where there are regions of points that cannot be alligned in the
  #buffer area without any overlap. If that is the case, priority_space=2 can result in points getting clumped
  #in center in that regions. But priority_space=2 may need less iterations to find the optimum state if 
  #There are no over-dense areas.
  #penalty_over:
  #Penalty for overlap. The higher the value in comparison to the penalty for distance to center, 
  #the more the simulation tries to avoid overlap. Only of effect if priority_space=2.
  #penalty_cen:
  #Same as before, but penalty for distance from center. Only of relevance if priority_space==2,
  #or if priority_space=1 and it itself = 0.
  #lwd:
  #line width of point characters. Values below 1 may mess up the distances, depending
  #on the plotting device you use.
  
  local_jitter<-function(fact_coord,gradual_coord,factorial_axis,buffer,
                         categories=NA,sizes=rep(1,length(fact_coord)),
                         center_space_rel=0,distance_fac=0.1,sd_p=1,iterations=100,start_seed=as.numeric(Sys.time()),
                         priority_space=1,penalty_over=1,penalty_cen=1,
                         verbose=T,lwd=1){
    
    #Get rid of NAs.
    if(length(categories)==1&sum(is.na(categories))>0){
      fact_coord_new<-fact_coord[is.na(fact_coord)+is.na(gradual_coord)+
                                   is.na(sizes)==0]
      gradual_coord_new<-gradual_coord[is.na(fact_coord)+is.na(gradual_coord)+
                                         is.na(sizes)==0]
      sizes_new<-sizes[is.na(fact_coord)+is.na(gradual_coord)+
                         is.na(sizes)==0]
      fact_coord<-fact_coord_new
      gradual_coord<-gradual_coord_new
      sizes<-sizes_new
    } else{
      fact_coord_new<-fact_coord[is.na(fact_coord)+is.na(gradual_coord)+
                                   is.na(sizes)+is.na(categories)==0]
      gradual_coord_new<-gradual_coord[is.na(fact_coord)+is.na(gradual_coord)+
                                         is.na(sizes)+is.na(categories)==0]
      sizes_new<-sizes[is.na(fact_coord)+is.na(gradual_coord)+
                         is.na(sizes)+is.na(categories)==0]
      categories_new<-categories[is.na(fact_coord)+is.na(gradual_coord)+
                                   is.na(sizes)+is.na(categories)==0]
      fact_coord<-fact_coord_new
      gradual_coord<-gradual_coord_new
      sizes<-sizes_new
      categories<-categories_new
    }
    
    #This function calculates the overlap of two circles in a Cartesian coordinate system.
    #From https://stackoverflow.com/questions/44437793/how-to-calculate-the-intersection-area-of-two-circles-in-shiny-or-r-code
    #Person didn't comment on the script.
    circle_intersection <- function(x1, y1, r1, x2, y2, r2){
      rr1 <- r1 * r1
      rr2 <- r2 * r2
      d <- sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
      
      if (d > r2 + r1) # Circles do not overlap
      {
        return(0)
      } else if (d <= abs(r1 - r2) && r1 >= r2){ # Circle2 is completely inside circle1  
        return(pi*rr2)
      } else if (d <= abs(r1 - r2) && r1 < r2){ # Circle1 is completely inside circle2
        return(pi*rr1)
      } else { # Circles partially overlap
        phi <- (acos((rr1 + (d * d) - rr2) / (2 * r1 * d))) * 2
        theta <- (acos((rr2 + (d * d) - rr1) / (2 * r2 * d))) * 2
        area2 <- 0.5 * theta * rr2 - 0.5 * rr2 * sin(theta)
        area1 <- 0.5 * phi * rr1 - 0.5 * rr1 * sin(phi)
        return(area1 + area1)
      }
    }
    
    #Make categories not a factor
    if(is.factor(categories)){
      categories<-levels(categories)[categories]
    }
    
    #Retrieve current setting for cex (changes with e.g. mfrow) and correct sizes
    curr_cex<-par("cex")
    sizes<-sizes*curr_cex
    
    #Depending on which axis is factorial
    #Get xlims and ylims
    #AND
    #As from https://stat.ethz.ch/pipermail/r-help/2003-July/035796.html
    #see also: https://www.rstudio.com/wp-content/uploads/2016/10/how-big-is-your-graph.pdf
    #Get radii in factorial direction. Relativize in x direction later
    #AND
    #Scale all gradual to factorial
    if(factorial_axis==1){
      #range of x and y axis
      fact_dist<-abs(par("usr")[1]-par("usr")[2])
      grad_dist<-abs(par("usr")[3]-par("usr")[4])
      
      #cin is character size in inches
      #Relativize up-down extent of points by width size of plot and
      #multiply by extent of x axis
      fact_rel<-(par("cin")[2]/par("pin")[1])*(par("usr")[2]-par("usr")[1])
      
      #THIS CALCULATES THE RADIUS IN X DIMENSION OF EACH DOT
      #(sizes+0.5*distance_fac) is the radius of the dot+the cex space that should exist
      #around it
      #0.375 is a specific conversion factor for pch 1 and 21
      #see more here: https://stat.ethz.ch/pipermail/r-help/2003-July/035796.html
      #In the end, add lwd
      fact_rad_distance_all<-(fact_rel*0.5*(sizes+0.5*distance_fac)*0.375)+((0.5*lwd*(1/96))/par("pin")[1])*fact_dist
      
      grad_dist_new<-fact_dist*(par("pin")[2]/par("pin")[1])
      gradual_coord_rel<-(abs(gradual_coord-par("usr")[3])/grad_dist)*grad_dist_new
      
    } else{
      fact_dist<-abs(par("usr")[3]-par("usr")[4])
      grad_dist<-abs(par("usr")[1]-par("usr")[2])
      
      #Same as par("cxy")[2]
      fact_rel<-(par("cin")[2]/par("pin")[2])*(par("usr")[4]-par("usr")[3])
      fact_rad_distance_all<-fact_rel*0.5*(sizes+0.5*distance_fac)*0.375+((0.5*lwd*(1/96))/par("pin")[2])*fact_dist
      
      grad_dist_new<-fact_dist*(par("pin")[1]/par("pin")[2])
      gradual_coord_rel<-(abs(gradual_coord-par("usr")[1])/grad_dist)*grad_dist_new
    }
    
    #Get unique coordinates of factorial axis
    uni_fact_coord<-unique(fact_coord)
    
    #This is needed to print percentage of runs performed
    every_it<-round(seq(0,length(uni_fact_coord)*iterations,(length(uni_fact_coord)*iterations)/100))[-1]
    
    #The smallest possible number on this PC
    smallest_pos_number<-.Machine$double.xmin
    
    #Get unique categories (if NA, this becomes also NA, but doesn't matter)
    uni_cat<-unique(categories)
    
    if(verbose){
      #Function begins
      print("start")
    }
    
    #Set seed
    set.seed(start_seed)
    
    #Function runs through all unique stati of the factorial axis
    for(uni_sub in 1:length(uni_fact_coord)){
      #current subset
      curr_sub<-uni_fact_coord[uni_sub]
      #Subset to the current factor
      
      #CHANGED
      
      ID_which<-(1:length(fact_coord))[fact_coord==curr_sub]
      fact_coord_s<-fact_coord[ID_which]
      gradual_coord_s<-gradual_coord[ID_which]
      sizes_s<-sizes[ID_which]
      fact_rad_distance_all_s<-fact_rad_distance_all[ID_which]
      gradual_coord_s_rel<-gradual_coord_rel[ID_which]
      if(sum(is.na(categories))==0){
        categories_s<-categories[ID_which]
      }
      #Sort by size (helps later)
      NEW_ID<-order(sizes_s,decreasing=T)
      fact_coord_s<-fact_coord_s[NEW_ID]
      gradual_coord_s<-gradual_coord_s[NEW_ID]
      sizes_s<-sizes_s[NEW_ID]
      fact_rad_distance_all_s<-fact_rad_distance_all_s[NEW_ID]
      gradual_coord_s_rel<-gradual_coord_s_rel[NEW_ID]
      if(sum(is.na(categories))==0){
        categories_s<-categories_s[NEW_ID]
      }
      ID_which<-ID_which[NEW_ID]
      
      
      #Get length of current subset
      length_s<-length(gradual_coord_s)
      
      #Get minimum and maximum of distributions
      unique_s_minb<-curr_sub-buffer
      unique_s_plub<-curr_sub+buffer
      
      #FIRST find out which circles actually potentially overlap with each other
      OVERLAP_LIST<-vector("list",length_s) 
      
      for(poin in 1:length_s){
        
        for(compo in 1:length_s){
          
          #Depending on whether x or y is factorial
          if(factorial_axis==1){
            #Calculate new overlap with other point.
            OVERLAP_LIST[[poin]]<-c(OVERLAP_LIST[[poin]],circle_intersection(fact_coord_s[poin],gradual_coord_s_rel[poin],fact_rad_distance_all_s[poin],
                                                                             fact_coord_s[compo],gradual_coord_s_rel[compo],fact_rad_distance_all_s[compo]))
          } else{
            #Calculate new overlap with other point.
            OVERLAP_LIST[[poin]]<-c(OVERLAP_LIST[[poin]],circle_intersection(gradual_coord_s_rel[poin],fact_coord_s[poin],fact_rad_distance_all_s[poin],
                                                                             gradual_coord_s_rel[compo],fact_coord_s[compo],fact_rad_distance_all_s[compo]))
          }
        }
        OVERLAP_LIST[[poin]][poin]<-0
      }
      
      #If we have categories, remove overlaps with dots that are not in same
      #category
      if(sum(is.na(categories))==0){
        if(length(unique(categories_s))>1){
          unicat<-unique(categories_s)
          cat1<-(1:length_s)[categories_s==unicat[1]]
          cat2<-(1:length_s)[categories_s==unicat[2]]
          cat_list<-list(cat1,cat2)
          for(over_rem in 1:length_s){
            OVERLAP_LIST[[over_rem]][cat_list[[(1:2)[!c(over_rem%in%cat_list[[1]],over_rem%in%cat_list[[2]])]]]]<-0
          }
        }
      }
      
      overlap_dots<-!sapply(1:length_s,function(x) sum(OVERLAP_LIST[[x]])==0) 
      
      if(sum(overlap_dots)>0){
        
        #Distribute points  over range.
        #Depending on whether we have different categories.
        #If not.
        if(sum(is.na(categories))>0){
          #Do a first distribution of points
          
          if(length_s!=1){
            #Distribute them gradually according to their sizes.
            mult_list<-rep(list(c(-1,1),c(1,-1))[[sample(c(1,2),1)]],ceiling(sum(overlap_dots)/2))
            if(priority_space==1){
              #If space always has priority, then we can sample here from the whole range.
              
              #New
              buffer_pos<-sapply(1:sum(overlap_dots),function(x) c(buffer,-buffer)[mult_list[1:sum(overlap_dots)][x]==c(-1,1)])
              fact_coord_s[overlap_dots]<-curr_sub+buffer_pos+mult_list[1:sum(overlap_dots)]*fact_rad_distance_all_s[overlap_dots]
              #help bigger ones to come closer to center
              #fix the coming-closer to 50% of their possible range
              max_rad<-max(fact_rad_distance_all_s[overlap_dots])
              fact_coord_s[overlap_dots]<-fact_coord_s[overlap_dots]+mult_list[1:sum(overlap_dots)]*abs(fact_coord_s[overlap_dots]-(curr_sub-mult_list[1:sum(overlap_dots)]*fact_rad_distance_all_s[overlap_dots]))*(((fact_rad_distance_all_s[overlap_dots]/max_rad))^3)*0.66
              
              #Newer old
              #fact_coord_s[overlap_dots]<-curr_sub-mult_list[1:sum(overlap_dots)][order(sizes_s[overlap_dots])]*max(fact_rad_distance_all_s[overlap_dots])
              #fact_coord_s[overlap_dots]<-fact_coord_s[overlap_dots]-mult_list[1:sum(overlap_dots)][order(sizes_s[overlap_dots])]*rev(seq(0,buffer-2*max(fact_rad_distance_all_s[overlap_dots]),(buffer-2*max(fact_rad_distance_all_s[overlap_dots]))/(sum(overlap_dots)-1)))[order(sizes_s[overlap_dots])]
              
              #OLD
              #fact_coord_s[overlap_dots][order(sizes_s[overlap_dots])]<-curr_sub-mult_list[1:sum(overlap_dots)]*rev(seq(max(fact_rad_distance_all_s[overlap_dots]),buffer-max(fact_rad_distance_all_s[overlap_dots]),(buffer-2*max(fact_rad_distance_all_s[overlap_dots]))/(sum(overlap_dots)-1)))
            } else{
              #New
              buffer_pos<-sapply(1:sum(overlap_dots),function(x) c(buffer,-buffer)[mult_list[1:sum(overlap_dots)][x]==c(-1,1)])
              fact_coord_s[overlap_dots]<-curr_sub+buffer_pos+mult_list[1:sum(overlap_dots)]*fact_rad_distance_all_s[overlap_dots]
              #help bigger ones to come closer to center
              #fix the coming-closer to 50% of their possible range
              max_rad<-max(fact_rad_distance_all_s[overlap_dots])
              fact_coord_s[overlap_dots]<-fact_coord_s[overlap_dots]+mult_list[1:sum(overlap_dots)]*abs(fact_coord_s[overlap_dots]-(curr_sub-mult_list[1:sum(overlap_dots)]*fact_rad_distance_all_s[overlap_dots]))*(((fact_rad_distance_all_s[overlap_dots]/max_rad))^3)*0.66
              
              #OLD
              #If not, it is better to give a little space from center, so that points are not already caught in optima.
              #fact_coord_s[overlap_dots][order(sizes_s[overlap_dots])]<-rev(curr_sub-mult_list[1:sum(overlap_dots)]*seq(max(fact_rad_distance_all_s[overlap_dots],0.1*buffer),buffer-max(fact_rad_distance_all_s[overlap_dots]),(buffer-max(fact_rad_distance_all_s[overlap_dots])-max(fact_rad_distance_all_s[overlap_dots],0.1*buffer))/(sum(overlap_dots)-1)))
            }
          }
        } else{
          
          #Calculate space from center of biggest dot to center such that it still keeps
          #the rule.
          center_space<-center_space_rel+max(fact_rad_distance_all_s)
          
          #For later: get vector of two types
          neg_cat<-(1:length_s)[categories_s==uni_cat[1]]
          pos_cat<-(1:length_s)[categories_s==uni_cat[2]]
          
          #Distribute them gradually according to their sizes
          for(cato in 1:2){
            current<-fact_coord_s[get(c("neg_cat","pos_cat")[cato])]
            if(length(current)>0&sum(is.na(current))==0){
              mult_list<-rep(c((-1),1)[cato],length(current))
              if(length(current)>1){
                if(priority_space==1){
                  #If space always has priority, then we can sample here from the whole range.
                  
                  #NEW
                  current<-curr_sub-c(1,-1)[cato]*buffer+c(1,-1)[cato]*fact_rad_distance_all_s[get(c("neg_cat","pos_cat")[cato])]
                  #help bigger ones to come closer to center
                  #fix the coming-closer to 50% of their possible range
                  max_rad<-max(fact_rad_distance_all_s[get(c("neg_cat","pos_cat")[cato])])
                  current<-current+c(1,-1)[cato]*abs(current-(curr_sub-c(1,-1)[cato]*fact_rad_distance_all_s[get(c("neg_cat","pos_cat")[cato])]))*(((fact_rad_distance_all_s[get(c("neg_cat","pos_cat")[cato])]/max_rad))^3)*0.66
                  
                  #OLD
                  #current[order(sizes_s[get(c("neg_cat","pos_cat")[cato])])]<-rev(curr_sub+mult_list*seq(center_space,buffer-max(fact_rad_distance_all_s),(buffer-center_space-max(fact_rad_distance_all_s))/(length(current)-1)))
                } else{
                  #NEW
                  current<-curr_sub-c(1,-1)[cato]*buffer+c(1,-1)[cato]*fact_rad_distance_all_s[get(c("neg_cat","pos_cat")[cato])]
                  #help bigger ones to come closer to center
                  #fix the coming-closer to 50% of their possible range
                  max_rad<-max(fact_rad_distance_all_s[get(c("neg_cat","pos_cat")[cato])])
                  current<-current+c(1,-1)[cato]*abs(current-(curr_sub-c(1,-1)[cato]*fact_rad_distance_all_s[get(c("neg_cat","pos_cat")[cato])]))*(((fact_rad_distance_all_s[get(c("neg_cat","pos_cat")[cato])]/max_rad))^3)*0.66
                  
                  #OLD
                  #If not, it is better to give a little space from center, so that points are not already caught in optima.
                  #current[order(sizes_s[get(c("neg_cat","pos_cat")[cato])])]<-rev(curr_sub+mult_list*seq(max(center_space,0.1*buffer),buffer-max(fact_rad_distance_all_s),(buffer-max(center_space,0.1*buffer)-max(fact_rad_distance_all_s))/(length(current)-1)))
                }
                fact_coord_s[get(c("neg_cat","pos_cat")[cato])]<-current
              } else{
                #NEW
                current<-curr_sub-c(1,-1)[cato]*buffer+c(1,-1)[cato]*fact_rad_distance_all_s[get(c("neg_cat","pos_cat")[cato])]
                #OLD
                #fact_coord_s[get(c("neg_cat","pos_cat")[cato])]<-curr_sub+mult_list*max(center_space,0.1*buffer)
              }
            }
          }
          
          #Put those without overlap into minimum position
          if(sum(overlap_dots)>0){
            fact_coord_s<-sapply(1:length(overlap_dots),function(x) ifelse(!overlap_dots[x],curr_sub+(center_space_rel+fact_rad_distance_all_s[x])*(-c(1,-1)[c(x%in%neg_cat,x%in%pos_cat)]),fact_coord_s[x]))
          }
        }
        
        ###NEW
        #bring them closer to the center
        
        
        
        if(iterations>0){
          #Repeat this iterations times
          for(iter in 1:iterations){
            #for the case that we don't differentiate by categories
            if(sum(is.na(categories))>0){
              #Go through points that have potential overlaps
              for(poin in (1:length_s)[overlap_dots]){
                #Get new coordinate for point on the factorial axis. 
                #Do this with a truncated normal distribution with sd = sd_p*buffer.
                new_poin<-rtruncnorm(1,unique_s_minb+fact_rad_distance_all_s[poin],unique_s_plub-fact_rad_distance_all_s[poin],mean=fact_coord_s[poin],sd=sd_p*buffer)
                old_over<-0
                new_over<-0
                
                if(sum(OVERLAP_LIST[[poin]][-poin])>0){
                  #Go through all points where potential overlap occurs.
                  for(compo in (1:length_s)[-poin][OVERLAP_LIST[[poin]][-poin]>0]){
                    #print(paste0(iter," ",poin," ",compo))
                    
                    #Depending on whether x or y is factorial
                    if(factorial_axis==1){
                      #Calculate new overlap with other point.
                      new_over<-new_over+circle_intersection(new_poin,gradual_coord_s_rel[poin],fact_rad_distance_all_s[poin],
                                                             fact_coord_s[compo],gradual_coord_s_rel[compo],fact_rad_distance_all_s[compo])
                      #Calculate old overlap with other point.
                      old_over<-old_over+circle_intersection(fact_coord_s[poin],gradual_coord_s_rel[poin],fact_rad_distance_all_s[poin],
                                                             fact_coord_s[compo],gradual_coord_s_rel[compo],fact_rad_distance_all_s[compo])
                      
                      #old_dist<-sqrt(((abs(fact_coord_s[poin]-fact_coord_s[compo])/fact_dist)*(par("pin")[1]/dev.size()[1])*x_y_rat)^2+((abs(gradual_coord_s[poin]-gradual_coord_s[compo])/grad_dist)*(par("pin")[2]/dev.size()[2]))^2)
                    } else{
                      #Calculate new overlap with other point.
                      new_over<-new_over+circle_intersection(gradual_coord_s_rel[poin],new_poin,fact_rad_distance_all_s[poin],
                                                             gradual_coord_s_rel[compo],fact_coord_s[compo],fact_rad_distance_all_s[compo])
                      #Calculate old overlap with other point.
                      old_over<-old_over+circle_intersection(gradual_coord_s_rel[poin],fact_coord_s[poin],fact_rad_distance_all_s[poin],
                                                             gradual_coord_s_rel[compo],fact_coord_s[compo],fact_rad_distance_all_s[compo])
                    }
                  }
                }
                
                #Calculate the percentage of change. Make sure nothing is divided by 0.
                #Take this to the power of the penalty factor.
                over_perc<-(max(new_over,smallest_pos_number)/max(old_over,smallest_pos_number))^penalty_over
                
                #If less overlap, accept. If no change in overlap, accept if center distance did 
                #not decrease.
                if(priority_space==1){
                  if(log(over_perc)<0){
                    fact_coord_s[poin]<-new_poin
                  } else{
                    if(log(over_perc)==0){
                      #Same with distance to center
                      cent_perc<-(max(abs(new_poin-curr_sub),smallest_pos_number)/max(abs(fact_coord_s[poin]-curr_sub),smallest_pos_number))^penalty_cen
                      if(log(cent_perc)<=0){
                        fact_coord_s[poin]<-new_poin
                      }
                    }
                  }
                } else{
                  #If total percentage is bigger than 0, accept new coordinate
                  cent_perc<-(max(abs(new_poin-curr_sub),smallest_pos_number)/max(abs(fact_coord_s[poin]-curr_sub),smallest_pos_number))^penalty_cen
                  if(sum(log(cent_perc),log(over_perc))<=0){
                    fact_coord_s[poin]<-new_poin
                  }
                }
              }
              
            } else{
              #Go through two categories
              for(cato in 1:2){
                current<-get(c("neg_cat","pos_cat")[cato])
                #Only take those that have overlap
                current<-current[current%in%(1:length_s)[overlap_dots]]
                
                if(length(current)>0&sum(is.na(current))==0){
                  
                  #Go through points
                  for(poin in current){
                    
                    #Sample either out of the range from one side until middle or from the middle to the other side.
                    #sd gets multiplied times 0.5
                    new_poin<-rtruncnorm(1,c(unique_s_minb+fact_rad_distance_all_s[poin],curr_sub+center_space_rel+fact_rad_distance_all_s[poin])[cato],c(curr_sub-center_space_rel-fact_rad_distance_all_s[poin],unique_s_plub-fact_rad_distance_all_s[poin])[cato],mean=fact_coord_s[poin],sd=sd_p*0.5*buffer)
                    old_over<-0
                    new_over<-0
                    #Go through all other points out of this side of distribution which
                    #may overlap with the current point.
                    #Therefore, this goes much faster than without categories.
                    
                    if(length(current[current!=poin][OVERLAP_LIST[[poin]][current[current!=poin]]>0])>0){
                      for(compo in current[current!=poin][OVERLAP_LIST[[poin]][current[current!=poin]]>0]){
                        
                        #Depending on whether x or y is factorial
                        if(factorial_axis==1){
                          #Calculate new overlap with other point.
                          new_over<-new_over+circle_intersection(new_poin,gradual_coord_s_rel[poin],fact_rad_distance_all_s[poin],
                                                                 fact_coord_s[compo],gradual_coord_s_rel[compo],fact_rad_distance_all_s[compo])
                          #Calculate old overlap with other point.
                          old_over<-old_over+circle_intersection(fact_coord_s[poin],gradual_coord_s_rel[poin],fact_rad_distance_all_s[poin],
                                                                 fact_coord_s[compo],gradual_coord_s_rel[compo],fact_rad_distance_all_s[compo])
                          
                        } else{
                          #Calculate new overlap with other point.
                          new_over<-new_over+circle_intersection(gradual_coord_s_rel[poin],new_poin,fact_rad_distance_all_s[poin],
                                                                 gradual_coord_s_rel[compo],fact_coord_s[compo],fact_rad_distance_all_s[compo])
                          #Calculate old overlap with other point.
                          old_over<-old_over+circle_intersection(gradual_coord_s_rel[poin],fact_coord_s[poin],fact_rad_distance_all_s[poin],
                                                                 gradual_coord_s_rel[compo],fact_coord_s[compo],fact_rad_distance_all_s[compo])
                        }
                      }
                    }
                    #Calculate the percentage of change. Make sure nothing is divided by 0.
                    #Take this to the power of the penalty factor.
                    over_perc<-(max(new_over,smallest_pos_number)/max(old_over,smallest_pos_number))^penalty_over
                    
                    #If less overlap, accept. If no change in overlap, accept if center distance did 
                    #not decrease.
                    if(priority_space==1){
                      if(log(over_perc)<0){
                        fact_coord_s[poin]<-new_poin
                      } else{
                        if(log(over_perc)==0){
                          #Same with distance to center
                          cent_perc<-(max(abs(new_poin-curr_sub),smallest_pos_number)/max(abs(fact_coord_s[poin]-curr_sub),smallest_pos_number))^penalty_cen
                          if(log(cent_perc)<=0){
                            fact_coord_s[poin]<-new_poin
                          }
                        }
                      }
                    } else{
                      #If total percentage is bigger than 0, accept new coordinate
                      cent_perc<-(max(abs(new_poin-curr_sub),smallest_pos_number)/max(abs(fact_coord_s[poin]-curr_sub),smallest_pos_number))^penalty_cen
                      if(sum(log(cent_perc),log(over_perc))<=0){
                        fact_coord_s[poin]<-new_poin
                      }
                    }
                  }
                }
              }
            }
            
            if(sum(iter+(uni_sub-1)*iterations==every_it)>0){
              #Inform how far we are
              if(verbose==T){
                print(paste0((1:length(every_it))[iter+(uni_sub-1)*iterations==every_it]," %"))
              }
            }
          }
        }
        #Bring into same order again:
        fact_coord_s<-fact_coord_s[order(ID_which)]
        #Pass changes to vector.
        fact_coord[fact_coord==curr_sub]<-fact_coord_s
      } else{
        #If we have categories:
        #If no overlap within categories, move them at least to their
        #category-specific position
        if(sum(is.na(categories))==0){
          #Calculate space from center of biggest dot to center such that it still keeps
          #the rule.
          center_space<-center_space_rel+max(fact_rad_distance_all_s)
          
          #For later: get vector of two types
          neg_cat<-(1:length_s)[categories_s==uni_cat[1]]
          pos_cat<-(1:length_s)[categories_s==uni_cat[2]]
          fact_coord_s<-sapply(1:length(overlap_dots),function(x) ifelse(!overlap_dots[x],curr_sub+(center_space_rel+fact_rad_distance_all_s[x])*(-c(1,-1)[c(x%in%neg_cat,x%in%pos_cat)]),fact_coord_s[x]))
          #Bring into same order again:
          fact_coord_s<-fact_coord_s[order(ID_which)]
          #Pass changes to vector.
          fact_coord[fact_coord==curr_sub]<-fact_coord_s
        }
      }
    }
    #Return vector.
    return(fact_coord)
  }
  
  
  #This is the first part of the jitter function. This function will be used
  #to evaluate the jitter function.
  
  local_jitter_eval<-function(fact_coord,gradual_coord,factorial_axis,buffer,
                              categories=NA,sizes=rep(1,length(fact_coord)),
                              center_space_rel=0,distance_fac=0.1,sd_p=1,iterations=100,start_seed=as.numeric(Sys.time()),
                              priority_space=1,penalty_over=1,penalty_cen=1,
                              verbose=T,lwd=1){
    
    #Get rid of NAs.
    if(length(categories)==1&sum(is.na(categories))>0){
      fact_coord_new<-fact_coord[is.na(fact_coord)+is.na(gradual_coord)+
                                   is.na(sizes)==0]
      gradual_coord_new<-gradual_coord[is.na(fact_coord)+is.na(gradual_coord)+
                                         is.na(sizes)==0]
      sizes_new<-sizes[is.na(fact_coord)+is.na(gradual_coord)+
                         is.na(sizes)==0]
      fact_coord<-fact_coord_new
      gradual_coord<-gradual_coord_new
      sizes<-sizes_new
    } else{
      fact_coord_new<-fact_coord[is.na(fact_coord)+is.na(gradual_coord)+
                                   is.na(sizes)+is.na(categories)==0]
      gradual_coord_new<-gradual_coord[is.na(fact_coord)+is.na(gradual_coord)+
                                         is.na(sizes)+is.na(categories)==0]
      sizes_new<-sizes[is.na(fact_coord)+is.na(gradual_coord)+
                         is.na(sizes)+is.na(categories)==0]
      categories_new<-categories[is.na(fact_coord)+is.na(gradual_coord)+
                                   is.na(sizes)+is.na(categories)==0]
      fact_coord<-fact_coord_new
      gradual_coord<-gradual_coord_new
      sizes<-sizes_new
      categories<-categories_new
    }
    
    #This function calculates the overlap of two circles in a Cartesian coordinate system.
    #From https://stackoverflow.com/questions/44437793/how-to-calculate-the-intersection-area-of-two-circles-in-shiny-or-r-code
    #Person didn't comment on the script.
    circle_intersection <- function(x1, y1, r1, x2, y2, r2){
      rr1 <- r1 * r1
      rr2 <- r2 * r2
      d <- sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
      
      if (d > r2 + r1) # Circles do not overlap
      {
        return(0)
      } else if (d <= abs(r1 - r2) && r1 >= r2){ # Circle2 is completely inside circle1  
        return(pi*rr2)
      } else if (d <= abs(r1 - r2) && r1 < r2){ # Circle1 is completely inside circle2
        return(pi*rr1)
      } else { # Circles partially overlap
        phi <- (acos((rr1 + (d * d) - rr2) / (2 * r1 * d))) * 2
        theta <- (acos((rr2 + (d * d) - rr1) / (2 * r2 * d))) * 2
        area2 <- 0.5 * theta * rr2 - 0.5 * rr2 * sin(theta)
        area1 <- 0.5 * phi * rr1 - 0.5 * rr1 * sin(phi)
        return(area1 + area1)
      }
    }
    
    #Make categories not a factor
    if(is.factor(categories)){
      categories<-levels(categories)[categories]
    }
    
    #Retrieve current setting for cex (changes with e.g. mfrow) and correct sizes
    curr_cex<-par("cex")
    sizes<-sizes*curr_cex
    
    #Depending on which axis is factorial
    #Get xlims and ylims
    #AND
    #As from https://stat.ethz.ch/pipermail/r-help/2003-July/035796.html
    #see also: https://www.rstudio.com/wp-content/uploads/2016/10/how-big-is-your-graph.pdf
    #Get radii in factorial direction. Relativize in x direction later
    #AND
    #Scale all gradual to factorial
    if(factorial_axis==1){
      #range of x and y axis
      fact_dist<-abs(par("usr")[1]-par("usr")[2])
      grad_dist<-abs(par("usr")[3]-par("usr")[4])
      
      #cin is character size in inches
      #Relativize up-down extent of points by width size of plot and
      #multiply by extent of x axis
      fact_rel<-(par("cin")[2]/par("pin")[1])*(par("usr")[2]-par("usr")[1])
      
      #THIS CALCULATES THE RADIUS IN X DIMENSION OF EACH DOT
      #(sizes+0.5*distance_fac) is the radius of the dot+the cex space that should exist
      #around it
      #0.375 is a specific conversion factor for pch 1 and 21
      #see more here: https://stat.ethz.ch/pipermail/r-help/2003-July/035796.html
      #In the end, add lwd
      fact_rad_distance_all<-(fact_rel*0.5*(sizes+0.5*distance_fac)*0.375)+((0.5*lwd*(1/96))/par("pin")[1])*fact_dist
      
      grad_dist_new<-fact_dist*(par("pin")[2]/par("pin")[1])
      gradual_coord_rel<-(abs(gradual_coord-par("usr")[3])/grad_dist)*grad_dist_new
      
    } else{
      fact_dist<-abs(par("usr")[3]-par("usr")[4])
      grad_dist<-abs(par("usr")[1]-par("usr")[2])
      
      #Same as par("cxy")[2]
      fact_rel<-(par("cin")[2]/par("pin")[2])*(par("usr")[4]-par("usr")[3])
      fact_rad_distance_all<-fact_rel*0.5*(sizes+0.5*distance_fac)*0.375+((0.5*lwd*(1/96))/par("pin")[2])*fact_dist
      
      grad_dist_new<-fact_dist*(par("pin")[1]/par("pin")[2])
      gradual_coord_rel<-(abs(gradual_coord-par("usr")[1])/grad_dist)*grad_dist_new
    }
    return(fact_rad_distance_all)
  }
  
  
  #######################################################
  #Finally, the actual function starts
  
  
  
  #Calculate appearance of all individuals in table
  appearance_all<-rowSums(tfinal[,unlist(response_col)])
  #Make a new subset of table with the types we are interested in
  #and only including individuals that showed responses.
  #Leaving 0s does not make sense in a binomial approach.
  tfinal<-tfinal[tfinal[,sub1_col]%in%sub1_states&appearance_all>0,]
  
  #If there is no data, give error.
  #If there is not at least two types in the first subsetting, give warning.
  if(length(unique(tfinal[,sub1_col]))==0){
    stop("No data point matches any category in subset 1.")
  }
  if(length(unique(tfinal[,sub1_col]))<2){
    if(verbose){
      print("Subset 1 did not result in at least two groups. No statistical test for first subset to be performed.")
    }
  }
  
  #Find out if we can save the table summed by sum_by_col_plot 
  #(which we will produce below) for stats. If we don't 
  #do binomial stats and have other_effects not included in 
  #sum_by_col_plot, we cannot use the reduced table.
  if(length(sum_by_col_plot)==0){
    sum_also_for_GLMM<-F
  } else{
    sum_also_for_GLMM<-T
  }
  
  if(type_of_stats!="freq_binom"){
    if(length(sum_by_col_plot)!=0&((check_sub2_states&stats_for_2sub)|stats_for_1sub)){
      if(check_sub2_states&stats_for_2sub&stats_for_1sub){
        check_randomn<-gsub(" ","",paste0(other_effects_sub1,other_effects_sub2))
        effects1<-T
        effects2<-T
      } else{
        if(stats_for_1sub){
          check_randomn<-gsub(" ","",other_effects_sub1)
          effects1<-T
          effects2<-F
        } else{
          if(check_sub2_states&stats_for_2sub){
            check_randomn<-gsub(" ","",other_effects_sub2)
            effects1<-F
            effects2<-T
          }
        }
      }
      if(check_randomn!=""){
        other_eff_split<-c()
        if(effects1){
          if(gsub(" ","",other_effects_sub1)!=""){
            other_eff_split<-unlist(strsplit(unlist(strsplit(strsplit(other_effects_sub1, "\\+|\\(|\\)|\\:|\\=|\\}|\\{|\\[|\\]|\\/|\\-|\\&|\\*|\\~")[[1]],"\\|")),"[\\\\]|[^[:print:]]",fixed=F))
            other_eff_split<-trimws(unique(other_eff_split[suppressWarnings(is.na(as.numeric(other_eff_split)))]))
          }
        }
        other_eff_split2<-c()
        if(effects2){
          if(gsub(" ","",other_effects_sub2)!=""){
            other_eff_split2<-unlist(strsplit(unlist(strsplit(strsplit(other_effects_sub2, "\\+|\\(|\\)|\\:|\\=|\\}|\\{|\\[|\\]|\\/|\\-|\\&|\\*|\\~")[[1]],"\\|")),"[\\\\]|[^[:print:]]",fixed=F))
            other_eff_split2<-trimws(unique(other_eff_split2[suppressWarnings(is.na(as.numeric(other_eff_split2)))]))
          }
        }
        other_eff_split<-unique(c(other_eff_split,other_eff_split2))
        
        coln_split<-unlist(strsplit(unlist(strsplit(unlist(strsplit(names(tfinal), "\\+|\\(|\\)|\\:|\\=|\\}|\\{|\\[|\\]|\\/|\\-|\\&|\\*|\\~")),"\\|")),"[\\\\]|[^[:print:]]",fixed=F))
        if(length(coln_split)!=length(names(tfinal))&!already_warned){
          warning("One or more of your column names contain unusual characters, which may cause the function to be slightly slower (or even do wrong things in a very unlikely scenario). Please try not to include the following characters in your column names: +():=}{[]/-&*~|")
        }
        if(sum(other_eff_split%in%trimws(sum_by_col_plot))!=length(other_eff_split)){
          sum_also_for_GLMM<-F
        }
      }
    }
  }
  
  #If GLMM/GLM dataset cannot be summed by the sum_by_col_plot,
  #save it here
  if(!sum_also_for_GLMM|!simplify_for_model){
    tfinal_stats<-tfinal
  }
  
  #Unite different rows by a certain identifier column for plotting,
  #if chosen.
  if(length(sum_by_col_plot)>0){
    all_unique<-as.data.frame(unique(tfinal[,sum_by_col_plot]))
    for(i in 1:length(all_unique[1,])){
      if(is.factor(all_unique[,i])){
        all_unique[,i]<-levels(all_unique[,i])[all_unique[,i]]
      }
    }
    all_unique_paste<-as.vector(apply(all_unique,1,paste,collapse="-"))
    tfinal_paste<-as.vector(apply(tfinal[,sum_by_col_plot,drop=F],1,paste,collapse="-"))
    new_responses<-matrix(rep(NA,NROW(all_unique)*length(unlist(response_col))),ncol=length(unlist(response_col)))
    for(new_res in 1:length(unlist(response_col))){
      new_responses[,new_res]<-sapply(1:NROW(all_unique),function(x) sum(tfinal[tfinal_paste==all_unique_paste[x],unlist(response_col)[new_res]]))
    }
    new_responses<-as.data.frame(new_responses)
    names(new_responses)<-unlist(response_col)
    
    other_cols<-names(tfinal)[!names(tfinal)%in%unique(c(sum_by_col_plot,unlist(response_col)))]
    if(length(other_cols)>0){
      other_col_mat<-matrix(rep(NA,NROW(all_unique)*length(other_cols)),ncol=length(other_cols))
      for(get_new in 1:length(other_cols)){
        other_col_mat[,get_new]<-sapply(1:NROW(all_unique),function(x) tfinal[tfinal_paste==all_unique_paste[x],other_cols[get_new]][1])
      }
      other_col_mat<-as.data.frame(other_col_mat)
      names(other_col_mat)<-other_cols
      tfinal_new<-data.frame(new_responses,other_col_mat)
    } else{
      tfinal_new<-data.frame(new_responses)
    }
    for(add_colo in length(sum_by_col_plot):1){
      tfinal_new<-data.frame(NEW=all_unique[,add_colo],tfinal_new)
      names(tfinal_new)[1]<-sum_by_col_plot[add_colo]
    }
    tfinal<-tfinal_new
    #transform everything to character that is factor
    for(i in 1:length(tfinal[1,])){
      if(is.factor(tfinal[,i])){
        tfinal[,i]<-levels(tfinal[,i])[tfinal[,i]]
      }
    }
  }
  
  #Save processed table for stats if we either do binomial stats, don't have other
  #effects or if all other effects included in sum_by_col_plot
  if(sum_also_for_GLMM){
    if((length(sub1_col)>0&stats_for_1sub)|(length(sub2_col)>0&stats_for_2sub)){
      if(type_of_stats=="freq_binom"){
        print("sum_by_col_plot was also applied to table which will be passed to the statistical analyses since you set type_of_stats to 'freq_binom'.")
        tfinal_stats<-tfinal
      } else{
        if(type_of_stats!="bayes_own_brm"&simplify_for_model){
          print("sum_by_col_plot was also applied to table which will be passed to the statistical analyses since none of the columns in your other_effects are missing from sum_by_col_plot. This will save time during modeling.")
          tfinal_stats<-tfinal
        }
      }
    }
  }
  
  #Calculate appearance and preference
  appearance_DUMMY<-rowSums(tfinal[,unlist(response_col)])
  if(length(response_col[[1]])>1){
    preference_DUMMY<-rowSums(tfinal[,response_col[[1]]])/appearance_DUMMY
  } else{
    preference_DUMMY<-tfinal[,response_col[[1]]]/appearance_DUMMY
  }
  
  #Position on either left or right side of graph.
  positioning<-tfinal[,sub1_col]
  positioning_back_up<-tfinal[,sub1_col]
  if(!skip_sub1_panel_weights&length(sub1_states)>1){
    for(pos_c in 1:length(sub1_states)){
      positioning[positioning==sub1_states[pos_c]]<-sum(sub1_panel_weights[1:pos_c])-0.5*sub1_panel_weights[pos_c]
      positioning_back_up[positioning_back_up==sub1_states[pos_c]]<-pos_c
    }
  } else{
    for(pos_c in 1:length(sub1_states)){
      positioning[positioning==sub1_states[pos_c]]<-pos_c-0.5
      positioning_back_up[positioning_back_up==sub1_states[pos_c]]<-pos_c
    }
  }
  positioning<-as.numeric(positioning)
  positioning_back_up<-as.numeric(positioning_back_up)
  
  #In case second subset is wished, this gives the positioning
  #(left right) in either of the plot segments.
  if(!check_sub2_states){
    positioning_sub2<-rep(NA,nrow(tfinal))
  } else{
    if(length(sub2_col)==1){
      positioning_sub2<-tfinal[,sub2_col]
    } else{
      positioning_sub2<-rep(NA,nrow(tfinal))
      for(col_spec_enter in 1:length(sub2_col)){
        if(!is.na(sub2_col[col_spec_enter])){
          positioning_sub2[tfinal[,sub1_col]==sub1_states[col_spec_enter]]<-tfinal[tfinal[,sub1_col]==sub1_states[col_spec_enter],sub2_col[col_spec_enter]]
        }
      }
    }
    #To avoid that numeric entries may mess all up, make them character strings
    pos_sub2_temp<-positioning_sub2
    positioning_sub2<-paste0("remove_this_temp_",positioning_sub2)
    for(pos2_put in 1:length(sub1_states)){
      positioning_sub2[tfinal[,sub1_col]==sub1_states[pos2_put]][pos_sub2_temp[tfinal[,sub1_col]==sub1_states[pos2_put]]==sub2_states[[pos2_put]][1]]<-1
      positioning_sub2[tfinal[,sub1_col]==sub1_states[pos2_put]][pos_sub2_temp[tfinal[,sub1_col]==sub1_states[pos2_put]]==sub2_states[[pos2_put]][2]]<-2
    }
    positioning_sub2[suppressWarnings(is.na(as.numeric(positioning_sub2)))]<-NA
    positioning_sub2<-as.numeric(positioning_sub2)
  }
  
  if(!only_stats){
    #Get a vector of colour for the dots
    if(!is.list(dot_colours)){
      #If colours from column
      if(colour_as_columns){
        dot_colourss<-tfinal[,dot_colours]
      } else{
        dot_colourss<-rep(dot_colours[1],nrow(tfinal))
      }
    } else{
      dot_colourss<-c()
      for(colo in 1:nrow(tfinal)){
        if(length(dot_colours[[positioning_back_up[colo]]])==2&length(sub2_col)>0){
          dot_colourss<-c(dot_colourss,dot_colours[[positioning_back_up[colo]]][positioning_sub2[colo]])
        } else{
          dot_colourss<-c(dot_colourss,dot_colours[[positioning_back_up[colo]]][1])
        }
      }
    }
  } else{
    dot_colourss<-rep(NA,nrow(tfinal))  
  }
  
  
  #Add to table.
  tfinal<-cbind(tfinal,positioning,positioning_sub2,dot_colourss,appearance_DUMMY,preference_DUMMY,positioning_back_up)
  
  
  #######################################################
  ####GLMM (or other stats, GLM, binomial stats)
  
  
  #Perform statistics on subset 1 data
  
  #First check two_sub1_cols_for_stats
  no_test1<-F
  if(!skip_error&stats_for_1sub){
    if(sum(two_sub1_cols_for_stats%in%tfinal[,sub1_col])!=2&!"THIS_IS_A_DUMMY_COL"%in%names(tfinal)){
      warning("Not all two_sub1_cols_for_stats appear in your sub1_col. Therefore no test focussing on two sub1_states can be performed.")
      no_test1<-T
    } else{
      no_test1<-F
    }
  }
  
  #Make testo empty, in case no stats will be performed
  testo<-list()
  
  #Set seed for Bayesian simulations (if chosen)
  if(type_of_stats=="bayes_glm_glmer"){
    no_seed<-F
    if(missing(bayes_seedi)){
      no_seed<-T
    } else{
      if(length(bayes_seedi)==0){
        no_seed<-T
      } else{
        if(length(bayes_seedi)==1){
          if(is.vector(bayes_seedi)&!is.list(bayes_seedi)){
            if(is.na(bayes_seedi)){
              no_seed<-T
            } else{
              if(is.character(bayes_seedi)){
                if(gsub(" ","",bayes_seedi)==""){
                  no_seed<-T
                }
              }
            }
          }
        }
      }
    }
    
    if(!no_seed){
      if(length(bayes_seedi)!=1){
        stop("bayes_seedi has to be a numeric vector of size 1.")
      }
      if(!is.vector(bayes_seedi)|is.list(bayes_seedi)){
        stop("bayes_seedi has to be a numeric vector of size 1.")
      }
      if(!is.numeric(bayes_seedi)){
        stop("bayes_seedi has to be a numeric vector of size 1.")
      }
      #set.seed
      seed_set<-T
      set.seed(bayes_seedi)
      if(verbose){
        print(paste0("Seed of ",bayes_seedi," was set before simulation of posteriors."))
      }
      
      if(verbose){
        print("--------")
      }
      
    }
    
  }
  
  #For warning notifier from glmm function
  warning_col_glmm<-c()
  
  #If stats on first subset should be performed
  if(stats_for_1sub){
    
    #Either simple binomial
    if(type_of_stats=="freq_binom"){
      testo<-list(FIRST_SUB_TEST=binom_return(sub_states=sub1_states,sub_col=sub1_col,input_tfinal=tfinal[tfinal[,sub1_col]%in%two_sub1_cols_for_stats,]))
    } else{
      
      #If model should run separately on sub1_col
      if(!sub1_2_in_one_model){
        
        #or GLMM/GLM
        if(length(unique(tfinal$positioning))>1&!no_test1){
          testo<-list(FIRST_SUB_TEST=glm_glmer(data1=tfinal_stats[tfinal_stats[,sub1_col]%in%two_sub1_cols_for_stats,],
                                               court_cols_spec1=response_col[[1]],
                                               court_cols_spec2=response_col[[2]],
                                               subset_col=sub1_col,
                                               subset_vec=sub1_states,
                                               other_effects_expr=other_effects_sub1,
                                               dont_make_factor_glmm1=dont_make_factor_glmm,
                                               warning_col_glmm1=warning_col_glmm,
                                               for_error_sub="First",
                                               if_sub2_sub1_cat=c(),
                                               verbose=verbose,
                                               scale_center1=scale_center,
                                               perform_GLM_GLMM=perform_GLM_GLMM,
                                               brm_model1=brm_model[[1]],
                                               test_type=test_type))
          warning_col_glmm<-testo$FIRST_SUB_TEST$problem_col
          testo$FIRST_SUB_TEST<-testo$FIRST_SUB_TEST[-(1:length(testo$FIRST_SUB_TEST))[names(testo$FIRST_SUB_TEST)=="problem_col"]]
        } else{
          empt_tabl<-as.data.frame(matrix(rep(NA,4*length(sub1_states)),nrow=length(sub1_states)))
          empt_tabl[,1]<-sub1_states
          colnames(empt_tabl)<-c(sub1_col,"estimator","lower0.025","upper0.975")
          if(type_of_stats%in%c("bayes_glm_glmer","bayes_own_brm")){
            testo<-list(FIRST_SUB_TEST=list(FULL_MODEL=NA,P_VALUE=NA,OVERDISP=NA,CrI_EST=data.frame(empt_tabl),posterior=NA,parameter_post=NA))
          } else{
            testo<-list(FIRST_SUB_TEST=list(FULL_MODEL=NA,P_VALUE=NA,OVERDISP=NA,CI_EST=data.frame(empt_tabl)))
          }
        }
        
        #If both sub_cols should be run in same model with interaction
      } else{
        if(length(unique(tfinal$positioning))>1&length(unique(tfinal$positioning_sub2))>1&!no_test1){
          testo<-list(INTERACTION_TEST=glm_glmer(data1=tfinal_stats[tfinal_stats[,sub1_col]%in%two_sub1_cols_for_stats,],
                                                 court_cols_spec1=response_col[[1]],
                                                 court_cols_spec2=response_col[[2]],
                                                 subset_col=sub1_col,
                                                 subset_vec=sub1_states,
                                                 other_effects_expr=other_effects_sub1,
                                                 dont_make_factor_glmm1=dont_make_factor_glmm,
                                                 warning_col_glmm1=warning_col_glmm,
                                                 for_error_sub="First",
                                                 if_sub2_sub1_cat=c(),
                                                 verbose=verbose,
                                                 scale_center1=scale_center,
                                                 sub1_2_in_one_model1=T,
                                                 sub2_col_for_both=sub2_col,
                                                 perform_GLM_GLMM=perform_GLM_GLMM,
                                                 brm_model1=brm_model[[1]],
                                                 test_type=test_type))
          warning_col_glmm<-testo$INTERACTION_TEST$problem_col
          testo$INTERACTION_TEST<-testo$INTERACTION_TEST[-(1:length(testo$INTERACTION_TEST))[names(testo$INTERACTION_TEST)=="problem_col"]]
        } else{
          empt_tabl<-as.data.frame(matrix(rep(NA,4*length(sub1_states)),nrow=length(sub1_states)))
          empt_tabl[,1]<-sub1_states
          colnames(empt_tabl)<-c(sub1_col,"estimator","lower0.025","upper0.975")
          empt_tabl2<-as.data.frame(matrix(rep(NA,5*2*length(sub1_states)),nrow=length(sub1_states)*2))
          empt_tabl2[,1]<-rep(sub1_states,each=2)
          empt_tabl2[,2]<-rep(unique(unlist(sub2_states)),length(sub1_states))
          colnames(empt_tabl2)<-c(sub1_col,sub2_col,"estimator","lower0.025","upper0.975")
          if(type_of_stats%in%c("bayes_glm_glmer","bayes_own_brm")){
            testo<-list(INTERACTION_TEST=list(FULL_MODEL=NA,P_VALUE=NA,OVERDISP=NA,CrI_EST=list(data.frame(empt_tabl),data.frame(empt_tabl2)),posterior=NA,parameter_post=NA))
          } else{
            testo<-list(INTERACTION_TEST=list(FULL_MODEL=NA,P_VALUE=list(NA,NA),OVERDISP=NA,CI_EST=list(data.frame(empt_tabl),data.frame(empt_tabl2))))
          }
        }
      }
      
    }
    
  }
  #Warn user:
  if(length(unique(tfinal$positioning))!=length(sub1_states)){
    warning("There is no data for at least one of the sub1_states.")
  }
  
  
  if(check_sub2_states){
    #How many categories do we have in the second subset
    subtest<-sapply(1:length(sub2_states),function(x) length(sub2_states[[x]]))
  } else{
    subtest<-rep(1,length(sub1_states))
  }
  #Actually existing subcategories
  true_exist<-sapply(1:length(sub1_states),function(x) length(unique(tfinal$positioning_sub2[tfinal$positioning_back_up==x])))
  
  #If GLMM/GLM on second subset should be performed, do this.
  if(stats_for_2sub){
    
    if(check_sub2_states&sum(subtest>1)>0){
      
      #Give warning if the second subsetting failed in some group
      if(sum(sapply(1:length(sub1_states),function(x) true_exist[x]!=2&subtest[x]==2))!=0){
        warning("Subset 2 did not result in the desired number of groups in at least one of the levels of subset 1.")
      }
      
      if(!sub1_2_in_one_model){
        for(sub2_GLMM in 1:length(sub1_states)){
          if(subtest[sub2_GLMM]==2&true_exist[sub2_GLMM]==2){
            if(!is.na(ifelse(length(sub2_col)>1,sub2_col[sub2_GLMM],sub2_col))){
              #If we want to perform simple binomial stats
              if(type_of_stats=="freq_binom"){
                testo[[length(testo)+1]]<-binom_return(sub_states=sub2_states[[sub2_GLMM]],sub_col=ifelse(length(sub2_col)>1,sub2_col[sub2_GLMM],sub2_col),input_tfinal=tfinal[tfinal[,sub1_col]==sub1_states[sub2_GLMM],])
                names(testo)[length(testo)]<-paste0("SEC_SUB_TEST_GROUP_",sub1_states[sub2_GLMM])
              } else{
                #for glmm/glm
                testo[[length(testo)+1]]<-glm_glmer(data1=tfinal_stats[tfinal_stats[,sub1_col]==sub1_states[sub2_GLMM],],
                                                    court_cols_spec1=response_col[[1]],
                                                    court_cols_spec2=response_col[[2]],
                                                    subset_col=ifelse(length(sub2_col)>1,sub2_col[sub2_GLMM],sub2_col),
                                                    subset_vec=sub2_states[[sub2_GLMM]],
                                                    other_effects_expr=other_effects_sub2,
                                                    dont_make_factor_glmm1=dont_make_factor_glmm,
                                                    warning_col_glmm1=warning_col_glmm,
                                                    for_error_sub="Second",
                                                    if_sub2_sub1_cat=c(sub1_states[sub2_GLMM]),
                                                    verbose=verbose,
                                                    scale_center1=scale_center,
                                                    perform_GLM_GLMM=perform_GLM_GLMM,
                                                    brm_model1=brm_model[[1+sub2_GLMM]],
                                                    test_type=test_type)
                warning_col_glmm<-unique(c(warning_col_glmm,testo[[length(testo)]]$problem_col))
                testo[[length(testo)]]<-testo[[length(testo)]][-(1:length(testo[[length(testo)]]))[names(testo[[length(testo)]])=="problem_col"]]
                names(testo)[length(testo)]<-paste0("SEC_SUB_TEST_GROUP_",sub1_states[sub2_GLMM])
              }
            }
          }
        }
      }
    }
  }
  
  #Now sort first by first subset, then second subset.
  #This may help for later plot aesthetics, if the function is
  #rerun on a data set that includes (part of) the data used here.
  # I removed this random unsorting because I think it only causes problems...
  # tfinal<-tfinal[sample(1:nrow(tfinal)),]
  tfinal<-tfinal[order(tfinal$positioning,tfinal$positioning_sub2),]
  
  #Calculate reaction norms, if this was wished
  reaction_norm<-F
  if(check_sub2_states){
    if(length(reaction_norm_col)>0){
      if(reaction_norm_max_trans>0){
        if(length((1:length(sub2_states))[true_exist>1])>0){
          IDs_frame<-data.frame(ID=c(),BIN=c(),starty=c(),endy=c(),weight1=c(),weight2=c())
          names(IDs_frame)<-c()
          problems_dupl<-F
          for(sub_12_states in (1:length(sub2_states))[true_exist>1]){
            sub_table_resp<-tfinal[tfinal[,sub1_col]==sub1_states[sub_12_states],]
            unique_dupl<-sub_table_resp[,reaction_norm_col][duplicated(sub_table_resp[,reaction_norm_col])]
            if(length(unique_dupl)==0){
              stop(paste0("No reaction norm line could be drawn for your sub1_state ",sub1_states[sub_12_states],". Every value in your reaction_norm_col only gets passed as one row to the function. Check your settings for sum_by_col_plot. If you sum up by the same column as your reaction_norm_col, then this is what causes this error. Consider adding your sub2_col as element to the vector sum_by_col_plot."))
            }
            entries_sub2<-list()
            for(enterer in 1:length(unique_dupl)){
              entries_sub2[[length(entries_sub2)+1]]<-sub_table_resp[,ifelse(length(sub2_col)>1,sub2_col[sub_12_states],sub2_col)][sub_table_resp[,reaction_norm_col]==unique_dupl[enterer]]
            }
            dupl_states2_check<-sapply(1:length(entries_sub2),function(x) sum(duplicated(entries_sub2[[x]])))
            exist_in_sub2_states<-sapply(1:length(entries_sub2),function(x) sum(!entries_sub2[[x]]%in%sub2_states[[sub_12_states]]))
            if(sum(sapply(1:length(unique_dupl),function(x) sum(unique_dupl==unique_dupl[x])>1|dupl_states2_check[x]>0))>0){
              stop("During reaction norm plotting, ann error occurred. The same ID in reaction_norm_col can only appear two times for each sub1_state and both appearances must have a different value in the respective sub2_col.")
            }
            if(sum(exist_in_sub2_states)>0){
              stop("Some of the different states in sub2_col of a respective sub1_state do not match sub2_states.")
            }
            for(line_filler in (1:length(unique_dupl))[lengths(entries_sub2)>1]){
              IDs_frame<-rbind(IDs_frame,matrix(c(unique_dupl[line_filler],sub1_pos_in_plot[sub_12_states],
                                                  sub_table_resp$preference_DUMMY[sub_table_resp[,reaction_norm_col]==unique_dupl[line_filler]&sub_table_resp[,ifelse(length(sub2_col)>1,sub2_col[sub_12_states],sub2_col)]==sub2_states[[sub_12_states]][1]],
                                                  sub_table_resp$preference_DUMMY[sub_table_resp[,reaction_norm_col]==unique_dupl[line_filler]&sub_table_resp[,ifelse(length(sub2_col)>1,sub2_col[sub_12_states],sub2_col)]==sub2_states[[sub_12_states]][2]],
                                                  sub_table_resp$appearance_DUMMY[sub_table_resp[,reaction_norm_col]==unique_dupl[line_filler]&sub_table_resp[,ifelse(length(sub2_col)>1,sub2_col[sub_12_states],sub2_col)]==sub2_states[[sub_12_states]][1]],
                                                  sub_table_resp$appearance_DUMMY[sub_table_resp[,reaction_norm_col]==unique_dupl[line_filler]&sub_table_resp[,ifelse(length(sub2_col)>1,sub2_col[sub_12_states],sub2_col)]==sub2_states[[sub_12_states]][2]]),
                                                nrow=1))
            }
          }
          if(nrow(IDs_frame)>0){
            names(IDs_frame)<-c("ID","BIN","starty","endy","weight1","weight2")
            for(i in 1:length(IDs_frame[1,])){
              if(is.factor(IDs_frame[,i])){
                IDs_frame[,i]<-levels(IDs_frame[,i])[IDs_frame[,i]]
              }
            }
            IDs_frame[,2]<-as.numeric(IDs_frame[,2])
            IDs_frame[,3]<-as.numeric(IDs_frame[,3])
            IDs_frame[,4]<-as.numeric(IDs_frame[,4])
            IDs_frame[,5]<-as.numeric(IDs_frame[,5])
            IDs_frame[,6]<-as.numeric(IDs_frame[,6])
            
            #weights
            if(reaction_norm_weights==1){
              IDs_frame$weight<-rep(reaction_norm_max_trans,nrow(IDs_frame))
            } else{
              if(reaction_norm_weights==2){
                IDs_frame$weight<-reaction_norm_max_trans*((IDs_frame$weight1+IDs_frame$weight2)/max((IDs_frame$weight1+IDs_frame$weight2)))
              } else{
                IDs_frame$weight<-sapply(1:nrow(IDs_frame),function(x) mean(c(diff(as.numeric(binom.confint(as.vector(IDs_frame$weight1)[x]*IDs_frame$starty[x],as.vector(IDs_frame$weight1)[x],
                                                                                                            method="exact")[5:6])),
                                                                              diff(as.numeric(binom.confint(as.vector(IDs_frame$weight2)[x]*IDs_frame$endy[x],as.vector(IDs_frame$weight2)[x],
                                                                                                            method="exact")[5:6])))))
                IDs_frame$weight<-reaction_norm_max_trans*((1/IDs_frame$weight)/max(1/IDs_frame$weight))
              }
            }
            reaction_norm<-T
          } 
        } else{
          if(verbose){
            print("No reaction norm could be set because there is never dots of two different sub2_col categories available.")
          }
        }
      }
    }
  }
  
  
  
  
  #######################################################
  #PLOT
  
  
  #If plotting is wished, produce the plot.
  if(!only_stats){
    
    if(verbose){
      print("Begin plotting.")
    }
    
    #Setting up the graph
    
    #Make decisions on dimensions
    #Decision for side 1 has to do with subset 2.
    #Decisions for side 3 have to do with the number of subset 1
    #and subset 2 states.
    if(check_sub2_states){
      dim1_dec<-sum(is.na(unlist(sub2_labels)))!=length(unlist(sub2_labels))&!(sum(is.na(unlist(sub2_labels)))==1&length(unlist(sub2_labels))==1)&sum(subtest>1)>0
    } else{
      dim1_dec<-F
    }
    dim3_dec1<-!show_p_values_plot[1]|!stats_for_1sub|type_of_stats%in%c("bayes_glm_glmer","bayes_own_brm")
    if("FIRST_SUB_TEST"%in%names(testo)){
      if(is.na(unlist(testo$FIRST_SUB_TEST$P_VALUE))>0){
        dim3_dec1<-T
      }
    }
    if("INTERACTION_TEST"%in%names(testo)){
      if(sum(is.na(unlist(testo$INTERACTION_TEST$P_VALUE)))>0){
        dim3_dec1<-T
      }
    }
    dim3_dec2<-stats_for_2sub&check_sub2_states&sum(true_exist>1)>0&show_p_values_plot[2]&!type_of_stats%in%c("bayes_glm_glmer","bayes_own_brm")
    if(dim3_dec2){
      if(!sub1_2_in_one_model){
        p_values<-sapply((1:length(testo))[(grepl("SEC_SUB_TEST_GROUP",names(testo)))],function(x) testo[[x]]$P_VALUE[1])
      } else{
        p_values<-unlist(testo$INTERACTION_TEST$P_VALUE)
      }
      if(sum(!is.na(p_values))==0){
        dim3_dec2<-F
      }
    }
    dim4_dec<-c(3.3,0.2)[c(!no_legend_check,no_legend_check)]
    
    mar_dims<-c(c(1.9,2.4)[c(!dim1_dec,dim1_dec)]-c(0,0.5)[c(show_N,!show_N)],2.9,
                c(0.2,2,2.8)[c(dim3_dec1&!dim3_dec2,
                               (!dim3_dec1&!dim3_dec2)|(dim3_dec1&dim3_dec2),
                               !dim3_dec1&dim3_dec2)],dim4_dec)
    
    
    #If saving path was given, save as png
    if(output_as_file){
      if(length(plot_dim)==0){
        png(output_dir,
            width=c(1,4/3)[c(!reaction_norm,reaction_norm)]*(165+length(sub1_states)*(837/2)+(dim4_dec/3.3)*198),
            height=966+(mar_dims[1]/1.9)*114+(mar_dims[3]/2)*120,
            res=300)
      } else{
        png(output_dir,
            width=plot_dim[1],
            height=plot_dim[2],
            res=300)
      }
    }
    
    if(skip_par_mar){
      par(mar=mar_dims)
    } else{
      par(mar=par_mar)
    }
    
    plot(1,1,type="n",xlim=c(0,sum(sub1_panel_weights)),
         ylim=c(0,1),xaxt="none",yaxt="none",
         ylab="",xlab="",xaxs="i")
    
    
    #BOXPLOTS with coloured box
    
    #Add boxplot, if chosen and stats_for_1sub==T
    if(sum(unlist(sub_box_yn))>0){
      if(stats_for_1sub){
        for(AB in 1:length(sub1_states)){
          plot_the_box<-F
          if(!is.list(sub_box_yn)){
            if(sub_box_yn){
              plot_the_box<-T
            }
          } else{
            if(length(sub_box_yn[[AB]])==1){
              if(sub_box_yn[[AB]]){
                plot_the_box<-T
              }
            } else{
              if(sub_box_yn[[AB]][2]){
                plot_the_box<-T
              }
            }
          }
          if(sum(tfinal$positioning_back_up==AB)>0&plot_the_box){
            x1<-as.vector(tfinal$preference_DUMMY[tfinal$positioning_back_up==AB])
            w1<-as.vector(tfinal$appearance_DUMMY[tfinal$positioning_back_up==AB])
            #weights depending on setting
            if(box_type==3){
              bw1<-1/(sapply(1:sum(tfinal$positioning_back_up==AB),function(x) ifelse(tfinal$appearance_DUMMY[tfinal$positioning_back_up==AB][x]>0,diff(as.numeric(binom.confint(as.vector(tfinal$preference_DUMMY)[tfinal$positioning_back_up==AB][x]*tfinal$appearance_DUMMY[tfinal$positioning_back_up==AB][x],tfinal$appearance_DUMMY[tfinal$positioning_back_up==AB][x],
                                                                                                                                                                                 method="exact")[5:6])),NA)))
            } else{
              if(box_type==2){
                bw1<-w1
              } else{
                bw1<-rep(1,length(w1))
              }
            }
            
            suppressWarnings(wtd.boxplot(
              x1[w1>0],
              weights=list(bw1[w1>0],rep(1,sum(w1>0)))[c(length(unique(bw1[w1>0]))>1,length(unique(bw1[w1>0]))==1)],
              at = sub1_pos_in_plot[AB],
              add = T,
              axes = F,
              outline = F,
              whisklty = c(0,1)[c(!box_whiskers,box_whiskers)], staplelty = c(0,1)[c(!box_whiskers,box_whiskers)],
              xlab = "",
              ylab = "",
              col = box_colour,
              lwd = 2.5,
              border="white",
              medlwd=NA,
              range=1.5,
              boxwex=boxwex_set
            ))
            
            suppressWarnings(wtd.boxplot(
              x1[w1>0],
              weights=list(bw1[w1>0],rep(1,sum(w1>0)))[c(length(unique(bw1[w1>0]))>1,length(unique(bw1[w1>0]))==1)],
              at = sub1_pos_in_plot[AB],
              add = T,
              axes = F,
              outline = F,
              whisklty = c(0,2)[c(!box_whiskers,box_whiskers)], staplelty = c(0,1)[c(!box_whiskers,box_whiskers)],
              xlab = "",
              ylab = "",
              col = box_colour,
              lwd = 1,
              medlwd=NA,
              range=1.5,
              boxwex=boxwex_set
            )
            )
          }
        }
      }
    }
    
    
    #If we have categories in the second subset level
    if(check_sub2_states&sum(subtest>1)>0){
      
      #If also boxplots should be plotted for second subset
      if(sum(unlist(sub_box_yn))>0){
        if(stats_for_2sub){
          for(sub2_GLMM in 1:length(sub1_states)){
            if(subtest[sub2_GLMM]==2&sum(tfinal$positioning_back_up==sub2_GLMM)>0){
              for (AB in 1:2) {
                x1<-as.vector(tfinal$preference_DUMMY[tfinal$positioning_back_up==sub2_GLMM][tfinal$positioning_sub2[tfinal$positioning_back_up==sub2_GLMM]==AB])
                plot_the_box<-F
                if(!is.list(sub_box_yn)){
                  if(sub_box_yn){
                    plot_the_box<-T
                  }
                } else{
                  if(sub_box_yn[[sub2_GLMM]][c(1,3)[c(AB==1,AB==2)]]){
                    plot_the_box<-T
                  }
                }
                
                if(length(x1)>0&plot_the_box){
                  w1<-as.vector(tfinal$appearance_DUMMY[tfinal$positioning_back_up==sub2_GLMM][tfinal$positioning_sub2[tfinal$positioning_back_up==sub2_GLMM]==AB])
                  if(box_type==3){
                    bw1<-1/(sapply(1:sum(tfinal$positioning_sub2[tfinal$positioning_back_up==sub2_GLMM]==AB),function(x) ifelse(tfinal$appearance_DUMMY[tfinal$positioning_back_up==sub2_GLMM][tfinal$positioning_sub2[tfinal$positioning_back_up==sub2_GLMM]==AB][x]>0,diff(as.numeric(binom.confint(as.vector(tfinal$preference_DUMMY)[tfinal$positioning_back_up==sub2_GLMM][tfinal$positioning_sub2[tfinal$positioning_back_up==sub2_GLMM]==AB][x]*tfinal$appearance_DUMMY[tfinal$positioning_back_up==sub2_GLMM][tfinal$positioning_sub2[tfinal$positioning_back_up==sub2_GLMM]==AB][x],tfinal$appearance_DUMMY[tfinal$positioning_back_up==sub2_GLMM][tfinal$positioning_sub2[tfinal$positioning_back_up==sub2_GLMM]==AB][x],
                                                                                                                                                                                                                                                                                                      method="exact")[5:6])),NA)))
                  } else{
                    if(box_type==2){
                      bw1<-w1
                    } else{
                      bw1<-rep(1,length(w1))
                    }
                  }
                  
                  suppressWarnings(wtd.boxplot(
                    x1[w1>0],
                    weights=list(bw1[w1>0],rep(1,sum(w1>0)))[c(length(unique(bw1[w1>0]))>1,length(unique(bw1[w1>0]))==1)],
                    at = sub1_pos_in_plot[sub2_GLMM]+c(-sub2_est_perc_cent*0.5*sub1_panel_weights[sub2_GLMM],sub2_est_perc_cent*0.5*sub1_panel_weights[sub2_GLMM])[AB==(1:2)],
                    add = T,
                    axes = F,
                    outline = F,
                    whisklty = c(0,1)[c(!box_whiskers,box_whiskers)], staplelty = c(0,1)[c(!box_whiskers,box_whiskers)],
                    xlab = "",
                    ylab = "",
                    col = box_colour,
                    lwd = 2.5,
                    border="white",
                    medlwd=NA,
                    range=1.5,
                    boxwex=boxwex_set/3
                  )
                  )
                  
                  suppressWarnings(wtd.boxplot(
                    x1[w1>0],
                    weights=list(bw1[w1>0],rep(1,sum(w1>0)))[c(length(unique(bw1[w1>0]))>1,length(unique(bw1[w1>0]))==1)],
                    at = sub1_pos_in_plot[sub2_GLMM]+c(-sub2_est_perc_cent*0.5*sub1_panel_weights[sub2_GLMM],sub2_est_perc_cent*0.5*sub1_panel_weights[sub2_GLMM])[AB==(1:2)],
                    add = T,
                    axes = F,
                    outline = F,
                    whisklty = c(0,2)[c(!box_whiskers,box_whiskers)], staplelty = c(0,1)[c(!box_whiskers,box_whiskers)],
                    xlab = "",
                    ylab = "",
                    col = box_colour,
                    lwd = 1,
                    medlwd=NA,
                    range=1.5,
                    boxwex=boxwex_set/3
                  )
                  )
                }
              }
            }
          }
        }
      }
    }
    
    
    #Add thin lines if wished
    if(thin_line_sub){
      abline(v=sub1_pos_in_plot,lwd=0.25)
    }
    #Some helpful lines
    if(horiz_0.5){
      abline(h=0.5,lty="dashed",lwd=1.5)
    }
    #Add panel dividers if wished
    if(panel_dividers){
      abline(v=sapply(1:length(sub1_panel_weights),function(x) sum(sub1_panel_weights[1:x])),
             lwd=1.4)
    }
    
    
    #seedi
    
    no_seed_plot<-F
    if(missing(seedi)){
      no_seed_plot<-T
    } else{
      if(length(seedi)==0){
        no_seed_plot<-T
      } else{
        if(length(seedi)==1){
          if(is.vector(seedi)&!is.list(seedi)){
            if(is.na(seedi)){
              no_seed_plot<-T
            } else{
              if(is.character(seedi)){
                if(gsub(" ","",seedi)==""){
                  no_seed_plot<-T
                }
              }
            }
          }
        }
      }
    }
    
    if(!only_stats){
      if(!no_seed_plot){
        if(length(seedi)!=1){
          stop("seedi has to be a numeric vector of size 1.")
        }
        if(!is.vector(seedi)|is.list(seedi)){
          stop("seedi has to be a numeric vector of size 1.")
        }
        if(!is.numeric(seedi)){
          stop("seedi has to be a numeric vector of size 1.")
        }
        #set.seed
        seed_set_plot<-T
        set.seed(seedi)
        if(verbose){
          print(paste0("Seed of ",seedi," was set before jittering of dots."))
        }
        
        if(verbose){
          print("--------")
        }
      }
    }
    
    tryCatch(
      {
        
        
        #If we have categories in the second subset level
        if(check_sub2_states&sum(subtest>1)>0){
          
          
          #Local jittering
          
          #First, we will do an evaluation of the dot_size setting.
          #This is the most likely reason for the jittering to give
          #an error.
          
          #If we have a second subset, jittering gets more complicated. First we 
          #have to test if this is the case.
          
          #First for the case where all are subsetted
          if(length(subtest)==sum(subtest>1)){

            if(length(tfinal$positioning)>0){
              #Check if errors due to dot_size.
              
              jitter_call<-"(verbose=F,lwd=dot_lwd,fact_coord=tfinal$positioning,
                            gradual_coord=as.vector(tfinal$preference_DUMMY),
                            factorial_axis=1,
                            buffer=border_space,
                            sizes=sqrt(tfinal$appearance_DUMMY)*dot_size,
                            distance_fac=0,
                            sd_p=0.500,
                            iterations=numb_iterations,
                            priority_space=1,
                            center_space_rel=center_space_cat,
                            categories = tfinal$positioning_sub2,
                            start_seed = seedi)"
              
              jitter_try<-max(eval(parse(text=paste0("local_jitter_eval",jitter_call))))
              max_dot_size<-((0.2-0.5*center_space_cat)/jitter_try)*dot_size
              
              #Call function
              tryc_expr<-paste0("assign('pos_in_plot_n',local_jitter",jitter_call,")")
              tryCatch(eval(parse(text=tryc_expr)), error=function(e) stop(paste0("Jittering of dots went wrong. Most likely your setting (dot_size = ",dot_size,") is too big. Alternatively, center_space_cat is set to too big of a value (it shouldn't be bigger than maybe 0.1) or dot_lwd is too big (values around 1 seem reasonable). A quick approximation suggests the setting for dot_size to not exceed ",round(max_dot_size,4)," (this may differ if you set sub1_panel_weights). Your setting should be a lot smaller than this maximum setting to result in pretty jittering (try as an example a setting of ",round(max_dot_size/10,4)," to start with; this may be a bad suggestion if you set sub1_panel_weights). In case the values you see here are negative, the problem is your too high setting for center_space_cat.")))
              
              #Make sure that all were jittered to the right side
              #in cases where second subsetting only resulted in one group
              if(sum(true_exist==1)>0){
                for(checkers in (1:length(true_exist))[true_exist==1]){
                  if(!is.na(ifelse(length(sub2_col)>1,sub2_col[checkers],sub2_col))){
                    what_lev<-sub2_states[[checkers]]
                    testero<-(tfinal[,ifelse(length(sub2_col)>1,sub2_col[checkers],sub2_col)][tfinal$positioning_back_up==checkers]==what_lev[2])[1]
                    if(testero){
                      testero2<-sum(((pos_in_plot_n[tfinal$positioning_back_up==checkers])>tfinal$positioning[tfinal$positioning_back_up==checkers]))==0
                      if(testero2){
                        pos_in_plot_n[tfinal$positioning_back_up==checkers]<-pos_in_plot_n[tfinal$positioning_back_up==checkers]+2*(tfinal$positioning[tfinal$positioning_back_up==checkers]-pos_in_plot_n[tfinal$positioning_back_up==checkers])
                      }
                    }
                  }
                }
              }
              points(pos_in_plot_n,tfinal$preference_DUMMY,
                     pch=21,cex=sqrt(tfinal$appearance_DUMMY)*dot_size,
                     bg=adjustcolor(tfinal$dot_colourss,transparency),lwd=dot_lwd)
            }
          } else{
            
            #Now we care about cases where one category is subsetted
            #again and the other isn't.
            #Find out, which is subsetted.
            which_one_categ<-(1:length(sub1_states))[subtest>1]
            
            max_dot_size1<-0
            max_dot_size2<-0
            
            if(sum(tfinal$positioning_back_up%in%which_one_categ)>0){
              #Jitter these with a second category
              #First get the function check though.
              
              #Check if errors due to dot_size.
              jitter_call1<-"(verbose=F,lwd=dot_lwd,fact_coord=tfinal$positioning[tfinal$positioning_back_up%in%which_one_categ],
                             gradual_coord=as.vector(tfinal$preference_DUMMY)[tfinal$positioning_back_up%in%which_one_categ],
                             factorial_axis=1,
                             buffer=border_space,
                             sizes=sqrt(tfinal$appearance_DUMMY)[tfinal$positioning_back_up%in%which_one_categ]*dot_size,
                             distance_fac=0,
                             sd_p=0.500,
                             iterations=numb_iterations,
                             priority_space=1,
                             center_space_rel=center_space_cat,
                             categories = tfinal$positioning_sub2[tfinal$positioning_back_up%in%which_one_categ],
                             start_seed = seedi)"
              
              jitter_try<-max(eval(parse(text=paste0("local_jitter_eval",jitter_call1))))
              max_dot_size1<-((0.2-0.5*center_space_cat)/jitter_try)*dot_size
              
            } 
            
            if(sum(!tfinal$positioning_back_up%in%which_one_categ)>0){
              #Jitter the others normally
              #Again, first get the function check
              
              #Check if errors due to dot_size.
              jitter_call2<-"(verbose=F,lwd=dot_lwd,fact_coord=tfinal$positioning[!tfinal$positioning_back_up%in%which_one_categ],
                             gradual_coord=as.vector(tfinal$preference_DUMMY)[!tfinal$positioning_back_up%in%which_one_categ],
                             factorial_axis=1,
                             buffer=border_space,
                             sizes=sqrt(tfinal$appearance_DUMMY)[!tfinal$positioning_back_up%in%which_one_categ]*dot_size,
                             distance_fac=0,
                             sd_p=0.500,
                             iterations=numb_iterations,
                             priority_space=1,
                             start_seed = seedi)"
              
              jitter_try<-max(eval(parse(text=paste0("local_jitter_eval",jitter_call2))))
              max_dot_size2<-2*((0.2/jitter_try)*dot_size)
              
            }
            
            if(sum(tfinal$positioning_back_up%in%which_one_categ)>0){
              
              #Call jitter function
              tryc_expr<-paste0("assign('pos_in_plot_n_cat',local_jitter",jitter_call1,")")
              tryCatch(eval(parse(text=tryc_expr)), error=function(e) stop(paste0("Jittering of dots went wrong. Most likely your setting (dot_size = ",dot_size,") is too big. Alternatively, center_space_cat is set to too big of a value (it shouldn't be bigger than maybe 0.1) or dot_lwd is too big (values around 1 seem reasonable). A quick approximation suggests the setting for dot_size to not exceed ",round(min(max_dot_size1,max_dot_size2),4)," (this may differ if you set sub1_panel_weights). Your setting should be a lot smaller than this maximum setting to result in pretty jittering (try as an example a setting of ",round(min(max_dot_size1,max_dot_size2)/10,4)," to start with; this may be a bad suggestion if you set sub1_panel_weights). In case the values you see here are negative, the problem is your too high setting for center_space_cat.")))
              
              #Make sure that all were jittered to the right side
              #in cases where second subsetting only resulted in one group
              if(sum(true_exist[which_one_categ]==1)>0){
                for(checkers in which_one_categ[true_exist[which_one_categ]==1]){
                  if(!is.na(ifelse(length(sub2_col)>1,sub2_col[checkers],sub2_col))){
                    what_lev<-sub2_states[[checkers]]
                    testero<-(tfinal[,ifelse(length(sub2_col)>1,sub2_col[checkers],sub2_col)][tfinal$positioning_back_up==checkers]==what_lev[2])[1]
                    if(testero){
                      testero2<-sum(((tfinal$positioning[tfinal$positioning_back_up[tfinal$positioning_back_up%in%which_one_categ]==checkers])<pos_in_plot_n_cat[tfinal$positioning_back_up[tfinal$positioning_back_up%in%which_one_categ]==checkers]))==0
                      if(testero2){
                        pos_in_plot_n_cat[tfinal$positioning_back_up[tfinal$positioning_back_up%in%which_one_categ]==checkers]<-pos_in_plot_n_cat[tfinal$positioning_back_up[tfinal$positioning_back_up%in%which_one_categ]==checkers]+2*(tfinal$positioning[tfinal$positioning_back_up[tfinal$positioning_back_up%in%which_one_categ]==checkers]-pos_in_plot_n_cat[tfinal$positioning_back_up[tfinal$positioning_back_up%in%which_one_categ]==checkers])
                      }
                    }
                  }
                }
              }
              
              #Add points to graph.
              points(pos_in_plot_n_cat,as.vector(tfinal$preference_DUMMY)[tfinal$positioning_back_up%in%which_one_categ],
                     pch=21,cex=sqrt(tfinal$appearance_DUMMY[tfinal$positioning_back_up%in%which_one_categ])*dot_size,
                     bg=adjustcolor(tfinal$dot_colourss[tfinal$positioning_back_up%in%which_one_categ],transparency),lwd=dot_lwd)
            }
            
            
            #Jitter the others normally
            if(sum(!tfinal$positioning_back_up%in%which_one_categ)>0){
              
              #Call jitter function
              tryc_expr<-paste0("assign('pos_in_plot_n_no_cat',local_jitter",jitter_call2,")")
              tryCatch(eval(parse(text=tryc_expr)), error=function(e) stop(paste0("Jittering of dots went wrong. Most likely your setting (dot_size = ",dot_size,") is too big. Alternatively, center_space_cat is set to too big of a value (it shouldn't be bigger than maybe 0.1) or dot_lwd is too big (values around 1 seem reasonable). A quick approximation suggests the setting for dot_size to not exceed ",round(min(max_dot_size1,max_dot_size2),4)," (this may differ if you set sub1_panel_weights). Your setting should be a lot smaller than this maximum setting to result in pretty jittering (try as an example a setting of ",round(min(max_dot_size1,max_dot_size2)/10,4)," to start with; this may be a bad suggestion if you set sub1_panel_weights). In case the values you see here are negative, the problem is your too high setting for center_space_cat.")))
              
              #Add points to graph.
              points(pos_in_plot_n_no_cat,as.vector(tfinal$preference_DUMMY)[!tfinal$positioning_back_up%in%which_one_categ],
                     pch=21,cex=sqrt(tfinal$appearance_DUMMY[!tfinal$positioning_back_up%in%which_one_categ])*dot_size,
                     bg=adjustcolor(tfinal$dot_colourss[!tfinal$positioning_back_up%in%which_one_categ],transparency),lwd=dot_lwd)
            }
          }
        } else{
          
          #Check if errors due to dot_size.
          jitter_call<-"(verbose=F,lwd=dot_lwd,fact_coord=tfinal$positioning,
                        gradual_coord=as.vector(tfinal$preference_DUMMY),
                        factorial_axis=1,
                        buffer=border_space,
                        sizes=sqrt(tfinal$appearance_DUMMY)*dot_size,
                        distance_fac=0,
                        sd_p=0.500,
                        iterations=numb_iterations,
                        priority_space=1,
                        start_seed = seedi)"
          
          jitter_try<-max(eval(parse(text=paste0("local_jitter_eval",jitter_call))))
          max_dot_size<-2*((0.2/jitter_try)*dot_size)
          
          #Call jitter function
          tryc_expr<-paste0("assign('pos_in_plot_n',local_jitter",jitter_call,")")
          tryCatch(eval(parse(text=tryc_expr)), error=function(e) stop(paste0("Jittering of dots went wrong. Most likely your setting (dot_size = ",dot_size,") is too big. Alternatively, center_space_cat is set to too big of a value (it shouldn't be bigger than maybe 0.1) or dot_lwd is too big (values around 1 seem reasonable). A quick approximation suggests the setting for dot_size to not exceed ",round(max_dot_size,4)," (this may differ if you set sub1_panel_weights). Your setting should be a lot smaller than this maximum setting to result in pretty jittering (try as an example a setting of ",round(max_dot_size/10,4)," to start with; this may be a bad suggestion if you set sub1_panel_weights). In case the values you see here are negative, the problem is your too high setting for center_space_cat.")))
          
          points(pos_in_plot_n,tfinal$preference_DUMMY,
                 pch=21,cex=sqrt(tfinal$appearance_DUMMY)*dot_size,
                 bg=adjustcolor(tfinal$dot_colourss,transparency),lwd=dot_lwd)
        }
        
        
        
        #BOXPLOTS without coloured box
        
        #Add boxplot, if chosen and stats_for_1sub==T
        if(sum(unlist(sub_box_yn))>0){
          if(stats_for_1sub){
            for (AB in 1:length(sub1_states)) {
              plot_the_box<-F
              if(!is.list(sub_box_yn)){
                if(sub_box_yn){
                  plot_the_box<-T
                }
              } else{
                if(length(sub_box_yn[[AB]])==1){
                  if(sub_box_yn[[AB]]){
                    plot_the_box<-T
                  }
                } else{
                  if(sub_box_yn[[AB]][2]){
                    plot_the_box<-T
                  }
                }
              }
              if(sum(tfinal$positioning_back_up==AB)>0&plot_the_box){
                x1<-as.vector(tfinal$preference_DUMMY[tfinal$positioning_back_up==AB])
                w1<-as.vector(tfinal$appearance_DUMMY[tfinal$positioning_back_up==AB])
                #weights depending on setting
                if(box_type==3){
                  bw1<-1/(sapply(1:sum(tfinal$positioning_back_up==AB),function(x) ifelse(tfinal$appearance_DUMMY[tfinal$positioning_back_up==AB][x]>0,diff(as.numeric(binom.confint(as.vector(tfinal$preference_DUMMY)[tfinal$positioning_back_up==AB][x]*tfinal$appearance_DUMMY[tfinal$positioning_back_up==AB][x],tfinal$appearance_DUMMY[tfinal$positioning_back_up==AB][x],
                                                                                                                                                                                     method="exact")[5:6])),NA)))
                } else{
                  if(box_type==2){
                    bw1<-w1
                  } else{
                    bw1<-rep(1,length(w1))
                  }
                }
                
                suppressWarnings(wtd.boxplot(
                  x1[w1>0],
                  weights=list(bw1[w1>0],rep(1,sum(w1>0)))[c(length(unique(bw1[w1>0]))>1,length(unique(bw1[w1>0]))==1)],
                  at = sub1_pos_in_plot[AB],
                  add = T,
                  axes = F,
                  outline = F,
                  whisklty = c(0,1)[c(!box_whiskers,box_whiskers)], staplelty = c(0,1)[c(!box_whiskers,box_whiskers)],
                  xlab = "",
                  ylab = "",
                  col = NA,
                  lwd = 2.5,
                  border="white",
                  medlwd=NA,
                  range=1.5,
                  boxwex=boxwex_set
                ))
                
                suppressWarnings(wtd.boxplot(
                  x1[w1>0],
                  weights=list(bw1[w1>0],rep(1,sum(w1>0)))[c(length(unique(bw1[w1>0]))>1,length(unique(bw1[w1>0]))==1)],
                  at = sub1_pos_in_plot[AB],
                  add = T,
                  axes = F,
                  outline = F,
                  whisklty = c(0,2)[c(!box_whiskers,box_whiskers)], staplelty = c(0,1)[c(!box_whiskers,box_whiskers)],
                  xlab = "",
                  ylab = "",
                  col = NA,
                  lwd = 1,
                  medlwd=NA,
                  range=1.5,
                  boxwex=boxwex_set
                )
                )
              }
            }
          }
        }
        
        
        #If we have categories in the second subset level
        if(check_sub2_states&sum(subtest>1)>0){
          
          #If also boxplots should be plotted for second subset
          if(sum(unlist(sub_box_yn))>0){
            if(stats_for_2sub){
              for(sub2_GLMM in 1:length(sub1_states)){
                if(subtest[sub2_GLMM]==2&sum(tfinal$positioning_back_up==sub2_GLMM)>0){
                  for (AB in 1:2) {
                    x1<-as.vector(tfinal$preference_DUMMY[tfinal$positioning_back_up==sub2_GLMM][tfinal$positioning_sub2[tfinal$positioning_back_up==sub2_GLMM]==AB])
                    plot_the_box<-F
                    if(!is.list(sub_box_yn)){
                      if(sub_box_yn){
                        plot_the_box<-T
                      }
                    } else{
                      if(sub_box_yn[[sub2_GLMM]][c(1,3)[c(AB==1,AB==2)]]){
                        plot_the_box<-T
                      }
                    }
                    
                    if(length(x1)>0&plot_the_box){
                      w1<-as.vector(tfinal$appearance_DUMMY[tfinal$positioning_back_up==sub2_GLMM][tfinal$positioning_sub2[tfinal$positioning_back_up==sub2_GLMM]==AB])
                      if(box_type==3){
                        bw1<-1/(sapply(1:sum(tfinal$positioning_sub2[tfinal$positioning_back_up==sub2_GLMM]==AB),function(x) ifelse(tfinal$appearance_DUMMY[tfinal$positioning_back_up==sub2_GLMM][tfinal$positioning_sub2[tfinal$positioning_back_up==sub2_GLMM]==AB][x]>0,diff(as.numeric(binom.confint(as.vector(tfinal$preference_DUMMY)[tfinal$positioning_back_up==sub2_GLMM][tfinal$positioning_sub2[tfinal$positioning_back_up==sub2_GLMM]==AB][x]*tfinal$appearance_DUMMY[tfinal$positioning_back_up==sub2_GLMM][tfinal$positioning_sub2[tfinal$positioning_back_up==sub2_GLMM]==AB][x],tfinal$appearance_DUMMY[tfinal$positioning_back_up==sub2_GLMM][tfinal$positioning_sub2[tfinal$positioning_back_up==sub2_GLMM]==AB][x],
                                                                                                                                                                                                                                                                                                          method="exact")[5:6])),NA)))
                      } else{
                        if(box_type==2){
                          bw1<-w1
                        } else{
                          bw1<-rep(1,length(w1))
                        }
                      }
                      
                      suppressWarnings(wtd.boxplot(
                        x1[w1>0],
                        weights=list(bw1[w1>0],rep(1,sum(w1>0)))[c(length(unique(bw1[w1>0]))>1,length(unique(bw1[w1>0]))==1)],
                        at = sub1_pos_in_plot[sub2_GLMM]+c(-sub2_est_perc_cent*0.5*sub1_panel_weights[sub2_GLMM],sub2_est_perc_cent*0.5*sub1_panel_weights[sub2_GLMM])[AB==(1:2)],
                        add = T,
                        axes = F,
                        outline = F,
                        whisklty = c(0,1)[c(!box_whiskers,box_whiskers)], staplelty = c(0,1)[c(!box_whiskers,box_whiskers)],
                        xlab = "",
                        ylab = "",
                        col = NA,
                        lwd = 2.5,
                        border="white",
                        medlwd=NA,
                        range=1.5,
                        boxwex=boxwex_set/3
                      )
                      )
                      
                      suppressWarnings(wtd.boxplot(
                        x1[w1>0],
                        weights=list(bw1[w1>0],rep(1,sum(w1>0)))[c(length(unique(bw1[w1>0]))>1,length(unique(bw1[w1>0]))==1)],
                        at = sub1_pos_in_plot[sub2_GLMM]+c(-sub2_est_perc_cent*0.5*sub1_panel_weights[sub2_GLMM],sub2_est_perc_cent*0.5*sub1_panel_weights[sub2_GLMM])[AB==(1:2)],
                        add = T,
                        axes = F,
                        outline = F,
                        whisklty = c(0,2)[c(!box_whiskers,box_whiskers)], staplelty = c(0,1)[c(!box_whiskers,box_whiskers)],
                        xlab = "",
                        ylab = "",
                        col = NA,
                        lwd = 1,
                        medlwd=NA,
                        range=1.5,
                        boxwex=boxwex_set/3
                      )
                      )
                    }
                  }
                }
              }
            }
          }
        }
        
        
        #Add reaction norm if wished
        if(reaction_norm){
          #draw reaction norm
          segments(IDs_frame$BIN-(center_space_cat-0.01),IDs_frame$starty,IDs_frame$BIN+(center_space_cat+0.01),IDs_frame$endy,
                   sapply(1:nrow(IDs_frame),function(x) adjustcolor("black",IDs_frame$weight[x])))
        } 
        
        #Add significance codes depending on whether only one test was done or not
        
        #Also, we differentiate between cases were sub1_col and sub2_col were run with interaction in the same model
        #And where they were not. First the case for separate models.
        
        if(sum(c("INTERACTION_TEST","FIRST_SUB_TEST")%in%names(testo))>0){
          if(!is.na(testo[[1]][1])){
            
            sign_testo<-testo[[1]]
            if(is.list(sign_testo$P_VALUE)){
              sign_testo$P_VALUE<-sign_testo$P_VALUE[[1]]
              sign_testo[[4]]<-sign_testo[[4]][[1]]
            }
            if(type_of_stats%in%c("bayes_glm_glmer","bayes_own_brm")){
              sign_testo$P_VALUE[1]<-1
            }
            CI_CrI<-sign_testo[[4]]
            if(sum(is.na(CI_CrI[,2:4]))!=prod(dim(CI_CrI[,2:4]))){
              CI_CrI<-CI_CrI[!is.na(CI_CrI[,2]),]
              CI_order<-CI_CrI[,1]
              CI_order<-sapply(1:length(CI_order),function(x) (1:length(sub1_states))[CI_order[x]==sub1_states])
              
              #Get position of significance results
              is.sec_glmm<-stats_for_2sub&check_sub2_states&sum(subtest>1)>0&sum(true_exist>1)>0&show_p_values_plot[2]
              liner<-c(1000,0.9,1.8)[c(dim3_dec1,!is.sec_glmm&!dim3_dec1,is.sec_glmm&!dim3_dec1)]
              
              positioning_of_bars<-list(sub1_pos_in_plot[CI_order],rep(-10000,length(CI_order)))[c(show_estimators[1],!show_estimators[1])][[1]]
              positioning_of_p<-positioning_of_bars
              #Remove sub1_col estimators if sub2_col subsetting is there (and wished by user)
              if(suppress_est1_if2&is.sec_glmm){
                positioning_of_bars[subtest[CI_order]>1]<-(-10000)
              }
              if(length(CI_order)>2){
                sign_coder(values=sign_testo,
                           lino=liner,ticki=0.01,
                           where=c(sub1_pos_in_plot[1],0.5*sum(sub1_panel_weights),sub1_pos_in_plot[length(sub1_pos_in_plot)]),
                           bars_where=positioning_of_bars,
                           conf=T,
                           width_mean_bar=width_mean_sub1,
                           width_error_sub1=width_error_sub1,
                           thickness_est_CI1=thickness_est_CI)
              } else{
                sign_coder(values=sign_testo,
                           lino=liner,ticki=0.015,
                           where=positioning_of_p,
                           bars_where=positioning_of_bars,
                           conf=T,
                           width_mean_bar=width_mean_sub1,
                           width_error_sub1=width_error_sub1,
                           thickness_est_CI1=thickness_est_CI)
              }
            }
          }
        }
        
        
        
        if(stats_for_2sub&check_sub2_states&sum(subtest>1)>0){
          #Get position of significance results
          liner<-c(0.42,0.9)[c(!dim3_dec1,dim3_dec1)]
          if(!show_p_values_plot[2]|type_of_stats%in%c("bayes_glm_glmer","bayes_own_brm")){
            liner<-1000
          }
          
          #Differentiate between interaction test and normal.
          if("INTERACTION_TEST"%in%names(testo)){
            
            sign_testo<-testo[[1]]
            if(type_of_stats%in%c("bayes_glm_glmer","bayes_own_brm")){
              sign_testo$P_VALUE[[2]][,2]<-rep(1,nrow(sign_testo$P_VALUE[[2]]))
            }
            
            CI_CrI<-sign_testo[[4]][[2]]
            if(sum(is.na(CI_CrI[,3:5]))!=prod(dim(CI_CrI[,3:5]))){
              CI_CrI<-CI_CrI[!is.na(CI_CrI[,3]),]
              CI_order<-CI_CrI[,1:2]
              CI_order<-sapply(1:nrow(CI_order),function(x) paste0((1:length(sub1_states))[CI_order[x,1]==sub1_states],"_",(1:2)[CI_order[x,2]==sub2_states[[(1:length(sub2_states))[sub1_states==CI_CrI[x,1]]]]]))
              CI_order<-order(CI_order)
              CI_CrI<-CI_CrI[CI_order,]
              CI_order<-(1:nrow(CI_CrI))
              P_order<-sign_testo$P_VALUE[[2]]
              P_order<-sapply(1:nrow(P_order),function(x) (1:length(sub1_states))[P_order[x,1]==sub1_states])
              sign_testo$P_VALUE[[2]]<-sign_testo$P_VALUE[[2]][order(P_order),]
              
              for(by_group_int in 1:(length(CI_order)/2)){
                sign_testo_sub<-sign_testo
                sign_testo_sub$P_VALUE<-sign_testo_sub$P_VALUE[[2]][by_group_int,2]
                sign_testo_sub[[4]]<-CI_CrI[c(2*by_group_int-1,2*by_group_int),]
                sign_testo_sub[[4]]<-sign_testo_sub[[4]][,c(1,3:5)]
                sign_coder(values=sign_testo_sub,
                           lino=liner,ticki=c(0.012,0.015,0.01)[liner==c(0.42,0.9,1000)],
                           where=sub1_pos_in_plot[sub1_states==CI_CrI[c(2*by_group_int-1,2*by_group_int),1][1]]+sub1_panel_weights[sub1_states==CI_CrI[c(2*by_group_int-1,2*by_group_int),1][1]]*c(-0.25,0.25),
                           bars_where=list(sub1_pos_in_plot[sub1_states==CI_CrI[c(2*by_group_int-1,2*by_group_int),1][1]]+sub1_panel_weights[sub1_states==CI_CrI[c(2*by_group_int-1,2*by_group_int),1][1]]*c(-0.25,0.25),rep(-10000,2))[c(show_estimators[2],!show_estimators[2])][[1]],
                           conf=T,
                           width_mean_bar=width_mean_sub2,
                           width_error_sub1=width_error_sub2,
                           thickness_est_CI1=thickness_est_CI)
                
              }
            }
          } else{
            
            for(sub2_GLMM in 1:length(sub1_states)){
              if(subtest[sub2_GLMM]==2&true_exist[sub2_GLMM]==2){
                
                sign_testo<-testo[[paste0("SEC_SUB_TEST_GROUP_",sub1_states[sub2_GLMM])]]
                if(!is.na(sign_testo[1])){
                  if(type_of_stats%in%c("bayes_glm_glmer","bayes_own_brm")){
                    sign_testo$P_VALUE[1]<-1
                  }
                  if(sum(is.na(sign_testo[[4]][,2:4]))!=prod(dim(sign_testo[[4]][,2:4]))){
                    sign_testo[[4]]<-sign_testo[[4]][!is.na(sign_testo[[4]][,2]),]
                    CI_order<-sign_testo[[4]][,1]
                    CI_order<-sapply(1:length(CI_order),function(x) (1:length(sub2_states[[sub2_GLMM]]))[CI_order[x]==sub2_states[[sub2_GLMM]]])
                    
                    sign_coder(values=sign_testo,
                               lino=liner,ticki=c(0.012,0.015,0.01)[liner==c(0.42,0.9,1000)],where=sub1_pos_in_plot[sub2_GLMM]+sub1_panel_weights[sub2_GLMM]*c(-0.25,0.25),
                               bars_where=list(sub1_pos_in_plot[sub2_GLMM]+sub1_panel_weights[sub2_GLMM]*list(c(-sub2_est_perc_cent*0.5,sub2_est_perc_cent*0.5),c(sub2_est_perc_cent*0.5,-sub2_est_perc_cent*0.5))[[(1:2)[CI_order[1]==c(1,2)]]],rep(-10000,2))[c(show_estimators[2],!show_estimators[2])][[1]],
                               conf=T,
                               width_mean_bar=width_mean_sub2,
                               width_error_sub1=width_error_sub2,
                               thickness_est_CI1=thickness_est_CI)
                  }
                }
              }
            }
          }
        } 
        
        #yaxis
        axis(2,at=c(0,0.25,0.5,0.75,1),labels=F,tck=y_tck,lwd=1)
        if(add_y_labels){
          axis(2,at=c(0,0.25,0.5,0.75,1),c("0","0.25","0.5","0.75","1"),las=1,cex.axis=0.85,line=-0.45,lwd=0)
        }
        
        box(,lwd=1.4)
        
        par(xpd=NA)
        
        
        #Get proportions of outer edges to make the outer edges look the same on all plotting devices
        if(legend_omi){
          prop_right_stripe<-(par("mai")[4]+par("omi")[4])/(par("pin")[1])
        } else{
          prop_right_stripe<-par("mai")[4]/(par("pin")[1])
        }
        prop_bottom_stripe<-par("mai")[1]/(par("pin")[2])
        
        
        #x axis depending on whether second subset is shown.
        #line_sub1<-c(2/5,1/2)[c(!dim1_dec,dim1_dec)]
        #Same height for all Sub1_labels
        if(show_N){
          line_sub1<-1/2
        } else{
          line_sub1<-3/5
        }
        
        #Subset 1
        if(!italics_sub1){
          sapply(1:length(sub1_labels),function(x) text(sub1_pos_in_plot[x],par("usr")[3]-(line_sub1)*diff(par("usr")[3:4])*prop_bottom_stripe,
                                                        sub1_labels[x],
                                                        cex=cex_sub1_labels))
        } else{
          # lowl<-c("q","p","g","j","y")
          # midl<-c("w","e","r","z","u","o","a","s","x","c","v","n","m")
          # highl<-c("t","i","d","f","h","k","l","b")
          # unders<-"_"
          # string_cat<-c()
          # for(abccheck in 1:length(sub1_labels)){
          #   checkcat<-c()
          #   letto<-strsplit(sub1_labels[abccheck],"")[[1]]
          #   if(length(letto)>0){
          #     for(eachlett in 1:length(letto)){
          #       if(letto[eachlett]==toupper(letto[eachlett])&letto[eachlett]%in%c(LETTERS,letters)){
          #         checkcat<-c(checkcat,2)
          #       } else{
          #         if(suppressWarnings(!is.na(as.numeric(letto[eachlett])))){
          #           checkcat<-c(checkcat,2)
          #         } else{
          #           checkcat<-c(checkcat,c(1:4,2)[c(letto[eachlett]%in%lowl,letto[eachlett]%in%midl,letto[eachlett]%in%highl,letto[eachlett]%in%unders,
          #                                           !letto[eachlett]%in%c(lowl,midl,highl))])
          #         }
          #       }
          #     }
          #   } else{
          #     checkcat<-2
          #   }
          #   string_cat<-c(string_cat,c(-1,-1,-0.87,-0.95)[c(sum(checkcat==3)>0&sum(checkcat==1)==0&sum(checkcat==4)==0,
          #                                                  sum(checkcat==3)==0&sum(checkcat==1)==0&sum(checkcat==4)==0,
          #                                                  sum(checkcat==1)>0,sum(checkcat==1)==0&sum(checkcat==4)>0)])
          # }
          
          sapply(1:length(sub1_labels),function(x) text(sub1_pos_in_plot[x],par("usr")[3]-(line_sub1)*diff(par("usr")[3:4])*prop_bottom_stripe,
                                                        substitute(paste(italic(b)),list(b=sub1_labels[x])),
                                                        cex=cex_sub1_labels))
        }
        if(show_N){
          if(length(label_count_col)==0){
            text(sub1_pos_in_plot,par("usr")[3]-(line_sub1+0.36)*diff(par("usr")[3:4])*prop_bottom_stripe,
                 sapply(1:length(sub1_states),function(x) paste0("(",sapply(1:length(sub1_states),function(y) sum(tfinal$positioning_back_up==y))[x],")")),
                 cex=cex_sub1_counts)
          } else{
            text(sub1_pos_in_plot,par("usr")[3]-(line_sub1+0.36)*diff(par("usr")[3:4])*prop_bottom_stripe,
                 sapply(1:length(sub1_states),function(x) paste0("(",sapply(1:length(sub1_states),function(y) length(unique(tfinal[,label_count_col][tfinal$positioning_back_up==y])))[x],")")),
                 cex=cex_sub1_counts)
          }
          #Subset 2
          if(dim1_dec){
            for(addo in 1:length(sub1_states)){
              if(subtest[addo]>1){
                if(length(label_count_col)==0){
                  text(sub1_pos_in_plot[addo]+c(-0.25,0.25)*sub1_panel_weights[addo],par("usr")[3]-(line_sub1-0.3)*diff(par("usr")[3:4])*prop_bottom_stripe,
                       sapply(1:length(sub2_states[[addo]]),function(x) paste0(sub2_labels[[addo]][x]," (",sapply(1:2,function(y) sum(tfinal$positioning_sub2[tfinal$positioning_back_up==addo]==y&tfinal$appearance_DUMMY[tfinal$positioning_back_up==addo]>0))[x],")")),
                       cex=cex_sub2_labels)
                } else{
                  text(sub1_pos_in_plot[addo]+c(-0.25,0.25)*sub1_panel_weights[addo],par("usr")[3]-(line_sub1-0.3)*diff(par("usr")[3:4])*prop_bottom_stripe,
                       sapply(1:length(sub2_states[[addo]]),function(x) paste0(sub2_labels[[addo]][x]," (",sapply(1:2,function(y) length(unique(tfinal[,label_count_col][tfinal$positioning_sub2==y&tfinal$positioning_back_up==addo&tfinal$appearance_DUMMY>0])))[x],")")),
                       cex=cex_sub2_labels)
                }
              }
            }
          }
        } else{
          #Subset 2
          if(dim1_dec){
            for(addo in 1:length(sub1_states)){
              if(subtest[addo]>1){
                text(sub1_pos_in_plot[addo]+c(-0.25,0.25)*sub1_panel_weights[addo],par("usr")[3]-(line_sub1-0.4)*diff(par("usr")[3:4])*prop_bottom_stripe,
                     sub2_labels[[addo]],
                     cex=cex_sub2_labels)
              }
            }
          }
        }
        
        #If legend wished
        if(!no_legend_check){
          
          poss_dots<-list(rev(c(0.5-0.5*0.19*squeeze_legend))+c(0,0.045)[c(legend_info,!legend_info)],
                          rev(c(0.5-0.5*0.19*squeeze_legend)+0.5*c(-0.085,0.085)*squeeze_legend)+c(0,0.045)[c(legend_info,!legend_info)],
                          rev(c(0.5-0.5*0.19*squeeze_legend)+c(-0.085,0,0.085)*squeeze_legend)+c(0,0.045)[c(legend_info,!legend_info)],
                          rev(c(0.5-0.5*0.19*squeeze_legend)+0.5*c(-0.255,-0.085,0.085,0.255)*squeeze_legend)+c(0,0.045)[c(legend_info,!legend_info)],
                          rev(c(0.5-0.5*0.19*squeeze_legend)+c(-0.17,-0.085,0,0.085,0.17)*squeeze_legend)+c(0,0.045)[c(legend_info,!legend_info)])[[length(legend_dots)]]
          
          
          # points(rep(sum(sub1_panel_weights)+sum(sub1_panel_weights)*(prop_right_stripe-(1.911/20)),
          #            length(legend_dots)),poss_dots,
          #        cex=sqrt(legend_dots)*dot_size,pch=21,bg=adjustcolor(dot_colour_legend,transparency),lwd=dot_lwd)
          # text(rep(sum(sub1_panel_weights)+sum(sub1_panel_weights)*(prop_right_stripe-(0.1764247/6.2)),length(legend_dots)),poss_dots,
          #      as.character(legend_dots),cex=0.7)
          # text(sum(sub1_panel_weights)+sum(sub1_panel_weights)*(prop_right_stripe-0.05880824),max(poss_dots)+c(0.19,0.10)[c(legend_info,!legend_info)]*squeeze_legend,"# Obs.",cex=0.85)
          points(rep(sum(sub1_panel_weights)*(1+prop_right_stripe-(0.4060079/par("pin")[1])),
                     length(legend_dots)),poss_dots,
                 cex=sqrt(legend_dots)*dot_size,pch=21,bg=adjustcolor(dot_colour_legend,transparency),lwd=dot_lwd)
          text(rep(sum(sub1_panel_weights)*(1+prop_right_stripe-(0.1209126/par("pin")[1])),length(legend_dots)),poss_dots,
               as.character(legend_dots),cex=0.7)
          text(sum(sub1_panel_weights)*(1+prop_right_stripe-(0.249886/par("pin")[1])),max(poss_dots)+c(0.19,0.10)[c(legend_info,!legend_info)]*squeeze_legend,"# Obs.",cex=0.85)
          # points(rep(sum(sub1_panel_weights)+(7/20)*sum(sub1_panel_weights)*prop_right_stripe,length(legend_dots)),poss_dots,
          #        cex=sqrt(legend_dots)*dot_size,pch=21,bg=adjustcolor(dot_colour_legend,transparency),lwd=dot_lwd)
          # text(rep(sum(sub1_panel_weights)+(5/6.2)*sum(sub1_panel_weights)*prop_right_stripe,length(legend_dots)),poss_dots,
          #      as.character(legend_dots),cex=0.7)
          # text(sum(sub1_panel_weights)+0.6*sum(sub1_panel_weights)*prop_right_stripe,max(poss_dots)+c(0.19,0.10)[c(legend_info,!legend_info)]*squeeze_legend,"# Obs.",cex=0.85)
          
          if(legend_info){
            text(sum(sub1_panel_weights)*(1+prop_right_stripe-(0.249886/par("pin")[1])),max(poss_dots)+0.14*squeeze_legend,paste0("(Max = ",max(tfinal$appearance_DUMMY),")"),cex=0.5)
            text(sum(sub1_panel_weights)*(1+prop_right_stripe-(0.249886/par("pin")[1])),max(poss_dots)+0.10*squeeze_legend,paste0("(Tot = ",sum(tfinal$appearance_DUMMY),")"),cex=0.5)
          }
          
        }
        
        if(show_N){
          segments(0,par("usr")[3]-(0.68)*diff(par("usr")[3:4])*prop_bottom_stripe,sum(sub1_panel_weights),par("usr")[3]-(0.68)*diff(par("usr")[3:4])*prop_bottom_stripe)
        } 
        
        par(xpd=F)
        
        mtext(yaxs_label,2,line=1.95,font=2,cex=0.8)
        
        
      },
      error=function(cond) {
        message(paste("Plotting function crashed. Function will just return the stats results, if stats could be successfully performed. Error that made the function crash:"))
        message(cond)
        
        #Set random seed
        if(seed_set_plot|seed_set){
          set.seed(as.numeric(Sys.time()))
        }
        
        #And return stats
        return(testo)
        
        #No need to close plotting devices here because tryCatch seems to do it.
      }
      
    )
    
    #If saving path was given.
    if(output_as_file){
      invisible(dev.off())
    }
    
    #Set random seed
    if(seed_set_plot|seed_set){
      set.seed(as.numeric(Sys.time()))
    }
    
    
  } else{
    if(verbose){
      print("Only stats were performed, since only_stats=T. Therefore all parameters relevant for plotting were omitted.")
    }
  }
  
  #Set random seed
  if(seed_set_plot|seed_set){
    set.seed(as.numeric(Sys.time()))
  }
  
  if(verbose){
    print("--DONE--")
  }
  
  #Return stats results!
  return(testo)
}
