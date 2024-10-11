################################################################################
################################################################################
#   The R code provided below for demonstrating the exploration of data can be 
#   used as is, or modified as needed. 
################################################################################
################################################################################

# Load this library, automatically installed if not present in local machine
if(!(require("pacman"))){install.packages("packman",
                                          dependencies=TRUE)}

pacman::p_load("dplyr",
              "magrittr",
              "ggplot2",
              "readr",
              "purrr",
              "tidyr")

data_cases <- readr::read_csv("data/mockdata_cases1.csv")

####################################################################################
##### 1.  Check the data for potential errors
####################################################################################

# Prevalence is a fraction defined [0,1]
# Note: Prevalence of 1 while not statistically erroneous, need checking for accuracy
# What observations have errors
data_cases%>%
   dplyr::filter(prev <= 0 | prev >= 1)

# Defensive programming
# Note: The use of "::" enables us to call a function from a specific R package
#       I have had instances where if "stats" base R package was called first, 
#       the filter function if not specified with the R package fails

data_cases%>%
    stats::filter(prev < 0 | prev > 1) 

# We correct the two prevalence by re-calculating 
# Good practice to leave the original data intact (advantage of R over Stata)
data_prev <- data_cases%>%
                       dplyr::mutate(prev_updated=positive/total)

# We have a case erroneous reported with a negative value
# What are your options?
# 1. Never delete data
# 2. Query and have DM team make the necessary investigations and make a correction

data_prev%>%
    dplyr::filter(prev_updated <= 0 | prev_updated >= 1)

# For now (in order to proceed with this demo), we drop the problematic observation
# Why is this not working?

data_use <- data_prev%>%
              dplyr::filter (prev_updated >= 0 | prev_updated <= 1)

# Why is this working?
data_use <- data_prev%>%
             dplyr::filter (prev_updated >= 0 )%>%
              dplyr::filter (prev_updated <= 1)

data_use%>%
       dplyr::filter(prev_updated <= 0 | prev_updated >= 1)

####################################################################################
##### 2.  Look at summary statistics
####################################################################################
# Summary stats by location (across all time points)

data_use%>%
   dplyr::group_by(location)%>%
     dplyr::summarise(nobs=n(),
                      mean_prev=mean(prev_updated),
                      min_prev=min(prev_updated),
                      max_prev=max(prev_updated))

# Summary stats by location and year (across all time points)
# Table getting longer. Might be too cumbersome to add checks by month and age group
# Note: point of query - why just 3 measurements in 2020? 

data_use%>%
  dplyr::group_by(location, year)%>%
  dplyr::summarise(nobs=n(),
                   mean_prev=mean(prev_updated),
                   min_prev=min(prev_updated),
                   max_prev=max(prev_updated))
  

# Slightly more advanced. Use of lists (not scope of the course but there is a point here)

data_use_list <- data_use%>%
                  dplyr::group_split(location)



data_use_age_summary <- purrr::map(.x=seq(length(data_use_list)),
                                   .f=function(x){
                                    
                                     data_use_list[[x]]%>%
                                       dplyr::group_by(location,year,ages)%>%
                                       dplyr::summarise(nobs=n(),
                                                        mean_prev=mean(prev_updated),
                                                        min_prev=min(prev_updated),
                                                        max_prev=max(prev_updated)) 
                                     
                                   })
  
  
data_use_age_summary


### Now let's focus on the first list object (mordor)                         
# We know pregnant mothers, children <5 are most vulnerable
# Output (ages) isn't ordered as we would want (chronologically)

data_mordor <- data_use_age_summary[[1]]

data_mordor

### How to proceed?

age_order <- c("under_5","5_to_14","15_above")

data_use_ordered <- data_use

data_use_ordered$age_group <- factor(data_use$ages, levels =age_order)

data_mordor_reordered <- data_use_ordered%>%
                           dplyr::group_by(location, year,age_group)%>%
                            dplyr::summarise(nobs=n(),
                                             mean_prev=mean(prev_updated),
                                             min_prev=min(prev_updated),
                                             max_prev=max(prev_updated))%>%
                                 dplyr::filter(location=="mordor")


### Compare 
data_mordor
data_mordor_reordered

####################################################################################
##### 3.  Use of graphs
####################################################################################
# Need to assess the evolution of prevalence for all regions by month

evolution_plot <- ggplot2::ggplot(data=data_use_ordered,
                                  mapping=aes(x=month,
                                              y=prev_updated,
                                              group=location,
                                              colour=location))+
                        ggplot2::geom_line(lwd=1.1)+
                           ggplot2::facet_wrap(~year)+ 
                            ggplot2::theme_bw()+
                             ggplot2::xlab("Month of the Year")+
                               ggplot2::ylab("Prevalence")+
                                ggplot2::scale_x_discrete(limits=factor(1:12),
                                                          labels=c("J","F","M",
                                                                   "A","M","J",
                                                                   "J","A","S",
                                                                   "O","N","D"))+
                                   ggplot2::scale_y_continuous(breaks=seq(from=0,
                                                                          to=0.7,
                                                                          by=0.1))

evolution_plot

# Observation: Prevalence graph with vertical lines per month and year, means
#              we have several subgroups for prevalence data, we plot facets for levels of `age_group`
evolution_plot_ages <- ggplot2::ggplot(data=data_use_ordered,
                                  mapping=aes(x=month,
                                              y=prev_updated,
                                              group=location,
                                              colour=location))+
  ggplot2::geom_line(lwd=1.1)+
  ggplot2::facet_wrap(age_group~year)+ 
  ggplot2::theme_bw()+
  ggplot2::xlab("Month of the Year")+
  ggplot2::ylab("Prevalence")+
  ggplot2::scale_x_discrete(limits=factor(1:12),
                            labels=c("J","F","M",
                                     "A","M","J",
                                     "J","A","S",
                                     "O","N","D"))+
  ggplot2::scale_y_continuous(breaks=seq(from=0,
                                         to=0.7,
                                         by=0.1))

evolution_plot_ages
# Observation: Some improvements, but we still have vertical lines, maybe we have 
#              other group variables. Let's only look at those rows that have more than
#              one entry per location, month, year, age_group

data_use_ordered%>%
  group_by(location,month,year,age_group)%>%
  tally()%>%
  filter(n>1)%>%
  left_join(data_use_ordered)
# Observation: OK, we see that within one location there are several prevalence data
#              points, they differ by the `xcoord` and `ycoord`. In order to plot 
#              by location, we could average across `xcoord` and `ycoord` witin
#              each location; maybe those are duplicated recordings, since `xcoord` and `ycoord`
#              are very close?

data_use_ordered%>%
  group_by(location,month,year,age_group)%>%
  summarize(prev_updated_mean=mean(prev_updated),
            prev_updated_min=min(prev_updated),
            prev_updated_max=max(prev_updated))%>%
  ggplot2::ggplot(mapping=aes(x=month,
                              y=prev_updated_mean,
                              file=location,
                              group=location,
                              colour=location))+
  ggplot2::geom_line(lwd=1.1)+
  ggplot2::facet_wrap(age_group~year)+ 
  ggplot2::theme_bw()+
  ggplot2::xlab("Month of the Year")+
  ggplot2::ylab("Prevalence")+
  ggplot2::scale_x_discrete(limits=factor(1:12),
                            labels=c("J","F","M",
                                     "A","M","J",
                                     "J","A","S",
                                     "O","N","D"))+
  ggplot2::scale_y_continuous(breaks=seq(from=0,
                                         to=0.7,
                                         by=0.1))

# Observation: Prevalence widely variable throughout they year across the locations
#              on average, wonderland affected by high prevalence while oz has the
#              lowest prevalence






# Need to check (not just prevalence) but count of cases and total vulnerable

data_use_ordered_long <- tidyr::pivot_longer(data=data_use_ordered,
                                             cols=c("positive","total"),
                                             names_to="Outcome",
                                             values_to="counts")



mordor_stacked_bar_graph <- ggplot2::ggplot(data=data_use_ordered_long%>%
                                                       dplyr::filter(location=="mordor"),
                                                 mapping=aes(x=month,
                                                             y=counts,
                                                             fill=Outcome))+
                                       ggplot2::scale_x_discrete(limits=factor(1:12),
                                                                 labels=c("J","F","M",
                                                                          "A","M","J",
                                                                          "J","A","S",
                                                                          "O","N","D"))+
                                           ggplot2::geom_bar(position="stack", stat="identity")+
                                             ggplot2::facet_wrap(~year)+ 
                                               ggplot2::theme_bw()+
                                                 ggplot2::xlab("Month of the Year")+
                                                    ggplot2::ylab("Count")

mordor_stacked_bar_graph
# Observation: stacked bar graph adds positive and total counts, better to show 
#              them side by side as positive counts are a subset of the total counts

mordor_dodged_bar_graph <- ggplot2::ggplot(data=data_use_ordered_long%>%
                                              dplyr::filter(location=="mordor"),
                                            mapping=aes(x=month,
                                                        y=counts,
                                                        fill=Outcome))+
  ggplot2::scale_x_discrete(limits=factor(1:12),
                            labels=c("J","F","M",
                                     "A","M","J",
                                     "J","A","S",
                                     "O","N","D"))+
  ggplot2::geom_bar(position="dodge", stat="identity")+
  ggplot2::facet_wrap(~year)+ 
  ggplot2::theme_bw()+
  ggplot2::xlab("Month of the Year")+
  ggplot2::ylab("Count")

mordor_dodged_bar_graph


####################################################################################
##### The mosquitoe HLC data
####################################################################################
mosq_data <- readr::read_csv("data/mosq_mock1.csv")

## Let's check the sanity of this data set
mosq_data %>%
  map( function(x) table(x) )

## The columns `Village` and `Method` seem to have some data entry errors
mosq_data<-mosq_data%>%
  mutate(Method=ifelse(Method=="ALC","HLC",Method),
         Village=ifelse(Village=="naernia","narnia",Village))

## What are the data types of each column?
mosq_data %>%
  map(typeof)


