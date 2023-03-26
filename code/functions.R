

# create scale ------------------------------------------------------------


# function to generate scale by averaging z-scores of each q on a topic
# x -- matrix of responses 
gen_std_scale = function(x){
  x_stdize = apply(x, 2, function(z){
    (z - mean(z,na.rm=TRUE)) / sd(z,na.rm=TRUE)
  })
  # use the first column as the directionality reference
  for (i in 2:ncol(x_stdize)) {
    if (cor(x_stdize[, 1], x_stdize[, i], use = "pa") < 0){
      x_stdize[, i] = x_stdize[, i] * -1
    }
  }
  if (any(sign(cor(x_stdize, use="pa")) == -1)) warning("Not all items are positively correlated")
  num_nonmiss = apply(x_stdize, 1, function(x) sum(!is.na(x)))
  out = apply(x_stdize, 1, mean, na.rm=TRUE)
  out[num_nonmiss < 3] = NA
  out = (out - mean(out, na.rm=TRUE)) / sd(out, na.rm=TRUE)
  std_cor = cor(x_stdize, use = "pa")
  avg_cor = mean(as.numeric(std_cor)[1:(length(std_cor)/2)])
  message(paste0("Average interitem correlation after standardizing columns is ", round(avg_cor, 3)))
  return(out)
}


# density estimate at user-specified grid ---------------------------------

dens_at_grid = Vectorize(function(point, data, ...){
  stopifnot(length(point) == 1)  
  density(data, from = point, to = point, n = 1, ...)$y
}, "point")




# loess w/ gcv optimal bw -------------------------------------------------

# calculate generalized cross validation statistic of a model
loess.gcv = function(span, y, x, ...){
  mod = loess(y ~ x, span = span, ...)
  gcv = (1 / mod$n) * sum((mod$residuals / (1 - (mod$trace.hat / mod$n)))^2)
  return(gcv)
}

# find span that minimizes gcv
loess.opt.span = function(y, x, interval = c(.1, 5), ...){
  out = optimize(loess.gcv, interval = interval, y = y, x = x, ...)
  return(out$minimum)
}

# function to find gcv-optimal bandwidth and then return the loess with
# that bandwidth
loess.opt = function(y, x, interval = NULL, ...){
  if (!is.null(interval)){
    loess(y~x, span = loess.opt.span(y, x, interval = interval, ...), ...)
  } else {
    loess(y~x, span = loess.opt.span(y, x, ...), ...)
  }
}


# cumulative vote switching by racism_scale -------------------------------

calc_vote_switching_race = function(
  st, # char --> state we're analyzing
  real_cces_data, # cces data frame
  prediction_df,  # df that contains vote predictions by racism & 2012 voting
  n_obama_voters, # number of votes for obama in 2012
  n_romney_voters  # number of votes for romney in 2012 
){
  require(assertthat)
  filter = dplyr::filter
  
  vote_switchers_summary = data.frame(
    expand.grid(pattern = c("Obama-Trump", "Romney-Clinton"),
                type = c("Model-based", "Survey-based"))
  )
  vote_switchers_summary$proportion = NA
  vote_switchers_summary$number     = NA
  
  
  # subset to the state
  state_df = filter(real_cces_data, state == st)
  assert_that(nrow(state_df) > 0)
  
  ## 1) Obama voter analysis
  # racism distribution within state among Obama voters
  obama_racism = state_df$racism_scale[state_df$pres2012 == "Obama"]
  
  # create obama_voters data frame from the prediction df
  obama_voters = filter(prediction_df, pres2012 == "Obama")
  obama_voters$race_density = dens_at_grid(
    obama_voters$racism_scale, 
    data = state_df$racism_scale[state_df$pres2012 == "Obama"],
    weights = state_df$commonweight_vv[state_df$pres2012=="Obama"] / 
      sum(state_df$commonweight_vv[state_df$pres2012=="Obama"])
  )  
  
  # calculate model-based proportion of Obama voters who went for trump in state
  obama_trump_prop_model = sum(obama_voters$trump.uncondo.pred * (obama_voters$race_density / sum(obama_voters$race_density)))
  vote_switchers_summary$proportion[vote_switchers_summary$pattern == "Obama-Trump" & vote_switchers_summary$type == "Model-based"] = obama_trump_prop_model
  
  # compare it to survey-based estimate
  obama_trump_prop_svy = weighted.mean(
    state_df$trump[state_df$pres2012=="Obama"], 
    w = state_df$commonweight_vv[state_df$pres2012=="Obama"], 
    na.rm=T
  )
  vote_switchers_summary$proportion[vote_switchers_summary$pattern == "Obama-Trump" & vote_switchers_summary$type == "Survey-based"] = obama_trump_prop_svy
  
  # Calculate cumulative number of votes from Obama voters, by racism_scale
  obama_voters = arrange(obama_voters, racism_scale)
  
  # Calculate cumulative number of Obama -> Trump voters by racism_scale
  cumulative_obama_trump = 
    n_obama_voters * 
    cumsum(obama_voters$trump.uncondo.pred * (obama_voters$race_density / sum(obama_voters$race_density)))
  
  cumulative_obama_trump = data.frame(racism_scale = obama_voters$racism_scale, 
                                      prob = cumulative_obama_trump)
  
  
  # Calculate cumulative number of Obama -> Clinton voters by racism_scale
  cumulative_obama_clinton = 
    n_obama_voters * 
    cumsum(obama_voters$clinton.uncondo.pred * (obama_voters$race_density / sum(obama_voters$race_density)))
  
  cumulative_obama_clinton = data.frame(racism_scale = obama_voters$racism_scale, 
                                        prob = cumulative_obama_clinton)
  
  
  
  ## 2) Romney voter analysis
  # racism distribution within state among romney voters
  romney_racism = state_df$racism_scale[state_df$pres2012 == "Romney"]
  
  # create romney_voters data frame from the prediction df
  romney_voters = filter(prediction_df, pres2012 == "Romney")
  romney_voters$race_density = dens_at_grid(
    romney_voters$racism_scale, 
    data = state_df$racism_scale[state_df$pres2012 == "Romney"],
    weights = state_df$commonweight_vv[state_df$pres2012=="Romney"] / 
      sum(state_df$commonweight_vv[state_df$pres2012=="Romney"])
  )  
  
  # calculate model-based proportion of Romney voters who went for Clinton in state
  romney_clinton_prop_model = sum(romney_voters$clinton.uncondo.pred * (romney_voters$race_density / sum(romney_voters$race_density)))
  vote_switchers_summary$proportion[vote_switchers_summary$pattern == "Romney-Clinton" & vote_switchers_summary$type == "Model-based"] = romney_clinton_prop_model
  
  # compare it to survey-based estimate
  romney_clinton_prop_svy = weighted.mean(
    state_df$clinton[state_df$pres2012=="Romney"], 
    w = state_df$commonweight_vv[state_df$pres2012=="Romney"], 
    na.rm=T
  )
  vote_switchers_summary$proportion[vote_switchers_summary$pattern == "Romney-Clinton" & vote_switchers_summary$type == "Survey-based"] = romney_clinton_prop_svy
  
  # Calculate cumulative number of votes from romney voters, by racism_scale
  romney_voters = arrange(romney_voters, racism_scale)
  
  # Calculate cumulative number of Romney -> Clinton voters by racism_scale
  cumulative_romney_clinton = 
    n_romney_voters * 
    cumsum(romney_voters$clinton.uncondo.pred * (romney_voters$race_density / sum(romney_voters$race_density)))
  
  cumulative_romney_clinton = data.frame(racism_scale = romney_voters$racism_scale, 
                                         prob = cumulative_romney_clinton)
  
  
  # Calculate cumulative number of Romney -> Trump voters by racism_scale
  cumulative_romney_trump = 
    n_romney_voters * 
    cumsum(romney_voters$trump.uncondo.pred * (romney_voters$race_density / sum(romney_voters$race_density)))
  
  cumulative_romney_trump = data.frame(racism_scale = romney_voters$racism_scale, 
                                       prob = cumulative_romney_trump)
  
  
  
  
  ## 3) Merge cumulative vote data frames
  cumulative_obama_clinton = cumulative_obama_clinton %>% 
    rename(cumvotes_obama_clinton = prob)
  cumulative_obama_trump = cumulative_obama_trump %>% 
    rename(cumvotes_obama_trump = prob)
  cumulative_romney_clinton = cumulative_romney_clinton %>% 
    rename(cumvotes_romney_clinton = prob)
  cumulative_romney_trump = cumulative_romney_trump %>% 
    rename(cumvotes_romney_trump = prob)
  
  cumulative_obama  = merge(cumulative_obama_clinton, cumulative_obama_trump, by = "racism_scale")
  cumulative_romney = merge(cumulative_romney_clinton, cumulative_romney_trump, by = "racism_scale")
  cumulative = merge(cumulative_obama, cumulative_romney, by = "racism_scale")
  cumulative$net_trump_vote_switchers = cumulative$cumvotes_obama_trump - cumulative$cumvotes_romney_clinton
  cumulative$cumvotes_clinton = cumulative$cumvotes_obama_clinton + cumulative$cumvotes_romney_clinton
  cumulative$cumvotes_trump = cumulative$cumvotes_obama_trump + cumulative$cumvotes_romney_trump
  
  
  ## return analysis
  out = list(vote_switch_summary = vote_switchers_summary,
             cumulative_race_votes = cumulative)
  
  return(out)
}




# HH income change analysis  ------------------------------

hhincchange_analysis = function(data12, data16, plot.title = NULL, 
                                coords = TRUE, plot = TRUE){
  
  
  # print out sample sizes
  message(paste0("In 2012, N = ", prettyNum(nrow(data12), big.mark = ",")))
  message(paste0("In 2016, N = ", prettyNum(nrow(data16), big.mark = ",")))
  
  ## household income change
  income_grid = data.frame(hhinc_change = levels(data16$hhinc_change)[1:5])
  
  # turnout on income scale
  turnout16 = questionr::wtd.table(data16$validated_turnout, data16$hhinc_change, weights = data16$commonweight_vv)
  turnout16 = as.data.frame(t(apply(turnout16, 2, function(x) x / sum(x))))
  turnout16[,"0"] = NULL
  names(turnout16) = "pr_turnout_16"
  turnout16$hhinc_change = rownames(turnout16)
  
  turnout12 = questionr::wtd.table(data12$validated_turnout, data12$hhinc_change, weights = data12$weight_vv)
  turnout12 = as.data.frame(t(apply(turnout12, 2, function(x) x / sum(x))))
  turnout12[,"0"] = NULL
  names(turnout12) = "pr_turnout_12"
  turnout12$hhinc_change = rownames(turnout12)
  
  # add turnout to income grid
  income_grid = left_join(income_grid, turnout12, by = "hhinc_change")
  income_grid = left_join(income_grid, turnout16, by = "hhinc_change")
  
  # vote choice conditional on turnout -- 2016
  vote16_rep = with(subset(data16, validated_turnout == 1), 
                    questionr::wtd.table(rep16_vv, hhinc_change, weights = commonweight_vv))
  vote16_rep = apply(vote16_rep, 2, function(x) x / sum(x))
  vote16_rep = as.data.frame(t(vote16_rep))
  vote16_rep[, "0"] = NULL
  names(vote16_rep) = "pr_voterep_16"
  vote16_rep$hhinc_change = rownames(vote16_rep)
  
  vote16_dem = with(subset(data16, validated_turnout == 1), 
                    questionr::wtd.table(dem16_vv, hhinc_change, weights = commonweight_vv))
  vote16_dem = apply(vote16_dem, 2, function(x) x / sum(x))
  vote16_dem = as.data.frame(t(vote16_dem))
  vote16_dem[, "0"] = NULL
  names(vote16_dem) = "pr_votedem_16"
  vote16_dem$hhinc_change = rownames(vote16_dem)
  
  # add 2016 vote choice to income grid
  income_grid = left_join(income_grid, vote16_dem, by = "hhinc_change")
  income_grid = left_join(income_grid, vote16_rep, by = "hhinc_change")
  
  # vote choice conditional on turnout -- 2012
  vote12_rep = with(subset(data12, validated_turnout == 1), 
                    questionr::wtd.table(rep12_vv, hhinc_change, weights = weight_vv))
  vote12_rep = apply(vote12_rep, 2, function(x) x / sum(x))
  vote12_rep = as.data.frame(t(vote12_rep))
  vote12_rep[, "0"] = NULL
  names(vote12_rep) = "pr_voterep_12"
  vote12_rep$hhinc_change = rownames(vote12_rep)
  
  vote12_dem = with(subset(data12, validated_turnout == 1), 
                    questionr::wtd.table(dem12_vv, hhinc_change, weights = weight_vv))
  vote12_dem = apply(vote12_dem, 2, function(x) x / sum(x))
  vote12_dem = as.data.frame(t(vote12_dem))
  vote12_dem[, "0"] = NULL
  names(vote12_dem) = "pr_votedem_12"
  vote12_dem$hhinc_change = rownames(vote12_dem)
  
  # add 2012 vote choice to income grid
  income_grid = left_join(income_grid, vote12_dem, by = "hhinc_change")
  income_grid = left_join(income_grid, vote12_rep, by = "hhinc_change")
  
  
  
  # evaluate density of income change in each year
  density_16 = questionr::wtd.table(data16$hhinc_change, weights = data16$commonweight_vv)[1:5]
  density_16 = as.data.frame(density_16)
  names(density_16) = c("hhinc_change", "density_16")
  density_16$density_16 = as.numeric(density_16$density_16 / sum(density_16$density_16))
  
  
  density_12 = questionr::wtd.table(data12$hhinc_change, weights = data12$weight_vv)[1:5]
  density_12 = as.data.frame(density_12)
  names(density_12) = c("hhinc_change", "density_12")
  density_12$density_12 = as.numeric(density_12$density_12 / sum(density_12$density_12))
  
  income_grid = left_join(income_grid, density_16, by = "hhinc_change")
  income_grid = left_join(income_grid, density_12, by = "hhinc_change")
  
  
  # multiply distributions together
  income_grid$rep_dens_16 = with(income_grid, density_16 * pr_turnout_16 * pr_voterep_16)
  income_grid$dem_dens_16 = with(income_grid, density_16 * pr_turnout_16 * pr_votedem_16)
  
  income_grid$rep_dens_12 = with(income_grid, density_12 * pr_turnout_12 * pr_voterep_12)
  income_grid$dem_dens_12 = with(income_grid, density_12 * pr_turnout_12 * pr_votedem_12)
  
  
  # calculate 2016 - 2012 change
  income_grid$dem_dens_diff = with(income_grid, dem_dens_16 - dem_dens_12)
  income_grid$rep_dens_diff = with(income_grid, rep_dens_16 - rep_dens_12)
  
  # calculate dem-rep diff in each year
  income_grid$vs_diff_12 = with(income_grid, rep_dens_12 - dem_dens_12)
  income_grid$vs_diff_16 = with(income_grid, rep_dens_16 - dem_dens_16)
  
  # calculate "diff in diff" 
  income_grid$vs_diff_diff = with(income_grid, vs_diff_16 - vs_diff_12)
  
  # calculate cleavage absolute value statistic
  overall_margin = abs(sum(income_grid$vs_diff_diff))
  ngroups = nrow(income_grid)
  cleavage_var = sum((income_grid$vs_diff_diff - (overall_margin/ngroups))^2)
  cleavage_abs = sum(abs(income_grid$vs_diff_diff - (overall_margin/ngroups)))
  
  # fix labels for plotting
  income_grid$hhinc_change = as.character(income_grid$hhinc_change)
  income_grid$hhinc_change = gsub(" a lot", "\na lot", income_grid$hhinc_change)
  income_grid$hhinc_change = gsub(" somewhat", "\nsomewhat", income_grid$hhinc_change)
  income_grid$hhinc_change = gsub(" the same", "\nthe same", income_grid$hhinc_change)
  income_grid$hhinc_change = factor(income_grid$hhinc_change, 
                                    c('Increased\na lot', 'Increased\nsomewhat',
                                      'Stayed about\nthe same','Decreased\nsomewhat',
                                      'Decreased\na lot'))
  
  # split into 2 dfs then put back together -- one for each year
  income_grid_12 = income_grid %>% 
    select(hhinc_change, contains("12")) %>% 
    mutate(year = "2012")
  names(income_grid_12) = gsub("_12", "", names(income_grid_12))
  
  income_grid_16 = income_grid %>% 
    select(hhinc_change, contains("16")) %>% 
    mutate(year = "2016")
  names(income_grid_16) = gsub("_16", "", names(income_grid_16))
  
  income_grid2 = bind_rows(income_grid_12, income_grid_16)
  
  # make plots
  if(plot){
    # density 
    ggdens = ggplot(income_grid2) + 
      aes(x = hhinc_change, y = density, fill = year) + 
      geom_bar(stat = "identity", position = position_dodge()) + 
      scale_fill_grey(name = NULL) + 
      labs(x = "Household income change", y = "Density") + 
      theme(panel.grid = element_blank()) + 
      coord_cartesian(ylim = c(0, .5)) + 
      ggtitle(plot.title)
    
    # pr(turnout | hhinc_change)
    ggturnout = ggplot(income_grid2) + 
      aes(x = as.numeric(hhinc_change), y = pr_turnout, lty = year) + 
      # geom_bar(stat = "identity") + 
      geom_point(cex = 2, pch = 20) + 
      geom_line() + 
      coord_cartesian(ylim = c(0, 1)) + 
      scale_linetype(name = NULL) + 
      scale_x_continuous(labels = income_grid$hhinc_change) + 
      labs(x = "Household income change", y = "Turnout") + 
      theme(panel.grid = element_blank())  + 
      ggtitle(plot.title)
    
    # pr(vote | hhinc_change)
    ggvoteincome = ggplot(income_grid2) + 
      aes(x = as.numeric(hhinc_change), lty = year) + 
      geom_line(aes(y = pr_voterep, colour = "Republican")) + 
      geom_line(aes(y = pr_votedem, colour = "Democrat")) + 
      geom_point(aes(y = pr_voterep, colour = "Republican"), cex = 2, pch = 20) + 
      geom_point(aes(y = pr_votedem, colour = "Democrat"), cex = 2, pch = 20) + 
      coord_cartesian(ylim = c(0, 1)) + 
      scale_x_continuous(labels = income_grid$hhinc_change) + 
      scale_colour_manual(name = NULL, values = c("Republican" = scales::muted("red"), "Democrat" = scales::muted("blue"))) + 
      scale_linetype(name = NULL) + 
      labs(x = "Household income change", y = "Pr(Vote | Turnout)") + 
      theme(panel.grid = element_blank()) +
      ggtitle(plot.title)
    
    
    # make vote plot
    ggvotedens = ggplot(income_grid2) + 
      aes(x = as.numeric(hhinc_change), lty = year) + 
      geom_line(aes(y = rep_dens, colour = "Republican")) + 
      geom_line(aes(y = dem_dens, colour = "Democrat")) + 
      geom_point(aes(y = rep_dens, colour = "Republican"), cex = 2, pch = 20) + 
      geom_point(aes(y = dem_dens, colour = "Democrat"), cex = 2, pch = 20) + 
      scale_x_continuous(labels = income_grid$hhinc_change) + 
      scale_colour_manual(name = NULL, values = c("Republican" = scales::muted("red"), "Democrat" = scales::muted("blue"))) + 
      scale_linetype(name = NULL) + 
      labs(x = "Household income change", y = "Vote density") + 
      theme(panel.grid = element_blank()) +
      ggtitle(plot.title)
    if(coords) ggvotedens = ggvotedens + coord_cartesian(ylim = c(0, .2)) 
    
    
    # plot differences over time
    ggincdiff = ggplot(income_grid) + 
      aes(x = as.numeric(hhinc_change)) + 
      geom_hline(yintercept = 0, colour = "grey50") + 
      geom_line(aes(y = rep_dens_diff, colour = "Republican")) + 
      geom_line(aes(y = dem_dens_diff, colour = "Democrat")) + 
      geom_point(aes(y = rep_dens_diff, colour = "Republican"), cex = 2, pch = 20) + 
      geom_point(aes(y = dem_dens_diff, colour = "Democrat"), cex = 2, pch = 20) + 
      scale_x_continuous(labels = income_grid$hhinc_change) + 
      scale_colour_manual(name = NULL, values = c("Republican" = scales::muted("red"), "Democrat" = scales::muted("blue"))) + 
      scale_linetype(name = NULL) + 
      labs(x = "Household income change", y = "Shift in vote density from 2012 to 2016") + 
      theme(panel.grid = element_blank())  +
      ggtitle(plot.title)
    if (coords) ggincdiff = ggincdiff +  coord_cartesian(ylim = c(-.06, .06))
    
    
    # compare dem and rep in same election
    ggrepdemdiff = ggplot(income_grid) + 
      aes(x = as.numeric(hhinc_change)) + 
      geom_hline(yintercept = 0, colour = "grey50") + 
      geom_line(aes(y = vs_diff_12, lty = "2012")) + 
      geom_line(aes(y = vs_diff_16, lty = "2016")) + 
      geom_point(aes(y = vs_diff_12), cex = 2, pch = 20) + 
      geom_point(aes(y = vs_diff_16), cex = 2, pch = 20) + 
      scale_x_continuous(labels = income_grid$hhinc_change) + 
      scale_y_continuous(breaks = round(seq(-.075, .075, .025),3)) + 
      scale_linetype_manual(name = NULL, values = c("2012" = 1, "2016" = 3)) + 
      labs(x = "Household income change", y = "Rep. bloc size minus Dem. bloc size") + 
      theme(panel.grid = element_blank()) + 
      ggtitle(plot.title)
    if(coords) ggrepdemdiff = ggrepdemdiff + coord_cartesian(ylim = c(-.075, .075)) 
    
    
    # compare diff in diff
    ggdiffdiff = ggplot(income_grid) + 
      aes(x = as.numeric(hhinc_change), y = vs_diff_diff) + 
      geom_point(cex = 2, pch = 20) + 
      geom_hline(yintercept = 0, colour = "grey50") + 
      geom_line() + 
      scale_x_continuous(labels = income_grid$hhinc_change) + 
      labs(x = "Household income change", y = "(Rep '16 - Dem '16) -\n(Rep '12 - Dem '12)") + 
      theme(panel.grid = element_blank())  +
      ggtitle(plot.title)
    if(coords) ggdiffdiff = ggdiffdiff + coord_cartesian(ylim = c( -.05, .05)) 
    
    out = list(results_wide = income_grid, 
               results_long = income_grid2, 
               n12 = nrow(data12),
               n16 = nrow(data16),
               cleavage_var = sqrt(cleavage_var),
               cleavage_abs = cleavage_abs,
               plots = list(dens = ggdens, 
                            turnout = ggturnout, 
                            vote = ggvoteincome, 
                            votedens = ggvotedens, 
                            diff = ggincdiff, 
                            repdemdiff = ggrepdemdiff, 
                            diffdiff = ggdiffdiff))
    return(out)
  } else {
    out = list(results_wide = income_grid, 
               results_long = income_grid2, 
               n12 = nrow(data12),
               n16 = nrow(data16),
               cleavage_var = sqrt(cleavage_var),
               cleavage_abs = cleavage_abs)
    return(out)
  }
}




# immig scale analysis  ---------------------------------------------------

immigscale_analysis = function(data12, data16, plot.title = NULL, 
                               coords = TRUE, plot = TRUE){
  
  # print out sample sizes
  message(paste0("In 2012, N = ", prettyNum(nrow(data12), big.mark = ",")))
  message(paste0("In 2016, N = ", prettyNum(nrow(data16), big.mark = ",")))
  
  
  
  ## household immig change
  immig_grid = data.frame(immig_scale = round(sort(unique(data16$immig_scale)), 2))
  
  # turnout on immig scale
  turnout16 = questionr::wtd.table(data16$validated_turnout, data16$immig_scale, weights = data16$commonweight_vv)
  turnout16 = as.data.frame(t(apply(turnout16, 2, function(x) x / sum(x))))
  turnout16[,"0"] = NULL
  names(turnout16) = "pr_turnout_16"
  turnout16$immig_scale = round(as.numeric(rownames(turnout16)), 2)
  
  turnout12 = questionr::wtd.table(data12$validated_turnout, data12$immig_scale, weights = data12$weight_vv)
  turnout12 = as.data.frame(t(apply(turnout12, 2, function(x) x / sum(x))))
  turnout12[,"0"] = NULL
  names(turnout12) = "pr_turnout_12"
  turnout12$immig_scale = round(as.numeric(rownames(turnout12)), 2)
  
  # add turnout to immig grid
  immig_grid = left_join(immig_grid, turnout12, by = "immig_scale")
  immig_grid = left_join(immig_grid, turnout16, by = "immig_scale")
  
  # vote choice conditional on turnout -- 2016
  vote16_rep = with(subset(data16, validated_turnout == 1), 
                    questionr::wtd.table(rep16_vv, immig_scale, weights = commonweight_vv))
  vote16_rep = apply(vote16_rep, 2, function(x) x / sum(x))
  vote16_rep = as.data.frame(t(vote16_rep))
  vote16_rep[, "0"] = NULL
  names(vote16_rep) = "pr_voterep_16"
  vote16_rep$immig_scale = round(as.numeric(rownames(vote16_rep)),2)
  
  vote16_dem = with(subset(data16, validated_turnout == 1), 
                    questionr::wtd.table(dem16_vv, immig_scale, weights = commonweight_vv))
  vote16_dem = apply(vote16_dem, 2, function(x) x / sum(x))
  vote16_dem = as.data.frame(t(vote16_dem))
  vote16_dem[, "0"] = NULL
  names(vote16_dem) = "pr_votedem_16"
  vote16_dem$immig_scale = round(as.numeric(rownames(vote16_dem)), 2)
  
  # add 2016 vote choice to immig grid
  immig_grid = left_join(immig_grid, vote16_dem, by = "immig_scale")
  immig_grid = left_join(immig_grid, vote16_rep, by = "immig_scale")
  
  # vote choice conditional on turnout -- 2012
  vote12_rep = with(subset(data12, validated_turnout == 1), 
                    questionr::wtd.table(rep12_vv, immig_scale, weights = weight_vv))
  vote12_rep = apply(vote12_rep, 2, function(x) x / sum(x))
  vote12_rep = as.data.frame(t(vote12_rep))
  vote12_rep[, "0"] = NULL
  names(vote12_rep) = "pr_voterep_12"
  vote12_rep$immig_scale = round(as.numeric(rownames(vote12_rep)), 2)
  
  vote12_dem = with(subset(data12, validated_turnout == 1), 
                    questionr::wtd.table(dem12_vv, immig_scale, weights = weight_vv))
  vote12_dem = apply(vote12_dem, 2, function(x) x / sum(x))
  vote12_dem = as.data.frame(t(vote12_dem))
  vote12_dem[, "0"] = NULL
  names(vote12_dem) = "pr_votedem_12"
  vote12_dem$immig_scale = round(as.numeric(rownames(vote12_dem)), 2)
  
  # add 2012 vote choice to immig grid
  immig_grid = left_join(immig_grid, vote12_dem, by = "immig_scale")
  immig_grid = left_join(immig_grid, vote12_rep, by = "immig_scale")
  
  
  
  # evaluate density of immig change in each year
  density_16 = questionr::wtd.table(data16$immig_scale, weights = data16$commonweight_vv)
  density_16 = as.data.frame(density_16)
  names(density_16) = c("immig_scale", "density_16")
  density_16$density_16 = as.numeric(density_16$density_16 / sum(density_16$density_16))
  density_16$immig_scale = round(as.numeric(as.character(density_16$immig_scale)), 2)
  
  density_12 = questionr::wtd.table(data12$immig_scale, weights = data12$weight_vv)
  density_12 = as.data.frame(density_12)
  names(density_12) = c("immig_scale", "density_12")
  density_12$density_12 = as.numeric(density_12$density_12 / sum(density_12$density_12))
  density_12$immig_scale = round(as.numeric(as.character(density_12$immig_scale)), 2)
  
  
  immig_grid = left_join(immig_grid, density_16, by = "immig_scale")
  immig_grid = left_join(immig_grid, density_12, by = "immig_scale")
  
  
  # multiply distributions together
  immig_grid$rep_dens_16 = with(immig_grid, density_16*pr_turnout_16*pr_voterep_16)
  immig_grid$dem_dens_16 = with(immig_grid, density_16*pr_turnout_16*pr_votedem_16)
  immig_grid$rep_dens_12 = with(immig_grid, density_12*pr_turnout_12*pr_voterep_12)
  immig_grid$dem_dens_12 = with(immig_grid, density_12*pr_turnout_12*pr_votedem_12)
  
  # calculate 2016 - 2012 change
  immig_grid$dem_dens_diff = with(immig_grid, dem_dens_16 - dem_dens_12)
  immig_grid$rep_dens_diff = with(immig_grid, rep_dens_16 - rep_dens_12)
  
  # calculate dem-rep diff in each year
  immig_grid$vs_diff_12 = with(immig_grid, rep_dens_12 - dem_dens_12)
  immig_grid$vs_diff_16 = with(immig_grid, rep_dens_16 - dem_dens_16)
  
  # calculate "diff in diff" 
  immig_grid$vs_diff_diff = with(immig_grid, vs_diff_16 - vs_diff_12)
  
  
  # calculate cleavage stat: sum(| diff in diff |) for each group minus overall margin
  overall_margin = abs(sum(immig_grid$vs_diff_diff))
  n_groups = nrow(immig_grid)
  cleavage_var = sum((immig_grid$vs_diff_diff - (overall_margin/n_groups))^2)
  cleavage_abs = sum(abs(immig_grid$vs_diff_diff - (overall_margin/n_groups)))
  
  
  # split into 2 dfs then put back together -- one for each year
  immig_grid_12 = immig_grid %>% 
    select(immig_scale, contains("12")) %>% 
    mutate(year = "2012")
  names(immig_grid_12) = gsub("_12", "", names(immig_grid_12))
  
  immig_grid_16 = immig_grid %>% 
    select(immig_scale, contains("16")) %>% 
    mutate(year = "2016")
  names(immig_grid_16) = gsub("_16", "", names(immig_grid_16))
  
  immig_grid2 = bind_rows(immig_grid_12, immig_grid_16)
  
  # make plots
  if(plot){
    # density 
    ggdens = ggplot(immig_grid2) + 
      aes(x = immig_scale, y = density, fill = year) + 
      geom_bar(stat = "identity", position = position_dodge()) + 
      scale_fill_grey(name = NULL) + 
      labs(x = "Opposition to immigration", y = "Density") + 
      scale_x_continuous(breaks = c(0.02, 0.98), labels = c("Less opposed", "More opposed")) + 
      theme(panel.grid = element_blank()) +
      ggtitle(plot.title)
    
    # pr(turnout | immig_scale)
    ggturnout = ggplot(immig_grid2) + 
      aes(x = as.numeric(immig_scale), y = pr_turnout, lty = year) + 
      geom_point(cex = 2, pch = 20) + 
      # geom_bar(stat = "identity") + 
      geom_line() + 
      coord_cartesian(ylim = c(0, 1)) + 
      scale_linetype(name = NULL) + 
      scale_x_continuous(breaks = c(0.02, 0.98), labels = c("Less opposed", "More opposed")) + 
      labs(x = "Opposition to immigration", y = "Turnout") + 
      theme(panel.grid = element_blank()) +
      ggtitle(plot.title)
    
    # pr(vote | immig_scale)
    ggvote = ggplot(immig_grid2) + 
      aes(x = as.numeric(immig_scale), lty = year) + 
      geom_point(aes(y = pr_voterep, colour = "Republican"), cex = 2, pch = 20) + 
      geom_point(aes(y = pr_votedem, colour = "Democrat"), cex = 2, pch = 20) + 
      geom_line(aes(y = pr_voterep, colour = "Republican")) + 
      geom_line(aes(y = pr_votedem, colour = "Democrat")) + 
      coord_cartesian(ylim = c(0, 1)) + 
      scale_x_continuous(breaks = c(0.02, 0.98), labels = c("Less opposed", "More opposed")) + 
      scale_colour_manual(name = NULL, values = c("Republican" = scales::muted("red"), "Democrat" = scales::muted("blue"))) + 
      scale_linetype(name = NULL) + 
      labs(x = "Opposition to immigration", y = "Pr(Vote | Turnout)") + 
      theme(panel.grid = element_blank())   + 
      ggtitle(plot.title)
    
    # make vote plot
    ggvotedens = ggplot(immig_grid2) + 
      aes(x = as.numeric(immig_scale), lty = year) + 
      geom_point(aes(y = rep_dens, colour = "Republican"), cex = 2, pch = 20) + 
      geom_point(aes(y = dem_dens, colour = "Democrat"), cex = 2, pch = 20) + 
      geom_line(aes(y = rep_dens, colour = "Republican")) + 
      geom_line(aes(y = dem_dens, colour = "Democrat")) + 
      scale_x_continuous(breaks = c(0.02, 0.98), labels = c("Less opposed", "More opposed")) + 
      scale_colour_manual(name = NULL, values = c("Republican" = scales::muted("red"), "Democrat" = scales::muted("blue"))) + 
      scale_linetype(name = NULL) + 
      labs(x = "Opposition to immigration", y = "Vote density") + 
      theme(panel.grid = element_blank())  +
      ggtitle(plot.title)
    if(coords) ggvotedens = ggvotedens + coord_cartesian(ylim = c(0, .2))
    
    # plot differences over time
    ggdiff = ggplot(immig_grid) + 
      aes(x = as.numeric(immig_scale)) + 
      geom_hline(yintercept = 0, colour = "grey50") + 
      geom_point(aes(y = rep_dens_diff, colour = "Republican"), cex = 2, pch = 20) + 
      geom_point(aes(y = dem_dens_diff, colour = "Democrat"), cex = 2, pch = 20) + 
      geom_line(aes(y = rep_dens_diff, colour = "Republican")) + 
      geom_line(aes(y = dem_dens_diff, colour = "Democrat")) + 
      scale_x_continuous(breaks = c(0.02, 0.98), labels = c("Less opposed", "More opposed")) + 
      scale_colour_manual(name = NULL, values = c("Republican" = scales::muted("red"), "Democrat" = scales::muted("blue"))) + 
      scale_linetype(name = NULL) + 
      labs(x = "Opposition to immigration", y = "Shift in vote density from 2012 to 2016") + 
      theme(panel.grid = element_blank())  +
      ggtitle(plot.title)
    if (coords) ggdiff = ggdiff + coord_cartesian(ylim = c(-.05, .05)) 
    
    
    # compare dem and rep in same election
    ggrepdemdiff = ggplot(immig_grid) + 
      aes(x = as.numeric(immig_scale)) + 
      geom_hline(yintercept = 0, colour = "grey50") + 
      geom_point(aes(y = vs_diff_12), cex = 2, pch = 20) + 
      geom_point(aes(y = vs_diff_16), cex = 2, pch = 20) + 
      geom_line(aes(y = vs_diff_12, lty = "2012")) + 
      geom_line(aes(y = vs_diff_16, lty = "2016")) + 
      scale_x_continuous(breaks = c(0.02, 0.98), labels = c("Less opposed", "More opposed")) + 
      scale_linetype_manual(name = NULL, values = c("2012" = 1, "2016" = 3)) + 
      labs(x = "Opposition to immigration", y = "Rep. bloc size minus Dem. bloc size") + 
      theme(panel.grid = element_blank())  +
      ggtitle(plot.title)
    if(coords) ggrepdemdiff = ggrepdemdiff + coord_cartesian(ylim = c(-.12, .12)) 
    
    
    # compare diff in diff
    ggdiffdiff = ggplot(immig_grid) + 
      aes(x = as.numeric(immig_scale), y = vs_diff_diff) + 
      geom_point(cex = 2, pch = 20) +
      geom_hline(yintercept = 0, colour = "grey50") + 
      geom_line() + 
      scale_x_continuous(breaks = c(0.02, 0.98), labels = c("Less opposed", "More opposed")) + 
      labs(x = "Opposition to immigration", y = "(Rep '16 - Dem '16) -\n(Rep '12 - Dem '12)") + 
      theme(panel.grid = element_blank())  +
      ggtitle(plot.title)
    if(coords) ggdiffdiff = ggdiffdiff + coord_cartesian(ylim = c( -.05, .05)) 
    
    out = list(results_wide = immig_grid, 
               results_long = immig_grid2, 
               n12 = nrow(data12),
               n16 = nrow(data16),
               cleavage_var = sqrt(cleavage_var),
               cleavage_abs = cleavage_abs,
               plots = list(dens = ggdens, 
                            turnout = ggturnout, 
                            vote = ggvote, 
                            votedens = ggvotedens, 
                            diff = ggdiff, 
                            repdemdiff = ggrepdemdiff, 
                            diffdiff = ggdiffdiff))
    return(out)
  } else {
    out = list(results_wide = immig_grid, 
               results_long = immig_grid2, 
               n12 = nrow(data12),
               n16 = nrow(data16),
               cleavage_var = sqrt(cleavage_var),
               cleavage_abs = cleavage_abs)
    return(out)
  }
}





# race analysis -----------------------------------------------------------

race_analysis = function(data12, data16, plot.title = NULL, racevar = "race2",
                         raceorder = c("White", "Black", "Hispanic", "Asian", "Other"),
                         coords = TRUE, plot = TRUE){
  
  # print out sample sizes
  message(paste0("In 2012, N = ", prettyNum(nrow(data12), big.mark = ",")))
  message(paste0("In 2016, N = ", prettyNum(nrow(data16), big.mark = ",")))
  
  data12$race = data12[[racevar]]
  data16$race = data16[[racevar]]
  
  
  ## household race change
  race_grid = data.frame(race = sort(unique(data16$race)))
  
  # turnout on race scale
  turnout16 = questionr::wtd.table(data16$validated_turnout, data16$race, weights = data16$commonweight_vv)
  turnout16 = as.data.frame(t(apply(turnout16, 2, function(x) x / sum(x))))
  turnout16[,"0"] = NULL
  names(turnout16) = "pr_turnout_16"
  turnout16$race = rownames(turnout16)
  
  turnout12 = questionr::wtd.table(data12$validated_turnout, data12$race, weights = data12$weight_vv)
  turnout12 = as.data.frame(t(apply(turnout12, 2, function(x) x / sum(x))))
  turnout12[,"0"] = NULL
  names(turnout12) = "pr_turnout_12"
  turnout12$race = rownames(turnout12)
  
  # add turnout to race grid
  race_grid = left_join(race_grid, turnout12, by = "race")
  race_grid = left_join(race_grid, turnout16, by = "race")
  
  # vote choice conditional on turnout -- 2016
  vote16_rep = with(subset(data16, validated_turnout == 1), 
                    questionr::wtd.table(rep16_vv, race, weights = commonweight_vv))
  vote16_rep = apply(vote16_rep, 2, function(x) x / sum(x))
  vote16_rep = as.data.frame(t(vote16_rep))
  vote16_rep[, "0"] = NULL
  names(vote16_rep) = "pr_voterep_16"
  vote16_rep$race = rownames(vote16_rep)
  
  vote16_dem = with(subset(data16, validated_turnout == 1), 
                    questionr::wtd.table(dem16_vv, race, weights = commonweight_vv))
  vote16_dem = apply(vote16_dem, 2, function(x) x / sum(x))
  vote16_dem = as.data.frame(t(vote16_dem))
  vote16_dem[, "0"] = NULL
  names(vote16_dem) = "pr_votedem_16"
  vote16_dem$race = rownames(vote16_dem)
  
  # add 2016 vote choice to race grid
  race_grid = left_join(race_grid, vote16_dem, by = "race")
  race_grid = left_join(race_grid, vote16_rep, by = "race")
  
  # vote choice conditional on turnout -- 2012
  vote12_rep = with(subset(data12, validated_turnout == 1), 
                    questionr::wtd.table(rep12_vv, race, weights = weight_vv))
  vote12_rep = apply(vote12_rep, 2, function(x) x / sum(x))
  vote12_rep = as.data.frame(t(vote12_rep))
  vote12_rep[, "0"] = NULL
  names(vote12_rep) = "pr_voterep_12"
  vote12_rep$race = rownames(vote12_rep)
  
  vote12_dem = with(subset(data12, validated_turnout == 1), 
                    questionr::wtd.table(dem12_vv, race, weights = weight_vv))
  vote12_dem = apply(vote12_dem, 2, function(x) x / sum(x))
  vote12_dem = as.data.frame(t(vote12_dem))
  vote12_dem[, "0"] = NULL
  names(vote12_dem) = "pr_votedem_12"
  vote12_dem$race = rownames(vote12_dem)
  
  # add 2012 vote choice to race grid
  race_grid = left_join(race_grid, vote12_dem, by = "race")
  race_grid = left_join(race_grid, vote12_rep, by = "race")
  
  
  
  # evaluate density of race change in each year
  density_16 = questionr::wtd.table(data16$race, weights = data16$commonweight_vv)
  density_16 = as.data.frame(density_16)
  names(density_16) = c("race", "density_16")
  density_16$density_16 = as.numeric(density_16$density_16 / sum(density_16$density_16))
  
  
  density_12 = questionr::wtd.table(data12$race, weights = data12$weight_vv)
  density_12 = as.data.frame(density_12)
  names(density_12) = c("race", "density_12")
  density_12$density_12 = as.numeric(density_12$density_12 / sum(density_12$density_12))
  
  
  race_grid = left_join(race_grid, density_16, by = "race")
  race_grid = left_join(race_grid, density_12, by = "race")
  
  
  # multiply distributions together
  race_grid$rep_dens_16 = with(race_grid, density_16*pr_turnout_16*pr_voterep_16)
  race_grid$dem_dens_16 = with(race_grid, density_16*pr_turnout_16*pr_votedem_16)
  race_grid$rep_dens_12 = with(race_grid, density_12*pr_turnout_12*pr_voterep_12)
  race_grid$dem_dens_12 = with(race_grid, density_12*pr_turnout_12*pr_votedem_12)
  
  # calculate 2016 - 2012 change
  race_grid$dem_dens_diff = with(race_grid, dem_dens_16 - dem_dens_12)
  race_grid$rep_dens_diff = with(race_grid, rep_dens_16 - rep_dens_12)
  
  # calculate dem-rep diff in each year
  race_grid$vs_diff_12 = with(race_grid, rep_dens_12 - dem_dens_12)
  race_grid$vs_diff_16 = with(race_grid, rep_dens_16 - dem_dens_16)
  
  # calculate "diff in diff" 
  race_grid$vs_diff_diff = with(race_grid, vs_diff_16 - vs_diff_12)
  
  
  # calculate cleavage stat: sum(| diff in diff |) for each group minus overall margin
  overall_margin = abs(sum(race_grid$vs_diff_diff))
  n_groups = nrow(race_grid)
  cleavage_var = sum((race_grid$vs_diff_diff - (overall_margin/n_groups))^2)
  cleavage_abs = sum(abs(race_grid$vs_diff_diff - (overall_margin/n_groups)))

  # split into 2 dfs then put back together -- one for each year
  race_grid_12 = race_grid %>% 
    select(race, contains("12")) %>% 
    mutate(year = "2012")
  names(race_grid_12) = gsub("_12", "", names(race_grid_12))
  
  race_grid_16 = race_grid %>% 
    select(race, contains("16")) %>% 
    mutate(year = "2016")
  names(race_grid_16) = gsub("_16", "", names(race_grid_16))
  
  race_grid2 = bind_rows(race_grid_12, race_grid_16)
  
  # order race factor
  assert_that(all(raceorder %in% race_grid$race))
  race_grid$race  = factor(race_grid$race, raceorder)
  race_grid2$race = factor(race_grid2$race, raceorder)
  
  
  # make plots
  if(plot){
    # density 
    ggdens = ggplot(race_grid2) + 
      aes(x = race, y = density, fill = year) + 
      geom_bar(stat = "identity", position = position_dodge()) + 
      scale_fill_grey(name = NULL) + 
      labs(x = "Race", y = "Density") + 
      theme(panel.grid = element_blank()) +
      ggtitle(plot.title)
    
    # pr(turnout | race)
    ggturnout = ggplot(race_grid2) + 
      aes(x = race, y = pr_turnout, fill = year) + 
      geom_bar(stat = "identity", position = position_dodge()) + 
      coord_cartesian(ylim = c(0, 1)) + 
      theme(panel.grid = element_blank()) +
      scale_fill_grey(name = NULL) + 
      ggtitle(plot.title) + 
      labs(x = "Race", y = "Turnout") 
    
    # pr(vote | race)
    ggvote = ggplot(race_grid2) + 
      aes(x = as.numeric(race), lty = year) + 
      geom_point(aes(y = pr_voterep, colour = "Republican"), cex = 2.5, pch = 20) + 
      geom_point(aes(y = pr_votedem, colour = "Democrat"), cex = 2.5, pch = 20) + 
      geom_line(aes(y = pr_voterep, colour = "Republican")) + 
      geom_line(aes(y = pr_votedem, colour = "Democrat")) + 
      coord_cartesian(ylim = c(0, 1)) + 
      scale_x_continuous(labels = levels(race_grid2$race)) +
      scale_colour_manual(name = NULL, values = c("Republican" = scales::muted("red"), "Democrat" = scales::muted("blue"))) + 
      scale_linetype(name = NULL) + 
      labs(x = "Race", y = "Pr(Vote | Turnout)") + 
      theme(panel.grid = element_blank())   + 
      ggtitle(plot.title)
    
    # make vote plot
    ggvotedens = ggplot(race_grid2) + 
      aes(x = as.numeric(race), lty = year) + 
      geom_point(aes(y = rep_dens, colour = "Republican"), cex = 2, pch = 20) + 
      geom_point(aes(y = dem_dens, colour = "Democrat"), cex = 2, pch = 20) + 
      geom_line(aes(y = rep_dens, colour = "Republican")) + 
      geom_line(aes(y = dem_dens, colour = "Democrat")) + 
      scale_x_continuous(labels = levels(race_grid2$race)) +
      scale_y_continuous(breaks = seq(0, .3, .05)) +
      scale_colour_manual(name = NULL, values = c("Republican" = scales::muted("red"), "Democrat" = scales::muted("blue"))) + 
      scale_linetype(name = NULL) + 
      labs(x = "Race", y = "Vote density") + 
      theme(panel.grid = element_blank())  +
      ggtitle(plot.title)
    if(coords) ggvotedens = ggvotedens + coord_cartesian(ylim = c(0, .3))
    
    # plot differences over time
    ggdiff = ggplot(race_grid) + 
      aes(x = as.numeric(race)) + 
      geom_hline(yintercept = 0, colour = "grey50") + 
      geom_point(aes(y = rep_dens_diff, colour = "Republican"), cex = 2, pch = 20) + 
      geom_point(aes(y = dem_dens_diff, colour = "Democrat"), cex = 2, pch = 20) + 
      geom_line(aes(y = rep_dens_diff, colour = "Republican")) + 
      geom_line(aes(y = dem_dens_diff, colour = "Democrat")) + 
      scale_x_continuous(labels = levels(race_grid2$race)) +
      scale_y_continuous(breaks = seq(-.075, .075, .025), label = function(x) round(x, 3)) + 
      scale_colour_manual(name = NULL, values = c("Republican" = scales::muted("red"), "Democrat" = scales::muted("blue"))) + 
      scale_linetype(name = NULL) + 
      labs(x = "Race", y = "Shift in vote density from 2012 to 2016") + 
      theme(panel.grid = element_blank())  +
      ggtitle(plot.title)
    if (coords) ggdiff = ggdiff + coord_cartesian(ylim = c(-.08, .08)) 
    
    
    # compare dem and rep in same election
    ggrepdemdiff = ggplot(race_grid) + 
      aes(x = as.numeric(race)) + 
      geom_hline(yintercept = 0, colour = "grey50") + 
      geom_point(aes(y = vs_diff_12), cex = 2, pch = 20) + 
      geom_point(aes(y = vs_diff_16), cex = 2, pch = 20) + 
      geom_line(aes(y = vs_diff_12, lty = "2012")) + 
      geom_line(aes(y = vs_diff_16, lty = "2016")) + 
      scale_x_continuous(labels = levels(race_grid2$race)) +
      scale_y_continuous(breaks = seq(-.075, .075, .025), label = function(x) round(x, 3)) +
      scale_linetype_manual(name = NULL, values = c("2012" = 1, "2016" = 3)) + 
      labs(x = "Race", y = "Rep. bloc size minus Dem. bloc size") + 
      theme(panel.grid = element_blank())  +
      ggtitle(plot.title)
    if(coords) ggrepdemdiff = ggrepdemdiff + coord_cartesian(ylim = c(-.08, .08)) 
    
    
    # compare diff in diff
    ggdiffdiff = ggplot(race_grid) + 
      aes(x = as.numeric(race), y = vs_diff_diff) + 
      geom_point(cex = 3, pch = 20) +
      geom_hline(yintercept = 0, colour = "grey50") + 
      #geom_line() + 
      scale_x_continuous(labels = levels(race_grid2$race)) +
      scale_y_continuous(breaks = seq(-.06, .06, .02)) + 
      labs(x = "Race", y = "(Rep '16 - Dem '16) -\n(Rep '12 - Dem '12)") + 
      theme(panel.grid = element_blank())  +
      ggtitle(plot.title)
    if(coords) ggdiffdiff = ggdiffdiff + coord_cartesian(ylim = c( -.04, .04)) 
    
    out = list(results_wide = race_grid, 
               results_long = race_grid2, 
               n12 = nrow(data12),
               n16 = nrow(data16),
               cleavage_var = sqrt(cleavage_var),
               cleavage_abs = cleavage_abs,
               plots = list(dens = ggdens, 
                            turnout = ggturnout, 
                            vote = ggvote, 
                            votedens = ggvotedens, 
                            diff = ggdiff, 
                            repdemdiff = ggrepdemdiff, 
                            diffdiff = ggdiffdiff))
    return(out)
  } else {
    out = list(results_wide = race_grid, 
               results_long = race_grid2, 
               n12 = nrow(data12),
               n16 = nrow(data16),
               cleavage_var = sqrt(cleavage_var),
               cleavage_abs = cleavage_abs)
    return(out)
  }
}


# racial resentment analysis  ------------------------------

# data12 and data16 must be svydesign objects
resent_discrete_analysis = function(data12, data16, plot.title = NULL, 
                           coords = TRUE, plot = TRUE){
  
  assert_that("survey.design" %in% class(data12), msg = "need to declare svydesign for data12")
  assert_that("survey.design" %in% class(data16), msg = "need to declare svydesign for data16")
  
  
  # print out sample sizes
  message(paste0("In 2012, N = ", prettyNum(nrow(data12), big.mark = ",")))
  message(paste0("In 2016, N = ", prettyNum(nrow(data16), big.mark = ",")))
  
  ## household resentment change
  resentment_grid = data.frame(resentment_discrete = sort(unique(data16$variables$resentment_discrete)))
  
  # turnout on resentment scale
  turnout16 = svytable(~ validated_turnout + resentment_discrete, svy16)
  turnout16 = as.data.frame(t(apply(turnout16, 2, function(x) x / sum(x))))
  turnout16[,"0"] = NULL
  names(turnout16) = "pr_turnout_16"
  turnout16$resentment_discrete = rownames(turnout16)
  turnout16$resentment_discrete = as.numeric(turnout16$resentment_discrete)
  
  turnout12 = svytable(~ validated_turnout + resentment_discrete, svy12)
  turnout12 = as.data.frame(t(apply(turnout12, 2, function(x) x / sum(x))))
  turnout12[,"0"] = NULL
  names(turnout12) = "pr_turnout_12"
  turnout12$resentment_discrete = rownames(turnout12)
  turnout12$resentment_discrete = as.numeric(turnout12$resentment_discrete)
  
  # add turnout to resentment grid
  resentment_grid = left_join(resentment_grid, turnout12, by = "resentment_discrete")
  resentment_grid = left_join(resentment_grid, turnout16, by = "resentment_discrete")
  
  # vote choice conditional on turnout -- 2016
  vote16_rep = svytable(~ rep16_vv + resentment_discrete, subset(data16, validated_turnout == 1))
  vote16_rep = apply(vote16_rep, 2, function(x) x / sum(x))
  vote16_rep = as.data.frame(t(vote16_rep))
  vote16_rep[, "0"] = NULL
  names(vote16_rep) = "pr_voterep_16"
  vote16_rep$resentment_discrete = as.numeric(rownames(vote16_rep))
  
  vote16_dem = svytable(~ dem16_vv + resentment_discrete, subset(data16, validated_turnout == 1))
  vote16_dem = apply(vote16_dem, 2, function(x) x / sum(x))
  vote16_dem = as.data.frame(t(vote16_dem))
  vote16_dem[, "0"] = NULL
  names(vote16_dem) = "pr_votedem_16"
  vote16_dem$resentment_discrete = as.numeric(rownames(vote16_dem))
  
  # add 2016 vote choice to resentment grid
  resentment_grid = left_join(resentment_grid, vote16_dem, by = "resentment_discrete")
  resentment_grid = left_join(resentment_grid, vote16_rep, by = "resentment_discrete")
  
  # vote choice conditional on turnout -- 2012
  vote12_rep = svytable(~ rep12_vv + resentment_discrete, subset(data12, validated_turnout == 1))
  vote12_rep = apply(vote12_rep, 2, function(x) x / sum(x))
  vote12_rep = as.data.frame(t(vote12_rep))
  vote12_rep[, "0"] = NULL
  names(vote12_rep) = "pr_voterep_12"
  vote12_rep$resentment_discrete = as.numeric(rownames(vote12_rep))
  
  vote12_dem = svytable(~ dem12_vv + resentment_discrete, subset(data12, validated_turnout == 1))
  vote12_dem = apply(vote12_dem, 2, function(x) x / sum(x))
  vote12_dem = as.data.frame(t(vote12_dem))
  vote12_dem[, "0"] = NULL
  names(vote12_dem) = "pr_votedem_12"
  vote12_dem$resentment_discrete = as.numeric(rownames(vote12_dem))
  
  # add 2012 vote choice to resentment grid
  resentment_grid = left_join(resentment_grid, vote12_dem, by = "resentment_discrete")
  resentment_grid = left_join(resentment_grid, vote12_rep, by = "resentment_discrete")
  
  
  
  # evaluate density of resentment change in each year
  density_16 = svytable(~resentment_discrete, data16, Ntotal=1)
  density_16 = as.data.frame(density_16)
  names(density_16) = c("resentment_discrete", "density_16")
  density_16$resentment_discrete = as.numeric(density_16$resentment_discrete)
  
  density_12 = svytable(~resentment_discrete, data12, Ntotal=1)
  density_12 = as.data.frame(density_12)
  names(density_12) = c("resentment_discrete", "density_12")
  density_12$resentment_discrete = as.numeric(density_12$resentment_discrete)
  
  resentment_grid = left_join(resentment_grid, density_16, by = "resentment_discrete")
  resentment_grid = left_join(resentment_grid, density_12, by = "resentment_discrete")
  
  
  # multiply distributions together
  resentment_grid$rep_dens_16 = with(resentment_grid, density_16 * pr_turnout_16 * pr_voterep_16)
  resentment_grid$dem_dens_16 = with(resentment_grid, density_16 * pr_turnout_16 * pr_votedem_16)
  
  resentment_grid$rep_dens_12 = with(resentment_grid, density_12 * pr_turnout_12 * pr_voterep_12)
  resentment_grid$dem_dens_12 = with(resentment_grid, density_12 * pr_turnout_12 * pr_votedem_12)
  
  
  # calculate 2016 - 2012 change
  resentment_grid$dem_dens_diff = with(resentment_grid, dem_dens_16 - dem_dens_12)
  resentment_grid$rep_dens_diff = with(resentment_grid, rep_dens_16 - rep_dens_12)
  
  # calculate dem-rep diff in each year
  resentment_grid$vs_diff_12 = with(resentment_grid, rep_dens_12 - dem_dens_12)
  resentment_grid$vs_diff_16 = with(resentment_grid, rep_dens_16 - dem_dens_16)
  
  # calculate "diff in diff" 
  resentment_grid$vs_diff_diff = with(resentment_grid, vs_diff_16 - vs_diff_12)
  
  # calculate cleavage absolute value statistic
  overall_margin = abs(sum(resentment_grid$vs_diff_diff))
  n_groups = nrow(resentment_grid)
  cleavage_var = sum((resentment_grid$vs_diff_diff - (overall_margin/n_groups))^2)
  cleavage_abs = sum(abs(resentment_grid$vs_diff_diff - (overall_margin/n_groups)))
  
  
  
  # rescale resentment to 0-1
  resentment_grid$resentment_discrete = resentment_grid$resentment_discrete - 1
  resentment_grid$resentment_discrete = resentment_grid$resentment_discrete / max(resentment_grid$resentment_discrete)
  
  # split into 2 dfs then put back together -- one for each year
  resentment_grid_12 = resentment_grid %>% 
    select(resentment_discrete, contains("12")) %>% 
    mutate(year = "2012")
  names(resentment_grid_12) = gsub("_12", "", names(resentment_grid_12))
  
  resentment_grid_16 = resentment_grid %>% 
    select(resentment_discrete, contains("16")) %>% 
    mutate(year = "2016")
  names(resentment_grid_16) = gsub("_16", "", names(resentment_grid_16))
  
  resentment_grid2 = bind_rows(resentment_grid_12, resentment_grid_16)
  
  # make plots
  if(plot){
    
    # density 
    ggdens = ggplot(resentment_grid2) + 
      aes(x = resentment_discrete, y = density, fill = year) + 
      geom_bar(stat = "identity", position = position_dodge()) + 
      scale_fill_grey(name = NULL) + 
      scale_x_continuous(breaks = c(.02, .98), labels = c("Low", "High")) + 
      labs(x = "Racial resentment", y = "Density") + 
      theme(panel.grid = element_blank()) + 
      coord_cartesian(ylim = c(0, .5)) + 
      ggtitle(plot.title)
    
    # pr(turnout | resentment_discrete)
    ggturnout = ggplot(resentment_grid2) + 
      aes(x = as.numeric(resentment_discrete), y = pr_turnout, lty = year) + 
      # geom_bar(stat = "identity") + 
      geom_point(cex = 2, pch = 20) + 
      geom_line() + 
      coord_cartesian(ylim = c(0, 1)) + 
      scale_linetype(name = NULL) + 
      scale_x_continuous(breaks = c(.02, .98), labels = c("Low", "High")) + 
      labs(x = "Racial resentment", y = "Turnout") + 
      theme(panel.grid = element_blank())  + 
      ggtitle(plot.title)
    
    # pr(vote | resentment_discrete)
    ggvoteresentment = ggplot(resentment_grid2) + 
      aes(x = as.numeric(resentment_discrete), lty = year) + 
      geom_line(aes(y = pr_voterep, colour = "Republican")) + 
      geom_line(aes(y = pr_votedem, colour = "Democrat")) + 
      geom_point(aes(y = pr_voterep, colour = "Republican"), cex = 2, pch = 20) + 
      geom_point(aes(y = pr_votedem, colour = "Democrat"), cex = 2, pch = 20) + 
      coord_cartesian(ylim = c(0, 1)) + 
      scale_x_continuous(breaks = c(.02, .98), labels = c("Low", "High")) + 
      scale_colour_manual(name = NULL, values = c("Republican" = scales::muted("red"), "Democrat" = scales::muted("blue"))) + 
      scale_linetype(name = NULL) + 
      labs(x = "Racial resentment", y = "Pr(Vote | Turnout)") + 
      theme(panel.grid = element_blank()) +
      ggtitle(plot.title)
    
    
    # make vote plot
    ggvotedens = ggplot(resentment_grid2) + 
      aes(x = as.numeric(resentment_discrete), lty = year) + 
      geom_line(aes(y = rep_dens, colour = "Republican")) + 
      geom_line(aes(y = dem_dens, colour = "Democrat")) + 
      geom_point(aes(y = rep_dens, colour = "Republican"), cex = 2, pch = 20) + 
      geom_point(aes(y = dem_dens, colour = "Democrat"), cex = 2, pch = 20) + 
      scale_x_continuous(breaks = c(.02, .98), labels = c("Low", "High")) + 
      scale_colour_manual(name = NULL, values = c("Republican" = scales::muted("red"), "Democrat" = scales::muted("blue"))) + 
      scale_linetype(name = NULL) + 
      labs(x = "Racial resentment", y = "Vote density") + 
      theme(panel.grid = element_blank()) +
      ggtitle(plot.title)
    if(coords) ggvotedens = ggvotedens + coord_cartesian(ylim = c(0, .15)) 
    
    
    # plot differences over time
    ggdiff = ggplot(resentment_grid) + 
      aes(x = as.numeric(resentment_discrete)) + 
      geom_hline(yintercept = 0, colour = "grey50") + 
      geom_line(aes(y = rep_dens_diff, colour = "Republican")) + 
      geom_line(aes(y = dem_dens_diff, colour = "Democrat")) + 
      geom_point(aes(y = rep_dens_diff, colour = "Republican"), cex = 2, pch = 20) + 
      geom_point(aes(y = dem_dens_diff, colour = "Democrat"), cex = 2, pch = 20) + 
      scale_x_continuous(breaks = c(.02, .98), labels = c("Low", "High")) + 
      scale_colour_manual(name = NULL, values = c("Republican" = scales::muted("red"), "Democrat" = scales::muted("blue"))) + 
      scale_linetype(name = NULL) + 
      labs(x = "Racial resentment", y = "Shift in vote density from 2012 to 2016") + 
      theme(panel.grid = element_blank())  +
      ggtitle(plot.title)
    if (coords) ggdiff = ggdiff +  coord_cartesian(ylim = c(-.04, .04))
    
    
    # compare dem and rep in same election
    ggrepdemdiff = ggplot(resentment_grid) + 
      aes(x = as.numeric(resentment_discrete)) + 
      geom_hline(yintercept = 0, colour = "grey50") + 
      geom_line(aes(y = vs_diff_12, lty = "2012")) + 
      geom_line(aes(y = vs_diff_16, lty = "2016")) + 
      geom_point(aes(y = vs_diff_12), cex = 2, pch = 20) + 
      geom_point(aes(y = vs_diff_16), cex = 2, pch = 20) + 
      scale_x_continuous(breaks = c(.02, .98), labels = c("Low", "High")) + 
      scale_y_continuous(breaks = round(seq(-.1, .1, .025),3)) + 
      scale_linetype_manual(name = NULL, values = c("2012" = 1, "2016" = 3)) + 
      labs(x = "Racial resentment", y = "Rep. bloc size minus Dem. bloc size") + 
      theme(panel.grid = element_blank()) + 
      ggtitle(plot.title)
    if(coords) ggrepdemdiff = ggrepdemdiff + coord_cartesian(ylim = c(-.1, .1)) 
    
    
    # compare diff in diff
    ggdiffdiff = ggplot(resentment_grid) + 
      aes(x = as.numeric(resentment_discrete), y = vs_diff_diff) + 
      geom_point(cex = 2, pch = 20) + 
      geom_hline(yintercept = 0, colour = "grey50") + 
      geom_line() + 
      scale_x_continuous(breaks = c(.02, .98), labels = c("Low", "High")) + 
      labs(x = "Racial resentment", y = "(Rep '16 - Dem '16) -\n(Rep '12 - Dem '12)") + 
      theme(panel.grid = element_blank())  +
      ggtitle(plot.title)
    if(coords) ggdiffdiff = ggdiffdiff + coord_cartesian(ylim = c( -.05, .05)) 
    
    out = list(results_wide = resentment_grid, 
               results_long = resentment_grid2, 
               n12 = nrow(data12),
               n16 = nrow(data16),
               cleavage_var = sqrt(cleavage_var),
               cleavage_abs = cleavage_abs,
               plots = list(dens = ggdens, 
                            turnout = ggturnout, 
                            vote = ggvoteresentment, 
                            votedens = ggvotedens, 
                            diff = ggdiff, 
                            repdemdiff = ggrepdemdiff, 
                            diffdiff = ggdiffdiff))
    return(out)
  } else {
    out = list(results_wide = resentment_grid, 
               results_long = resentment_grid2, 
               n12 = nrow(data12),
               n16 = nrow(data16),
               cleavage_var = sqrt(cleavage_var),
               cleavage_abs = cleavage_abs)
    return(out)
  }
}



# data12 and data16 are NOT svydesign objects here, though they must contain the following
# variables: 
#  - validated_turnout
#  - weight_ftf
#  - resentment (continuous)
#  - voterepXX_vv --> XX = 12 and 16
#  - votedemXX_vv --> XX = 12 and 16
resent_cont_analysis = function(data12, data16, plot.title = NULL, 
                                coords = TRUE, plot = TRUE, 
                                grid.length = 500){
  
  
  # print out sample sizes
  message(paste0("In 2012, N = ", prettyNum(nrow(data12), big.mark = ",")))
  message(paste0("In 2016, N = ", prettyNum(nrow(data16), big.mark = ",")))
  
  # omit data with missing values
  data12 = subset(data12, !is.na(resentment))
  data16 = subset(data16, !is.na(resentment))
  
  # make grid of racial resentment
  minres = min(c(data12$resentment, data16$resentment), na.rm=TRUE)
  maxres = max(c(data12$resentment, data16$resentment), na.rm=TRUE)
  grid.width = (maxres - minres) / grid.length
  resentment_grid = data.frame(resentment = seq(minres, maxres, length.out = grid.length))
  
  # turnout on resentment scale
  turnout16 = loess.opt(y = data16$validated_turnout, 
                        x = data16$resentment,
                        weights = data16$weight_ftf)
  resentment_grid$pr_turnout_16 = predict(turnout16, newdata = rename(resentment_grid, x = resentment))
  
  turnout12 = loess.opt(y = data12$validated_turnout, 
                        x = data12$resentment,
                        weights = data12$weight_ftf)
  resentment_grid$pr_turnout_12 = predict(turnout12, newdata = rename(resentment_grid, x = resentment))
  
  
  # vote choice conditional on turnout -- 2016
  vote16_rep = loess.opt(y = data16$rep16_vv[data16$validated_turnout == 1], 
                         x = data16$resentment[data16$validated_turnout == 1],
                         weights = data16$weight_ftf[data16$validated_turnout == 1])
  resentment_grid$pr_voterep_16 = predict(vote16_rep, newdata = rename(resentment_grid, x = resentment))
  resentment_grid$pr_voterep_16[resentment_grid$pr_voterep_16>1]<- 1
  resentment_grid$pr_voterep_16[resentment_grid$pr_voterep_16<0]<- 0


  vote16_dem = loess.opt(y = data16$dem16_vv[data16$validated_turnout == 1], 
                         x = data16$resentment[data16$validated_turnout == 1],
                         weights = data16$weight_ftf[data16$validated_turnout == 1])
  resentment_grid$pr_votedem_16 = predict(vote16_dem, newdata = rename(resentment_grid, x = resentment))
  resentment_grid$pr_votedem_16[resentment_grid$pr_votedem_16>1]<- 1
  resentment_grid$pr_votedem_16[resentment_grid$pr_votedem_16<0]<- 0
  
  # vote choice conditional on turnout -- 2012
  # vote choice conditional on turnout -- 2012
  vote12_rep = loess.opt(y = data12$rep12_vv[data12$validated_turnout == 1], 
                         x = data12$resentment[data12$validated_turnout == 1],
                         weights = data12$weight_ftf[data12$validated_turnout == 1])
  resentment_grid$pr_voterep_12 = predict(vote12_rep, newdata = rename(resentment_grid, x = resentment))
  resentment_grid$pr_voterep_12[resentment_grid$pr_voterep_12>1]<- 1
  resentment_grid$pr_voterep_12[resentment_grid$pr_voterep_12<0]<- 0


  vote12_dem = loess.opt(y = data12$dem12_vv[data12$validated_turnout == 1], 
                         x = data12$resentment[data12$validated_turnout == 1],
                         weights = data12$weight_ftf[data12$validated_turnout == 1])
  resentment_grid$pr_votedem_12 = predict(vote12_dem, newdata = rename(resentment_grid, x = resentment))
  resentment_grid$pr_votedem_12[resentment_grid$pr_votedem_12>1]<- 1
  resentment_grid$pr_votedem_12[resentment_grid$pr_votedem_12<0]<- 0


  
  # evaluate density of resentment change in each year
  ## FIGURE OUT DENSITY ESTIMATOR - for now just use bw.bcv() with unweighted data
  bw16 = bw.bcv(data16$resentment, lower = .005, upper = 1.5)
  resentment_grid$density_16 = dens_at_grid(point = resentment_grid$resentment,
                                            data = data16$resentment,
                                            weights = data16$weight_ftf / sum(data16$weight_ftf),
                                            bw = bw16)
  
  bw12 = bw.bcv(data12$resentment, lower = .005, upper = 1.5)
  resentment_grid$density_12 = dens_at_grid(point = resentment_grid$resentment,
                                            data = data12$resentment,
                                            weights = data12$weight_ftf / sum(data12$weight_ftf),
                                            bw = bw12)
  
  
  # multiply distributions together
  resentment_grid$rep_dens_16 = with(resentment_grid, density_16 * pr_turnout_16 * pr_voterep_16)
  resentment_grid$dem_dens_16 = with(resentment_grid, density_16 * pr_turnout_16 * pr_votedem_16)
  
  resentment_grid$rep_dens_12 = with(resentment_grid, density_12 * pr_turnout_12 * pr_voterep_12)
  resentment_grid$dem_dens_12 = with(resentment_grid, density_12 * pr_turnout_12 * pr_votedem_12)
  
  
  # calculate 2016 - 2012 change
  resentment_grid$dem_dens_diff = with(resentment_grid, dem_dens_16 - dem_dens_12)
  resentment_grid$rep_dens_diff = with(resentment_grid, rep_dens_16 - rep_dens_12)
  
  # calculate dem-rep diff in each year
  resentment_grid$vs_diff_12 = with(resentment_grid, rep_dens_12 - dem_dens_12)
  resentment_grid$vs_diff_16 = with(resentment_grid, rep_dens_16 - dem_dens_16)
  
  # calculate "diff in diff" 
  resentment_grid$vs_diff_diff = with(resentment_grid, vs_diff_16 - vs_diff_12)
  
  
  # calculate overall change in votes -- \int DiffDiff | Resentment d Resentment
  overall_margin = sum(((resentment_grid$vs_diff_diff[-1] + resentment_grid$vs_diff_diff[-grid.length])/2) * grid.width)
  
  
  # calculate cleavage absolute value statistic. In the continuous case
  # need to subtract off the level implied by a uniform distribution on 
  # the support of the continuous variable of interest, multiplied by
  # the overall margin. Then take integral. 
  height = overall_margin / (maxres - minres) 
  cleavage_var_integrand = (resentment_grid$vs_diff_diff - height)^2
  cleavage_abs_integrand = abs(resentment_grid$vs_diff_diff - height)
  cleavage_var = sum(grid.width*((cleavage_var_integrand[-1] + cleavage_var_integrand[-grid.length])/2))
  cleavage_abs = sum(grid.width*((cleavage_abs_integrand[-1] + cleavage_abs_integrand[-grid.length])/2))
  
  
  # split into 2 dfs then put back together -- one for each year
  resentment_grid_12 = resentment_grid %>% 
    select(resentment, contains("12")) %>% 
    mutate(year = "2012")
  names(resentment_grid_12) = gsub("_12", "", names(resentment_grid_12))
  
  resentment_grid_16 = resentment_grid %>% 
    select(resentment, contains("16")) %>% 
    mutate(year = "2016")
  names(resentment_grid_16) = gsub("_16", "", names(resentment_grid_16))
  
  resentment_grid2 = bind_rows(resentment_grid_12, resentment_grid_16)
  
  # make plots
  if(plot){
    
    # density 
    ggdens = ggplot(resentment_grid2) + 
      aes(x = resentment, y = density, lty = year) + 
      geom_line() + 
      scale_linetype(name = NULL) + 
      scale_x_continuous(breaks = c(ceil(minres), floor(maxres) + .5), labels = c("Low", "High")) + 
      labs(x = "Racial resentment", y = "Density") + 
      theme(panel.grid = element_blank()) + 
      coord_cartesian(ylim = c(0, .5)) + 
      ggtitle(plot.title)
    
    # pr(turnout | resentment)
    ggturnout = ggplot(resentment_grid2) + 
      aes(x = as.numeric(resentment), y = pr_turnout, lty = year) + 
      geom_line() + 
      coord_cartesian(ylim = c(0, 1)) + 
      scale_linetype(name = NULL) + 
      scale_x_continuous(breaks = c(ceil(minres), floor(maxres) + .5), labels = c("Low", "High")) + 
      labs(x = "Racial resentment", y = "Turnout") + 
      theme(panel.grid = element_blank())  + 
      ggtitle(plot.title)
    
    # pr(vote | resentment, turnout)
    ggvoteresentment = ggplot(resentment_grid2) + 
      aes(x = as.numeric(resentment), lty = year) + 
      geom_line(aes(y = pr_voterep, colour = "Republican")) + 
      geom_line(aes(y = pr_votedem, colour = "Democrat")) + 
      coord_cartesian(ylim = c(0, 1)) + 
      scale_linetype(name = NULL) + 
      scale_x_continuous(breaks = c(ceil(minres), floor(maxres) + .5), labels = c("Low", "High")) + 
      scale_colour_manual(name = NULL, values = c("Republican" = scales::muted("red"), "Democrat" = scales::muted("blue"))) + 
      labs(x = "Racial resentment", y = "Pr(Vote | Turnout)") + 
      theme(panel.grid = element_blank()) +
      ggtitle(plot.title)
    
    
    # make vote plot
    ggvotedens = ggplot(resentment_grid2) + 
      aes(x = resentment, lty = year) + 
      geom_line(aes(y = rep_dens, colour = "Republican")) + 
      geom_line(aes(y = dem_dens, colour = "Democrat")) + 
      scale_x_continuous(breaks = c(ceil(minres), floor(maxres) + .5), labels = c("Low", "High")) + 
      scale_colour_manual(name = NULL, values = c("Republican" = scales::muted("red"), "Democrat" = scales::muted("blue"))) + 
      scale_linetype(name = NULL) + 
      labs(x = "Racial resentment", y = "Vote density") + 
      theme(panel.grid = element_blank()) +
      ggtitle(plot.title)
    if(coords) ggvotedens = ggvotedens + coord_cartesian(ylim = c(0, .15)) 
    
    
    # plot differences over time
    ggdiff = ggplot(resentment_grid) + 
      aes(x = resentment) + 
      geom_hline(yintercept = 0, colour = "grey50") + 
      geom_line(aes(y = rep_dens_diff, colour = "Republican")) + 
      geom_line(aes(y = dem_dens_diff, colour = "Democrat")) + 
      scale_x_continuous(breaks = c(ceil(minres), floor(maxres) + .5), labels = c("Low", "High")) + 
      scale_colour_manual(name = NULL, values = c("Republican" = scales::muted("red"), "Democrat" = scales::muted("blue"))) + 
      scale_linetype(name = NULL) + 
      labs(x = "Racial resentment", y = "Shift in vote density from 2012 to 2016") + 
      theme(panel.grid = element_blank())  +
      ggtitle(plot.title)
    if (coords) ggdiff = ggdiff +  coord_cartesian(ylim = c(-.04, .04))
    
    
    # compare dem and rep in same election
    ggrepdemdiff = ggplot(resentment_grid) + 
      aes(x = resentment) + 
      geom_hline(yintercept = 0, colour = "grey50") + 
      geom_line(aes(y = vs_diff_12, lty = "2012")) + 
      geom_line(aes(y = vs_diff_16, lty = "2016")) + 
      scale_x_continuous(breaks = c(ceil(minres), floor(maxres) + .5), labels = c("Low", "High")) + 
      scale_y_continuous(breaks = round(seq(-.1, .1, .025),3)) + 
      scale_linetype_manual(name = NULL, values = c("2012" = 1, "2016" = 3)) + 
      labs(x = "Racial resentment", y = "Rep. bloc size minus Dem. bloc size") + 
      theme(panel.grid = element_blank()) + 
      ggtitle(plot.title)
    if(coords) ggrepdemdiff = ggrepdemdiff + coord_cartesian(ylim = c(-.1, .1)) 
    
    
    # compare diff in diff
    ggdiffdiff = ggplot(resentment_grid) + 
      aes(x = resentment, y = vs_diff_diff) + 
      geom_hline(yintercept = 0, colour = "grey50") + 
      geom_line() + 
      scale_x_continuous(breaks = c(ceil(minres), floor(maxres) + .5), labels = c("Low", "High")) + 
      labs(x = "Racial resentment", y = "(Rep '16 - Dem '16) -\n(Rep '12 - Dem '12)") + 
      theme(panel.grid = element_blank())  +
      ggtitle(plot.title)
    if(coords) ggdiffdiff = ggdiffdiff + coord_cartesian(ylim = c( -.12, .12)) 
    
    out = list(results_wide = resentment_grid, 
               results_long = resentment_grid2, 
               n12 = nrow(data12),
               n16 = nrow(data16),
               cleavage_var = sqrt(cleavage_var),
               cleavage_abs = cleavage_abs,
               grid_width = grid.width,
               plots = list(dens = ggdens, 
                            turnout = ggturnout, 
                            vote = ggvoteresentment, 
                            votedens = ggvotedens, 
                            diff = ggdiff, 
                            repdemdiff = ggrepdemdiff, 
                            diffdiff = ggdiffdiff))
    return(out)
  } else {
    out = list(results_wide = resentment_grid, 
               results_long = resentment_grid2, 
               n12 = nrow(data12),
               n16 = nrow(data16),
               cleavage_var = sqrt(cleavage_var),
               cleavage_abs = cleavage_abs,
               grid_width = grid.width)
    return(out)
  }
}

group_assert <-
  function(data, by, grp_expr, assertion) {
    
    require(data.table)
    require(assertthat)
    
    assert_that(is.data.table(data))
    assert_that(is.character(by))
    assert_that(is.expression(grp_expr)  | is.call(grp_expr))
    assert_that(is.expression(assertion) | is.call(assertion))
    
    grp_eval <- data[ , eval(grp_expr, envir = .SD), by = by]
    
    if(!is.character(grp_eval[[by]])){
      
      grp_eval[ , (by) := as.character(get(by))]
      setDT(grp_eval, key = by)
    }
    
    for(grp in grp_eval[[by]])
      assert_that(grp_eval[grp, eval(assertion)],
                  msg = sprintf("%s is not TRUE for %s", deparse(assertion), grp))
    
    return(TRUE)
  }


wtd_quantile <- function(x, probs = seq(0, 1, 0.25), weight, na.rm = FALSE, ...){
  
  #' Weighted quantiles ' ' This function calls \link[collapse]{fnth} repeatedly
  #' over a vector of probabilities to produce output like \link[stats]{quantile}.
  #' Fast, with minimal dependencies, but does not accept negative weights.
  #' @param x      numeric vector.
  #' @param probs  numeric vector of probabilities.
  #' @param weight numeric vector of non-negative weights.
  #' @param na.rm  logical whether to remove missing values
  #' @param ...    further arguments passed to \link[collapse]{fnth}.
  
  if(any(weight < 0)) stop("collapse::fnth does not support negative weights.")
  probs_tags <- paste0(probs * 100, "%")
  
  # collapse::fnth doesn't allow 0 or 1 probabilities
  if(0 %in% probs) probs[probs == 0] <- .Machine$double.xmin
  if(1 %in% probs) probs[probs == 1] <- 1 - .Machine$double.xmin
  probs0_ind <- which(probs == 0)
  probs1_ind <- which(probs == 1)
  
  probs[c(probs0_ind, probs1_ind)] <- 0.5
  
  out <-
    vapply(X = probs, FUN.VALUE = double(1),
           FUN = function(p) collapse::fnth(x, w = weight, n = p,
                                            na.rm = na.rm, ...))
  out[probs0_ind] <- collapse::fmin(x)
  out[probs1_ind] <- collapse::fmax(x)
  
  names(out) <- probs_tags
  
  
  return(out)
  
}
