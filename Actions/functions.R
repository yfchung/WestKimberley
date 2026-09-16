# This script is to keep all the functions used in the analysis. 

#' Function to annualise periodic costs over the time horizon
#' @param Cost numeric, periodic cost
#' @param Interval numeric, interval of the cost in years
#' @param Disc_Rate numeric, discount rate
#' @param TimeHorizon_yr numeric, time horizon in years
#' @return numeric, annualised cost

Annual_period_cost_fn <- function(Cost, Interval, Disc_Rate, TimeHorizon_yr){

  Yrs <- seq(0, TimeHorizon_yr - 1, by = Interval)

  PV_Cost <- sum(Cost / (1 + Disc_Rate)^Yrs)

  Annuity_Fac <- sum(1 / (1 + Disc_Rate)^(0:(TimeHorizon_yr - 1)))

  return(PV_Cost / Annuity_Fac)
}



#' Function to adjust field costs with overhead and contingency
#' @param LabourCost numeric, labour cost
#' @param TransportCost numeric, transport cost
#' @param ConsumablesCost numeric, consumables cost
#' @param Rate_LbrOverhead numeric, labour overhead rate
#' @param Rate_FieldContin numeric, field contingency rate
#' @return numeric, adjusted field cost

Adjust_Field_Cost_fn <- function(LabourCost, TransportCost, ConsumablesCost, 
                              Rate_LbrOverhead, Rate_FieldContin){

  return(((LabourCost * (1 + Rate_LbrOverhead)) + 
         TransportCost + ConsumablesCost) *
         (1 + Rate_FieldContin))
}

#' Function to adjust office costs with overhead
#' @param LabourCost numeric, labour cost
#' @param Rate_LbrOverhead numeric, labour overhead rate
#' @return numeric, adjusted office cost

Adjust_Office_Cost_fn <- function(LabourCost, Rate_LbrOverhead){
  return(LabourCost * (1 + Rate_LbrOverhead))
}


