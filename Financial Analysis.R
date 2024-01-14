FINANCIAL ANALYSIS


#CapEx capacity installation:1.16M USD/MW installed power; CapEx Network connection inland: 590 USD/MW.km

calc_NPV <- function(annual_revenue, i=0.05, lifetime_yrs, CAPEX, OPEX=0){
  costs_op <- rep(OPEX, lifetime_yrs) #operating cost
  revenue <- rep(annual_revenue, lifetime_yrs) 
  t <- seq(1, lifetime_yrs, 1) #output: 1, 2, 3, ...25
  
  NPV <- sum( (revenue - costs_op)/(1 + i)**t ) - CAPEX
  return(round(NPV, 0))
}
##from cost calculation

#ACEH
npv= calc_NPV(annual_revenue = 9022800000,lifetime_yrs=25, CAPEX=1162950000  )
npv
ifelse(npv>0, "Support","object" )

#Riau-sumatra
npv= calc_NPV(annual_revenue = 9022800000,lifetime_yrs=25, CAPEX=1162950000  )
npv
ifelse(npv>0, "Support","object" )


#Sumatera Barat
npv= calc_NPV(annual_revenue = 2706840000,lifetime_yrs=25, CAPEX=3488850000  )
npv
ifelse(npv>0, "Support","obeject" )


#Jambi
npv= calc_NPV(annual_revenue = 9022800000,lifetime_yrs=25, CAPEX=1162950000 )
npv
ifelse(npv>0, "Support","obeject" )

#Lampung
npv= calc_NPV(annual_revenue = 9022800000,lifetime_yrs=25, CAPEX=1162950000  )
npv
ifelse(npv>0, "Support","obeject" )


#Kalimantan Utara
npv= calc_NPV(annual_revenue = 1353420000,lifetime_yrs=25, CAPEX=1744420000  )
npv
ifelse(npv>0, "Support","obeject" )

#North Kalimantan
npv= calc_NPV(annual_revenue = 3789576000,lifetime_yrs=25, CAPEX=4884390000  )
npv
ifelse(npv>0, "Support","obeject" )

#Kalimantan Selatan
npv= calc_NPV(annual_revenue = 9022800000,lifetime_yrs=25, CAPEX=1162950000  )
npv
ifelse(npv>0, "Support","obeject" )

#Papua
npv= calc_NPV(annual_revenue = 1353420000,lifetime_yrs=25, CAPEX=1744425000  )
npv
ifelse(npv>0, "Support","obeject" )

#West Java
npv= calc_NPV(annual_revenue = 3699348000,lifetime_yrs=25, CAPEX=4768095000 )
npv
ifelse(npv>0, "Support","obeject" )

#Sulawesi Selatan
npv= calc_NPV(annual_revenue = 5052768000,lifetime_yrs=25, CAPEX=6512520000 )
npv
ifelse(npv>0, "Support","obeject" )

#Nusa T Barat
npv= calc_NPV(annual_revenue = 5233224000,lifetime_yrs=25, CAPEX=6745110000  )
npv
ifelse(npv>0, "Support","obeject" )

#West Sulawesi
npv= calc_NPV(annual_revenue = 2797068000,lifetime_yrs=25, CAPEX=3605145000  )
npv
ifelse(npv>0, "Support","obeject" )


#Nusa T Taimur
npv= calc_NPV(annual_revenue = 3879804000,lifetime_yrs=25, CAPEX=5000685000  )
npv
ifelse(npv>0, "Support","obeject" )


#LCOE

#ACEH

Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

Life_span_generation_kWH
LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 8760000000 #kwh
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 

#2

Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

Life_span_generation_kWH
LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 26280000000 #kwh
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 

#3
Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

Life_span_generation_kWH
LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 61320000000#kwh
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 

#4
Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

Life_span_generation_kWH
LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 13140000000 #kwh
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 

#5.
Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

Life_span_generation_kWH
LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 36792000000 #kwh
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 

#6.
Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

Life_span_generation_kWH
LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 71832000000 #kwh
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 

#7.
Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

Life_span_generation_kWH
LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 35916000000 #kwh
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 

#8
Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

Life_span_generation_kWH
LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 49056000000 #kwh
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 

#9
Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

Life_span_generation_kWH
LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 50808000000 #kwh
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 

#10
Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

Life_span_generation_kWH
LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 50808000000 #kwh
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 

11.
FINANCIAL ANALYSIS


#CapEx capacity installation:1.16M USD/MW installed power; CapEx Network connection inland: 590 USD/MW.km

calc_NPV <- function(annual_revenue, i=0.05, lifetime_yrs, CAPEX, OPEX=0){
  costs_op <- rep(OPEX, lifetime_yrs) #operating cost
  revenue <- rep(annual_revenue, lifetime_yrs) 
  t <- seq(1, lifetime_yrs, 1) #output: 1, 2, 3, ...25
  
  NPV <- sum( (revenue - costs_op)/(1 + i)**t ) - CAPEX
  return(round(NPV, 0))
}
##from cost calculation

#ACEH
npv= calc_NPV(annual_revenue = 9022800000,lifetime_yrs=25, CAPEX=1162950000  )
npv
ifelse(npv>0, "Support","object" )

#Riau-sumatra
npv= calc_NPV(annual_revenue = 9022800000,lifetime_yrs=25, CAPEX=1162950000  )
npv
ifelse(npv>0, "Support","object" )


#Sumatera Barat
npv= calc_NPV(annual_revenue = 2706840000,lifetime_yrs=25, CAPEX=3488850000  )
npv
ifelse(npv>0, "Support","obeject" )


#Jambi
npv= calc_NPV(annual_revenue = 9022800000,lifetime_yrs=25, CAPEX=1162950000 )
npv
ifelse(npv>0, "Support","obeject" )

#Lampung
npv= calc_NPV(annual_revenue = 9022800000,lifetime_yrs=25, CAPEX=1162950000  )
npv
ifelse(npv>0, "Support","obeject" )


#Kalimantan Utara
npv= calc_NPV(annual_revenue = 1353420000,lifetime_yrs=25, CAPEX=1744420000  )
npv
ifelse(npv>0, "Support","obeject" )

#North Kalimantan
npv= calc_NPV(annual_revenue = 3789576000,lifetime_yrs=25, CAPEX=4884390000  )
npv
ifelse(npv>0, "Support","obeject" )

#Kalimantan Selatan
npv= calc_NPV(annual_revenue = 9022800000,lifetime_yrs=25, CAPEX=1162950000  )
npv
ifelse(npv>0, "Support","obeject" )

#Papua
npv= calc_NPV(annual_revenue = 1353420000,lifetime_yrs=25, CAPEX=1744425000  )
npv
ifelse(npv>0, "Support","obeject" )

#West Java
npv= calc_NPV(annual_revenue = 3699348000,lifetime_yrs=25, CAPEX=4768095000 )
npv
ifelse(npv>0, "Support","obeject" )

#Sulawesi Selatan
npv= calc_NPV(annual_revenue = 5052768000,lifetime_yrs=25, CAPEX=6512520000 )
npv
ifelse(npv>0, "Support","obeject" )

#Nusa T Barat
npv= calc_NPV(annual_revenue = 5233224000,lifetime_yrs=25, CAPEX=6745110000  )
npv
ifelse(npv>0, "Support","obeject" )

#West Sulawesi
npv= calc_NPV(annual_revenue = 2797068000,lifetime_yrs=25, CAPEX=3605145000  )
npv
ifelse(npv>0, "Support","obeject" )


#Nusa T Taimur
npv= calc_NPV(annual_revenue = 3879804000,lifetime_yrs=25, CAPEX=5000685000  )
npv
ifelse(npv>0, "Support","obeject" )


#LCOE

#ACEH

Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

Life_span_generation_kWH
LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 8760000000 #kwh
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 

#2

Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

Life_span_generation_kWH
LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 26280000000 #kwh
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 

#3
Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

Life_span_generation_kWH
LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 61320000000#kwh
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 

#4
Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

Life_span_generation_kWH
LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 13140000000 #kwh
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 

#5.
Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

Life_span_generation_kWH
LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 36792000000 #kwh
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 

#6.
Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

Life_span_generation_kWH
LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 71832000000 #kwh
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 

#7.
Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

Life_span_generation_kWH
LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 35916000000 #kwh
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 

#8
Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

Life_span_generation_kWH
LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 49056000000 #kwh
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 

#9
Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

Life_span_generation_kWH
LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 50808000000 #kwh
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 

#10
Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

Life_span_generation_kWH
LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 50808000000 #kwh
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 


#11
Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

Life_span_generation_kWH
LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 27156000000 #kwh
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 


#12
Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

Life_span_generation_kWH
LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 37668000000 #kwh
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 


