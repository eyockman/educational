# BIOE107 | Emma Yockman
# discussion 3 | Thermoregulation
rm(list=ls())

# everything below copied verbatim from Marm's code:

#Location, date and time of day
Jday = 1#day of year; 1 = jan 1; 365 = dec 31
hr=12 #hour in military time; 12 = noon; 0=24=midnight
Latitude=36.97 #in degrees, Santa Cruz, CA

#animal and env conditions
T_liz=15 #body temp in deg C
Wind=0 #Wind speed in m/s; 1 m/hr = 0.447 m/s
Tair=10 #Air temp in deg C
sunny_rock=T #is lizard sitting on a sunny rock? T = yes, F = no
burrow=F #Is lizard in burrow? T = yes, F = no
if(burrow&sunny_rock) {print("Lizard can't be on a sunny rock and in its burrow")
  stop(burrow&sunny_rock)}
liz_Diam = 0.0254 #diameter of lizard in m
liz_Length=0.15 #Length of lizard
A_liz_cond=0.25 * pi*liz_Diam*liz_Length #approximate area touching ground
A_liz_conv=0.75 * pi*liz_Diam*liz_Length # approximate area exposed to wind
A_liz_rad_sky = 0.5 *pi*liz_Diam*liz_Length #approximate area exposed to sky
A_liz_rad_surr = 0.5 *pi*liz_Diam*liz_Length #approximate area exposed to radiation from surroundings 
A_liz_solrad=liz_Diam*liz_Length #approximate area exposed to solar radiation viewed from above - rectangle
em_abs_liz=0.65 #approximate emissivity and absorbance of lizard

#Forced convection
#3 Functions that provide values of dynamic viscosity, and two constants for forced convection
Visc=function(Tair) (13.35+0.09768*Tair)/10^6 #dynamic viscosity as function of air Temp
Cp=function(log10Re) 1.19754-0.19894*log10Re #constant that uses log10 Reynolds number
np=function(log10Re) 0.23976+0.08777*log10Re #constant that uses log10 Reynolds number
k_tc=0.025 #Heat capacity of air in W/(m^2 deg C)
Re=(Wind+.01)*liz_Diam/Visc(Tair) #Reynolds number
Pr=0.71 #Prandtl number
h_conv=(Cp(log10(Re))*(Re^np(log10(Re)))*Pr^0.33)*(k_tc/liz_Diam) #heat transfer coefficient
q_conv=h_conv*A_liz_conv*(Tair-T_liz) #convection heat transfer in W
q_conv=ifelse(burrow,0,q_conv)

#Solar radiation
S0=1000 #solar radiation at equator at noon W/m2
radfrac=cos(Latitude*pi/180)*cos(pi/180*(90*(hr-12)/12)) # fraction of full radiation latitude in deg
NS=sign(Latitude);aLatitude=abs(Latitude) #used for calculating daylength
P1 = asin(.39795*cos(.2163108 + 2*atan(.9671396*tan(.00860*(Jday-186)) ) )) #used for calculating daylength
DL=ifelse(aLatitude<69, #daylength calculation; approximation poor above Latitude=+/-69
          ((NS+1)/2)*24 - NS*(24/pi)*acos(sin(0.8333*pi/180) + sin(aLatitude*pi/180)*sin(P1) / 
                                            cos(aLatitude*pi/180)*cos(P1)),24)
solrad=S0*radfrac*ifelse((hr-12)<DL/2,1,0) #If its daylight hr, then solar radiation >0, otherwise 0
q_solrad=A_liz_solrad*em_abs_liz*solrad #solar radiation absorbed by lizard
q_solrad=ifelse(burrow,0,q_solrad)

#Conduction from ground
k_cond=0.075 #W/(m2*deg C)
dx= liz_Diam*.05 #conduction thickness
T_rock=ifelse (sunny_rock,Tair+solrad/50,Tair) #rock temp
q_cond=k_cond*A_liz_cond*(T_rock-T_liz)/dx #conduction heat transfer

#Radiation exchange with sky and surroundings
stef_bolt=5.67e-8 #Stefan-boltzmann constant W/(m2 deg C^4)
q_rad_surr=stef_bolt*A_liz_rad_surr*em_abs_liz*((Tair+273.15)^4-(T_liz+273.15)^4) #radiation from surroundings
q_rad_sky=stef_bolt*A_liz_rad_sky*em_abs_liz*((Tair+273.15-25)^4-(T_liz+273.15)^4) #radiation from sky
q_rad_sky=ifelse(burrow,q_rad_surr,q_rad_sky) #if in burrow use q_rad_surr for all radiative exchange

#Lizard metabolic rate
# MR=i0 M^(3/4)*exp(-E/(k*Temp)) equation from Brown et al 2004 Ecology/Gillooly et al 2001 Science
i0=exp(20) #intercept
E=0.63 #constant eV
k=8.617e-5 #constant eV/deg K
M=20 #lizard mass in grams
T_liz_K=273.16+T_liz #Lizard temp in kelvin
MR=i0*M^(3/4)*exp(-E/(k*T_liz_K))

tot_heat_to_liz=q_conv+q_solrad+q_cond+q_rad_surr+q_rad_sky+MR;tot_heat_to_liz
