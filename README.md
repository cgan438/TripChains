# TripChains
Functions used in TripChains

This file outlines the functions used for my project analyzing the results
of the 14-18 NZHTS. These results are not included.

To use, the following should be done:

1) read in the data
example as below
tr.df <- read.csv("tr_y1418_2021-08-18.csv")
hh.df <- read.csv("hh_y1418_2021-08-18.csv")
pe.df <- read.csv("pe_y1418_2021-08-18.csv")

2) clean the data

tr.df <- clean.trips(tr.df)
pe.df <- clean.people(pe.df, hh.df)
hh.df <- clean.households(hh.df)

3) generate the trip chains
should be noted this takes ~5min
chains.data <- generate.trip.chains(pe.df, tr.df)

the variables are:
key- identifier of person
mesh.id- identifier of meshblock of person (disguised, nothing identifiable)
region - region of person
trip.weights - weight of trip (see nzhts methodology reports for more detail)
gender - gender of person
area - type of area, rural major urban or small urban area
age - age group, either 0-4, 5-17, 18-35, 36-59 or 60+
employ -  employed (Y) or unemployed (N)
child.status - in household with children (Y) or not (N)
num.segments - number of trip segments in trip chain
duration - total duration of trip chain (in hr)
trip.length - total distance travelled in trip chain (in km)
pt - was public transport used in any segment? 0 = no, 1 = yes
active - was an active mode (walking/cycling) used in any segment? 0 = no, 1 = yes
car.driver - was driving a car in any segment? 0 = no, 1 = yes
car.passenger - was being a passenger in any segment? 0 = no, 1 = yes
every.segment.car - was every segment in the trip chain in a vehicle (either driver or passenger)
main.purpose - main purpose of trip chain (1 is mode change, 2 is home, 3 is work, 4 is maintain, 5 is leisure, 6 is accompany)
main.mode- main travel mode of trip chain (1 is car.driving, 2 is car.passenger, 3 is active.mode, 4 is pt.mode, 5 is other.modes)
home.purpose - did any segment involve travelling home? 0 = no, 1 = yes
work.purpose - did any segment involve travelling to work/school? 0 = no, 1 = yes
leisure.purpose- did any segment involve travelling for leisure? 0 = no, 1 = yes
app.purpose - did any segment involve travelling for maintenance (appointments)? 0 = no, 1 = yes
pickup.purpose - did any segment involve travelling for accompanying?0 = no, 1 = yes


4) generate the individual statistics
should be noted takes ~10 min
people.data <- generate.individual.stats(pe.df, tr.df)

the variables are:
work.and.maintenance/leisure/pickup - % of trip chains for person with main purpose of work and a secondary maintenance/leisure/accompanying segment
home.and.maintenance/leisure/pickup - % of trip chains for person with main purpose of home and a secondary maintenance/leisure/accompanying segment
car.and.active/car.and.pt/pass.and.active/pass.and.pt/active.and.pt - % of trip chains with
both trip modes where car = driver, pt = public transport, pass = passenger in car and active = active mode
home.segments/work.segments/main.segments/leisure.segments/pickup.segments - average number of trip segments in chains for person for each main purpose
driving.segments/passenger.segments/active.segments/pt.segments - average number of trip segments in chains for person for each main mode
every.segment.car - % of time for person that trip chain only involves driving
pt.use/active.use - % of time for person that trip chain involves either public transport ot active mode
home.purpose/work.purpose/maintain.purpose/leisure.purpose/accompany.purpose - % of time for person that main purpose of trip chain is each purpose
drive.mode/passenger.mode/active.mode/pt.mode - % of time for person that main mode of trip chain is each travel mode

the other variables are as defined in the data dictionary.

5) complete analysis
this was completed with help of the survey package, and is not included.
example usage would be:
for the people:
people.design <- svydesign(ids = ~mesh.id, strata = ~Region, weights = ~peweight_fullhh, data = people.data, nest = TRUE)
for the trip chains:
chains.design <- svydesign(ids = ~mesh.id, strata = ~region, weights = ~trip.weights, data = chains.data, nest = TRUE)

it should be noted that the other functions included are used in the functions
provided aboved. therefore, an example usage for them has been ommitted,
however the function itself has documentation about its purpose.
