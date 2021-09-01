
use Compustat1970_2020
keep if loc=="USA"
keep gvkey fyear conm xrd xrdp sic sale at
gen year=fyear
sort gvkey year
save CompustatRD.dta, replace
clear

use compustat_portfolio_shares.dta
sort gvkey year
merge gvkey year using CompustatRD.dta
drop _merge
sort year

destring sic, replace


gen pharma=0
replace pharma=1 if sic==2834
replace pharma=1 if sic==2835
replace pharma=1 if sic==2836

gen xrd_pharma=pharma*xrd





gen RD_0=xrd*share_0
gen RD_25=xrd*share_25
gen RD_45=xrd*share_45
gen RD_65=xrd*share_65
collapse (sum) xrd_pharma xrd RD_0 RD_25 RD_45 RD_65, by(year)
gen xrd_pharma_total_share=xrd_pharma/xrd
gen RD_65_share=RD_65/xrd_pharma
gen RD_45_share=RD_45/xrd_pharma
gen RD_25_share=RD_25/xrd_pharma
gen RD_0_share=RD_0/xrd_pharma


**************** Figure 3 **************

scatter xrd_pharma_total_share year if year>=1970 & year<=2019, ytitle("") xscale(range(1970 2019)) xlabel(1970(5)2019)




*************** Figure 5 ************************************


scatter RD_45_share year if year>=1997 & year<=2013, ytitle("") xscale(range(1997 2013)) xlabel(1997(2)2013)
scatter RD_65_share year if year>=1997 & year<=2013, ytitle("") xscale(range(1997 2013)) xlabel(1997(2)2013)
