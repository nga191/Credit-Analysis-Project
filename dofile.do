
use "D:\Data Khóa Luận\VHLSS 2018\VHLSS2018_Household\Data_goc.dta", clear
	
		gen bq_income = total_income/ tsnguoi
		gen bq_income_month= bq_income/12
			gen income_class = " " 
			gen income_class_1 = real(income_class)
				* Phân loại cho khu vực nông thôn 
				replace income_class_1 = 1 if ttnt == 2 & bq_income_month <= 1500
				replace income_class_1 = 2 if ttnt == 2 &  bq_income_month > 1500 &  bq_income_month<= 2250 
				replace income_class_1  = 3 if ttnt == 2 &  bq_income_month > 2250
				* Phân loại cho khu vực thành thị 
				replace income_class_1 = 1 if ttnt == 1 &  bq_income_month <= 2000
				replace income_class_1  = 2 if ttnt == 1 &  bq_income_month > 2000 &  bq_income_month <= 3000
				replace income_class_1  = 3 if ttnt == 1 &  bq_income_month > 3000

		gen dependency_ratio = dependency/ tsnguoi
		rename m1ac2 gender
		rename m1ac5 age
		
		egen avg_age = mean(age)
		gen age_tru_mean = age - avg_age
		gen age2 = (age_tru_mean)^2
		
		egen avg_income = mean(total_income)
		replace total_income = avg_income if total_income ==0		
		gen log_income = log(total_income)
		
		recode m8c7 (1/10 = 0 "Chính thức") (11/15 = 1 "Phi chính thức") (.=99 "Missing"), gen(informal)
		keep if annual_rate !=.		
		
		
* Xử lý outliers
* age:
		drop if age>82.5
		drop if age<14.5
		drop if age>80
*age2 
		drop if age2 > 423.47232
		drop if age2 > 324.32016
		drop if age2 >268.43491
		drop if age2 >255.6589355
	
* edu:
		drop if edu>3.5
		/* Tạo biến giả
		
		gen Tiểu học = 1 if edu == 1 & edu !=.
				replace Tiểu học = 0 if edu != 1 & edu !=.

			gen THCS = 1 if edu == 2 & edu !=.
				replace THCS = 0 if edu != 2 & edu !=.
				
			gen THPT = 1 if edu == 3 & edu !=.
				replace THPT = 0 if edu !=3 & edu !=. */
				

*tsnguoi:
		drop if tsnguoi>8

*dependency:
		drop if dependency >3.5 

* total_income
		drop if total_income > 125250
		drop if total_income > 115771.875
		
*asset
		recode asset (7=0)
		
		gen asset_group_1 = " "
		gen asset_group = real(asset_group_1)
		replace asset_group = 2 if asset ==1
		replace asset_group = 3 if asset ==2
		replace asset_group = 1 if asset == 0 | asset ==  2 | asset ==  3 | asset == 4 | asset == 6
		lab var asset_group "Nhóm tài sản thế chấp"
		lab def asset_group 1 "Tài sản khác" 2 "Nhà và đất có sổ đỏ" 3 "Nhà và đất không có sổ đỏ"
		lab val asset_group asset_group
		
		gen Tài_sản_khác = 1 if asset_group == 1 & asset_group !=.
				replace Tài_sản_khác = 0 if asset_group != 1 & asset_group !=.

			gen Nhà_và_đất_có_sổ_đỏ= 1 if asset_group == 2 & asset_group !=.
				replace Nhà_và_đất_có_sổ_đỏ = 0 if asset_group != 2 & asset_group !=.
				
			
*annual_rate
		drop if annual_rate>55.2948
		
* log_income
		drop if log_income < 9.57063
		drop if log_income < 10.128505
		*drop if log_income < 10.55168
		*drop if log_income > 11.43704
		*drop if log_income > 11.23709
		*drop if log_income <10.88493

* m8c11b
	replace m8c11b =3 if m8c11b ==.
	gen annual_rate_2 = .
		replace annual_rate_2 = interest * 365 if m8c11b == 1  // Ngày
		replace annual_rate_2 = interest * 52 if m8c11b == 2    // Tuần
		replace annual_rate_2 = interest * 12 if m8c11b == 3    // Tháng
		replace annual_rate_2 = interest * 4 if m8c11b == 4     // Quý
		replace annual_rate_2 = interest * 2 if m8c11b == 5     // Nửa năm
		replace annual_rate_2 = interest if m8c11b == 6         // Năm

*income_class_1
		gen Hộ_nghèo = 1 if income_class_1 == 1 & income_class_1 !=.
				replace Hộ_nghèo = 0 if income_class_1 != 1 & income_class_1 !=.

			gen Hộ_cận_nghèo = 1 if income_class_1 == 2 & income_class_1 !=.
				replace Hộ_cận_nghèo = 0 if income_class_1 != 2 & income_class_1 !=.
			
			gen Hộ_giàu = 1 if income_class_1 == 3 & income_class_1 !=.
				replace Hộ_giàu = 0 if income_class_1 != 3 & income_class_1 !=.
		
	
	
	
	replace gender = 0 if gender == 2
	replace ttnt = 0 if ttnt ==2
		
	duplicates report
	duplicates drop
	
		reg credit i.gender age  age2 i.edu tsnguoi i.ttnt dependency_ratio Hộ_cận_nghèo Hộ_giàu Nhà_và_đất_có_sổ_đỏ i.informal
		estat ovtest 
		vif
		estat hettest
		
		replace credit = 0 if credit ==2 
		
		logit credit i.gender age age2 i.edu tsnguoi i.ttnt dependency_ratio Hộ_cận_nghèo Hộ_giàu Nhà_và_đất_có_sổ_đỏ  informal, robust
		eststo model1
		esttab model1 , pr2 b(%12.4f) se(%12.4f) scalar(p chi2 N) star(* 0.10 ** 0.05 *** 0.01) nogaps brackets

	

	estat gof, group(10)
	
	tab credit m8c17, row nofre
	tab m8c13
	
	sum  credit i.gender age  age2 i.edu tsnguoi i.ttnt dependency_ratio Hộ_cận_nghèo Hộ_giàu Nhà_và_đất_có_sổ_đỏ informal
	tab income_class_1 edu, row nofre
	tab edu income_class_1 , row nofre
	tab gender income_class_1, row nofre
	tab ttnt edu, row nofre
	
