
*get raw data;
%macro getNA(data_name);
data &data_name;
infile '/folders/myfolders/marketing_data.txt' delimiter=' ';
input y (var1-var13) ($);
%DO i = 1 %TO 13;
if var&i='NA' then var&i=.;
%end;
run;
%mend;
%getNA(marketing);

%macro stringToCharacter(data_name);
data &data_name;
set marketing;
%DO i = 1 %TO 13;
x&i=input(var&i,1.);
%end;
run;
%mend;
%stringToCharacter(marketing);

data model;
set marketing(drop=var1-var13);
run;

*chi square test;
proc logistic data=model;
class x1-x13;
model y= x1-x13/selection=stepwise details;
run;

*correlation check;
proc freq data=model;
table x8*x9/chisq;
run;

proc freq data=model;
table x12*x13/chisq;
run;

proc freq data=model;
table x2*x7/chisq;
run;

*multiple imputation;
proc MI data= model seed=1234 minimum=1 maximum=9 out=model3 round=1 nimpute=1;
var x8 x9;
run;

proc freq data=model3;
table x8*x9/chisq;
run;

proc MI data= model3 seed=1234 minimum=1 maximum=9 out=model4 round=1 nimpute=1;
var x12 x13;
run;

proc freq data=model4;
table x12*x13/chisq;
run;

proc MI data= model4 seed=1234 minimum=1 maximum=9 out=model4 round=1 nimpute=1;
var x2 x7;
run;

proc freq data=model4;
table x2*x7/chisq;
run;

*chi square test;
proc logistic data=model4;
class x1-x13;
model y= x1-x13/selection=stepwise details;
run;

data model5;
set model4;
var1=x8-x9;
run;

*new variable;
data model5;
set model5(drop=x8 x9);
if var1 <0 then var1=.;
run;

*replace missing variables;
data model5;
set model5;
if x4=. then x4=7;
if x5=. then x5=10;
if x6=. then x6=10;
if x10=. then x10=4;
if x11=. then x11=6;
run;

proc logistic data=model5;
class  x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x13 var1;
model y= x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x13 var1/selection=stepwise details;
run;

data model6;
set model5(drop=x6);
if (1<=y<=5) then y = 0;
else y=1;
run;

proc freq data=model6;
table y*(x1 x2 x3 x4 x5 x7 x10 x11 x12 x13 var1)/chisq;
run;

proc logistic data=model6;
class x1 x2 x3 x4 x5 x7 x10 x11 x12 x13 var1;
model y=x1 x2 x3 x4 x5 x7 x10 x11 x12 x13 var1/selection=stepwise details;
run;

*split test and training data;
data model7;
set model6;
if ranuni(5555) <0.7 then split=1;
else split=.;
records=1;
run;

proc logistic data=model7 descending;
class x1 x2 x3 x4 x5 x7 x10 x11 x12 x13 var1;
model y=x1 x2 x3 x4 x5 x7 x10 x11 x12 x13 var1;
output out=model7_out pred=pred;
run;

proc sort data=model7_out;
by descending pred;
run;

*decile analysis using training data;
proc univariate data=model7_out(where=(split=1));
var pred y;
output out=preddata sumwgt=sumwgt;
run;

data model7_dec;
set model7_out(where=(split=1));
if (_n_=1) then set preddata;
retain sumwgt;
number =_n_;
if number < .1*sumwgt then dec=0; else
if number < .2*sumwgt then dec=1; else
if number < .3*sumwgt then dec=2; else
if number < .4*sumwgt then dec=3; else
if number < .5*sumwgt then dec=4; else
if number < .6*sumwgt then dec=5; else
if number < .7*sumwgt then dec=6; else
if number < .8*sumwgt then dec=7; else
if number < .9*sumwgt then dec=8; else
dec=9;
run;

proc freq data=model7_dec;
table dec;
run;

proc tabulate data=model7_dec;
class dec;
var y pred records;
table dec='Decile' all='Total',
	records='Prospects'*sum=''*f=comma10.
	pred='Predicted Probability'*(mean='')
	y='percent Income'*(mean='')
	/rts=9 row=float;
run;

*decile analysis using test data;
proc univariate data=model7_out(where=(split=.));
var pred y;
output out=preddata sumwgt=sumwgt;
run;

data model7_dec;
set model7_out(where=(split=.));
if (_n_=1) then set preddata;
retain sumwgt;
number =_n_;
if number < .1*sumwgt then dec=0; else
if number < .2*sumwgt then dec=1; else
if number < .3*sumwgt then dec=2; else
if number < .4*sumwgt then dec=3; else
if number < .5*sumwgt then dec=4; else
if number < .6*sumwgt then dec=5; else
if number < .7*sumwgt then dec=6; else
if number < .8*sumwgt then dec=7; else
if number < .9*sumwgt then dec=8; else
dec=9;
run;

proc tabulate data=model7_dec;
class dec;
var y pred records;
table dec='Decile' all='Total',
	records='Prospects'*sum=''*f=comma10.
	pred='Predicted Probability'*(mean='')
	y='percent Income'*(mean='')
	/rts=9 row=float;
run;
proc freq data=model7_dec;
table dec*y;
run;

*gains chart-validation graph;
data graph;
input y1 y2 x @@;
cards; 
0 0 0
19.09 17.64 10
37.40 34.38 20
54.46 50.33 30
67.29 63.77 40
77.62 76.14 50
85.05 85.76 60
91.24 93.35 70
95.15 96.53 80
97.50 99.06 90
100 100 100
;

proc sgplot data=graph;
xaxis type=discrete;
series x=x y=y1;
series x=x y=y2;
series x=x y=x;
run;
