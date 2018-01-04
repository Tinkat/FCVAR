clear all;

%------Import data
cds_input = xlsread('CDS_Bond_5y_sync.xlsx',1); %skip first row because var name
credit = csvread('bond_spreadBP_usriskfree_06122017.csv',1); %skip first row because var name

cds = cds_input(:,[2 4 6 8 10 12 14 16 18 20 22 24 26]);

% Add path containing Auxillary files required for estimation.
addpath Auxiliary/
%%-------Unit root tests
%-----Dickey-Fuller test
%[h1,pValue1,stat1,cValue1,reg1] = adftest(x1);

%--------Choosing estimation options
opt = EstOptions; %Define variable to store Estimation Options (object)
opt.dbMin = [0.01 0.01]; %lower bound for d, b
opt.dbMax = [2.00 2.00]; %upper bound for d, b
opt.unrConstant = 0; %include an unrestricted constant? 1 = yes, 0 = no
opt.rConstant = 0; %include a restricted constant? 1 = yes, 0 = no
opt.levelParam = 1; %include level parameter? 1 = yes, 0 = no
opt.constrained = 0; %impose restriction dbMax >= d >= b >= dbMin? 1 = yes, 0 = no
opt.restrictDB = 0; %impose restriction d=b? 1 = yes, 0 = no
opt.db0 = [.8 .8]; %set starting values for optimization algorithm
opt.N = 0; %number of initial values to condition upon
opt.print2screen = 1; %print output
opt.printRoots = 1; %print roots of characteristic
opt.plotRoots = 1; %plot roots of characteristic polynomial
opt.gridSearch = 0; %For more accurate estimation, perform the grid search.
                        %This will make estimation take longer
opt.plotLike = 0;  %Plot the likelihood (if gridSearch = 1)
opt.progress = 0; %Show grid search progress indicator waitbar
opt.updateTime = 5; %How often progress is updated (seconds)

%Linux example
opt.progLoc = '"H:/FCVAR/fdpval"'; %location path with program name
                                    %of fracdist program, if installed
                                    %NOTE: use both single (outside) and
                                    %double quotes (inside). This is
                                    %especially important if path name has
                                    %space
DefaultOpt = opt; % store the options for restoring them in between hypothesis

cds_names = {'Citi','JPM','BoA','BNP','DB','BARC','GS','WF','CA', 'MS','RBS','SG','Unicredit'};
str_names = string(cds_names);


for i=1:13
    bank= i;
x1 = horzcat(credit(:,bank),cds(:,bank));

%%-------Initialization
p = size(x1,2); %system dimension
kmax = 5; %maximum number of lags for VECM
order = 12; %number of lags for white noise test in lag selection
printWNtest = 1; %to print results of white noise tests post-estimation

%------Select lag-order ------------
fprintf(str_names(i)); 
LagSelect(x1, kmax, p, order, opt);

%------COINTEGRATION RANK Test------
 k=1; %lag parameter
 rankTestStats = RankTests(x1,k,opt);
%------Johansen COINTEGRATION RANK Test------
%[h,pValue,stat,cValue] = jciftest(x1,'model','H1','lags',1:2);

%plot(x2)
%% --------- UNRESTRICTED MODEL ESTIMATION ---------- %
%plot(x1)
r=1; 
%k=6;
opt1 = DefaultOpt;  opt1.gridSearch = 0;
m1 = FCVARestn(x1, k, r, opt1); % This model is now in the structure m1.
mv_wntest(m1.Residuals, order, printWNtest);

%% --------- IMPOSE RESTRICTIONS AND TEST THEM ---------- %
DefaultOpt.gridSearch = 0;	% turn off grid search for restricted models1
							%	because it's too intensive.1
%% Test restriction that d=b=1.
opt1 = DefaultOpt;
%opt1.restrictDB = 0; %impose restriction d=b? 1 = yes, 0 = no
opt1.R_psi = [1 0];
opt1.r_psi = 1;

m1r1 = FCVARestn(x1, k, r, opt1); % This restricted model is now in the structure m1r1.
mv_wntest(m1r1.Residuals, order, printWNtest);
Hdb = HypoTest(m1, m1r1); 	% Test the null of m1r1 against the alternative m1 and
							% store the results in the structure Hdb.

end                           
                            
                            
                            
                            

                                    






