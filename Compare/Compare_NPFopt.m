% A penalty-free method with nonmonotone line search for nonlinear optimization, Oct. 27, 2025.
% min. f(x) subject to c_ineq(x)>=0, c_eq(x)=0.
% Comparision on CUTEst problems for Algorithm 2.1 by T. Xu et al. as well as Lancelot B.
% Coded by Wenhao Fu.
clear; clc
pwd_compare = pwd;
cd('..')
addpath(pwd);
cd(pwd_compare)
MF_all = [0,2,4,6];

%%
for MF_idx=1:length(MF_all)
    MF = MF_all(MF_idx);
    %% Comparision for c(x)=0
    Test_Problem = 'Eq';
    pwd_Problem = fullfile(pwd_compare,'All_Problems/',Test_Problem);
    cd(pwd_Problem)
    SubFolderNames = GetFolders(pwd)';
    N = length(SubFolderNames);
    TP_index = 1 : N;
    Data_NPFoptEq = zeros(N,6);
    for Loop_i = TP_index
        fprintf('Comparision for c(x)=0,  MF=%1d, Problem Name%9s:  ',MF,SubFolderNames{Loop_i});
        Dir = fullfile(pwd_Problem,SubFolderNames{Loop_i});
        cd(Dir)
        try prob = cutest_setup(); catch; cutest_terminate(); prob = cutest_setup(); end
        x0 = prob.x; funf = @(x) cutest_obj(x); func = @(x) nonlcon(x,prob); 
        [ x, fval, exitflag, output, lambda ] = NPFopt(funf,func,x0,MF);
        Data_NPFoptEq(Loop_i,1:6) = [ fval, output.con, exitflag, output.iter, output.nf, output.ngf ];
    end
    %% Comparision for c(x)>0
    Test_Problem = 'IneqGe';
    pwd_Problem = fullfile(pwd_compare,'All_Problems/',Test_Problem);
    cd(pwd_Problem)
    SubFolderNames = GetFolders(pwd)';
    N = length(SubFolderNames);
    TP_index = 1 : N;
    Data_NPFoptIneqGe = zeros(N,6);
    for Loop_i=TP_index
        fprintf('Comparision for c(x)>0,  MF=%1d, Problem Name%9s:  ',MF,SubFolderNames{Loop_i});
        Dir = fullfile(pwd_Problem,SubFolderNames{Loop_i});
        cd(Dir)
        try prob = cutest_setup(); catch; cutest_terminate(); prob = cutest_setup(); end
        x0 = prob.x; funf = @(x) cutest_obj(x); func = @(x) nonlcon(x,prob); 
        [ x, fval, exitflag, output, lambda ] = NPFopt(funf,func,x0,MF);
        Data_NPFoptIneqGe(Loop_i,1:6) = [ fval, output.con, exitflag, output.iter, output.nf, output.ngf ];
    end
    %% Comparision for c(x)<0
    Test_Problem = 'IneqLe';
    pwd_Problem = fullfile(pwd_compare,'All_Problems/',Test_Problem);
    cd(pwd_Problem)
    SubFolderNames = GetFolders(pwd)';
    N = length(SubFolderNames);
    TP_index = 1 : N;
    Data_NPFoptIneqLe = zeros(N,6);
    for Loop_i=TP_index
        fprintf('Comparision for c(x)<0,  MF=%1d, Problem Name%9s:  ',MF,SubFolderNames{Loop_i});
        Dir = fullfile(pwd_Problem,SubFolderNames{Loop_i});
        cd(Dir)
        try prob = cutest_setup(); catch; cutest_terminate(); prob = cutest_setup(); end
        x0 = prob.x; funf = @(x) cutest_obj(x); func = @(x) nonlcon(x,prob); 
        [ x, fval, exitflag, output, lambda ] = NPFopt(funf,func,x0,MF);
        Data_NPFoptIneqLe(Loop_i,1:6) = [ fval, output.con, exitflag, output.iter, output.nf, output.ngf ];
    end
    Data_NPFopt{MF_idx} = [ Data_NPFoptEq; Data_NPFoptIneqGe; Data_NPFoptIneqLe ];
end
cd(pwd_compare);

save Data_NPFopt Data_NPFopt
 
%% Comparision with Lancelot
load Data_NPFopt
load('All_Problems_Lan/Lancelot_data_Eq.mat')
NitEq = Nit; NitEq_flag = Nit_flag;
load('All_Problems_Lan/Lancelot_data_IneqGe.mat')
NitIneqGe = Nit; NitIneqGe_flag = Nit_flag;
load('All_Problems_Lan/Lancelot_data_IneqLe.mat')
NitIneqLe = Nit; NitIneqLe_flag = Nit_flag;
Data_Lancelot = [ NitEq; NitIneqGe; NitIneqLe ];
Data_Lancelot_flag = [ NitEq_flag; NitIneqGe_flag; NitIneqLe_flag ];
N = size(Data_NPFopt{1},1);
Nf_Lan = Data_Lancelot(:,3);
Ng_Lan = Data_Lancelot(:,4);
Nf_MF0 = Data_NPFopt{1}(:,5);
Ng_MF0 = Data_NPFopt{1}(:,6);
Nf_MF2 = Data_NPFopt{2}(:,5);
Ng_MF2 = Data_NPFopt{2}(:,6);
Nf_MF4 = Data_NPFopt{3}(:,5);
Ng_MF4 = Data_NPFopt{3}(:,6);
Nf_MF6 = Data_NPFopt{4}(:,5);
Ng_MF6 = Data_NPFopt{4}(:,6);
%% Print to screan
fprintf('No.    name     n    m   |  lan: f       nf   ng  flag | MF0: f         nf  ng  flag |  MF2: f       nf   ng  flag |  MF4: f       nf   ng  flag |  MF6: f       nf   ng  flag\n');
for i=1:N
    fprintf(['%3d%9s %4d %4d:  ' ...
        '|%12.4e %4d %4d  %4d ' ...
        '|%12.4e %4d %4d  %4d ', ...
        '|%12.4e %4d %4d  %4d ', ...
        '|%12.4e %4d %4d  %4d ', ...
        '|%12.4e %4d %4d  %4d\n'], ...
        i,Data_Lancelot_flag{i,3},Data_Lancelot(i,1:2),str2num(Data_Lancelot_flag{i,2}),Data_Lancelot(i,3:5),Data_NPFopt{1}(i,[1,5,6,3]),Data_NPFopt{2}(i,[1,5,6,3]),Data_NPFopt{3}(i,[1,5,6,3]),Data_NPFopt{4}(i,[1,5,6,3]));
end
%% Write to txt
fid_output_detailed = fopen(fullfile(pwd_compare,'/output_detailed.txt'),'w');
fid_output_all = fopen(fullfile(pwd_compare,'/output_all.txt'),'w');
fprintf(fid_output_detailed,'No.     name     n    m   |  lan: f       nf   ng  flag | MF0: f         nf  ng  iter flag |  MF2: f       nf   ng  iter flag |  MF4: f       nf   ng iter  flag |  MF6: f       nf   ng iter  flag\n');
for i=1:N
    fprintf(fid_output_detailed,['%3d %9s %4d %4d:  ' ...
        '|%12.4e %4d %4d  %4d ' ...
        '|%12.4e %4d %4d %4d  %4d ', ...
        '|%12.4e %4d %4d %4d  %4d ', ...
        '|%12.4e %4d %4d %4d  %4d ', ...
        '|%12.4e %4d %4d %4d  %4d\n'], ...
        i,Data_Lancelot_flag{i,3},Data_Lancelot(i,1:2),str2num(Data_Lancelot_flag{i,2}),Data_Lancelot(i,3:5),Data_NPFopt{1}(i,[1,5,6,4,3]),Data_NPFopt{2}(i,[1,5,6,4,3]),Data_NPFopt{3}(i,[1,5,6,4,3]),Data_NPFopt{4}(i,[1,5,6,4,3]));
end
for i = 1 : N
    fprintf(fid_output_all,'%3d&%9s&%4d&%4d',i,Data_Lancelot_flag{i,3},Data_Lancelot(i,1:2));
    if Data_Lancelot(i,5) ~= 0
        fprintf(fid_output_all,'&%4d/%4d*',Data_Lancelot(i,3:4));
    else
        fprintf(fid_output_all,'&%4d/%4d ',Data_Lancelot(i,3:4));
    end
    for j = 1 : 4
        if Data_NPFopt{j}(i,3) <= 0
            fprintf(fid_output_all,'&%4d/%4d/%4d*',Data_NPFopt{j}(i,[5,6,4]));
        else
            fprintf(fid_output_all,'&%4d/%4d/%4d ',Data_NPFopt{j}(i,[5,6,4]));
        end
    end
    fprintf(fid_output_all,' \\\\ \n');
end
fclose(fid_output_detailed);
fclose(fid_output_all);


%% Plot
IdxFailed_Lan = []; IdxFailed_MF0 = []; IdxFailed_MF2 = []; IdxFailed_MF4 = []; IdxFailed_MF6 = [];
n_fail_lan = 0; n_fail_MF0 = 0; n_fail_MF2 = 0; n_fail_MF4 = 0; n_fail_MF6 = 0;
for i = 1 : N
    if Data_Lancelot(i,5) ~= 0
        Nf_Lan(i) = Inf; Ng_Lan(i) = Inf; IdxFailed_Lan=union(IdxFailed_Lan,i); n_fail_lan=n_fail_lan+1;
    end
    if Data_NPFopt{1}(i,3) <= 0
        Nf_MF0(i) = Inf; Ng_MF0(i) = Inf; IdxFailed_MF0=union(IdxFailed_MF0,i);n_fail_MF0=n_fail_MF0+1;
    end
    if Data_NPFopt{2}(i,3) <= 0
        Nf_MF2(i) = Inf; Ng_MF2(i) = Inf; IdxFailed_MF2=union(IdxFailed_MF2,i);n_fail_MF2=n_fail_MF2+1;
    end
    if Data_NPFopt{3}(i,3) <= 0
        Nf_MF4(i) = Inf; Ng_MF4(i) = Inf; IdxFailed_MF4=union(IdxFailed_MF4,i);n_fail_MF4=n_fail_MF4+1;
    end
    if Data_NPFopt{4}(i,3) <= 0
        Nf_MF6(i) = Inf; Ng_MF6(i) = Inf; IdxFailed_MF6=union(IdxFailed_MF6,i);n_fail_MF6=n_fail_MF6+1;
    end
end
Nf_min = min([ Nf_Lan,Nf_MF0, Nf_MF2, Nf_MF4, Nf_MF6 ]')';
Ng_min = min([ Ng_Lan, Ng_MF0, Ng_MF2, Ng_MF4, Ng_MF6 ]')';
clear P_Nf_Lan P_Ng_Lan P_Nf_MF0 P_Ng_MF0 P_Nf_MF2 P_Ng_MF2 P_Nf_MF4 P_Ng_MF4 P_Nf_MF6 P_Ng_MF6
k=1;
axis_tau = 1:0.01:25;
for tau=axis_tau
    P_Nf_Lan(k) = (sum((Nf_Lan./Nf_min) <= tau) / N);
    P_Ng_Lan(k) = (sum((Ng_Lan./Ng_min) <= tau) / N);
    P_Nf_MF0(k) = (sum((Nf_MF0./Nf_min) <= tau) / N);
    P_Ng_MF0(k) = (sum((Ng_MF0./Ng_min) <= tau) / N);
    P_Nf_MF2(k) = (sum((Nf_MF2./Nf_min) <= tau) / N);
    P_Ng_MF2(k) = (sum((Ng_MF2./Ng_min) <= tau) / N);
    P_Nf_MF4(k) = (sum((Nf_MF4./Nf_min) <= tau) / N);
    P_Ng_MF4(k) = (sum((Ng_MF4./Ng_min) <= tau) / N);
    P_Nf_MF6(k) = (sum((Nf_MF6./Nf_min) <= tau) / N);
    P_Ng_MF6(k) = (sum((Ng_MF6./Ng_min) <= tau) / N);
    k = k + 1;
end

fig = figure;
fig.Position = [655 283 600 300];
subplot(1,2,1)
stairs(axis_tau,P_Nf_Lan,'LineWidth',1.5)
hold on
stairs(axis_tau,P_Nf_MF0,'LineWidth',1.5)
stairs(axis_tau,P_Nf_MF2,'LineWidth',1.5)
stairs(axis_tau,P_Nf_MF4,'LineWidth',1.5)
stairs(axis_tau,P_Nf_MF6,'LineWidth',1.5)
legend('LANCELOT','MF=0','MF=2','MF=4','MF=6','Location','Southeast')
axis([1,tau,0.2,0.9])
title('nf/best nf')

subplot(1,2,2)
stairs(axis_tau,P_Ng_Lan,'LineWidth',1.5)
hold on
stairs(axis_tau,P_Ng_MF0,'LineWidth',1.5)
stairs(axis_tau,P_Ng_MF2,'LineWidth',1.5)
stairs(axis_tau,P_Ng_MF4,'LineWidth',1.5)
stairs(axis_tau,P_Ng_MF6,'LineWidth',1.5)
legend('LANCELOT','MF=0','MF=2','MF=4','MF=6','Location','Southeast')
title('ngf/best ngf')
axis([1,tau,0.2,0.9])
print(fig,'Fig.eps','-depsc2','-r600')

fprintf('Number of problems solved for Lancelot: %3d\n',N-n_fail_lan);
fprintf('Number of problems solved when MF=0: %3d\n',N-n_fail_MF0);
fprintf('Number of problems solved when MF=2: %3d\n',N-n_fail_MF2);
fprintf('Number of problems solved when MF=4: %3d\n',N-n_fail_MF4);
fprintf('Number of problems solved when MF=6: %3d\n',N-n_fail_MF6);

%% GetFolderNames
function [SubFolders] = GetFolders(ParentFolder)
SubFolderNames = dir(ParentFolder);
for i=1:length(SubFolderNames)
    if( isequal( SubFolderNames( i ).name, '.' )||...
            isequal( SubFolderNames( i ).name, '..')||...
            ~SubFolderNames( i ).isdir)
        continue;
    end
    SubFolder(i).SubFolderName = fullfile( SubFolderNames( i ).name );
end
temp = {SubFolder.SubFolderName};
idx = cellfun(@(x)~isempty(x),temp,'UniformOutput',true);
SubFolders = temp(idx);
end

%%  Constraint functions
function [cineq,ceq,gcineq,gceq] = nonlcon(x,prob)
% clear;clc
% try prob=cutest_setup();catch;cutest_terminate();prob=cutest_setup();end
% x = prob.x;
n = prob.n;
m = prob.m;
[ c, gc ] = cutest_cons(x); 
cl = prob.cl; cu = prob.cu; bl = prob.bl; bu = prob.bu;
meq = 0; mineq = 0; ceq = zeros(0,0); gceq = zeros(0,n); cineq = zeros(0,0); gcineq = zeros(0,n);
for i=1:m
    if cl(i) == cu(i)
        meq = meq + 1;
        ceq(meq,1) = c(i) - cu(i); gceq(meq,:) = gc(i,:); % i \in E
        continue
    end
    if cl(i) == -1e+20
        mineq = mineq + 1;
        cineq(mineq,1) = c(i) - cu(i); gcineq(mineq,:) = gc(i,:); % i \in I
        continue
    end
    if cu(i) == 1e+20
        mineq = mineq + 1;
        cineq(mineq,1) = - c(i) + cl(i); gcineq(mineq,:) = - gc(i,:); % i \in I
        continue
    end
    mineq = mineq + 1;
    cineq(mineq,1) = - c(i) + cl(i); gcineq(mineq,:) = - gc(i,:); 
    mineq = mineq + 1;
    cineq(mineq,1) = c(i) - cu(i); gcineq(mineq,:) = gc(i,:);
end
for i=1:n
    if bu(i) == 1e+20 && bl(i) ==  - 1e+20
        continue
    end
    if bl(i) == bu(i)
        meq = meq + 1;
        ceq(meq,1) = x(i) - bu(i); gceq(meq,i) = 1; % i \in E
        continue
    end
    if bl(i) == -1e+20 && bu(i) < 1e+20
        mineq = mineq + 1;
        cineq(mineq,1) = x(i) - bu(i); gcineq(mineq,i) = 1; % i \in I
        continue
    end
    if bu(i) == 1e+20 && bl(i) > - 1e+20
        mineq = mineq + 1;
        cineq(mineq,1) = - x(i) + bl(i); gcineq(mineq,i) = - 1; % i \in I
        continue
    end
    mineq = mineq + 1;
    cineq(mineq,1) = - x(i) + bl(i); gcineq(mineq,i) = - 1; 
    mineq = mineq + 1;
    cineq(mineq,1) = x(i) - bu(i); gcineq(mineq,i) = 1;
end
cineq = - cineq; gcineq = - gcineq;
end
