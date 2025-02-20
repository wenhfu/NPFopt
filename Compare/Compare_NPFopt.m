% A penalty-free method with nonmonotone line search for nonlinear optimization, Feb. 10, 2025.
% min. f(x) subject to c(x)=0, c(x)<0 or c(x)>0.
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
    con_type = 0;
    pwd_Problem = fullfile(pwd_compare,'All_Problems/',Test_Problem);
    cd(pwd_Problem)
    SubFolderNames = GetFolders(pwd)';
    N = length(SubFolderNames);
    TP_index = 1:N;         
    Data_NPFoptEq = zeros(N,6);
    for Loop_i=TP_index
        fprintf('\nComparision for c(x)=0,  MF=%1d, Problem Name%9s:  ',MF,SubFolderNames{Loop_i});
        Dir = fullfile(pwd_Problem,SubFolderNames{Loop_i});
        cd(Dir)
        prob=cutest_setup();
        funf=@(x) cutest_obj(x); func=@(x) cutest_cons(x); x0=prob.x;
        [x,fval,exitflag,output,lambda] = NPFopt(funf,func,con_type,x0,MF);
        cutest_terminate();
        Data_NPFoptEq(Loop_i,1:6) = [fval,output.con,exitflag,output.iter,output.nf,output.ngf];
    end
    %% Comparision for c(x)>0
    Test_Problem = 'IneqGe';
    con_type = 1;
    pwd_Problem = fullfile(pwd_compare,'All_Problems/',Test_Problem);
    cd(pwd_Problem)
    SubFolderNames = GetFolders(pwd)';
    N = length(SubFolderNames);
    TP_index = 1:N;
    Data_NPFoptIneqGe = zeros(N,6);
    for Loop_i=TP_index
        fprintf('\nComparision for c(x)>0,  MF=%1d, Problem Name%9s:  ',MF,SubFolderNames{Loop_i});
        Dir = fullfile(pwd_Problem,SubFolderNames{Loop_i});
        cd(Dir)
        prob=cutest_setup();
        funf=@(x) cutest_obj(x); func=@(x) cutest_cons(x); x0=prob.x;
        [x,fval,exitflag,output,lambda] = NPFopt(funf,func,con_type,x0,MF);
        cutest_terminate();
        Data_NPFoptIneqGe(Loop_i,1:6) = [fval,output.con,exitflag,output.iter,output.nf,output.ngf];
    end
    %% Comparision for c(x)<0
    Test_Problem = 'IneqLe';
    con_type = -1;
    pwd_Problem = fullfile(pwd_compare,'All_Problems/',Test_Problem);
    cd(pwd_Problem)
    SubFolderNames = GetFolders(pwd)';
    N = length(SubFolderNames);
    TP_index = 1:N;
    Data_NPFoptIneqLe = zeros(N,6);
    for Loop_i=TP_index
        fprintf('\nComparision for c(x)<0,  MF=%1d, Problem Name%9s:  ',MF,SubFolderNames{Loop_i});
        Dir = fullfile(pwd_Problem,SubFolderNames{Loop_i});
        cd(Dir)
        prob=cutest_setup();
        funf=@(x) cutest_obj(x); func=@(x) cutest_cons(x); x0=prob.x;
        [x,fval,exitflag,output,lambda] = NPFopt(funf,func,con_type,x0,MF);
        cutest_terminate();
        Data_NPFoptIneqLe(Loop_i,1:6) = [fval,output.con,exitflag,output.iter,output.nf,output.ngf];
    end
    Data_NPFopt{MF_idx} = [Data_NPFoptEq;Data_NPFoptIneqGe;Data_NPFoptIneqLe];
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
Data_Lancelot = [NitEq;NitIneqGe;NitIneqLe];
Data_Lancelot_flag = [NitEq_flag;NitIneqGe_flag;NitIneqLe_flag];
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
Idx_large=[];
for i=1:N
    if Nf_MF0(i)==0 && Ng_MF0(i)==0
        Idx_large=union(Idx_large,i);
    end
end
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
fid_output_detailed=fopen(fullfile(pwd_compare,'/output_detailed.txt'),'w');
fid_output_all=fopen(fullfile(pwd_compare,'/output_all.txt'),'w');
fid_output_successful=fopen(fullfile(pwd_compare,'/output_successful.txt'),'w');
fid_output_failed=fopen(fullfile(pwd_compare,'/output_failed.txt'),'w');
fprintf(fid_output_detailed,'No.     name     n    m   |  lan: f       nf   ng  flag | MF0: f         nf  ng  iter flag |  MF2: f       nf   ng  iter flag |  MF4: f       nf   ng iter  flag |  MF6: f       nf   ng iter  flag\n');
for i=1:N
    fprintf(fid_output_detailed,['%3d %9s %4d %4d:  ' ...
        '|%12.4e %4d %4d  %4d ' ...
        '|%12.4e %4d %4d %4d  %4d ', ...
        '|%12.4e %4d %4d %4d  %4d ', ...
        '|%12.4e %4d %4d %4d  %4d ', ...
        '|%12.4e %4d %4d %4d  %4d\n'], ...
        i,Data_Lancelot_flag{i,3},Data_Lancelot(i,1:2),str2num(Data_Lancelot_flag{i,2}),Data_Lancelot(i,3:5),Data_NPFopt{1}(i,[1,5,6,4,3]),Data_NPFopt{2}(i,[1,5,6,4,3]),Data_NPFopt{3}(i,[1,5,6,4,3]),Data_NPFopt{4}(i,[1,5,6,4,3]));
    fprintf(fid_output_all,['%3d&%9s&%4d&%4d&' ...
        '%4d/%4d&' ...
        '%4d/%4d/%4d&', ...
        '%4d/%4d/%4d&', ...
        '%4d/%4d/%4d&', ...
        '%4d/%4d/%4d \\\\ \n'], ...
        i,Data_Lancelot_flag{i,3},Data_Lancelot(i,1:2),Data_Lancelot(i,3:4),Data_NPFopt{1}(i,[5,6,4]),Data_NPFopt{2}(i,[5,6,4]),Data_NPFopt{3}(i,[5,6,4]),Data_NPFopt{4}(i,[5,6,4]));
    if Data_NPFopt{4}(i,3)>0
        fprintf(fid_output_successful,['%3d&%9s&%4d&%4d&' ...
            '%4d/%4d&' ...
            '%4d/%4d/%4d&', ...
            '%4d/%4d/%4d&', ...
            '%4d/%4d/%4d&', ...
            '%4d/%4d/%4d \\\\ \n'], ...
           i,Data_Lancelot_flag{i,3},Data_Lancelot(i,1:2),Data_Lancelot(i,3:4),Data_NPFopt{1}(i,[5,6,4]),Data_NPFopt{2}(i,[5,6,4]),Data_NPFopt{3}(i,[5,6,4]),Data_NPFopt{4}(i,[5,6,4]));
    elseif ~isempty(setdiff(i,Idx_large))
        fprintf(fid_output_failed,['%3d&%9s&%4d&%4d&' ...
            '%4d/%4d&' ...
            '%4d/%4d/%4d&', ...
            '%4d/%4d/%4d&', ...
            '%4d/%4d/%4d&', ...
            '%4d/%4d/%4d \\\\ \n'], ...
            i,Data_Lancelot_flag{i,3},Data_Lancelot(i,1:2),Data_Lancelot(i,3:4),Data_NPFopt{1}(i,[5,6,4]),Data_NPFopt{2}(i,[5,6,4]),Data_NPFopt{3}(i,[5,6,4]),Data_NPFopt{4}(i,[5,6,4]));
    end
end
fclose(fid_output_detailed);
fclose(fid_output_all);
fclose(fid_output_successful);
fclose(fid_output_failed);

%% Plot
IdxFailed = [];
n_fail_lan = 0; n_fail_MF0 = 0; n_fail_MF2 = 0; n_fail_MF4 = 0; n_fail_MF6 = 0;
for i=1:N
    if Data_Lancelot(i,5)~=0 && ~isempty(setdiff(i,Idx_large))
        Nf_Lan(i) = Inf; Ng_Lan(i) = Inf; IdxFailed=union(IdxFailed,i); n_fail_lan=n_fail_lan+1;
    end
    if Data_NPFopt{1}(i,3)<=0 && ~isempty(setdiff(i,Idx_large))
        Nf_MF0(i) = Inf; Ng_MF0(i) = Inf; IdxFailed=union(IdxFailed,i);n_fail_MF0=n_fail_MF0+1;
    end
    if Data_NPFopt{2}(i,3)<=0 && ~isempty(setdiff(i,Idx_large))
        Nf_MF2(i) = Inf; Ng_MF2(i) = Inf; IdxFailed=union(IdxFailed,i);n_fail_MF2=n_fail_MF2+1;
    end
    if Data_NPFopt{3}(i,3)<=0 && ~isempty(setdiff(i,Idx_large))
        Nf_MF4(i) = Inf; Ng_MF4(i) = Inf; IdxFailed=union(IdxFailed,i);n_fail_MF4=n_fail_MF4+1;
    end
    if Data_NPFopt{4}(i,3)<=0 && ~isempty(setdiff(i,Idx_large))
        Nf_MF6(i) = Inf; Ng_MF6(i) = Inf; IdxFailed=union(IdxFailed,i);n_fail_MF6=n_fail_MF6+1;
    end
end
% Idx=IdxSucceed;
% IdxSucceed=setdiff(1:length(Nf_Lan),IdxFailed);
Nf_min = min([Nf_Lan,Nf_MF0,Nf_MF2,Nf_MF4,Nf_MF6]')';
Ng_min = min([Ng_Lan,Ng_MF0,Ng_MF2,Ng_MF4,Ng_MF6]')';
Idx = setdiff(1:N,Idx_large);
N_output = length(Idx);
Nf_min=Nf_min(Idx);
Ng_min=Ng_min(Idx);
clear P_Nf_Lan P_Ng_Lan P_Nf_MF0 P_Ng_MF0 P_Nf_MF2 P_Ng_MF2 P_Nf_MF4 P_Ng_MF4 P_Nf_MF6 P_Ng_MF6
k=1;
axis_tau = 1:0.5:25;
for tau=axis_tau
    P_Nf_Lan(k) = (sum((Nf_Lan(Idx)./Nf_min)<tau)/N_output);
    P_Ng_Lan(k) = (sum((Ng_Lan(Idx)./Ng_min)<tau)/N_output);
    P_Nf_MF0(k) = (sum((Nf_MF0(Idx)./Nf_min)<tau)/N_output);
    P_Ng_MF0(k) = (sum((Ng_MF0(Idx)./Ng_min)<tau)/N_output);
    P_Nf_MF2(k) = (sum((Nf_MF2(Idx)./Nf_min)<tau)/N_output);
    P_Ng_MF2(k) = (sum((Ng_MF2(Idx)./Ng_min)<tau)/N_output);
    P_Nf_MF4(k) = (sum((Nf_MF4(Idx)./Nf_min)<tau)/N_output);
    P_Ng_MF4(k) = (sum((Ng_MF4(Idx)./Ng_min)<tau)/N_output);
    P_Nf_MF6(k) = (sum((Nf_MF6(Idx)./Nf_min)<tau)/N_output);
    P_Ng_MF6(k) = (sum((Ng_MF6(Idx)./Ng_min)<tau)/N_output);
    k=k+1;
end

fig = figure;
fig.Position = [655 283 600 300];
subplot(1,2,1)
stairs(axis_tau,P_Nf_Lan,'LineWidth',1)
hold on
stairs(axis_tau,P_Nf_MF0,'LineWidth',1)
stairs(axis_tau,P_Nf_MF2,'LineWidth',1)
stairs(axis_tau,P_Nf_MF4,'LineWidth',1)
stairs(axis_tau,P_Nf_MF6,'LineWidth',1)
legend('LANCELOT','MF=0','MF=2','MF=4','MF=6','Location','Southeast')
axis([2,tau,0.3,0.9])
title('nf/best nf')

subplot(1,2,2)
stairs(axis_tau,P_Ng_Lan,'LineWidth',1)
hold on
stairs(axis_tau,P_Ng_MF0,'LineWidth',1)
stairs(axis_tau,P_Ng_MF2,'LineWidth',1)
stairs(axis_tau,P_Ng_MF4,'LineWidth',1)
stairs(axis_tau,P_Ng_MF6,'LineWidth',1)
legend('LANCELOT','MF=0','MF=2','MF=4','MF=6','Location','Southeast')
title('ngf/best ngf')
axis([2,tau,0.3,0.9])
print(fig,'Fig.eps','-depsc2','-r600')

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
