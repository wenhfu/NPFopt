clear;clc
warning('off')
pwd_original = pwd;
pwd_SIF = '/home/mpc-linux-01/Softwares/CUTE/sif';
pwd_sdlan = '/home/mpc-linux-01/Softwares/CUTE/lancelot/bin/sdlan ';
cd(pwd_original)
opts = delimitedTextImportOptions("NumVariables", 15);
opts.DataLines = [1,3];
opts.Delimiter = " ";
opts.EmptyLineRule = "read";
opts.ConsecutiveDelimitersRule = "join";
opts.LeadingDelimitersRule = "ignore";
%% Copy_SIFs & Run_LANCELOT
%% Eq
cd('../All_Problems/Eq');
pwd_Problem = pwd;
pwd_Problem_Lan = [pwd_original,'/Eq/'];
SubFolderNames = GetFolders(pwd_Problem)';
N = length(SubFolderNames);
TP_index = 1:N;
for i=TP_index
    mkdir([pwd_Problem_Lan,SubFolderNames{i}])
    cd([pwd_Problem_Lan,SubFolderNames{i}])
    % copyfile([pwd_SIF,'/',SubFolderNames{i},'.SIF']);
    SHELL = [pwd_sdlan,SubFolderNames{i}];
    system(SHELL);
    SUMMARY = readtable("SUMMARY.d", opts);
    n = str2double(SUMMARY.Var2(2));
    Nf = str2double(SUMMARY.Var12(2));
    Ng = str2double(SUMMARY.Var13(2));
    Flag = str2double(SUMMARY.Var15(2));
    meqlin = str2double(SUMMARY.Var10(1));
    meqnlin = str2double(SUMMARY.Var11(1));
    mieqlin = str2double(SUMMARY.Var12(1));
    mieqnlin = str2double(SUMMARY.Var13(1));
    Obj_Type = SUMMARY.Var3{2};
    Obj_Value = SUMMARY.Var7{3};
    Obj_Name = SUMMARY.Var1{1};
    Nit(i,1) = n;
    Nit(i,2) = meqlin+meqnlin;
    Nit(i,3) = Nf;
    Nit(i,4) = Ng;
    Nit(i,5) = Flag;
    Nit_flag{i,1} = Obj_Type;
    Nit_flag{i,2} = Obj_Value;
    Nit_flag{i,3} = Obj_Name;
    if min(meqlin+meqnlin,mieqlin+mieqnlin)~=0
        disp(SubFolderNames{i})
    end
end
% disp(Nit)
cd(pwd_original)
save Lancelot_data_Eq Nit Nit_flag
clear Nit Nit_flag
%% IneqGe
cd('../All_Problems/IneqGe');
pwd_Problem = pwd;
pwd_Problem_Lan = [pwd_original,'/IneqGe/'];
SubFolderNames = GetFolders(pwd_Problem)';
N = length(SubFolderNames);
TP_index = 1:N;
for i=TP_index
    mkdir([pwd_Problem_Lan,SubFolderNames{i}])
    cd([pwd_Problem_Lan,SubFolderNames{i}])
    % copyfile([pwd_SIF,'/',SubFolderNames{i},'.SIF']);
    SHELL = [pwd_sdlan,SubFolderNames{i}];
    system(SHELL);
    SUMMARY = readtable("SUMMARY.d", opts);
    n = str2double(SUMMARY.Var2(2));
    Nf = str2double(SUMMARY.Var12(2));
    Ng = str2double(SUMMARY.Var13(2));
    Flag = str2double(SUMMARY.Var15(2));
    meqlin = str2double(SUMMARY.Var10(1));
    meqnlin = str2double(SUMMARY.Var11(1));
    mieqlin = str2double(SUMMARY.Var12(1));
    mieqnlin = str2double(SUMMARY.Var13(1));
    Obj_Type = SUMMARY.Var3{2};
    Obj_Value = SUMMARY.Var7{3};
    Obj_Name = SUMMARY.Var1{1};
    Nit(i,1) = n;
    Nit(i,2) = mieqlin+mieqnlin;
    Nit(i,3) = Nf;
    Nit(i,4) = Ng;
    Nit(i,5) = Flag;
    Nit_flag{i,1} = Obj_Type;
    Nit_flag{i,2} = Obj_Value;
    Nit_flag{i,3} = Obj_Name;
    if min(meqlin+meqnlin,mieqlin+mieqnlin)~=0
        disp(SubFolderNames{i})
    end
end
% disp(Nit)
cd(pwd_original)
save Lancelot_data_IneqGe Nit Nit_flag
clear Nit Nit_flag
%% IneqLe
cd('../All_Problems/IneqLe');
pwd_Problem = pwd;
pwd_Problem_Lan = [pwd_original,'/IneqLe/'];
SubFolderNames = GetFolders(pwd_Problem)';
N = length(SubFolderNames);
TP_index = 1:N;
for i=TP_index
    mkdir([pwd_Problem_Lan,SubFolderNames{i}])
    cd([pwd_Problem_Lan,SubFolderNames{i}])
    % copyfile([pwd_SIF,'/',SubFolderNames{i},'.SIF']);
    SHELL = [pwd_sdlan,SubFolderNames{i}];
    system(SHELL);
    SUMMARY = readtable("SUMMARY.d", opts);
    n = str2double(SUMMARY.Var2(2));
    Nf = str2double(SUMMARY.Var12(2));
    Ng = str2double(SUMMARY.Var13(2));
    Flag = str2double(SUMMARY.Var15(2));
    meqlin = str2double(SUMMARY.Var10(1));
    meqnlin = str2double(SUMMARY.Var11(1));
    mieqlin = str2double(SUMMARY.Var12(1));
    mieqnlin = str2double(SUMMARY.Var13(1));
    Obj_Type = SUMMARY.Var3{2};
    Obj_Value = SUMMARY.Var7{3};
    Obj_Name = SUMMARY.Var1{1};
    Nit(i,1) = n;
    Nit(i,2) = mieqlin+mieqnlin;
    Nit(i,3) = Nf;
    Nit(i,4) = Ng;
    Nit(i,5) = Flag;
    Nit_flag{i,1} = Obj_Type;
    Nit_flag{i,2} = Obj_Value;
    Nit_flag{i,3} = Obj_Name;
    if min(meqlin+meqnlin,mieqlin+mieqnlin)~=0
        disp(SubFolderNames{i})
    end
end
% disp(Nit)
cd(pwd_original)
save Lancelot_data_IneqLe Nit Nit_flag


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