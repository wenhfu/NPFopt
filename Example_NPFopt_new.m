% A penalty-free method with nonmonotone line search for nonlinear optimization
% This is a test example for optimization problem
%       min       f(x) = - x1 + rho * ( x1^2 + x2^2-1 )
%  subject to  c(x) = x1^2 + x2^2 - 1 = 0 

clear; clc;
opts.nfmax=5000;
opts.itermax=300;
opts.display=0;
con_type = 0;
MF_all = [0,1,2,3,4,5,6];

%% Results of Tab 1
fid = fopen('Example_out_new.txt','w');
fprintf(fid,'Results for Tab 1:\n');
for MF = MF_all
    fprintf(fid,'\nMF  = %4d:\n',MF);
    for rho = [10,100,1000]
        fprintf(fid,'rho = %4d:   ',rho);
        funf = @(x) deal(-x(1)+rho*(x(1)^2+x(2)^2-1), [-1+2*rho*x(1); 2*rho*x(2)]);
        func = @(x) deal([],x(1)^2+x(2)^2-1,[],[2*x(1), 2*x(2)]);
        x0 = [0.8;0.6];
        [x,fval,exitflag,output,lambda] = NPFopt_new(funf,func,x0,MF,opts);
        fprintf(fid,'nit = %4d,  nf = %4d,  ngf = %4d,  ||c(x)|| = %.4e,   Res = %.4e\n',output.iter,output.nf,output.ngf,output.con,output.Res);
    end
end

%% Results of Tab 2
fprintf(fid,'\nResults for Tab 2\n');
opts.epsilon=1e-9;
for MF = MF_all
    fprintf(fid,'\nMF  = %4d:\n',MF);
    for rho = [10,100,1000]
        fprintf(fid,'rho = %4d:   ',rho);
        funf = @(x) deal(-x(1)+rho*(x(1)^2+x(2)^2-1), [-1+2*rho*x(1); 2*rho*x(2)]);
        % func = @(x) deal(x(1)^2+x(2)^2-1,[2*x(1), 2*x(2)],[],[]);
        func = @(x) deal([],x(1)^2+x(2)^2-1,[],[2*x(1), 2*x(2)]);
        t = 1e-4;
        x0 = [cos(t);sin(t)];
        [x,fval,exitflag,output,lambda] = NPFopt_new(funf,func,x0,MF,opts);
        fprintf(fid,'nit = %4d,  nf = %4d,  ngf = %4d,  ||c(x)|| = %.4e,   Res = %.4e\n',output.iter,output.nf,output.ngf,output.con,output.Res);
    end
end

fclose(fid);
%%
fileread('Example_out_new.txt')

