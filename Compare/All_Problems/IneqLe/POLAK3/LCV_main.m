% clear;  clc

prob=cutest_setup();
n=prob.n;mie=prob.m;
pname=prob.name;
xold=prob.x;
gamma = 0.6; eta = 1e-3;
nmax = 100; epsilon = 1e-3; sigma = 5;
nit = 0; flag = 0;
% fxold = funf(xold); gfxold = gradf(xold);
% cxold = func(xold); gcxold = gradc(xold);
[fxold,gfxold]=cutest_obj(xold);
[cxold,J]=cutest_cons(xold);gcxold=J';
m = length(cxold);
vxold = sum(max(cxold,0));
Bk = eye(n);

while nit < nmax && flag == 0
    if vxold>1e-8
        shift = sub1(cxold,gcxold);
    else
        shift.sum = 0;
        shift.tk = zeros(mie,1);
    end
    [dk,shift] = sub2(gfxold,Bk,cxold,gcxold,shift);
    lambda = sub3(dk,gfxold,Bk,cxold,gcxold,shift);
    if abs(vxold-sum(shift.tk))>1e-6
        delta = (norm(max(cxold-shift.tk,0),1))/(vxold-sum(shift.tk));
    else
        delta = 1;
    end
    if sigma <= delta*norm(lambda,'inf')
        sigma = max(5*sigma,delta*norm(lambda,'inf'));
    end
    Pxold = fxold + sigma * vxold;
    if norm(dk) <= epsilon && flag == 0; flag = 2; end
    alpha = 1;
    while alpha > 1e-6
        xnew = xold + alpha*dk;
        % fxnew = funf(xnew);
        % cxnew = func(xnew);
        [fxnew,gfxnew]=cutest_obj(xnew);
        [cxnew,J]=cutest_cons(xnew);gcxnew=J';
        vxnew = sum(max(cxnew,0));
        Pxnew = fxnew + sigma * vxnew;
        eqn38 = Pxnew - Pxold + 0.5*eta*alpha*dk'*Bk*dk;
        if eqn38<=0
            break
        end
        alpha = alpha * gamma;
    end
    if alpha <= 1e-6 && flag == 0; flag = -1; end
    fprintf('nit=%2d, dk=%.4f, alpha=%.4f,delta=%.4f, sigma=%.4f, lkv=%.4f, vk=%.4f, fk=%.4e\n',nit,norm(dk),alpha,delta,sigma,shift.sum,vxold,fxold);
    % gfxnew = gradf(xnew);
    % gcxnew = gradc(xnew);
    s = xnew - xold;
    q = gfxnew + gcxnew*lambda - ( gfxold + gcxold*lambda);
    Bk = bfgs(Bk,s,q);
    mm = eig(Bk);
    if min(mm)<1e-4
        Bk=eye(n);
    end
    xold = xnew;
    fxold = fxnew; gfxold = gfxnew;
    cxold = cxnew; gcxold = gcxnew;
    vxold = vxnew; Pxold = Pxnew;
    nit = nit + 1;
end
% fprintf('$(%.2f,%.2f,%.2f,%.2f)$ & %2d & %.4f & %.4f & %.4f & %.4f\\\\ \n',x0,nit-1,norm(dk),mk(xold,dkfea),sum(abs(funh(xold)))+sum(max(func(xold),0)),fxold);

cutest_terminate();




function B=bfgs(B,d,y)
bd=B*d;dbd=d'*bd;dy=d'*y;
if (dy<0.2*dbd)
    theta=0.8*dbd/(dbd-dy);
    y=theta*y+(1.0-theta)*bd;
end
dybar=d'*y;
B=B+y*y'/dybar-bd*bd'/dbd;
end

function shift = sub1(cxold,gcxold)
[n,mi] = size(gcxold);
prob = optimproblem;
d = optimvar('d',n,1);
t = optimvar('t',mi,1);
Sol0.d = zeros(n,1);
Sol0.t = zeros(mi,1);
prob.Objective = sum(t);
prob.Constraints.c1 = cxold+gcxold'*d<=t;
prob.Constraints.c2 = t>=0;
Options = optimset('Display','none');
[~,opt] = solve(prob,Sol0,options=Options,Solver='fmincon');
shift.sum = opt;
end

function [dk,shift] = sub2(gfxold,Bk,cxold,gcxold,shift)
[n,mi] = size(gcxold);
prob = optimproblem;
d = optimvar('d',n,1);
t = optimvar('t',mi,1);
Sol0.d = zeros(n,1);
Sol0.t = zeros(mi,1);
prob.Objective = gfxold'*d+0.5*d'*Bk*d;
prob.Constraints.c1 = cxold+gcxold'*d<=t;
prob.Constraints.c2 = t>=0;
prob.Constraints.c3 = sum(t)==shift.sum;
Options = optimset('Display','none');
Sol = solve(prob,Sol0,options=Options,Solver='fmincon');
dk = Sol.d;
shift.tk = Sol.t;
end

function lambda = sub3(dk,gfxold,Bk,cxold,gcxold,shift)
[n,mi] = size(gcxold);
mi_S = find(cxold+gcxold'*dk<-1e-8);
mi_V = find(cxold+gcxold'*dk>1e-8);
% mi_A = setdiff(1:mi,[mi_S;mi_V]);
prob = optimproblem;
lambda = optimvar('lambda',mi,1);
Sol0.lambda = zeros(mi,1);
prob.Objective = norm(lambda(mi_S),2)^2+norm(lambda(mi_V),2)^2;
prob.Constraints.c1 = gfxold+Bk*dk+gcxold*lambda==0;
prob.Constraints.c2 = lambda'*(cxold+gcxold'*dk-shift.tk)==0;
prob.Constraints.c3 = lambda>=0;
Options = optimset('Display','none');
Sol = solve(prob,Sol0,options=Options,Solver='fmincon');
lambda = Sol.lambda;
end
