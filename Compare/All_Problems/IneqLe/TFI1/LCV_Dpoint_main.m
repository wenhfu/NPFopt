% clear;  clc
% function [] = LCV_Dpoint_main()
prob=cutest_setup();
n=prob.n;mie=prob.m;
pname=prob.name;
xold=prob.x;
gamma = 0.6; etaf = 1e-3; etav = 1e-3; etaq = 0.1; beta1 = 0.9; beta2 = 0.75;
nmax = 100; epsilon = 1e-3; rho = 1; tau = 2.1; delta = 5;
nit = 0; flag = 0;
% fxold = funf(xold); gfxold = gradf(xold);
% cxold = func(xold); gcxold = gradc(xold);
[fxold,gfxold]=cutest_obj(xold);
[cxold,J]=cutest_cons(xold);gcxold=J';
m = length(cxold);
vxold = sum(max(cxold,0)); vkmax = max(vxold,5);
Bk = eye(n);

while nit < nmax && flag == 0
    shift = sub1(Bk,cxold,gcxold);
    while rho>0
        [dk,shift,lambda] = sub2(rho,gfxold,Bk,cxold,gcxold,shift);
        eqn1 = vxold - shift.dkrho - etaq*(vxold - shift.dkinf);
        eqn2 = vxold -rho*gfxold'*dk-0.5*dk'*Bk*dk - shift.dkrho - etaq*(vxold - shift.dkinf);
        if eqn1>=0 && eqn2>=0
            break
        end
        rho = rho/2;
    end

    if norm(dk) <= epsilon && flag == 0; flag = 2; end
    alpha = 1;
    while alpha > 1e-6
        xnew = xold + alpha*dk;
        [fxnew,gfxnew]=cutest_obj(xold);
        [cxnew,J]=cutest_cons(xold);gcxnew=J';
        % gfxnew = gradf(xnew);
        % gcxnew = gradc(xnew);
        vxnew = sum(max(cxnew,0));
        eqn36 = (-alpha*gfxold'*dk)-delta*(vxold-norm(max(cxold+alpha*gcxold'*dk,0),1))^tau;
        if eqn36 >= 0
            eqn37 = fxold - fxnew - etaf*(-alpha*gfxold'*dk);
            eqn38 = vxnew - vkmax;
            if eqn36>=0 && eqn37 <=0
                break
            end
        else
            eqn39 = vxold-vxnew - etav*(vxold-norm(max(cxold+alpha*gcxold'*dk,0),1));
            if eqn39>=0
                vkmax = max(beta1*vkmax,beta2*vxold+(1-beta2)*vxnew);
                break
            end
        end
        alpha = alpha * gamma;
    end
    if alpha <= 1e-6 && flag == 0; flag = -1; end
    fprintf('nit=%2d, dk=%.4f, alpha=%.4f, rho=%.4f, lkv=%.4f, vk=%.4f, fk=%.4e\n\n',nit,norm(dk),alpha,rho,sum(shift.dkrho),vxold,fxold);
    s = xnew - xold;
    q = gfxnew + gcxnew*lambda - ( gfxold + gcxold*lambda);
    Bk = bfgs(Bk,s,q);
    xold = xnew;
    fxold = fxnew; gfxold = gfxnew;
    cxold = cxnew; gcxold = gcxnew;
    vxold = vxnew;
    nit = nit + 1;
end
% fprintf('$(%.2f,%.2f,%.2f,%.2f)$ & %2d & %.4f & %.4f & %.4f & %.4f\\\\ \n',x0,nit-1,norm(dk),mk(xold,dkfea),sum(abs(funh(xold)))+sum(max(func(xold),0)),fxold);

cutest_terminate();

% end



function B=bfgs(B,d,y)
bd=B*d;dbd=d'*bd;dy=d'*y;
if (dy<0.2*dbd)
    theta=0.8*dbd/(dbd-dy);
    y=theta*y+(1.0-theta)*bd;
end
dybar=d'*y;
B=B+y*y'/dybar-bd*bd'/dbd;
end

function shift = sub1(Bk,cxold,gcxold)
[n,mi] = size(gcxold);
prob = optimproblem;
d = optimvar('d',n,1);
t = optimvar('t',mi,1);
prob.Objective = sum(t)+0.5*d'*Bk*d;
prob.Constraints.c1 = cxold+gcxold'*d<=t;
prob.Constraints.c2 = t>=0;
Options = optimset('Display','none');
[Sol,~,exitflag] = solve(prob,options=Options);
if exitflag<=0
    disp(exitflag)
end
shift.dkinf = sum(Sol.t);
end

function [dk,shift,lambda] = sub2(rho,gfxold,Bk,cxold,gcxold,shift)
[n,mi] = size(gcxold);
prob = optimproblem;
d = optimvar('d',n,1);
t = optimvar('t',mi,1);
prob.Objective = rho*gfxold'*d+0.5*d'*Bk*d+sum(t);
prob.Constraints.c1 = cxold+gcxold'*d<=t;
prob.Constraints.c2 = t>=0;
Options = optimset('Display','none');
[Sol,~,exitflag,~,Lambda] = solve(prob,options=Options);
if exitflag<=0
    disp(exitflag)
end
dk = Sol.d;
shift.dkrho = sum(Sol.t);
lambda = (Lambda.Constraints.c1);
end
