
%%  Constraint functions
function [cineq,ceq,gcineq,gceq] = nonlcon(x,prob)
% clear;clc
% try prob=cutest_setup();catch;cutest_terminate();prob=cutest_setup();end
% x = prob.x;
n = prob.n;
m = prob.m;
[c,gc] = cutest_cons(x); 
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
    cineq(mineq,1) = - x(i) + bl(i); gcineq(mineq,i) = -1; 
    mineq = mineq + 1;
    cineq(mineq,1) = x(i) - bu(i); gcineq(mineq,i) = 1;
end
cineq = - cineq; gcineq = - gcineq;
end