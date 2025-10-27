% 
% A penalty-free method with nonmonotone line search for nonlinear optimization.
%
% Coded by Wenhao Fu, Oct. 27, 2025.
%
% Problem setting:
%   minimize        f(x)
%  subject to  c_ineq(x) >= 0,
%                c_eq(x)  = 0.
%
% Input:
% [...] = NPFopt(funf,func,con_type,x0)
% [...] = NPFopt(funf,func,con_type,x0,MF)
% [...] = NPFopt(funf,func,con_type,x0,MF,opts)
%
% funf: A matlab function that calculates the value of f(x) and gradient value of f(x).
% [f,gf] = funf(x)
%
% func: A matlab function that calculates the value of c(x) and gradient value of c(x).
% [c_ineq,c_eq,Jc_ineq,Jc_eq] = func(x)
%
% x0: Initial point, specified as a real vector or real array.
%
% MF: Number of the maximum allowed consecutive unsuccessful iterations, a positive integer;  the default is 0.
%
% opts.epsilon: Termination tolerance; the default is 1e-5.
% opts.nfmax: Maximum number of function evaluation allowed, a positive integer; the default is 1000.
% opts.itermax: Maximum number of iterations allowed, a positive integer; the default is 1000
% opts.display: Level of display. 0: displays no output (defult); 1: displays output at each iteration
%
% Output:
% [x,fval,exitflag,output,lambda] = NPFopt(...)
%
% x: Solution.
%
% fval: Objective function value at the solution.
%
% exitflag =  2: Successfully converge to a feasible solution
% exitflag =  1: Successfully converge to an infeasible stationary point
% exitflag =  0: Reached the Maximum number iteration
% exitflag = -1: Line search failed
% exitflag = -2: LP subproblem failed
% exitflag = -3: QP subproblem failed
% exitflag = -4: Numerical problem
% exitflag = -5: Problem size too large (deminson > 500).
%
% output: Containing detailed information when the algorithm terminates.
%
% lambda: Lagrange multipliers.

function [x,fval,exitflag,output,lambda] = NPFopt(funf,func,x0,MF,opts)
if ~exist('MF','var')
    MF = 0;
end
if exist('opts','var')
    if isfield(opts,'epsilon'); epsilon = opts.epsilon; else; epsilon = 1e-5; end
    if isfield(opts,'nfmax'); nfmax = opts.nfmax; else; nfmax = 1000; end
    if isfield(opts,'itermax'); itermax = opts.itermax; else; itermax = 1000; end
    if isfield(opts,'display'); alg_display = opts.display; else; alg_display = 1; end
else
    epsilon = 1e-5; itermax = 1000; nfmax = 1000; alg_display = 1;
end
n = length(x0); 
% The parameters for the algorithm
Delta = 1e3; beta = 1e-3;
delta = 1; sv = 2.1; etaf = 1e-3; etac = 1e-3; etav = 1e-3; niu = 2.1; gamma = 0.6;
beta1 = 0.1; beta2 = 0.75; iter = 0; H = eye(n,n); FL = 0;
% Begining
xold = x0;
[fxold, gfxold] = funf(xold); nf = 1; ngf = 1; % f(x), gradf(x)
[cxold_ineq, cxold_eq, Jc_ineq, Jc_eq] = func(xold); gcxold_ineq = Jc_ineq'; gcxold_eq = Jc_eq'; nc = 1; ngc = 1;% gcxold:n by meq
m_eq = length(cxold_eq); m_ineq = length(cxold_ineq);
m = m_eq + m_ineq; Ntry = 0;
if m_eq == 0; gcxold_eq=zeros(n,0); end; if m_ineq == 0; gcxold_ineq = zeros(n,0); end
flag_Alg = 0;
if max(n,(m_eq + m_ineq))>=500
    if alg_display == 1
        fprintf('Large scaled problem, pass.');
    end
    x = x0; fval = fxold; flag_Alg = -5;
    output.con = 0; output.iter = 0; output.nf = 0; output.nc = 0; output.ngf = 0; output.ngc = 0; output.n = n; output.m = m_eq + m_ineq; output.Res = inf;
    lambda = zeros(m_eq + m_ineq, 1);
    return
end
vxold = norm(cxold_eq, 1) + norm(max(- cxold_ineq,0),1);
vmax = max(1, vxold); 
Mu = ones(m_eq, 1); Lam = ones(m_ineq,1);
xnew = xold; fxnew = fxold; vxnew = vxold;
options_qp = optimoptions('quadprog','Display','none');
if n>2
    options_lp = optimoptions('linprog','Display','none');
else
    options_lp = optimoptions('linprog','Display','none','Algorithm','interior-point-legacy');
end
while ((iter <= itermax) && (nf <= nfmax)) && (flag_Alg == 0)
    subc = [ zeros(n,1); ones(2*m_eq + m_ineq,1) ];
    A = [ - gcxold_ineq', zeros(m_ineq, m_eq), zeros(m_ineq, m_eq), - eye(m_ineq) ]; b = cxold_ineq;
    Aeq = - [ gcxold_eq', - eye(m_eq), eye(m_eq), zeros(m_eq, m_ineq) ]; beq = cxold_eq;
    v1 = [ - Delta*ones(n,1); zeros(2*m_eq + m_ineq,1) ];
    v2 = [ Delta*ones(n,1); Inf*ones(2*m_eq + m_ineq,1) ];
    [ dlin, flin, exitflag ] = linprog(subc,A,b,Aeq,beq,v1,v2,options_lp);
    if exitflag <= 0
        options_lp_tmp = optimoptions('linprog','Display','none','Algorithm','interior-point-legacy');
        [ dlin, flin, exitflag, output_lp ] = linprog(subc,A,b,Aeq,beq,v1,v2,options_lp_tmp);
        if exitflag <= 0
            if alg_display == 1
                fprintf('         LP failed            '); disp(output_lp);
            end
            flag_Alg = - 2; break
        end
    end
    dfea = dlin(1:n); rfea = dlin(n + 1 : n + m_eq); sfea = dlin(n + m_eq + 1 : n + 2*m_eq); tfea = dlin(n + 2*m_eq + 1 : n + 2*m_eq + m_ineq);
    if abs(norm(dfea,Inf) - Delta) < 1e-8
        Delta = min(1000*Delta,1e6);
    else
        Delta = max(0.1*Delta,1e2);
    end
    if ((norm(dfea,Inf) < 1e-8) && (vxold > 1.0e-5))
        if alg_display == 1
            if Ntry ==0
                rng(0); xold = x0 + randn(n,1)/1e3; Ntry = 1; [fxold, gfxold] = funf(xold);  H = eye(n);
                [cxold_ineq, cxold_eq, Jc_ineq, Jc_eq] = func(xold); gcxold_ineq = Jc_ineq'; gcxold_eq = Jc_eq';
                continue
            end
            fprintf('1 infeasible stationary point ');
        end
        flag_Alg = 1; break;
    end
    if ((abs(vxold - flin) < 1e-8) && (vxold > 1e-5))
        if alg_display == 1
            if Ntry ==0
                rng(0); xold = x0 + randn(n,1)/1e3; Ntry = 1; [fxold, gfxold] = funf(xold);  H = eye(n);
                [cxold_ineq, cxold_eq, Jc_ineq, Jc_eq] = func(xold); gcxold_ineq = Jc_ineq'; gcxold_eq = Jc_eq';
                continue
            end
            fprintf('2 infeasible stationary point ');
        end
        flag_Alg = 1; break;
    end
    Rk_eq = zeros(m_eq,1); Rkcount_eq = 0; % Rkcount=m-|E_k|
    for j = 1 : m_eq
        if rfea(j) + sfea(j) > 1e-8
            Rkcount_eq = Rkcount_eq + 1; Rk_eq(Rkcount_eq) = j;
        end
    end
    Rk_ineq = zeros(m_ineq,1); Rkcount_ineq = 0; % Rkcount=m-|E_k|
    for j = 1 : m_ineq
        if tfea(j) > 1e-8
            Rkcount_ineq = Rkcount_ineq + 1; Rk_ineq(Rkcount_ineq) = j;
        end
    end
    if Rkcount_eq + Rkcount_ineq == 0 % Linearized constraints are compatible
        [dopt,~,exitflag,output_qp1] = quadprog(H, gfxold, - gcxold_ineq', cxold_ineq, - gcxold_eq', cxold_eq, [ ], [ ], [ ], options_qp);
        if exitflag <= 0 && FL == inf
            if alg_display == 1
                if Ntry == 0
                    rng(0); xold = x0 + randn(n,1); Ntry = 1; [fxold, gfxold] = funf(xold); H = eye(n);
                    [cxold_ineq, cxold_eq, Jc_ineq, Jc_eq] = func(xold); gcxold_ineq = Jc_ineq'; gcxold_eq = Jc_eq';
                    continue
                end
                fprintf('         QP1 failed           '); disp(output_qp1);
            end
            flag_Alg = - 3; break
        end
        if exitflag <= 0 && FL > 0
            FL = inf; xold = Rxold; fxold = Rfxold; gfxold = Rgfxold;
            cxold_eq = Rcxold_eq; gcxold_eq = Rgcxold_eq; cxold_ineq = Rcxold_ineq; gcxold_ineq = Rgcxold_ineq; vxold = Rvxold;
            continue
        end
        if ((vxold <= epsilon) && (norm(dopt,Inf) <= epsilon))
            if alg_display == 1
                fprintf('successfully end              ');
            end
            flag_Alg = 2; break;
        end
    else
        Rkcount = 2*Rkcount_eq + Rkcount_ineq;
        subg = [ gfxold; ones(Rkcount,1) ];
        Bk = [ H, zeros(n,Rkcount); zeros(Rkcount,n), zeros(Rkcount,Rkcount)];
        A2_eq = - [ gcxold_eq', zeros(m_eq,Rkcount)]; b2_eq = cxold_eq;
        for i = 1 : Rkcount_eq
            A2_eq(Rk_eq(i), n + i ) = - 1; A2_eq(Rk_eq(i), n + Rkcount_eq + i ) = 1;
        end
        A2_ineq = [ - gcxold_ineq', zeros(m_ineq,Rkcount)]; b2_ineq = cxold_ineq;
        for i = 1 : Rkcount_ineq
            A2_ineq(Rk_ineq(i),n + i) = - 1;
        end
        v1 = [ - Inf*ones(n,1); zeros(Rkcount,1) ];
        [ dquad, ~, exitflag, output_qp2 ] = quadprog(Bk, subg, A2_ineq, b2_ineq, A2_eq, b2_eq, v1, [ ], [ ], options_qp);
        if exitflag <= 0 && FL == inf
            if alg_display == 1
                fprintf('         QP2 failed           '); disp(output_qp2);
            end
            flag_Alg = - 3; break
        end
        if exitflag <= 0 && FL > 0
            FL = inf; xold = Rxold; fxold = Rfxold; gfxold = Rgfxold;
            cxold_eq = Rcxold_eq; gcxold_eq = Rgcxold_eq; cxold_ineq = Rcxold_ineq; gcxold_ineq = Rgcxold_ineq; vxold = Rvxold;
            continue
        end
        dopt = dquad(1 : n);
    end
    % Compute the parameter tau.
    mkdopt = norm(cxold_eq + gcxold_eq'*dopt,1) + norm(max( - (cxold_ineq + gcxold_ineq'*dopt ),0 ),1 );
    temp = ( 1 - beta )*abs( vxold - flin );
    if temp - abs( mkdopt - flin ) >= - 1e-8
        dk = dopt;
    else
        tau = temp / abs( mkdopt - flin );
        if ( tau < 0 ) || ( tau > 1 )
            if alg_display == 1
                fprintf('tau=%12.8e \n',tau);
            end
            flag_Alg = - 4;break;
        end
        dk = (1 - tau)*dfea + tau*dopt;
    end  
    % dk
    % Algorithm does not stop and the search direction dk is obtained.  
    if FL == 0 || MF == 0 && norm([ gcxold_eq, gcxold_ineq ]) + vxold <= 1e+10 % iter=0 is regarded as first successful iteration.
        Rxold = xold; Rfxold = fxold; Rgfxold = gfxold; Rcxold_eq = cxold_eq; Rgcxold_eq = gcxold_eq; Rcxold_ineq = cxold_ineq; Rgcxold_ineq = gcxold_ineq;
        Rvxold = vxold; Rvmax = vmax; Rdk = dk;
        mkRdk = norm(cxold_eq + gcxold_eq'*dk,1) + norm(max( - (cxold_ineq + gcxold_ineq'*dk),0 ),1 );
        temp1 = - Rgfxold'*Rdk; temp2 = delta*(Rvxold)^sv;
    end
    xnew = xold + dk;
    [ fxnew, ~ ]=funf(xnew);
    [ cxnew_ineq, cxnew_eq, ~, ~ ] = func(xnew);
    vxnew = norm(cxnew_eq,1) + norm(max( - cxnew_ineq,0 ),1 );
    if FL <= MF && MF > 0 && vxnew + norm([ gcxold_eq, gcxold_ineq ]) <= 1e+10
        nf = nf + 1; nc = nc + 1;
        if temp1 > temp2 % The condition (2.5) holds.
            % Check condition (2.6) and condition (2.7)
            if (Rfxold - fxnew >= etaf*temp1) && (vxnew <= Rvmax)
                vmax = Rvmax; FL = 0; % (2.6)&(2.7) all hold. f-type successful                
            else
                vmax = max(Rvmax,vxnew); FL = FL + 1; % Unsuccessful iteration
            end
        elseif Rvxold - vxnew >= etac*(Rvxold - mkRdk) % (2.5) does not holds.
             FL = 0; % (2.8) hold, c-type successful
             vmax = max(beta1*Rvmax,vxnew + beta2*(Rvxold - vxnew));
        else
            eqn210 = vxnew <= (1 - etav)*Rvxold && Rvxold >= norm(Rdk)^niu;
            if (FL >= 1 ) && (eqn210) % FL>=1, (2.10) holds.
                FL = 0; % v-type successful
                vmax = max(beta1*Rvmax,vxnew + beta2*(Rvxold - vxnew));
            else
                vmax = max(Rvmax,vxnew); FL = FL + 1; % Unsuccessful iteration
            end
        end % end for temp1>temp2         
    else % FL>MF, return the last successful iteration to do line search
        if MF>0
            dk = Rdk; xold = Rxold; alpha = gamma;
        else
            alpha = 1;
        end
        while alpha >= 1e-8
            xnew = xold + alpha*dk;
            [ fxnew, ~ ] = funf(xnew); nf  = nf + 1;
            [ cxnew_ineq, cxnew_eq, ~, ~ ] = func(xnew); nc = nc + 1;
            if nf > nfmax; break; end
            vxnew = norm(cxnew_eq,1) + norm(max( - cxnew_ineq,0 ),1 );
            if alpha*temp1 > temp2 % (2.5) holds
                if ((Rfxold - fxnew >= etaf*(alpha * temp1) - 1e-8) && (vxnew <= vmax))
                    vmax = Rvmax; FL = 0;break;
                else
                    alpha = gamma*alpha;
                end
            else % Next check (2.8)                
                if Rvxold - vxnew >= etac*alpha*(Rvxold - mkRdk) - 1e-8
                    vmax = max(beta1*Rvmax, vxnew + beta2*(Rvxold - vxnew));
                    FL = 0; break;
                else
                    alpha = gamma*alpha;
                end
            end
            % if abs(fxold-fxnew)/max(abs(fxnew),1) < 0.1*epsilon && vxnew < 0.01*epsilon
            %     fprintf('successfully end              ');
            %     flag_Alg = 3; break
            % end
        end % for while (alpha)
        if alpha <= 1e-8
            if alg_display == 1
                fprintf('      Line search fails       '); 
            end
            flag_Alg = - 1; break; %alpha is too small, go to the end and output the results.
        end
    end  % end for FL<=MF  
    % Updatation
    % Unsuccessful iteration,i.e.,FL<>0, H and Rdk,Rxold,etc. are unchanged.
    % Successful iteration,i.e.,FL=0:
    % (1) Update Rdk,Rxold,etc.
    % (2) Update the matrix H. 
    [ fxnew, gfxnew ] = funf(xnew); ngf = ngf + 1;
    [ cxnew_ineq, cxnew_eq, J_ineq, J_eq ] = func(xnew); gcxnew_eq = J_eq'; gcxnew_ineq = J_ineq'; ngc = ngc + 1;
    if m_eq == 0; gcxnew_eq = zeros(n,0); end; if m_ineq == 0; gcxnew_ineq = zeros(n,0); end
    if FL == 0
        Rvxold = vxnew; Rvmax = vmax; s = xnew - Rxold;
        Mu = pinv(gcxnew_eq)*gfxnew; Lam = max(pinv(gcxnew_ineq)*gfxnew,0);
        y = gfxnew - gcxnew_eq*Mu - gcxnew_ineq*Lam - (gfxold - gcxold_eq*Mu - gcxold_ineq*Lam);
        H = bfgs(H,s,y);
        if cond(H) > 1e8
            H = eye(n);
        end
        xold = xnew; fxold = fxnew; gfxold = gfxnew;
        cxold_eq = cxnew_eq; gcxold_eq = gcxnew_eq; cxold_ineq = cxnew_ineq; gcxold_ineq = gcxnew_ineq; vxold = vxnew;
        Rxold = xnew; Rfxold = fxnew; Rgfxold = gfxnew;
        Rcxold_eq = cxnew_eq; Rgcxold_eq = gcxnew_eq; Rcxold_ineq = cxnew_ineq; Rgcxold_ineq = gcxnew_ineq;
    elseif FL <= MF
        xold = xnew; fxold = fxnew; cxold_eq = cxnew_eq; cxold_ineq = cxnew_ineq; vxold = vxnew;
        gfxold = gfxnew; gcxold_eq = gcxnew_eq; gcxold_ineq = gcxnew_ineq;
    else
        xold = Rxold; fxold = Rfxold; gfxold = Rgfxold;
        cxold_eq = Rcxold_eq; cxold_ineq = Rcxold_ineq; gcxold_eq = Rgcxold_eq; gcxold_ineq = Rgcxold_ineq; vxold = Rvxold;
    end
    iter = iter + 1;
    Res1=max(norm(gfxold - gcxold_eq*Mu - gcxold_ineq*Lam), vxold);
    if Res1 <= epsilon
        if alg_display == 1
            fprintf('successfully end              ');
        end
        flag_Alg = 2; break;
    end
end % for while (iter)
% Output the computational results
if alg_display == 1
    if iter > itermax || nf > nfmax
        fprintf('     Reach Max iteration      ');
    end
    fprintf('n =%4d, meq =%4d, mineq =%4d, nf =%4d, ng =%4d, iter =%3d, f =%12.4e, v =%12.4e ',n,m_eq,m_ineq,nf,ngf,iter,fxnew,vxnew);
end
fprintf('\n');
x = xnew; fval = fxnew; exitflag = flag_Alg;
output.con = vxnew;
output.iter = iter;
output.nf = nf;
output.nc = nc;
output.ngf = ngf;
output.ngc = ngc;
output.n = n;
output.meq = m_eq;
output.m_ineq = m_ineq;
Mu = pinv(gcxold_eq)*gfxold;
Lam = max(pinv(gcxnew_ineq)*gfxnew,0);
Res = norm(gfxold - gcxold_eq*Mu - gcxold_ineq*Lam);
output.Res = Res;
lambda = [ Mu; Lam ];
end

function B=bfgs(B,s,y)
   bs = B*s; sbs = s'*bs; sy = s'*y;
   if sy < 0.2*sbs
      theta = 0.8*sbs/(sbs - sy);
   else
      theta = 1;
   end
   rbar = theta*y + (1 - theta)*bs;
   srbar = s'*rbar;
   if (abs(srbar) <= 1e-12) || (abs(sbs) <= 1e-12)
       return;
   end
   B = B + rbar*rbar'/srbar - bs*bs'/sbs;
end

