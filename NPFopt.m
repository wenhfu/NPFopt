% 
% A penalty-free method with nonmonotone line search for nonlinear optimization.
%
% Coded by Wenhao Fu, Feb. 10, 2025.
%
% Problem setting:
%       min       f(x)=-x1+rho*(x1^2+x2^2-1)
%  subject to  c(x)=0, c(x)>0, or c(x)<0
%
% Input:
% [...] = NPFopt(funf,func,con_type,x0)
% [...] = NPFopt(funf,func,con_type,x0,MF)
% [...] = NPFopt(funf,func,con_type,x0,MF,opts)
%
% funf: A matlab function that calculates the value of f(x) and gradient value of f(x).
%
% func: A matlab function that calculates the value of c(x) and gradient value of c(x).
%
% con_type =  0: problem with c(x)=0.
% con_type =  1: problem with c(x)>0.
% con_type = -1: problem with c(x)<0.
%
% x0: Initial point, specified as a real vector or real array.
%
% MF: Number of the maximum allowed consecutive unsuccessful iterations, a positive integer;  the default is 0.
%
% opts.epsilon: Termination tolerance; the default is 1e-5.
% opts.nfmax: Maximum number of function evaluation allowed, a positive integer; the default is 1000.
% opts.itermax: Maximum number of iterations allowed, a positive integer; the default is 300
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

function [x,fval,exitflag,output,lambda] = NPFopt(funf,func,con_type,x0,MF,opts)
if ~exist('MF','var')
    MF = 0;
end
if exist('opts','var')
    if isfield(opts,'epsilon'); epsilon = opts.epsilon; else; epsilon=1e-5; end
    if isfield(opts,'nfmax'); nfmax = opts.nfmax; else; nfmax=1000; end
    if isfield(opts,'itermax'); itermax = opts.itermax; else; itermax=300; end
    if isfield(opts,'display'); alg_display = opts.display; else; alg_display=1; end
else
    epsilon=1e-5; itermax=300; nfmax=1000; alg_display=0;
end
n = length(x0); 
% The parameters for the algorithm
Delta=1e3;beta=1e-3;
delta=1;sv=2.1;etaf=1e-3;etac=1e-3;etav=1e-3;niu=2.1;gamma=0.6;
beta1=0.1;beta2=0.75;iter=0;H=eye(n,n);FL=0; alpha=1; % beta1=0.9;
% Begining
xold=x0;
[fxold,gfxold]=funf(xold);nf=1;ngf=1; % f(x), gradf(x)
[cxold,J]=func(xold);gcxold=J';nc=1;ngc=1;% gcxold:n by meq
if con_type==-1
    cxold = -cxold; gcxold = -gcxold; J = -J;
end
meq = length(cxold);
if max(n,meq)>=500
    if alg_display==1
        fprintf('Large scaled problem, pass.');
    end
    x = x0; fval = fxold; exitflag = -5;
    output.con = 0; output.iter = 0; output.nf = 0; output.nc = 0; output.ngf = 0; output.ngc = 0; output.n=n; output.m=meq; output.Res=inf;
    lambda = zeros(meq,1);
    return
end
if con_type==0
    vxold=norm(cxold,1);
else
    vxold=norm(max(-cxold,0),1);
end
vmax=max(1,vxold); 
Lam=ones(meq,1);
xnew=xold; fxnew=fxold; vxnew=vxold;
flag_Alg = 0;
options_qp = optimoptions('quadprog','Display','none');
if n>2
    options_lp = optimoptions('linprog','Display','none');
else
    options_lp = optimoptions('linprog','Display','none','Algorithm','interior-point-legacy');
end
while ((iter<=itermax)&&(nf<=nfmax))
    if con_type==0
        subc=[zeros(n,1);ones(meq,1);ones(meq,1)];
        A=[gcxold',-eye(meq),eye(meq)];b=-cxold;
        v1=[-Delta*ones(n,1);zeros(meq,1);zeros(meq,1)];
        v2=[Delta*ones(n,1);Inf*ones(meq,1);Inf*ones(meq,1)];
        [dlin,flin,exitflag] =linprog(subc,[],[],A,b,v1,v2,options_lp);
        if exitflag<=0
            options_lp = optimoptions('linprog','Display','none','Algorithm','interior-point-legacy');
            [dlin,flin,exitflag,output_lp] =linprog(subc,[],[],A,b,v1,v2,options_lp);
            if exitflag<=0
                if alg_display==1
                    fprintf('         LP failed            ');disp(output_lp);
                end
                flag_Alg=-2;break;end
        end
        dfea=dlin(1:n);rfea=dlin(n+1:n+meq);sfea=dlin(n+meq+1:n+2*meq);
    else
        subc=[zeros(n,1);ones(meq,1)];
        A=[-gcxold',-eye(meq)];b=cxold;
        v1=[-Delta*ones(n,1);zeros(meq,1)];
        v2=[Delta*ones(n,1);Inf*ones(meq,1)];
        [dlin,flin,exitflag] =linprog(subc,A,b,[],[],v1,v2,options_lp);
        if exitflag<=0
            options_lp = optimoptions('linprog','Display','none','Algorithm','interior-point-legacy');
            [dlin,flin,exitflag,output_lp] =linprog(subc,A,b,[],[],v1,v2,options_lp);
            if exitflag<=0
                if alg_display==1
                    fprintf('         LP failed            ');disp(output_lp);
                end
                flag_Alg=-2;break;end
        end
        dfea=dlin(1:n);tfea=dlin(n+1:n+meq);
    end
    if abs(norm(dfea,Inf)-Delta)<1e-8
        Delta = min(1000*Delta,1e6);
    else
        Delta = max(0.1*Delta,1e2);
    end
    if ((norm(dfea,Inf)<1e-8)&&(vxold>1.0e-5))
        if alg_display==1
            fprintf('1 infeasible stationary point ');
        end
        flag_Alg=1;break;
    end
    if ((abs(vxold-flin)<1e-8)&&(vxold>1e-5))
        if alg_display==1
            fprintf('2 infeasible stationary point ');
        end
        flag_Alg=1;break;
    end
    Rk=zeros(meq,1);Rkcount=0;% Rkcount=m-|E_k|
    for j=1:meq
        if con_type==0
            if rfea(j)+sfea(j)>1.0e-8
                Rkcount=Rkcount+1;Rk(Rkcount)=j;
            end
        else
            if tfea(j)>1.0e-8
                Rkcount=Rkcount+1;Rk(Rkcount)=j;
            end
        end
    end
    if Rkcount==0 % Linearized constraints are compatible
        if con_type == 0
            [dopt,fquad,exitflag,output_qp1]=quadprog(H,gfxold,[],[],gcxold',-cxold,[],[],[],options_qp);
        else
            [dopt,fquad,exitflag,output_qp1]=quadprog(H,gfxold,-gcxold',cxold,[],[],[],[],[],options_qp);
        end
        if exitflag<=0 && FL==inf
            if alg_display==1
                fprintf('         QP1 failed           ');disp(output_qp1);
            end
            flag_Alg=-3;break;end
        if exitflag<=0 && FL>0
            FL=inf;xold=Rxold;fxold=Rfxold;gfxold=Rgfxold;
            cxold=Rcxold;gcxold=Rgcxold;vxold=Rvxold;
            continue
        end
        if ((vxold<=epsilon)&&(norm(dopt,Inf)<=epsilon))
            if alg_display==1
                fprintf('successfully end              ');
            end
            flag_Alg=2;break;
        end
    else
        if con_type == 0
            subg=[gfxold;ones(2*Rkcount,1)];
            Bk=[H ,zeros(n,2*Rkcount);zeros(2*Rkcount,n),zeros(2*Rkcount,2*Rkcount)];
            A2=[gcxold' zeros(meq,2*Rkcount)];
            for i=1:Rkcount
                A2(Rk(i),n+i)=-1;A2(Rk(i),n+Rkcount+i)=1;
            end
            v1=[-Inf*ones(n,1);zeros(2*Rkcount,1)];subb2=-cxold;
            [dquad,fquad,exitflag,output_qp2]=quadprog(Bk,subg,[],[],A2,subb2,v1,[],[],options_qp);
        else
            subg=[gfxold;ones(Rkcount,1)];
            Bk=[H,zeros(n,Rkcount);zeros(Rkcount,n),zeros(Rkcount,Rkcount)];
            A2=[-gcxold',zeros(meq,Rkcount)];
            for i=1:Rkcount
                A2(Rk(i),n+i)=-1;
            end
            v1=[-Inf*ones(n,1);zeros(Rkcount,1)];subb2=cxold;
            [dquad,fquad,exitflag,output_qp2]=quadprog(Bk,subg,A2,subb2,[],[],v1,[],[],options_qp);
        end
        if exitflag<=0 && FL==inf
            if alg_display==1
                fprintf('         QP2 failed           ');disp(output_qp2);
            end
            flag_Alg=-3;break;end
        if exitflag<=0 && FL>0
            FL=inf;xold=Rxold;fxold=Rfxold;gfxold=Rgfxold;
            cxold=Rcxold;gcxold=Rgcxold;vxold=Rvxold;
            continue
        end
        dopt=dquad(1:n);
    end
    % Compute the parameter tau.
    if con_type == 0
        mkdopt=norm(cxold+gcxold'*dopt,1);
    else
        mkdopt=norm(max(-(cxold+gcxold'*dopt),0),1);
    end
    temp=(1-beta)*abs(vxold-flin);
    if temp - abs(mkdopt-flin) >= -1e-8
        dk=dopt;
    else
        tau=temp/abs(mkdopt-flin);
        if ((tau<0)||(tau>1))
            if alg_display==1
                fprintf('tau=%12.8e \n',tau);
            end
            flag_Alg=-4;break;
        end
        dk=(1-tau)*dfea+tau*dopt;
    end  
    % dk
    % Algorithm does not stop and the search direction dk is obtained.  
    if FL==0 || MF==0 && norm(gcxold)+vxold<= 1e+10 % iter=0 is regarded as first successful iteration.
        Rxold=xold;Rfxold=fxold;Rgfxold=gfxold;Rcxold=cxold;Rgcxold=gcxold;
        Rvxold=vxold;Rvmax=vmax;Rdk=dk;
        if con_type == 0
            mkRdk=norm(cxold+gcxold'*dk,1);
        else
            mkRdk=norm(max(-(cxold+gcxold'*dk),0),1);
        end
        temp1=-Rgfxold'*Rdk;temp2=delta*(Rvxold)^sv;
    end
    xnew=xold+dk;
    [fxnew,~]=funf(xnew);
    [cxnew,~]=func(xnew);
    if con_type==-1
        cxnew = -cxnew;
    end
    if con_type==0
        vxnew=norm(cxnew,1);
    else
        vxnew=norm(max(-cxnew,0),1);
    end
    if FL<=MF && MF>0 && vxnew+norm(gcxold)<= 1e+10
        nf=nf+1; nc=nc+1;
        if temp1>temp2 % The condition (2.5) holds.
            % Check condition (2.6) and condition (2.7)
            if ((Rfxold-fxnew>=etaf*temp1)&&(vxnew<=Rvmax))
                vmax=Rvmax;FL=0; % (2.6)&(2.7) all hold. f-type successful                
            else
                vmax=max(Rvmax,vxnew);FL=FL+1; % Unsuccessful iteration
            end
        elseif Rvxold-vxnew>=etac*(Rvxold-mkRdk) % (2.5) does not holds.
             FL=0; % (2.8) hold, c-type successful
             vmax=max(beta1*Rvmax,vxnew+beta2*(Rvxold-vxnew));
        else
            eqn210=vxnew<=(1-etav)*Rvxold && Rvxold>=norm(Rdk)^niu;
            if ((FL>=1)&&(eqn210)) % FL>=1, (2.10) holds.
                FL=0; % v-type successful
                vmax=max(beta1*Rvmax,vxnew+beta2*(Rvxold-vxnew));
            else
                vmax=max(Rvmax,vxnew);FL=FL+1; % Unsuccessful iteration
            end
        end % end for temp1>temp2         
    else % FL>MF, return the last successful iteration to do line search
        if MF>0
            dk=Rdk;xold=Rxold;alpha=gamma;
        else
            alpha=1;
        end
        while alpha>=1e-8
            xnew=xold+alpha*dk;
            [fxnew,~]=funf(xnew);nf=nf+1;
            [cxnew,~]=func(xnew);nc=nc+1;
            if nf>nfmax; break; end
            if con_type==-1
                cxnew = -cxnew;
            end
            if con_type==0
                vxnew=norm(cxnew,1);
            else
                vxnew=norm(max(-cxnew,0),1);
            end
            if alpha*temp1>temp2 % (2.5) holds
                if ((Rfxold-fxnew>=etaf*(alpha*temp1) - 1e-8)&&(vxnew<=vmax))
                    vmax=Rvmax;FL=0;break;
                else
                    alpha=gamma*alpha;
                end
            else % Next check (2.8)                
                if Rvxold-vxnew>=etac*alpha*(Rvxold-mkRdk) - 1e-8
                    vmax=max(beta1*Rvmax,vxnew+beta2*(Rvxold-vxnew));
                    FL=0;break;
                else
                    alpha=gamma*alpha;
                end
            end
        end % for while (alpha)
        if alpha<=1e-8
            if alg_display==1
                fprintf('      Line search fails       '); 
            end
            flag_Alg=-1;break;%alpha is too small, go to the end and output the results.
        end
    end  % end for FL<=MF  
    % Updatation
    % Unsuccessful iteration,i.e.,FL<>0, H and Rdk,Rxold,etc. are unchanged.
    % Successful iteration,i.e.,FL=0:
    % (1) Update Rdk,Rxold,etc.
    % (2) Update the matrix H. 
    [fxnew,gfxnew]=funf(xnew);ngf=ngf+1;
    [cxnew,J]=func(xnew);gcxnew=J';ngc=ngc+1;
    if con_type==-1
        cxnew = -cxnew; gcxnew=-gcxnew;
    end
    if FL==0
        Rvxold=vxnew;Rvmax=vmax;s=xnew-Rxold;
        % if con_type==0
        %     Lam = lambda_qp.eqlin(1:meq);
        % else
        %     Lam = lambda_qp.ineqlin(1:meq);
        % end
        Lam=pinv(gcxnew)*gfxnew;
        y=gfxnew-gcxnew*Lam-(gfxold-gcxold*Lam);
        H=bfgs(H,s,y);
        if cond(H)>1e8
            H=eye(n);
        end
        xold=xnew;fxold=fxnew;gfxold=gfxnew;
        cxold=cxnew;gcxold=gcxnew;vxold=vxnew;
        Rxold=xnew;Rfxold=fxnew;Rgfxold=gfxnew;
        Rcxold=cxnew;Rgcxold=gcxnew;
    elseif FL<=MF
        xold=xnew;fxold=fxnew;cxold=cxnew;vxold=vxnew;
        gfxold=gfxnew;gcxold=gcxnew;
    else
        xold=Rxold;fxold=Rfxold;gfxold=Rgfxold;
        cxold=Rcxold;gcxold=Rgcxold;vxold=Rvxold;
    end
    % iter,fxold,norm(dfea),norm(dk),vxold
    % if exist('alpha');alpha,end
    iter=iter+1;
    Res1=max(norm(gfxold-gcxold*Lam),vxold);
    if Res1<=epsilon
        if alg_display==1
            fprintf('successfully end              ');
        end
        flag_Alg=2;break;
    end
end % for while (iter)
% Output the computational results
% Lam=pinv(gcxold)*gfxold;
% Lam=gcxold\gfxold;
Res=norm(gfxold-gcxold*Lam);
if alg_display==1
    if iter>itermax || nf>nfmax
        fprintf('     Reach Max iteration      ');
    end
    fprintf('n =%4d, m =%4d, nf =%4d, ng =%4d, iter =%3d, f =%12.4e, v =%12.4e ',n,meq,nf,ngf,iter,fxnew,vxnew);
end
x = xnew; fval = fxnew; exitflag = flag_Alg;
output.con = vxnew;
output.iter = iter;
output.nf = nf;
output.nc = nc;
output.ngf = ngf;
output.ngc = ngc;
output.n=n;
output.m=meq;
output.Res=Res;
lambda = Lam;
end

function B=bfgs(B,s,y)
   bs=B*s;sbs=s'*bs; sy=s'*y;
   if (sy<0.2*sbs )
      theta=0.8*sbs/(sbs-sy);
   else
      theta=1.0;
   end
   rbar=theta*y+(1.0-theta)*bs;
   srbar=s'*rbar;
   if (abs(srbar)<=1.0e-12)||(abs(sbs)<=1.0e-12)
       return;
   end
   B=B+rbar*rbar'/srbar-bs*bs'/sbs;
end









