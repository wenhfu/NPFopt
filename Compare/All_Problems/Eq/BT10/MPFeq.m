% A penalty-free method with monotone line search for solving equality 
% constrained optimization with Maratos effect, Oct. 5, 2020.
% min. f(x) subject to c(x)=0  Test for CUTEr
% clear all; clc;
% The initial data of the problem as follows
prob=cutest_setup();
n=prob.n;meq=prob.m;
pname=prob.name;
xold=prob.x;
% The parameters for the algorithm
nmax=300;nfmax=20000;epsilon=1e-5;Delta=1000;beta=1e-3;delta=1;sv=2.1;
gamma=0.6;etaf=1e-3;etac=1e-3;beta1=0.9;beta2=0.75;iter=0;H=eye(n,n);
% Begining
fid=fopen('MDeq.txt','at');
[fxold,gfxold]=cutest_obj(xold);nf=1;ngf=1; % f(x), gradf(x)
[cxold,J]=cutest_cons(xold);gcxold=J';nc=1;ngc=1;% gcxold:n by meq
vxold=norm(cxold,1);vmax=max(1,vxold); % v(x)=||c(x)||_1
flag_Alg = 0;
options = optimset('Display','none');
while ((iter<=nmax)&&(nf<=nfmax))
    subc=[zeros(n,1);ones(meq,1);ones(meq,1)];
    Aeq=[gcxold' -eye(meq) eye(meq)];beq=-cxold;
    v1=[-Delta*ones(n,1);zeros(meq,1);zeros(meq,1)];
    v2=[Delta*ones(n,1);Inf*ones(meq,1);Inf*ones(meq,1)];
    [dlin,flin,exitflag] =linprog(subc,[],[],Aeq,beq,v1,v2,options);
    if exitflag<=0;break;end
    dfea=dlin(1:n);rfea=dlin(n+1:n+meq);sfea=dlin(n+meq+1:n+2*meq); 
    if ((norm(dfea,Inf)<1e-8)&&(vxold>1.0e-4))
        fprintf(fid,'%s Check: ||c||_2=%12.8e ||A*c||=%12.8e \n',pname,norm(cxold),norm(gcxold*cxold));
        fprintf(fid,'%s 1--infeasible stationary point \n',pname);flag_Alg=1;break;
    end
    Rk=zeros(meq,1);Rkcount=0;% Rkcount=m-|E_k|
    for j=1:meq 
        if rfea(j)+sfea(j)>1.0e-4
            Rkcount=Rkcount+1;Rk(Rkcount)=j;
        end
    end
    if Rkcount==0 % Linearized constraints are compatible
        [dopt,fquad,exitflag]=quadprog(H,gfxold,[],[],gcxold',-cxold,[],[],[],options);
        if exitflag<=0;break;end
        if ((vxold<=epsilon)&&(norm(dopt)<=epsilon))  
            fprintf(fid,'%s successfully end ||d||_2=%12.8e \n',pname,norm(dopt));flag_Alg=2;break;
        end
    else        
        subg=[gfxold;ones(2*Rkcount,1)]; 
        Bk=[H zeros(n,2*Rkcount);zeros(2*Rkcount,n) zeros(2*Rkcount,2*Rkcount)];
        A2=[gcxold' zeros(meq,2*Rkcount)];        
        for i=1:Rkcount
            A2(Rk(i),n+i)=-1;A2(Rk(i),n+Rkcount+i)=1;
        end
        v1=[-Inf*ones(n,1);zeros(2*Rkcount,1)];subb2=-cxold;
        [dquad,fquad,exitflag]=quadprog(Bk,subg,[],[],A2,subb2,v1,[],[],options);
        if exitflag<=0;break;end
        dopt=dquad(1:n);
    end
    if ((abs(vxold-flin)<1e-8)&&(vxold>1e-4))
        fprintf(fid,'%s Check: ||c||_2=%12.8e ||A*c||=%12.8e \n',pname,norm(cxold),norm(gcxold*cxold));
        fprintf(fid,'%s 2--infeasible stationary point\n',pname);
        flag_Alg=1;break;
    end
    % Compute the parameter tau.
    mkdopt=norm(cxold+gcxold'*dopt,1);temp=(1-beta)*abs(vxold-flin);
    if temp>=abs(mkdopt-flin)
        dk=dopt;
    else
        tau=temp/abs(mkdopt-flin);
        if ((tau<0)||(tau>1))
            fprintf(fid,'%s tau=%12.8e \n',pname,tau);break;
        end
        dk=(1-tau)*dfea+tau*dopt;
    end    
    % Algorithm does not stop and the search direction dk is obtained.
    alpha=1;mkdk=norm(cxold+gcxold'*dk,1);
    temp1=-gfxold'*dk;temp2=delta*(vxold)^sv;
    while alpha>=1e-8
        xnew=xold+alpha*dk;
        fxnew=cutest_obj(xnew);nf=nf+1;
        cxnew=cutest_cons(xnew);nc=nc+1;
        vxnew=norm(cxnew,1);
        if alpha*temp1>temp2 % (2.5) holds
            if ((fxold-fxnew>=etaf*(alpha*temp1))&&(vxnew<=vmax))
                break;
            else
                alpha=gamma*alpha;
            end
        else % Next check (2.8)
            if vxold-vxnew>=etac*alpha*(vxold-mkdk)
                vmax=max(beta1*vmax,vxnew+beta2*(vxold-vxnew));break;
            else
                alpha=gamma*alpha;
            end
        end
    end % for while (alpha)
    if alpha<=1e-8
        fprintf(fid,'%s alpha=%12.8e Line search fails \n',pname,alpha);
        break; % alpha is too small, go to the end and output the results.
    end
    % Update the matrix H.
    [fxnew,gfxnew]=cutest_obj(xnew);ngf=ngf+1;
    [cxnew,J]=cutest_cons(xnew);gcxnew=J';ngc=ngc+1;
    Lam=pinv(gcxnew)*gfxnew;s=xnew-xold;
    y=gfxnew-gcxnew*Lam-(gfxold-gcxold*Lam);
    H=bfgs(H,s,y);
    % Update iterate point for successful or unsuccessful iteration
    xold=xnew;fxold=fxnew;gfxold=gfxnew;
    cxold=cxnew;gcxold=gcxnew;vxold=vxnew;
    iter=iter+1;
    Res1=max(norm(gfxold-gcxold*Lam),norm(cxold));
    if Res1<=epsilon
        fprintf(fid,'%s successfully end ||g-A*lambda||<=e, ||c||<=e\n',pname);flag_Alg=2;break;
    end
end % for while (iter)
% Output the computational results
Lam=pinv(gcxold)*gfxold;Res=norm(gfxold-gcxold*Lam);
fprintf(fid,'%s & %4d & %4d & %4d & %4d & %3d & f=%12.8e ||c||_1=%12.8e Res=%12.8e MF=0\n',pname,n,meq,nf,ngf,iter,fxold,vxold,Res);
fclose(fid);
cutest_terminate();








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


