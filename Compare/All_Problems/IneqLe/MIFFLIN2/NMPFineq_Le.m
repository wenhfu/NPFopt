% A penalty-free method with nonmonotone line search for solving inequality 
% constrained optimization with Maratos effect, Oct. 5, 2020.
% min. f(x) subject to c(x)>=0 Test for CUTEr
% clear all; clc;
% The initial data of the problem as follows
prob=cutest_setup();
n=prob.n;mie=prob.m;
pname=prob.name;
xold=prob.x;
% The parameters for the algorithm
nmax=300;fvmax=20000;epsilon=1e-5;Delta=1000;beta=1e-3;
delta=1;sv=2.1;etaf=1e-3;etac=1e-3;etav=1e-3;niu=2.1;gamma=0.6;
beta1=0.9;beta2=0.75;iter=0;H=eye(n,n);FL=0;
% Begining
fid=fopen('ineq_Le.txt','at');
[fxold,gfxold]=cutest_obj(xold);nf=1;ngf=1; % f(x), gradf(x)
[cxold,J]=cutest_cons(xold);cxold=-cxold;J=-J;gcxold=J';nc=1;ngc=1;% gcxold:n by mie
vxold=0; % v(x)=||c_E(x)||_1+||c_I(x)^-||_1, a^_=max(-a,0)
Lam=ones(mie,1);
for i=1:mie
    vxold=vxold+max(0,-cxold(i));
end
vmax=max(1,vxold); % v(x)=||c(x)||_1
flag_Alg=0;
options = optimset('Display','none');
while ((iter<=nmax)&&(nf<=fvmax))
    subc=[zeros(n,1);ones(mie,1)];
    Aieq=[-gcxold' -eye(mie)];bieq=cxold;
    v1=[-Delta*ones(n,1);zeros(mie,1)];
    v2=[Delta*ones(n,1);Inf*ones(mie,1)];
    [dlin,flin,exitflag]=linprog(subc,Aieq,bieq,[],[],v1,v2,options);
    if exitflag<=0;break;end
    dfea=dlin(1:n);tfea=dlin(n+1:n+mie);      
    if ((norm(dfea,Inf)<1e-8)&&(vxold>1.0e-4))
        fprintf(fid,'&s 1--infeasible stationary point ||c-||_1=%12.8e \n',pname,vxold);
        flag_Alg=1;break;
    end
    Rk=zeros(mie,1);Rkcount=0;% Rkcount: t_i>0
    for j=1:mie 
        if tfea(j)>1.0e-4
            Rkcount=Rkcount+1;Rk(Rkcount)=j;
        end
    end
    if Rkcount==0 % Linearized constraints are compatible
        [dopt,fquad,exitflag]=quadprog(H,gfxold,-gcxold',cxold,[],[],[],[],[],options);
        if exitflag<=0;break;end
        if ((vxold<=epsilon)&&(norm(dopt)<=epsilon))  
            fprintf(fid,'%s successfully end ||d||_2=%12.8e \n',pname,norm(dopt));
            flag_Alg=2;break;
        end
    else        
        subg=[gfxold;ones(Rkcount,1)]; 
        Bk=[H zeros(n,Rkcount);zeros(Rkcount,n) zeros(Rkcount,Rkcount)];
        A2=[-gcxold' zeros(mie,Rkcount)];        
        for i=1:Rkcount
            A2(Rk(i),n+i)=-1;
        end
        v1=[-Inf*ones(n,1);zeros(Rkcount,1)];subb2=cxold;
        [dquad,fquad,exitflag]=quadprog(Bk,subg,A2,subb2,[],[],v1,[],[],options);
        if exitflag<=0;break;end
        dopt=dquad(1:n);
    end        
    if ((abs(vxold-flin)<1e-8)&&(vxold>1e-4))
        fprintf(fid,'%s 2--infeasible stationary point ||c-||_1=%12.8e \n',pname,vxold);
        flag_Alg=1;break;
    end
    % Compute the parameter tau.
    mkdopt=0; % v(x)=||[c_I(x)+A*d]_||_1, a_=max(-a,0)
    for i=1:mie
        mkdopt=mkdopt+max(0,-cxold(i)-gcxold(:,i)'*dopt);
    end
    temp=(1-beta)*abs(vxold-flin);
    if temp>=abs(mkdopt-flin)
        dk=dopt;
    else
        tau=temp/abs(mkdopt-flin);
        if ((tau<0)||(tau>1))
            fprintf(fid,'tau=%12.8e in (0,1] \n',tau);break;
        end
        dk=(1-tau)*dfea+tau*dopt;
    end
    % Algorithm does not stop and the search direction dk is obtained.  
    if FL==0 && norm(gcxold)+norm(cxold)<= 1e+10 % iter=0 is regarded as first successful iteration.
        Rxold=xold;Rfxold=fxold;Rgfxold=gfxold;Rcxold=cxold;Rgcxold=gcxold;
    	Rvxold=vxold;Rvmax=vmax;Rdk=dk;
        mkRdk=0; % v(x)=||[c_I(x)+A*d]_||_1, a_=max(-a,0)
        for i=1:mie
            mkRdk=mkRdk+max(0,-cxold(i)-gcxold(:,i)'*dk);
        end
        temp1=-Rgfxold'*Rdk;temp2=delta*(Rvxold)^sv;
    end
    xnew=xold+dk;
    fxnew=cutest_obj(xnew);nf=nf+1;
    [cxnew,J]=cutest_cons(xnew);cxnew=-cxnew;J=-J;nc=nc+1;
    vxnew=0;
    for i=1:mie
        vxnew=vxnew+max(0,-cxnew(i));
    end
    if FL<=MF && norm(cxnew)+norm(J)<= 1e+10
        if temp1>temp2 % The condition (2.5) holds.
            % Check condition (2.6) and condition (2.7)
            if ((Rfxold-fxnew>=etaf*temp1)&&(vxnew<=Rvmax))
                vmax=Rvmax;FL=0; % (2.6)&(2.7) all hold. f-type successful                
            else
                vmax=max(Rvmax,vxnew);FL=FL+1; % Unsuccessful iteration
            end
        elseif Rvxold-vxnew>=etac*(Rvxold-mkRdk) % (2.5) does not holds.
             FL=0; % (2.8) hold��c-type successful
             vmax=max(beta1*Rvmax,vxnew+beta2*(Rvxold-vxnew));
        else
            eqn210=vxnew<=(1-etav)*Rvxold && Rvxold>=norm(Rdk)^niu;
            if ((FL>=1)&&(eqn210)) % FL>=1��(2.10) holds.
                FL=0; % v-type successful
                vmax=max(beta1*Rvmax,vxnew+beta2*(Rvxold-vxnew));
            else
                vmax=max(Rvmax,vxnew);FL=FL+1; % Unsuccessful iteration
            end
        end % end for temp1>temp2         
    else % FL>MF, return the last successful iteration to do line search
        dk=Rdk;xold=Rxold;alpha=gamma;
        while alpha>=1e-8
            xnew=xold+alpha*dk;
            fxnew=cutest_obj(xnew);nf=nf+1;
            cxnew=cutest_cons(xnew);cxnew=-cxnew;nc=nc+1;
            vxnew=0;
            for i=1:mie
                vxnew=vxnew+max(0,-cxnew(i));
            end
            if alpha*temp1>temp2 % (2.5) holds
                if ((Rfxold-fxnew>=etaf*(alpha*temp1))&&(vxnew<=vmax))
                    vmax=Rvmax;FL=0;break;
                else
                    alpha=gamma*alpha;
                end
            else % Next check (2.8)                
                if Rvxold-vxnew>=etac*alpha*(Rvxold-mkRdk)                    
                    vmax=max(beta1*Rvmax,vxnew+beta2*(Rvxold-vxnew));
                    FL=0;break;
                else
                    alpha=gamma*alpha;
                end
            end
        end % for while (alpha)
        if alpha<=1e-8
            fprintf(fid,'%s alpha=%12.8e Line search fails \n',pname,alpha);
            break;%alpha is too small, go to the end and output the results.
        end
    end  % end for FL<=MF  
    % Updatation
    % Unsuccessful iteration,i.e.,FL<>0, H and Rdk,Rxold,etc. are unchanged.
    % Successful iteration,i.e.,FL=0:
    % (1) Update Rdk,Rxold,etc.
    % (2) Update the matrix H. 
    [fxnew,gfxnew]=cutest_obj(xnew);ngf=ngf+1;
    [cxnew,J]=cutest_cons(xnew);cxnew=-cxnew;J=-J;gcxnew=J';ngc=ngc+1;
    if FL==0            
        Rvxold=vxnew;Rvmax=vmax;Lam=pinv(gcxnew)*gfxnew;
        s=xnew-Rxold;y=gfxnew-gcxnew*Lam-(Rgfxold-Rgcxold*Lam);
        H=bfgs(H,s,y); % gfxold=gfxnew;gcxold=gcxnew;
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
    iter=iter+1;  
    Res1=max(norm(gfxold-gcxold*Lam),vxold);    
    if Res1<=epsilon
        fprintf(fid,'%s successfully end ||g-A*lambda||<=e, ||c||<=e\n',pname);
        flag_Alg=2;break;
    end      
end % for while (iter)
% Output the computational results
Lam=pinv(gcxold)*gfxold;Res=norm(gfxold-gcxold*Lam);
fprintf(fid,'%s & %4d & %4d & %4d & %4d & %3d & f=%12.8e ||c||_1=%12.8e Res=%12.8e MF=%1d\n',pname,n,mie,nf,ngf,iter,fxold,vxold,Res,MF);
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