%% File->Set Path->Add Folder->selecting the TCAP folder->Open->Save
%% File->Import Data (Find and select your file)->Finish

tcap(data,textdata,'AD');  %%%import input matrix for each type of lung cancer, AD represents the the first class of lung cancer
tcap(data,textdata,'psi');  %%calculate measure matrix
%% double-click AD_psi.mat in Current Directory, psi_star_vector can be presented in Workspace
A=psi_star_vector;  
%% A is a vector. the following command transforms A into a symmetric matrix
m=length(A);
n=(-1+sqrt(1+8*m))/2 ;
AA=zeros(n,n);
for i=1:n;
    for j=i:n;
        index=sum(n:-1:n-i+2)+j-i+1;  
        AA(i,j)=A(index);       
    end
end
AA; 
t=length(AA); 
 B=zeros(t,1);
 C=zeros(1,t+1);
 S=[B,AA];
 S=[S;C];
 t=length(S)
 for i=1:t;
     for j=1:t;
         S(j,i)=S(i,j);
     end
 end
 size(S)  
 for i=1:1000
     S(i,i)=0.25534;                  %%this value can be seen in the Current Directory
 end
size(S)
dlmwrite('ADsimilarity.txt',S,'\t');  %%save results
