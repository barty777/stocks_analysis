%Function calculates weights for given lenght of an array and an increment
%E.g. n=5, d=20 means it will make an matrix in which every
%rows has a lenght of 5 and all elements in that row sum up to 1.
%d=20 means that increment between elements in array will be 1/20=0.05

n = 9;
d = 20;
c = nchoosek(1:d+n-1,n-1);

m = size(c,1);
t = ones(m,d+n-1);
t(repmat((1:m).',1,n-1)+(c-1)*m) = 0;

 u = [zeros(1,m);t.';zeros(1,m)];
 v = cumsum(u,1);
 x = diff(reshape(v(u==0),n+1,m),1).'/d;