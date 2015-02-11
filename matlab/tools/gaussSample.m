function ind = gaussSample(myPlot,N,Nmax,sigma,h0,h,TF)
%% gaussSample(N,Nmax,sigma,h0,h,TF) returns N indexes between 1 and
% Nmax concentrated about h0 with intensity sigma. The end points are
% always included.
%
% INPUTS:
%    N     = number of indexes to return
%    Nmax  = largest index value
%    sigma = squeezing factor
%            (high concentration) 0 < sigma < 1 (uniform)
%    h0    = position to squeeze about
%            min(h) < h0 < max(h) correspondig to indexes
%    h     = position vector
%    TF    = plot distribution of indexes (true/false)
%
% OUTPUTS:
%    ind   = indexes to be plotted
%           1 <= ind <= Nmax
%
% EXAMPLE:
%    Nmax = 201; x = linspace(0,25,Nmax);
%    gaussSample(15,Nmax,0.3,12,x,true)
%
% NOTES:
% gaussSample may not always output the same indexes since they are
% determined from random samples. To fix this, maxiter number of random 
% samples are taken and averaged. This seems to result in reproducibility.
%
% IMPROVEMENTS:
% Consider making a normalized input option to use percentages instead of
% h0 being dimensional.

%% First, normalize central point
mu = h0 - min(h);
mu = 2*mu/(max(h)-min(h))-1;

x = linspace(-1,1,Nmax);
y = normpdf(x,mu,sigma);

maxiter = 500;
Y = zeros(maxiter,N);
for j = 1:maxiter
    Y(j,:) = randsample(1:Nmax,N,true,y);
    Y(j,:) = sort(Y(j,:));
    Y(j,1) = 1;
    Y(j,end) = Nmax;
end
Y = mean(Y);
Y = unique(Y);
Y = floor(Y);
if TF
    figure('Position',myPlot.vector)
    subplot(2,1,1)
    plot(x,y)
    title(['Sample distribution: N=' num2str(N) ' pts, Nmax = ' num2str(Nmax) ', \sigma = ' num2str(sigma) ', about ' num2str(h0)])
    xlabel('x')
    ylabel('Gaussian(x)')

    subplot(2,1,2)
    plot(Y,h(Y),'ro')
    title('Sampled Points')
    xlabel('index')
    ylabel('h')
    axis([1 Nmax min(h) max(h)])
end

ind = Y;

end