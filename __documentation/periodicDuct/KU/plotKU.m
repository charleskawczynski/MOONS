% Plot KU
clc; clear; close all;

f = dir(fullfile('','*.dat'))

s = length(f)
% f(files).name

% for i=1:s
%     L = load(f.name(i));
% end
delimiterIn = ' ';
files = dir('*.dat');
headerlinesIn = 3;
figure
for i=1:length(files)
    L = importdata(files(i).name,delimiterIn,headerlinesIn)
    plot(L.data(:,1),L.data(:,2)); hold on
end