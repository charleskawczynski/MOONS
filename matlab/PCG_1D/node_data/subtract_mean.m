function x = subtract_mean(x)
x(1) = 0; x(end) = 0;
x(2:end-1) = x(2:end-1) - mean(x(2:end-1));
x(1) = 0; x(end) = 0;
end