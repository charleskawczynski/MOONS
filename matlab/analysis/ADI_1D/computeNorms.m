function err = computeNorms(f_approx,f_exact)
err.L2 = norm(f_approx-f_exact)/norm(f_exact);
err.L1 = norm(f_approx-f_exact,1)/norm(f_exact,1);
err.max = max(abs(f_approx-f_exact))/max(abs(f_exact));
disp(['L1 error = ' num2str(err.L1)]);
disp(['L2 error = ' num2str(err.L2)]);
disp(['Linf error = ' num2str(err.max)]);
end