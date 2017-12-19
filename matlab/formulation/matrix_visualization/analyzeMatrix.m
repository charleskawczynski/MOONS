function analyzeMatrix(A,props)

test_symmetry = props.test_symmetry;
compute_condition_number = props.compute_condition_number;
compute_preconditioned_condition_number = props.compute_preconditioned_condition_number;
debug = props.debug;
compute_eig = props.compute_eig;
name = props.title;

%% Check if SPD
s = size(A);
if test_symmetry
    u = rand(s(1),1); v = rand(s(1),1);
    e1 = (v'*A*u - u'*A*v)/length(u);
    e2 = max(max(abs(A - A')));
    disp([' ---------------------------- Analysis of matrix operator ' name ' ---------------------------- '])
    disp(['Symmetry error (v^T*A*u - u^T*A*v)/size(u) = ' num2str(e1)])
    disp(['Symmetry error max(max(abs(A - A^T))) = ' num2str(e2)])
end
if compute_condition_number
    disp(['Condition number = ' num2str(cond(A))])
end
if compute_preconditioned_condition_number
    M = diag(1./diag(A));
    A_prec = M*A;
end
if compute_preconditioned_condition_number && compute_condition_number
    disp(['Condition number after preconditioning = ' num2str(cond(A_prec))])
end
if compute_eig
    disp(['Eigenvalues = ']); Eig = eig(A)'
end

%% Plot original matrix
figure; spy(A,'ko')
h = gcf;
ms = 2;
lw = 2;
set( findobj(h, 'Type', 'line'), 'LineWidth',lw, 'MarkerEdgeColor','k','MarkerFaceColor',[0 0 0], 'MarkerSize',ms);

title([props.title], 'FontSize', props.title_size)
set(gca, 'FontSize', props.axis_size)

temp=findobj(h,'type','axe');
xtemp=get(get(temp,'xlabel'),'string');
x_label = strcat('Number of non-zeros = ',xtemp(6:end));
x_label = '';
xlabel(x_label, 'FontSize', props.label_size);
set(gca,'xticklabel',{[]})
set(gca,'yticklabel',{[]})

if props.save_fig
    saveas(h,[props.filedir props.filename '.fig'])
    saveas(h,[props.filedir props.filename '.eps'])
end
if props.save_fig
    set( findobj(h, 'Type', 'line'), 'LineWidth',lw*2, 'MarkerEdgeColor','k','MarkerFaceColor',[0 0 0], 'MarkerSize',ms);
    print('-dpng','-r400',[props.filedir props.filename '.png']);
    set( findobj(h, 'Type', 'line'), 'LineWidth',lw, 'MarkerEdgeColor','k','MarkerFaceColor',[0 0 0], 'MarkerSize',ms);
end

% h = gcf;
% set(h,'Color','b')
% figure; surf(A)
% title(['Matrix surface of ' name ' operator'])

if compute_preconditioned_condition_number
%     figure; surf(A_prec)
%     title(['Matrix surface of preconditioned ' name ' operator'])
    figure; plot(diag(A))
    title(['diag(' name ')'])
%     figure; plot(1./diag(A))
%     title(['1/diag(' name ')'])
end


if debug
    if (max(max(abs(A-A')))>0)
        figure; spy(A-A')
        title(['A-transpose(A) : ' name ' operator'])
    end
end

% if debug
%     idx = eye(size(A)); Y = (1-idx).*A;
%     figure
%     surf(Y)
%     title(['Matrix (minus diagonal): ' name ' operator'])
%     %% Plot SPD error of matrix (minus diagonal, since this likely does not contribute)
%     if (max(max(abs(A-A')))>0)
%         figure; spy(A-A')
%         title(['SPD erorr of matrix : ' name ' operator'])

%         idx = eye(size(A)); Y = (1-idx).*A;
%         figure
%         contourf(Y-Y')
%         title(['SPD error of matrix (minus diagonal): ' name ' operator'])
%     end
% end

end