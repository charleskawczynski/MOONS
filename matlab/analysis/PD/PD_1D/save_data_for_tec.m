function save_data_for_tec(file_name,data,header,format)
fp = fopen(file_name,'w');
disp(file_name)
N = length(header);
for i=1:N
	fprintf(fp,header{i});
end
fclose(fp);
dlmwrite(file_name, data, '-append', 'precision', format, 'delimiter', '\t');
end