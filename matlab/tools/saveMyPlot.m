function saveMyPlot(dir,name)
if dir.saveTo
    if isfield(name,'saveFile')
        try
            fh = gcf; temp = [dir.working name.field '\figs\' name.saveFile];
            saveas(fh,temp,'fig'); print('-depsc',temp); print('-dpng',temp)
        catch
            mkdir([dir.working name.field '\figs'])
            fh = gcf; temp = [dir.working name.field '\figs\' name.saveFile];
            saveas(fh,temp,'fig'); print('-depsc',temp); print('-dpng',temp)
        end
    else
        try
            fh = gcf; temp = [dir.working name.field '\figs\' name.file];
            saveas(fh,temp,'fig'); print('-depsc',temp); print('-dpng',temp)
        catch
            mkdir([dir.working name.field '\figs'])
            fh = gcf; temp = [dir.working name.field '\figs\' name.file];
            saveas(fh,temp,'fig'); print('-depsc',temp); print('-dpng',temp)
        end
    end
end
end