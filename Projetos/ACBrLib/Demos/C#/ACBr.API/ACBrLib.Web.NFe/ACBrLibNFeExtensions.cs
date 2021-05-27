using System;
using System.IO;
using ACBrLib.NFe;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Options;

namespace ACBrLib.Web.NFe
{
    public static class ACBrLibNFeExtensions
    {
        public static void AddACBrNFe(this IServiceCollection services, Action<ACBrNFeOptions> setupAction = null)
        {
            services.AddOptions<ACBrNFeOptions>();
            services.Configure(setupAction);

            services.AddTransient((s) =>
            {
                var options = s.GetService<IOptions<ACBrNFeOptions>>();

                if (options.Value.UseMemory)
                    return new ACBrNFe("[Memory]", options.Value.Senha);

                var hosting = s.GetService<IWebHostEnvironment>();
                return new ACBrNFe($@"{hosting.ContentRootPath}\{options.Value.ConfigName}", options.Value.Senha);
            });
        }

        public static void ConfigFromFile(this ACBrNFe acbrNFe, string file)
        {
            if (!File.Exists(file)) return;

            acbrNFe.ImportarConfig(File.ReadAllText(file));
        }
    }
}