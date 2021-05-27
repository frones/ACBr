using System;
using System.IO;
using ACBrLib.Boleto;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Options;

namespace ACBrLib.Web.Boleto
{
    public static class ACBrLibBoletoExtensions
    {
        public static void AddACBrBoleto(this IServiceCollection services, Action<ACBrBoletoOptions> setupAction = null)
        {
            services.AddOptions<ACBrBoletoOptions>();
            services.Configure(setupAction);

            services.AddTransient((s) =>
            {
                var options = s.GetService<IOptions<ACBrBoletoOptions>>();

                if (options.Value.UseMemory)
                    return new ACBrBoleto("[Memory]", options.Value.Senha);

                var hosting = s.GetService<IWebHostEnvironment>();
                return new ACBrBoleto($@"{hosting.ContentRootPath}\{options.Value.ConfigName}", options.Value.Senha);
            });
        }

        public static void ConfigFromFile(this ACBrBoleto acbrBoleto, string file)
        {
            if (!File.Exists(file)) return;

            acbrBoleto.ImportarConfig(File.ReadAllText(file));
        }
    }
}