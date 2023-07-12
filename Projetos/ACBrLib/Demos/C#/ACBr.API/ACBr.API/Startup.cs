using ACBrLib.Boleto;
using ACBrLib.NFe;
using ACBrLib.Web.Boleto;
using ACBrLib.Web.NFe;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.OpenApi.Models;
using System.IO;

namespace ACBr.API
{
    public class Startup
    {
        #region Internal

        //Instancia criada devido a problemas com a impressão, não use essa instancia, use a registrada no IOC.
        //Checar nos controles como usar de forma correta.
        private static ACBrNFe nfe;
        private static ACBrBoleto boleto;

        static Startup()
        {
            //Uso ACBrLib em Memória.. 
            //nfe = new ACBrNFe("[Memory]");
            //boleto = new ACBrBoleto("[Memory]");

            nfe = new ACBrNFe();
            boleto = new ACBrBoleto();

        }

        #endregion Internal

        public Startup(IConfiguration configuration)
        {
            Configuration = configuration;
        }

        public IConfiguration Configuration { get; }

        // This method gets called by the runtime. Use this method to add services to the container.
        public void ConfigureServices(IServiceCollection services)
        {
            services.AddACBrNFe(o => o.UseMemory = false);
            services.AddACBrBoleto(o => o.UseMemory = false);
            services.AddControllers();
            services.AddSwaggerGen(c =>
            {
                c.SwaggerDoc("v1", new OpenApiInfo { Title = "ACBr.API", Version = "v1" });
            });
        }

        // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
        public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
        {
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
                app.UseSwagger();
                app.UseSwaggerUI(c => c.SwaggerEndpoint("/swagger/v1/swagger.json", "ACBr.API v1"));
            }

            app.UseHttpsRedirection();

            app.UseRouting();

            app.UseAuthorization();

            app.UseEndpoints(endpoints =>
            {
                endpoints.MapControllers();
            });
        }
    }
}