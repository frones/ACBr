using System;
using System.IO;
using System.Threading.Tasks;
using ACBrLib.Boleto;
using ACBrLib.Core.Boleto;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;

namespace ACBr.API.Controllers
{
    [ApiController]
    [Route("[controller]")]
    public class BoletoController : ControllerBase
    {
        [HttpPost]
        public async Task<IActionResult> GerarPdf([FromServices] ACBrBoleto boleto, [FromServices] IWebHostEnvironment hostingEnvironment, IFormFile iniBoleto)
        {
            if (!iniBoleto.FileName.EndsWith(".ini")) return BadRequest();

            var codigo = Guid.NewGuid();
            var path = Path.GetTempPath();
            var nomeArquivo = $@"{codigo}.pdf";

            boleto.Config.Impressao.MostrarPreview = false;
            boleto.Config.Impressao.MostrarProgresso = false;
            boleto.Config.Impressao.MostrarSetup = false;
            boleto.Config.Impressao.DirLogo = Path.Combine(hostingEnvironment.ContentRootPath, "Logos");

            boleto.Config.Impressao.NomeArquivo = Path.Combine(path, nomeArquivo);

            using var stream = new StreamReader(iniBoleto.OpenReadStream());
            boleto.IncluirTitulos(await stream.ReadToEndAsync(), BoletoTpSaida.PDF);

            var fs = new FileStream(Path.Combine(path, nomeArquivo), FileMode.Open);
            return File(fs, "application/pdf", nomeArquivo);
        }
    }
}