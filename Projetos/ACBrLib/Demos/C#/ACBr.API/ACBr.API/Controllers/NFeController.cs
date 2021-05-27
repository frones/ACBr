using System;
using Microsoft.AspNetCore.Mvc;
using System.IO;
using System.Threading.Tasks;
using ACBrLib.NFe;
using Microsoft.AspNetCore.Http;

namespace ACBr.API.Controllers
{
    [ApiController]
    [Route("[controller]")]
    public class NFeController : ControllerBase
    {
        [HttpPost]
        public async Task<IActionResult> GerarPdf([FromServices] ACBrNFe nfe, IFormFile xmlNFe)
        {
            if (!xmlNFe.FileName.EndsWith(".xml")) return BadRequest();

            using var stream = new StreamReader(xmlNFe.OpenReadStream());
            nfe.CarregarXML(await stream.ReadToEndAsync());

            var codigo = Guid.NewGuid();
            var path = Path.GetTempPath();
            var nomeArquivo = $@"{codigo}.pdf";

            nfe.Config.DANFe.MostraSetup = false;
            nfe.Config.DANFe.MostraPreview = false;
            nfe.Config.DANFe.MostraStatus = false;
            nfe.Config.DANFe.PathPDF = path;
            nfe.Config.DANFe.NomeDocumento = nomeArquivo;

            nfe.ImprimirPDF();

            var fs = new FileStream(Path.Combine(path, nomeArquivo), FileMode.Open);
            return File(fs, "application/pdf", nomeArquivo);
        }
    }
}