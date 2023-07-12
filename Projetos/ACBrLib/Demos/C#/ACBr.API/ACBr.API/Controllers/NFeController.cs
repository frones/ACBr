using System;
using Microsoft.AspNetCore.Mvc;
using System.IO;
using System.Threading.Tasks;
using ACBrLib.NFe;
using Microsoft.AspNetCore.Http;
using ACBrLib.Core.DFe;
using ACBrLib.Core.NFe;

namespace ACBr.API.Controllers
{
    [ApiController]
    [Route("[controller]")]
    public class NFeController : ControllerBase
    {
        [HttpPost]
        [Route("GerarPDF")]
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

        [HttpPost]
        [Route("EnviarNFe")]
        public async Task<IActionResult> EnviarNFe([FromServices] ACBrNFe nfe)
        {
            Console.WriteLine("Digite numero da NF");
            int numeroNF = Convert.ToInt32(Console.ReadLine());

            for (int i = 0; i < numeroNF; i++)
            {
                Console.WriteLine("Processando NF Número " + i);
            }
                try
                {
                    //Aponte o Schemas NFe e arquivo ACBrNFeServicos.ini
                    nfe.Config.PathSchemas = @"C:\temp\Schemas\";
                    nfe.Config.IniServicos = @"C:\temp\ACBrNFeServicos.ini";

                    nfe.ConfigGravar();

                    var notaFiscal = new NotaFiscal();

                    //infNFe
                    notaFiscal.InfNFe.Versao = "4.0";

                    //Identificação
                    notaFiscal.Identificacao.cNF = 400;
                    notaFiscal.Identificacao.natOp = "COMPRA DE MERCADORIA IMPORTADA PARA COMERCIALIZAÇÃO";
                    notaFiscal.Identificacao.indPag = IndicadorPagamento.ipVista;
                    notaFiscal.Identificacao.mod = ModeloNFe.moNFe;
                    notaFiscal.Identificacao.Serie = "1";
                    notaFiscal.Identificacao.nNF = numeroNF;
                    notaFiscal.Identificacao.dhEmi = DateTime.Now;
                    notaFiscal.Identificacao.dhSaiEnt = DateTime.Now;
                    notaFiscal.Identificacao.tpNF = TipoNFe.tnSaida;
                    notaFiscal.Identificacao.idDest = DestinoOperacao.doInterna;
                    notaFiscal.Identificacao.tpAmb = TipoAmbiente.taHomologacao;
                    notaFiscal.Identificacao.tpImp = TipoDANFE.tiRetrato;
                    notaFiscal.Identificacao.tpEmis = TipoEmissao.teNormal;
                    notaFiscal.Identificacao.finNFe = FinalidadeNFe.fnNormal;
                    notaFiscal.Identificacao.indFinal = ConsumidorFinal.cfConsumidorFinal;
                    notaFiscal.Identificacao.indPres = PresencaComprador.pcPresencial;
                    notaFiscal.Identificacao.procEmi = ProcessoEmissao.peAplicativoContribuinte;
                    notaFiscal.Identificacao.indIntermed = IndIntermed.iiOperacaoSemIntermediador;
                    notaFiscal.Identificacao.verProc = "v1.09.90";

                    //Emitente
                    notaFiscal.Emitente.CRT = CRT.crtSimplesNacional;
                    notaFiscal.Emitente.CNPJCPF = "99999999999999";
                    notaFiscal.Emitente.xNome = "PROJETO ACBR";
                    notaFiscal.Emitente.xFant = "PROJETO ACBR";
                    notaFiscal.Emitente.IE = "999999999999";
                    notaFiscal.Emitente.IEST = "";
                    notaFiscal.Emitente.IM = "";
                    notaFiscal.Emitente.CNAE = "";
                    notaFiscal.Emitente.xLgr = "Rua Cel Aureliano Camargo";
                    notaFiscal.Emitente.nro = "973";
                    notaFiscal.Emitente.xCpl = "";
                    notaFiscal.Emitente.xBairro = "Centro";
                    notaFiscal.Emitente.cMun = 3554003;
                    notaFiscal.Emitente.xMun = "Tatui";
                    notaFiscal.Emitente.cUF = "35";
                    notaFiscal.Emitente.UF = "SP";
                    notaFiscal.Emitente.CEP = "18270000";
                    notaFiscal.Emitente.cPais = 1058;
                    notaFiscal.Emitente.xPais = "BRASIL";
                    notaFiscal.Emitente.Fone = "(11)9999-9999";
                    notaFiscal.Emitente.cMunFG = 3554003;

                    //Destinatario
                    notaFiscal.Destinatario.idEstrangeiro = "";
                    notaFiscal.Destinatario.CNPJCPF = "99999999999999";
                    notaFiscal.Destinatario.xNome = "Nome Destinatario";
                    notaFiscal.Destinatario.indIEDest = IndicadorIE.inIsento;
                    notaFiscal.Destinatario.IE = "ISENTO";
                    notaFiscal.Destinatario.ISUF = "";
                    notaFiscal.Destinatario.Email = "acbr@projetoacbr.com.br";
                    notaFiscal.Destinatario.xLgr = "Rua das Flores";
                    notaFiscal.Destinatario.nro = "973";
                    notaFiscal.Destinatario.xCpl = "";
                    notaFiscal.Destinatario.xBairro = "Centro";
                    notaFiscal.Destinatario.cMun = 3550308;
                    notaFiscal.Destinatario.xMun = "São Paulo";
                    notaFiscal.Destinatario.UF = "SP";
                    notaFiscal.Destinatario.CEP = "04615000";
                    notaFiscal.Destinatario.cPais = 1058;
                    notaFiscal.Destinatario.xPais = "BRASIL";
                    notaFiscal.Destinatario.Fone = "(11)9999-9999";

                    //Produto
                    var produto = new ProdutoNFe();

                    produto.nItem = 1;
                    produto.cProd = "123456";
                    produto.cEAN = "7896523206646";
                    produto.xProd = "Camisa Polo ACBr";
                    produto.NCM = "61051000";
                    produto.EXTIPI = "";
                    produto.CFOP = "5101";
                    produto.uCom = "UN";
                    produto.qCom = 1;
                    produto.vUnCom = 100;
                    produto.vProd = 100;
                    produto.cEANTrib = "7896523206646";
                    produto.uTrib = "UN";
                    produto.qTrib = 1;
                    produto.vUnTrib = 100;
                    produto.vOutro = 0;
                    produto.vFrete = 0;
                    produto.vSeg = 0;
                    produto.vDesc = 0;
                    produto.infAdProd = "Informacao Adicional do Produto";
                    produto.indTot = IndicadorTotal.itSomaTotalNFe;

                    //ICMS
                    produto.ICMS.orig = 0;
                    produto.ICMS.CSOSN = CSOSNIcms.csosn900;
                    produto.ICMS.modBC = DeterminacaoBaseIcms.dbiPrecoTabelado;
                    produto.ICMS.pRedBC = 0;
                    produto.ICMS.vBC = 100;
                    produto.ICMS.pICMS = 18;
                    produto.ICMS.vICMS = 18;
                    produto.ICMS.modBCST = DeterminacaoBaseIcmsST.dbisMargemValorAgregado;

                    //PIS
                    produto.PIS.CST = CSTPIS.pis98;

                    //COFINS
                    produto.COFINS.CST = CSTCofins.cof98;

                    notaFiscal.Produtos.Add(produto);

                    notaFiscal.Total.vBC = 100;
                    notaFiscal.Total.vICMS = 18;
                    notaFiscal.Total.vBCST = 0;
                    notaFiscal.Total.vST = 0;
                    notaFiscal.Total.vProd = 100;
                    notaFiscal.Total.vFrete = 0;
                    notaFiscal.Total.vSeg = 0;
                    notaFiscal.Total.vDesc = 0;
                    notaFiscal.Total.vII = 0;
                    notaFiscal.Total.vIPI = 0;
                    notaFiscal.Total.vPIS = 0;
                    notaFiscal.Total.vCOFINS = 0;
                    notaFiscal.Total.vOutro = 0;
                    notaFiscal.Total.vNF = 100;

                    // lei da transparencia de impostos
                    notaFiscal.Total.vTotTrib = 0;

                    // partilha do icms e fundo de probreza
                    notaFiscal.Total.vFCPUFDest = 0;
                    notaFiscal.Total.vICMSUFDest = 0;
                    notaFiscal.Total.vICMSUFRemet = 0;

                    var pagamento = new PagamentoNFe();
                    pagamento.indPag = IndicadorPagamento.ipVista;
                    pagamento.tPag = FormaPagamento.fpDinheiro;
                    pagamento.xPag = "";
                    pagamento.vPag = 100;

                    notaFiscal.Pagamentos.Add(pagamento);

                    string nota = notaFiscal.ToString();


                    nfe.LimparLista();
                    nfe.CarregarINI(nota);
                    //nfe.CarregarINI(@"C:\temp\NFeCompra.INI");
                    nfe.Enviar(1);

                    return Ok();

                }
                catch (Exception ex)
                {
                    Console.WriteLine($"Erro emissão NFe: {ex.Message}");
                    return BadRequest();
                }
        }
    }
}
