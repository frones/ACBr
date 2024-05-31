using System;
using System.Collections.Generic;
using System.Linq;
using ACBr.Net.Core.Extensions;
using ACBrLib.Core;
using ACBrLib.Core.DFe;
using ACBrLib.Core.NFe;
using ACBrLib.NFe;
using ACBrLib.Sat;

namespace ACBr.PDV.Model
{
    public class Venda
    {
        #region Constructors

        public Venda()
        {
            Numero = 0;
            Serie = 0;
            Data = DateTime.Now;
            Cliente = new Cliente();
            Pagamentos = new List<Pagamento>();
            Items = new List<RegistroVenda>();
        }

        #endregion Constructors

        #region Properties

        public Cliente Cliente { get; }

        public int Serie { get; set; }

        public int Numero { get; set; }

        public DateTime Data { get; set; }

        public decimal TotalVenda
        {
            get
            {
                return Items.Where(x => !x.Cancelado).Sum(x => x.ValorTotal).RoundABNT();
            }
        }

        public decimal TotalPago
        {
            get
            {
                return Pagamentos.Sum(x => x.Valor).RoundABNT();
            }
        }

        public decimal Troco
        {
            get
            {
                var ret = TotalPago - TotalVenda;
                return ret < 0 ? 0 : ret;
            }
        }

        public List<RegistroVenda> Items { get; }

        public List<Pagamento> Pagamentos { get; }

        #endregion Properties

        #region Methods

        public void Clear()
        {
            Numero = 0;
            Serie = 0;
            Data = DateTime.Now;
            Cliente.Documento = string.Empty;
            Cliente.Nome = string.Empty;
            Items.Clear();
            Pagamentos.Clear();
        }

        public NotaFiscal ToNFCe()
        {
            var ret = new NotaFiscal();

            // InfNFe
            ret.InfNFe.Versao = "4.0";

            //Identificação
            ret.Identificacao.natOp = "Venda de Mercadoria";
            ret.Identificacao.indPag = IndicadorPagamento.ipVista;
            ret.Identificacao.modelo = ModeloNFe.moNFCe;
            ret.Identificacao.Serie = Serie.ToString();
            ret.Identificacao.nNF = Numero;
            ret.Identificacao.dhEmi = Data;
            ret.Identificacao.tpNF = TipoNFe.tnSaida;
            ret.Identificacao.finNFe = FinalidadeNFe.fnNormal;
            ret.Identificacao.idDest = DestinoOperacao.doInterna;
            ret.Identificacao.indFinal = ConsumidorFinal.cfConsumidorFinal;
            ret.Identificacao.indPres = PresencaComprador.pcPresencial;
            ret.Identificacao.indIntermed = IndIntermed.iiSemOperacao;
            ret.Identificacao.tpImp = TipoDANFE.tiNFCe;
            ret.Identificacao.tpAmb = TipoAmbiente.taHomologacao;

            //Emitente
            ret.Emitente.CRT = Configuracao.Instance.Emitente.CRT;
            ret.Emitente.CNPJCPF = Configuracao.Instance.Emitente.CNPJ.OnlyNumbers();
            ret.Emitente.IE = Configuracao.Instance.Emitente.IE.OnlyNumbers();
            ret.Emitente.xNome = Configuracao.Instance.Emitente.Razao;
            ret.Emitente.xFant = Configuracao.Instance.Emitente.Fantasia;
            ret.Emitente.Fone = Configuracao.Instance.Emitente.Fone;
            ret.Emitente.CEP = Configuracao.Instance.Emitente.CEP;
            ret.Emitente.xLgr = Configuracao.Instance.Emitente.Logradouro;
            ret.Emitente.nro = Configuracao.Instance.Emitente.Numero;
            ret.Emitente.xCpl = Configuracao.Instance.Emitente.Complemento;
            ret.Emitente.xBairro = Configuracao.Instance.Emitente.Bairro;
            ret.Emitente.cMun = Configuracao.Instance.Emitente.CidadeCod.ToInt32();
            ret.Emitente.xMun = Configuracao.Instance.Emitente.Cidade;
            ret.Emitente.UF = Configuracao.Instance.Emitente.UF;

            //Destinatario
            ret.Destinatario.indIEDest = IndicadorIE.inNaoContribuinte;
            if (!string.IsNullOrEmpty(Cliente.Documento))
                ret.Destinatario.CNPJCPF = Cliente.Documento.OnlyNumbers();
            if (!string.IsNullOrEmpty(Cliente.Nome))
                ret.Destinatario.xNome = Cliente.Nome;

            foreach (var item in Items)
            {
                if (item.Cancelado) continue;
                var produto = new ProdutoNFe
                {
                    CFOP = "5.102",
                    cProd = item.Produto.Codigo,
                    cEAN = "SEM GTIN",
                    xProd = item.Produto.Descricao,
                    NCM = "84719012",
                    uCom = item.Produto.Unidade,
                    qCom = item.Quantidade,
                    vUnCom = item.Produto.Valor,
                    vProd = item.ValorTotal,
                    vDesc = 0M,
                    vFrete = 0M,
                    vSeg = 0M,
                    vOutro = 0M,
                    indEscala = IndEscala.ieNaoRelevante,
                    CNPJFab = "05481336000137",
                    uTrib = item.Produto.Unidade,
                    cEANTrib = "SEM GTIN"
                };

                produto.ICMS.CSOSN = CSOSNIcms.csosn900;
                produto.ICMS.orig = OrigemMercadoria.oeNacional;
                produto.ICMS.vBC = 100M;
                produto.ICMS.pICMS = 10M;
                produto.ICMS.vICMS = 10M;
                produto.ICMS.pCredSN = 0M;
                produto.ICMS.vCredICMSSN = 0M;
                produto.ICMS.modBCST = DeterminacaoBaseIcmsST.dbisMargemValorAgregado;
                produto.ICMS.vBCST = 0M;
                produto.ICMS.pICMSST = 0M;
                produto.ICMS.vICMSST = 0M;
                produto.ICMS.pRedBC = 0M;
                produto.ICMS.vBCFCP = 0M;
                produto.ICMS.pFCP = 0M;
                produto.ICMS.vFCP = 0M;

                produto.PIS.CST = CSTPIS.pis01;
                produto.PIS.vBC = 0M;
                produto.PIS.pPIS = 0M;
                produto.PIS.vPIS = 0M;

                produto.COFINS.CST = CSTCofins.cof01;
                produto.COFINS.vBC = 0M;
                produto.COFINS.pCOFINS = 0M;
                produto.COFINS.vCOFINS = 0M;

                produto.IPI.CST = CSTIPI.ipi53;
                produto.IPI.vBC = 0M;
                produto.IPI.pIPI = 0M;
                produto.IPI.vIPI = 0M;

                ret.Produtos.Add(produto);
            }

            ret.Total.vBC = (100 * Items.Count);
            ret.Total.vICMS = (10 * Items.Count);
            ret.Total.vICMSDeson = 0M;
            ret.Total.vBCST = 0M;
            ret.Total.vST = 0M;
            ret.Total.vProd = Items.Sum(x => x.ValorTotal).RoundABNT();
            ret.Total.vFrete = 0M;
            ret.Total.vSeg = 0M;
            ret.Total.vDesc = 0M;
            ret.Total.vIPI = 0M;
            ret.Total.vPIS = 0M;
            ret.Total.vCOFINS = 0M;
            ret.Total.vOutro = 0M;
            ret.Total.vNF = Items.Sum(x => x.ValorTotal).RoundABNT();
            ret.Total.vFCP = 0M;

            ret.DadosAdicionais.infCpl = "";
            ret.Transportador.modFrete = ModalidadeFrete.mfSemFrete;

            for (var i = 0; i < Pagamentos.Count; i++)
            {
                var pagamento = new PagamentoNFe
                {
                    tPag = Pagamentos[i].TipoNFe,
                    vPag = Pagamentos[i].Valor
                };

                if (i == 0 && Troco > 0)
                    pagamento.vTroco = Troco;

                ret.Pagamentos.Add(pagamento);
            }

            return ret;
        }

        public CupomFiscal ToCFeIni()
        {
            var cfe = new CupomFiscal();

            //Identificação
            cfe.Identificacao.numeroCaixa = "01";

            //Emitente
            cfe.Emitente.CNPJ = Configuracao.Instance.Emitente.CNPJ.OnlyNumbers();
            cfe.Emitente.IE = Configuracao.Instance.Emitente.IE.OnlyNumbers();
            cfe.Emitente.IM = Configuracao.Instance.Emitente.IM.OnlyNumbers();

            //Destinatario
            if (!string.IsNullOrEmpty(Cliente.Documento))
                cfe.Destinatario.CNPJCPF = Cliente.Documento;
            if (!string.IsNullOrEmpty(Cliente.Nome))
                cfe.Destinatario.xNome = Cliente.Nome;

            foreach (var item in Items)
            {
                if (item.Cancelado) continue;
                var produto = new ProdutoSat
                {
                    cProd = item.Produto.Codigo,
                    xProd = item.Produto.Descricao,
                    qCom = item.Quantidade,
                    vUnCom = item.Produto.Valor,
                    cEAN = "",
                    uCom = item.Produto.Unidade,
                    NCM = "04072100",
                    CFOP = "5102",
                    Combustivel = false,
                    indRegra = IndRegra.irArredondamento,
                    vDesc = 0,
                    vOutro = 0,
                    vItem12741 = 0
                };

                // Demo feito para trabalhar com simples
                produto.ICMS.orig = OrigemMercadoria.oeNacional;
                produto.ICMS.CSOSN = CSOSNIcms.csosn500;
                produto.PIS.CST = CSTPIS.pis01;
                produto.COFINS.CST = CSTCofins.cof01;

                cfe.Produtos.Add(produto);
            }

            cfe.Total.vCFeLei12741 = cfe.Produtos.Sum(x => x.vItem12741).RoundABNT();

            foreach (var item in Pagamentos)
            {
                var pagamento = new PagamentoSat
                {
                    cMP = item.TipoSAT,
                    vMP = item.Valor
                };

                cfe.Pagamentos.Add(pagamento);
            }

            cfe.DadosAdicionais.infCpl = "Demo ACBr PDV C#";

            return cfe;
        }

        #endregion Methods
    }
}