using System;
using System.Collections.Generic;
using System.Linq;
using ACBr.Net.Core.Extensions;
using ACBrLib;
using ACBrLib.Core;

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

        public string ToNFCeIni()
        {
            var vendaIni = new ACBrIniFile();

            // InfNFe
            vendaIni["infNFe"]["versao"] = "4.0";

            //Identificação
            vendaIni["Identificacao"]["natOp"] = "Venda de Mercadoria";
            vendaIni["Identificacao"]["indPag"] = "0";
            vendaIni["Identificacao"]["mod"] = "65";
            vendaIni["Identificacao"]["serie"] = Serie.ToString();
            vendaIni["Identificacao"]["nNF"] = Numero.ToString();
            vendaIni["Identificacao"]["dEmi"] = Data.ToString("dd/MM/yyyy HH:mm:ss");
            vendaIni["Identificacao"]["tpNF"] = "1";
            vendaIni["Identificacao"]["Finalidade"] = "0";
            vendaIni["Identificacao"]["idDest"] = "1";
            vendaIni["Identificacao"]["indFinal"] = "1";
            vendaIni["Identificacao"]["indPres"] = "1";
            vendaIni["Identificacao"]["tpimp"] = "4";
            vendaIni["Identificacao"]["tpAmb"] = "2";

            //Emitente
            vendaIni["Emitente"]["CRT"] = ((int)Configuracao.Instance.Emitente.CRT).ToString();
            vendaIni["Emitente"]["CNPJ"] = Configuracao.Instance.Emitente.CNPJ.OnlyNumbers();
            vendaIni["Emitente"]["IE"] = Configuracao.Instance.Emitente.IE.OnlyNumbers();
            vendaIni["Emitente"]["Razao"] = Configuracao.Instance.Emitente.Razao;
            vendaIni["Emitente"]["Fantasia"] = Configuracao.Instance.Emitente.Fantasia;
            vendaIni["Emitente"]["Fone"] = Configuracao.Instance.Emitente.Fone;
            vendaIni["Emitente"]["CEP"] = Configuracao.Instance.Emitente.CEP;
            vendaIni["Emitente"]["Logradouro"] = Configuracao.Instance.Emitente.Logradouro;
            vendaIni["Emitente"]["Numero"] = Configuracao.Instance.Emitente.Numero;
            vendaIni["Emitente"]["Complemento"] = Configuracao.Instance.Emitente.Complemento;
            vendaIni["Emitente"]["Bairro"] = Configuracao.Instance.Emitente.Bairro;
            vendaIni["Emitente"]["CidadeCod"] = Configuracao.Instance.Emitente.CidadeCod;
            vendaIni["Emitente"]["Cidade"] = Configuracao.Instance.Emitente.Cidade;
            vendaIni["Emitente"]["UF"] = Configuracao.Instance.Emitente.UF;

            //Destinatario
            vendaIni["Destinatario"]["indIEDest"] = "9";
            if (!string.IsNullOrEmpty(Cliente.Documento))
                vendaIni["Destinatario"]["CNPJCPF"] = Cliente.Documento;
            if (!string.IsNullOrEmpty(Cliente.Nome))
                vendaIni["Destinatario"]["xNome"] = Cliente.Nome;

            for (var i = 0; i < Items.Count; i++)
            {
                var item = Items[i];
                if (item.Cancelado) continue;
                var sessaoProduto = $"Produto{i + 1:000}";
                var sessaoICMS = $"ICMS{i + 1:000}";
                var sessaoPIS = $"PIS{i + 1:000}";
                var sessaoCOFINS = $"COFINS{i + 1:000}";
                var sessaoIPI = $"IPI{i + 1:000}";

                vendaIni[sessaoProduto]["CFOP"] = "5.102";
                vendaIni[sessaoProduto]["Codigo"] = item.Produto.Codigo;
                vendaIni[sessaoProduto]["cEAN"] = "SEM GTIN";
                vendaIni[sessaoProduto]["Descricao"] = item.Produto.Descricao;
                vendaIni[sessaoProduto]["NCM"] = "84719012";
                vendaIni[sessaoProduto]["Unidade"] = item.Produto.Unidade;
                vendaIni[sessaoProduto]["Quantidade"] = item.Quantidade.ToString("N4");
                vendaIni[sessaoProduto]["ValorUnitario"] = item.Produto.Valor.ToString("N10");
                vendaIni[sessaoProduto]["ValorTotal"] = item.ValorTotal.ToString("N2");
                vendaIni[sessaoProduto]["ValorDesconto"] = "0,00";
                vendaIni[sessaoProduto]["vFrete"] = "0,00";
                vendaIni[sessaoProduto]["vSeg"] = "0,00";
                vendaIni[sessaoProduto]["vOutro"] = "0,00";
                vendaIni[sessaoProduto]["indEscala"] = "N";
                vendaIni[sessaoProduto]["CNPJFab"] = "05481336000137";
                vendaIni[sessaoProduto]["uTrib"] = item.Produto.Unidade;
                vendaIni[sessaoProduto]["cEANTrib"] = "SEM GTIN";

                vendaIni[sessaoICMS]["CSOSN"] = "900";
                vendaIni[sessaoICMS]["Origem"] = "0";
                vendaIni[sessaoICMS]["ValorBase"] = "100";
                vendaIni[sessaoICMS]["Aliquota"] = "10";
                vendaIni[sessaoICMS]["Valor"] = "10";
                vendaIni[sessaoICMS]["pCredSN"] = "0,00";
                vendaIni[sessaoICMS]["vCredICMSSN"] = "0,00";
                vendaIni[sessaoICMS]["ModalidadeST"] = "4";
                vendaIni[sessaoICMS]["ValorBaseST"] = "0,00";
                vendaIni[sessaoICMS]["AliquotaST"] = "0,00";
                vendaIni[sessaoICMS]["ValorST"] = "0,00";
                vendaIni[sessaoICMS]["PercentualReducao"] = "0,00";
                vendaIni[sessaoICMS]["vBCFCP"] = "0";
                vendaIni[sessaoICMS]["pFCP"] = "0";
                vendaIni[sessaoICMS]["vFCP"] = "0";

                vendaIni[sessaoPIS]["CST"] = "01";
                vendaIni[sessaoPIS]["ValorBase"] = "0,00";
                vendaIni[sessaoPIS]["Aliquota"] = "0,00";
                vendaIni[sessaoPIS]["Valor"] = "0,00";

                vendaIni[sessaoCOFINS]["CST"] = "01";
                vendaIni[sessaoCOFINS]["ValorBase"] = "0,00";
                vendaIni[sessaoCOFINS]["Aliquota"] = "0,00";
                vendaIni[sessaoCOFINS]["Valor"] = "0,00";

                vendaIni[sessaoIPI]["CST"] = "53";
                vendaIni[sessaoIPI]["ValorBase"] = "0,00";
                vendaIni[sessaoIPI]["Aliquota"] = "0,00";
                vendaIni[sessaoIPI]["Valor"] = "0,00";
            }

            vendaIni["Total"]["BaseICMS"] = (100 * Items.Count).ToString("N2");
            vendaIni["Total"]["ValorICMS"] = (10 * Items.Count).ToString("N2");
            vendaIni["Total"]["vICMSDeson"] = "0,00";
            vendaIni["Total"]["BaseICMSSubstituicao"] = "0,00";
            vendaIni["Total"]["ValorICMSSubstituicao"] = "0,00";
            vendaIni["Total"]["ValorProduto"] = Items.Sum(x => x.ValorTotal).RoundABNT().ToString("N2");
            vendaIni["Total"]["ValorFrete"] = "0,00";
            vendaIni["Total"]["ValorSeguro"] = "0,00";
            vendaIni["Total"]["ValorDesconto"] = "0,00";
            vendaIni["Total"]["ValorIPI"] = "0,00";
            vendaIni["Total"]["ValorPIS"] = "0,00";
            vendaIni["Total"]["ValorCOFINS"] = "0,00";
            vendaIni["Total"]["ValorOutrasDespesas"] = "0,00";
            vendaIni["Total"]["ValorNota"] = Items.Sum(x => x.ValorTotal).RoundABNT().ToString("N2");
            vendaIni["Total"]["vFCP"] = "0";

            vendaIni["DadosAdicionais"]["Complemento"] = "";

            vendaIni["Transportador"]["modFrete"] = "9";

            for (var i = 0; i < Pagamentos.Count; i++)
            {
                var item = Pagamentos[i];
                var sessaoPagamento = $"pag{i + 1:000}";

                vendaIni[sessaoPagamento]["tPag"] = item.TipoNFe.ToString("00");
                vendaIni[sessaoPagamento]["vPag"] = item.Valor.ToString("N2");
                if (i == 0 && Troco > 0)
                    vendaIni[sessaoPagamento]["vTroco"] = Troco.ToString("N2");
            }

            return vendaIni.ToString();
        }

        public string ToCFeIni()
        {
            var vendaIni = new ACBrIniFile();

            //Identificação
            vendaIni["Identificacao"]["numeroCaixa"] = "01";

            //Emitente
            vendaIni["Emitente"]["CNPJ"] = Configuracao.Instance.Emitente.CNPJ.OnlyNumbers();
            vendaIni["Emitente"]["IE"] = Configuracao.Instance.Emitente.IE.OnlyNumbers();
            vendaIni["Emitente"]["IM"] = Configuracao.Instance.Emitente.IM.OnlyNumbers();

            //Destinatario
            if (!string.IsNullOrEmpty(Cliente.Documento))
                vendaIni["Destinatario"]["CNPJCPF"] = Cliente.Documento;
            if (!string.IsNullOrEmpty(Cliente.Nome))
                vendaIni["Destinatario"]["xNome"] = Cliente.Nome;

            var vItem12741 = 0M;

            for (var i = 0; i < Items.Count; i++)
            {
                var item = Items[i];
                if (item.Cancelado) continue;
                var sessaoProduto = $"Produto{i + 1:000}";
                var sessaoICMS = $"ICMS{i + 1:000}";
                var sessaoPIS = $"PIS{i + 1:000}";
                var sessaoCOFINS = $"COFINS{i + 1:000}";

                vendaIni[sessaoProduto]["cProd"] = item.Produto.Codigo;
                vendaIni[sessaoProduto]["xProd"] = item.Produto.Descricao;
                vendaIni[sessaoProduto]["qCom"] = item.Quantidade.ToString("N4");
                vendaIni[sessaoProduto]["vUnCom"] = item.Produto.Valor.ToString("N2");
                vendaIni[sessaoProduto]["cEAN"] = "";
                vendaIni[sessaoProduto]["uCom"] = item.Produto.Unidade;
                vendaIni[sessaoProduto]["NCM"] = "04072100";
                vendaIni[sessaoProduto]["CFOP"] = "5102";
                vendaIni[sessaoProduto]["Combustivel"] = "0";
                vendaIni[sessaoProduto]["indRegra"] = "A";
                vendaIni[sessaoProduto]["vDesc"] = "0";
                vendaIni[sessaoProduto]["vOutro"] = "0";
                vendaIni[sessaoProduto]["vItem12741"] = "0";

                // Demo feito para trabalhar com simples
                vendaIni[sessaoICMS]["Origem"] = "0";
                vendaIni[sessaoICMS]["CSOSN"] = "500";

                vendaIni[sessaoPIS]["CST"] = "01";

                vendaIni[sessaoCOFINS]["CST"] = "01";
            }

            vendaIni["Total"]["vCFeLei12741"] = vItem12741.ToString("N2");

            for (var i = 0; i < Pagamentos.Count; i++)
            {
                var item = Pagamentos[i];
                var sessaoPagamento = $"Pagto{i + 1:000}";

                vendaIni[sessaoPagamento]["cMP"] = item.TipoSAT.ToString();
                vendaIni[sessaoPagamento]["vMP"] = item.Valor.ToString("N2");
            }

            vendaIni["DadosAdicionais"]["infCpl"] = "Demo ACBr PDV C#";

            return vendaIni.ToString();
        }

        #endregion Methods
    }
}