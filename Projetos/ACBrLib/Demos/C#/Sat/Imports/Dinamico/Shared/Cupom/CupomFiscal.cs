using System.Collections.Generic;
using System.IO;
using ACBrLib.Core;

namespace ACBrLib.Sat
{
    public sealed class CupomFiscal
    {
        #region Constructor

        public CupomFiscal()
        {
        }

        internal CupomFiscal(ACBrIniFile ini) : this()
        {
            ReadFromIni(ini);
        }

        #endregion Constructor

        #region Properties

        public InfCFe InfCFe { get; } = new InfCFe();

        public IdentificacaoSat Identificacao { get; } = new IdentificacaoSat();

        public EmitenteSat Emitente { get; } = new EmitenteSat();

        public DestinatarioSat Destinatario { get; } = new DestinatarioSat();

        public EntregaSat Entrega { get; } = new EntregaSat();

        public List<ProdutoSat> Produtos { get; } = new List<ProdutoSat>();

        public TotalSat Total { get; } = new TotalSat();

        public TotalISSQN ISSQNtot {  get; } = new TotalISSQN();

        public List<PagamentoSat> Pagamentos { get; } = new List<PagamentoSat>();

        public DadosAdicionaisSat DadosAdicionais { get; } = new DadosAdicionaisSat();

        public List<ObsFiscoSat> ObsFisco { get; } = new List<ObsFiscoSat>();

        #endregion Properties

        #region Methods

        /// <inheritdoc/>
        public override string ToString()
        {
            return WriteToIni().ToString();
        }

        private ACBrIniFile WriteToIni()
        {
            var iniData = new ACBrIniFile();

            iniData.WriteToIni(InfCFe, "infCFe");
            iniData.WriteToIni(Identificacao, "Identificacao");
            iniData.WriteToIni(Emitente, "Emitente");
            iniData.WriteToIni(Destinatario, "Destinatario");
            if (!string.IsNullOrEmpty(Entrega.xCpl))
                iniData.WriteToIni(Entrega, "Entrega");

            for (var i = 0; i < Produtos.Count; i++)
            {
                iniData.WriteToIni(Produtos[i], $"Produto{i + 1:000}");

                for (var j = 0; j < Produtos[i].ObsFisco.Count; j++)
                    iniData.WriteToIni(Produtos[i].ObsFisco[j], $"OBSFISCODET{i + 1:000}{j + 1:000}");

                if (Produtos[i].ICMS.CST != Core.DFe.CSTIcms.cstVazio || Produtos[i].ICMS.CSOSN != Core.DFe.CSOSNIcms.csosnVazio)
                    iniData.WriteToIni(Produtos[i].ICMS, $"ICMS{i + 1:000}");

                if (Produtos[i].PIS.pPIS > 0)
                    iniData.WriteToIni(Produtos[i].PIS, $"PIS{i + 1:000}");

                if (Produtos[i].PISST.vBC > 0)
                    iniData.WriteToIni(Produtos[i].PISST, $"PISST{i + 1:000}");

                if (Produtos[i].COFINS.pCOFINS > 0)
                    iniData.WriteToIni(Produtos[i].COFINS, $"COFINS{i + 1:000}");

                if (Produtos[i].COFINSST.vBC > 0)
                    iniData.WriteToIni(Produtos[i].COFINSST, $"COFINSST{i + 1:000}");

                if (Produtos[i].ISSQN.vBC > 0)
                    iniData.WriteToIni(Produtos[i].ISSQN, $"ISSQN{i + 1:000}");
            }

            iniData.WriteToIni(Total, "Total");

            for (var i = 0; i < Pagamentos.Count; i++)
                iniData.WriteToIni(Pagamentos[i], $"pag{i + 1:000}");

            iniData.WriteToIni(DadosAdicionais, "DadosAdicionais");

            for (var i = 0; i < ObsFisco.Count; i++)
                iniData.WriteToIni(ObsFisco[i], $"ObsFisco{i + 1:000}");

            return iniData;
        }

        private void ReadFromIni(ACBrIniFile iniData)
        {
            iniData.ReadFromIni(InfCFe, "infCFe");
            iniData.ReadFromIni(Identificacao, "Identificacao");
            iniData.ReadFromIni(Emitente, "Emitente");
            iniData.ReadFromIni(Destinatario, "Destinatario");
            iniData.ReadFromIni(Entrega, "Entrega");

            var i = 0;
            ProdutoSat produto;
            do
            {
                i++;
                produto = iniData.ReadFromIni<ProdutoSat>($"Produto{i:000}");
                if (produto == null) continue;

                var j = 0;
                ObsFiscoDetSat obsFiscoDet;
                do
                {
                    j++;
                    obsFiscoDet = iniData.ReadFromIni<ObsFiscoDetSat>($"Produto{i:000}{j:000}");
                    if (obsFiscoDet == null) continue;

                    produto.ObsFisco.Add(obsFiscoDet);
                } while (obsFiscoDet != null);

                iniData.ReadFromIni(produto.ICMS, $"ICMS{i:000}");
                iniData.ReadFromIni(produto.PIS, $"PIS{i:000}");
                iniData.ReadFromIni(produto.PISST, $"PISST{i:000}");
                iniData.ReadFromIni(produto.COFINS, $"COFINS{i:000}");
                iniData.ReadFromIni(produto.COFINSST, $"COFINSST{i:000}");
                iniData.ReadFromIni(produto.ISSQN, $"ISSQN{i:000}");

                Produtos.Add(produto);
            } while (produto != null);

            iniData.ReadFromIni(Total, "Total");
            iniData.ReadFromIni(ISSQNtot, "ISSQNtot");

            i = 0;
            PagamentoSat pagamento;
            do
            {
                i++;
                pagamento = iniData.ReadFromIni<PagamentoSat>($"pag{i:000}");
                if (pagamento == null) continue;

                Pagamentos.Add(pagamento);
            } while (pagamento != null);

            iniData.ReadFromIni(DadosAdicionais, "DadosAdicionais");

            i = 0;
            ObsFiscoSat obsFisco;
            do
            {
                i++;
                obsFisco = iniData.ReadFromIni<ObsFiscoSat>($"ObsFisco{i:000}");
                if (obsFisco == null) continue;

                ObsFisco.Add(obsFisco);
            } while (obsFisco != null);
        }

        public static CupomFiscal Load(string conteudo) => ACBrIniFile.Parse(conteudo);

        public static CupomFiscal LoadFromFile(string filePath) => ACBrIniFile.Load(filePath);

        public static CupomFiscal LoadFromFile(Stream stream) => ACBrIniFile.Load(stream);

        #endregion Methods

        #region Operators

        public static implicit operator CupomFiscal(ACBrIniFile source) => new CupomFiscal(source);

        public static implicit operator ACBrIniFile(CupomFiscal source) => source.WriteToIni();

        #endregion Operators
    }
}