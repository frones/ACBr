using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using ACBrLib.Core;
using ACBrLib.Core.Boleto;

namespace ACBrLib.Boleto
{
    public sealed class Titulo
    {
        #region Properties

        [IniIgnore]
        public int Index { get; internal set; } = 0;

        public Sacado Sacado { get; } = new Sacado();

        public Ocorrencia OcorrenciaOriginal { get; } = new Ocorrencia();

        public AceiteTitulo Aceite { get; set; }

        public TipoDiasIntrucao TipoDiasProtesto { get; set; }

        public TipoDiasIntrucao TipoDiasNegativacao { get; set; }

        public TipoImpressao TipoImpressao { get; set; }

        public TipoDesconto TipoDesconto { get; set; }

        public TipoDesconto TipoDesconto2 { get; set; }

        public CarteiraEnvio CarteiraEnvio { get; set; }

        public bool MultaValorFixo { get; set; }

        public string LocalPagamento { get; set; }

        public DateTime Vencimento { get; set; }

        public DateTime DataDocumento { get; set; }

        public DateTime DataProcessamento { get; set; }

        public DateTime? DataAbatimento { get; set; }

        public DateTime? DataDesconto { get; set; }

        public DateTime? DataMoraJuros { get; set; }

        public DateTime? DataMulta { get; set; }

        public int DiasDeProtesto { get; set; }

        public DateTime? DataProtesto { get; set; }

        public CodigoNegativacao CodigoNegativacao { get; set; } = CodigoNegativacao.cnNaoProtestar;

        public int DiasDeNegativacao { get; set; }

        public DateTime? DataNegativacao { get; set; }

        public DateTime? DataBaixa { get; set; }

        public DateTime? DataLimitePagto { get; set; }

        public string NumeroDocumento { get; set; }

        public string Especie { get; set; }

        public string Carteira { get; set; }

        public string NossoNumero { get; set; }

        public decimal ValorDocumento { get; set; }

        public string EspecieMod { get; set; }

        public List<string> Mensagem { get; } = new List<string>(2);

        public List<string> Detalhamento { get; } = new List<string>(3);

        public List<string> Informativo { get; } = new List<string>(4);

        public string Instrucao1 { get; set; }

        public string Instrucao2 { get; set; }

        public int Parcela { get; set; }

        public int TotalParcelas { get; set; }

        public decimal ValorAbatimento { get; set; }

        public decimal ValorDesconto { get; set; }

        public decimal ValorMoraJuros { get; set; }

        public decimal ValorIOF { get; set; }

        public decimal ValorOutrasDespesas { get; set; }

        public string SeuNumero { get; set; }

        public decimal PercentualMulta { get; set; }

        public string CodigoMora { get; set; }

        public CodigoJuros CodigoMoraJuros { get; set; }

        public string CodigoGeracao { get; set; }

        public string Competencia { get; set; }

        public string ArquivoLogoEmp { get; set; }

        public bool Verso { get; set; }

        public List<BoletoNotaFiscal> NotaFiscais { get; } = new List<BoletoNotaFiscal>();

        #endregion Properties

        #region Methods

        /// <inheritdoc/>
        public override string ToString() => WriteToIni().ToString();

        private ACBrIniFile WriteToIni()
        {
            var iniFile = new ACBrIniFile();
            WriteToIni(iniFile);
            return iniFile;
        }

        internal void WriteToIni(ACBrIniFile iniFile)
        {
            var sessao = Index > 0 ? $"Titulo{Index}" : "Titulo";

            iniFile.WriteToIni(this, sessao);
            iniFile.WriteToIni(OcorrenciaOriginal, sessao);
            iniFile.WriteToIni(Sacado, sessao);
            if (!string.IsNullOrEmpty(Sacado.Avalista.CNPJCPF) || (Sacado.Avalista.Pessoa == ACBrPessoa.pNenhum) )
                iniFile.WriteToIni(Sacado.Avalista, sessao);

            if (Mensagem.Any())
                iniFile[sessao]["Mensagem"] = string.Join("|", Mensagem.Select(x => Regex.Replace(x, @"\r\n?|\n", "")));

            if (Detalhamento.Any())
                iniFile[sessao]["Detalhamento"] = string.Join("|", Detalhamento.Select(x => Regex.Replace(x, @"\r\n?|\n", "")));

            if (Informativo.Any())
                iniFile[sessao]["Informativo"] = string.Join("|", Informativo.Select(x => Regex.Replace(x, @"\r\n?|\n", "")));

            if (!NotaFiscais.Any()) return;

            var sessaoNfe = Index > 0 ? $"NFe{Index}-" : "NFe";
            for (var i = 0; i < NotaFiscais.Count; i++)
                iniFile.WriteToIni(NotaFiscais[i], $"{sessaoNfe}{i + 1}");
        }

        #endregion Methods

        #region Operators

        public static implicit operator ACBrIniFile(Titulo source) => source.WriteToIni();

        #endregion Operators
    }
}