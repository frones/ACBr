using ACBrLib.Core;
using ACBrLib.Core.DFe;

namespace ACBrLib.MDFe
{
    public sealed class EncerramentoResposta : DFeRespostaBase
    {
        #region Properties

        public string ChMDFe { get; set; }

        public string NProt { get; set; }

        public TipoEventoMDFe TpEvento {get; set; }

        public string XEvento { get; set; }

        public int NSeqEvento { get; set; }

        public string CNPJDest { get; set; }

        public string EmailDest { get; set; }

        public string CNPJCPF { get; set; }

        public string XML { get; set; }

        public string Arquivo { get; set; }

        #endregion Properties

        #region Methods

        public static EncerramentoResposta LerResposta(string resposta)
        {
            var iniresposta = ACBrIniFile.Parse(resposta);
            var ret = iniresposta.ReadFromIni<EncerramentoResposta>("Encerramento");
            ret.Resposta = resposta;

            return ret;
        }

        #endregion Methods
    }
}