using ACBrLib.Core;
using ACBrLib.Core.DFe;

namespace ACBrLib.MDFe
{
    public sealed class ConsultaMDFeResposta : DFeRespostaBase
    {
        #region Properties

        public string ChMDFe { get; set; }

        public string NProt { get; set; }

        public string DigVal { get; set; }

        #endregion Properties

        #region Methods

        public static ConsultaMDFeResposta LerResposta(string resposta)
        {
            var iniresposta = ACBrIniFile.Parse(resposta);
            var ret = iniresposta.ReadFromIni<ConsultaMDFeResposta>("Consulta");
            ret.Resposta = resposta;

            return ret;
        }

        #endregion Methods
    }
}