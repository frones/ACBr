using ACBrLib.Core;
using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class InutilizarNFeResposta : DFeRespostaBase
    {
        #region Properties

        public string NomeArquivo { get; set; }

        public string Xml { get; set; }

        public string NProt { get; set; }

        #endregion Properties

        #region Methods

        public static InutilizarNFeResposta LerResposta(string resposta)
        {
            var iniresposta = ACBrIniFile.Parse(resposta);
            var ret = iniresposta.ReadFromIni<InutilizarNFeResposta>("Inutilizacao");
            ret.Resposta = resposta;
            return ret;
        }

        #endregion Methods
    }
}