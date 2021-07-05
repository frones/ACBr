using System.Linq;
using ACBrLib.Core;
using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class EnvioRetornoResposta
    {
        #region Properties

        public EnvioResposta Envio { get; set; }

        public RetornoResposta Retorno { get; set; }

        public string Resposta { get; private set; }

        #endregion Properties

        #region Methods

        public static EnvioRetornoResposta LerResposta(string resposta)
        {
            var iniresposta = ACBrIniFile.Parse(resposta);
            var ret = new EnvioRetornoResposta
            {
                Envio = iniresposta.ReadFromIni<EnvioResposta>("Envio"),
                Retorno = iniresposta.ReadFromIni<RetornoResposta>("Retorno"),
                Resposta = resposta
            };

            var sections = iniresposta.Where(x => x.Name.StartsWith("NFe"));
            foreach (var section in sections)
            {
                var item = new RetornoItemResposta();
                section.ReadFromINi(item);
                ret.Retorno.Items.Add(item);
            }

            return ret;
        }

        #endregion Methods
    }
}