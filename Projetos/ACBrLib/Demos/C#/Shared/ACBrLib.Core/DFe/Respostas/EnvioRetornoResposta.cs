using System.Linq;
using System.Text.RegularExpressions;

namespace ACBrLib.Core.DFe
{
    public sealed class EnvioRetornoResposta
    {
        #region Properties

        public EnvioResposta Envio { get; set; }

        public RetornoResposta Retorno { get; set; }

        public string Resposta { get; private set; }

        #endregion Properties

        #region Methods

        public static EnvioRetornoResposta LerResposta(string resposta, string prefix)
        {
            var iniresposta = ACBrIniFile.Parse(resposta);
            var ret = new EnvioRetornoResposta
            {
                Envio = iniresposta.ReadFromIni<EnvioResposta>("Envio"),
                Retorno = iniresposta.ReadFromIni<RetornoResposta>("Retorno"),
                Resposta = resposta
            };

            if (ret.Retorno == null)
            {
                var section = iniresposta.SingleOrDefault(x => x.Name.StartsWith(prefix));
                if (section == null) return ret;

                var item = section.ReadFromINi<RetornoItemResposta>();
                ret.Envio.ItemResposta = item;
            }
            else
            {
                var sections = iniresposta.Where(x => x.Name.StartsWith(prefix));
                  
                foreach (var section in sections)
                {               
                    var item = section.ReadFromINi<RetornoItemResposta>();
                    item.Numero = Regex.Replace(section.Name, "[^0-9]", string.Empty);
                    ret.Retorno.Items.Add(item);
                }
            }

            return ret;
        }

        #endregion Methods
    }
}