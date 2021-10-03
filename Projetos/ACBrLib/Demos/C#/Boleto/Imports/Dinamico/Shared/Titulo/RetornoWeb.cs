using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using ACBrLib.Core;

namespace ACBrLib.Boleto
{
    public sealed class RetornoWeb
    {
        #region Properties

        public List<RetornoRegistroWeb> Registros { get; } = new List<RetornoRegistroWeb>();

        public string Retorno { get; set; }

        #endregion Properties

        #region Methods

        public static RetornoWeb LerRetorno(string retorno)
        {
            var ret = new RetornoWeb
            {
                Retorno = retorno
            };

            var iniFile = ACBrIniFile.Parse(retorno);
            foreach (var section in iniFile.Where(x => x.Name.StartsWith("REGISTRO")))
            {
                var id = Regex.Replace(section.Name, "[^0-9]", string.Empty);
                var item = section.ReadFromINi<RetornoRegistroWeb>();
                iniFile.ReadFromIni(item.Titulo, $"TITULORETORNO{id}");
                iniFile.ReadFromIni(item.Titulo.Sacado, $"Sacado{id}");
                iniFile.ReadFromIni(item.Titulo.SacadoAvalista, $"SacadoAvalista{id}");

                foreach (var sec in iniFile.Where(x => x.Name.StartsWith($"REJEICAO{id}-")))
                {
                    var rejeicao = sec.ReadFromINi<RetornoRejeicaoWeb>();
                    item.Rejeicoes.Add(rejeicao);
                }

                ret.Registros.Add(item);
            }

            return ret;
        }

        #endregion Methods
    }
}