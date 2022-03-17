using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ACBrLib.Core;

namespace ACBrLib.CEP
{
    public sealed class ACBrEndereco
    {
        #region Properties

        public string Tipo_Logradouro { get; set; }

        public string Logradouro { get; set; }

        public string Complemento { get; set; }

        public string Bairro { get; set; }

        public string Municipio { get; set; }

        public string UF { get; set; }

        public string CEP { get; set; }

        public string IBGE_Municipio { get; set; }

        public string IBGE_UF { get; set; }

        [IniIgnore]
        public string Resposta { get; set; }

        #endregion Properties

        #region Methods

        public static ACBrEndereco LerResposta(ACBrIniSection section)
        {
            var ret = new ACBrEndereco();
            section.ReadFromINi(ret);
            ret.Resposta = section.ToString();

            return ret;
        }

        #endregion Methods
    }
}