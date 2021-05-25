using System;

namespace ACBrLib.Core.DFe
{
    public sealed class ConsultaCadastroItemResposta
    {
        #region Properties

        public string arquivo { get; set; }

        public string IE { get; set; }

        public string CNPJ { get; set; }

        public string CPF { get; set; }

        public string UF { get; set; }

        public int cSit { get; set; }

        public string xNome { get; set; }

        public string xFant { get; set; }

        public string xRegApur { get; set; }

        public int CNAE { get; set; }

        public DateTime dIniAtiv { get; set; }

        public DateTime dUltSit { get; set; }

        public DateTime dBaixa { get; set; }

        public string IEUnica { get; set; }

        public string IEAtual { get; set; }

        public string xLgr { get; set; }

        public string nro { get; set; }

        public string xCpl { get; set; }

        public string xBairro { get; set; }

        public int cMun { get; set; }

        public string xMun { get; set; }

        public string CEP { get; set; }

        #endregion Properties
    }
}