using ACBrLib.Core;

namespace ACBrLib.Boleto
{
    public sealed class RetornoConta
    {
        #region Properties

        [IniKey("Conta")]
        public string Numero { get; set; }

        [IniKey("DigitoConta")]
        public string Digito { get; set; }

        public string Agencia { get; set; }

        public string DigitoAgencia { get; set; }

        public string DigitoVerificadorAgenciaConta { get; set; }

        #endregion Properties
    }
}