using ACBrLib.Core;

namespace ACBrLib.Boleto
{
    public sealed class RetornoSacado
    {
        #region Properties

        [IniKey("Sacado_Nome")]
        public string Nome { get; set; }

        [IniKey("Sacado_CNPJCPF")]
        public string CNPJCPF { get; set; }

        #endregion Properties
    }
}