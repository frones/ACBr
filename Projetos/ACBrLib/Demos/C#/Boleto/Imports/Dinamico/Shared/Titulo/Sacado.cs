using ACBrLib.Core;
using ACBrLib.Core.Boleto;

namespace ACBrLib.Boleto
{
    public sealed class Sacado
    {
        #region Properties

        public SacadoAvalista Avalista { get; } = new SacadoAvalista();

        [IniKey("Sacado.Pessoa")]
        public ACBrPessoa Pessoa { get; set; }

        [IniKey("Sacado.NomeSacado")]
        public string NomeSacado { get; set; }

        [IniKey("Sacado.Email")]
        public string Email { get; set; }

        [IniKey("Sacado.CNPJCPF")]
        public string CNPJCPF { get; set; }

        [IniKey("Sacado.Logradouro")]
        public string Logradouro { get; set; }

        [IniKey("Sacado.Numero")]
        public string Numero { get; set; }

        [IniKey("Sacado.Bairro")]
        public string Bairro { get; set; }

        [IniKey("Sacado.Complemento")]
        public string Complemento { get; set; }

        [IniKey("Sacado.Cidade")]
        public string Cidade { get; set; }

        [IniKey("Sacado.UF")]
        public string UF { get; set; }

        [IniKey("Sacado.CEP")]
        public string CEP { get; set; }

        #endregion Properties
    }
}