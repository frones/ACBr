using ACBrLib.Core;
using ACBrLib.Core.Boleto;

namespace ACBrLib.Boleto
{
    public sealed class SacadoAvalista
    {
        #region Properties

        [IniKey("Sacado.SacadoAvalista.Pessoa")]
        public ACBrPessoa Pessoa { get; set; }

        [IniKey("Sacado.SacadoAvalista.NomeAvalista")]
        public string NomeAvalista { get; set; }

        [IniKey("Sacado.SacadoAvalista.Email")]
        public string Email { get; set; }

        [IniKey("Sacado.SacadoAvalista.CNPJCPF")]
        public string CNPJCPF { get; set; }

        [IniKey("Sacado.SacadoAvalista.Logradouro")]
        public string Logradouro { get; set; }

        [IniKey("Sacado.SacadoAvalista.Numero")]
        public string Numero { get; set; }

        [IniKey("Sacado.SacadoAvalista.Bairro")]
        public string Bairro { get; set; }

        [IniKey("Sacado.SacadoAvalista.Complemento")]
        public string Complemento { get; set; }

        [IniKey("Sacado.SacadoAvalista.Cidade")]
        public string Cidade { get; set; }

        [IniKey("Sacado.SacadoAvalista.UF")]
        public string UF { get; set; }

        [IniKey("Sacado.SacadoAvalista.CEP")]
        public string CEP { get; set; }

        [IniKey("Sacado.SacadoAvalista.InscricaoNr")]
        public string InscricaoNr { get; set; }

        #endregion Properties
    }
}