using ACBrLib.Core.Boleto;

namespace ACBrLib.Boleto
{
    public sealed class RetornoSacadoWeb
    {
        public ACBrPessoa Pessoa { get; set; }

        public string NomeSacado { get; set; }

        public string CNPJCPF { get; set; }

        public string Logradouro { get; set; }

        public string Numero { get; set; }

        public string Complemento { get; set; }

        public string Bairro { get; set; }

        public string Cidade { get; set; }

        public string UF { get; set; }

        public string CEP { get; set; }

        public string Email { get; set; }

        public string Fone { get; set; }
    }
}