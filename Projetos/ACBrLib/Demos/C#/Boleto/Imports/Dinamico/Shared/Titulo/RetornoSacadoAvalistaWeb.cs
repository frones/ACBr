using ACBrLib.Core.Boleto;

namespace ACBrLib.Boleto
{
    public sealed class RetornoSacadoAvalistaWeb
    {
        public ACBrPessoa Pessoa { get; set; }

        public string NomeAvalista { get; set; }

        public string CNPJCPF { get; set; }
    }
}