using System.ComponentModel;

namespace ACBrLib.Core.Boleto
{
    public enum TipoChavePIX
    {
        [Description("Nenhuma")]
        tchNenhuma = 0,

        [Description("Email")]
        tchEmail = 1,

        [Description("CPF")]
        tchCPF = 2,

        [Description("CNPJ")]
        tchCNPJ = 3,

        [Description("Celular")]
        tchCelular = 4,

        [Description("Aleatoria")]
        tchAleatoria = 5,
    }
}