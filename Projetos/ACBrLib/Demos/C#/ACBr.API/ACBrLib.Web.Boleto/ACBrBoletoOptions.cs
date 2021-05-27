namespace ACBrLib.Web.Boleto
{
    public class ACBrBoletoOptions
    {
        public bool UseMemory { get; set; } = true;

        public string ConfigName { get; set; } = "ACBrLib.ini";

        public string Senha { get; set; } = "";
    }
}