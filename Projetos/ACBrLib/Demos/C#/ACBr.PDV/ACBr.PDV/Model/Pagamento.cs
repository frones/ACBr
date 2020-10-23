namespace ACBr.PDV.Model
{
    public class Pagamento
    {
        public int TipoNFe { get; set; }

        public int TipoSAT { get; set; }

        public string Descricao { get; set; }

        public decimal Valor { get; set; }
    }
}