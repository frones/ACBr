using ACBrLib.NFe;

namespace ACBr.PDV.Model
{
    public class Pagamento
    {
        public FormaPagamento TipoNFe { get; set; }

        public int TipoSAT { get; set; }

        public string Descricao { get; set; }

        public decimal Valor { get; set; }
    }
}