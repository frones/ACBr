using ACBrLib.NFe;
using ACBrLib.Sat;

namespace ACBr.PDV.Model
{
    public class Pagamento
    {
        public FormaPagamento TipoNFe { get; set; }

        public CodigoMP TipoSAT { get; set; }

        public string Descricao { get; set; }

        public decimal Valor { get; set; }
    }
}