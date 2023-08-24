using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.Core.CTe
{
    public enum TipoServicoCTe
    {
        tsNormal = 0,
        tsSubcontratacao = 1,
        tsRedespacho = 2,
        tsRedespachoIntermediario = 3,
        tsServicoVinculadoMultimodal = 4,
        tsTransportePessoas = 6,
        tsTransporteValores = 7,
        tsExcessoBagagem = 8,
        tsGTV = 9
    }
}
