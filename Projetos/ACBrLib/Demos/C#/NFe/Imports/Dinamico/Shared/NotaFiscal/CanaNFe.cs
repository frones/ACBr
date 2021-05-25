using System.Collections.Generic;

namespace ACBrLib.NFe
{
    public class CanaNFe
    {
        public string safra { get; set; }

        public string @ref { get; set; }

        public decimal qTotMes { get; set; }

        public decimal qTotAnt { get; set; }

        public decimal qTotGer { get; set; }

        public decimal vFor { get; set; }

        public decimal vTotDed { get; set; }

        public decimal vLiqFor { get; set; }

        public List<ForDiaNFe> forDia { get; } = new List<ForDiaNFe>();

        public List<DeducNFe> deduc { get; } = new List<DeducNFe>();
    }
}