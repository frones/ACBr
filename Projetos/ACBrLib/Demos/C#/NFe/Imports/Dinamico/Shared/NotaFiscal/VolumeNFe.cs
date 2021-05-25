using System.Collections.Generic;

namespace ACBrLib.NFe
{
    /// <summary>
    /// Informações do Transporte da NF-e - Volume
    /// </summary>
    public class VolumeNFe
    {
        #region Properties

        /// <summary>
        /// Quantidade de volumes transportados
        /// </summary>
        public int? qVol { get; set; }

        /// <summary>
        /// Espécie dos volumes transportados
        /// </summary>
        public string esp { get; set; }

        /// <summary>
        /// Marca dos volumes transportados
        /// </summary>
        public string Marca { get; set; }

        /// <summary>
        /// Numeração dos volumes transportados
        /// </summary>
        public string nVol { get; set; }

        /// <summary>
        /// Peso Líquido (em kg)
        /// </summary>
        public decimal? pesoL { get; set; }

        /// <summary>
        /// Peso Bruto (em kg)
        /// </summary>
        public decimal? pesoB { get; set; }

        public List<LacresNFe> Lacres { get; } = new List<LacresNFe>();

        #endregion Properties
    }
}