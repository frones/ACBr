using ACBrLib.Core;
using ACBrLib.Core.Boleto;

namespace ACBrLib.Boleto
{
    public sealed class Banco : BoletoInfo
    {
        #region Constructors

        public Banco()
        {
        }

        private Banco(ACBrIniFile ini) : base(ini)
        {
        }

        #endregion Constructors

        #region Properties

        public ACBrTipoCobranca TipoCobranca { get; set; }

        public ACBrLayoutRemessa CNAB { get; set; }

        public int NumeroCorrespondente { get; set; }

        public int VersaoArquivo { get; set; }

        public int VersaoLote { get; set; }

        public int CasasDecimaisMoraJuros { get; set; }

        public int Numero { get; set; }

        public string DensidadeGravacao { get; set; }


        #endregion Properties

        #region Methods

        internal override void WriteToIni(ACBrIniFile iniFile) => iniFile.WriteToIni(this, "Banco");

        protected override void ReadFromIni(ACBrIniFile iniData) => iniData.ReadFromIni(this, "Banco");

        #endregion Methods

        #region Operators

        public static implicit operator Banco(ACBrIniFile source) => new Banco(source);

        public static implicit operator ACBrIniFile(Banco source) => source.WriteToIni();

        #endregion Operators
    }
}