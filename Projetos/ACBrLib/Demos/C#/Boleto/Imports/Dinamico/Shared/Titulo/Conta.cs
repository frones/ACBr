using ACBrLib.Core;
using ACBrLib.Core.Boleto;

namespace ACBrLib.Boleto
{
    public sealed class Conta : BoletoInfo
    {
        #region Constructors

        public Conta()
        {
        }

        private Conta(ACBrIniFile ini) : base(ini)
        {
        }

        #endregion Constructors

        #region Properties

        [IniKey("Conta")]
        public string Numero { get; set; }

        [IniKey("DigitoConta")]
        public string Digito { get; set; }

        public string Agencia { get; set; }

        public string DigitoAgencia { get; set; }

        public string DigitoVerificadorAgenciaConta { get; set; }

        #endregion Properties

        #region Methods

        internal override void WriteToIni(ACBrIniFile iniFile) => iniFile.WriteToIni(this, "Conta");

        protected override void ReadFromIni(ACBrIniFile iniData) => iniData.ReadFromIni(this, "Conta");

        #endregion Methods

        #region Operators

        public static implicit operator Conta(ACBrIniFile source) => new Conta(source);

        public static implicit operator ACBrIniFile(Conta source) => source.WriteToIni();

        #endregion Operators
    }
}