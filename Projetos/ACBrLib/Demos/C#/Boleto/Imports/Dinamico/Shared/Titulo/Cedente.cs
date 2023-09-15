using ACBrLib.Core;
using ACBrLib.Core.Boleto;

namespace ACBrLib.Boleto
{
    public sealed class Cedente : BoletoInfo
    {
        #region Constructors

        public Cedente()
        {
        }

        private Cedente(ACBrIniFile ini) : base(ini)
        {
        }

        #endregion Constructors

        #region Properties

        public string Nome { get; set; }

        public string CNPJCPF { get; set; }

        public string Logradouro { get; set; }

        public string Numero { get; set; }

        public string Cidade { get; set; }

        public string CEP { get; set; }

        public string Complemento { get; set; }

        public string UF { get; set; }

        public string Telefone { get; set; }

        public string CodigoCedente { get; set; }

        [IniKey("MODALIDADE")]
        public string Modalidade { get; set; }

        [IniKey("CODTRANSMISSAO")]
        public string CodigoTransmissao { get; set; }

        [IniKey("CONVENIO")]
        public string Convenio { get; set; }

        public CaracTitulo CaracTitulo { get; set; }

        public ACBrTipoCarteira TipoCarteira { get; set; }

        public ACBrTipoDocumento TipoDocumento { get; set; }

        [IniKey("LAYOUTBOL")]
        public ACBrBolLayOut LayoutBol { get; set; } = ACBrBolLayOut.lPadrao;

        public ACBrResponEmissao RespEmis { get; set; } = ACBrResponEmissao.tbCliEmite;

        public ACBrPessoa TipoPessoa { get; set; } = ACBrPessoa.pJuridica;

        #endregion Properties

        #region Methods

        internal override void WriteToIni(ACBrIniFile iniFile) => iniFile.WriteToIni(this, "Cedente");

        protected override void ReadFromIni(ACBrIniFile iniData) => iniData.ReadFromIni(this, "Cedente");

        #endregion Methods

        #region Operators

        public static implicit operator Cedente(ACBrIniFile source) => new Cedente(source);

        public static implicit operator ACBrIniFile(Cedente source) => source.WriteToIni();

        #endregion Operators
    }
}