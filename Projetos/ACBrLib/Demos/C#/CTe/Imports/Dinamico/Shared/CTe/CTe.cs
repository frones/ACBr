using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ACBrLib.Core;
using ACBrLib.Core.DFe;

namespace ACBrLib.CTe
{
    public sealed class CTe
    {
        #region Constructor

        public CTe()
        {
            InfCTe = new InfCTe();
            Identificacao = new IdentificacaoCTe();
            Complemento = new ComplementoCTe();
            Emitente = new EmitenteCTe();
            Remetente = new RemetenteCTe();
            Expedidor = new ExpedidorCTe();
            Recebedor = new RecebedorCTe();
            Destinatario = new DestinatarioCTe();
            ValoresPrestacaoServico = new ValoresPrestacaoServicoCTe();
            InformacoesRelativasImpostos = new InformacoesRelativasImpostosCTe();
            GrupoInformacoesNormalSubstituto = new GrupoInformacoesNormalSubstitutoCTe();
            DetalhamentoComplementado = new DetalhamentoComplementadoCTe();
            DetalhamentoAnulacao = new DetalhamentoAnulacaoCTe();
            InformacoesSuplementares = new InformacoesSuplementaresCTe();
        }

        internal CTe(ACBrIniFile ini) : this()
        {
            ReadFromIni(ini);
        }

        #endregion Constructor

        #region Properties

        public InfCTe InfCTe { get; }

        public IdentificacaoCTe Identificacao { get; }

        public ComplementoCTe Complemento { get; }

        public EmitenteCTe Emitente { get; }

        public RemetenteCTe Remetente { get; }

        public ExpedidorCTe Expedidor { get; }

        public RecebedorCTe Recebedor { get; }

        public DestinatarioCTe Destinatario { get; }

        public ValoresPrestacaoServicoCTe ValoresPrestacaoServico { get; }

        public InformacoesRelativasImpostosCTe InformacoesRelativasImpostos { get; }

        public GrupoInformacoesNormalSubstitutoCTe GrupoInformacoesNormalSubstituto { get; }

        public DetalhamentoComplementadoCTe DetalhamentoComplementado { get; }

        public DetalhamentoAnulacaoCTe DetalhamentoAnulacao { get; }

        public InformacoesSuplementaresCTe InformacoesSuplementares { get; }

        #endregion Properties

        #region Methods

        public override string ToString()
        {
            return WriteToIni().ToString();
        }

        private ACBrIniFile WriteToIni()
        {
            var iniData = new ACBrIniFile();

            iniData.WriteToIni(InfCTe, "InfCTe");
            iniData.WriteToIni(Identificacao, "Identificacao");
            iniData.WriteToIni(Complemento, "Complemento");
            iniData.WriteToIni(Emitente, "Emitente");
            iniData.WriteToIni(Remetente, "Remetente");
            iniData.WriteToIni(Expedidor, "Expedidor");
            iniData.WriteToIni(Recebedor, "Recebedor");
            iniData.WriteToIni(Destinatario, "Destinatario");
            iniData.WriteToIni(ValoresPrestacaoServico, "ValoresPrestacaoServico");
            iniData.WriteToIni(InformacoesRelativasImpostos, "InformacoesRelativasImpostos");
            iniData.WriteToIni(GrupoInformacoesNormalSubstituto, "GrupoInformacoesNormalSubstituto");
            iniData.WriteToIni(DetalhamentoComplementado, "DetalhamentoComplementado");
            iniData.WriteToIni(DetalhamentoAnulacao, "DetalhamentoAnulacao");
            iniData.WriteToIni(InformacoesSuplementares, "InformacoesSuplementares");

            return iniData;
        }

        private void ReadFromIni(ACBrIniFile iniData)
        {
            iniData.ReadFromIni(InfCTe, "InfCTe");
            iniData.ReadFromIni(Identificacao, "Identificacao");
            iniData.ReadFromIni(Complemento, "Complemento");
            iniData.ReadFromIni(Emitente, "Emitente");
            iniData.ReadFromIni(Remetente, "Remetente");
            iniData.ReadFromIni(Expedidor, "Expedidor");
            iniData.ReadFromIni(Recebedor, "Recebedor");
            iniData.ReadFromIni(Destinatario, "Destinatario");
            iniData.ReadFromIni(ValoresPrestacaoServico, "ValoresPrestacaoServico");
            iniData.ReadFromIni(InformacoesRelativasImpostos, "InformacoesRelativasImpostos");
            iniData.ReadFromIni(GrupoInformacoesNormalSubstituto, "GrupoInformacoesNormalSubstituto");
            iniData.ReadFromIni(DetalhamentoComplementado, "DetalhamentoComplementado");
            iniData.ReadFromIni(DetalhamentoAnulacao, "DetalhamentoAnulacao");
            iniData.ReadFromIni(InformacoesSuplementares, "InformacoesSuplementares");
        }

        public static CTe Load(string conteudo) => ACBrIniFile.Parse(conteudo);

        public static CTe LoadFromFile(string filepath) => ACBrIniFile.Load(filepath);

        public static CTe LoadFromFile(Stream stream) => ACBrIniFile.Load(stream);

        #endregion Methods

        #region Operators

        public static implicit operator CTe(ACBrIniFile source) => new CTe(source);

        public static implicit operator ACBrIniFile(CTe source) => source.WriteToIni();

        #endregion Operators
    }
}
