using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ACBrLib.Core;
using ACBrLib.Core.CTe;
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
            Tomador4 = new Tomador4CTe();
            Complemento = new ComplementoCTe();
            Fluxo = new FluxoCTe();
            Entrega = new EntregaCTe();
            ObsCont = new ObsContCTe();
            ObsFisco = new ObsFiscoCTe();
            Emitente = new EmitenteCTe();
            Remetente = new RemetenteCTe();
            Expedidor = new ExpedidorCTe();
            Recebedor = new RecebedorCTe();
            Destinatario = new DestinatarioCTe();
            ValoresPrestacaoServico = new ValoresPrestacaoServicoCTe();
            ComponentesValorPrestacao = new ComponentesValorPrestacaoCTe();
            InformacoesRelativasImpostos = new InformacoesRelativasImpostosCTe();
            ICMS = new ICMSCTe();
            ICMSUFFim = new ICMSUFFimCTe();
            GrupoInformacoesNormalSubstituto = new GrupoInformacoesNormalSubstitutoCTe();
            DetalhamentoComplementado = new DetalhamentoComplementadoCTe();
            DetalhamentoAnulacao = new DetalhamentoAnulacaoCTe();
            InformacoesSuplementares = new InformacoesSuplementaresCTe();
            Rodoviario = new RodoviarioCTe();
        }

        internal CTe(ACBrIniFile ini) : this()
        {
            ReadFromIni(ini);
        }

        #endregion Constructor

        #region Properties

        public InfCTe InfCTe { get; }

        public IdentificacaoCTe Identificacao { get; }

        public Tomador4CTe Tomador4 { get;  }

        public ComplementoCTe Complemento { get; }

        public FluxoCTe Fluxo { get; }

        public EntregaCTe Entrega { get;  }

        public ObsContCTe ObsCont { get; }

        public ObsFiscoCTe ObsFisco { get; }

        public EmitenteCTe Emitente { get; }

        public RemetenteCTe Remetente { get; }

        public ExpedidorCTe Expedidor { get; }

        public RecebedorCTe Recebedor { get; }

        public DestinatarioCTe Destinatario { get; }

        public ValoresPrestacaoServicoCTe ValoresPrestacaoServico { get; }

        public ComponentesValorPrestacaoCTe ComponentesValorPrestacao { get; }

        public InformacoesRelativasImpostosCTe InformacoesRelativasImpostos { get; }

        public ICMSCTe ICMS { get; }

        public ICMSUFFimCTe ICMSUFFim { get; }

        public GrupoInformacoesNormalSubstitutoCTe GrupoInformacoesNormalSubstituto { get; }

        public DetalhamentoComplementadoCTe DetalhamentoComplementado { get; }

        public DetalhamentoAnulacaoCTe DetalhamentoAnulacao { get; }

        public InformacoesSuplementaresCTe InformacoesSuplementares { get; }

        public RodoviarioCTe Rodoviario { get; }

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
            iniData.WriteToIni(Identificacao, "Ide");
            iniData.WriteToIni(Tomador4, "toma4");
            iniData.WriteToIni(Complemento, "Compl");
            iniData.WriteToIni(Fluxo, "fluxo");
            iniData.WriteToIni(Entrega, "Entrega");
            iniData.WriteToIni(ObsCont, "ObsCont");
            iniData.WriteToIni(ObsFisco, "ObsFisco");
            iniData.WriteToIni(Emitente, "Emit");
            iniData.WriteToIni(Remetente, "Rem");
            iniData.WriteToIni(Expedidor, "Exped");
            iniData.WriteToIni(Recebedor, "Receb");
            iniData.WriteToIni(Destinatario, "Dest");
            iniData.WriteToIni(ValoresPrestacaoServico, "vPrest");
            iniData.WriteToIni(ComponentesValorPrestacao, "Comp");
            iniData.WriteToIni(InformacoesRelativasImpostos, "Imp");
            iniData.WriteToIni(ICMS, "ICMS");
            iniData.WriteToIni(ICMSUFFim, "ICMSUFFim");
            iniData.WriteToIni(GrupoInformacoesNormalSubstituto, "infCTeNorm");
            iniData.WriteToIni(GrupoInformacoesNormalSubstituto.infCarga, "infCarga");
            iniData.WriteToIni(GrupoInformacoesNormalSubstituto.infDoc, "infDoc");
            iniData.WriteToIni(GrupoInformacoesNormalSubstituto.docAnt, "docAnt");
            iniData.WriteToIni(GrupoInformacoesNormalSubstituto.infModal, "infModal");
            iniData.WriteToIni(GrupoInformacoesNormalSubstituto.veicNovos, "veicNovos");
            iniData.WriteToIni(GrupoInformacoesNormalSubstituto.cobr, "cobr");
            iniData.WriteToIni(GrupoInformacoesNormalSubstituto.infCTeSub, "infCTeSub");
            iniData.WriteToIni(GrupoInformacoesNormalSubstituto.infGlobalizado, "infGlobalizado");
            iniData.WriteToIni(GrupoInformacoesNormalSubstituto.infServVinc, "infServVinc");
            
            for (var i = 0; i < GrupoInformacoesNormalSubstituto.infDoc.infNFe.Count; i++)
            {
                var chaveNFe = GrupoInformacoesNormalSubstituto.infDoc.infNFe[i];
                iniData.WriteToIni(chaveNFe, $"infNFe{i + 1:000}");
            }

            iniData.WriteToIni(DetalhamentoComplementado, "infCteComp");
            iniData.WriteToIni(DetalhamentoAnulacao, "InfCteAnu");
            iniData.WriteToIni(InformacoesSuplementares, "infCTeSupl");
            iniData.WriteToIni(Rodoviario, "rodo");

            return iniData;
        }

        private void ReadFromIni(ACBrIniFile iniData)
        {
            iniData.ReadFromIni(InfCTe, "InfCTe");
            iniData.ReadFromIni(Identificacao, "Ide");
            iniData.ReadFromIni(Tomador4, "toma4");
            iniData.ReadFromIni(Complemento, "Compl");
            iniData.ReadFromIni(Fluxo, "fluxo");
            iniData.ReadFromIni(Entrega, "Entrega");
            iniData.ReadFromIni(ObsCont, "ObsCont");
            iniData.ReadFromIni(ObsFisco, "ObsFisco");
            iniData.ReadFromIni(Emitente, "Emit");
            iniData.ReadFromIni(Remetente, "Rem");
            iniData.ReadFromIni(Expedidor, "Exped");
            iniData.ReadFromIni(Recebedor, "Receb");
            iniData.ReadFromIni(Destinatario, "Dest");
            iniData.ReadFromIni(ValoresPrestacaoServico, "vPrest");
            iniData.ReadFromIni(ComponentesValorPrestacao, "Comp");
            iniData.ReadFromIni(InformacoesRelativasImpostos, "Imp");
            iniData.ReadFromIni(ICMS, "ICMS");
            iniData.ReadFromIni(ICMSUFFim, "ICMSUFFim");
            iniData.ReadFromIni(GrupoInformacoesNormalSubstituto, "infCTeNorm");
            iniData.ReadFromIni(GrupoInformacoesNormalSubstituto.infCarga, "infCarga");
            iniData.ReadFromIni(GrupoInformacoesNormalSubstituto.infDoc, "infDoc");
            iniData.ReadFromIni(GrupoInformacoesNormalSubstituto.docAnt, "docAnt");
            iniData.ReadFromIni(GrupoInformacoesNormalSubstituto.infModal, "infModal");
            iniData.ReadFromIni(GrupoInformacoesNormalSubstituto.veicNovos, "veicNovos");
            iniData.ReadFromIni(GrupoInformacoesNormalSubstituto.cobr, "cobr");
            iniData.ReadFromIni(GrupoInformacoesNormalSubstituto.infCTeSub, "infCTeSub");
            iniData.ReadFromIni(GrupoInformacoesNormalSubstituto.infGlobalizado, "infGlobalizado");
            iniData.ReadFromIni(GrupoInformacoesNormalSubstituto.infServVinc, "infServVinc");

            var i = 0;
            InfNFeCTe infNFe;
            do
            {
                i++;
                infNFe = iniData.ReadFromIni<InfNFeCTe>($"chave{i:000}");
                if (infNFe != null) continue;

                GrupoInformacoesNormalSubstituto.infDoc.infNFe.Add(infNFe);

            } while (infNFe != null);

            iniData.ReadFromIni(DetalhamentoComplementado, "infCteComp");
            iniData.ReadFromIni(DetalhamentoAnulacao, "InfCteAnu");
            iniData.ReadFromIni(InformacoesSuplementares, "infCTeSupl");
            iniData.ReadFromIni(Rodoviario, "rodo");
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
