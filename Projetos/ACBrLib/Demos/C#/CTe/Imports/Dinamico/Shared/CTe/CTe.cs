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
            iniData.WriteToIni(InformacoesRelativasImpostos.ICMSSN, "ICMSSN");
            iniData.WriteToIni(GrupoInformacoesNormalSubstituto, "infCTeNorm");
            iniData.WriteToIni(GrupoInformacoesNormalSubstituto.infCarga, "infCarga");

            for (var i = 0; i < GrupoInformacoesNormalSubstituto.infCarga.infQ.Count; i++)
            {
                var infQ = GrupoInformacoesNormalSubstituto.infCarga.infQ[i];
                iniData.WriteToIni(infQ, $"infQ{i + 1:000}");

            }

            iniData.WriteToIni(Rodoviario, "rodo");
            iniData.WriteToIni(GrupoInformacoesNormalSubstituto.infModal, "infModal");
            
            for (var i = 0; i < GrupoInformacoesNormalSubstituto.infDoc.infNFe.Count; i++)
            {
                var chaveNFe = GrupoInformacoesNormalSubstituto.infDoc.infNFe[i];
                iniData.WriteToIni(chaveNFe, $"infNFe{i + 1:000}");
            }

            iniData.WriteToIni(DetalhamentoComplementado, "infCteComp");
            iniData.WriteToIni(DetalhamentoAnulacao, "InfCteAnu");
            iniData.WriteToIni(InformacoesSuplementares, "infCTeSupl");
            
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
            iniData.ReadFromIni(InformacoesRelativasImpostos.ICMSSN, "ICMSSN");
            iniData.ReadFromIni(GrupoInformacoesNormalSubstituto, "infCTeNorm");
            iniData.ReadFromIni(GrupoInformacoesNormalSubstituto.infCarga, "infCarga");

            var i = 0;
            InfQCTe infQ;
            do
            {
                i++;
                infQ = iniData.ReadFromIni<InfQCTe>($"infQ{i:000}");
                if (infQ != null) continue;

                GrupoInformacoesNormalSubstituto.infCarga.infQ.Add(infQ);

            } while (infQ != null);
            
            iniData.ReadFromIni(Rodoviario, "rodo");
            iniData.ReadFromIni(GrupoInformacoesNormalSubstituto.infModal, "infModal");

            var j = 0;
            InfNFeCTe infNFe;
            do
            {
                j++;
                infNFe = iniData.ReadFromIni<InfNFeCTe>($"chave{j:000}");
                if (infNFe != null) continue;

                GrupoInformacoesNormalSubstituto.infDoc.infNFe.Add(infNFe);

            } while (infNFe != null);

            iniData.ReadFromIni(DetalhamentoComplementado, "infCteComp");
            iniData.ReadFromIni(DetalhamentoAnulacao, "InfCteAnu");
            iniData.ReadFromIni(InformacoesSuplementares, "infCTeSupl");
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
