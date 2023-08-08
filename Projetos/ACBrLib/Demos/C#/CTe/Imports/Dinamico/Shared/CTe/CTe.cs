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
            Tomador3 = new Tomador3CTe();
            ProtCTe = new ProtCTe();
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

        [Obsolete("Descontinuado: Para o correto preenchimento, use Complemento")]
        public EntregaCTe Entrega { get;  }

        public List<ObsContCTe> ObsCont { get; } = new List<ObsContCTe>();

        public List<ObsFiscoCTe> ObsFisco { get; } = new List<ObsFiscoCTe>();

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

        public Tomador3CTe Tomador3 { get; } 

        public ProtCTe ProtCTe { get; }

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
            iniData.WriteToIni(Tomador3, "toma3");
            iniData.WriteToIni(Tomador4, "toma4");
            iniData.WriteToIni(Complemento, "Compl");
            iniData.WriteToIni(Fluxo, "fluxo");
            iniData.WriteToIni(Entrega, "Entrega");
            
            for (var i = 0; i < ObsCont.Count; i++)
            {
                var obsCont = ObsCont[i];
                iniData.WriteToIni(obsCont, $"ObsCont{i + 1:000}");
            }
            
            for (var i = 0; i < ObsFisco.Count; i++)
            {
                var obsFisco = ObsFisco[i];
                iniData.WriteToIni(obsFisco, $"ObsFisco{i + 1:000}");
            }

            iniData.WriteToIni(Emitente, "Emit");
            iniData.WriteToIni(Remetente, "Rem");
            iniData.WriteToIni(Expedidor, "Exped");
            iniData.WriteToIni(Recebedor, "Receb");
            iniData.WriteToIni(Destinatario, "Dest");
            iniData.WriteToIni(ValoresPrestacaoServico, "vPrest");
            iniData.WriteToIni(ComponentesValorPrestacao, "Comp");
            iniData.WriteToIni(InformacoesRelativasImpostos, "Imp");
            if (InformacoesRelativasImpostos.ICMSSN.CST == CSTCTe.ICMSSN)
            {
                iniData.WriteToIni(InformacoesRelativasImpostos.ICMSSN, "ICMSSN");
            }
            if (InformacoesRelativasImpostos.ICMS00.CST == CSTCTe.tributacaoNormal)
            {
                iniData.WriteToIni(InformacoesRelativasImpostos.ICMS00, "ICMS00");
            }
            if (InformacoesRelativasImpostos.ICMS20.CST == CSTCTe.tributacaoBCreduzidaICMS)
            {
                iniData.WriteToIni(InformacoesRelativasImpostos.ICMS20, "ICMS20");
            }
            if ((InformacoesRelativasImpostos.ICMS45.CST & (CSTCTe.isencaoICMS | CSTCTe.ICMSNaoTributada | CSTCTe.ICMSDiferido)) != 0)
            {
                iniData.WriteToIni(InformacoesRelativasImpostos.ICMS45, "ICMS45");
            }
            if (InformacoesRelativasImpostos.ICMS60.CST == CSTCTe.ICMSCobradoSubstituicaoTributaria)
            {
                iniData.WriteToIni(InformacoesRelativasImpostos.ICMS60, "ICMS60");
            }
            if (InformacoesRelativasImpostos.ICMS90.CST == CSTCTe.ICMSOutros)
            {
                iniData.WriteToIni(InformacoesRelativasImpostos.ICMS90, "ICMS90");
            }
            if (InformacoesRelativasImpostos.ICMSOutraUF.CST == CSTCTe.ICMSOutraUF)
            {
                iniData.WriteToIni(InformacoesRelativasImpostos.ICMSOutraUF, "ICMSOutraUF");
            }
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
            iniData.WriteToIni(ProtCTe, "procCTe");
            
            return iniData;
        }

        private void ReadFromIni(ACBrIniFile iniData)
        {
            iniData.ReadFromIni(InfCTe, "InfCTe");
            iniData.ReadFromIni(Identificacao, "Ide");
            iniData.ReadFromIni(Tomador3, "toma3");
            iniData.ReadFromIni(Tomador4, "toma4");
            iniData.ReadFromIni(Complemento, "Compl");
            iniData.ReadFromIni(Fluxo, "fluxo");
            iniData.ReadFromIni(Entrega, "Entrega");

            var i = 0;
            ObsContCTe obsCont;
            do
            {
                i++;
                obsCont = iniData.ReadFromIni<ObsContCTe>($"ObsCont{i:000}");

                if (obsCont != null) ObsCont.Add(obsCont);

            } while (obsCont != null);

            i = 0;
            ObsFiscoCTe obsFisco;
            do
            {
                i++;
                obsFisco = iniData.ReadFromIni<ObsFiscoCTe>($"ObsFisco{i:000}");

                if(obsFisco != null) ObsFisco.Add(obsFisco);

            } while (obsFisco != null);
            
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
            iniData.ReadFromIni(InformacoesRelativasImpostos.ICMSOutraUF, "ICMSOutrasUF");
            iniData.ReadFromIni(InformacoesRelativasImpostos.ICMS00, "ICMS00");
            iniData.ReadFromIni(InformacoesRelativasImpostos.ICMS20, "ICMS20");
            iniData.ReadFromIni(InformacoesRelativasImpostos.ICMS45, "ICMS45");
            iniData.ReadFromIni(InformacoesRelativasImpostos.ICMS60, "ICMS60");
            iniData.ReadFromIni(InformacoesRelativasImpostos.ICMS90, "ICMS90");
            iniData.ReadFromIni(GrupoInformacoesNormalSubstituto, "infCTeNorm");
            iniData.ReadFromIni(GrupoInformacoesNormalSubstituto.infCarga, "infCarga");

            i = 0;
            InfQCTe infQ;
            do
            {
                i++;
                infQ = iniData.ReadFromIni<InfQCTe>($"infQ{i:000}");
                
                if (infQ != null) GrupoInformacoesNormalSubstituto.infCarga.infQ.Add(infQ);

            } while (infQ != null);
            
            iniData.ReadFromIni(Rodoviario, "rodo");
            iniData.ReadFromIni(GrupoInformacoesNormalSubstituto.infModal, "infModal");

            var j = 0;
            InfNFeCTe infNFe;
            do
            {
                j++;
                infNFe = iniData.ReadFromIni<InfNFeCTe>($"infNFe{j:000}");                

                if (infNFe != null) GrupoInformacoesNormalSubstituto.infDoc.infNFe.Add(infNFe);

            } while (infNFe != null);

            iniData.ReadFromIni(DetalhamentoComplementado, "infCteComp");
            iniData.ReadFromIni(DetalhamentoAnulacao, "InfCteAnu");
            iniData.ReadFromIni(InformacoesSuplementares, "infCTeSupl");
            iniData.ReadFromIni(ProtCTe, "procCTe");
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
