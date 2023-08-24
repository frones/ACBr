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
            DetalhamentoAnulacao = new DetalhamentoAnulacaoCTe();
            InformacoesSuplementares = new InformacoesSuplementaresCTe();
            Rodoviario = new RodoviarioCTe();
            Tomador3 = new Tomador3CTe();
            ProtCTe = new ProtCTe();
            Tomador = new TomadorCTeOS();
            InfRespTec = new InfRespTec();
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

        public List<DetalhamentoComplementadoCTe> DetalhamentoComplementado { get; } = new List<DetalhamentoComplementadoCTe>();

        public DetalhamentoAnulacaoCTe DetalhamentoAnulacao { get; }

        public InformacoesSuplementaresCTe InformacoesSuplementares { get; }

        public RodoviarioCTe Rodoviario { get; }

        public Tomador3CTe Tomador3 { get; } 

        public ProtCTe ProtCTe { get; }

        public TomadorCTeOS Tomador { get; }

        public InfRespTec InfRespTec { get; }

        public List<AutXML> autXML { get; } = new List<AutXML>();

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

            for (var i = 0; i < Identificacao.infPercurso.Count; i++)
            {
                InfPercursoCTe infPercuso = Identificacao.infPercurso[i];
                iniData.WriteToIni(infPercuso, $"infPercurso{i + 1:000}");
            }

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
            iniData.WriteToIni(InformacoesRelativasImpostos.infTribFed, "InfTribFed");
            iniData.WriteToIni(GrupoInformacoesNormalSubstituto, "infCTeNorm");
            iniData.WriteToIni(GrupoInformacoesNormalSubstituto.infServico, "infServico");

            switch (Identificacao.mod)
            {
                case 57:
                    { 
                        iniData.WriteToIni(Rodoviario, "rodo"); 

                        for(var i=0; i < Rodoviario.occ.Count; i++)
                        {
                            OrdensColetaCTe occ = Rodoviario.occ[i];
                            iniData.WriteToIni(occ, $"occ{i + 1:000}");
                        }
                        
                        break; 
                    }
                case 67:
                    {
                        iniData.WriteToIni(Rodoviario, "rodoOS");
                        iniData.WriteToIni(Rodoviario.veic, "veic001");
                        iniData.WriteToIni(Rodoviario.veic.prop, "prop001");
                        iniData.WriteToIni(Rodoviario.infFretamento, "infFretamento");
                        break;                        
                    }
            }

            for (var i = 0; i < GrupoInformacoesNormalSubstituto.infDocRef.Count; i++)
            {
                InfDocRef infDocRef = GrupoInformacoesNormalSubstituto.infDocRef[i];
                iniData.WriteToIni(infDocRef, $"infDocRef{i + 1:000}");
            }

            for (var i = 0; i < GrupoInformacoesNormalSubstituto.seg.Count; i++)
            {
                Seguro seg = GrupoInformacoesNormalSubstituto.seg[i];
                iniData.WriteToIni(seg, $"seg{i + 1:000}");
            }

            iniData.WriteToIni(GrupoInformacoesNormalSubstituto.infCarga, "infCarga");

            for (var i = 0; i < GrupoInformacoesNormalSubstituto.infCarga.infQ.Count; i++)
            {
                var infQ = GrupoInformacoesNormalSubstituto.infCarga.infQ[i];
                iniData.WriteToIni(infQ, $"infQ{i + 1:000}");

            }

            for (var i = 0; i < GrupoInformacoesNormalSubstituto.docAnt.emiDocAnt.Count; i++)
            {
                var emiDocAnt = GrupoInformacoesNormalSubstituto.docAnt.emiDocAnt[i];
                iniData.WriteToIni(emiDocAnt, $"emiDocAnt{i + 1:000}");

                for (var j = 0; j < emiDocAnt.idDocAntPap.Count; j++)
                {
                    var idDocAntPap = emiDocAnt.idDocAntPap[j];
                    iniData.WriteToIni(idDocAntPap, $"idDocAntPap{i + 1:000}{j + 1:000}");
                }

                for (var j = 0; j < emiDocAnt.idDocAntEle.Count; j++)
                {
                    var idDocAntEle = emiDocAnt.idDocAntEle[j];
                    iniData.WriteToIni(idDocAntEle, $"idDocAntEle{i + 1:000}{j + 1:000}");
                }
            }

            var fat = GrupoInformacoesNormalSubstituto.cobr.fat;
            if (fat.vDesc != 0 || fat.vOrig != 0 || fat.vLiq != 0 || fat.nFat != "")
            {
                iniData.WriteToIni(fat, "cobr");
            }

            for (var i = 0; i < GrupoInformacoesNormalSubstituto.cobr.dup.Count; i++)
            {
                var dup = GrupoInformacoesNormalSubstituto.cobr.dup[i];
                iniData.WriteToIni(dup, $"dup{i + 1:000}");
            }

            for (var i = 0; i < GrupoInformacoesNormalSubstituto.infServVinc.infCTeMultimodal.Count; i++)
            {
                var infCTeMultimodal = GrupoInformacoesNormalSubstituto.infServVinc.infCTeMultimodal[i];
                iniData.WriteToIni(infCTeMultimodal, $"infCTeMultimodal{i + 1:000}");
            }
            
            iniData.WriteToIni(GrupoInformacoesNormalSubstituto.infModal, "infModal");
            
            for (var i = 0; i < GrupoInformacoesNormalSubstituto.infDoc.infNFe.Count; i++)
            {
                var chaveNFe = GrupoInformacoesNormalSubstituto.infDoc.infNFe[i];
                iniData.WriteToIni(chaveNFe, $"infNFe{i + 1:000}");

                for (var j = 0; j < chaveNFe.infUnidCarga.Count; j++)
                {
                    InfUnidCargaCTe infUnidCarga = chaveNFe.infUnidCarga[j];

                    if (iniData.Contains($"infUnidCarga{i + 1:000}{j + 1:000}")) continue;

                    iniData.WriteToIni(infUnidCarga, $"infUnidCarga{i + 1:000}{j + 1:000}");

                    for (var k = 0; k < infUnidCarga.lacUnidCarga.Count; k++)
                    {
                        LacreUnidadeCargaCTe lacUnidCarga = infUnidCarga.lacUnidCarga[k];
                        iniData.WriteToIni(lacUnidCarga, $"lacUnidCarga{i + 1:000}{j + 1:000}{k + 1:000}");
                    }
                }

                for (var j = 0; j < chaveNFe.infUnidTransp.Count; j++)
                {
                    InfUnidTranspCTe infUnidTransp = chaveNFe.infUnidTransp[j];

                    if (iniData.Contains($"infUnidTransp{i + 1:000}{j + 1:000}")) continue;

                    iniData.WriteToIni(infUnidTransp, $"infUnidTransp{i + 1:000}{j + 1:000}");

                    for (var k = 0; k < infUnidTransp.lacUnidTransp.Count ; k++)
                    {
                        LacreUnidadeTranspCTe lacUnidTransp = infUnidTransp.lacUnidTransp[k];
                        iniData.WriteToIni(lacUnidTransp, $"lacUnidTransp{i + 1:000}{j + 1:000}{k + 1:000}");
                    }

                    for (var k = 0; k < infUnidTransp.InfUnidCargaCTe.Count; k++)
                    {
                        InfUnidCargaCTe infUnidCarga = infUnidTransp.InfUnidCargaCTe[k];
                        iniData.WriteToIni(infUnidCarga, $"infUnidCarga{i + 1:000}{j +  1:000}{k + 1:000}");

                        for (var l = 0; l < infUnidCarga.lacUnidCarga.Count; l++)
                        {
                            LacreUnidadeCargaCTe lacUnidCarga = infUnidCarga.lacUnidCarga[l];
                            iniData.WriteToIni(lacUnidCarga, $"lacUnidCarga{i + 1:000}{j + 1:000}{k + 1:000}{l + 1:000}");
                        }
                    }
                }
            }

            for (var i = 0; i < GrupoInformacoesNormalSubstituto.infDoc.infNF.Count; i++)
            {
                InfNFCTe infNF = GrupoInformacoesNormalSubstituto.infDoc.infNF[i];
                iniData.WriteToIni(infNF, $"infNF{i + 1:000}");

                for (var j = 0; j < infNF.infUnidCarga.Count; j++)
                {
                    InfUnidCargaCTe infUnidCarga = infNF.infUnidCarga[j];

                    if (iniData.Contains($"infUnidCarga{i + 1:000}{j + 1:000}")) continue;

                    iniData.WriteToIni(infUnidCarga, $"infUnidCarga{i + 1:000}{j + 1:000}");

                    for (var k = 0; k < infUnidCarga.lacUnidCarga.Count; k++)
                    {
                        LacreUnidadeCargaCTe lacUnidCarga = infUnidCarga.lacUnidCarga[k];
                        iniData.WriteToIni(lacUnidCarga, $"lacUnidCarga{i + 1:000}{j + 1:000}{k + 1:000}");
                    }
                }

                for (var j = 0; j < infNF.infUnidTransp.Count; j++)
                {
                    InfUnidTranspCTe infUnidTransp = infNF.infUnidTransp[j];

                    if (iniData.Contains($"infUnidTransp{i + 1:000}{j + 1:000}")) continue;

                    iniData.WriteToIni(infUnidTransp, $"infUnidTransp{i + 1:000}{j + 1:000}");

                    for (var k = 0; k < infUnidTransp.lacUnidTransp.Count; k++)
                    {
                        LacreUnidadeTranspCTe la = infUnidTransp.lacUnidTransp[k];
                        iniData.WriteToIni(la, $"lacUnidTransp{i + 1:000}{j + 1:000}{k + 1:000}");
                    }
                    
                    for (var k = 0; k < infUnidTransp.InfUnidCargaCTe.Count; k++)
                    {
                        InfUnidCargaCTe infUnidCarga = infUnidTransp.InfUnidCargaCTe[k];
                        iniData.WriteToIni(infUnidCarga, $"infUnidCarga{i + 1:000}{j + 1:000}{k + 1:000}");

                        for (var  l = 0; l < infUnidCarga.lacUnidCarga.Count; l++)
                        {
                            LacreUnidadeCargaCTe lac = infUnidCarga.lacUnidCarga[l];
                            iniData.WriteToIni(lac, $"lacUnidCarga{i + 1:000}{j +1:000}{k +1:000}{l + 1:000}");
                        }
                    }
                }
            }

            for (var i = 0; i < GrupoInformacoesNormalSubstituto.infDoc.infOutros.Count; i++)
            {
                InfOutrosCTe infOutros = GrupoInformacoesNormalSubstituto.infDoc.infOutros[i];

                iniData.WriteToIni(infOutros, $"infOutros{i + 1:000}");

                for (var j = 0; j < infOutros.InfUnidCarga.Count; j++)
                {
                    InfUnidCargaCTe infUnidCarga = infOutros.InfUnidCarga[j];

                    if (iniData.Contains($"infUnidCarga{i + 1:000}{j + 1:000}")) continue;

                    iniData.WriteToIni(infUnidCarga, $"infUnidCarga{i + 1:000}{j +  1:000}");

                    for (var k = 0; k < infUnidCarga.lacUnidCarga.Count; k++)
                    {
                        LacreUnidadeCargaCTe lacUnidCarga = infUnidCarga.lacUnidCarga[k];
                        iniData.WriteToIni(lacUnidCarga, $"lacUnidCarga{i + 1:000}{j + 1:000}{k + 1:000}");
                    }
                }

                for(var j = 0;  j < infOutros.InfUnidTransp.Count; j++)
                {
                    InfUnidTranspCTe infUnidTransp = infOutros.InfUnidTransp[j];

                    if (iniData.Contains($"infUnidTransp{i +1:000}{j + 1:000}")) continue;

                    iniData.WriteToIni(infUnidTransp, $"infUnidTransp{i + 1:000}{j + 1:000}");

                    for (var k = 0; k < infUnidTransp.lacUnidTransp.Count; k++)
                    {
                        LacreUnidadeTranspCTe lacUnidTransp = infUnidTransp.lacUnidTransp[k];
                        iniData.WriteToIni(lacUnidTransp, $"lacUnidTransp{i + 1:000}{j + 1:000}{k + 1:000}");
                    }

                    for (var k = 0; k < infUnidTransp.InfUnidCargaCTe.Count; k++)
                    {
                        InfUnidCargaCTe infUnidCarga = infUnidTransp.InfUnidCargaCTe[k];
                        iniData.WriteToIni(infUnidCarga, $"infUnidCarga{i + 1:000}{j + 1:000}{k + 1:000}");

                        for (var l = 0; l < infUnidCarga.lacUnidCarga.Count; l++)
                        {
                            LacreUnidadeCargaCTe lacUnidCarga = infUnidCarga.lacUnidCarga[l];
                            iniData.WriteToIni(lacUnidCarga, $"lacUnidCarga{i + 1:000}{j + 1:000}{k + 1:000}{l + 1:000}");

                        }

                    }
                }
              
            }

            for (var i = 0; i < DetalhamentoComplementado.Count; i++)
            {
                iniData.WriteToIni(DetalhamentoComplementado[i], $"infCteComp{i + 1:00}");
            }           
            
            iniData.WriteToIni(DetalhamentoAnulacao, "InfCteAnu");
            iniData.WriteToIni(InformacoesSuplementares, "infCTeSupl");

            for (var i = 0; i < autXML.Count; i++)
            {
                iniData.WriteToIni(autXML[i], $"autXML{i + 1:00}");
            }

            iniData.WriteToIni(InfRespTec, "infRespTec");
            
            if (ProtCTe.cStat != 0) iniData.WriteToIni(ProtCTe, "procCTe");

            //CTe-OS
            if (Tomador.CNPJCPF != "") iniData.WriteToIni(Tomador, "toma");

            for (var i = 0; i < GrupoInformacoesNormalSubstituto.infGTVe.Count; i++)
            {
                var infGTVe = GrupoInformacoesNormalSubstituto.infGTVe[i];
                iniData.WriteToIni(infGTVe, $"infGTVe{i + 1:000}");

                for (var j = 0; j < infGTVe.Comp.Count; j++)
                {
                    var Comp = infGTVe.Comp[j];
                    iniData.WriteToIni(Comp, $"infGTVeComp{i + 1:000}{j + 1:000}");
                }
            }
            
            return iniData;
        }

        private void ReadFromIni(ACBrIniFile iniData)
        {
            iniData.ReadFromIni(InfCTe, "InfCTe");
            iniData.ReadFromIni(Identificacao, "Ide");

            var i = 0;
            if (Identificacao != null)
            {                
                InfPercursoCTe infPercurso;
                do
                {
                    i++;
                    infPercurso = iniData.ReadFromIni<InfPercursoCTe>($"infPercurso{i:000}");
                    if (infPercurso != null) Identificacao.infPercurso.Add(infPercurso);
                } while (infPercurso != null);
            };

            iniData.ReadFromIni(Tomador3, "toma3");
            iniData.ReadFromIni(Tomador4, "toma4");
            iniData.ReadFromIni(Complemento, "Compl");
            iniData.ReadFromIni(Fluxo, "fluxo");
            iniData.ReadFromIni(Entrega, "Entrega");

            i = 0;
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
            iniData.ReadFromIni(InformacoesRelativasImpostos.infTribFed, "InfTribFed");
            iniData.ReadFromIni(GrupoInformacoesNormalSubstituto, "infCTeNorm");
            iniData.ReadFromIni(GrupoInformacoesNormalSubstituto.infServico, "infServico");

            switch (Identificacao.mod)
            {
                case 57:
                    {
                        iniData.ReadFromIni(Rodoviario, "rodo");

                        i = 0;
                        OrdensColetaCTe occ;
                        do
                        {
                            i++;
                            occ = iniData.ReadFromIni<OrdensColetaCTe>($"occ{i:000}");

                            if (occ != null) Rodoviario.occ.Add(occ);

                        }while(occ != null);

                        break;
                    }
                case 67:
                    {
                        iniData.ReadFromIni(Rodoviario, "rodoOS");
                        iniData.ReadFromIni(Rodoviario.veic, "veic001");
                        iniData.ReadFromIni(Rodoviario.veic.prop, "prop001");
                        iniData.ReadFromIni(Rodoviario.infFretamento, "infFretamento");
                        break;
                    }
            }

            i = 0;
            InfDocRef infDocRef;
            do
            {
                i++;
                infDocRef = iniData.ReadFromIni<InfDocRef>($"infDocRef{i:000}");

                if (infDocRef != null) GrupoInformacoesNormalSubstituto.infDocRef.Add( infDocRef );
            } while (infDocRef != null);

            i = 0;
            Seguro seg;
            do
            {
                i++;
                seg = iniData.ReadFromIni<Seguro>($"seg{i:000}");

                if (seg != null) GrupoInformacoesNormalSubstituto.seg.Add(seg);
            } while (seg != null);

            iniData.ReadFromIni(GrupoInformacoesNormalSubstituto.infCarga, "infCarga");

            var a = 0;
            EmiDocAntCTe emiDocAnt;
            do
            {
                a++;
                emiDocAnt = iniData.ReadFromIni<EmiDocAntCTe>($"emiDocAnt{a:000}");

                if (emiDocAnt != null)
                {
                    var b = 0;
                    IdDocAntPapCTe idDocAntPap;
                    do
                    {
                        b++;
                        idDocAntPap = iniData.ReadFromIni<IdDocAntPapCTe>($"idDocAntPap{a:000}{b:000}");

                        if (idDocAntPap != null) emiDocAnt.idDocAntPap.Add(idDocAntPap);

                    } while (idDocAntPap!= null);

                    b = 0;
                    IdDocAntEleCTe idDocAntEle;
                    do
                    {
                        b++;
                        idDocAntEle = iniData.ReadFromIni<IdDocAntEleCTe>($"idDocAntEle{a:000}{b:000}");

                        if (idDocAntEle != null) emiDocAnt.idDocAntEle.Add(idDocAntEle);

                    } while (idDocAntEle != null);

                    GrupoInformacoesNormalSubstituto.docAnt.emiDocAnt.Add(emiDocAnt);
                }

            } while (emiDocAnt != null);

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

                if (infNFe != null)
                {
                    int k = 0;
                    InfUnidCargaCTe infUnidCarga;
                    do
                    {
                        k++;
                        infUnidCarga = iniData.ReadFromIni<InfUnidCargaCTe>($"infUnidCarga{j:000}{k:000}");

                        if (infUnidCarga != null)
                        {
                            int l = 0;
                            LacreUnidadeCargaCTe lacUnidCTe;
                            do
                            {
                                l++;
                                lacUnidCTe = iniData.ReadFromIni<LacreUnidadeCargaCTe>($"lacUnidCarga{j:000}{k:000}{l:000}");

                                if (lacUnidCTe != null) infUnidCarga.lacUnidCarga.Add(lacUnidCTe);

                            } while (lacUnidCTe != null);

                            infNFe.infUnidCarga.Add(infUnidCarga);
                        }

                    } while (infUnidCarga != null);

                    k = 0;
                    InfUnidTranspCTe infUnidTransp;
                    do
                    {
                        k++;
                        infUnidTransp = iniData.ReadFromIni<InfUnidTranspCTe>($"infUnidTransp{j:000}{k:000}");

                        if (infUnidTransp != null)
                        {


                            int l = 0;
                            LacreUnidadeTranspCTe lacUnidTransp;
                            do
                            {
                                l++;
                                lacUnidTransp = iniData.ReadFromIni<LacreUnidadeTranspCTe>($"lacUnidTransp{j:000}{k:000}{l:000}");

                                if (lacUnidTransp != null) infUnidTransp.lacUnidTransp.Add(lacUnidTransp);

                            } while (lacUnidTransp != null);

                            l = 0;
                            InfUnidCargaCTe infCargaCTe;
                            do
                            {
                                l++;
                                infCargaCTe = iniData.ReadFromIni<InfUnidCargaCTe>($"infUnidCarga{j:000}{k:000}{l:000}");

                                if (infCargaCTe != null)
                                {
                                    int m = 0;
                                    LacreUnidadeCargaCTe lacCargaCTe;
                                    do
                                    {
                                        m++;
                                        lacCargaCTe = iniData.ReadFromIni<LacreUnidadeCargaCTe>($"lacUnidCarga{j:000}{k:000}{l:000}{m:000}");

                                        if (lacCargaCTe != null) infCargaCTe.lacUnidCarga.Add(lacCargaCTe);

                                    } while (lacCargaCTe != null);

                                    infUnidTransp.InfUnidCargaCTe.Add(infCargaCTe);
                                }

                            } while (infCargaCTe != null);


                            infNFe.infUnidTransp.Add(infUnidTransp);
                        }

                    } while (infUnidTransp != null);

                    GrupoInformacoesNormalSubstituto.infDoc.infNFe.Add(infNFe);
                }


            } while (infNFe != null);

            j = 0;
            InfNFCTe infNF;
            do
            {
                j++;
                infNF = iniData.ReadFromIni<InfNFCTe>($"infNF{j:000}");

                if (infNF != null)
                {

                    int k = 0;
                    InfUnidCargaCTe infUnidCarga;
                    do
                    {
                        k++;
                        infUnidCarga = iniData.ReadFromIni<InfUnidCargaCTe>($"infUnidCarga{j:000}{k:000}");
                        if (infUnidCarga != null)
                        {
                            int l = 0;
                            LacreUnidadeCargaCTe lacUnidCarga;
                            do
                            {
                                l++;
                                lacUnidCarga = iniData.ReadFromIni<LacreUnidadeCargaCTe>($"lacUnidCarga{j:000}{k:000}{l:000}");

                                if (lacUnidCarga != null) infUnidCarga.lacUnidCarga.Add(lacUnidCarga);

                            } while (lacUnidCarga != null);

                            infNF.infUnidCarga.Add(infUnidCarga);
                        }

                    } while (infUnidCarga != null);

                    k = 0;
                    InfUnidTranspCTe infUnidTransp;
                    do
                    {
                        k++;
                        infUnidTransp = iniData.ReadFromIni<InfUnidTranspCTe>($"infUnidTransp{j:000}{k:000}");

                        if (infUnidTransp != null)
                        {
                            int l = 0;
                            LacreUnidadeTranspCTe lacUnidTransp;
                            do
                            {
                                l++;
                                lacUnidTransp = iniData.ReadFromIni<LacreUnidadeTranspCTe>($"lacUnidTransp{j:000}{k:000}{l:000}");

                                if (lacUnidTransp != null) infUnidTransp.lacUnidTransp.Add(lacUnidTransp);

                            } while (lacUnidTransp != null);

                            l = 0;
                            InfUnidCargaCTe infCargaCTe;
                            do
                            {
                                l++;
                                infCargaCTe = iniData.ReadFromIni<InfUnidCargaCTe>($"infUnidCarga{j:000}{k:000}{l:000}");

                                if (infCargaCTe != null)
                                {
                                    int m = 0;
                                    LacreUnidadeCargaCTe lacCargaCTe;
                                    do
                                    {
                                        m++;
                                        lacCargaCTe = iniData.ReadFromIni<LacreUnidadeCargaCTe>($"lacUnidCarga{j:000}{k:000}{l:000}{m:000}");

                                        if (lacCargaCTe != null) infCargaCTe.lacUnidCarga.Add(lacCargaCTe);
                                    } while (lacCargaCTe != null);

                                    infUnidTransp.InfUnidCargaCTe.Add(infCargaCTe);
                                }

                            } while (infCargaCTe != null);

                            infNF.infUnidTransp.Add(infUnidTransp);
                        }

                    } while (infUnidTransp != null);


                    GrupoInformacoesNormalSubstituto.infDoc.infNF.Add(infNF);
                }

            } while (infNF != null);

            j = 0;
            InfOutrosCTe infOutros;
            do
            {
                j++;
                infOutros = iniData.ReadFromIni<InfOutrosCTe>($"infOutros{j:000}");
                if ( infOutros != null)
                {
                    int k = 0;
                    InfUnidCargaCTe infUnidCarga;
                    do
                    {
                        k++;
                        infUnidCarga = iniData.ReadFromIni<InfUnidCargaCTe>($"infUnidCarga{j:000}{k:000}");
                        
                        if (infUnidCarga != null)
                        {

                            int l = 0;
                            LacreUnidadeCargaCTe lacUnidCarga;
                            do
                            {
                                l++;
                                lacUnidCarga = iniData.ReadFromIni<LacreUnidadeCargaCTe>($"lacUnidCarga{j:000}{k:000}{l:000}");

                                if (lacUnidCarga != null) infUnidCarga.lacUnidCarga.Add(lacUnidCarga);

                            } while (lacUnidCarga != null);

                            infOutros.InfUnidCarga.Add(infUnidCarga);
                        }
                    } while (infUnidCarga != null);

                    k = 0;
                    InfUnidTranspCTe infUnidTransp;
                    do
                    {
                        k++;
                        infUnidTransp = iniData.ReadFromIni<InfUnidTranspCTe>($"infUnidTransp{j:000}{k:000}");

                        if(infUnidTransp != null)
                        {
                            int l = 0;
                            LacreUnidadeTranspCTe lacUnidTransp;
                            do
                            {
                                l++;
                                lacUnidTransp = iniData.ReadFromIni<LacreUnidadeTranspCTe>($"lacUnidTransp{j:000}{k:000}{l:000}");

                                if (lacUnidTransp != null) infUnidTransp.lacUnidTransp.Add(lacUnidTransp);
                            }while(lacUnidTransp != null);

                            l = 0;
                            InfUnidCargaCTe infCargaCTe;
                            do
                            {
                                l++;
                                infCargaCTe = iniData.ReadFromIni<InfUnidCargaCTe>($"infUnidCarga{j:000}{k:000}{l:000}");
                                if (infCargaCTe != null)
                                {
                                    int m = 0;
                                    LacreUnidadeCargaCTe lacUnidCarga;
                                    do
                                    {
                                        m++;
                                        lacUnidCarga = iniData.ReadFromIni<LacreUnidadeCargaCTe>($"lacUnidCarga{j:000}{k:000}{l:000}{m:000}");

                                        if (lacUnidCarga != null) infCargaCTe.lacUnidCarga.Add(lacUnidCarga);
                                    }while( lacUnidCarga != null );
                                }

                            }while( infCargaCTe != null);


                            infOutros.InfUnidTransp.Add(infUnidTransp);
                        }

                    } while (infUnidTransp != null);


                    GrupoInformacoesNormalSubstituto.infDoc.infOutros.Add(infOutros);
                }

            } while( infOutros != null );

            iniData.ReadFromIni(GrupoInformacoesNormalSubstituto.cobr.fat, "cobr");
            a = 0;
            DupCTe dup;
            do
            {
                a++;
                dup = iniData.ReadFromIni<DupCTe>($"dup{a:000}");

                if (dup != null) GrupoInformacoesNormalSubstituto.cobr.dup.Add(dup);
            } while(dup != null);

            a = 0;
            InfCTeMultimodalCTe infCTeMultimodal;
            do
            {
                a++;
                infCTeMultimodal = iniData.ReadFromIni<InfCTeMultimodalCTe>($"infCTeMultimodal{a:000}");

                if (infCTeMultimodal != null) GrupoInformacoesNormalSubstituto.infServVinc.infCTeMultimodal.Add(infCTeMultimodal);
            } while (infCTeMultimodal != null);

            a = 0;
            DetalhamentoComplementadoCTe detalhamentoComplementado;
            do
            {
                a++;
                detalhamentoComplementado = iniData.ReadFromIni<DetalhamentoComplementadoCTe>($"infCteComp{a:00}");

                if (detalhamentoComplementado != null) DetalhamentoComplementado.Add(detalhamentoComplementado);
            } while(detalhamentoComplementado!= null);
                        
            iniData.ReadFromIni(DetalhamentoAnulacao, "InfCteAnu");
            iniData.ReadFromIni(InformacoesSuplementares, "infCTeSupl");

            a = 0;
            AutXML autXMLDistDFe;
            do
            {
                a++;
                autXMLDistDFe = iniData.ReadFromIni<AutXML>($"autXML{a:00}");


                if (autXMLDistDFe != null) autXML.Add(autXMLDistDFe);
            }while(autXMLDistDFe != null);

            iniData.ReadFromIni(InfRespTec, "InfRespTec");
            iniData.ReadFromIni(ProtCTe, "procCTe");

            //CTe-OS
            iniData.ReadFromIni(Tomador, "toma");

            a = 0;
            InfGTVe infGTVe;
            do
            {
                a ++;
                infGTVe = iniData.ReadFromIni<InfGTVe>($"infGTVe{a:000}");

                if (infGTVe != null)
                {
                    var b = 0;
                    ComponentesValGTVe compValGTVe;
                    do
                    {
                        b++;
                        compValGTVe = iniData.ReadFromIni<ComponentesValGTVe>($"infGTVeComp{a:000}{b:000}");

                        if (compValGTVe != null) infGTVe.Comp.Add(compValGTVe);

                    } while (compValGTVe != null);

                    GrupoInformacoesNormalSubstituto.infGTVe.Add(infGTVe);
                }

            } while(infGTVe != null);
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
