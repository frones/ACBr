using System;
using System.Collections.Generic;
using System.IO;
using ACBrLib.Core;
using ACBrLib.Core.DFe;

namespace ACBrLib.MDFe
{
    public sealed class Manifesto
    {
        #region Constructors

        /// <summary>
        /// Inicializa uma nova instancia da classe <see cref="Manifesto"/>.
        /// </summary>
        public Manifesto()
        {
        }

        internal Manifesto(ACBrIniFile ini)
        {
            ReadFromIni(ini);
        }

        #endregion Constructors

        #region Properties

        public ProcMDFe ProcMDFe { get; } = new ProcMDFe();

        public InfMDFe InfMDFe { get; } = new InfMDFe();

        public IdeMDFe Identificacao { get; } = new IdeMDFe();

        public EmitMDFe Emitente { get; } = new EmitMDFe();

        public List<InfMunCarregaMDFe> InfMunCarrega { get; } = new List<InfMunCarregaMDFe>();

        public List<InfPercurso> InfPercurso { get; } = new List<InfPercurso>();

        public IModalMDFe Modal { get; set; }

        public InfDocMDFe InfDoc { get; } = new InfDocMDFe();

        public List<SeguroMDFe> Seg { get; } = new List<SeguroMDFe>();

        public ProdPredMDFe ProdPred { get; } = new ProdPredMDFe();

        public TotalMDFe Tot { get; } = new TotalMDFe();

        public List<LacreMDFe> Lacres { get; } = new List<LacreMDFe>();

        public List<AutXML> AutXml { get; } = new List<AutXML>();

        public DadosAdicionais DadosAdicionais { get; } = new DadosAdicionais();

        public InfRespTec InfRespTec { get; } = new InfRespTec();

        #endregion Properties

        #region Methods

        /// <inheritdoc/>
        public override string ToString()
        {
            return WriteToIni().ToString();
        }

        private ACBrIniFile WriteToIni()
        {
            var iniData = new ACBrIniFile();

            if (!string.IsNullOrEmpty(ProcMDFe.nProt))
                iniData.WriteToIni(ProcMDFe, "procMDFe");

            iniData.WriteToIni(InfMDFe, "infMDFe");
            iniData.WriteToIni(Identificacao, "ide");

            for (var i = 0; i < InfMunCarrega.Count; i++)
                iniData.WriteToIni(InfMunCarrega[i], $"CARR{i + 1:000}");

            for (var i = 0; i < InfPercurso.Count; i++)
                iniData.WriteToIni(InfPercurso[i], $"PERC{i + 1:000}");

            iniData.WriteToIni(Emitente, "emit");

            switch (Modal)
            {
                case ModalRodoMDFe modal:
                    iniData.WriteToIni(modal, "Rodo");
                    iniData.WriteToIni(modal.InfANTT, "infANTT");

                    for (var i = 0; i < modal.InfANTT.InfCIOT.Count; i++)
                        iniData.WriteToIni(modal.InfANTT.InfCIOT[i], $"infCIOT{i + 1:000}");

                    iniData.WriteToIni(modal.InfANTT.valePed, "valePed");

                    for (var i = 0; i < modal.InfANTT.valePed.disp.Count; i++)
                        iniData.WriteToIni(modal.InfANTT.valePed.disp[i], $"disp{i + 1:000}");

                    for (var i = 0; i < modal.InfANTT.InfContratante.Count; i++)
                        iniData.WriteToIni(modal.InfANTT.InfContratante[i], $"infContratante{i + 1:000}");

                    for (var i = 0; i < modal.InfANTT.InfPag.Count; i++)
                    {
                        iniData.WriteToIni(modal.InfANTT.InfPag[i], $"infPag{i + 1:000}");
                        iniData.WriteToIni(modal.InfANTT.InfPag[i].InfBanco, $"infBanc{i + 1:000}");

                        for (var j = 0; j < modal.InfANTT.InfPag[i].Comp.Count; j++)
                            iniData.WriteToIni(modal.InfANTT.InfPag[i].Comp[j], $"Comp{i + 1:000}{j + 1:000}");

                        for (var j = 0; j < modal.InfANTT.InfPag[i].InfPrazo.Count; j++)
                            iniData.WriteToIni(modal.InfANTT.InfPag[i].InfPrazo[j], $"infPrazo{i + 1:000}{j + 1:000}");
                    }

                    iniData.WriteToIni(modal.VeicTracao, "veicTracao");
                    if (!string.IsNullOrEmpty(modal.VeicTracao.Proprietario.CNPJCPF))
                        iniData.WriteToIni(modal.VeicTracao.Proprietario, "veicTracao");

                    for (var i = 0; i < modal.VeicTracao.Condutor.Count; i++)
                        iniData.WriteToIni(modal.VeicTracao.Condutor[i], $"moto{i + 1:000}");

                    for (var i = 0; i < modal.Reboque.Count; i++)
                    {
                        var sessao = $"reboque{i + 1:00}";
                        iniData.WriteToIni(modal.Reboque[i], sessao);
                        if (!string.IsNullOrEmpty(modal.Reboque[i].Proprietario.CNPJCPF))
                            iniData.WriteToIni(modal.Reboque[i].Proprietario, sessao);
                    }

                    for (var i = 0; i < modal.Lacres.Count; i++)
                        iniData.WriteToIni(modal.Lacres[i], $"lacRodo{i + 1:000}");
                    break;

                case ModalAereoMDFe modal:
                    iniData.WriteToIni(modal, "aereo");
                    break;

                case ModalAquaviarioMDFe modal:
                    iniData.WriteToIni(modal, "aquav");

                    for (var i = 0; i < modal.InfTermCarreg.Count; i++)
                        iniData.WriteToIni(modal.InfTermCarreg[i], $"infTermCarreg{i + 1}");

                    for (var i = 0; i < modal.InfTermDescarreg.Count; i++)
                        iniData.WriteToIni(modal.InfTermDescarreg[i], $"infTermDescarreg{i + 1}");

                    for (var i = 0; i < modal.InfEmbComb.Count; i++)
                        iniData.WriteToIni(modal.InfEmbComb[i], $"infEmbComb{i + 1:00}");

                    for (var i = 0; i < modal.InfUnidCargaVazia.Count; i++)
                        iniData.WriteToIni(modal.InfUnidCargaVazia[i], $"infUnidCargaVazia{i + 1:000}");

                    for (var i = 0; i < modal.InfUnidTranspVazia.Count; i++)
                        iniData.WriteToIni(modal.InfUnidTranspVazia[i], $"infUnidTranspVazia{i + 1:000}");
                    break;

                case ModalFerroviarioMDFe modal:
                    iniData.WriteToIni(modal, "ferrov");

                    for (var i = 0; i < modal.Vagao.Count; i++)
                        iniData.WriteToIni(modal.Vagao[i], $"vag{i + 1:000}");
                    break;

                default:
                    throw new ApplicationException("Modal não informado.");
            }

            for (var i = 0; i < InfDoc.infMunDescarga.Count; i++)
            {
                var munDescarga = InfDoc.infMunDescarga[i];
                iniData.WriteToIni(munDescarga, $"DESC{i + 1:000}");

                for (var j = 0; j < munDescarga.InfCTe.Count; j++)
                {
                    var cte = munDescarga.InfCTe[j];
                    iniData.WriteToIni(cte, $"infCTe{i + 1:000}{j + 1:000}");

                    for (var k = 0; k < cte.Peri.Count; k++)
                        iniData.WriteToIni(cte.Peri[k], $"peri{i + 1:000}{j + 1:000}{k + 1:000}");

                    for (var k = 0; k < cte.InfEntregaParcial.Count; k++)
                        iniData.WriteToIni(cte.InfEntregaParcial[k], $"infEntregaParcial{i + 1:000}{j + 1:000}{k + 1:000}");

                    for (var k = 0; k < cte.InfUnidTransp.Count; k++)
                    {
                        var unidade = cte.InfUnidTransp[k];
                        iniData.WriteToIni(unidade, $"infUnidTransp{i + 1:000}{j + 1:000}{k + 1:000}");

                        for (var l = 0; l < unidade.lacUnidTransp.Count; l++)
                            iniData.WriteToIni(unidade.lacUnidTransp[l], $"lacUnidTransp{i + 1:000}{j + 1:000}{k + 1:000}{l + 1:000}");

                        for (var l = 0; l < unidade.infUnidCarga.Count; l++)
                        {
                            iniData.WriteToIni(unidade.infUnidCarga[l], $"infUnidCarga{i + 1:000}{j + 1:000}{k + 1:000}{l + 1:000}");
                            for (var m = 0; m < unidade.infUnidCarga[l].lacUnidCarga.Count; m++)
                                iniData.WriteToIni(unidade.infUnidCarga[l].lacUnidCarga[m], $"lacUnidCarga{i + 1:000}{j + 1:000}{k + 1:000}{l + 1:000}{m + 1:000}");
                        }
                    }
                }

                for (var j = 0; j < munDescarga.InfNFe.Count; j++)
                {
                    var nfe = munDescarga.InfNFe[j];
                    iniData.WriteToIni(nfe, $"infNFe{i + 1:000}{j + 1:000}");

                    for (var k = 0; k < nfe.Peri.Count; k++)
                        iniData.WriteToIni(nfe.Peri[k], $"peri{i + 1:000}{j + 1:000}{k + 1:000}");

                    for (var k = 0; k < nfe.InfEntregaParcial.Count; k++)
                        iniData.WriteToIni(nfe.InfEntregaParcial[k], $"infEntregaParcial{i + 1:000}{j + 1:000}{k + 1:000}");

                    for (var k = 0; k < nfe.InfUnidTransp.Count; k++)
                    {
                        var unidade = nfe.InfUnidTransp[k];
                        iniData.WriteToIni(unidade, $"infUnidTransp{i + 1:000}{j + 1:000}{k + 1:000}");

                        for (var l = 0; l < unidade.lacUnidTransp.Count; l++)
                            iniData.WriteToIni(unidade.lacUnidTransp[l], $"lacUnidTransp{i + 1:000}{j + 1:000}{k + 1:000}{l + 1:000}");

                        for (var l = 0; l < unidade.infUnidCarga.Count; l++)
                        {
                            iniData.WriteToIni(unidade.infUnidCarga[l], $"infUnidCarga{i + 1:000}{j + 1:000}{k + 1:000}{l + 1:000}");
                            for (var m = 0; m < unidade.infUnidCarga[l].lacUnidCarga.Count; m++)
                                iniData.WriteToIni(unidade.infUnidCarga[l].lacUnidCarga[m], $"lacUnidCarga{i + 1:000}{j + 1:000}{k + 1:000}{l + 1:000}{m + 1:000}");
                        }
                    }
                }

                for (var j = 0; j < munDescarga.InfMDFeTransp.Count; j++)
                {
                    var mdfe = munDescarga.InfMDFeTransp[j];
                    iniData.WriteToIni(mdfe, $"infMDFeTransp{i + 1:000}{j + 1:000}");

                    for (var k = 0; k < mdfe.Peri.Count; k++)
                        iniData.WriteToIni(mdfe.Peri[k], $"peri{i + 1:000}{j + 1:000}{k + 1:000}");

                    for (var k = 0; k < mdfe.InfEntregaParcial.Count; k++)
                        iniData.WriteToIni(mdfe.InfEntregaParcial[k], $"infEntregaParcial{i + 1:000}{j + 1:000}{k + 1:000}");

                    for (var k = 0; k < mdfe.InfUnidTransp.Count; k++)
                    {
                        var unidade = mdfe.InfUnidTransp[k];
                        iniData.WriteToIni(unidade, $"infUnidTransp{i + 1:000}{j + 1:000}{k + 1:000}");

                        for (var l = 0; l < unidade.lacUnidTransp.Count; l++)
                            iniData.WriteToIni(unidade.lacUnidTransp[l], $"lacUnidTransp{i + 1:000}{j + 1:000}{k + 1:000}{l + 1:000}");

                        for (var l = 0; l < unidade.infUnidCarga.Count; l++)
                        {
                            iniData.WriteToIni(unidade.infUnidCarga[l], $"infUnidCarga{i + 1:000}{j + 1:000}{k + 1:000}{l + 1:000}");
                            for (var m = 0; m < unidade.infUnidCarga[l].lacUnidCarga.Count; m++)
                                iniData.WriteToIni(unidade.infUnidCarga[l].lacUnidCarga[m], $"lacUnidCarga{i + 1:000}{j + 1:000}{k + 1:000}{l + 1:000}{m + 1:000}");
                        }
                    }
                }
            }

            for (var i = 0; i < Seg.Count; i++)
            {
                iniData.WriteToIni(Seg[i], $"seg{i + 1:000}");
                for (var j = 0; j < Seg[i].Averb.Count; j++)
                    iniData.WriteToIni(Seg[i].Averb[j], $"aver{i + 1:000}{j + 1:000}");
            }

            iniData.WriteToIni(ProdPred, "prodPred");
            if (ProdPred.InfLocalCarrega.CEP > 0)
                iniData.WriteToIni(ProdPred.InfLocalCarrega, "infLocalCarrega");

            if (ProdPred.InfLocalDescarrega.CEP > 0)
                iniData.WriteToIni(ProdPred.InfLocalDescarrega, "infLocalDescarrega");

            iniData.WriteToIni(Tot, "tot");

            for (var i = 0; i < Lacres.Count; i++)
                iniData.WriteToIni(Lacres[i], $"lacres{i + 1:000}");

            for (var i = 0; i < AutXml.Count; i++)
                iniData.WriteToIni(AutXml[i], $"autXML{i + 1:000}");

            iniData.WriteToIni(DadosAdicionais, "infAdic");
            iniData.WriteToIni(InfRespTec, "infRespTec");

            return iniData;
        }

        private void ReadFromIni(ACBrIniFile iniData)
        {
            iniData.ReadFromIni(ProcMDFe, "procMDFe");
            iniData.ReadFromIni(InfMDFe, "infMDFe");
            iniData.ReadFromIni(Identificacao, "ide");

            var i = 0;
            InfMunCarregaMDFe munCarrega;
            do
            {
                i++;
                munCarrega = iniData.ReadFromIni<InfMunCarregaMDFe>($"CARR{i:000}");
                if (munCarrega == null) continue;

                InfMunCarrega.Add(munCarrega);
            } while (munCarrega != null);

            i = 0;
            InfPercurso infPercurso;
            do
            {
                i++;
                infPercurso = iniData.ReadFromIni<InfPercurso>($"PERC{i:000}");
                if (infPercurso == null) continue;

                InfPercurso.Add(infPercurso);
            } while (infPercurso != null);

            iniData.ReadFromIni(Emitente, "emit");

            if (iniData.Contains("Rodo") ||
                iniData.Read("infANTT", "RNTRC", "") != "" ||
                iniData.Contains("infCIOT001") ||
                iniData.Contains("valePed001") ||
                iniData.Contains("infContratante001") ||
                iniData.Contains("valePed"))
            {
                var rodo = iniData.Contains("Rodo") ? iniData.ReadFromIni<ModalRodoMDFe>("Rodo") : new ModalRodoMDFe();
                iniData.ReadFromIni(rodo.InfANTT, "infANTT");

                i = 0;
                InfCIOTMDFe CIOT;
                do
                {
                    i++;
                    CIOT = iniData.ReadFromIni<InfCIOTMDFe>($"infCIOT{i:000}");
                    if (CIOT == null) continue;

                    rodo.InfANTT.InfCIOT.Add(CIOT);
                } while (CIOT != null);

                iniData.ReadFromIni(rodo.InfANTT.valePed, "valePed");

                i = 0;
                DispMDFe disp;
                do
                {
                    i++;
                    disp = iniData.ReadFromIni<DispMDFe>($"disp{i:000}");
                    if (disp == null) continue;

                    rodo.InfANTT.valePed.disp.Add(disp);
                } while (disp != null);

                i = 0;
                InfContratanteMDFe contratante;
                do
                {
                    i++;
                    contratante = iniData.ReadFromIni<InfContratanteMDFe>($"infContratante{i:000}");
                    if (contratante == null) continue;

                    rodo.InfANTT.InfContratante.Add(contratante);
                } while (contratante != null);

                i = 0;
                InfPagMDFe infPag;
                do
                {
                    i++;
                    infPag = iniData.ReadFromIni<InfPagMDFe>($"infPag{i:000}");
                    if (infPag == null) continue;

                    iniData.ReadFromIni(infPag.InfBanco, $"infBanc{i + 1:000}");

                    var j = 0;
                    CompMDFe comp;
                    do
                    {
                        i++;
                        comp = iniData.ReadFromIni<CompMDFe>($"Comp{i + 1:000}{j + 1:000}");
                        if (comp == null) continue;

                        infPag.Comp.Add(comp);
                    } while (comp != null);

                    InfPrazoMDFe prazo;
                    do
                    {
                        i++;
                        prazo = iniData.ReadFromIni<InfPrazoMDFe>($"infPrazo{i + 1:000}{j + 1:000}");
                        if (prazo == null) continue;

                        infPag.InfPrazo.Add(prazo);
                    } while (prazo != null);

                    rodo.InfANTT.InfPag.Add(infPag);
                } while (infPag != null);

                iniData.ReadFromIni(rodo.VeicTracao, "veicTracao");
                iniData.ReadFromIni(rodo.VeicTracao.Proprietario, "veicTracao");

                i = 0;
                CondutorMDFe condutor;
                do
                {
                    i++;
                    condutor = iniData.ReadFromIni<CondutorMDFe>($"moto{i:000}");
                    if (condutor == null) continue;

                    rodo.VeicTracao.Condutor.Add(condutor);
                } while (condutor != null);

                i = 0;
                ReboqueMDFe reboque;
                do
                {
                    i++;
                    reboque = iniData.ReadFromIni<ReboqueMDFe>($"reboque{i:000}");
                    if (reboque == null) continue;

                    iniData.WriteToIni(reboque.Proprietario, $"reboque{i:000}");
                    rodo.Reboque.Add(reboque);
                } while (reboque != null);

                i = 0;
                LacreMDFe lacre;
                do
                {
                    i++;
                    lacre = iniData.ReadFromIni<LacreMDFe>($"lacRodo{i:000}");
                    if (lacre == null) continue;

                    rodo.Lacres.Add(lacre);
                } while (lacre != null);

                Modal = rodo;
            }

            if (iniData.Read("aereo", "nac", "") != "")
                Modal = iniData.ReadFromIni<ModalAereoMDFe>("aereo");

            if (iniData.Read("aquav", "CNPJAgeNav", "") != "" ||
                iniData.Read("aquav", "irin", "") != "")
            {
                var modal = iniData.ReadFromIni<ModalAquaviarioMDFe>("aquav");

                i = 0;
                InfTerminalCarregMDFe infCarrega;
                do
                {
                    i++;
                    infCarrega = iniData.ReadFromIni<InfTerminalCarregMDFe>($"infTermCarreg{i}");
                    if (infCarrega == null) continue;

                    modal.InfTermCarreg.Add(infCarrega);
                } while (infCarrega != null);

                i = 0;
                InfTerminalDescarregMDFe infDescarrega;
                do
                {
                    i++;
                    infDescarrega = iniData.ReadFromIni<InfTerminalDescarregMDFe>($"infTermDescarreg{i}");
                    if (infDescarrega == null) continue;

                    modal.InfTermDescarreg.Add(infDescarrega);
                } while (infDescarrega != null);

                i = 0;
                InfEmbCombMDFe infEmb;
                do
                {
                    i++;
                    infEmb = iniData.ReadFromIni<InfEmbCombMDFe>($"infEmbComb{i:00}");
                    if (infEmb == null) continue;

                    modal.InfEmbComb.Add(infEmb);
                } while (infEmb != null);

                i = 0;
                InfUnidCargaVaziaMDFe infUnid;
                do
                {
                    i++;
                    infUnid = iniData.ReadFromIni<InfUnidCargaVaziaMDFe>($"infUnidCargaVazia{i:000}");
                    if (infUnid == null) continue;

                    modal.InfUnidCargaVazia.Add(infUnid);
                } while (infUnid != null);

                i = 0;
                InfUnidTranspVaziaMDFe infCarga;
                do
                {
                    i++;
                    infCarga = iniData.ReadFromIni<InfUnidTranspVaziaMDFe>($"infUnidTranspVazia{i:000}");
                    if (infCarga == null) continue;

                    modal.InfUnidTranspVazia.Add(infCarga);
                } while (infCarga != null);

                Modal = modal;
            }

            i = 0;
            InfMunDescargaMDFe municipio;
            do
            {
                i++;
                municipio = iniData.ReadFromIni<InfMunDescargaMDFe>($"DESC{i:000}");
                if (municipio == null) continue;

                var j = 0;
                InfCTeMDFe cte;
                do
                {
                    j++;
                    cte = iniData.ReadFromIni<InfCTeMDFe>($"infCTe{i:000}{j:000}");
                    if (cte == null) continue;

                    var k = 0;
                    PeriMDFe peri;
                    do
                    {
                        k++;
                        peri = iniData.ReadFromIni<PeriMDFe>($"peri{i:000}{j:000}{k:000}");
                        if (peri == null) continue;

                        cte.Peri.Add(peri);
                    } while (peri != null);

                    k = 0;
                    InfEntregaParcialMDFe entregaParcial;
                    do
                    {
                        k++;
                        entregaParcial = iniData.ReadFromIni<InfEntregaParcialMDFe>($"infEntregaParcial{i:000}{j:000}{k:000}");
                        if (entregaParcial == null) continue;

                        cte.InfEntregaParcial.Add(entregaParcial);
                    } while (entregaParcial != null);

                    k = 0;
                    InfUnidTranspMDFe unidTransp;
                    do
                    {
                        k++;
                        unidTransp = iniData.ReadFromIni<InfUnidTranspMDFe>($"infUnidTransp{i:000}{j:000}{k:000}");
                        if (unidTransp == null) continue;

                        var l = 0;
                        LacreMDFe lacre;
                        do
                        {
                            l++;
                            lacre = iniData.ReadFromIni<LacreMDFe>($"lacUnidTransp{i:000}{j:000}{k:000}{l:000}");
                            if (lacre == null) continue;

                            unidTransp.lacUnidTransp.Add(lacre);
                        } while (lacre != null);

                        l = 0;
                        InfUnidCargaMDFe unidCarga;
                        do
                        {
                            l++;
                            unidCarga = iniData.ReadFromIni<InfUnidCargaMDFe>($"infUnidCarga{i:000}{j:000}{k:000}{l:000}");
                            if (unidCarga == null) continue;

                            var m = 0;
                            LacreMDFe lacreCarga;
                            do
                            {
                                m++;
                                lacreCarga = iniData.ReadFromIni<LacreMDFe>($"lacUnidCarga{i:000}{j:000}{k:000}{l:000}{m:000}");
                                if (lacreCarga == null) continue;

                                unidCarga.lacUnidCarga.Add(lacreCarga);
                            } while (lacreCarga != null);

                            unidTransp.infUnidCarga.Add(unidCarga);
                        } while (unidCarga != null);

                        cte.InfUnidTransp.Add(unidTransp);
                    } while (unidTransp != null);

                    municipio.InfCTe.Add(cte);
                } while (cte != null);

                j = 0;
                InfNFeMDFe nfe;
                do
                {
                    j++;
                    nfe = iniData.ReadFromIni<InfNFeMDFe>($"infNFe{i:000}{j:000}");
                    if (nfe == null) continue;

                    var k = 0;
                    PeriMDFe peri;
                    do
                    {
                        k++;
                        peri = iniData.ReadFromIni<PeriMDFe>($"peri{i:000}{j:000}{k:000}");
                        if (peri == null) continue;

                        nfe.Peri.Add(peri);
                    } while (peri != null);

                    k = 0;
                    InfEntregaParcialMDFe entregaParcial;
                    do
                    {
                        k++;
                        entregaParcial = iniData.ReadFromIni<InfEntregaParcialMDFe>($"infEntregaParcial{i:000}{j:000}{k:000}");
                        if (entregaParcial == null) continue;

                        nfe.InfEntregaParcial.Add(entregaParcial);
                    } while (entregaParcial != null);

                    k = 0;
                    InfUnidTranspMDFe unidTransp;
                    do
                    {
                        k++;
                        unidTransp = iniData.ReadFromIni<InfUnidTranspMDFe>($"infUnidTransp{i:000}{j:000}{k:000}");
                        if (unidTransp == null) continue;

                        var l = 0;
                        LacreMDFe lacre;
                        do
                        {
                            l++;
                            lacre = iniData.ReadFromIni<LacreMDFe>($"lacUnidTransp{i:000}{j:000}{k:000}{l:000}");
                            if (lacre == null) continue;

                            unidTransp.lacUnidTransp.Add(lacre);
                        } while (lacre != null);

                        l = 0;
                        InfUnidCargaMDFe unidCarga;
                        do
                        {
                            l++;
                            unidCarga = iniData.ReadFromIni<InfUnidCargaMDFe>($"infUnidCarga{i:000}{j:000}{k:000}{l:000}");
                            if (unidCarga == null) continue;

                            var m = 0;
                            LacreMDFe lacreCarga;
                            do
                            {
                                m++;
                                lacreCarga = iniData.ReadFromIni<LacreMDFe>($"lacUnidCarga{i:000}{j:000}{k:000}{l:000}{m:000}");
                                if (lacreCarga == null) continue;

                                unidCarga.lacUnidCarga.Add(lacreCarga);
                            } while (lacreCarga != null);

                            unidTransp.infUnidCarga.Add(unidCarga);
                        } while (unidCarga != null);

                        nfe.InfUnidTransp.Add(unidTransp);
                    } while (unidTransp != null);

                    municipio.InfNFe.Add(nfe);
                } while (nfe != null);

                j = 0;
                InfMDFeTransp mdfe;
                do
                {
                    j++;
                    mdfe = iniData.ReadFromIni<InfMDFeTransp>($"infMDFeTransp{i:000}{j:000}");
                    if (mdfe == null) continue;

                    var k = 0;
                    PeriMDFe peri;
                    do
                    {
                        k++;
                        peri = iniData.ReadFromIni<PeriMDFe>($"peri{i:000}{j:000}{k:000}");
                        if (peri == null) continue;

                        mdfe.Peri.Add(peri);
                    } while (peri != null);

                    k = 0;
                    InfEntregaParcialMDFe entregaParcial;
                    do
                    {
                        k++;
                        entregaParcial = iniData.ReadFromIni<InfEntregaParcialMDFe>($"infEntregaParcial{i:000}{j:000}{k:000}");
                        if (entregaParcial == null) continue;

                        mdfe.InfEntregaParcial.Add(entregaParcial);
                    } while (entregaParcial != null);

                    k = 0;
                    InfUnidTranspMDFe unidTransp;
                    do
                    {
                        k++;
                        unidTransp = iniData.ReadFromIni<InfUnidTranspMDFe>($"infUnidTransp{i:000}{j:000}{k:000}");
                        if (unidTransp == null) continue;

                        var l = 0;
                        LacreMDFe lacre;
                        do
                        {
                            l++;
                            lacre = iniData.ReadFromIni<LacreMDFe>($"lacUnidTransp{i:000}{j:000}{k:000}{l:000}");
                            if (lacre == null) continue;

                            unidTransp.lacUnidTransp.Add(lacre);
                        } while (lacre != null);

                        l = 0;
                        InfUnidCargaMDFe unidCarga;
                        do
                        {
                            l++;
                            unidCarga = iniData.ReadFromIni<InfUnidCargaMDFe>($"infUnidCarga{i:000}{j:000}{k:000}{l:000}");
                            if (unidCarga == null) continue;

                            var m = 0;
                            LacreMDFe lacreCarga;
                            do
                            {
                                m++;
                                lacreCarga = iniData.ReadFromIni<LacreMDFe>($"lacUnidCarga{i:000}{j:000}{k:000}{l:000}{m:000}");
                                if (lacreCarga == null) continue;

                                unidCarga.lacUnidCarga.Add(lacreCarga);
                            } while (lacreCarga != null);

                            unidTransp.infUnidCarga.Add(unidCarga);
                        } while (unidCarga != null);

                        mdfe.InfUnidTransp.Add(unidTransp);
                    } while (unidTransp != null);

                    municipio.InfMDFeTransp.Add(mdfe);
                } while (mdfe != null);

                InfDoc.infMunDescarga.Add(municipio);
            } while (municipio != null);

            i = 0;
            SeguroMDFe seg;
            do
            {
                i++;
                seg = iniData.ReadFromIni<SeguroMDFe>($"seg{i}");
                if (seg == null) continue;

                var j = 0;
                AverbacaoMDFe averb;
                do
                {
                    j++;
                    averb = iniData.ReadFromIni<AverbacaoMDFe>($"aver{i:000}{j:000}");
                    if (averb == null) continue;

                    seg.Averb.Add(averb);
                } while (averb != null);

                Seg.Add(seg);
            } while (seg != null);

            iniData.ReadFromIni(ProdPred, "prodPred");
            iniData.ReadFromIni(ProdPred.InfLocalCarrega, "infLocalCarrega");
            iniData.ReadFromIni(ProdPred.InfLocalDescarrega, "infLocalDescarrega");

            iniData.ReadFromIni(Tot, "tot");

            i = 0;
            LacreMDFe lacreMDFe;
            do
            {
                i++;
                lacreMDFe = iniData.ReadFromIni<LacreMDFe>($"lacres{i:000}");
                if (lacreMDFe == null) continue;

                Lacres.Add(lacreMDFe);
            } while (lacreMDFe != null);

            i = 0;
            AutXML autXml;
            do
            {
                i++;
                autXml = iniData.ReadFromIni<AutXML>($"autXML{i:000}");
                if (autXml == null) continue;

                AutXml.Add(autXml);
            } while (autXml != null);

            iniData.ReadFromIni(DadosAdicionais, "infAdic");
            iniData.ReadFromIni(InfRespTec, "infRespTec");
        }

        public static Manifesto Load(string conteudo) => ACBrIniFile.Parse(conteudo);

        public static Manifesto LoadFromFile(string filePath) => ACBrIniFile.Load(filePath);

        public static Manifesto LoadFromFile(Stream stream) => ACBrIniFile.Load(stream);

        #endregion Methods

        #region Operators

        public static implicit operator Manifesto(ACBrIniFile source) => new Manifesto(source);

        public static implicit operator ACBrIniFile(Manifesto source) => source.WriteToIni();

        #endregion Operators
    }
}