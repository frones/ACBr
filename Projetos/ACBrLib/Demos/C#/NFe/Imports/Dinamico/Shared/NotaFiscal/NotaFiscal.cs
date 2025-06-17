using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using ACBrLib.Core;
using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class NotaFiscal
    {
        CSTPIS[] CSTPISValues = { CSTPIS.pis04, CSTPIS.pis05, CSTPIS.pis06, CSTPIS.pis07, CSTPIS.pis08, CSTPIS.pis09 };
        CSTCofins[] CSTCofinsValues = { CSTCofins.cof04, CSTCofins.cof05, CSTCofins.cof06, CSTCofins.cof07, CSTCofins.cof08, CSTCofins.cof09 };

        #region Constructor

        public NotaFiscal()
        {
            ProcNFe = new ProcNFe();
            InfNFe = new InfNFe();
            Identificacao = new IdentificacaoNFe();
            Emitente = new EmitenteNFe();
            Avulsa = new AvulsaNFe();
            Destinatario = new DestinatarioNFe();
            Retirada = new RetiradaEntregaNFe();
            Entrega = new RetiradaEntregaNFe();
            AutXML = new List<AutXML>(10);
            Produtos = new List<ProdutoNFe>(990);
            Total = new TotalNFe();
            ISSQNtot = new ISSQNtotNFe();
            RetTrib = new RetTribNFe();
            Transportador = new TransportadorNFe();
            Volumes = new List<VolumeNFe>();
            Fatura = new FaturaNFe();
            Duplicatas = new List<DuplicataNFe>();
            Pagamentos = new List<PagamentoNFe>();
            InfIntermed = new InfIntermedNFe();
            DadosAdicionais = new DadosAdicionaisNFe();
            Exporta = new ExportaNFe();
            Compra = new CompraNFe();
            Cana = new CanaNFe();
            InfNFeSupl = new InfNFeSupl();
            InfRespTec = new InfRespTec();

            InfNFe.Versao = "4.00";
        }

        internal NotaFiscal(ACBrIniFile ini) : this()
        {
            ReadFromIni(ini);
        }

        #endregion Constructor

        #region Properties

        public ProcNFe ProcNFe { get; }

        public InfNFe InfNFe { get; }

        public IdentificacaoNFe Identificacao { get; }

        public EmitenteNFe Emitente { get; }

        public AvulsaNFe Avulsa { get; }

        public DestinatarioNFe Destinatario { get; }

        public RetiradaEntregaNFe Retirada { get; }

        public RetiradaEntregaNFe Entrega { get; }

        public List<AutXML> AutXML { get; }

        public List<ProdutoNFe> Produtos { get; }

        public TotalNFe Total { get; }

        public ISSQNtotNFe ISSQNtot { get; }

        public RetTribNFe RetTrib { get; }

        public TransportadorNFe Transportador { get; }

        public List<VolumeNFe> Volumes { get; }

        public FaturaNFe Fatura { get; }

        public List<DuplicataNFe> Duplicatas { get; }

        public List<PagamentoNFe> Pagamentos { get; }

        public InfIntermedNFe InfIntermed { get; }

        public DadosAdicionaisNFe DadosAdicionais { get; }

        public ExportaNFe Exporta { get; }

        public CompraNFe Compra { get; }

        public CanaNFe Cana { get; }

        public InfNFeSupl InfNFeSupl { get; }

        public InfRespTec InfRespTec { get; }

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

            if (!string.IsNullOrEmpty(ProcNFe.nProt))
                iniData.WriteToIni(ProcNFe, "procNFe");

            iniData.WriteToIni(InfNFe, "infNFe");
            iniData.WriteToIni(Identificacao, "Identificacao");

            for (var i = 0; i < Identificacao.gPagAntecipado.Count; i++)
                iniData.WriteToIni(Identificacao.gPagAntecipado[i], $"gPagAntecipado{i + 1:000}");

            for (var i = 0; i < Identificacao.NFref.Count; i++)
                iniData.WriteToIni(Identificacao.NFref[i], $"NFRef{i + 1:000}");

            iniData.WriteToIni(Emitente, "Emitente");
            if (!string.IsNullOrEmpty(Avulsa.CNPJ))
                iniData.WriteToIni(Avulsa, "Avulsa");

            iniData.WriteToIni(Destinatario, "Destinatario");

            if (!string.IsNullOrEmpty(Retirada.xLgr))
                iniData.WriteToIni(Retirada, "Retirada");

            if (!string.IsNullOrEmpty(Entrega.xLgr))
                iniData.WriteToIni(Entrega, "Entrega");

            for (var i = 0; i < AutXML.Count; i++)
                iniData.WriteToIni(AutXML[i], $"autXML{i + 1:00}");

            for (var i = 0; i < Produtos.Count; i++)
            {
                var produto = Produtos[i];

                iniData.WriteToIni(produto, $"Produto{i + 1:000}");

                for (var k = 0; k < produto.NVE.Count; k++)
                    iniData.WriteToIni(produto.NVE[k], $"NVE{i + 1:000}{k + 1:000}");

                for (var k = 0; k < produto.DI.Count; k++)
                {
                    iniData.WriteToIni(produto.DI[k], $"DI{i + 1:000}{k + 1:000}");
                    for (var j = 0; j < produto.DI[k].LADI.Count; j++)
                        iniData.WriteToIni(produto.DI[k].LADI[j], $"LADI{i + 1:000}{k + 1:000}{j + 1:000}");
                }

                for (var k = 0; k < produto.DetExport.Count; k++)
                    iniData.WriteToIni(produto.DetExport[k], $"detExport{i + 1:000}{k + 1:000}");

                for (var k = 0; k < produto.Rastro.Count; k++)
                    iniData.WriteToIni(produto.Rastro[k], $"rastro{i + 1:000}{k + 1:000}");

                for (var k = 0; k < produto.Medicamento.Count; k++)
                    iniData.WriteToIni(produto.Medicamento[k], $"Medicamento{i + 1:000}{k + 1:000}");

                for (var k = 0; k < produto.Arma.Count; k++)
                    iniData.WriteToIni(produto.Arma[k], $"Arma{i + 1:000}{k + 1:000}");

                if (produto.ImpostoDevol.pDevol > 0)
                    iniData.WriteToIni(produto.ImpostoDevol, $"impostoDevol{i + 1:000}");

                if (!string.IsNullOrEmpty(produto.Veiculo.chassi))
                    iniData.WriteToIni(produto.Veiculo, $"Veiculo{i + 1:000}");

                if (produto.Combustivel.cProdANP > 0)
                {
                    iniData.WriteToIni(produto.Combustivel, $"Combustivel{i + 1:000}");

                    if (produto.Combustivel.CIDE.qBCprod > 0 || produto.Combustivel.CIDE.vAliqProd > 0 ||
                        produto.Combustivel.CIDE.vCIDE > 0)
                        iniData.WriteToIni(produto.Combustivel.CIDE, $"CIDE{i + 1:000}");

                    if (produto.Combustivel.Encerrante.nBico > 0)
                        iniData.WriteToIni(produto.Combustivel.Encerrante, $"encerrante{i + 1:000}");

                    if (produto.Combustivel.OrigComb.Count > 0)
                        for (var k = 0; k < produto.Combustivel.OrigComb.Count; k++)
                             iniData.WriteToIni(produto.Combustivel.OrigComb[k], $"origComb{i + 1:000}{k + 1:00}");
                }

                if (produto.DFeReferenciado.chaveAcesso != "")
                {
                    iniData.WriteToIni(produto.DFeReferenciado, $"DFeReferenciado{i + 1:000}");
                }

                iniData.WriteToIni(produto.ICMS, $"ICMS{i + 1:000}");
                if (produto.ICMSUFDEST.pICMSInterPart.HasValue)
                    iniData.WriteToIni(produto.ICMSUFDEST, $"ICMSUFDEST{i + 1:000}");

                if (!String.IsNullOrEmpty(produto.IPI.cEnq))
                    iniData.WriteToIni(produto.IPI, $"IPI{i + 1:000}");

                if (produto.II.vBC.HasValue)
                    iniData.WriteToIni(produto.II, $"II{i + 1:000}");

                if (produto.PIS.vBC.HasValue || CSTPISValues.Contains(produto.PIS.CST))
                    iniData.WriteToIni(produto.PIS, $"PIS{i + 1:000}");

                if (produto.PISST.vBC.HasValue)
                    iniData.WriteToIni(produto.PISST, $"PISST{i + 1:000}");

                if (produto.COFINS.vBC.HasValue || CSTCofinsValues.Contains(produto.COFINS.CST))
                    iniData.WriteToIni(produto.COFINS, $"COFINS{i + 1:000}");
                
                if (produto.COFINSST.vBC.HasValue)
                    iniData.WriteToIni(produto.COFINSST, $"COFINSST{i + 1:000}");

                if (produto.ISSQN.vBC.HasValue)
                    iniData.WriteToIni(produto.ISSQN, $"ISSQN{i + 1:000}");

                if (produto.IS.CSTIS > 0)
                {
                    iniData.WriteToIni(produto.IS, $"IS{i + 1:000}");
                }

                if (produto.IBSCBS.CST != CSTIBSCBS.cstVazio)
                {
                    iniData.WriteToIni(produto.IBSCBS, $"IBSCBS{i + 1:000}");
                    if (produto.IBSCBS.gIBSCBS.vBC > 0)
                    {
                        iniData.WriteToIni(produto.IBSCBS.gIBSCBS, $"gIBSCBS{i + 1:000}");
                        iniData.WriteToIni(produto.IBSCBS.gIBSCBS.gIBSUF, $"gIBSUF{i + 1:000}");
                        iniData.WriteToIni(produto.IBSCBS.gIBSCBS.gIBSMun, $"gIBSMun{i + 1:000}");
                        iniData.WriteToIni(produto.IBSCBS.gIBSCBS.gCBS, $"gCBS{i + 1:000}");

                        if (produto.IBSCBS.gIBSCBS.gTribRegular.CSTReg != CSTIBSCBS.cstVazio)
                        {
                            iniData.WriteToIni(produto.IBSCBS.gIBSCBS.gTribRegular, $"gTribRegular{i + 1:000}");
                        }
                        if (produto.IBSCBS.gIBSCBS.gIBSCredPres.cCredPres > 0)
                        {
                            iniData.WriteToIni(produto.IBSCBS.gIBSCBS.gIBSCredPres, $"gIBSCredPres{i + 1:000}");
                        }
                        if (produto.IBSCBS.gIBSCBS.gCBSCredPres.cCredPres > 0)
                        {
                            iniData.WriteToIni(produto.IBSCBS.gIBSCBS.gCBSCredPres, $"gCBSCredPres{i + 1:000}");
                        }
                    }else if (produto.IBSCBS.gIBSCBSMono.adRemIBS > 0)
                    {
                        iniData.WriteToIni(produto.IBSCBS.gIBSCBSMono, $"gIBSCBSMono{i + 1:000}");
                    }
                    else
                    {
                        if (Identificacao.modelo == Core.NFe.ModeloNFe.moNFe &&
                            produto.IBSCBS?.CST == CSTIBSCBS.cst800)
                        {
                            iniData.WriteToIni(produto.IBSCBS.gTransfCred, $"gTransfCred{i + 1:000}");
                        }
                    }

                    if (Identificacao.modelo == Core.NFe.ModeloNFe.moNFe &&
                            produto.IBSCBS.gCredPresIBSZFM.tpCredPresIBSZFM != TipoCredPresIBSZFM.tcpNenhum)
                    {
                        iniData.WriteToIni(produto.IBSCBS.gCredPresIBSZFM, $"gCredPresIBSZFM{i + 1:000}");
                    }

                    if (produto.IBSCBS.gIBSCBS.gTribCompraGov.pAliqIBSUF > 0)
                    {
                        iniData.WriteToIni(produto.IBSCBS.gIBSCBS.gTribCompraGov, $"gTribCompraGov{i + 1:000}");
                    }
                }
            }

            iniData.WriteToIni(Total, "Total");
            if (ISSQNtot.vBC.HasValue)
                iniData.WriteToIni(ISSQNtot, "ISSQNtot");

            iniData.WriteToIni(Total.ISTot, "ISTot");
            iniData.WriteToIni(Total.IBSCBSTot, "IBSCBSTot");
            iniData.WriteToIni(Total.IBSCBSTot.gIBS, "gIBS");
            iniData.WriteToIni(Total.IBSCBSTot.gIBS.gIBSUF, "gIBSUFTot");
            iniData.WriteToIni(Total.IBSCBSTot.gIBS.gIBSMun, "gIBSMunTot");
            iniData.WriteToIni(Total.IBSCBSTot.gCBS, "gCBSTot");
            iniData.WriteToIni(Total.IBSCBSTot.gMono, "gMono");

            iniData.WriteToIni(RetTrib, "retTrib");

            iniData.WriteToIni(Transportador, "Transportador");
            for (var i = 0; i < Transportador.Reboque.Count; i++)
                iniData.WriteToIni(Transportador.Reboque[i], $"Reboque{i + 1:000}");

            for (var i = 0; i < Volumes.Count; i++)
            {
                iniData.WriteToIni(Volumes[i], $"Volume{i + 1:000}");
                for (var k = 0; k < Volumes[i].Lacres.Count; k++)
                    iniData.WriteToIni(Volumes[i].Lacres[k], $"Lacre{i + 1:000}{k + 1:000}");
            }

            iniData.WriteToIni(Fatura, "Fatura");

            for (var i = 0; i < Duplicatas.Count; i++)
                iniData.WriteToIni(Duplicatas[i], $"Duplicata{i + 1:000}");

            for (var i = 0; i < Pagamentos.Count; i++)
                iniData.WriteToIni(Pagamentos[i], $"pag{i + 1:000}");

            if (!string.IsNullOrEmpty(InfIntermed.CNPJ))
                iniData.WriteToIni(InfIntermed, "infIntermed");

            if (!string.IsNullOrEmpty(DadosAdicionais.infAdFisco) || !string.IsNullOrEmpty(DadosAdicionais.infCpl))
                iniData.WriteToIni(DadosAdicionais, "DadosAdicionais");

            for (var i = 0; i < DadosAdicionais.ObsCont.Count; i++)
                iniData.WriteToIni(DadosAdicionais.ObsCont[i], $"obsCont{i + 1:000}");

            for (var i = 0; i < DadosAdicionais.ObsFisco.Count; i++)
                iniData.WriteToIni(DadosAdicionais.ObsFisco[i], $"obsFisco{i + 1:000}");

            for (var i = 0; i < DadosAdicionais.ProcRef.Count; i++)
                iniData.WriteToIni(DadosAdicionais.ProcRef[i], $"procRef{i + 1:000}");

            if (!string.IsNullOrEmpty(Exporta.UFSaidaPais))
                iniData.WriteToIni(Exporta, "Exporta");

            if (!string.IsNullOrEmpty(Compra.xNEmp) ||
                !string.IsNullOrEmpty(Compra.xCont) ||
                !string.IsNullOrEmpty(Compra.xPed))
                iniData.WriteToIni(Compra, "compra");

            if (!string.IsNullOrEmpty(Cana.safra))
            {
                iniData.WriteToIni(Cana, "cana");
                for (var i = 0; i < Cana.forDia.Count; i++)
                    iniData.WriteToIni(Cana.forDia[i], $"forDia{i + 1:000}");

                for (var i = 0; i < Cana.deduc.Count; i++)
                    iniData.WriteToIni(Cana.deduc[i], $"deduc{i + 1:000}");
            }

            iniData.WriteToIni(InfNFeSupl, "infNFeSupl");

            if (!string.IsNullOrEmpty(InfRespTec.CNPJ))
                iniData.WriteToIni(InfRespTec, "infRespTec");

            return iniData;
        }

        private void ReadFromIni(ACBrIniFile iniData)
        {
            iniData.ReadFromIni(ProcNFe, "procNFe");
            iniData.ReadFromIni(InfNFe, "infNFe");
            iniData.ReadFromIni(Identificacao, "Identificacao");

            var i = 0;
            gPagAntecipado gPagAntecipado;
            do
            {
                i++;
                gPagAntecipado = iniData.ReadFromIni<gPagAntecipado>($"gPagAntecipado{i:000}");
                if (gPagAntecipado == null) continue;

                Identificacao.gPagAntecipado.Add(gPagAntecipado);
            } while (gPagAntecipado != null);

            i = 0;
            NFRef nfRef;
            do
            {
                i++;
                nfRef = iniData.ReadFromIni<NFRef>($"NFRef{i:000}");
                if (nfRef == null) continue;

                Identificacao.NFref.Add(nfRef);
            } while (nfRef != null);

            iniData.ReadFromIni(Emitente, "Emitente");
            iniData.ReadFromIni(Avulsa, "Avulsa");
            iniData.ReadFromIni(Destinatario, "Destinatario");
            iniData.ReadFromIni(Retirada, "Retirada");
            iniData.ReadFromIni(Entrega, "Entrega");

            i = 0;
            AutXML autXml;
            do
            {
                i++;
                autXml = iniData.ReadFromIni<AutXML>($"autXML{i:00}");
                if (autXml == null) continue;

                AutXML.Add(autXml);
            } while (autXml != null);

            i = 0;
            ProdutoNFe produto;
            do
            {
                i++;
                produto = iniData.ReadFromIni<ProdutoNFe>($"Produto{i:000}");
                if (produto == null) continue;

                var k = 0;
                NVENFe nveItem;
                do
                {
                    k++;
                    nveItem = iniData.ReadFromIni<NVENFe>($"NVE{i:000}{k:000}");
                    if (nveItem == null) continue;

                    produto.NVE.Add(nveItem);
                } while (nveItem != null);

                k = 0;
                DINFe diItem;
                do
                {
                    k++;
                    diItem = iniData.ReadFromIni<DINFe>($"DI{i:000}{k:000}");
                    if (diItem == null) continue;

                    var j = 0;
                    LADINFe ladiItem;
                    do
                    {
                        j++;
                        ladiItem = iniData.ReadFromIni<LADINFe>($"DI{i:000}{k:000}{j:000}");
                        if (ladiItem != null) diItem.LADI.Add(ladiItem);
                    } while (ladiItem != null);

                    produto.DI.Add(diItem);
                } while (diItem != null);

                k = 0;
                DetExport detExport;
                do
                {
                    k++;
                    detExport = iniData.ReadFromIni<DetExport>($"detExport{i:000}{k:000}");
                    if (detExport == null) continue;
                    produto.DetExport.Add(detExport);
                } while (detExport != null);

                k = 0;
                RastroNFe rastroItem;
                do
                {
                    k++;
                    rastroItem = iniData.ReadFromIni<RastroNFe>($"Rastro{i:000}{k:000}");
                    if (rastroItem == null) continue;

                    produto.Rastro.Add(rastroItem);
                } while (rastroItem != null);

                k = 0;
                MedicamentoNFe medItem;
                do
                {
                    k++;
                    medItem = iniData.ReadFromIni<MedicamentoNFe>($"Medicamento{i:000}{k:000}");
                    if (medItem == null) continue;

                    produto.Medicamento.Add(medItem);
                } while (medItem != null);

                k = 0;
                ArmaNFe armaItem;
                do
                {
                    k++;
                    armaItem = iniData.ReadFromIni<ArmaNFe>($"Arma{i:000}{k:000}");
                    if (armaItem == null) continue;

                    produto.Arma.Add(armaItem);
                } while (armaItem != null);

                iniData.ReadFromIni(produto.ImpostoDevol, $"impostoDevol{i:000}");
                iniData.ReadFromIni(produto.Veiculo, $"Veiculo{i:000}");
                iniData.ReadFromIni(produto.Combustivel, $"Combustivel{i:000}");
                iniData.ReadFromIni(produto.Combustivel.CIDE, $"CIDE{i:000}");
                iniData.ReadFromIni(produto.Combustivel.Encerrante, $"encerrante{i:000}");
                iniData.ReadFromIni(produto.DFeReferenciado, $"DFeReferenciado{i:000}");

                k = 0;
                OrigCombNFe OrigComb;
                do
                {
                    k++;
                    OrigComb = iniData.ReadFromIni<OrigCombNFe>($"origComb{i:000}{k:00}");
                    if (OrigComb == null) continue;

                    produto.Combustivel.OrigComb.Add(OrigComb);
                } while (OrigComb != null);

                iniData.ReadFromIni(produto.ICMS, $"ICMS{i:000}");
                iniData.ReadFromIni(produto.IPI, $"IPI{i:000}");
                iniData.ReadFromIni(produto.II, $"II{i:000}");
                iniData.ReadFromIni(produto.PIS, $"PIS{i:000}");
                iniData.ReadFromIni(produto.PISST, $"PISST{i:000}");
                iniData.ReadFromIni(produto.COFINS, $"COFINS{i:000}");
                iniData.ReadFromIni(produto.COFINSST, $"COFINSST{i:000}");
                iniData.ReadFromIni(produto.ISSQN, $"ISSQN{i:000}");
                iniData.ReadFromIni(produto.IS, $"IS{i:000}");
                iniData.ReadFromIni(produto.IBSCBS, $"IBSCBS{i:000}");
                iniData.ReadFromIni(produto.IBSCBS.gIBSCBS, $"gIBSCBS{i:000}");
                iniData.ReadFromIni(produto.IBSCBS.gIBSCBS.gIBSUF, $"gIBSUF{i:000}");
                iniData.ReadFromIni(produto.IBSCBS.gIBSCBS.gIBSMun, $"gIBSMun{i:000}");
                iniData.ReadFromIni(produto.IBSCBS.gIBSCBS.gCBS, $"gCBS{i:000}");
                iniData.ReadFromIni(produto.IBSCBS.gIBSCBS.gTribRegular, $"gTribRegular{i:000}");                
                iniData.ReadFromIni(produto.IBSCBS.gIBSCBS.gIBSCredPres, $"gIBSCredPres{i:000}");
                iniData.ReadFromIni(produto.IBSCBS.gIBSCBS.gCBSCredPres, $"gCBSCredPres{i:000}");
                iniData.ReadFromIni(produto.IBSCBS.gIBSCBSMono, $"gIBSCBSMono{i:000}");
                iniData.ReadFromIni(produto.IBSCBS.gTransfCred, $"gTransfCred{i:000}");
                iniData.ReadFromIni(produto.IBSCBS.gCredPresIBSZFM, $"gCredPresIBSZFM{i:000}");
                iniData.ReadFromIni(produto.IBSCBS.gIBSCBS.gTribCompraGov, $"gTribCompraGov{i:000}");

                Produtos.Add(produto);
            } while (produto != null);

            iniData.ReadFromIni(Total, "Total");
            iniData.ReadFromIni(ISSQNtot, "ISSQNtot");
            iniData.ReadFromIni(Total.ISTot, "ISTot");
            iniData.ReadFromIni(Total.IBSCBSTot, "IBSCBSTot");
            iniData.ReadFromIni(Total.IBSCBSTot.gIBS, "gIBS");
            iniData.ReadFromIni(Total.IBSCBSTot.gIBS.gIBSUF, "gIBSUFTot");
            iniData.ReadFromIni(Total.IBSCBSTot.gIBS.gIBSMun, "gIBSMunTot");
            iniData.ReadFromIni(Total.IBSCBSTot.gCBS, "gCBSTot");
            iniData.ReadFromIni(Total.IBSCBSTot.gMono, "gMono");
            iniData.ReadFromIni(RetTrib, "retTrib");
            iniData.ReadFromIni(Transportador, "Transportador");

            i = 0;
            ReboqueNFe reboque;
            do
            {
                i++;
                reboque = iniData.ReadFromIni<ReboqueNFe>($"Reboque{i:000}");
                if (reboque == null) continue;

                Transportador.Reboque.Add(reboque);
            } while (reboque != null);

            i = 0;
            VolumeNFe volume;
            do
            {
                i++;
                volume = iniData.ReadFromIni<VolumeNFe>($"Volume{i:000}");
                if (volume == null) continue;

                var k = 0;
                LacresNFe lacre;
                do
                {
                    k++;
                    lacre = iniData.ReadFromIni<LacresNFe>($"Lacre{i:000}{k:000}");
                    if (lacre == null) continue;

                    volume.Lacres.Add(lacre);
                } while (lacre != null);

                Volumes.Add(volume);
            } while (volume != null);

            iniData.ReadFromIni(Fatura, "Fatura");

            i = 0;
            DuplicataNFe duplicata;
            do
            {
                i++;
                duplicata = iniData.ReadFromIni<DuplicataNFe>($"Duplicata{i:000}");
                if (duplicata == null) continue;

                Duplicatas.Add(duplicata);
            } while (duplicata != null);

            i = 0;
            PagamentoNFe pag;
            do
            {
                i++;
                pag = iniData.ReadFromIni<PagamentoNFe>($"pag{i:000}");
                if (pag == null) continue;

                Pagamentos.Add(pag);
            } while (pag != null);

            iniData.ReadFromIni(DadosAdicionais, "DadosAdicionais");

            i = 0;
            InfoAdicionalNfe info;
            do
            {
                i++;
                info = iniData.ReadFromIni<InfoAdicionalNfe>($"obsCont{i:000}");
                if (info == null) continue;

                DadosAdicionais.ObsCont.Add(info);
            } while (info != null);

            i = 0;
            do
            {
                i++;
                info = iniData.ReadFromIni<InfoAdicionalNfe>($"obsFisco{i:000}");
                if (info == null) continue;

                DadosAdicionais.ObsFisco.Add(info);
            } while (info != null);

            i = 0;
            ProcRefNFe procRef;
            do
            {
                i++;
                procRef = iniData.ReadFromIni<ProcRefNFe>($"procRef{i:000}");
                if (procRef == null) continue;

                DadosAdicionais.ProcRef.Add(procRef);
            } while (procRef != null);

            iniData.ReadFromIni(Exporta, "Exporta");
            iniData.ReadFromIni(Compra, "compra");
            iniData.ReadFromIni(Cana, "cana");

            i = 0;
            ForDiaNFe forDia;
            do
            {
                i++;
                forDia = iniData.ReadFromIni<ForDiaNFe>($"forDia{i:000}");
                if (forDia == null) continue;

                Cana.forDia.Add(forDia);
            } while (forDia != null);

            i = 0;
            DeducNFe deduc;
            do
            {
                i++;
                deduc = iniData.ReadFromIni<DeducNFe>($"deduc{i:000}");
                if (deduc == null) continue;

                Cana.deduc.Add(deduc);
            } while (deduc != null);

            iniData.ReadFromIni(InfNFeSupl, "infNFeSupl");

            iniData.ReadFromIni(InfRespTec, "infRespTec");
        }

        public static NotaFiscal Load(string conteudo) => ACBrIniFile.Parse(conteudo);

        public static NotaFiscal LoadFromFile(string filePath) => ACBrIniFile.Load(filePath);

        public static NotaFiscal LoadFromFile(Stream stream) => ACBrIniFile.Load(stream);

        #endregion Methods

        #region Operators

        public static implicit operator NotaFiscal(ACBrIniFile source) => new NotaFiscal(source);

        public static implicit operator ACBrIniFile(NotaFiscal source) => source.WriteToIni();

        #endregion Operators
    }
}