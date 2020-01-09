using System;
using System.Drawing.Printing;
using System.IO;
using System.IO.Ports;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using System.Windows.Forms;
using ACBrLib.Core;
using ACBrLib.Core.DFe;
using ACBrLib.Core.NFe;
using ACBrLib.Core.PosPrinter;

namespace ACBrLib.NFe.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBrNFe ACBrNFe;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            ACBrNFe = new ACBrNFe();
        }

        private void FrmMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            ACBrNFe.Dispose();
            ACBrNFe = null;
        }

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            SplashScreenManager.Show<FrmWait>();
            SplashScreenManager.Default.ShowInfo(SplashInfo.Message, "Carregando...");

            try
            {
                cmbFormaEmissao.EnumDataSource(TipoEmissao.teNormal);
                cmbModeloDocumento.EnumDataSource(ModeloNFe.moNFe);
                cmbVersaoDF.EnumDataSource(VersaoNFe.ve400);
                cmbCrypt.EnumDataSource(SSLCryptLib.cryWinCrypt);
                cmbHttp.EnumDataSource(SSLHttpLib.httpWinHttp);
                cmbXmlSign.EnumDataSource(SSLXmlSignLib.xsLibXml2);

                cmbUfDestino.SelectedItem = "SP";
                cmbSSlType.EnumDataSource(SSLType.LT_all);

                cbbPortas.Items.AddRange(SerialPort.GetPortNames());
                cbbPortas.Items.Add(@"\\localhost\Epson");
                cbbPortas.Items.Add(@"c:\temp\posprinter.txt");

                cbbPortas.SelectedIndex = cbbPortas.Items.Count - 1;

                cbbPortas.Items.Add("TCP:192.168.0.31:9100");

                foreach (string printer in PrinterSettings.InstalledPrinters)
                    cbbPortas.Items.Add($"RAW:{printer}");

                cbbModelo.EnumDataSource(ACBrPosPrinterModelo.ppTexto);
                cbbPaginaCodigo.EnumDataSource(PosPaginaCodigo.pc850);

                // Altera as config de log
                ACBrNFe.ConfigGravarValor(ACBrSessao.Principal, "LogNivel", 4);

                var logPath = Path.Combine(Application.StartupPath, "Logs");
                if (!Directory.Exists(logPath))
                    Directory.CreateDirectory(logPath);

                ACBrNFe.ConfigGravarValor(ACBrSessao.Principal, "LogPath", logPath);
                ACBrNFe.ConfigGravar();

                LoadConfig();
            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        #endregion Constructors

        #region Methods

        private void SalvarConfig()
        {
            SplashScreenManager.Show<FrmWait>();
            SplashScreenManager.Default.ShowInfo(SplashInfo.Message, "Salvando...");

            try
            {
                //Config Geral
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "AtualizarXMLCancelado", ckbAtualizarXML.Checked);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "ExibirErroSchema", ckbExibirErroSchema.Checked);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "FormatoAlerta", txtFormatoAlerta.Text);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "FormaEmissao",
                    cmbFormaEmissao.GetSelectedValue<TipoEmissao>());
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "ModeloDF", cmbModeloDocumento.GetSelectedValue<ModeloNFe>());
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "VersaoDF", cmbVersaoDF.GetSelectedValue<VersaoNFe>());
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "RetirarAcentos", ckbRetirarAcentos.Checked);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "SalvarWS", ckbSalvar.Checked);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "PathSalvar", txtLogs.Text);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "PathSchemas", txtSchemaPath.Text);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "IdCSC", txtIdCSC.Text);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "CSC", txtCSC.Text);

                //Config Webservice
                ACBrNFe.ConfigGravarValor(ACBrSessao.DFe, "UF", cmbUfDestino.Text);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "SSLType", cmbSSlType.GetSelectedValue<SSLType>());
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "Timeout", nudTimeOut.Text);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "Ambiente",
                    rdbHomologacao.Checked ? TipoAmbiente.taHomologacao : TipoAmbiente.taProducao);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "Visualizar", ckbVisualizar.Checked);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "SalvarWS", ckbSalvarSOAP.Checked);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "AjustaAguardaConsultaRet", ckbAjustarAut.Checked);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "AguardarConsultaRet", (int)nudAguardar.Value);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "Tentativas", (int)nudTentativas.Value);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "IntervaloTentativas", (int)nudIntervalos.Value);
                ACBrNFe.ConfigGravarValor(ACBrSessao.Proxy, "Servidor", txtProxyServidor.Text);
                ACBrNFe.ConfigGravarValor(ACBrSessao.Proxy, "Porta", nudProxyPorta.Text);
                ACBrNFe.ConfigGravarValor(ACBrSessao.Proxy, "Usuario", txtProxyUsuario.Text);
                ACBrNFe.ConfigGravarValor(ACBrSessao.Proxy, "Senha", txtProxySenha.Text);

                //Config Certificado
                ACBrNFe.ConfigGravarValor(ACBrSessao.DFe, "SSLCryptLib", cmbCrypt.GetSelectedValue<SSLCryptLib>());
                ACBrNFe.ConfigGravarValor(ACBrSessao.DFe, "SSLHttpLib", cmbHttp.GetSelectedValue<SSLHttpLib>());
                ACBrNFe.ConfigGravarValor(ACBrSessao.DFe, "SSLXmlSignLib",
                    cmbXmlSign.GetSelectedValue<SSLXmlSignLib>());
                ACBrNFe.ConfigGravarValor(ACBrSessao.DFe, "ArquivoPFX", txtCertPath.Text);
                ACBrNFe.ConfigGravarValor(ACBrSessao.DFe, "DadosPFX", txtDadosPFX.Text);
                ACBrNFe.ConfigGravarValor(ACBrSessao.DFe, "Senha", txtCertPassword.Text);
                ACBrNFe.ConfigGravarValor(ACBrSessao.DFe, "NumeroSerie", txtCertNumero.Text);

                //Config Arquivos
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "SalvarGer", ckbSalvarArqs.Checked);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "SepararPorMes", ckbPastaMensal.Checked);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "AdicionarLiteral", ckbAdicionaLiteral.Checked);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "EmissaoPathNFe", ckbEmissaoPathNFe.Checked);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "SalvarArq", ckbSalvaPathEvento.Checked);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "SepararPorCNPJ", ckbSepararPorCNPJ.Checked);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "SepararPorModelo", ckbSepararPorModelo.Checked);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "PathNFe", txtArqNFe.Text);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "PathInu", txtArqInu.Text);
                ACBrNFe.ConfigGravarValor(ACBrSessao.NFe, "PathEvento", txtArqEvento.Text);

                //Config Documento Auxiliar
                ACBrNFe.ConfigGravarValor(ACBrSessao.DANFE, "PathLogo", txtLogomarca.Text);
                ACBrNFe.ConfigGravarValor(ACBrSessao.DANFE, "TipoDANFE",
                    rdbRetrato.Checked ? TipoDANFE.tiRetrato : TipoDANFE.tiPaisagem);

                var relNFCe = rdbFortes.Checked ? TipoRelatorioBobina.tpFortes :
                    rdbEscPos.Checked ? TipoRelatorioBobina.tpEscPos : TipoRelatorioBobina.tpFortesA4;
                ACBrNFe.ConfigGravarValor(ACBrSessao.DANFENFCe, "TipoRelatorioBobina", relNFCe);

                ACBrNFe.ConfigGravarValor(ACBrSessao.PosPrinter, "Modelo",
                    cbbModelo.GetSelectedValue<ACBrPosPrinterModelo>());
                ACBrNFe.ConfigGravarValor(ACBrSessao.PosPrinter, "Porta", cbbPortas.Text);
                ACBrNFe.ConfigGravarValor(ACBrSessao.PosPrinter, "ColunasFonteNormal", (int)nudColunas.Value);
                ACBrNFe.ConfigGravarValor(ACBrSessao.PosPrinter, "EspacoEntreLinhas", (int)nudEspacos.Value);
                ACBrNFe.ConfigGravarValor(ACBrSessao.PosPrinter, "LinhasBuffer", (int)nudBuffer.Value);
                ACBrNFe.ConfigGravarValor(ACBrSessao.PosPrinter, "LinhasEntreCupons", (int)nudLinhasPular.Value);
                ACBrNFe.ConfigGravarValor(ACBrSessao.PosPrinter, "ControlePorta", cbxControlePorta.Checked);
                ACBrNFe.ConfigGravarValor(ACBrSessao.PosPrinter, "CortaPapel", cbxCortarPapel.Checked);
                ACBrNFe.ConfigGravarValor(ACBrSessao.PosPrinter, "TraduzirTags", cbxTraduzirTags.Checked);
                ACBrNFe.ConfigGravarValor(ACBrSessao.PosPrinter, "IgnorarTags", cbxIgnorarTags.Checked);
                ACBrNFe.ConfigGravarValor(ACBrSessao.PosPrinter, "PaginaDeCodigo",
                    cbbPaginaCodigo.GetSelectedValue<PosPaginaCodigo>());

                //Config Email
                ACBrNFe.ConfigGravarValor(ACBrSessao.Email, "Nome", txtNome.Text);
                ACBrNFe.ConfigGravarValor(ACBrSessao.Email, "Conta", txtEmail.Text);
                ACBrNFe.ConfigGravarValor(ACBrSessao.Email, "Usuario", txtUsuario.Text);
                ACBrNFe.ConfigGravarValor(ACBrSessao.Email, "Senha", txtSenha.Text);
                ACBrNFe.ConfigGravarValor(ACBrSessao.Email, "Servidor", txtHost.Text);
                ACBrNFe.ConfigGravarValor(ACBrSessao.Email, "Porta", nudPorta.Text);
                ACBrNFe.ConfigGravarValor(ACBrSessao.Email, "SSL", ckbSSL.Checked);
                ACBrNFe.ConfigGravarValor(ACBrSessao.Email, "TLS", ckbTLS.Checked);
                ACBrNFe.ConfigGravar("");

                Application.DoEvents();
            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        private void LoadConfig()
        {
            ACBrNFe.ConfigLer();

            //Config Geral
            ckbAtualizarXML.Checked = ACBrNFe.ConfigLerValor<bool>(ACBrSessao.NFe, "AtualizarXMLCancelado");
            ckbExibirErroSchema.Checked = ACBrNFe.ConfigLerValor<bool>(ACBrSessao.NFe, "ExibirErroSchema");
            txtFormatoAlerta.Text = ACBrNFe.ConfigLerValor<string>(ACBrSessao.NFe, "FormatoAlerta");
            cmbFormaEmissao.SetSelectedValue(ACBrNFe.ConfigLerValor<TipoEmissao>(ACBrSessao.NFe, "FormaEmissao"));
            cmbModeloDocumento.SetSelectedValue(ACBrNFe.ConfigLerValor<ModeloNFe>(ACBrSessao.NFe, "ModeloDF"));
            cmbVersaoDF.SetSelectedValue(ACBrNFe.ConfigLerValor<VersaoNFe>(ACBrSessao.NFe, "VersaoDF"));
            ckbRetirarAcentos.Checked = ACBrNFe.ConfigLerValor<bool>(ACBrSessao.NFe, "RetirarAcentos");
            ckbSalvar.Checked = ACBrNFe.ConfigLerValor<bool>(ACBrSessao.NFe, "SalvarWS");
            txtLogs.Text = ACBrNFe.ConfigLerValor<string>(ACBrSessao.NFe, "PathSalvar");
            txtSchemaPath.Text = ACBrNFe.ConfigLerValor<string>(ACBrSessao.NFe, "PathSchemas");
            txtIdCSC.Text = ACBrNFe.ConfigLerValor<string>(ACBrSessao.NFe, "IdCSC");
            txtCSC.Text = ACBrNFe.ConfigLerValor<string>(ACBrSessao.NFe, "CSC");

            //Config Webservice
            cmbUfDestino.SelectedItem = ACBrNFe.ConfigLerValor<string>(ACBrSessao.DFe, "UF");
            cmbSSlType.SetSelectedValue(ACBrNFe.ConfigLerValor<SSLType>(ACBrSessao.NFe, "SSLType"));
            nudTimeOut.Value = ACBrNFe.ConfigLerValor<decimal>(ACBrSessao.NFe, "Timeout");

            var ambiente = ACBrNFe.ConfigLerValor<TipoAmbiente>(ACBrSessao.NFe, "Ambiente");
            rdbHomologacao.Checked = ambiente == TipoAmbiente.taHomologacao;
            rdbProducao.Checked = ambiente == TipoAmbiente.taProducao;

            ckbVisualizar.Checked = ACBrNFe.ConfigLerValor<bool>(ACBrSessao.NFe, "Visualizar");
            ckbSalvarSOAP.Checked = ACBrNFe.ConfigLerValor<bool>(ACBrSessao.NFe, "SalvarWS");
            ckbAjustarAut.Checked = ACBrNFe.ConfigLerValor<bool>(ACBrSessao.NFe, "AjustaAguardaConsultaRet");
            nudAguardar.Value = ACBrNFe.ConfigLerValor<int>(ACBrSessao.NFe, "AguardarConsultaRet");
            nudTentativas.Value = ACBrNFe.ConfigLerValor<int>(ACBrSessao.NFe, "Tentativas");
            nudIntervalos.Value = ACBrNFe.ConfigLerValor<int>(ACBrSessao.NFe, "IntervaloTentativas");
            txtProxyServidor.Text = ACBrNFe.ConfigLerValor<string>(ACBrSessao.Proxy, "Servidor");
            nudProxyPorta.Text = ACBrNFe.ConfigLerValor<string>(ACBrSessao.Proxy, "Porta");
            txtProxyUsuario.Text = ACBrNFe.ConfigLerValor<string>(ACBrSessao.Proxy, "Usuario");
            txtProxySenha.Text = ACBrNFe.ConfigLerValor<string>(ACBrSessao.Proxy, "Senha");

            //Config Certificado
            cmbCrypt.SetSelectedValue(ACBrNFe.ConfigLerValor<SSLCryptLib>(ACBrSessao.DFe, "SSLCryptLib"));
            cmbHttp.SetSelectedValue(ACBrNFe.ConfigLerValor<SSLHttpLib>(ACBrSessao.DFe, "SSLHttpLib"));
            cmbXmlSign.SetSelectedValue(ACBrNFe.ConfigLerValor<SSLXmlSignLib>(ACBrSessao.DFe, "SSLXmlSignLib"));
            txtCertPath.Text = ACBrNFe.ConfigLerValor<string>(ACBrSessao.DFe, "ArquivoPFX");
            txtDadosPFX.Text = ACBrNFe.ConfigLerValor<string>(ACBrSessao.DFe, "DadosPFX");
            txtCertPassword.Text = ACBrNFe.ConfigLerValor<string>(ACBrSessao.DFe, "Senha");
            txtCertNumero.Text = ACBrNFe.ConfigLerValor<string>(ACBrSessao.DFe, "NumeroSerie");

            //Config Arquivos
            ckbSalvarArqs.Checked = ACBrNFe.ConfigLerValor<bool>(ACBrSessao.NFe, "SalvarGer");
            ckbPastaMensal.Checked = ACBrNFe.ConfigLerValor<bool>(ACBrSessao.NFe, "SepararPorMes");
            ckbAdicionaLiteral.Checked = ACBrNFe.ConfigLerValor<bool>(ACBrSessao.NFe, "AdicionarLiteral");
            ckbEmissaoPathNFe.Checked = ACBrNFe.ConfigLerValor<bool>(ACBrSessao.NFe, "EmissaoPathNFe");
            ckbSalvaPathEvento.Checked = ACBrNFe.ConfigLerValor<bool>(ACBrSessao.NFe, "SalvarArq");
            ckbSepararPorCNPJ.Checked = ACBrNFe.ConfigLerValor<bool>(ACBrSessao.NFe, "SepararPorCNPJ");
            ckbSepararPorModelo.Checked = ACBrNFe.ConfigLerValor<bool>(ACBrSessao.NFe, "SepararPorModelo");
            txtArqNFe.Text = ACBrNFe.ConfigLerValor<string>(ACBrSessao.NFe, "PathNFe");
            txtArqInu.Text = ACBrNFe.ConfigLerValor<string>(ACBrSessao.NFe, "PathInu");
            txtArqEvento.Text = ACBrNFe.ConfigLerValor<string>(ACBrSessao.NFe, "PathEvento");

            //Config Documento Auxiliar
            txtLogomarca.Text = ACBrNFe.ConfigLerValor<string>(ACBrSessao.DANFE, "PathLogo");
            var tipoImpressao = ACBrNFe.ConfigLerValor<TipoDANFE>(ACBrSessao.DANFE, "TipoDANFE");
            rdbRetrato.Checked = tipoImpressao == TipoDANFE.tiRetrato;
            rdbPaisagem.Checked = tipoImpressao == TipoDANFE.tiPaisagem;

            var relNFCe = ACBrNFe.ConfigLerValor<TipoRelatorioBobina>(ACBrSessao.DANFENFCe, "TipoRelatorioBobina");
            rdbFortes.Checked = relNFCe == TipoRelatorioBobina.tpFortes;
            rdbEscPos.Checked = relNFCe == TipoRelatorioBobina.tpEscPos;
            rdbFortesA4.Checked = relNFCe == TipoRelatorioBobina.tpFortesA4;

            cbbModelo.SetSelectedValue(ACBrNFe.ConfigLerValor<ACBrPosPrinterModelo>(ACBrSessao.PosPrinter, "Modelo"));
            cbbPortas.SelectedItem = ACBrNFe.ConfigLerValor<string>(ACBrSessao.PosPrinter, "Porta");
            nudColunas.Value = ACBrNFe.ConfigLerValor<int>(ACBrSessao.PosPrinter, "ColunasFonteNormal");
            nudEspacos.Value = ACBrNFe.ConfigLerValor<int>(ACBrSessao.PosPrinter, "EspacoEntreLinhas");
            nudBuffer.Value = ACBrNFe.ConfigLerValor<int>(ACBrSessao.PosPrinter, "LinhasBuffer");
            nudLinhasPular.Value = ACBrNFe.ConfigLerValor<int>(ACBrSessao.PosPrinter, "LinhasEntreCupons");
            cbxControlePorta.Checked = ACBrNFe.ConfigLerValor<bool>(ACBrSessao.PosPrinter, "ControlePorta");
            cbxCortarPapel.Checked = ACBrNFe.ConfigLerValor<bool>(ACBrSessao.PosPrinter, "CortaPapel");
            cbxTraduzirTags.Checked = ACBrNFe.ConfigLerValor<bool>(ACBrSessao.PosPrinter, "TraduzirTags");
            cbxIgnorarTags.Checked = ACBrNFe.ConfigLerValor<bool>(ACBrSessao.PosPrinter, "IgnorarTags");
            cbbPaginaCodigo.SetSelectedValue(ACBrNFe.ConfigLerValor<PosPaginaCodigo>(ACBrSessao.PosPrinter, "PaginaDeCodigo"));

            //Config Email
            txtNome.Text = ACBrNFe.ConfigLerValor<string>(ACBrSessao.Email, "Nome");
            txtEmail.Text = ACBrNFe.ConfigLerValor<string>(ACBrSessao.Email, "Conta");
            txtUsuario.Text = ACBrNFe.ConfigLerValor<string>(ACBrSessao.Email, "Usuario");
            txtSenha.Text = ACBrNFe.ConfigLerValor<string>(ACBrSessao.Email, "Senha");
            txtHost.Text = ACBrNFe.ConfigLerValor<string>(ACBrSessao.Email, "Servidor");
            nudPorta.Value = ACBrNFe.ConfigLerValor<int>(ACBrSessao.Email, "Porta");
            ckbSSL.Checked = ACBrNFe.ConfigLerValor<bool>(ACBrSessao.Email, "SSL");
            ckbTLS.Checked = ACBrNFe.ConfigLerValor<bool>(ACBrSessao.Email, "TLS");
        }

        private void CheckNFeLista(bool xml = false)
        {
            if (MessageBox.Show(@"Limpar a lista ?", @"ACBrLibNFe", MessageBoxButtons.YesNo) == DialogResult.Yes)
                ACBrNFe.LimparLista();

            if (xml)
                CarregarNFeXml();
            else
                CarregarNFeIni();
        }

        private void CarregarNFeIni()
        {
            var arquivoIni = Helpers.OpenFile("Arquivo Ini NFe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(arquivoIni)) return;

            ACBrNFe.CarregarINI(arquivoIni);
        }

        private void CarregarNFeXml()
        {
            var arquivoIni = Helpers.OpenFile("Arquivo Xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(arquivoIni)) return;

            ACBrNFe.CarregarXML(arquivoIni);
        }

        #endregion Methods

        #region EventHandlers

        private void btnSelectLog_Click(object sender, EventArgs e)
        {
            txtLogs.Text = Helpers.SaveFile("Arquivos Logs (*.log)|*.log|Todos os Arquivos (*.*)|*.*");
        }

        private void BtnSelectSchema_Click(object sender, EventArgs e)
        {
            txtSchemaPath.Text = Helpers.SelectFolder();
        }

        private void BtnSelecionarCertificado_Click(object sender, EventArgs e)
        {
            txtCertPath.Text = Helpers.OpenFile("Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*");
        }

        private void btnDadosPFX_Click(object sender, EventArgs e)
        {
            if (!string.IsNullOrEmpty(txtDadosPFX.Text))
            {
                var cert = new X509Certificate2(Convert.FromBase64String(txtDadosPFX.Text), txtCertPassword.Text);
                MessageBox.Show(cert.SerialNumber);
            }

            var file = Helpers.OpenFile("Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*");
            if (!File.Exists(file)) return;

            var dados = File.ReadAllBytes(file);
            txtDadosPFX.Text = Convert.ToBase64String(dados);
        }

        private void btnObterCertificados_Click(object sender, EventArgs e)
        {
            var ret = ACBrNFe.ObterCertificados();
            rtbRespostas.AppendLine(ret.Select(x => x.ToString()).ToArray());
        }

        private void btnArqNFe_Click(object sender, EventArgs e)
        {
            txtArqNFe.Text = Helpers.SelectFolder();
        }

        private void btnArqEvento_Click(object sender, EventArgs e)
        {
            txtArqEvento.Text = Helpers.SelectFolder();
        }

        private void btnArqInu_Click(object sender, EventArgs e)
        {
            txtArqInu.Text = Helpers.SelectFolder();
        }

        private void btnLogomarca_Click(object sender, EventArgs e)
        {
            txtLogomarca.Text = Helpers.OpenFile("Image files (*.bmp, *.jpeg, *.png) | *.bmp; *.jpeg; *.png");
        }

        private void BtnSalvar_Click(object sender, EventArgs e)
        {
            try
            {
                SalvarConfig();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnStatusServ_Click(object sender, EventArgs e)
        {
            try
            {
                rtbRespostas.AppendText(ACBrNFe.StatusServico());
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnGerarXml_Click(object sender, EventArgs e)
        {
            try
            {
                ACBrNFe.LimparLista();
                CarregarNFeIni();

                ACBrNFe.Assinar();
                var ret = ACBrNFe.ObterXml(0);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarSincrono_Click(object sender, EventArgs e)
        {
            try
            {
                CheckNFeLista();

                var aLote = 1;
                if (InputBox.Show("WebServices Enviar", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var ret = ACBrNFe.Enviar(aLote, sincrono: true);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarAssincrono_Click(object sender, EventArgs e)
        {
            try
            {
                CheckNFeLista();

                var aLote = 1;
                if (InputBox.Show("WebServices Enviar", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var ret = ACBrNFe.Enviar(aLote);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCarregarIni_Click(object sender, EventArgs e)
        {
            try
            {
                CheckNFeLista();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCarregarXml_Click(object sender, EventArgs e)
        {
            try
            {
                CheckNFeLista(true);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnLimparLista_Click(object sender, EventArgs e)
        {
            try
            {
                if (MessageBox.Show(@"Limpar a lista ?", @"ACBrLibNFe", MessageBoxButtons.YesNo) == DialogResult.Yes)
                    ACBrNFe.LimparLista();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimir_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrNFe.LimparLista();
                ACBrNFe.CarregarXML(arquivoXml);
                ACBrNFe.Imprimir(bMostrarPreview: true);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimirPDF_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrNFe.LimparLista();
                ACBrNFe.CarregarXML(arquivoXml);
                ACBrNFe.Imprimir(bMostrarPreview: true);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarEmail_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xmnl NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                var destinatario = "";
                if (InputBox.Show("Envio email", "Digite o email do destinatario", ref destinatario) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(destinatario)) return;

                ACBrNFe.EnviarEmail(destinatario, arquivoXml, true, txtAssunto.Text, txtMensagem.Text);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnAssinar_Click(object sender, EventArgs e)
        {
            try
            {
                CheckNFeLista(true);

                ACBrNFe.Assinar();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnValidarRegra_Click(object sender, EventArgs e)
        {
            try
            {
                CheckNFeLista(true);

                rtbRespostas.AppendText(ACBrNFe.ValidarRegrasdeNegocios());
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultaXml_Click(object sender, EventArgs e)
        {
            try
            {
                var chaveOuNFe = Helpers.OpenFile("Arquivo Xmnl NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(chaveOuNFe)) return;

                ACBrNFe.LimparLista();

                var ret = ACBrNFe.Consultar(chaveOuNFe);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultaChave_Click(object sender, EventArgs e)
        {
            try
            {
                var chaveOuNFe = "";
                if (InputBox.Show("WebServices Consultar", "Chave da NF-e:", ref chaveOuNFe) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(chaveOuNFe)) return;

                ACBrNFe.LimparLista();
                var ret = ACBrNFe.Consultar(chaveOuNFe);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarRecibo_Click(object sender, EventArgs e)
        {
            try
            {
                var aRecibo = "";
                if (InputBox.Show("WebServices Consultar: Recibo", "Número do recibo.", ref aRecibo) != DialogResult.OK) return;

                var ret = ACBrNFe.ConsultarRecibo(aRecibo);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarCadastro_Click(object sender, EventArgs e)
        {
            try
            {
                var uf = "SP";
                var documento = "";
                var ie = false;

                if (InputBox.Show("WebServices Consultar: Cadastro", "UF", ref uf) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Consultar: Cadastro", "Documento", ref documento) != DialogResult.OK) return;
                ie = MessageBox.Show(@"O documento é uma inscrição estadual ?", @"WebServices Consultar: Cadastro", MessageBoxButtons.YesNo) == DialogResult.Yes;

                var ret = ACBrNFe.ConsultaCadastro(uf, documento, ie);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCancelar_Click(object sender, EventArgs e)
        {
            try
            {
                var idLote = 1;
                var aJustificativa = "";
                var eChave = "";
                var eCNPJ = "";
                if (InputBox.Show("WebServices Eventos: Cancelamento", "Identificador de controle do Lote de envio do Evento", ref idLote) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Cancelamento", "Chave da NF-e", ref eChave) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Cancelamento", "CNPJ ou o CPF do autor do Evento", ref eCNPJ) != DialogResult.OK) return;
                if (InputBox.Show("WebServices Eventos: Cancelamento", "Justificativa do Cancelamento", ref aJustificativa) != DialogResult.OK) return;

                var ret = ACBrNFe.Cancelar(eChave, aJustificativa, eCNPJ, idLote);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarEvento_Click(object sender, EventArgs e)
        {
            try
            {
                var idLote = 1;
                if (InputBox.Show("WebServices Eventos: Enviar", "Identificador de controle do Lote de envio do Evento", ref idLote) != DialogResult.OK) return;

                var ret = ACBrNFe.EnviarEvento(idLote);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCarregarEvento_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoIni = Helpers.OpenFile("Arquivo Ini NFe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoIni)) return;

                ACBrNFe.CarregarEventoINI(arquivoIni);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnLimparListaEvento_Click(object sender, EventArgs e)
        {
            try
            {
                if (MessageBox.Show(@"Limpar a lista de eventos ?", @"ACBrLibNFe", MessageBoxButtons.YesNo) == DialogResult.Yes)
                    ACBrNFe.LimparListaEventos();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimirEvento_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXmlEvento = Helpers.OpenFile("Arquivo Xml Evento (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXmlEvento)) return;

                var arquivoXml = Helpers.OpenFile("Arquivo Xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrNFe.ImprimirEvento(arquivoXml, arquivoXmlEvento);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimirEventoPDF_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXmlEvento = Helpers.OpenFile("Arquivo Xml Evento (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXmlEvento)) return;

                var arquivoXml = Helpers.OpenFile("Arquivo Xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrNFe.ImprimirEventoPDF(arquivoXml, arquivoXmlEvento);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarEmailEvento_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXmlEvento = Helpers.OpenFile("Arquivo Xml Evento (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXmlEvento)) return;

                var arquivoXml = Helpers.OpenFile("Arquivo Xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                var destinatario = "";
                if (InputBox.Show("Envio email", "Digite o email do destinatario", ref destinatario) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(destinatario)) return;

                ACBrNFe.EnviarEmailEvento(destinatario, arquivoXmlEvento, arquivoXml, true, txtAssunto.Text, txtMensagem.Text);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnInutilizar_Click(object sender, EventArgs e)
        {
            try
            {
                var ano = 1;
                var modelo = 57;
                var serie = 1;
                var numeroInicial = 1;
                var numeroFinal = 1;
                var aJustificativa = "";
                var eCNPJ = "";
                if (InputBox.Show("WebServices: Inutilização", "Ano", ref ano) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Inutilização", "Modelo", ref modelo) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Inutilização", "Serie", ref serie) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Inutilização", "Número Inicial", ref numeroInicial) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Inutilização", "Número Final", ref numeroFinal) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Inutilização", "CNPJ ou o CPF do emitente", ref eCNPJ) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Inutilização", "Justificativa", ref aJustificativa) != DialogResult.OK) return;

                var ret = ACBrNFe.Inutilizar(eCNPJ, aJustificativa, ano, modelo, serie, numeroInicial, numeroFinal);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimirInutilizacao_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xml Inutilização (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrNFe.ImprimirInutilizacao(arquivoXml);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimirInutilizacaoPDF_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xml Inutilização (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                ACBrNFe.ImprimirInutilizacaoPDF(arquivoXml);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnDFePorChave_Click(object sender, EventArgs e)
        {
            try
            {
                var codUf = 35;
                var cnpj = "";
                var chave = "";

                if (InputBox.Show("WebServices: Distribuição DFe", "Código da UF", ref codUf) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Distribuição DFe", "CNPJ do autor", ref cnpj) != DialogResult.OK) return;
                if (InputBox.Show("WebServices: Distribuição DFe", "Chave da NFe", ref chave) != DialogResult.OK) return;

                var ret = ACBrNFe.DistribuicaoDFePorChave(codUf, cnpj, chave);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnDFePorNSU_Click(object sender, EventArgs e)
        {
            var codUf = 35;
            var cnpj = "";
            var eNsu = "";

            if (InputBox.Show("WebServices: Distribuição DFe", "Código da UF", ref codUf) != DialogResult.OK) return;
            if (InputBox.Show("WebServices: Distribuição DFe", "CNPJ do autor", ref cnpj) != DialogResult.OK) return;
            if (InputBox.Show("WebServices: Distribuição DFe", "Número do NSU", ref eNsu) != DialogResult.OK) return;

            var ret = ACBrNFe.DistribuicaoDFePorNSU(codUf, cnpj, eNsu);
            rtbRespostas.AppendText(ret);
        }

        private void btnDFePorUltNSU_Click(object sender, EventArgs e)
        {
            var codUf = 35;
            var cnpj = "";
            var eNsu = "";

            if (InputBox.Show("WebServices: Distribuição DFe", "Código da UF", ref codUf) != DialogResult.OK) return;
            if (InputBox.Show("WebServices: Distribuição DFe", "CNPJ do autor", ref cnpj) != DialogResult.OK) return;
            if (InputBox.Show("WebServices: Distribuição DFe", "Número do último NSU", ref eNsu) != DialogResult.OK) return;

            var ret = ACBrNFe.DistribuicaoDFePorNSU(codUf, cnpj, eNsu);
            rtbRespostas.AppendText(ret);
        }

        #endregion EventHandlers
    }
}