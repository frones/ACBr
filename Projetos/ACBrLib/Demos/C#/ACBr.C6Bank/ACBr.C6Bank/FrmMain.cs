using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using ACBrLib;
using ACBrLib.Core;
using ACBrLib.Core.Boleto;
using ACBrLib.Core.PIXCD;
using ACBrLib.Boleto;
using ACBrLib.PIXCD;
using ACBrLib.Core.DFe;
using System.IO;
using System.IO.Ports;
using System.Drawing.Printing;
using System.Reflection;
using System.Globalization;


namespace ACBr.C6Bank
{
    public partial class FrmMain : Form
    {
        private ACBrBoleto boleto;
        private ACBrPIXCD pixcd;
        
        public FrmMain()
        {
            InitializeComponent();

            boleto = new ACBrBoleto();
            pixcd = new ACBrPIXCD();

        }

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            
            tabBoleto.Visible = false;
            tabPIXCD.Visible = false;

            SplashScreenManager.Show<FrmWait>();
            SplashScreenManager.ShowInfo(SplashInfo.Message, "Carregando Informações...");

            try
            {
                //Config Boleto
                cmbTipoInscricao.EnumDataSource(ACBrPessoa.pJuridica);
                cmbTipoDocumento.EnumDataSource(ACBrTipoDocumento.Tradicional);
                cmbTipoCarteira.EnumDataSource(ACBrTipoCarteira.tctSimples);
                cmbModeloImpressao.EnumDataSource(ACBrBolLayOut.lPadrao);
                cmbBanco.EnumDataSource(ACBrTipoCobranca.cobBancoC6);
                cmbRespEmissao.EnumDataSource(ACBrResponEmissao.tbCliEmite);
                cmbLayoutCNAB.EnumDataSource(ACBrLayoutRemessa.c400);
                cmbCaracTitulo.EnumDataSource(CaracTitulo.tcSimples);
                cmbTipoChavePIXBoleto.EnumDataSource(TipoChavePIX.tchNenhuma);
                cmbSSlType.EnumDataSource(SSLType.LT_TLSv1_2);
                cmbHttp.EnumDataSource(SSLHttpLib.httpOpenSSL);
                cmbOperacao.EnumDataSource(OperacaoBoleto.tpInclui);
                cmbTipoDesconto.EnumDataSource(TipoDesconto.tdNaoConcederDesconto);
                cmbTipoDiasIntrucao.EnumDataSource(TipoDiasIntrucao.diCorridos);
                cmbCodigoNegativacao.EnumDataSource(CodigoNegativacao.cnNenhum);
                cmbTipoDiasNegativacao.EnumDataSource(TipoDiasIntrucao.diCorridos);
                string basePath = AppDomain.CurrentDomain.BaseDirectory;
                string relativePath = Path.Combine("..", "..", "img");
                string imagePath = Path.GetFullPath(Path.Combine(basePath, relativePath));
                txtDirLogo.Text = imagePath;
                ckbMultaValorFixo.Checked = false;
                chkPreview.Checked = true;


                cmbImpressora.Items.AddRange(SerialPort.GetPortNames());
                cmbImpressora.Items.Add(@"\\localhost\Epson");
                cmbImpressora.Items.Add(@"c:\temp\boleto.txt");

                cmbImpressora.SelectedIndex = cmbImpressora.Items.Count - 1;

                cmbImpressora.Items.Add("TCP:192.168.0.31:9100");

                foreach (string printer in PrinterSettings.InstalledPrinters)
                    cmbImpressora.Items.Add($"RAW:{printer}");

                //Config PIXCD
                cmbPSP.EnumDataSource(PSP.pspC6Bank);
                cmbAmbiente.EnumDataSource(Ambiente.ambTeste);
                pixcd.Config.BancoBrasil.BBAPIVersao = BBAPIVersao.apiVersao2;
                cmbNivelLogPSP.EnumDataSource(NivelLogPSP.logPSPNenhum);
                cmbTipoChavePIXCD.EnumDataSource(TipoChave.tchNenhuma);
                pixcd.Config.ProxyPort = 0;


                //Configuração Log ACBrLib
                boleto.Config.Webservice.LogNivel = NivelLog.logParanoico;

                boleto.Config.Principal.LogNivel = NivelLog.logParanoico;
                var logPath = Path.Combine(Application.StartupPath, "Logs");
                if (!Directory.Exists(logPath))
                    Directory.CreateDirectory(logPath);

                boleto.Config.Principal.LogPath = logPath;
                
                LoadConfigBoleto();
                LoadConfigPIXCD();
            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        private void FrmMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            boleto.Dispose();
            pixcd.Dispose();
        }

        private void btnConfiguracoesBoleto_Click(object sender, EventArgs e)
        {
            tabPIXCD.Visible = false;
            tabBoleto.Visible = true;
            lblmsginicio.Visible = false;
        }

        private void btnConfiguracoesPIX_Click(object sender, EventArgs e)
        {
            tabBoleto.Visible = false;
            tabPIXCD.Visible = true;
            lblmsginicio.Visible = false;
        }

        private void btnSair_Click(object sender, EventArgs e)
        {
            DialogResult dialog = new DialogResult();   
            dialog = MessageBox.Show("Deseja mesmo sair da aplicação ?", "Atenção !!!", MessageBoxButtons.YesNo);
            if (dialog == DialogResult.Yes)
            {
                Application.Exit();
            }
        }

        private void btnSairTabBoleto_Click(object sender, EventArgs e)
        {
            tabBoleto.Visible = false;
        }

        private void btnSairTabPIXCD_Click(object sender, EventArgs e)
        {
            tabPIXCD.Visible = false;
        }

        private void btnSalvarConfiguracoesBoleto_Click(object sender, EventArgs e)
        {
            SaveConfigBoleto();
        }

        private void SaveConfigBoleto()
        {
            SplashScreenManager.Show<FrmWait>();
            SplashScreenManager.ShowInfo(SplashInfo.Message, "Salvando...");

            try
            {
                //Salvar primeiro Tipo de Inscrição depois CNPJ ou CPF.
                boleto.Config.Cedente.TipoInscricao = cmbTipoInscricao.GetSelectedValue<ACBrPessoa>();
                boleto.Config.Cedente.CNPJCPF = txtCNPJCPFCedente.Text;
                //
                boleto.Config.Impressao.MostrarPreview = chkPreview.Checked;
                boleto.Config.Impressao.MostrarProgresso = chkProgresso.Checked;
                boleto.Config.Impressao.MostrarSetup = chkSetup.Checked;
                boleto.Config.Impressao.NomeArquivo = txtNomeArquivo.Text;
                boleto.Config.Impressao.NumeroCopias = (int)nudCopias.Value;
                boleto.Config.Impressao.PrinterName = cmbImpressora.Text;
                boleto.Config.Impressao.DirLogo = txtDirLogo.Text;
                boleto.Config.Banco.TipoCobranca = cmbBanco.GetSelectedValue<ACBrTipoCobranca>();
                boleto.Config.Banco.LayoutVersaoArquivo = int.Parse(txtVersaoArquivo.Text);
                boleto.Config.Banco.LayoutVersaoLote = int.Parse(txtVersaoLote.Text);
                boleto.Config.Banco.LocalPagamento = txtLocalPagamento.Text;
                boleto.Config.Cedente.TipoCarteira = cmbTipoCarteira.GetSelectedValue<ACBrTipoCarteira>();
                boleto.Config.Cedente.TipoDocumento = cmbTipoDocumento.GetSelectedValue<ACBrTipoDocumento>();
                boleto.Config.Cedente.CaracTitulo = cmbCaracTitulo.GetSelectedValue<CaracTitulo>();
                boleto.Config.Cedente.TipoChavePIX = cmbTipoChavePIXBoleto.GetSelectedValue<TipoChavePIX>();
                boleto.Config.Cedente.ChavePIX = txtChavePIX.Text;
                boleto.Config.Cedente.Agencia = txtAgencia.Text;
                boleto.Config.Cedente.AgenciaDigito = txtDigAgencia.Text;
                boleto.Config.Cedente.Conta = txtConta.Text;
                boleto.Config.Cedente.ContaDigito = txtDigConta.Text;
                boleto.Config.Cedente.CodigoTransmissao = txtCodTransmissao.Text;
                boleto.Config.Cedente.Convenio = txtConvenio.Text;
                boleto.Config.Cedente.Modalidade = txtModalidade.Text;
                boleto.Config.Cedente.CodigoCedente = txtCodCedente.Text;
                boleto.Config.Cedente.ResponEmissao = cmbRespEmissao.GetSelectedValue<ACBrResponEmissao>();
                boleto.Config.Cedente.Bairro = txtBairro.Text;
                boleto.Config.Cedente.CEP = txtCEP.Text;
                boleto.Config.Cedente.Cidade = txtCidade.Text;
                boleto.Config.Cedente.Complemento = txtComplemento.Text;
                boleto.Config.Cedente.Logradouro = txtLogradouroCedente.Text;
                boleto.Config.Cedente.Nome = txtNomeRes.Text;
                boleto.Config.Cedente.NumeroRes = txtNumeroRes.Text;
                boleto.Config.Cedente.Telefone = txtTelefone.Text;
                boleto.Config.Cedente.UF = cmbUF.Text;
                boleto.Config.Diretorio.DirArqRemessa = txtDirRemessa.Text;
                boleto.Config.Diretorio.DirArqRetorno = txtDirRetorno.Text;
                boleto.Config.Diretorio.LayoutRemessa = cmbLayoutCNAB.GetSelectedValue<ACBrLayoutRemessa>();
                boleto.Config.Diretorio.NomeArqRemessa = txtNomeRemessa.Text;
                boleto.Config.Diretorio.NomeArqRetorno = txtNomeRetorno.Text;
                boleto.Config.Diretorio.LeCedenteRetorno = ckbCedenteRetorno.Checked;
                boleto.Config.Email.Nome = txtNome.Text;
                boleto.Config.Email.Conta = txtEmail.Text;
                boleto.Config.Email.Usuario = txtUsuario.Text;
                boleto.Config.Email.Senha = txtSenha.Text;
                boleto.Config.Email.Servidor = txtHost.Text;
                boleto.Config.Email.Porta = nudPorta.Text;
                boleto.Config.Email.SSL = ckbSSL.Checked;
                boleto.Config.Email.TLS = ckbTLS.Checked;

                boleto.Config.CedenteWebservice.ClientID = txtClientID.Text;
                boleto.Config.CedenteWebservice.ClientSecret = txtClientSecret.Text;
                boleto.Config.CedenteWebservice.KeyUser = txtKeyUser.Text;
                boleto.Config.CedenteWebservice.Scope = txtScope.Text;
                boleto.Config.CedenteWebservice.IndicadorPix = chkIndicadorPix.Checked;

                boleto.Config.Webservice.PathGravarRegistro = txtPathLog.Text;
                boleto.Config.Webservice.NomeArquivoLog = txtNomeArquivoLog.Text;
                boleto.Config.Webservice.Ambiente = rdbProducao.Checked ? AmbienteWebservice.Homologaçao : AmbienteWebservice.Producao;
                boleto.Config.Webservice.Operacao = cmbOperacao.GetSelectedValue<OperacaoBoleto>();
                boleto.Config.Webservice.SSLType = cmbSSlType.GetSelectedValue<SSLType>();
                boleto.Config.DFe.SSLHttpLib = cmbHttp.GetSelectedValue<SSLHttpLib>();
                boleto.Config.Webservice.VersaoDF = txtVersao.Text;
                boleto.Config.Webservice.Timeout = (int)nudTimeOut.Value;
                boleto.Config.Webservice.ArquivoCRT = txtArquivoCRT.Text;
                boleto.Config.Webservice.ArquivoKEY = txtArquivoKEY.Text;

                boleto.ConfigGravar();

                Application.DoEvents();
            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        private void btnCarregarConfiguracoesBoleto_Click(object sender, EventArgs e)
        {
            var file = Helpers.OpenFile("Arquivos Ini (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
            if (!File.Exists(file)) return;
            LoadConfigBoleto(file);
        }

        private void LoadConfigBoleto(string file = "ACBrLib.ini")
        {
            boleto.ConfigLer();

            cmbModeloImpressao.SetSelectedValue(boleto.Config.Impressao.Layout);
            chkPreview.Checked = boleto.Config.Impressao.MostrarPreview;
            chkProgresso.Checked = boleto.Config.Impressao.MostrarProgresso;
            chkSetup.Checked = boleto.Config.Impressao.MostrarSetup;
            txtNomeArquivo.Text = boleto.Config.Impressao.NomeArquivo;
            nudCopias.Value = boleto.Config.Impressao.NumeroCopias;
            cmbImpressora.SelectedItem = boleto.Config.Impressao.PrinterName;
            txtDirLogo.Text = boleto.Config.Impressao.DirLogo;
            cmbBanco.SetSelectedValue(boleto.Config.Banco.TipoCobranca);
            txtAgencia.Text = boleto.Config.Cedente.Agencia;
            txtDigAgencia.Text = boleto.Config.Cedente.AgenciaDigito;
            txtDigAgencia.Text = boleto.Config.Cedente.AgenciaDigito;
            txtConta.Text = boleto.Config.Cedente.Conta;
            txtDigConta.Text = boleto.Config.Cedente.ContaDigito;
            txtCodTransmissao.Text = boleto.Config.Cedente.CodigoTransmissao;
            txtConvenio.Text = boleto.Config.Cedente.Convenio;
            txtModalidade.Text = boleto.Config.Cedente.Modalidade;
            txtCodCedente.Text = boleto.Config.Cedente.CodigoCedente;
            txtVersaoArquivo.Text = boleto.Config.Banco.LayoutVersaoArquivo.ToString();
            txtVersaoLote.Text = boleto.Config.Banco.LayoutVersaoLote.ToString();
            txtLocalPagamento.Text = boleto.Config.Banco.LocalPagamento;
            cmbRespEmissao.SetSelectedValue(boleto.Config.Cedente.ResponEmissao);
            txtBairro.Text = boleto.Config.Cedente.Bairro;
            txtCEP.Text = boleto.Config.Cedente.CEP;
            txtCidade.Text = boleto.Config.Cedente.Cidade;
            txtCNPJCPFCedente.Text = boleto.Config.Cedente.CNPJCPF;
            txtComplemento.Text = boleto.Config.Cedente.Complemento;
            txtLogradouroCedente.Text = boleto.Config.Cedente.Logradouro;
            txtNomeRes.Text = boleto.Config.Cedente.Nome;
            txtNumeroRes.Text = boleto.Config.Cedente.NumeroRes;
            txtTelefone.Text = boleto.Config.Cedente.Telefone;
            cmbUF.SelectedItem = boleto.Config.Cedente.UF;
            cmbTipoCarteira.SetSelectedValue(boleto.Config.Cedente.TipoCarteira);
            cmbTipoDocumento.SetSelectedValue(boleto.Config.Cedente.TipoDocumento);
            cmbCaracTitulo.SetSelectedValue(boleto.Config.Cedente.CaracTitulo);
            cmbTipoInscricao.SetSelectedValue(boleto.Config.Cedente.TipoInscricao);
            cmbTipoChavePIXBoleto.SetSelectedValue(boleto.Config.Cedente.TipoChavePIX);
            txtChavePIX.Text = boleto.Config.Cedente.ChavePIX;
            txtDirRemessa.Text = boleto.Config.Diretorio.DirArqRemessa;
            txtDirRetorno.Text = boleto.Config.Diretorio.DirArqRetorno;
            cmbLayoutCNAB.SetSelectedValue(boleto.Config.Diretorio.LayoutRemessa);
            txtNomeRemessa.Text = boleto.Config.Diretorio.NomeArqRemessa;
            txtNomeRetorno.Text = boleto.Config.Diretorio.NomeArqRetorno;
            ckbCedenteRetorno.Checked = boleto.Config.Diretorio.LeCedenteRetorno;
            txtNome.Text = boleto.Config.Email.Nome;
            txtEmail.Text = boleto.Config.Email.Conta;
            txtUsuario.Text = boleto.Config.Email.Usuario;
            txtSenha.Text = boleto.Config.Email.Senha;
            txtHost.Text = boleto.Config.Email.Servidor;
            nudPorta.Text = boleto.Config.Email.Porta;
            ckbSSL.Checked = boleto.Config.Email.SSL;
            ckbTLS.Checked = boleto.Config.Email.TLS;

            txtClientID.Text = boleto.Config.CedenteWebservice.ClientID;
            txtClientSecret.Text = boleto.Config.CedenteWebservice.ClientSecret;
            txtKeyUser.Text = boleto.Config.CedenteWebservice.KeyUser;
            txtScope.Text = boleto.Config.CedenteWebservice.Scope;
            chkIndicadorPix.Checked = boleto.Config.CedenteWebservice.IndicadorPix;

            txtPathLog.Text = boleto.Config.Webservice.PathGravarRegistro;
            txtNomeArquivoLog.Text = boleto.Config.Webservice.NomeArquivoLog;
            var ambiente = boleto.Config.Webservice.Ambiente;
            rdbProducao.Checked = ambiente == AmbienteWebservice.Homologaçao;
            rdbHomologacao.Checked = ambiente == AmbienteWebservice.Producao;

            cmbOperacao.SetSelectedValue(boleto.Config.Webservice.Operacao);
            cmbSSlType.SetSelectedValue(boleto.Config.Webservice.SSLType);
            cmbHttp.SetSelectedValue(boleto.Config.DFe.SSLHttpLib);
            txtVersao.Text = boleto.Config.Webservice.VersaoDF;
            nudTimeOut.Value = boleto.Config.Webservice.Timeout;
            txtArquivoCRT.Text = boleto.Config.Webservice.ArquivoCRT;
            txtArquivoKEY.Text = boleto.Config.Webservice.ArquivoKEY;
        }

        private void btnSalvarConfiguracoesPIXCD_Click(object sender, EventArgs e)
        {
            SaveConfigPIXCD();
        }

        private void SaveConfigPIXCD()
        {
            SplashScreenManager.Show<FrmWait>();
            SplashScreenManager.ShowInfo(SplashInfo.Message, "Salvando...");

            try
            {
                //PIXCD
                pixcd.Config.Ambiente = cmbAmbiente.GetSelectedValue<Ambiente>();
                pixcd.Config.ArqLog = txtArqLogPSP.Text;
                pixcd.Config.NivelLog = cmbNivelLogPSP.GetSelectedValue<NivelLogPSP>();
                pixcd.Config.TipoChave = cmbTipoChavePIXCD.GetSelectedValue<TipoChave>();
                pixcd.Config.PSP = cmbPSP.GetSelectedValue<PSP>();
                pixcd.Config.Timeout = (int)nudPSPTimeout.Value;
                pixcd.Config.ProxyHost = txtProxyServidor.Text;
                pixcd.Config.ProxyPass = txtProxySenha.Text;
                pixcd.Config.ProxyPort = (int)nudProxyPorta.Value;
                pixcd.Config.ProxyUser = txtProxyUsuario.Text;
                pixcd.Config.CEPRecebedor = txtCEPRecebedor.Text;
                pixcd.Config.CidadeRecebedor = txtCidadeRecebedor.Text;
                pixcd.Config.NomeRecebedor = txtNomeRecebedor.Text;
                pixcd.Config.UFRecebedor = txtUFRecebedor.Text;


                Application.DoEvents();
            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        private void btnCarregarConfiguracoesPIXCD_Click(object sender, EventArgs e)
        {
            var file = Helpers.OpenFile("Arquivos Ini (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
            if (!File.Exists(file)) return;

            LoadConfigPIXCD(file);
        }

        private void LoadConfigPIXCD(string file = "ACBrLib.ini") 
        {
            pixcd.ConfigLer(file);

            //PIXCD
            cmbAmbiente.SetSelectedValue(pixcd.Config.Ambiente);
            txtArqLogPSP.Text = pixcd.Config.ArqLog;
            cmbNivelLogPSP.SetSelectedValue(pixcd.Config.NivelLog);
            cmbTipoChavePIXCD.SetSelectedValue(pixcd.Config.TipoChave);
            cmbPSP.SetSelectedValue(pixcd.Config.PSP);
            nudPSPTimeout.Value = pixcd.Config.Timeout;
            txtProxyServidor.Text = pixcd.Config.ProxyHost;
            txtProxySenha.Text = pixcd.Config.ProxyPass;
            nudProxyPorta.Value = pixcd.Config.ProxyPort;
            txtProxyUsuario.Text = pixcd.Config.ProxyUser;
            txtCEPRecebedor.Text = pixcd.Config.CEPRecebedor;
            txtCidadeRecebedor.Text = pixcd.Config.CidadeRecebedor;
            txtNomeRecebedor.Text = pixcd.Config.NomeRecebedor;
            txtUFRecebedor.Text = pixcd.Config.UFRecebedor;
        }

        private void btnDirLogo_Click(object sender, EventArgs e)
        {
            txtDirLogo.Text = Helpers.SelectFolder();
        }

        private void btnDirRemessa_Click(object sender, EventArgs e)
        {
            txtDirRemessa.Text = Helpers.SelectFolder();
        }

        private void btnDirRetorno_Click(object sender, EventArgs e)
        {
            txtDirRetorno.Text = Helpers.SelectFolder();
        }

        private void btnArquivoCRT_Click(object sender, EventArgs e)
        {
            txtArquivoCRT.Text = Helpers.OpenFile("Arquivos CRT (*.crt)|*.crt|Todos os Arquivos (*.*)|*.*");
        }

        private void btnArquivoKEY_Click(object sender, EventArgs e)
        {
            txtArquivoKEY.Text = Helpers.OpenFile("Arquivos KEY (*.key)|*.key|Todos os Arquivos (*.*)|*.*");
        }

        private void btnPathLog_Click(object sender, EventArgs e)
        {
            txtPathLog.Text = Helpers.SelectFolder();
        }

        private void btnArquivoChavePrivadaC6Bank_Click(object sender, EventArgs e)
        {
            txtArquivoChavePrivadaC6Bank.Text = Helpers.OpenFile("Arquivos KEY (*.key)|*.key|Todos os Arquivos (*.*)|*.*");
        }

        private void btnArquivoCertificadoC6Bank_Click(object sender, EventArgs e)
        {
            txtArquivoCertificadoC6Bank.Text = Helpers.OpenFile("Arquivos CER (*.cer)|*.cer|Todos os Arquivos (*.*)|*.*");
        }

        private void btnConsultarPix_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = pixcd.ConsultarPix(txte2eidConsultarPIX.Text);
                rtbRespostasPIXCD.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarPixRecebidos_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = pixcd.ConsultarPixRecebidos(DateTime.Parse(txtDataInicialConsultarPIXRecebidos.Text), DateTime.Parse(txtDataFinalConsultarPIXRecebidos.Text), txtTxIdConsultarPIXRecebidos.Text, txtCPFCNPJConsultarPIXRecebidos.Text, (int)nudPagAtualConsultarPIXRecebidos.Value, (int)nudItensPorPaginaConsultarPIXRecebidos.Value);
                rtbRespostasPIXCD.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnSolicitarDevolucaoPix_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = pixcd.SolicitarDevolucaoPix(rtbSolicitarDevolucaoPIX.Text, txte2eidSolicitarDevolucaoPIX.Text, txtIdDevolucaoSolicitarDevolucaoPIX.Text);
                rtbRespostasPIXCD.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarDevolucaoPix_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = pixcd.ConsultarDevolucaoPix(txte2eidConsultarDevolucaoPIX.Text, txtIdDevolucaoConsultarDevolucaoPIX.Text);
                rtbRespostasPIXCD.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCriarCobrancaImediata_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = pixcd.CriarCobrancaImediata(rtbCriarCobrancaImediata.Text, txtTxIdCriarCobrancaImediata.Text);
                rtbRespostasPIXCD.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarCobrancaImediata_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = pixcd.ConsultarCobrancaImediata(txtTxIdConsultarCobrancaImediata.Text, int.Parse(txtRevisaoConsultarCobrancaImediata.Text));
                rtbRespostasPIXCD.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarCobrancasCob_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = pixcd.ConsultarCobrancasCob(DateTime.Parse(txtDataInicialConsultarCobrancasCob.Text), DateTime.Parse(txtDataFinalConsultarCobrancasCob.Text), txtCPFCNPJConsultarCobrancasCob.Text, ckbLocationConsultarCobrancasCob.Checked, cmbStatusConsultarCobrancasCob.SelectedIndex, (int)nudPagAtualConsultarCobrancasCob.Value, (int)nudItensPorPaginaConsultarCobrancasCob.Value);
                rtbRespostasPIXCD.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnRevisarCobrancaImediata_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = pixcd.RevisarCobrancaImediata(rtbRevisarCobrancaImediata.Text, txtTxIdRevisarCobrancaImediata.Text);
                rtbRespostasPIXCD.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCancelarCobrancaImediata_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = pixcd.CancelarCobrancaImediata(txtTxIdCancelarCobrancaImediata.Text);
                rtbRespostasPIXCD.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCriarCobranca_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = pixcd.CriarCobrancaImediata(rtbCriarCobranca.Text, txtTxIdCriarCobranca.Text);
                rtbRespostasPIXCD.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarCobranca_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = pixcd.ConsultarCobranca(txtTxIdConsultarCobranca.Text, int.Parse(txtRevisaoConsultarCobranca.Text));
                rtbRespostasPIXCD.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarCobrancasCobV_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = pixcd.ConsultarCobrancasCobV(DateTime.Parse(txtDataInicialConsultarCobrancasCobV.Text), DateTime.Parse(txtDataFinalConsultarCobrancasCobV.Text), txtCPFCNPJConsultarCobrancasCobV.Text, ckbLocationConsultarCobrancasCobV.Checked, cmbStatusConsultarCobrancasCobV.SelectedIndex, (int)nudPagAtualConsultarCobrancasCobV.Value, (int)nudItensPorPaginaConsultarCobrancasCobV.Value);
                rtbRespostasPIXCD.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnRevisarCobranca_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = pixcd.RevisarCobranca(rtbRevisarCobranca.Text, txtTxIdRevisarCobranca.Text);
                rtbRespostasPIXCD.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCancelarCobranca_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = pixcd.CancelarCobranca(txtTxIdCancelarCobranca.Text);
                rtbRespostasPIXCD.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnLimparRespostas_Click(object sender, EventArgs e)
        {
            rtbRespostasPIXCD.Clear();
        }

        private void btnOpenSSLInfo_Click(object sender, EventArgs e)
        {
            var ret = pixcd.OpenSSLInfo();
            rtbRespostasPIXCD.AppendText(ret);
        }

        private void btnEnviarBoletoWebService_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = boleto.EnviarBoleto(OperacaoBoleto.tpInclui);
                rtbRespostasBoleto.AppendLine(ret.Retorno);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultaDetalhe_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = boleto.EnviarBoleto(OperacaoBoleto.tpConsultaDetalhe);
                rtbRespostasBoleto.AppendLine(ret.Retorno);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnGerarRemessa_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = "";
                if (string.IsNullOrEmpty(txtDirRemessa.Text))
                    ret = Application.StartupPath;
                else
                    ret = txtDirRemessa.Text;
                boleto.GerarRemessa(ret, 1, txtNomeRemessa.Text);
                rtbRespostasBoleto.AppendLine("Remessa Gerada.");
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnLerRetorno_Click(object sender, EventArgs e)
        {
            try
            {
                boleto.LerRetorno(txtDirRetorno.Text, txtNomeRetorno.Text);
                var ret = boleto.ObterRetorno(txtDirRetorno.Text, txtNomeRetorno.Text);
                rtbRespostasBoleto.AppendLine(ret.Retorno);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnIncluirTitulo_Click(object sender, EventArgs e)
        {
            try
            {
                boleto.IncluirTitulos(rtbIncluirTitulos.Text);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnTotalTitulo_Click(object sender, EventArgs e)
        {
            var ret = boleto.TotalTitulosLista();
            rtbRespostasBoleto.AppendLine(ret.ToString());
        }

        private void btnImprimirBoleto_Click(object sender, EventArgs e)
        {
            try
            {
                boleto.Imprimir(0);
                rtbRespostasBoleto.AppendLine("Boletos impressos.");

            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnLimparLista_Click(object sender, EventArgs e)
        {
            boleto.LimparLista();
            rtbRespostasBoleto.AppendLine("Lista Limpa.");
        }

        private void btnClasseTitulo_Click(object sender, EventArgs e)
        {
            try
            {
                ConfigBoleto();
                GerarTitulo();
                rtbRespostasBoleto.AppendLine("Título(s) adicionado(s).");

            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }
        private void ConfigBoleto()
        {
            var banco = new Banco();
            banco.TipoCobranca = cmbBanco.GetSelectedValue<ACBrTipoCobranca>();
            banco.CNAB = cmbLayoutCNAB.GetSelectedValue<ACBrLayoutRemessa>();
            banco.VersaoArquivo = int.Parse(txtVersaoArquivo.Text);
            banco.VersaoLote = int.Parse(txtVersaoLote.Text);

            var conta = new Conta();
            conta.Agencia = txtAgencia.Text;
            conta.DigitoAgencia = txtDigAgencia.Text;
            conta.Numero = txtConta.Text;
            conta.Digito = txtDigConta.Text;

            var cedente = new Cedente();
            cedente.Nome = txtNomeRes.Text;
            cedente.CNPJCPF = txtCNPJCPFCedente.Text;
            cedente.Logradouro = txtLogradouroCedente.Text;
            cedente.Numero = txtNumeroRes.Text;
            cedente.CEP = txtCEP.Text;
            cedente.Cidade = txtCidade.Text;
            cedente.UF = cmbUF.Text;
            cedente.Complemento = txtComplemento.Text;
            cedente.RespEmis = cmbRespEmissao.GetSelectedValue<ACBrResponEmissao>();
            cedente.CodigoCedente = txtCodCedente.Text;
            cedente.LayoutBol = cmbModeloImpressao.GetSelectedValue<ACBrBolLayOut>();
            cedente.CaracTitulo = cmbCaracTitulo.GetSelectedValue<CaracTitulo>();
            cedente.TipoCarteira = cmbTipoCarteira.GetSelectedValue<ACBrTipoCarteira>();
            cedente.TipoDocumento = cmbTipoDocumento.GetSelectedValue<ACBrTipoDocumento>();
            cedente.Modalidade = txtModalidade.Text;
            cedente.CodigoTransmissao = txtCodTransmissao.Text;
            cedente.Convenio = txtConvenio.Text;
            cedente.ChavePIX = txtChavePIX.Text;
            cedente.TipoChavePIX = cmbTipoChavePIXBoleto.GetSelectedValue<TipoChavePIX>();

            BoletoInfo[] boletoInfo = new BoletoInfo[3];
            boletoInfo[0] = banco;
            boletoInfo[1] = conta;
            boletoInfo[2] = cedente;

            boleto.ConfigurarDados(boletoInfo);
        }

        private void GerarTitulo()
        {
            Titulo[] titulo = new Titulo[1];
            titulo[0] = new Titulo();

            titulo[0].NumeroDocumento = txtNumeroDocumento.Text;
            titulo[0].NossoNumero = txtNossoNumero.Text;
            titulo[0].SeuNumero = txtSeuNumero.Text;
            titulo[0].Carteira = txtCarteira.Text;
            titulo[0].ValorDocumento = decimal.Parse(txtValorDocumento.Text, new CultureInfo("pt-BR"));
            titulo[0].Vencimento = DateTime.Parse(txtVencimento.Text, new CultureInfo("pt-BR"));
            titulo[0].DataDocumento = DateTime.Parse(txtDataDocumento.Text);
            titulo[0].DataProcessamento = DateTime.Parse(txtDataProcessamento.Text);
            titulo[0].DataAbatimento = DateTime.Parse(txtDataAbatimento.Text);
            titulo[0].ValorAbatimento = decimal.Parse(txtValorAbatimento.Text);
            titulo[0].DataDesconto = DateTime.Parse(txtDataDesconto.Text);
            titulo[0].TipoDesconto = cmbTipoDesconto.GetSelectedValue<TipoDesconto>();
            titulo[0].ValorDesconto = decimal.Parse(txtValorDesconto.Text);
            titulo[0].CodigoMora = txtCodMoraJuros.Text;
            titulo[0].ValorMoraJuros = decimal.Parse(txtValorMoraJuros.Text);
            titulo[0].DataMoraJuros = DateTime.Parse(txtDataMoraJuros.Text);
            titulo[0].ValorIOF = decimal.Parse(txtValorIOF.Text);
            titulo[0].ValorOutrasDespesas = decimal.Parse(txtValorOutrasDespesas.Text);
            titulo[0].DataMulta = DateTime.Parse(txtDataMulta.Text);

            if (ckbMultaValorFixo.Checked == true) 
            {
                titulo[0].MultaValorFixo = true;
            }
            else 
            {
                titulo[0].MultaValorFixo = false;
            }

            titulo[0].PercentualMulta = decimal.Parse(txtPercentualMulta.Text);
            titulo[0].DiasDeProtesto = int.Parse(txtDiasProtesto.Text);
            titulo[0].DataProtesto = DateTime.Parse(txtDataProtesto.Text);
            titulo[0].TipoDiasProtesto = cmbTipoDiasIntrucao.GetSelectedValue<TipoDiasIntrucao>();
            titulo[0].DiasDeNegativacao = int.Parse(txtDiasNegativacao.Text);
            titulo[0].DataNegativacao = DateTime.Parse(txtDataNegativacao.Text);
            titulo[0].CodigoNegativacao = cmbCodigoNegativacao.GetSelectedValue<CodigoNegativacao>();
            titulo[0].TipoDiasNegativacao = cmbTipoDiasNegativacao.GetSelectedValue<TipoDiasIntrucao>();
            titulo[0].DataBaixa = DateTime.Parse(txtDataBaixa.Text);
            titulo[0].DataLimitePagto = DateTime.Parse(txtDataLimitePagamento.Text);
            titulo[0].Especie = txtEspecieDocumento.Text;
            titulo[0].EspecieMod = txtEspecieMoeda.Text;
            titulo[0].Instrucao1 = rtbInstrucoesBoleto.Text;

            titulo[0].Sacado.NomeSacado = txtNomeSacado.Text;
            titulo[0].Sacado.Pessoa = cmbTipoInscricao.GetSelectedValue<ACBrPessoa>();
            titulo[0].Sacado.CNPJCPF = txtCNPJCPFSacado.Text;
            titulo[0].Sacado.Logradouro = txtLogradouroSacado.Text;
            titulo[0].Sacado.Numero = txtNumeroSacado.Text;
            titulo[0].Sacado.Bairro = txtBairroSacado.Text;
            titulo[0].Sacado.Complemento = txtComplementoSacado.Text;
            titulo[0].Sacado.Cidade = txtCidadeSacado.Text;
            titulo[0].Sacado.UF = txtUFSacado.Text;
            titulo[0].Sacado.CEP = txtCEPSacado.Text;
            titulo[0].Sacado.Email = txtEmailSacado.Text;

            boleto.IncluirTitulos(titulo);
        }

        private void btnLimparRespostasBoleto_Click(object sender, EventArgs e)
        {
            rtbRespostasBoleto.Clear();
        }

        private void btnPreencherDados_Click(object sender, EventArgs e)
        {
            //Cedente
            txtNomeRes.Text = "SAO JOAO LTDA";
            txtCNPJCPFCedente.Text = "18760540000139";
            txtLogradouroCedente.Text = "Rua Evaristo Mendes";
            txtNumeroRes.Text = "200";
            txtBairro.Text = "Centro";
            txtComplemento.Text = "Sala 10";
            txtCidade.Text = "Tatui";
            cmbUF.Text = "SP";
            txtCEP.Text = "18.270-000";
            txtTelefone.Text = "1145678564";
            txtCodTransmissao.Text = "10";
            txtConvenio.Text = "2330809";
            txtModalidade.Text = "17";
            txtCodCedente.Text = "2330809";

            //Conta Bancária
            txtConta.Text = "00000123873";
            txtDigConta.Text = "0";
            txtAgencia.Text = "0452";
            txtDigAgencia.Text = "0";
            txtVersaoArquivo.Text = "22";
            txtVersaoLote.Text = "40";
            txtLocalPagamento.Text = "Pagar preferencialmente nas agencias do C6BANK";

            //Informações Boleto
            txtNumeroDocumento.Text = "000001";
            txtNossoNumero.Text = "0";
            txtSeuNumero.Text = "000001";
            txtCarteira.Text = "21";

            var valorDocumento = 10.00M;
            txtValorDocumento.Text = valorDocumento.ToString();
            
            var vencimento = DateTime.Now.AddDays(30);
            txtVencimento.Text = vencimento.ToString("dd/MM/yyyy");

            var dataDocumento = DateTime.Now.Date;
            txtDataDocumento.Text = dataDocumento.ToString("dd/MM/yyyy");

            var dataProcessamento = DateTime.Now.Date;
            txtDataProcessamento.Text = dataProcessamento.ToString("dd/MM/yyyy");

            var dataAbatimento = DateTime.Now.Date;
            txtDataAbatimento.Text = dataAbatimento.ToString("dd/MM/yyyy");

            var valorAbatimento = 1.0M;
            txtValorAbatimento.Text = valorAbatimento.ToString();

            var dataDesconto = DateTime.Now.AddDays(20);
            txtDataDesconto.Text = dataDesconto.ToString("dd/MM/yyyy");

            cmbTipoDesconto.Text = TipoDesconto.tdNaoConcederDesconto.ToString();

            var valorDesconto = 0.5M;
            txtValorDesconto.Text = valorDesconto.ToString();

            txtCodMoraJuros.Text = "";

            var valorMoraJuros = 0.0M;
            txtValorMoraJuros.Text = valorMoraJuros.ToString();
            
            var dataMoraJuros = DateTime.Now.AddDays(30);
            txtDataMoraJuros.Text = dataMoraJuros.ToString("dd/MM/yyyy");

            var valorIOF = 0;
            txtValorIOF.Text = valorIOF.ToString();
            
            var valorOutrasDespesas = 2.50M;
            txtValorOutrasDespesas.Text = valorOutrasDespesas.ToString();
            
            var dataMulta = DateTime.Now.AddDays(30);
            txtDataMulta.Text = dataMulta.ToString("dd/MM/yyyy");

            ckbMultaValorFixo.Checked = true;

            var percentualMulta = 5.00M;
            txtPercentualMulta.Text = percentualMulta.ToString();

            var diasProtesto = 0;
            txtDiasProtesto.Text = diasProtesto.ToString();
            
            var dataProtesto = DateTime.Now.AddDays(60);
            txtDataProtesto.Text = dataProtesto.ToString("dd/MM/yyyy");

            cmbTipoDiasIntrucao.Text = TipoDiasIntrucao.diCorridos.ToString();

            var diasNegativacao = 0;
            txtDiasNegativacao.Text = diasNegativacao.ToString();
            
            var dataNegativacao = DateTime.Now.AddDays(90);
            txtDataNegativacao.Text = dataNegativacao.ToString("dd/MM/yyyy");

            cmbCodigoNegativacao.Text = CodigoNegativacao.cnNenhum.ToString();

            cmbTipoDiasNegativacao.Text = TipoDiasIntrucao.diCorridos.ToString();

            var dataBaixa = DateTime.Now.AddDays(30);
            txtDataBaixa.Text = dataBaixa.ToString("dd/MM/yyyy");
            
            var dataLimitePagamento = DateTime.Now.AddDays(30);
            txtDataLimitePagamento.Text = dataLimitePagamento.ToString("dd/MM/yyyy");

            txtEspecieDocumento.Text = "DM";    
            txtEspecieMoeda.Text = "R$";
            rtbInstrucoesBoleto.Text = "1";

            txtNomeSacado.Text = "José da Silva";
            cmbTipoInscricao.Text = ACBrPessoa.pJuridica.ToString();
            txtCNPJCPFSacado.Text = "99999999999";
            txtLogradouroSacado.Text = "Rua da Colina";
            txtNumeroSacado.Text = "1111";
            txtBairroSacado.Text = "Centro";
            txtComplementoSacado.Text = "Prédio 2";
            txtCidadeSacado.Text = "Tatui";
            txtUFSacado.Text = "SP";
            txtCEPSacado.Text = "18280-000";
            txtEmailSacado.Text = "josesilva@mail.com";
        }
    }
}
