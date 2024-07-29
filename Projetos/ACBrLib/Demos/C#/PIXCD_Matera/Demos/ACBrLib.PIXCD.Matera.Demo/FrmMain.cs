using System;
using System.IO;
using System.Windows.Forms;
using ACBrLib;
using ACBrLib.Core;
using ACBrLib.Core.DFe;
using ACBrLib.Core.Extensions;
using ACBrLib.Core.Config;
using ACBrLib.PIXCD;
using System.Linq;
using ACBrLib.Core.PIXCD;

namespace ACBrLibPIXCD.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBrPIXCD ACBrPIXCD;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            // Inicializando a classe e carregando a dll
            ACBrPIXCD = new ACBrPIXCD();
        }

        #endregion Constructors

        #region Methods

        #region EventHandlers

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            SplashScreenManager.Show<FrmWait>();
            SplashScreenManager.ShowInfo(SplashInfo.Message, "Carregando...");

            try
            {
                cmbPSP.EnumDataSource(PSP.pspMatera);
                cmbAmbiente.EnumDataSource(Ambiente.ambTeste);
                cmbNivelLogPSP.EnumDataSource(NivelLogPSP.logPSPNenhum);
                cmbTipoChave.EnumDataSource(TipoChave.tchNenhuma);
                ACBrPIXCD.Config.ProxyPort = 0;
                
                // Altera as config de log
                ACBrPIXCD.Config.Principal.LogNivel = NivelLog.logParanoico;

                var logPath = Path.Combine(Application.StartupPath, "Logs");
                if (!Directory.Exists(logPath))
                    Directory.CreateDirectory(logPath);

                ACBrPIXCD.Config.Principal.LogPath = logPath;
                ACBrPIXCD.ConfigGravar();

                LoadConfig();
            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        private void FrmMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            // Liberando a dll
            ACBrPIXCD.Dispose();
        }

        #endregion EventHandlers

        #endregion Methods

        private void btnSalvarConfiguracoes_Click(object sender, EventArgs e)
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

        private void SalvarConfig()
        {
            SplashScreenManager.Show<FrmWait>();
            SplashScreenManager.ShowInfo(SplashInfo.Message, "Salvando...");

            try
            {
                //PIXCD
                ACBrPIXCD.Config.Ambiente = cmbAmbiente.GetSelectedValue<Ambiente>();
                ACBrPIXCD.Config.ArqLog = txtArqLogPSP.Text;
                ACBrPIXCD.Config.NivelLog = cmbNivelLogPSP.GetSelectedValue<NivelLogPSP>();
                ACBrPIXCD.Config.TipoChave = cmbTipoChave.GetSelectedValue<TipoChave>();
                ACBrPIXCD.Config.PSP = cmbPSP.GetSelectedValue<PSP>();
                ACBrPIXCD.Config.Timeout = (int)nudPSPTimeout.Value;
                ACBrPIXCD.Config.ProxyHost = txtProxyServidor.Text;
                ACBrPIXCD.Config.ProxyPass = txtProxySenha.Text;
                ACBrPIXCD.Config.ProxyPort = (int)nudProxyPorta.Value;
                ACBrPIXCD.Config.ProxyUser = txtProxyUsuario.Text;
                ACBrPIXCD.Config.CEPRecebedor = txtCEPRecebedor.Text;
                ACBrPIXCD.Config.CidadeRecebedor = txtCidadeRecebedor.Text;
                ACBrPIXCD.Config.NomeRecebedor = txtNomeRecebedor.Text;
                ACBrPIXCD.Config.UFRecebedor = txtUFRecebedor.Text;

                //Matera
                ACBrPIXCD.Config.Matera.ChavePIX = txtChavePIXMatera.Text;
                ACBrPIXCD.Config.Matera.ClientID = txtClientIDMatera.Text;
                ACBrPIXCD.Config.Matera.SecretKey = txtSecretKeyMatera.Text;
                ACBrPIXCD.Config.Matera.ClientSecret = txtClientSecretMatera.Text;
                ACBrPIXCD.Config.Matera.ArqCertificado = txtArquivoCertificadoMatera.Text;
                ACBrPIXCD.Config.Matera.ArqChavePrivada = txtArquivoChavePrivadaMatera.Text;
                ACBrPIXCD.Config.Matera.AccountID = txtAccountIDMatera.Text;
                ACBrPIXCD.Config.Matera.MediatorFee = txtMediatorFeeMatera.Text;
                ACBrPIXCD.Config.Matera.Scopes = txtScopesMatera.Text;

                ACBrPIXCD.ConfigGravar();

                Application.DoEvents();
            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        private void btnCarregarConfiguracoes_Click(object sender, EventArgs e)
        {
            var file = Helpers.OpenFile("Arquivos Ini (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
            if (!File.Exists(file)) return;

            LoadConfig(file);
        }

        private void LoadConfig(string file = "ACBrLib.ini")
        {
            ACBrPIXCD.ConfigLer(file);

            //PIXCD
            cmbAmbiente.SetSelectedValue(ACBrPIXCD.Config.Ambiente);
            txtArqLogPSP.Text = ACBrPIXCD.Config.ArqLog;
            cmbNivelLogPSP.SetSelectedValue(ACBrPIXCD.Config.NivelLog);
            cmbTipoChave.SetSelectedValue(ACBrPIXCD.Config.TipoChave);
            cmbPSP.SetSelectedValue(ACBrPIXCD.Config.PSP);
            nudPSPTimeout.Value = ACBrPIXCD.Config.Timeout;
            txtProxyServidor.Text = ACBrPIXCD.Config.ProxyHost;
            txtProxySenha.Text = ACBrPIXCD.Config.ProxyPass;
            nudProxyPorta.Value = ACBrPIXCD.Config.ProxyPort;
            txtProxyUsuario.Text = ACBrPIXCD.Config.ProxyUser;
            txtCEPRecebedor.Text = ACBrPIXCD.Config.CEPRecebedor;
            txtCidadeRecebedor.Text = ACBrPIXCD.Config.CidadeRecebedor;
            txtNomeRecebedor.Text = ACBrPIXCD.Config.NomeRecebedor;
            txtUFRecebedor.Text = ACBrPIXCD.Config.UFRecebedor;

            //Matera
            txtChavePIXMatera.Text = ACBrPIXCD.Config.Matera.ChavePIX;
            txtClientIDMatera.Text = ACBrPIXCD.Config.Matera.ClientID;
            txtSecretKeyMatera.Text = ACBrPIXCD.Config.Matera.SecretKey;
            txtClientSecretMatera.Text = ACBrPIXCD.Config.Matera.ClientSecret;
            txtArquivoCertificadoMatera.Text = ACBrPIXCD.Config.Matera.ArqCertificado;
            txtArquivoChavePrivadaMatera.Text = ACBrPIXCD.Config.Matera.ArqChavePrivada;
            txtAccountIDMatera.Text = ACBrPIXCD.Config.Matera.AccountID;
            txtMediatorFeeMatera.Text = ACBrPIXCD.Config.Matera.MediatorFee;
            txtScopesMatera.Text = ACBrPIXCD.Config.Matera.Scopes;
        }

        private void btnArquivoChavePrivadaMatera_Click(object sender, EventArgs e)
        {
            txtArquivoChavePrivadaMatera.Text = Helpers.OpenFile("Arquivos KEY (*.key)|*.key|Todos os Arquivos (*.*)|*.*");
        }

        private void btnArquivoCertificadoMatera_Click(object sender, EventArgs e)
        {
            txtArquivoCertificadoMatera.Text = Helpers.OpenFile("Arquivos PEM (*.pem)|*.pem|Todos os Arquivos (*.*)|*.*");
        }

        private void btnOpenSSLInfo_Click(object sender, EventArgs e)
        {
            var ret = ACBrPIXCD.OpenSSLInfo();
            rtbRespostas.AppendText(ret);
        }

        private void btnLimparRespostas_Click(object sender, EventArgs e)
        {
            rtbRespostas.Clear();
        }

        private void btnCriarConta_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = ACBrPIXCD.IncluirConta(rtbIncluirConta.Text);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarConta_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = ACBrPIXCD.ConsultarConta(txtAccountIDConsultarConta.Text);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnInativarConta_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = ACBrPIXCD.InativarConta(txtAccountIDInativarConta.Text);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnIncluirChavePIX_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = ACBrPIXCD.IncluirChavePix(txtAccountIDIncluirChavePIX.Text, txtExternalIDIncluirChavePIX.Text);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarChavePIX_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = ACBrPIXCD.ConsultarChavePix(txtAccountIDConsultarChavePIX.Text);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnExcluirChavePIX_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = ACBrPIXCD.ExcluirChavePix(txtAccountIDExcluirChavePIX.Text, txtChavePIXExcluir.Text);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCriarQRCode_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = ACBrPIXCD.GerarQRCode(rtbQRCode.Text);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarTransacao_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = ACBrPIXCD.ConsultarTransacao(txtAccountIDConsultarTransacao.Text, txtTransactionIDConsultarTransacao.Text);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarSaldoEC_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = ACBrPIXCD.ConsultarSaldoEC(txtAccountIDConsultarSaldoEC.Text);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarExtratoEC_Click(object sender, EventArgs e)
        {
            try
            {
                var dataInicial = "dd/MM/yyyy";
                if (InputBox.Show("Consultar Extrato EC", "Informe a Data Inicial", ref dataInicial) != DialogResult.OK) return;

                var dataFinal = "dd/MM/yyyy";
                if (InputBox.Show("Consultar Extrato EC", "Informe a Data Final", ref dataFinal) != DialogResult.OK) return;

                var ret = ACBrPIXCD.ConsultarExtratoEC(txtAccountIDConsultarExtratoEC.Text, DateTime.Parse(dataInicial), DateTime.Parse(dataFinal));
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnAccountIDConsultarSaldoMediator_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = ACBrPIXCD.ConsultarSaldoEC(txtAccountIDConsultarSaldoMediator.Text);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnAccountIDConsultarExtratoMediator_Click(object sender, EventArgs e)
        {
            try
            {
                var dataInicial = "dd/MM/yyyy";
                if (InputBox.Show("Consultar Extrato Mediator", "Informe a Data Inicial", ref dataInicial) != DialogResult.OK) return;

                var dataFinal = "dd/MM/yyyy";
                if (InputBox.Show("Consultar Extrato Mediator", "Informe a Data Final", ref dataFinal) != DialogResult.OK) return;

                var ret = ACBrPIXCD.ConsultarExtratoEC(txtAccountIDConsultarExtratoMediator.Text, DateTime.Parse(dataInicial), DateTime.Parse(dataFinal));
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarMotivosDevolucoes_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = ACBrPIXCD.ConsultarMotivosDevolucao();
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnSolicitarDevolucao_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = ACBrPIXCD.SolicitarDevolucao(rtbEfetuarDevolucao.Text, txtAccountIDSolicitarDevolucao.Text, txtTransactionIDSolicitarDevolucao.Text);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarAliasDestinatario_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = ACBrPIXCD.ConsultarAliasRetirada(txtAccountIDConsultarAliasDestinatario.Text, txtAliasConsultarAliasDestinatario.Text);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnSolicitarRetirada_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = ACBrPIXCD.SolicitarRetirada(rtbSolicitarRetirada.Text, txtAccountIDSolicitarRetirada.Text);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }
    }
}