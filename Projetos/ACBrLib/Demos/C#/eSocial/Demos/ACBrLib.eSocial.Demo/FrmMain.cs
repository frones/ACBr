using System;
using System.IO;
using System.Windows.Forms;
using ACBrLib;
using ACBrLib.Core;
using ACBrLib.Core.DFe;
using ACBrLib.Core.eSocial;
using ACBrLib.eSocial;
using System.Linq;

namespace ACBrLibeSocial.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBreSocial ACBreSocial;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            // Inicializando a classe e carregando a dll
            ACBreSocial = new ACBreSocial();
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
                cmbUfDestino.SelectedItem = "SP";
                cmbSSlType.EnumDataSource(SSLType.LT_all);
                cmbCrypt.EnumDataSource(SSLCryptLib.cryWinCrypt);
                cmbHttp.EnumDataSource(SSLHttpLib.httpWinHttp);
                cmbXmlSign.EnumDataSource(SSLXmlSignLib.xsLibXml2);
                cmbTipoEmpregador.EnumDataSource(TipoEmpregador.tePessoaJuridica);
                cmbFormaEmissao.EnumDataSource(TipoEmissao.teNormal);
                cmbVersaoDF.SelectedItem = "S01_02_00";

                // Altera as config de log
                ACBreSocial.Config.Principal.LogNivel = NivelLog.logParanoico;

                var logPath = Path.Combine(Application.StartupPath, "Logs");
                if (!Directory.Exists(logPath))
                    Directory.CreateDirectory(logPath);

                ACBreSocial.Config.Principal.LogPath = logPath;
                ACBreSocial.ConfigGravar();

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
            ACBreSocial.Dispose();
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
                //Config Geral
                ACBreSocial.Config.ExibirErroSchema = ckbExibirErroSchema.Checked;
                ACBreSocial.Config.FormatoAlerta = txtFormatoAlerta.Text;
                ACBreSocial.Config.FormaEmissao = cmbFormaEmissao.GetSelectedValue<TipoEmissao>();
                ACBreSocial.Config.RetirarAcentos = ckbRetirarAcentos.Checked;
                ACBreSocial.Config.SalvarWS = ckbSalvar.Checked;
                ACBreSocial.Config.PathSalvar = txtLogs.Text;
                ACBreSocial.Config.PathSchemas = txtSchemaPath.Text;
                ACBreSocial.Config.IdEmpregador = txtEmpregador.Text;
                ACBreSocial.Config.IdTransmissor = txtTransmissor.Text;
                ACBreSocial.Config.TipoEmpregador = cmbTipoEmpregador.GetSelectedValue<TipoEmpregador>();
                ACBreSocial.Config.VersaoDF = cmbVersaoDF.SelectedIndex.ToString();

                //Config WebService
                ACBreSocial.Config.DFe.UF = cmbUfDestino.Text;

                ACBreSocial.Config.SSLType = cmbSSlType.GetSelectedValue<SSLType>();
                ACBreSocial.Config.Timeout = (int)nudTimeOut.Value;
                ACBreSocial.Config.Ambiente = rdbHomologacao.Checked ? TipoAmbiente.taHomologacao : TipoAmbiente.taProducao;
                ACBreSocial.Config.Visualizar = ckbVisualizar.Checked;
                ACBreSocial.Config.SalvarWS = ckbSalvarSOAP.Checked;
                ACBreSocial.Config.AjustaAguardaConsultaRet = ckbAjustarAut.Checked;
                ACBreSocial.Config.AguardarConsultaRet = (int)nudAguardar.Value;
                ACBreSocial.Config.Tentativas = (int)nudTentativas.Value;
                ACBreSocial.Config.IntervaloTentativas = (int)nudIntervalos.Value;

                //Proxy
                ACBreSocial.Config.Proxy.Servidor = txtProxyServidor.Text;
                ACBreSocial.Config.Proxy.Porta = nudProxyPorta.Text;
                ACBreSocial.Config.Proxy.Usuario = txtProxyUsuario.Text;
                ACBreSocial.Config.Proxy.Senha = txtProxySenha.Text;

                //Certificado
                ACBreSocial.Config.DFe.SSLCryptLib = cmbCrypt.GetSelectedValue<SSLCryptLib>();
                ACBreSocial.Config.DFe.SSLHttpLib = cmbHttp.GetSelectedValue<SSLHttpLib>();
                ACBreSocial.Config.DFe.SSLXmlSignLib = cmbXmlSign.GetSelectedValue<SSLXmlSignLib>();
                ACBreSocial.Config.DFe.ArquivoPFX = txtCertPath.Text;
                ACBreSocial.Config.DFe.Senha = txtCertPassword.Text;
                ACBreSocial.Config.DFe.NumeroSerie = txtCertNumero.Text;
                ACBreSocial.Config.DFe.DadosPFX = txtDadosPFX.Text;

                //Arquivos
                ACBreSocial.Config.SalvarGer = ckbSalvarArqs.Checked;
                ACBreSocial.Config.SepararPorMes = ckbPastaMensal.Checked;
                ACBreSocial.Config.AdicionarLiteral = ckbAdicionaLiteral.Checked;
                ACBreSocial.Config.EmissaoPatheSocial = ckbEmissaoPatheSocial.Checked;
                ACBreSocial.Config.SalvarArq = ckbSalvaPathEvento.Checked;
                ACBreSocial.Config.SepararPorCNPJ = ckbSepararPorCNPJ.Checked;
                ACBreSocial.Config.SepararPorModelo = ckbSepararPorModelo.Checked;
                ACBreSocial.Config.PatheSocial = txtArqeSocial.Text;

                ACBreSocial.ConfigGravar();

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
            ACBreSocial.ConfigLer(file);

            //Config Geral

            ckbExibirErroSchema.Checked = ACBreSocial.Config.ExibirErroSchema;
            txtFormatoAlerta.Text = ACBreSocial.Config.FormatoAlerta;
            cmbFormaEmissao.SetSelectedValue(ACBreSocial.Config.FormaEmissao);
            ckbRetirarAcentos.Checked = ACBreSocial.Config.RetirarAcentos;
            ckbSalvar.Checked = ACBreSocial.Config.SalvarWS;
            txtLogs.Text = ACBreSocial.Config.PathSalvar;
            txtSchemaPath.Text = ACBreSocial.Config.PathSchemas;
            txtEmpregador.Text = ACBreSocial.Config.IdEmpregador;
            txtTransmissor.Text = ACBreSocial.Config.IdTransmissor;
            cmbTipoEmpregador.SetSelectedValue(ACBreSocial.Config.TipoEmpregador);
            cmbVersaoDF.SelectedIndex.ToString(ACBreSocial.Config.VersaoDF);

            //Config Webservice
            cmbUfDestino.SelectedItem = ACBreSocial.Config.DFe.UF;
            cmbSSlType.SetSelectedValue(ACBreSocial.Config.SSLType);
            nudTimeOut.Value = ACBreSocial.Config.Timeout;

            var ambiente = ACBreSocial.Config.Ambiente;
            rdbHomologacao.Checked = ambiente == TipoAmbiente.taHomologacao;
            rdbProducao.Checked = ambiente == TipoAmbiente.taProducao;

            ckbVisualizar.Checked = ACBreSocial.Config.Visualizar;
            ckbSalvarSOAP.Checked = ACBreSocial.Config.SalvarWS;
            ckbAjustarAut.Checked = ACBreSocial.Config.AjustaAguardaConsultaRet;
            nudAguardar.Value = ACBreSocial.Config.AguardarConsultaRet;
            nudTentativas.Value = ACBreSocial.Config.Tentativas;
            nudIntervalos.Value = ACBreSocial.Config.IntervaloTentativas;
            
            //Proxy
            txtProxyServidor.Text = ACBreSocial.Config.Proxy.Servidor;
            nudProxyPorta.Text = ACBreSocial.Config.Proxy.Porta;
            txtProxyUsuario.Text = ACBreSocial.Config.Proxy.Usuario;
            txtProxySenha.Text = ACBreSocial.Config.Proxy.Senha;

            //Config Certificado
            cmbCrypt.SetSelectedValue(ACBreSocial.Config.DFe.SSLCryptLib);
            cmbHttp.SetSelectedValue(ACBreSocial.Config.DFe.SSLHttpLib);
            cmbXmlSign.SetSelectedValue(ACBreSocial.Config.DFe.SSLXmlSignLib);
            txtCertPath.Text = ACBreSocial.Config.DFe.ArquivoPFX;
            txtCertPassword.Text = ACBreSocial.Config.DFe.Senha;
            txtCertNumero.Text = ACBreSocial.Config.DFe.NumeroSerie;
            txtDadosPFX.Text = ACBreSocial.Config.DFe.DadosPFX;

            //Config Arquivos
            ckbSalvarArqs.Checked = ACBreSocial.Config.SalvarGer;
            ckbPastaMensal.Checked = ACBreSocial.Config.SepararPorMes;
            ckbAdicionaLiteral.Checked = ACBreSocial.Config.AdicionarLiteral;
            ckbEmissaoPatheSocial.Checked = ACBreSocial.Config.EmissaoPatheSocial;
            ckbSalvaPathEvento.Checked = ACBreSocial.Config.SalvarArq;
            ckbSepararPorCNPJ.Checked = ACBreSocial.Config.SepararPorCNPJ;
            ckbSepararPorModelo.Checked = ACBreSocial.Config.SepararPorModelo;
            txtArqeSocial.Text = ACBreSocial.Config.PatheSocial;

        }

        private void btnCriarEventoeSocial_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoIni = Helpers.OpenFile("Arquivo Ini (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoIni)) return;

                ACBreSocial.CriarEventoeSocial(arquivoIni);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviareSocial_Click(object sender, EventArgs e)
        {
            try
            {
                var grupo = 1;
                if (InputBox.Show("Enviar eSocial", "Grupo", ref grupo) != DialogResult.OK) return;

                var ret = ACBreSocial.EnviareSocial(grupo);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultareSocial_Click(object sender, EventArgs e)
        {
            try
            {
                var protocolo = "123456789";
                if (InputBox.Show("Consultar eSocial", "Consultar", ref protocolo) != DialogResult.OK) return;

                var ret = ACBreSocial.ConsultareSocial(protocolo.ToString());
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCriarEnviareSocial_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoIni = Helpers.OpenFile("Arquivo Ini (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoIni)) return;

                var grupo = 1;
                if (InputBox.Show("Enviar eSocial", "Grupo", ref grupo) != DialogResult.OK) return;

                string ret = ACBreSocial.CriarEnviareSocial(arquivoIni, grupo);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnLimpareSocial_Click(object sender, EventArgs e)
        {
            try
            {
                if (MessageBox.Show(@"Limpar a lista ?", @"ACBrLibeSocial", MessageBoxButtons.YesNo) == DialogResult.Yes)
                    ACBreSocial.LimpareSocial();
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCarregarXMLEventoeSocial_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoxml = Helpers.OpenFile("Arquivo Xml eSocial (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoxml)) return;

                ACBreSocial.CarregarXMLEventoeSocial(arquivoxml);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnSetIDEmpregador_Click(object sender, EventArgs e)
        {
            try
            {
                ACBreSocial.SetIDEmpregador(txtEmpregador.Text);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnSetIDTransmissor_Click(object sender, EventArgs e)
        {
            try
            {
                ACBreSocial.SetIDTransmissor(txtTransmissor.Text);
            }
            catch(Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnSetTipoEmpregador_Click(object sender, EventArgs e)
        {
            try
            {
                ACBreSocial.TipoEmpregador(cmbTipoEmpregador.SelectedIndex);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnSetVersaoDF_Click(object sender, EventArgs e)
        {
            try
            {
                ACBreSocial.SetVersao(cmbVersaoDF.SelectedIndex.ToString());
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultaIdentificadoresEventosEmpregador_Click(object sender, EventArgs e)
        {
            try
            {
                var tipoEvento = 1;
                if (InputBox.Show("Consulta Empregador eSocial", "Informe o Tipo do Evento", ref tipoEvento) != DialogResult.OK) return;

                var dataApuracao = DateTime.Now;
                string strDataApuracao = dataApuracao.ToString();
                if (InputBox.Show("Consulta Empregador eSocial", "Informe a Data de Apuração - 01/MM/AAAA", ref strDataApuracao) != DialogResult.OK) return;
                DateTime.TryParse(strDataApuracao, out dataApuracao);

                var ret = ACBreSocial.ConsultaIdentificadoresEventosEmpregador(txtEmpregador.Text, tipoEvento, dataApuracao);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultaIdentificadoresEventosTabela_Click(object sender, EventArgs e)
        {
            try
            {
                var tipoEvento = 1;
                if (InputBox.Show("Consulta Tabela eSocial", "Informe o Tipo do Evento", ref tipoEvento) != DialogResult.OK) return;

                var chave = "";
                if (InputBox.Show("Consulta Tabela eSocial", "Informe a Chave", ref chave) != DialogResult.OK) return;

                var dataInicial = DateTime.Now;
                string strDataInicial = dataInicial.ToString();
                if (InputBox.Show("Consulta Tabela eSocial", "Informe a Data Inicial - 01/MM/AAAA", ref strDataInicial) != DialogResult.OK) return;
                DateTime.TryParse(strDataInicial, out dataInicial);

                var dataFinal = DateTime.Now;
                string strDataFinal = dataFinal.ToString();
                if (InputBox.Show("Consulta Tabela eSocial", "Informe a Data Final - 01/MM/AAAA", ref strDataFinal) != DialogResult.OK) return;
                DateTime.TryParse(strDataFinal, out dataFinal);

                var ret = ACBreSocial.ConsultaIdentificadoresEventosTabela(txtEmpregador.Text, tipoEvento, chave, dataInicial, dataFinal);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultaIdentificadoresEventosTrabalhador_Click(object sender, EventArgs e)
        {
            try
            {
                var cpfTrabalhador = "";
                if (InputBox.Show("Consulta Trabalhador eSocial", "Informe o CPF do Trabalhador", ref cpfTrabalhador) != DialogResult.OK) return;

                var dataInicial = DateTime.Now;
                string strDataInicial = dataInicial.ToString();
                if (InputBox.Show("Consulta Trabalhador eSocial", "Informe a Data Inicial - 01/MM/AAAA", ref strDataInicial) != DialogResult.OK) return;
                DateTime.TryParse(strDataInicial, out dataInicial);

                var dataFinal = DateTime.Now;
                string strDataFinal = dataFinal.ToString();
                if (InputBox.Show("Consulta Trabalhador eSocial", "Informe a Data Final - 01/MM/AAAA", ref strDataFinal) != DialogResult.OK) return;
                DateTime.TryParse(strDataFinal, out dataFinal);

                var ret = ACBreSocial.ConsultaIdentificadoresEventosTrabalhador(txtEmpregador.Text, cpfTrabalhador, dataInicial, dataFinal);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnDownloadEventos_Click(object sender, EventArgs e)
        {
            try
            {
                var cpfTrabalhador = "";
                if (InputBox.Show("Download Eventos eSocial", "Informe o CPF do Trabalhador", ref cpfTrabalhador) != DialogResult.OK) return;

                var dataInicial = DateTime.Now;
                if (InputBox.Show("Download Eventos eSocial", "Informe a Data Inicial - 01/MM/AAAA", ref dataInicial) != DialogResult.OK) return;

                var dataFinal = DateTime.Now;
                if (InputBox.Show("Download Eventos eSocial", "Informe a Data Final - 01/MM/AAAA", ref dataFinal) != DialogResult.OK) return;

                var ret = ACBreSocial.DownloadEventos(txtEmpregador.Text, cpfTrabalhador, dataInicial, dataFinal);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnObterCertificados_Click(object sender, EventArgs e)
        {
            var ret = ACBreSocial.ObterCertificados();
            rtbRespostas.AppendLine(ret.Select(x => x.ToString()).ToArray());
        }
        private void btnOpenSSLInfo_Click(object sender, EventArgs e)
        {
            var ret = ACBreSocial.OpenSSLInfo();
            rtbRespostas.AppendText(ret);
        }

        private void btnSelectLog_Click(object sender, EventArgs e)
        {
            txtLogs.Text = Helpers.SaveFile("Arquivos Logs (*.log)|*.log|Todos os Arquivos (*.*)|*.*");
        }

        private void btnSelectSchema_Click(object sender, EventArgs e)
        {
            txtSchemaPath.Text = Helpers.SelectFolder();
        }

        private void btnSelecionarCertificado_Click(object sender, EventArgs e)
        {
            txtCertPath.Text = Helpers.OpenFile("Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*");
        }

        private void btnDadosPFX_Click(object sender, EventArgs e)
        {
            var file = Helpers.OpenFile("Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*");
            if (!File.Exists(file)) return;

            var dados = File.ReadAllBytes(file);
            txtDadosPFX.Text = Convert.ToBase64String(dados);
        }

        private void btnArqeSocial_Click(object sender, EventArgs e)
        {
            txtArqeSocial.Text = Helpers.SelectFolder();
        }
    }
}